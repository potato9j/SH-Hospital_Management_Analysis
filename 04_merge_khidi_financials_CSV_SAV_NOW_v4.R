# =============================================================================
# KHIDI 재무상태표/손익계산서 자동 병합 코드 v2
# =============================================================================
# 최종 출력 파일 수: 6개
#   1) KHIDI_재무상태표_통합.csv
#   2) KHIDI_손익계산서_통합.csv
#   3) KHIDI_재무상태표_분류ID_계정매칭.csv
#   4) KHIDI_손익계산서_분류ID_계정매칭.csv
#   5) KHIDI_재무상태표_통합.sav
#   6) KHIDI_손익계산서_통합.sav
#
# 처리 원칙:
#   - 각 파일의 좌측 금액열 = 당기
#   - 각 파일의 우측 금액열 = 전기
#   - 파일명 연도 = 당기 연도
#   - 파일명 연도 - 1 = 전기 연도
#   - 같은 병원-같은 연도-같은 계정이 당기/전기로 중복되면 당기값 우선 사용

# =============================================================================

# 0) 사용자 설정 -------------------------------------------------------------

data_dir <- "G:/A1_대학교 (여기ㅣ여기ㅣ여기ㅣ)/3-1/병원경영분석/1_기말프로젝트/0_자료모음/B_KHIDI_재무상태표&손익계산서"

# 최종 분석 대상 연도
# 2020년 파일의 전기는 2019년이므로 기본 분석 대상에서 제외
target_years <- 2020:2024

# 병원 수
expected_hospital_ids <- 1:12

# 동일 병원-파일연도-재무제표 종류 파일이 2개 이상 있을 때 처리 방식
# "latest" : 수정시간이 가장 최신인 파일 1개만 사용
# "stop"   : 중복 파일이 있으면 코드 중단
duplicate_file_policy <- "latest"

# 금액 후보 열 판단 기준
# 숫자가 3개 이상이고, 절대값 합계가 이 값 이상인 열을 우선 금액열로 봅니다.
# 기준이 너무 강하면 자동으로 숫자 열 전체를 다시 사용합니다.
amount_abs_min <- 1000
amount_numeric_n_min <- 3

# 최종 출력 폴더
# 이 폴더는 실행할 때마다 삭제 후 다시 생성됩니다.
out_dir <- file.path(data_dir, "merged_output_6files")

if (!dir.exists(data_dir)) {
  stop(
    "data_dir 폴더를 찾지 못했습니다. 경로를 확인하세요: ", data_dir,
    "\nWindows 경로에서는 \\ 대신 /를 쓰세요."
  )
}

if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE, force = TRUE)
}
dir.create(out_dir, recursive = TRUE)

# 1) 패키지 준비 -------------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  "jsonlite",
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "tibble", "readr", "haven"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

# 설치 후 다시 확인합니다.
still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(still_missing) > 0) {
  stop(
    "다음 패키지가 아직 설치되지 않았습니다: ", paste(still_missing, collapse = ", "),
    "\nR 콘솔에서 install.packages(c('", paste(still_missing, collapse = "','"), "'))를 먼저 실행하세요."
  )
}

suppressPackageStartupMessages({
  library(jsonlite)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(readr)
  library(haven)
})

# 2) 보조 함수 ---------------------------------------------------------------
normalize_key <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all("\\u00A0", " ") |>
    stringr::str_squish() |>
    stringr::str_replace_all("\\s+", "") |>
    stringr::str_replace_all("의료법인|학교법인|재단법인", "")
}

clean_account <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all("\\u00A0", " ") |>
    stringr::str_replace_all("\\r|\\n|\\t", " ") |>
    stringr::str_squish()
}

parse_amount <- function(x, dash_as_zero = TRUE) {
  raw <- as.character(x)
  raw <- stringr::str_replace_all(raw, "\\u00A0", " ")
  raw <- stringr::str_squish(raw)
  raw[raw %in% c("", "NA", "N/A", "NULL", "<NA>")] <- NA_character_

  if (dash_as_zero) {
    raw[raw %in% c("-", "－", "–", "—")] <- "0"
  } else {
    raw[raw %in% c("-", "－", "–", "—")] <- NA_character_
  }

  negative <- stringr::str_detect(raw, "^\\s*[-－−]") |
    stringr::str_detect(raw, "^\\s*△") |
    stringr::str_detect(raw, "^\\s*\\(.*\\)\\s*$")

  cleaned <- raw |>
    stringr::str_replace_all(",", "") |>
    stringr::str_replace_all("원|천원|백만원", "") |>
    stringr::str_replace_all("△", "") |>
    stringr::str_replace_all("\\(", "") |>
    stringr::str_replace_all("\\)", "") |>
    stringr::str_replace_all("[^0-9.\\-]", "")

  val <- suppressWarnings(as.numeric(cleaned))
  val[negative & !is.na(val)] <- -abs(val[negative & !is.na(val)])
  val
}

first_int_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_integer_)
  }
  as.integer(x[1])
}

get_vec <- function(df, col_no, row_no = NULL) {
  v <- as.character(df[[col_no]])
  if (!is.null(row_no)) {
    v <- v[row_no]
  }
  v
}

read_excel_as_text_df <- function(path) {
  raw <- suppressMessages(
    readxl::read_excel(
      path,
      col_names = FALSE,
      col_types = "text",
      trim_ws = FALSE,
      .name_repair = "minimal"
    )
  )

  raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(raw) == 0) {
    return(raw)
  }

  names(raw) <- paste0("V", seq_len(ncol(raw)))
  raw
}

make_clean_df <- function(raw) {
  out <- as.data.frame(
    lapply(raw, function(col) {
      x <- as.character(col)
      x[is.na(x)] <- ""
      x <- stringr::str_replace_all(x, "\\u00A0", " ")
      x <- stringr::str_squish(x)
      x
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out) <- names(raw)
  out
}

make_norm_df <- function(raw_clean) {
  out <- as.data.frame(
    lapply(raw_clean, function(col) {
      stringr::str_replace_all(as.character(col), "\\s+", "")
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out) <- names(raw_clean)
  out
}

# 파일명에서 B번호, 연도, 병원명, 재무제표 종류 추출
get_file_meta <- function(path) {
  fname <- basename(path)
  fname_no_ext <- stringr::str_remove(fname, stringr::regex("\\.xls[x]?$", ignore_case = TRUE))
  fname_norm <- normalize_key(fname_no_ext)

  hospital_id_from_file <- stringr::str_match(fname, "^B([0-9]+)_")[, 2]
  hospital_id_from_file <- suppressWarnings(as.integer(hospital_id_from_file))

  year <- stringr::str_extract(fname, "20[0-9]{2}")
  year <- suppressWarnings(as.integer(year))

  stmt_name <- dplyr::case_when(
    stringr::str_detect(fname_norm, "재무상태표") ~ "재무상태표",
    stringr::str_detect(fname_norm, "손익계산서") ~ "손익계산서",
    stringr::str_detect(fname_norm, "손익") & stringr::str_detect(fname_norm, "계산") ~ "손익계산서",
    TRUE ~ NA_character_
  )

  stmt_type <- dplyr::case_when(
    stmt_name == "재무상태표" ~ "BS",
    stmt_name == "손익계산서" ~ "IS",
    TRUE ~ NA_character_
  )

  hospital_file_name <- fname |>
    stringr::str_remove(stringr::regex("\\.xls[x]?$", ignore_case = TRUE)) |>
    stringr::str_remove("^(B[0-9]+_)?KHIDI_20[0-9]{2}_") |>
    stringr::str_remove(stringr::regex("_?(재무\\s*상태표|손익\\s*계산서|손익\\s*계산).*$", ignore_case = TRUE)) |>
    stringr::str_replace_all("_+$", "") |>
    stringr::str_squish()

  tibble::tibble(
    file_path = path,
    file_name = fname,
    hospital_id = hospital_id_from_file,
    hospital_file_name = hospital_file_name,
    hospital_key = normalize_key(hospital_file_name),
    file_year = year,
    stmt_type = stmt_type,
    stmt_name = stmt_name,
    file_size = file.info(path)$size,
    file_mtime = file.info(path)$mtime
  )
}

# D1 병원 현황 리스트 읽기
# D1 파일이 없어도 파일명 기준 병원명을 사용합니다.
read_hospital_map <- function(data_dir) {
  d1_files <- list.files(
    data_dir,
    pattern = "B0.*상급종합병원.*현황.*\\.xls[x]?$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(d1_files) == 0) {
    warning("D1 병원 현황 리스트 파일을 찾지 못했습니다. 파일명 기준 병원명을 사용합니다.")
    return(tibble::tibble(
      hospital_id = integer(),
      hospital_name = character(),
      hospital_key = character()
    ))
  }

  raw <- read_excel_as_text_df(d1_files[1])
  raw_clean <- make_clean_df(raw)

  rows <- purrr::map_dfr(seq_len(nrow(raw_clean)), function(i) {
    vals <- as.character(unlist(raw_clean[i, ], use.names = FALSE))
    vals <- stringr::str_squish(vals)
    vals <- vals[!is.na(vals) & vals != ""]

    if (length(vals) == 0) {
      return(NULL)
    }

    id_candidates <- suppressWarnings(as.integer(vals))
    id <- id_candidates[!is.na(id_candidates) & id_candidates >= 1 & id_candidates <= 12]
    hosp <- vals[stringr::str_detect(vals, "병원")]

    if (length(id) == 0 || length(hosp) == 0) {
      return(NULL)
    }

    tibble::tibble(
      hospital_id = id[1],
      hospital_name = hosp[1],
      hospital_key = normalize_key(hosp[1])
    )
  }) |>
    dplyr::distinct(hospital_id, .keep_all = TRUE) |>
    dplyr::arrange(hospital_id)

  if (nrow(rows) == 0) {
    warning("D1 파일은 찾았지만 병원 매칭표를 자동 인식하지 못했습니다. 파일명 기준 병원명을 사용합니다.")
  }

  rows
}

find_statement_layout <- function(raw, raw_clean, raw_norm, path) {
  # '계정과목', '계정 과목', '과목' 헤더 행 찾기
  header_rows <- which(vapply(seq_len(nrow(raw_norm)), function(i) {
    vals <- as.character(unlist(raw_norm[i, ], use.names = FALSE))
    any(vals %in% c("계정과목", "과목"), na.rm = TRUE)
  }, logical(1)))

  if (length(header_rows) == 0) {
    stop("'계정과목' 또는 '과목' 행을 찾지 못했습니다: ", basename(path))
  }

  all_layouts <- purrr::map_dfr(header_rows, function(header_row) {
    header_vals_norm <- as.character(unlist(raw_norm[header_row, ], use.names = FALSE))
    account_cols <- which(header_vals_norm %in% c("계정과목", "과목"))

    purrr::map_dfr(account_cols, function(account_col) {
      candidate_cols <- if (account_col < ncol(raw)) seq.int(account_col + 1, ncol(raw)) else integer(0)
      data_rows <- if (header_row < nrow(raw)) seq.int(header_row + 1, nrow(raw)) else integer(0)

      if (length(candidate_cols) == 0 || length(data_rows) == 0) {
        return(NULL)
      }

      score_tbl <- purrr::map_dfr(candidate_cols, function(j) {
        values <- get_vec(raw, j, data_rows)
        parsed <- parse_amount(values)

        label_rows <- seq.int(max(1, header_row - 4), min(nrow(raw_clean), header_row + 2))
        col_label <- paste(get_vec(raw_clean, j, label_rows), collapse = " ") |>
          stringr::str_squish()

        tibble::tibble(
          col_no = j,
          numeric_n = sum(!is.na(parsed)),
          total_abs = sum(abs(parsed), na.rm = TRUE),
          col_label = col_label,
          has_current_label = stringr::str_detect(col_label, "당\\s*기|제\\s*\\(\\s*당\\s*\\)\\s*기"),
          has_previous_label = stringr::str_detect(col_label, "전\\s*기|제\\s*\\(\\s*전\\s*\\)\\s*기")
        )
      }) |>
        dplyr::filter(numeric_n > 0)

      if (nrow(score_tbl) == 0) {
        return(NULL)
      }

      amount_candidates_strict <- score_tbl |>
        dplyr::filter(numeric_n >= amount_numeric_n_min, total_abs >= amount_abs_min) |>
        dplyr::arrange(col_no)

      amount_candidates <- if (nrow(amount_candidates_strict) > 0) {
        amount_candidates_strict
      } else {
        score_tbl |>
          dplyr::filter(numeric_n >= amount_numeric_n_min) |>
          dplyr::arrange(col_no)
      }

      if (nrow(amount_candidates) == 0) {
        amount_candidates <- score_tbl |>
          dplyr::arrange(col_no)
      }

      tibble::tibble(
        header_row = header_row,
        account_col = account_col,
        candidate_n = nrow(amount_candidates),
        numeric_total = sum(amount_candidates$numeric_n, na.rm = TRUE),
        abs_total = sum(amount_candidates$total_abs, na.rm = TRUE),
        score_tbl = list(score_tbl),
        amount_candidates = list(amount_candidates)
      )
    })
  })

  if (nrow(all_layouts) == 0) {
    stop("금액 열을 찾지 못했습니다: ", basename(path))
  }

  all_layouts |>
    dplyr::arrange(dplyr::desc(candidate_n), dplyr::desc(numeric_total), dplyr::desc(abs_total), header_row, account_col) |>
    dplyr::slice(1)
}

# 엑셀 파일 1개에서 당기와 전기 금액을 long 형태로 추출
read_statement_one <- function(path) {
  meta <- get_file_meta(path)

  if (is.na(meta$file_year) || is.na(meta$stmt_type) || is.na(meta$hospital_file_name)) {
    stop("파일명에서 연도/병원명/재무제표 종류를 추출하지 못했습니다: ", basename(path))
  }

  raw <- read_excel_as_text_df(path)

  if (nrow(raw) == 0 || ncol(raw) == 0) {
    stop("빈 파일입니다: ", basename(path))
  }

  raw_clean <- make_clean_df(raw)
  raw_norm <- make_norm_df(raw_clean)

  layout <- find_statement_layout(raw, raw_clean, raw_norm, path)

  header_row <- layout$header_row[1]
  account_col <- layout$account_col[1]
  amount_candidates <- layout$amount_candidates[[1]] |>
    dplyr::arrange(col_no)

  # 1순위: 라벨로 당기/전기 탐지
  current_col <- amount_candidates |>
    dplyr::filter(has_current_label) |>
    dplyr::arrange(col_no) |>
    dplyr::pull(col_no) |>
    first_int_or_na()

  previous_col <- amount_candidates |>
    dplyr::filter(has_previous_label) |>
    dplyr::arrange(col_no) |>
    dplyr::pull(col_no) |>
    first_int_or_na()

  # 2순위: 좌측 숫자열 = 당기, 우측 숫자열 = 전기
  if (is.na(current_col)) {
    current_col <- first_int_or_na(amount_candidates$col_no)
  }

  if (is.na(previous_col)) {
    previous_col <- first_int_or_na(amount_candidates$col_no[amount_candidates$col_no > current_col])
  }

  # 사용자가 확인한 구조: 좌측이 당기, 우측이 전기
  if (!is.na(previous_col) && previous_col < current_col) {
    cols_sorted <- sort(unique(c(current_col, previous_col)))
    current_col <- cols_sorted[1]
    previous_col <- cols_sorted[2]
  }

  data_rows <- seq.int(header_row + 1, nrow(raw))

  account_vec <- get_vec(raw, account_col, data_rows)
  current_vec <- get_vec(raw, current_col, data_rows)
  previous_vec <- if (!is.na(previous_col)) {
    get_vec(raw, previous_col, data_rows)
  } else {
    rep(NA_character_, length(data_rows))
  }

  base_dat <- tibble::tibble(
    source_row = data_rows,
    account_raw = account_vec,
    current_amount_raw = current_vec,
    previous_amount_raw = previous_vec
  ) |>
    dplyr::mutate(account_name = clean_account(account_raw)) |>
    dplyr::filter(!is.na(account_name), account_name != "") |>
    dplyr::mutate(item_seq = dplyr::row_number())

  current_dat <- base_dat |>
    dplyr::transmute(
      item_seq = item_seq,
      source_row = source_row,
      account_name = account_name,
      amount = parse_amount(current_amount_raw),
      fiscal_year = meta$file_year,
      period_source = "당기",
      period_priority = 1L
    )

  previous_dat <- base_dat |>
    dplyr::transmute(
      item_seq = item_seq,
      source_row = source_row,
      account_name = account_name,
      amount = parse_amount(previous_amount_raw),
      fiscal_year = meta$file_year - 1L,
      period_source = "전기",
      period_priority = 2L
    )

  result <- dplyr::bind_rows(current_dat, previous_dat) |>
    dplyr::filter(!is.na(amount)) |>
    dplyr::mutate(
      file_path = meta$file_path,
      file_name = meta$file_name,
      hospital_id = meta$hospital_id,
      hospital_file_name = meta$hospital_file_name,
      hospital_key = meta$hospital_key,
      file_year = meta$file_year,
      stmt_type = meta$stmt_type,
      stmt_name = meta$stmt_name,
      current_amount_col_no = current_col,
      previous_amount_col_no = previous_col,
      .before = 1
    )

  if (nrow(result) == 0) {
    stop("계정과목과 금액을 추출하지 못했습니다: ", basename(path))
  }

  result
}

# long 데이터를 wide 데이터셋과 분류ID-계정과목 매칭표로 변환
make_wide_dataset <- function(long_resolved, stmt_type_value, prefix, statement_label) {
  dat <- long_resolved |>
    dplyr::filter(stmt_type == stmt_type_value) |>
    dplyr::arrange(hospital_id, fiscal_year, item_seq)

  if (nrow(dat) == 0) {
    stop(statement_label, " 자료가 없습니다.")
  }

  codebook <- dat |>
    dplyr::group_by(item_seq) |>
    dplyr::summarise(
      대표계정과목 = dplyr::first(account_name),
      계정과목_변형개수 = dplyr::n_distinct(account_name),
      계정과목_변형목록 = paste(sort(unique(account_name)), collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::arrange(item_seq) |>
    dplyr::mutate(
      분류ID = sprintf("%s_%03d", prefix, item_seq),
      재무제표종류 = statement_label,
      계정순번 = item_seq
    ) |>
    dplyr::select(
      재무제표종류,
      분류ID,
      계정순번,
      대표계정과목,
      계정과목_변형개수,
      계정과목_변형목록
    )

  wide <- dat |>
    dplyr::left_join(
      codebook |> dplyr::select(계정순번, 분류ID),
      by = c("item_seq" = "계정순번")
    ) |>
    dplyr::select(hospital_id, hospital_name, fiscal_year, 분류ID, amount) |>
    tidyr::pivot_wider(
      names_from = 분류ID,
      values_from = amount,
      values_fn = list(amount = dplyr::first)
    ) |>
    dplyr::arrange(hospital_id, fiscal_year) |>
    dplyr::rename(year = fiscal_year)

  list(wide = wide, codebook = codebook)
}

# SPSS 변수 라벨 부여
add_spss_labels <- function(df, codebook) {
  out <- df

  if ("hospital_id" %in% names(out)) {
    attr(out$hospital_id, "label") <- "병원 ID"
  }
  if ("hospital_name" %in% names(out)) {
    attr(out$hospital_name, "label") <- "병원명"
  }
  if ("year" %in% names(out)) {
    attr(out$year, "label") <- "회계연도"
  }

  for (i in seq_len(nrow(codebook))) {
    var_name <- codebook$분류ID[i]
    var_label <- paste0(codebook$재무제표종류[i], " | ", codebook$대표계정과목[i])

    if (var_name %in% names(out)) {
      attr(out[[var_name]], "label") <- substr(var_label, 1, 250)
    }
  }

  out
}

# 3) 파일 탐색 ---------------------------------------------------------------
all_excel_files <- list.files(
  data_dir,
  pattern = "\\.xls[x]?$",
  full.names = TRUE,
  ignore.case = TRUE
)

all_excel_files <- all_excel_files[!grepl("^~\\$", basename(all_excel_files))]

statement_files <- all_excel_files[
  grepl("KHIDI", basename(all_excel_files), ignore.case = TRUE) &
    grepl("20[0-9]{2}", basename(all_excel_files))
]

if (length(statement_files) == 0) {
  stop("KHIDI 재무제표 파일을 찾지 못했습니다. data_dir 경로와 파일명을 확인하세요.")
}

file_meta <- purrr::map_dfr(statement_files, get_file_meta)

unclassified_files <- file_meta |>
  dplyr::filter(is.na(stmt_type) | is.na(file_year) | is.na(hospital_id))

if (nrow(unclassified_files) > 0) {
  message("아래 파일은 병원ID/연도/재무제표 종류를 인식하지 못했습니다.")
  print(unclassified_files |> dplyr::select(file_name, hospital_id, file_year, stmt_type))
  stop("파일명 형식을 확인하세요. 예: B1_KHIDI_2020_서울대학교병원_손익계산서.xls")
}

message("전체 Excel 파일 수: ", length(all_excel_files))
message("처음 인식된 KHIDI 재무제표 파일 수: ", nrow(file_meta))
message("처음 인식된 재무상태표 파일 수: ", sum(file_meta$stmt_type == "BS", na.rm = TRUE))
message("처음 인식된 손익계산서 파일 수: ", sum(file_meta$stmt_type == "IS", na.rm = TRUE))

# 4) 병원 매칭표 결합 --------------------------------------------------------
hospital_map <- read_hospital_map(data_dir)

file_meta <- file_meta |>
  dplyr::left_join(
    hospital_map |> dplyr::select(hospital_id, hospital_name_from_D1 = hospital_name),
    by = "hospital_id"
  ) |>
  dplyr::mutate(
    hospital_name = dplyr::coalesce(hospital_name_from_D1, hospital_file_name)
  ) |>
  dplyr::select(-hospital_name_from_D1)

# 5) 동일 병원-파일연도-재무제표 종류 중복 파일 처리 -------------------------
duplicated_files <- file_meta |>
  dplyr::count(stmt_type, hospital_id, hospital_name, file_year, name = "n") |>
  dplyr::filter(n > 1)

if (nrow(duplicated_files) > 0) {
  message("동일 병원-연도-재무제표 종류의 중복 파일이 발견되었습니다.")

  duplicate_detail <- file_meta |>
    dplyr::semi_join(
      duplicated_files,
      by = c("stmt_type", "hospital_id", "hospital_name", "file_year")
    ) |>
    dplyr::arrange(stmt_type, hospital_id, file_year, dplyr::desc(file_mtime), dplyr::desc(file_size)) |>
    dplyr::select(stmt_type, hospital_id, hospital_name, file_year, file_name, file_size, file_mtime)

  print(duplicate_detail)

  if (duplicate_file_policy == "stop") {
    stop("중복 파일이 있어 중단했습니다. 위에 출력된 파일명을 확인하세요.")
  }

  if (duplicate_file_policy == "latest") {
    message("중복 그룹마다 수정시간이 가장 최신인 파일 1개만 사용합니다.")

    file_meta <- file_meta |>
      dplyr::group_by(stmt_type, hospital_id, hospital_name, file_year) |>
      dplyr::arrange(dplyr::desc(file_mtime), dplyr::desc(file_size), .by_group = TRUE) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
  }
}

# 6) 12개 병원 × 5년 × 2종류 = 120개 조합 확인 ------------------------------
expected_grid <- tidyr::expand_grid(
  hospital_id = expected_hospital_ids,
  file_year = target_years,
  stmt_type = c("BS", "IS")
) |>
  dplyr::mutate(
    stmt_name = dplyr::case_when(
      stmt_type == "BS" ~ "재무상태표",
      stmt_type == "IS" ~ "손익계산서",
      TRUE ~ NA_character_
    )
  )

present_grid <- file_meta |>
  dplyr::distinct(hospital_id, file_year, stmt_type)

missing_expected <- expected_grid |>
  dplyr::anti_join(present_grid, by = c("hospital_id", "file_year", "stmt_type")) |>
  dplyr::left_join(hospital_map |> dplyr::select(hospital_id, hospital_name), by = "hospital_id") |>
  dplyr::arrange(stmt_type, hospital_id, file_year)

if (nrow(missing_expected) > 0) {
  message("120개 조합 중 누락된 병원-연도-재무제표 종류가 있습니다.")
  print(missing_expected |> dplyr::select(hospital_id, hospital_name, file_year, stmt_name))
  stop(
    "현재 인식된 파일로는 완전한 12개 병원 × 5년 × 2종류 데이터셋을 만들 수 없습니다. ",
    "위에 출력된 누락 파일의 실제 파일명 또는 위치를 확인하세요."
  )
}

statement_files <- file_meta$file_path

# 7) 파일 읽기: 당기와 전기 모두 추출 ----------------------------------------
long_all_periods <- purrr::map_dfr(statement_files, function(p) {
  tryCatch(
    read_statement_one(p),
    error = function(e) {
      stop("파일 읽기 실패: ", basename(p), "\n", conditionMessage(e), call. = FALSE)
    }
  )
}) |>
  dplyr::left_join(
    file_meta |> dplyr::select(file_path, hospital_name),
    by = "file_path"
  ) |>
  dplyr::select(
    hospital_id,
    hospital_name,
    fiscal_year,
    period_source,
    period_priority,
    file_year,
    stmt_type,
    stmt_name,
    item_seq,
    account_name,
    amount,
    file_name,
    file_path
  ) |>
  dplyr::arrange(stmt_type, hospital_id, fiscal_year, item_seq, period_priority)

# 8) 전기/당기 중복 해결 -----------------------------------------------------
# 같은 병원-같은 연도-같은 재무제표-같은 계정순번 값이 여러 개 있으면:
#   1순위: 해당 연도 파일의 당기값
#   2순위: 다음 연도 파일의 전기값
long_resolved <- long_all_periods |>
  dplyr::filter(fiscal_year %in% target_years) |>
  dplyr::group_by(hospital_id, hospital_name, fiscal_year, stmt_type, stmt_name, item_seq) |>
  dplyr::arrange(period_priority, dplyr::desc(file_year), .by_group = TRUE) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::arrange(stmt_type, hospital_id, fiscal_year, item_seq)

# 전기와 다음 연도 당기의 값 차이가 있는지 콘솔에만 표시합니다.
# 최종 파일 수를 6개로 유지하기 위해 별도 검증 파일은 생성하지 않습니다.
overlap_mismatch_n <- long_all_periods |>
  dplyr::filter(fiscal_year %in% target_years) |>
  dplyr::group_by(hospital_id, fiscal_year, stmt_type, item_seq) |>
  dplyr::summarise(
    current_values = list(unique(amount[period_source == "당기"])),
    previous_values = list(unique(amount[period_source == "전기"])),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    has_current = purrr::map_lgl(current_values, ~ length(.x) > 0),
    has_previous = purrr::map_lgl(previous_values, ~ length(.x) > 0),
    current_first = purrr::map_dbl(current_values, ~ if (length(.x) == 0) NA_real_ else .x[1]),
    previous_first = purrr::map_dbl(previous_values, ~ if (length(.x) == 0) NA_real_ else .x[1]),
    mismatch = has_current & has_previous & !is.na(current_first) & !is.na(previous_first) & current_first != previous_first
  ) |>
  dplyr::filter(mismatch) |>
  nrow()

# 9) wide 데이터셋과 분류ID-계정 매칭표 생성 --------------------------------
bs_result <- make_wide_dataset(
  long_resolved = long_resolved,
  stmt_type_value = "BS",
  prefix = "bs",
  statement_label = "재무상태표"
)

is_result <- make_wide_dataset(
  long_resolved = long_resolved,
  stmt_type_value = "IS",
  prefix = "is",
  statement_label = "손익계산서"
)

balance_sheet_wide <- bs_result$wide
income_statement_wide <- is_result$wide
balance_sheet_codebook <- bs_result$codebook
income_statement_codebook <- is_result$codebook

# 10) 최종 6개 파일 저장 -----------------------------------------------------
readr::write_excel_csv(
  balance_sheet_wide,
  file.path(out_dir, "KHIDI_재무상태표_통합.csv")
)

readr::write_excel_csv(
  income_statement_wide,
  file.path(out_dir, "KHIDI_손익계산서_통합.csv")
)

readr::write_excel_csv(
  balance_sheet_codebook,
  file.path(out_dir, "KHIDI_재무상태표_분류ID_계정매칭.csv")
)

readr::write_excel_csv(
  income_statement_codebook,
  file.path(out_dir, "KHIDI_손익계산서_분류ID_계정매칭.csv")
)

haven::write_sav(
  add_spss_labels(balance_sheet_wide, balance_sheet_codebook),
  file.path(out_dir, "KHIDI_재무상태표_통합.sav")
)

haven::write_sav(
  add_spss_labels(income_statement_wide, income_statement_codebook),
  file.path(out_dir, "KHIDI_손익계산서_통합.sav")
)

# 11) 콘솔 검증 메시지 -------------------------------------------------------
output_files <- list.files(out_dir, full.names = FALSE)

message("완료되었습니다.")
message("출력 폴더: ", out_dir)
message("최종 출력 파일 수: ", length(output_files))
message(paste(output_files, collapse = "\n"))
message("사용 파일 수: ", nrow(file_meta))
message("재무상태표 사용 파일 수: ", sum(file_meta$stmt_type == "BS", na.rm = TRUE))
message("손익계산서 사용 파일 수: ", sum(file_meta$stmt_type == "IS", na.rm = TRUE))
message("재무상태표 통합 데이터 크기: ", nrow(balance_sheet_wide), "행 x ", ncol(balance_sheet_wide), "열")
message("손익계산서 통합 데이터 크기: ", nrow(income_statement_wide), "행 x ", ncol(income_statement_wide), "열")
message("재무상태표 분류ID 수: ", nrow(balance_sheet_codebook))
message("손익계산서 분류ID 수: ", nrow(income_statement_codebook))
message("전기-당기 겹침 값 불일치 건수: ", overlap_mismatch_n)

# 정상 기대값:
#   - 사용 파일 수: 120
#   - 재무상태표 사용 파일 수: 60
#   - 손익계산서 사용 파일 수: 60
#   - 재무상태표 통합 데이터 크기: 60행 x ...열
#   - 손익계산서 통합 데이터 크기: 60행 x ...열
#   - 최종 출력 파일 수: 6
