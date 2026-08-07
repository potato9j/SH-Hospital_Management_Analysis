# Sensitivity analysis, Random Effects, Two-Way Fixed Effects, and Hausman test.
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
source(file.path(script_dir, "00_local_paths.R"))

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(plm); library(lmtest)
  library(sandwich); library(writexl); library(broom)
})
read_analysis_data <- function() {
  readxl::read_excel(FILES$final_results, sheet = "analysis_data_revised") %>%
    mutate(year_2023_dummy = as.integer(patient_exp_year == 2023))
}

lm_cluster <- function(df, y, xvars) {
  f <- as.formula(paste(y, "~", paste(xvars, collapse = " + ")))
  m <- lm(f, data = df)
  row_id <- as.integer(rownames(model.frame(m)))
  vc <- sandwich::vcovCL(m, cluster = df$hospital_id_code[row_id], type = "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc)
  tibble::tibble(term = rownames(ct), coef = ct[, 1], std_error = ct[, 2], p_value = ct[, 4])
}

run_sensitivity_and_panel <- function() {
  df <- read_analysis_data()
  y <- VARS$y
  base_controls <- c(VARS$beds_log, VARS$doctors_100, VARS$nursing_grade, VARS$equipment_100, VARS$metro, VARS$year_dummy)
  x_m5 <- MODEL_M5_X

  sensitivity_specs <- list(
    S00 = list(data = df, x = x_m5, label = "기준 통합모형"),
    S06 = list(data = df, x = c(VARS$finance_main, VARS$cost_main, VARS$quality_latest, base_controls), label = "최신가용 의료질지수 보조모형"),
    S09 = list(data = df, x = c(x_m5, "year_matched_valid_item_count"), label = "유효 평가항목 수 추가 통제"),
    S10 = list(data = df, x = c(VARS$finance_main, VARS$cost_main, "year_matched_common12_index", base_controls), label = "공통항목 제한"),
    S11 = list(data = df, x = c(VARS$finance_main, VARS$cost_main, "year_matched_high20_index", base_controls), label = "고커버리지 항목 제한"),
    S12 = list(data = df, x = c(VARS$finance_main, VARS$cost_main, "year_matched_balanced_category_index", base_controls), label = "대분류 균형가중")
  )

  sens_coef <- bind_rows(lapply(names(sensitivity_specs), function(id) {
    spec <- sensitivity_specs[[id]]
    lm_cluster(spec$data, y, spec$x) %>% mutate(ID = id, model = spec$label, .before = 1)
  }))

  # Panel models
  panel_df <- df %>%
    select(all_of(c(VARS$hospital_id, VARS$year, y, VARS$finance_main, VARS$cost_main, VARS$quality_main))) %>%
    tidyr::drop_na()
  pdata <- pdata.frame(panel_df, index = c(VARS$hospital_id, VARS$year))
  pf <- as.formula(paste(y, "~", paste(c(VARS$finance_main, VARS$cost_main, VARS$quality_main), collapse = " + ")))
  re <- plm::plm(pf, data = pdata, model = "random")
  twfe <- plm::plm(pf, data = pdata, model = "within", effect = "twoways")
  haus <- plm::phtest(twfe, re)

  re_tbl <- broom::tidy(re) %>% mutate(model = "Random Effects", .before = 1)
  twfe_tbl <- broom::tidy(twfe) %>% mutate(model = "Two-Way Fixed Effects", .before = 1)
  haus_tbl <- tibble::tibble(test = "Hausman Test", statistic = as.numeric(haus$statistic), df = as.numeric(haus$parameter), p_value = as.numeric(haus$p.value))

  out <- list(
    Sensitivity_Coefficients = sens_coef,
    Panel_Random_Effects = re_tbl,
    Panel_TwoWay_FE = twfe_tbl,
    Panel_Hausman = haus_tbl
  )
  writexl::write_xlsx(out, file.path(RESULT_DIR, "sensitivity_panel_results.xlsx"))
  invisible(out)
}

run_sensitivity_and_panel()
