# 부록 M. R 핵심 분석 코드

# R 전체 전문에서 그림 스타일 조정, 전체 저장 코드, 보조 출력 일부를 덜어낸 버전이다. 이 코드만 실행해도 논문 핵심 수치인 M5 회귀결과, VIF, Cook’s distance, 민감도 분석 핵심계수를 재현할 수 있다.

library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(sandwich)
library(car)
library(writexl)

base_dir <- getwd()

file_analysis <- file.path(
  base_dir,
  "9차_기술통계_상관분석_회귀분석_투입변수_최종선정.xlsx"
)

file_sensitivity <- file.path(
  base_dir,
  "11차_민감도_분석.xlsx"
)

out_file <- file.path(
  base_dir,
  "R_core_reproduced_results.xlsx"
)


# 1. 분석자료 불러오기
df <- read_excel(
  file_analysis,
  sheet = "Analysis_Data_Used",
  skip = 2
) %>%
  mutate(
    hospital_id_code = as.factor(hospital_id_code),
    patient_exp_year = as.numeric(patient_exp_year),
    year_2023_dummy = as.numeric(year_2023_dummy),
    metro_area = as.numeric(metro_area)
  )

if (nrow(df) != 94) stop("최종 분석 관측치 수가 94가 아닙니다.")
if (n_distinct(df$hospital_id_code) != 47) stop("병원 수가 47이 아닙니다.")


# 2. 회귀결과 정리 함수
cluster_tidy <- function(model, data, cluster_var = "hospital_id_code") {
  used_rows <- as.integer(row.names(model.frame(model)))
  cluster <- data[[cluster_var]][used_rows]

  vc <- vcovCL(model, cluster = cluster, type = "HC1")
  beta <- coef(model)
  se <- sqrt(diag(vc))

  g <- length(unique(cluster))
  test_df <- g - 1

  t_value <- beta / se
  p_value <- 2 * pt(abs(t_value), df = test_df, lower.tail = FALSE)
  ci_low <- beta - qt(0.975, df = test_df) * se
  ci_high <- beta + qt(0.975, df = test_df) * se

  tibble(
    term = names(beta),
    estimate = as.numeric(beta),
    std_error = as.numeric(se),
    t_value = as.numeric(t_value),
    p_value = as.numeric(p_value),
    ci95_low = as.numeric(ci_low),
    ci95_high = as.numeric(ci_high),
    n = nobs(model),
    cluster_n = g,
    df = test_df
  )
}

hc3_tidy <- function(model) {
  vc <- vcovHC(model, type = "HC3")
  beta <- coef(model)
  se <- sqrt(diag(vc))
  test_df <- df.residual(model)

  t_value <- beta / se
  p_value <- 2 * pt(abs(t_value), df = test_df, lower.tail = FALSE)
  ci_low <- beta - qt(0.975, df = test_df) * se
  ci_high <- beta + qt(0.975, df = test_df) * se

  tibble(
    term = names(beta),
    estimate = as.numeric(beta),
    std_error = as.numeric(se),
    t_value = as.numeric(t_value),
    p_value = as.numeric(p_value),
    ci95_low = as.numeric(ci_low),
    ci95_high = as.numeric(ci_high),
    n = nobs(model),
    df = test_df
  )
}


# 3. 기준 통합모형 M5
m5 <- lm(
  patient_experience_index ~
    main_medical_income_margin_pct +
    main_labor_cost_ratio_pct +
    latest_quality_index +
    log_total_beds +
    doctors_per_100_beds +
    nursing_grade_main_num +
    equipment_per_100_beds +
    metro_area +
    year_2023_dummy,
  data = df
)

m5_cluster <- cluster_tidy(m5, df) %>%
  mutate(model = "M5_cluster")

m5_hc3 <- hc3_tidy(m5) %>%
  mutate(model = "M5_HC3")

m5_fit <- tibble(
  model = "M5",
  n = nobs(m5),
  r_squared = summary(m5)$r.squared,
  adj_r_squared = summary(m5)$adj.r.squared,
  residual_df = df.residual(m5)
)


# 4. VIF
m5_vif <- vif(m5)

m5_vif_table <- tibble(
  variable = names(m5_vif),
  vif = as.numeric(m5_vif)
)


# 5. 회귀진단
m5_influence <- df %>%
  mutate(
    fitted_value = fitted(m5),
    residual = resid(m5),
    leverage = hatvalues(m5),
    std_residual = rstandard(m5),
    cooks_d = cooks.distance(m5),
    cooks_threshold = 4 / nobs(m5),
    leverage_threshold = 2 * length(coef(m5)) / nobs(m5),
    flag_cook = cooks_d > cooks_threshold,
    flag_leverage = leverage > leverage_threshold,
    flag_std_resid = abs(std_residual) > 2
  ) %>%
  select(
    analysis_row_id,
    hospital_id_code,
    standard_hospital_name,
    patient_exp_year,
    patient_experience_index,
    fitted_value,
    residual,
    leverage,
    std_residual,
    cooks_d,
    cooks_threshold,
    leverage_threshold,
    flag_cook,
    flag_leverage,
    flag_std_resid
  )

diagnostic_summary <- tibble(
  item = c(
    "n",
    "k_including_intercept",
    "cook_threshold",
    "leverage_threshold",
    "cook_exceed_count",
    "leverage_exceed_count",
    "std_resid_abs_gt_2_count"
  ),
  value = c(
    nobs(m5),
    length(coef(m5)),
    4 / nobs(m5),
    2 * length(coef(m5)) / nobs(m5),
    sum(m5_influence$flag_cook),
    sum(m5_influence$flag_leverage),
    sum(m5_influence$flag_std_resid)
  )
)


# 6. 민감도 분석자료 불러오기
sens_df <- read_excel(
  file_sensitivity,
  sheet = "Regression_Data_Used",
  skip = 2
) %>%
  mutate(
    hospital_id_code = as.factor(hospital_id_code),
    patient_exp_year = as.numeric(patient_exp_year),
    year_2023_dummy = as.numeric(year_2023_dummy),
    metro_area = as.numeric(metro_area)
  )


# 7. 민감도 분석 함수
run_sensitivity <- function(model_id, model_name, data, formula, se_type) {
  model <- lm(formula, data = data)

  if (se_type == "cluster") {
    tab <- cluster_tidy(model, data)
  } else {
    tab <- hc3_tidy(model)
  }

  fit <- tibble(
    model_id = model_id,
    model_name = model_name,
    n = nobs(model),
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    se_type = se_type
  )

  list(
    fit = fit,
    coef = tab %>%
      mutate(
        model_id = model_id,
        model_name = model_name,
        se_type = se_type
      )
  )
}


cook_exclude_ids <- m5_influence %>%
  filter(flag_cook) %>%
  pull(analysis_row_id)

sensitivity_list <- list(
  S00 = list(
    name = "기준 통합모형",
    data = sens_df,
    formula = patient_experience_index ~
      main_medical_income_margin_pct +
      main_labor_cost_ratio_pct +
      latest_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area +
      year_2023_dummy,
    se = "cluster"
  ),
  S01 = list(
    name = "2021년 단면분석",
    data = sens_df %>% filter(patient_exp_year == 2021),
    formula = patient_experience_index ~
      main_medical_income_margin_pct +
      main_labor_cost_ratio_pct +
      latest_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area,
    se = "hc3"
  ),
  S02 = list(
    name = "2023년 단면분석",
    data = sens_df %>% filter(patient_exp_year == 2023),
    formula = patient_experience_index ~
      main_medical_income_margin_pct +
      main_labor_cost_ratio_pct +
      latest_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area,
    se = "hc3"
  ),
  S03 = list(
    name = "동일연도 재무자료",
    data = sens_df,
    formula = patient_experience_index ~
      same_medical_income_margin_pct +
      same_labor_cost_ratio_pct +
      latest_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area +
      year_2023_dummy,
    se = "cluster"
  ),
  S04 = list(
    name = "2개년 평균 재무자료",
    data = sens_df,
    formula = patient_experience_index ~
      avg2_medical_income_margin_pct +
      avg2_labor_cost_ratio_pct +
      latest_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area +
      year_2023_dummy,
    se = "cluster"
  ),
  S05 = list(
    name = "2020~2024년 평균 재무자료",
    data = sens_df,
    formula = patient_experience_index ~
      avg2020_2024_medical_income_margin_pct +
      avg2020_2024_labor_cost_ratio_pct +
      latest_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area +
      year_2023_dummy,
    se = "cluster"
  ),
  S06 = list(
    name = "보수적 의료질지수 전체 관측치",
    data = sens_df,
    formula = patient_experience_index ~
      main_medical_income_margin_pct +
      main_labor_cost_ratio_pct +
      conservative_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area +
      year_2023_dummy,
    se = "cluster"
  ),
  S07 = list(
    name = "2023년 보수적 의료질지수 단면",
    data = sens_df %>% filter(patient_exp_year == 2023),
    formula = patient_experience_index ~
      main_medical_income_margin_pct +
      main_labor_cost_ratio_pct +
      conservative_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area,
    se = "hc3"
  ),
  S08 = list(
    name = "Cook's distance 영향점 제외",
    data = sens_df %>% filter(!(analysis_row_id %in% cook_exclude_ids)),
    formula = patient_experience_index ~
      main_medical_income_margin_pct +
      main_labor_cost_ratio_pct +
      latest_quality_index +
      log_total_beds +
      doctors_per_100_beds +
      nursing_grade_main_num +
      equipment_per_100_beds +
      metro_area +
      year_2023_dummy,
    se = "cluster"
  )
)

sensitivity_results <- imap(
  sensitivity_list,
  ~run_sensitivity(
    model_id = .y,
    model_name = .x$name,
    data = .x$data,
    formula = .x$formula,
    se_type = .x$se
  )
)

sensitivity_fit <- map_dfr(sensitivity_results, "fit")
sensitivity_coef <- map_dfr(sensitivity_results, "coef")

key_terms <- c(
  "main_medical_income_margin_pct",
  "main_labor_cost_ratio_pct",
  "same_medical_income_margin_pct",
  "same_labor_cost_ratio_pct",
  "avg2_medical_income_margin_pct",
  "avg2_labor_cost_ratio_pct",
  "avg2020_2024_medical_income_margin_pct",
  "avg2020_2024_labor_cost_ratio_pct",
  "latest_quality_index",
  "conservative_quality_index"
)

sensitivity_key_coef <- sensitivity_coef %>%
  filter(term %in% key_terms)


# 8. 핵심 수치 검산
check_m5_quality <- m5_cluster %>%
  filter(term == "latest_quality_index")

check_table <- tibble(
  item = c(
    "M5_adj_R2",
    "M5_latest_quality_coef",
    "M5_latest_quality_p",
    "Cook_threshold",
    "Cook_exceed_count",
    "Leverage_exceed_count"
  ),
  actual = c(
    m5_fit$adj_r_squared,
    check_m5_quality$estimate,
    check_m5_quality$p_value,
    4 / nobs(m5),
    sum(m5_influence$flag_cook),
    sum(m5_influence$flag_leverage)
  ),
  expected = c(
    0.277351,
    13.581708,
    0.000382,
    4 / 94,
    5,
    4
  )
)

check_table <- check_table %>%
  mutate(pass = abs(actual - expected) < 0.0005)

if (!all(check_table$pass)) {
  print(check_table)
  stop("핵심 수치 검산 실패")
}


# 9. 결과 저장
write_xlsx(
  list(
    check_table = check_table,
    M5_fit = m5_fit,
    M5_cluster = m5_cluster,
    M5_HC3 = m5_hc3,
    M5_VIF = m5_vif_table,
    diagnostic_summary = diagnostic_summary,
    influence_all = m5_influence,
    influence_cook_exceed = m5_influence %>% filter(flag_cook),
    influence_leverage_exceed = m5_influence %>% filter(flag_leverage),
    sensitivity_fit = sensitivity_fit,
    sensitivity_key_coef = sensitivity_key_coef
  ),
  path = out_file
)

print("R 핵심 분석 재현 완료")
print(check_table)