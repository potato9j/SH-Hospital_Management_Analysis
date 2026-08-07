# Regression, standard-error correction, VIF and influence diagnostics.
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
source(file.path(script_dir, "00_local_paths.R"))

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(broom); library(sandwich)
  library(lmtest); library(car); library(writexl)
})
read_analysis_data <- function() {
  # Prefer the final result workbook if it exists; it contains the revised panel.
  if (file.exists(FILES$final_results)) {
    readxl::read_excel(FILES$final_results, sheet = "analysis_data_revised")
  } else {
    readxl::read_excel(FILES$analysis_variables, sheet = "Analysis_Data_Used", skip = 2)
  }
}

cluster_tidy <- function(model, cluster) {
  vc <- sandwich::vcovCL(model, cluster = cluster, type = "HC1")
  ct <- lmtest::coeftest(model, vcov. = vc)
  tibble::tibble(
    term = rownames(ct),
    coef = ct[, 1], std_error = ct[, 2], t_value = ct[, 3], p_value = ct[, 4]
  )
}

hc3_tidy <- function(model) {
  vc <- sandwich::vcovHC(model, type = "HC3")
  ct <- lmtest::coeftest(model, vcov. = vc)
  tibble::tibble(
    term = rownames(ct),
    coef = ct[, 1], std_error = ct[, 2], t_value = ct[, 3], p_value = ct[, 4]
  )
}

run_regression_diagnostics <- function() {
  df <- read_analysis_data() %>% mutate(year_2023_dummy = as.integer(patient_exp_year == 2023))
  f <- as.formula(paste(VARS$y, "~", paste(MODEL_M5_X, collapse = " + ")))
  m5 <- lm(f, data = df)
  used <- model.frame(m5)
  row_id <- as.integer(rownames(used))
  cluster <- df[[VARS$hospital_id]][row_id]

  coef_ols <- broom::tidy(m5)
  coef_hc3 <- hc3_tidy(m5)
  coef_cluster <- cluster_tidy(m5, cluster)
  vif_tbl <- tibble::tibble(variable = names(car::vif(m5)), VIF = as.numeric(car::vif(m5)))

  infl <- influence.measures(m5)
  cooks <- cooks.distance(m5)
  lev <- hatvalues(m5)
  std_res <- rstandard(m5)
  influence_all <- df[row_id, ] %>%
    mutate(.fitted = fitted(m5), .resid = resid(m5), leverage = lev,
           standardized_residual = std_res, cooks_distance = cooks)
  diag_summary <- tibble::tibble(
    item = c("N", "k_including_intercept", "cook_threshold_4_over_n", "leverage_threshold_2k_over_n"),
    value = c(nrow(used), length(coef(m5)), 4 / nrow(used), 2 * length(coef(m5)) / nrow(used))
  )

  out <- list(
    M5_OLS = coef_ols,
    M5_HC3 = coef_hc3,
    M5_Cluster = coef_cluster,
    M5_VIF = vif_tbl,
    M5_Diagnostics_Summary = diag_summary,
    M5_Influence_All = influence_all
  )
  writexl::write_xlsx(out, file.path(RESULT_DIR, "regression_robust_diagnostics_results.xlsx"))
  invisible(out)
}

run_regression_diagnostics()
