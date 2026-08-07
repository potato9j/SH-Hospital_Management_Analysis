# Local path configuration for R scripts.
# Edit PROJECT_DIR only. Scripts are designed to run from RStudio without bash arguments.

PROJECT_DIR <- "D:/hospital_management_analysis"
RAW_DIR     <- file.path(PROJECT_DIR, "data", "raw")
STAGE_DIR   <- file.path(PROJECT_DIR, "outputs", "nth_excels")
RESULT_DIR  <- file.path(PROJECT_DIR, "outputs", "results")
FIGURE_DIR  <- file.path(PROJECT_DIR, "outputs", "figures")

dir.create(STAGE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

FILES <- list(
  quality_2021 = file.path(RAW_DIR, "C2_HIRA_2021_세부적정성평가_47개병원_전체평가항목.xlsx"),
  quality_2023 = file.path(RAW_DIR, "C2_HIRA_2023_세부적정성평가_47개병원_전체평가항목.xls"),
  analysis_variables = file.path(STAGE_DIR, "9차_기술통계_상관분석_회귀분석_투입변수_최종선정.xlsx"),
  sensitivity_old = file.path(STAGE_DIR, "11차_민감도_분석.xlsx"),
  final_results = file.path(RESULT_DIR, "hospital_management_full_numeric_results.xlsx")
)

VARS <- list(
  hospital_id = "hospital_id_code",
  year = "patient_exp_year",
  y = "patient_experience_index",
  finance_main = "main_medical_income_margin_pct",
  cost_main = "main_labor_cost_ratio_pct",
  quality_main = "year_matched_quality_index",
  quality_latest = "latest_quality_index",
  beds_log = "log_total_beds",
  doctors_100 = "doctors_per_100_beds",
  nursing_grade = "nursing_grade_main_num",
  equipment_100 = "equipment_per_100_beds",
  metro = "metro_area",
  year_dummy = "year_2023_dummy"
)

MODEL_M5_X <- c(
  VARS$finance_main, VARS$cost_main, VARS$quality_main,
  VARS$beds_log, VARS$doctors_100, VARS$nursing_grade,
  VARS$equipment_100, VARS$metro, VARS$year_dummy
)
