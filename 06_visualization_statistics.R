# ============================================================
# 부록 L. R 최종 분석 코드 (오류 수정 및 패키지 설치 포함)
# ============================================================

# 0. 필요한 패키지 설치 및 로드
required_pkgs <- c("readxl", "dplyr", "lmtest", "sandwich", "car", "ggplot2")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(missing_pkgs)) install.packages(missing_pkgs)

library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(ggplot2)

# 0-1. 작업 폴더 설정 (엑셀 파일이 있는 경로로 반드시 수정하세요)
# 예: setwd("C:/R_data")
setwd("C:/Users/potat/Desktop/R_data") 

# 1. 분석자료 불러오기
analysis_df <- read_excel(
  "9차_기술통계_상관분석_회귀분석_투입변수_최종선정.xlsx",
  sheet = "Analysis_Data_Used",
  skip = 2
)

# 2. 변수 형식 정리
analysis_df <- analysis_df %>%
  mutate(
    patient_exp_year = as.numeric(patient_exp_year),
    year_2023_dummy = ifelse(patient_exp_year == 2023, 1, 0),
    hospital_id_code = as.factor(hospital_id_code)
  )

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
  data = analysis_df
)

# 4. OLS 결과 확인
summary(m5)

# 5. HC3 강건표준오차 확인
m5_hc3 <- coeftest(
  m5,
  vcov. = vcovHC(m5, type = "HC3")
)
print(m5_hc3)

# 6. 병원 단위 군집표준오차 확인
m5_cluster <- coeftest(
  m5,
  vcov. = vcovCL(m5, cluster = analysis_df$hospital_id_code, type = "HC1")
)
print(m5_cluster)

# 7. VIF 다중공선성 진단
m5_vif <- car::vif(m5)
print(m5_vif)

# 8. 영향점 진단
m5_cook <- cooks.distance(m5)
m5_leverage <- hatvalues(m5)
m5_std_resid <- rstandard(m5)

cook_threshold <- 4 / nobs(m5)
leverage_threshold <- 2 * length(coef(m5)) / nobs(m5)

influence_df <- analysis_df %>%
  mutate(
    fitted = fitted(m5),
    residual = resid(m5),
    cooks_d = m5_cook,
    leverage = m5_leverage,
    std_resid = m5_std_resid,
    flag_cook = ifelse(cooks_d > cook_threshold, 1, 0),
    flag_leverage = ifelse(leverage > leverage_threshold, 1, 0),
    flag_std_resid = ifelse(abs(std_resid) > 2, 1, 0)
  )

# 9. 영향점 제외 민감도 분석
df_no_influence <- influence_df %>%
  filter(flag_cook == 0)

m5_no_influence <- lm(
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
  data = df_no_influence
)

# 영향점 제외 후 강건표준오차 결과 확인
coeftest(
  m5_no_influence,
  vcov. = vcovHC(m5_no_influence, type = "HC3")
)