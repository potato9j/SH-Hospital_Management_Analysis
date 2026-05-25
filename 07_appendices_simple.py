# 부록 L. Python 재현자료 생성 코드

## 원자료 전체 파싱용이 아닌, 이미 구축된 n차 산출물을 기준으로 최종 분석자료가 논문 구조와 맞는지 확인하고, R/SPSS 검산에 쓸 clean dataset을 만드는 코드다.

from pathlib import Path
import pandas as pd
import numpy as np

BASE = Path(".").resolve()

FILE_ANALYSIS = BASE / "9차_기술통계_상관분석_회귀분석_투입변수_최종선정.xlsx"
FILE_SENS = BASE / "11차_민감도_분석.xlsx"

OUT = BASE / "step12_final_analysis_dataset_for_reproduction.xlsx"


def read_excel_table(path, sheet, skiprows=2):
    if not path.exists():
        raise FileNotFoundError(f"파일을 찾을 수 없습니다: {path}")
    df = pd.read_excel(path, sheet_name=sheet, skiprows=skiprows)
    df = df.dropna(axis=0, how="all").dropna(axis=1, how="all")
    return df


def check_required_columns(df, required_cols, table_name):
    missing = [c for c in required_cols if c not in df.columns]
    if missing:
        raise ValueError(f"{table_name}에 필요한 열이 없습니다: {missing}")


# 1. 최종 분석자료 불러오기
df = read_excel_table(
    FILE_ANALYSIS,
    sheet="Analysis_Data_Used",
    skiprows=2
)

required_cols = [
    "analysis_row_id",
    "hospital_id_code",
    "standard_hospital_name",
    "patient_exp_year",
    "patient_experience_index",
    "nurse",
    "doctor",
    "treatment",
    "environment",
    "rights",
    "overall",
    "main_medical_income_margin_pct",
    "main_labor_cost_ratio_pct",
    "latest_quality_index",
    "conservative_quality_index",
    "log_total_beds",
    "doctors_per_100_beds",
    "nursing_grade_main_num",
    "equipment_per_100_beds",
    "metro_area",
    "year_2023_dummy",
]

check_required_columns(df, required_cols, "9차 Analysis_Data_Used")


# 2. 분석자료 기본 구조 검산
n_obs = len(df)
n_hospitals = df["hospital_id_code"].nunique()
year_counts = df["patient_exp_year"].value_counts().to_dict()

assert n_obs == 94, f"최종 관측치 수가 94가 아닙니다: {n_obs}"
assert n_hospitals == 47, f"병원 수가 47이 아닙니다: {n_hospitals}"
assert year_counts.get(2021, 0) == 47, year_counts
assert year_counts.get(2023, 0) == 47, year_counts


# 3. 환자경험지수 재계산 검산
domain_cols = ["nurse", "doctor", "treatment", "environment", "rights", "overall"]
df["patient_experience_index_recalc"] = df[domain_cols].mean(axis=1)

max_diff = (
    df["patient_experience_index"] - df["patient_experience_index_recalc"]
).abs().max()

assert max_diff < 1e-8, f"환자경험지수 재계산값이 기존 값과 다릅니다: {max_diff}"


# 4. 주 분석 변수 결측 확인
main_model_cols = [
    "patient_experience_index",
    "main_medical_income_margin_pct",
    "main_labor_cost_ratio_pct",
    "latest_quality_index",
    "log_total_beds",
    "doctors_per_100_beds",
    "nursing_grade_main_num",
    "equipment_per_100_beds",
    "metro_area",
    "year_2023_dummy",
]

missing_main = df[main_model_cols].isna().sum()

if missing_main.sum() != 0:
    raise ValueError(
        "기준 통합모형 투입 변수에 결측이 있습니다:\n"
        + missing_main[missing_main > 0].to_string()
    )


# 5. 민감도 분석용 재무시점 변수 보완
# 9차 파일에 same/avg2/avg2020_2024 변수가 없으면 11차 Regression_Data_Used 사용
sensitivity_cols = [
    "same_medical_income_margin_pct",
    "same_labor_cost_ratio_pct",
    "avg2_medical_income_margin_pct",
    "avg2_labor_cost_ratio_pct",
    "avg2020_2024_medical_income_margin_pct",
    "avg2020_2024_labor_cost_ratio_pct",
]

if any(col not in df.columns for col in sensitivity_cols):
    sens = read_excel_table(
        FILE_SENS,
        sheet="Regression_Data_Used",
        skiprows=2
    )
    check_required_columns(sens, required_cols + sensitivity_cols, "11차 Regression_Data_Used")
    df_out = sens.copy()
else:
    df_out = df.copy()


# 6. SPSS/R 재현용 clean dataset 저장
keep_cols = [
    "analysis_row_id",
    "hospital_id_code",
    "standard_hospital_name",
    "patient_exp_year",
    "patient_experience_index",
    "nurse",
    "doctor",
    "treatment",
    "environment",
    "rights",
    "overall",
    "main_medical_income_margin_pct",
    "main_labor_cost_ratio_pct",
    "same_medical_income_margin_pct",
    "same_labor_cost_ratio_pct",
    "avg2_medical_income_margin_pct",
    "avg2_labor_cost_ratio_pct",
    "avg2020_2024_medical_income_margin_pct",
    "avg2020_2024_labor_cost_ratio_pct",
    "latest_quality_index",
    "conservative_quality_index",
    "log_total_beds",
    "doctors_per_100_beds",
    "nursing_grade_main_num",
    "equipment_per_100_beds",
    "metro_area",
    "year_2023_dummy",
]

keep_cols = [c for c in keep_cols if c in df_out.columns]

with pd.ExcelWriter(OUT, engine="openpyxl") as writer:
    df_out[keep_cols].to_excel(writer, index=False, sheet_name="Final_Analysis_Data")

    pd.DataFrame({
        "check_item": [
            "final_n",
            "hospital_count",
            "year_2021_n",
            "year_2023_n",
            "patient_experience_index_max_diff",
        ],
        "value": [
            n_obs,
            n_hospitals,
            year_counts.get(2021, 0),
            year_counts.get(2023, 0),
            max_diff,
        ]
    }).to_excel(writer, index=False, sheet_name="Validation_Checks")

print("Python 재현자료 생성 완료")
print(f"저장 파일: {OUT}")