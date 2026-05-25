# -*- coding: utf-8 -*-
"""
부록 L. Python 재현자료 생성 및 검산 코드

프로젝트명:
    상급종합병원의 환자경험평가와 재무성과·적정성평가의 관계 분석

목적:
    1. 9차 분석자료에서 최종 분석패널을 재구성한다.
    2. 10차 회귀분석·회귀진단 결과와 11차 민감도 분석 결과를 부록용 표로 정리한다.
    3. 13차 작성계획의 corrected VIF 값을 별도로 정리한다.
    4. 논문에 사용한 핵심 수치가 현재 파일과 일치하는지 검산한다.

주의:
    - 이 코드는 원자료 zip 파일을 처음부터 파싱하는 코드가 아니라,
      논문 작성에 사용한 n차 산출파일을 기준으로 재현자료를 만드는 코드이다.
    - 원자료 파싱 전체 코드는 별도 관리
    - 본문 및 부록의 최종 회귀계수는 R 코드 실행 결과를 기준으로 한다.
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable, Sequence
import math
import zipfile

import numpy as np
import pandas as pd


# ============================================================
# 0. 경로 및 입력 파일 설정
# ============================================================

BASE_DIR = Path(".").resolve()

FILE_ANALYSIS_9 = BASE_DIR / "9차_기술통계_상관분석_회귀분석_투입변수_최종선정.xlsx"
FILE_REG_10 = BASE_DIR / "10차_회귀분석_회귀진단.xlsx"
FILE_SENS_11 = BASE_DIR / "11차_민감도_분석.xlsx"
FILE_PLAN_13 = BASE_DIR / "13차_작성계획.xlsx"
FILE_FIN_2 = BASE_DIR / "2차_재무비율_산출결과.xlsx"

OUT_DIR = BASE_DIR / "reproduction_output"
OUT_DIR.mkdir(exist_ok=True)

OUT_REPRO_DATA = OUT_DIR / "step12_final_analysis_dataset_for_reproduction.xlsx"
OUT_FULL_DATA = OUT_DIR / "analysis_dataset_full_for_reproduction.xlsx"
OUT_APPENDIX_TABLES = OUT_DIR / "appendix_reproduction_tables.xlsx"
OUT_SOURCE_INVENTORY = OUT_DIR / "source_zip_inventory.xlsx"


# ============================================================
# 1. 공통 함수
# ============================================================

def read_table(path: Path, sheet_name: str, skiprows: int = 0) -> pd.DataFrame:
    """엑셀 시트에서 표를 불러오고 완전 공백 행·열을 제거한다."""
    if not path.exists():
        raise FileNotFoundError(f"파일을 찾을 수 없습니다: {path}")

    df = pd.read_excel(path, sheet_name=sheet_name, skiprows=skiprows)
    df = df.dropna(axis=0, how="all").dropna(axis=1, how="all")
    return df


def require_columns(df: pd.DataFrame, columns: Sequence[str], table_name: str) -> None:
    """필수 열이 모두 있는지 확인한다."""
    missing = [c for c in columns if c not in df.columns]
    if missing:
        raise KeyError(f"[{table_name}] 필수 열이 없습니다: {missing}")


def numeric(df: pd.DataFrame, columns: Iterable[str]) -> pd.DataFrame:
    """지정한 열을 숫자형으로 변환한다."""
    out = df.copy()
    for col in columns:
        if col in out.columns:
            out[col] = pd.to_numeric(out[col], errors="coerce")
    return out


def approx_equal(a: float, b: float, tol: float = 1e-6) -> bool:
    """소수점 오차 허용 비교."""
    if pd.isna(a) or pd.isna(b):
        return False
    return abs(float(a) - float(b)) <= tol


def assert_close(label: str, actual: float, expected: float, tol: float = 1e-6) -> None:
    """검산값이 기대값과 일치하는지 확인한다."""
    if not approx_equal(actual, expected, tol):
        raise AssertionError(
            f"{label} 불일치: actual={actual}, expected={expected}, tol={tol}"
        )


def pct_missing(series: pd.Series) -> float:
    """결측률 계산."""
    return float(series.isna().mean() * 100)


# ============================================================
# 2. 9차 분석자료에서 최종 분석패널 재구성
# ============================================================

def load_analysis_data() -> pd.DataFrame:
    """
    9차 파일의 Analysis_Data_Used 시트에서 94개 병원-평가연도 분석자료를 불러온다.

    9차 파일은 상단 2개 행에 제목·설명이 있고,
    3행부터 실제 변수명이 시작되므로 skiprows=2를 적용한다.
    """
    df = read_table(
        FILE_ANALYSIS_9,
        sheet_name="Analysis_Data_Used",
        skiprows=2,
    )

    required = [
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
    require_columns(df, required, "9차 Analysis_Data_Used")

    numeric_cols = [
        "analysis_row_id",
        "patient_exp_year",
        "patient_experience_index",
        "nurse",
        "doctor",
        "treatment",
        "environment",
        "rights",
        "overall",
        "main_medical_income_margin_pct",
        "main_net_margin_pct",
        "main_roa_pct",
        "main_current_ratio_pct",
        "main_debt_ratio_pct",
        "main_equity_ratio_pct",
        "main_total_asset_turnover",
        "main_medical_revenue_growth_pct",
        "main_medical_expense_ratio_pct",
        "main_labor_cost_ratio_pct",
        "main_material_cost_ratio_pct",
        "main_admin_cost_ratio_pct",
        "main_inpatient_revenue_share_pct",
        "main_outpatient_revenue_share_pct",
        "main_nonmedical_revenue_ratio_pct",
        "latest_valid_item_count",
        "latest_quality_index",
        "latest_top_grade_count",
        "latest_top_grade_rate",
        "conservative_quality_item_count",
        "conservative_quality_index",
        "conservative_top_grade_count",
        "conservative_top_grade_rate",
        "conservative_min_gap_year",
        "conservative_max_gap_year",
        "total_beds_detail_sum",
        "log_total_beds",
        "total_doctors",
        "doctors_per_100_beds",
        "medical_specialists",
        "medical_specialists_per_100_beds",
        "nursing_grade_main_num",
        "equipment_total_count",
        "equipment_per_100_beds",
        "equipment_detail_category_count",
        "upper_bed_ratio",
        "icu_bed_ratio",
        "metro_area",
        "year_2023_dummy",
    ]

    df = numeric(df, numeric_cols)

    # 최종 분석자료의 기본 구조 검산
    if df.shape[0] != 94:
        raise AssertionError(f"최종 분석 관측치 수가 94개가 아닙니다: {df.shape[0]}")

    n_hospitals = df["hospital_id_code"].nunique()
    if n_hospitals != 47:
        raise AssertionError(f"병원 수가 47개가 아닙니다: {n_hospitals}")

    year_counts = df["patient_exp_year"].value_counts().to_dict()
    if year_counts.get(2021, 0) != 47 or year_counts.get(2023, 0) != 47:
        raise AssertionError(f"평가연도별 관측치 수가 47/47이 아닙니다: {year_counts}")

    return df


def make_reproduction_dataset(df_full: pd.DataFrame) -> pd.DataFrame:
    """
    R/SPSS 재현분석에 필요한 21개 핵심 변수만 추출한다.
    이 데이터셋은 11차 민감도 분석의 Regression_Data_Used와 같은 구조다.
    """
    columns = [
        "analysis_row_id",
        "hospital_id_code",
        "standard_hospital_name",
        "patient_exp_year",
        "patient_experience_index",
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

    missing = [c for c in columns if c not in df_full.columns]
    if missing:
        # 9차 파일에 민감도용 재무시점 변수가 없을 경우 11차 파일에서 보완한다.
        sens_data = read_table(
            FILE_SENS_11,
            sheet_name="Regression_Data_Used",
            skiprows=2,
        )
        df_merged = sens_data.copy()
        require_columns(df_merged, columns, "11차 Regression_Data_Used")
        return numeric(df_merged[columns], columns)

    return numeric(df_full[columns].copy(), columns)


def make_area_dataset(df_full: pd.DataFrame) -> pd.DataFrame:
    """환자경험 6개 영역별 보조분석에 사용하는 자료를 추출한다."""
    columns = [
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
        "main_net_margin_pct",
        "main_roa_pct",
        "main_labor_cost_ratio_pct",
        "main_material_cost_ratio_pct",
        "main_admin_cost_ratio_pct",
        "latest_quality_index",
        "conservative_quality_index",
        "log_total_beds",
        "doctors_per_100_beds",
        "nursing_grade_main_num",
        "equipment_per_100_beds",
        "metro_area",
        "year_2023_dummy",
    ]
    require_columns(df_full, columns, "영역별 분석자료")
    return df_full[columns].copy()


# ============================================================
# 3. 결측표 및 기술검산표
# ============================================================

def build_missing_table(df: pd.DataFrame) -> pd.DataFrame:
    """주요 분석변수의 최종 결측 현황표를 만든다."""
    variables = [
        ("종속변수", "환자경험지수", "patient_experience_index"),
        ("종속변수", "간호사 영역", "nurse"),
        ("종속변수", "의사 영역", "doctor"),
        ("종속변수", "투약 및 치료과정", "treatment"),
        ("종속변수", "병원환경", "environment"),
        ("종속변수", "환자권리보장", "rights"),
        ("종속변수", "전반적 평가", "overall"),
        ("재무성과", "의료수익의료이익률", "main_medical_income_margin_pct"),
        ("재무성과", "의료수익순이익률", "main_net_margin_pct"),
        ("재무성과", "총자산순이익률", "main_roa_pct"),
        ("재무성과", "의료수익증가율", "main_medical_revenue_growth_pct"),
        ("비용구조", "인건비율", "main_labor_cost_ratio_pct"),
        ("비용구조", "재료비율", "main_material_cost_ratio_pct"),
        ("비용구조", "관리운영비율", "main_admin_cost_ratio_pct"),
        ("의료질", "최신가용 의료질지수", "latest_quality_index"),
        ("의료질", "보수적 의료질지수", "conservative_quality_index"),
        ("병원특성", "로그 병상수", "log_total_beds"),
        ("병원특성", "100병상당 의사수", "doctors_per_100_beds"),
        ("병원특성", "간호등급", "nursing_grade_main_num"),
        ("병원특성", "100병상당 의료장비수", "equipment_per_100_beds"),
        ("병원특성", "수도권 여부", "metro_area"),
        ("평가연도", "2023년 더미", "year_2023_dummy"),
    ]

    rows = []
    n = len(df)
    for group, name_kr, col in variables:
        if col not in df.columns:
            continue
        valid_n = int(df[col].notna().sum())
        miss_n = int(df[col].isna().sum())
        rows.append(
            {
                "변수군": group,
                "변수명": name_kr,
                "code_variable": col,
                "전체 관측치": n,
                "유효 관측치": valid_n,
                "결측 관측치": miss_n,
                "결측률": round(miss_n / n * 100, 3),
            }
        )

    return pd.DataFrame(rows)


def build_descriptive_table(df: pd.DataFrame) -> pd.DataFrame:
    """주요 연속형 변수 기술통계표를 만든다."""
    variables = [
        "patient_experience_index",
        "main_medical_income_margin_pct",
        "main_labor_cost_ratio_pct",
        "latest_quality_index",
        "log_total_beds",
        "doctors_per_100_beds",
        "nursing_grade_main_num",
        "equipment_per_100_beds",
    ]
    rows = []
    for col in variables:
        s = pd.to_numeric(df[col], errors="coerce")
        rows.append(
            {
                "variable": col,
                "n": int(s.notna().sum()),
                "mean": s.mean(),
                "sd": s.std(ddof=1),
                "min": s.min(),
                "p25": s.quantile(0.25),
                "median": s.median(),
                "p75": s.quantile(0.75),
                "max": s.max(),
            }
        )
    return pd.DataFrame(rows)


# ============================================================
# 4. 10차·11차·13차 산출표 불러오기
# ============================================================

def load_regression_outputs() -> dict[str, pd.DataFrame]:
    """10차 회귀분석·회귀진단 결과표를 불러온다."""
    tables = {
        "Model_Fit": read_table(FILE_REG_10, "Model_Fit", skiprows=2),
        "Coef_PrimaryCluster": read_table(FILE_REG_10, "Coef_PrimaryCluster", skiprows=2),
        "Coef_HC3": read_table(FILE_REG_10, "Coef_HC3", skiprows=2),
        "Coef_OLS": read_table(FILE_REG_10, "Coef_OLS", skiprows=2),
        "VIF_MainModels": read_table(FILE_REG_10, "VIF_MainModels", skiprows=2),
        "Influence_M5_Top": read_table(FILE_REG_10, "Influence_M5_Top", skiprows=2),
        "Diagnostics_Summary": read_table(FILE_REG_10, "Diagnostics_Summary", skiprows=2),
        "Area_Coef_HC3": read_table(FILE_REG_10, "Area_Coef_HC3", skiprows=2),
        "Area_Coef_Cluster": read_table(FILE_REG_10, "Area_Coef_Cluster", skiprows=2),
        "Area_Model_Fit": read_table(FILE_REG_10, "Area_Model_Fit", skiprows=2),
    }
    return tables


def load_sensitivity_outputs() -> dict[str, pd.DataFrame]:
    """11차 민감도 분석 결과표를 불러온다."""
    tables = {
        "Sensitivity_Model_Definitions": read_table(FILE_SENS_11, "Sensitivity_Model_Definitions", skiprows=2),
        "Sensitivity_Model_Fit": read_table(FILE_SENS_11, "Sensitivity_Model_Fit", skiprows=2),
        "Coef_Primary": read_table(FILE_SENS_11, "Coef_Primary", skiprows=2),
        "Coef_HC3": read_table(FILE_SENS_11, "Coef_HC3", skiprows=2),
        "Coef_Cluster": read_table(FILE_SENS_11, "Coef_Cluster", skiprows=2),
        "Key_Coefficient_Comparison": read_table(FILE_SENS_11, "Key_Coefficient_Comparison", skiprows=2),
        "SE_Comparison_M5": read_table(FILE_SENS_11, "SE_Comparison_M5", skiprows=2),
        "Influence_M5_Baseline_Top": read_table(FILE_SENS_11, "Influence_M5_Baseline_Top", skiprows=2),
        "Influence_Exclusion_Comparison": read_table(FILE_SENS_11, "Influence_Exclusion_Comparison", skiprows=2),
    }
    return tables


def load_corrected_vif() -> pd.DataFrame:
    """13차 작성계획에서 최종 논문용 corrected VIF를 불러온다."""
    vif = read_table(FILE_PLAN_13, "Corrected_VIF_For_Paper", skiprows=2)
    return vif


# ============================================================
# 5. 자료원 zip 파일 목록 확인
# ============================================================

def build_source_zip_inventory() -> pd.DataFrame:
    """
    원자료 zip 파일 내부 목록을 정리한다.
    이 결과는 분석수치 산출에는 직접 사용하지 않고, 자료원 관리용으로만 사용한다.
    """
    zip_files = sorted(BASE_DIR.glob("*.zip"))
    rows = []
    for zpath in zip_files:
        with zipfile.ZipFile(zpath, "r") as z:
            for item in z.infolist():
                if item.is_dir():
                    continue
                rows.append(
                    {
                        "zip_file": zpath.name,
                        "internal_file": item.filename,
                        "file_size": item.file_size,
                    }
                )
    return pd.DataFrame(rows)


# ============================================================
# 6. 핵심 수치 검산
# ============================================================

def validate_core_numbers(
    df_repro: pd.DataFrame,
    reg_tables: dict[str, pd.DataFrame],
    sens_tables: dict[str, pd.DataFrame],
    corrected_vif: pd.DataFrame,
) -> pd.DataFrame:
    """
    논문에 사용한 핵심 수치와 파일 내 수치가 일치하는지 점검한다.

    검산 기준:
        - 관측치 수 94
        - 병원 수 47
        - 기준 통합모형 M5 최신가용 의료질지수 계수 13.581708
        - M5 최신가용 의료질지수 p값 0.000382
        - M5 수정 R² 0.277351
        - Cook's distance 기준값 4/94 = 0.042553
        - corrected VIF 최대값 3.940 이하
    """
    checks = []

    def add_check(item: str, actual, expected, passed: bool, note: str = "") -> None:
        checks.append(
            {
                "검산항목": item,
                "actual": actual,
                "expected": expected,
                "pass": bool(passed),
                "note": note,
            }
        )

    n_obs = int(df_repro.shape[0])
    n_hosp = int(df_repro["hospital_id_code"].nunique())
    add_check("최종 관측치 수", n_obs, 94, n_obs == 94)
    add_check("병원 수", n_hosp, 47, n_hosp == 47)

    model_fit = reg_tables["Model_Fit"]
    m5_fit = model_fit.loc[model_fit["model_id"] == "M5"].iloc[0]
    add_check(
        "M5 수정 R²",
        round(float(m5_fit["adj_r_squared"]), 6),
        0.277351,
        approx_equal(float(m5_fit["adj_r_squared"]), 0.277351, 1e-6),
    )

    coef_cluster = reg_tables["Coef_PrimaryCluster"]
    m5_quality = coef_cluster.loc[
        (coef_cluster["model_id"] == "M5")
        & (coef_cluster["coef_name"] == "latest_quality_index")
    ].iloc[0]

    add_check(
        "M5 최신가용 의료질지수 계수",
        round(float(m5_quality["coef"]), 6),
        13.581708,
        approx_equal(float(m5_quality["coef"]), 13.581708, 1e-6),
    )
    add_check(
        "M5 최신가용 의료질지수 p값",
        round(float(m5_quality["p_value"]), 6),
        0.000382,
        approx_equal(float(m5_quality["p_value"]), 0.000382, 1e-6),
    )

    cook_threshold = 4 / n_obs
    add_check(
        "Cook's distance 기준값",
        round(cook_threshold, 6),
        0.042553,
        approx_equal(cook_threshold, 0.0425531914893617, 1e-9),
        "최종 분석 n=94 기준",
    )

    # corrected VIF는 13차 시트의 열명이 다를 수 있어 숫자열을 탐색한다.
    vif_numeric_cols = corrected_vif.select_dtypes(include=[np.number]).columns.tolist()
    vif_max = np.nan
    if vif_numeric_cols:
        # 일반적으로 VIF 값이 들어 있는 첫 번째 숫자열 또는 vif 열을 사용한다.
        vif_col = "vif" if "vif" in corrected_vif.columns else vif_numeric_cols[-1]
        vif_max = float(pd.to_numeric(corrected_vif[vif_col], errors="coerce").max())
        add_check(
            "corrected VIF 최대값",
            round(vif_max, 3),
            "< 4.000",
            vif_max < 4.0,
            "13차 corrected VIF 기준",
        )

    # 민감도 분석 S08 최신가용 의료질지수 계수
    sens_key = sens_tables["Key_Coefficient_Comparison"]
    s08_quality = sens_key.loc[
        (sens_key["model_id"] == "S08")
        & (sens_key["term"] == "latest_quality_index")
    ].iloc[0]

    add_check(
        "S08 영향점 제외 최신가용 의료질지수 계수",
        round(float(s08_quality["coef"]), 6),
        14.618993,
        approx_equal(float(s08_quality["coef"]), 14.618993, 1e-6),
    )

    result = pd.DataFrame(checks)
    if not result["pass"].all():
        failed = result.loc[~result["pass"]]
        raise AssertionError("핵심 수치 검산 실패:\n" + failed.to_string(index=False))

    return result


# ============================================================
# 7. 엑셀 산출물 저장
# ============================================================

def save_reproduction_files(
    df_full: pd.DataFrame,
    df_repro: pd.DataFrame,
    df_area: pd.DataFrame,
    missing_table: pd.DataFrame,
    descriptive_table: pd.DataFrame,
    reg_tables: dict[str, pd.DataFrame],
    sens_tables: dict[str, pd.DataFrame],
    corrected_vif: pd.DataFrame,
    validation_table: pd.DataFrame,
) -> None:
    """재현자료와 부록표를 엑셀로 저장한다."""
    with pd.ExcelWriter(OUT_REPRO_DATA, engine="openpyxl") as writer:
        df_repro.to_excel(writer, index=False, sheet_name="Final_Analysis_Data")

        variable_dictionary = pd.DataFrame(
            [
                ["patient_experience_index", "환자경험지수", "6개 환자경험평가 영역 점수의 단순평균"],
                ["main_medical_income_margin_pct", "의료수익의료이익률", "의료이익 / 의료수익 × 100"],
                ["main_labor_cost_ratio_pct", "인건비율", "인건비 / 의료수익 × 100"],
                ["latest_quality_index", "최신가용 의료질지수", "최신 유효 적정성평가 등급점수 평균"],
                ["conservative_quality_index", "보수적 의료질지수", "평가연도 gap 제한 항목만 평균"],
                ["log_total_beds", "로그 병상수", "총 병상수 자연로그"],
                ["doctors_per_100_beds", "100병상당 의사수", "총 의사수 / 총 병상수 × 100"],
                ["nursing_grade_main_num", "간호등급", "숫자형 간호등급"],
                ["equipment_per_100_beds", "100병상당 의료장비수", "의료장비수 / 총 병상수 × 100"],
                ["metro_area", "수도권 여부", "수도권=1, 비수도권=0"],
                ["year_2023_dummy", "2023년 더미", "2023년 환자경험평가=1, 2021년=0"],
            ],
            columns=["variable_name", "한국어 명칭", "사전식 의미"],
        )
        variable_dictionary.to_excel(writer, index=False, sheet_name="Variable_Dictionary")

        notes = pd.DataFrame(
            [
                ["analysis_unit", "hospital-patient_experience_year", "병원-환자경험평가연도 단위"],
                ["n_obs", 94, "47개 병원 × 2개 평가연도"],
                ["main_financial_timing", "t-1", "2021년 환자경험=2020년 재무, 2023년 환자경험=2022년 재무"],
                ["primary_model", "M5", "기준 통합모형"],
                ["primary_se", "hospital-clustered SE", "병원 단위 군집표준오차"],
            ],
            columns=["항목", "값", "비고"],
        )
        notes.to_excel(writer, index=False, sheet_name="Data_Notes")

    with pd.ExcelWriter(OUT_FULL_DATA, engine="openpyxl") as writer:
        df_full.to_excel(writer, index=False, sheet_name="Analysis_Data_Full")
        df_area.to_excel(writer, index=False, sheet_name="Area_Model_Data")
        df_repro.to_excel(writer, index=False, sheet_name="Final_Analysis_Data")

    with pd.ExcelWriter(OUT_APPENDIX_TABLES, engine="openpyxl") as writer:
        validation_table.to_excel(writer, index=False, sheet_name="Validation_Checks")
        missing_table.to_excel(writer, index=False, sheet_name="Missing_Table")
        descriptive_table.to_excel(writer, index=False, sheet_name="Descriptive_Core")
        corrected_vif.to_excel(writer, index=False, sheet_name="Corrected_VIF")

        for name, table in reg_tables.items():
            sheet = name[:31]
            table.to_excel(writer, index=False, sheet_name=sheet)

        for name, table in sens_tables.items():
            sheet = ("Sens_" + name)[:31]
            table.to_excel(writer, index=False, sheet_name=sheet)


# ============================================================
# 8. 실행부
# ============================================================

def main() -> None:
    print("[1] 9차 분석자료 불러오는 중")
    df_full = load_analysis_data()

    print("[2] 재현분석용 데이터셋 생성")
    df_repro = make_reproduction_dataset(df_full)
    df_area = make_area_dataset(df_full)

    print("[3] 결측표 및 기술통계표 생성")
    missing_table = build_missing_table(df_full)
    descriptive_table = build_descriptive_table(df_full)

    print("[4] 10차·11차·13차 결과표 불러오기")
    reg_tables = load_regression_outputs()
    sens_tables = load_sensitivity_outputs()
    corrected_vif = load_corrected_vif()

    print("[5] 핵심 수치 검산")
    validation_table = validate_core_numbers(
        df_repro=df_repro,
        reg_tables=reg_tables,
        sens_tables=sens_tables,
        corrected_vif=corrected_vif,
    )

    print("[6] 재현자료 및 부록표 저장")
    save_reproduction_files(
        df_full=df_full,
        df_repro=df_repro,
        df_area=df_area,
        missing_table=missing_table,
        descriptive_table=descriptive_table,
        reg_tables=reg_tables,
        sens_tables=sens_tables,
        corrected_vif=corrected_vif,
        validation_table=validation_table,
    )

    print("[7] 원자료 zip 목록 저장")
    inventory = build_source_zip_inventory()
    if not inventory.empty:
        inventory.to_excel(OUT_SOURCE_INVENTORY, index=False)

    print("\n완료")
    print(f"- 재현분석 데이터셋: {OUT_REPRO_DATA}")
    print(f"- 전체 분석자료: {OUT_FULL_DATA}")
    print(f"- 부록표 묶음: {OUT_APPENDIX_TABLES}")
    print(f"- 원자료 zip 목록: {OUT_SOURCE_INVENTORY}")
    print("\n검산 결과")
    print(validation_table.to_string(index=False))


if __name__ == "__main__":
    main()