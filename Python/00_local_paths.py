# -*- coding: utf-8 -*-
"""
Local path configuration for the hospital management analysis project.
Edit PROJECT_DIR only, then run the scripts from an IDE or by pressing 'Run'.
No command-line arguments are required.
"""
from pathlib import Path

# ---------------------------------------------------------------------
# EDIT THIS PATH on your local computer.
# Example Windows: Path(r"D:/hospital_management_analysis")
# Example macOS  : Path("/Users/your_name/hospital_management_analysis")
# ---------------------------------------------------------------------
PROJECT_DIR = Path(r"D:/hospital_management_analysis")

RAW_DIR = PROJECT_DIR / "data" / "raw"                 # official ZIP / XLSX sources
STAGE_DIR = PROJECT_DIR / "outputs" / "nth_excels"     # 0차~13차 staged Excel files
RESULT_DIR = PROJECT_DIR / "outputs" / "results"       # final numeric result Excel/MD
FIGURE_DIR = PROJECT_DIR / "outputs" / "figures"       # paper figures

# Main source files used in the final paper
RAW_FILES = {
    "mohw_hospitals_zip": RAW_DIR / "A_MOHW_ 상급종합병원 지정기관명단.zip",
    "khidi_finance_zip": RAW_DIR / "B_KHIDI_재무상태표&손익계산서.zip",
    "hira_patient_exp_zip": RAW_DIR / "C_HIRA_환자경험평가 자료.zip",
    "hira_hospital_status_zip": RAW_DIR / "D_HIRA_병원 일반현황 자료.zip",
    "hira_quality_2021": RAW_DIR / "C2_HIRA_2021_세부적정성평가_47개병원_전체평가항목.xlsx",
    "hira_quality_2023": RAW_DIR / "C2_HIRA_2023_세부적정성평가_47개병원_전체평가항목.xls",
}

# Staged Excel files. These are the files generated or used in the pipeline.
STAGE_FILES = {
    "hospital_name_master": STAGE_DIR / "0차_원자료검수_병원명표준화표.xlsx",
    "finance_parsed": STAGE_DIR / "1차_KHIDI_재무원자료_파싱결과.xlsx",
    "finance_ratios": STAGE_DIR / "2차_재무비율_산출결과.xlsx",
    "patient_experience": STAGE_DIR / "3차_환자경험평가_정리결과.xlsx",
    "quality_legacy": STAGE_DIR / "4차_적정성평가_등급자료_정리결과.xlsx",
    "hospital_characteristics": STAGE_DIR / "5차_병원일반현황_병원특성_정리결과.xlsx",
    "master_panel": STAGE_DIR / "6차_analysis_master_dataset_rebuilt.xlsx",
    "analysis_variables": STAGE_DIR / "9차_기술통계_상관분석_회귀분석_투입변수_최종선정.xlsx",
    "regression_diagnostics_old": STAGE_DIR / "10차_회귀분석_회귀진단.xlsx",
    "sensitivity_old": STAGE_DIR / "11차_민감도_분석.xlsx",
}

# Final artifacts created by this reproducibility package.
FINAL_RESULTS_XLSX = RESULT_DIR / "hospital_management_full_numeric_results.xlsx"
FINAL_RESULTS_MD = RESULT_DIR / "hospital_management_full_numeric_results.md"

# Variable names used in the final thesis. Keep these aligned with the manuscript.
VARS = {
    "hospital_id": "hospital_id_code",
    "year": "patient_exp_year",
    "y": "patient_experience_index",
    "finance_main": "main_medical_income_margin_pct",
    "cost_main": "main_labor_cost_ratio_pct",
    "quality_main": "year_matched_quality_index",
    "quality_latest": "latest_quality_index",
    "beds_log": "log_total_beds",
    "doctors_100": "doctors_per_100_beds",
    "nursing_grade": "nursing_grade_main_num",
    "equipment_100": "equipment_per_100_beds",
    "metro": "metro_area",
    "year_dummy": "year_2023_dummy",
}

MODEL_M5_X = [
    VARS["finance_main"],
    VARS["cost_main"],
    VARS["quality_main"],
    VARS["beds_log"],
    VARS["doctors_100"],
    VARS["nursing_grade"],
    VARS["equipment_100"],
    VARS["metro"],
    VARS["year_dummy"],
]

# Create output directories when the config is imported.
for _p in [STAGE_DIR, RESULT_DIR, FIGURE_DIR]:
    _p.mkdir(parents=True, exist_ok=True)
