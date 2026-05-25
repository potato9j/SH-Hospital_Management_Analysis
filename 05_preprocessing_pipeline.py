# -*- coding: utf-8 -*-
"""
Appendix. Python preprocessing pipeline
부록용 재현 코드 초안

목적
1) KHIDI 재무자료, HIRA 환자경험평가, HIRA 적정성평가, 병원 일반현황 자료를 분석용 데이터셋으로 정리한다.
2) 재무비율, 환자경험지수, 적정성평가지수, 병원특성 변수를 생성한다.
3) 병원×환자경험평가연도 단위 분석패널을 만든다.

- 실제 파일명과 폴더명은 사용자의 저장 위치에 맞게 수정해야 한다.
- 평가대상제외, 등급제외, 빈칸은 0점이 아니라 결측으로 처리한다.
"""

from pathlib import Path
import re
import zipfile
import numpy as np
import pandas as pd

BASE = Path(".")
OUT = BASE / "output"
OUT.mkdir(exist_ok=True)

# ---------------------------------------------------------------------
# 1. Utility functions
# ---------------------------------------------------------------------

def safe_read_excel(path, **kwargs):
    """Read xlsx/xls. Some downloaded .xls files may be HTML tables."""
    try:
        return pd.read_excel(path, **kwargs)
    except Exception:
        tables = pd.read_html(path, encoding="utf-8")
        if not tables:
            raise
        return tables[0]


def pct(num, den):
    """Return percentage. Invalid denominator returns NaN."""
    if pd.isna(num) or pd.isna(den) or den == 0:
        return np.nan
    return num / den * 100


def ratio(num, den):
    """Return ratio. Invalid denominator returns NaN."""
    if pd.isna(num) or pd.isna(den) or den == 0:
        return np.nan
    return num / den

# ---------------------------------------------------------------------
# 2. Patient experience data
# ---------------------------------------------------------------------

def build_patient_experience(path_2021, path_2023):
    frames = []
    for year, path in [(2021, path_2021), (2023, path_2023)]:
        raw = safe_read_excel(path)
        # 실제 원자료 열 이름에 맞게 rename mapping 수정 가능
        rename_map = {
            "간호사서비스": "nurse",
            "간호사 영역": "nurse",
            "의사서비스": "doctor",
            "의사 영역": "doctor",
            "투약 및 치료과정": "treatment",
            "병원환경": "environment",
            "환자권리보장": "rights",
            "전반적 평가": "overall",
            "병원명": "standard_hospital_name",
            "의료기관명": "standard_hospital_name",
        }
        raw = raw.rename(columns={c: rename_map.get(c, c) for c in raw.columns})
        keep = [c for c in ["standard_hospital_name", "nurse", "doctor", "treatment", "environment", "rights", "overall"] if c in raw.columns]
        df = raw[keep].copy()
        df["patient_exp_year"] = year
        for c in ["nurse", "doctor", "treatment", "environment", "rights", "overall"]:
            df[c] = pd.to_numeric(df[c], errors="coerce")
        # 사용자 원자료 확인 정정: 영남대학교병원 2021년 투약 및 치료과정 87.32
        mask = (df["patient_exp_year"] == 2021) & (df["standard_hospital_name"].astype(str).str.contains("영남대학교병원", na=False))
        if mask.any():
            df.loc[mask, "treatment"] = 87.32
        df["patient_experience_index"] = df[["nurse", "doctor", "treatment", "environment", "rights", "overall"]].mean(axis=1)
        frames.append(df)
    return pd.concat(frames, ignore_index=True)

# ---------------------------------------------------------------------
# 3. Appropriateness grade scoring
# ---------------------------------------------------------------------

def score_grade(x):
    """Convert HIRA grade to score. Excluded cases remain missing."""
    if pd.isna(x):
        return np.nan
    s = str(x).strip()
    if s in ["평가대상제외", "등급제외", "", "-", "nan"]:
        return np.nan
    m = re.search(r"([1-5])", s)
    if not m:
        return np.nan
    grade = int(m.group(1))
    return {1: 5, 2: 4, 3: 3, 4: 2, 5: 1}.get(grade, np.nan)


def build_appropriateness_index(app_raw):
    """Create latest available quality index from wide grade table."""
    id_cols = [c for c in ["hospital_id_code", "standard_hospital_name", "hospital_name"] if c in app_raw.columns]
    value_cols = [c for c in app_raw.columns if c not in id_cols]
    long = app_raw.melt(id_vars=id_cols, value_vars=value_cols, var_name="eval_item", value_name="grade_raw")
    long["grade_score"] = long["grade_raw"].apply(score_grade)
    long["valid_grade"] = long["grade_score"].notna().astype(int)
    # 사용자 원자료 확인 정정 예시. 실제 wide table에서 이미 반영되어 있으면 변화 없음.
    # 고려대학교안산병원 성인중환자실 = 1등급, 울산대학교병원 결핵 = 1등급
    mask1 = long["standard_hospital_name"].astype(str).str.contains("고려대학교안산", na=False) & long["eval_item"].astype(str).str.contains("성인중환자실", na=False)
    long.loc[mask1, ["grade_raw", "grade_score", "valid_grade"]] = ["1등급", 5, 1]
    mask2 = long["standard_hospital_name"].astype(str).str.contains("울산대학교병원", na=False) & long["eval_item"].astype(str).str.contains("결핵", na=False)
    long.loc[mask2, ["grade_raw", "grade_score", "valid_grade"]] = ["1등급", 5, 1]
    key = "standard_hospital_name"
    idx = long.groupby(key).agg(
        latest_quality_index=("grade_score", "mean"),
        latest_valid_item_count=("valid_grade", "sum"),
        latest_top_grade_count=("grade_score", lambda s: (s == 5).sum()),
    ).reset_index()
    idx["latest_top_grade_rate"] = idx["latest_top_grade_count"] / idx["latest_valid_item_count"]
    return long, idx

# ---------------------------------------------------------------------
# 4. Financial ratios from already parsed KHIDI wide table
# ---------------------------------------------------------------------

def build_financial_ratios(fin):
    df = fin.copy()
    # Column names below follow the analysis workbooks. Adjust if source names differ.
    df["medical_income_margin_pct"] = df.apply(lambda r: pct(r.get("의료이익"), r.get("의료수익")), axis=1)
    df["net_margin_pct"] = df.apply(lambda r: pct(r.get("당기순이익"), r.get("의료수익")), axis=1)
    df["roa_pct"] = df.apply(lambda r: pct(r.get("당기순이익"), r.get("자산총계")), axis=1)
    df["current_ratio_pct"] = df.apply(lambda r: pct(r.get("유동자산"), r.get("유동부채")), axis=1)
    df["debt_ratio_pct"] = df.apply(lambda r: pct(r.get("부채총계"), r.get("자본총계")), axis=1)
    df["equity_ratio_pct"] = df.apply(lambda r: pct(r.get("자본총계"), r.get("자산총계")), axis=1)
    df["asset_turnover"] = df.apply(lambda r: ratio(r.get("의료수익"), r.get("자산총계")), axis=1)
    df["medical_cost_ratio_pct"] = df.apply(lambda r: pct(r.get("의료비용"), r.get("의료수익")), axis=1)
    df["labor_cost_ratio_pct"] = df.apply(lambda r: pct(r.get("인건비"), r.get("의료수익")), axis=1)
    df["material_cost_ratio_pct"] = df.apply(lambda r: pct(r.get("재료비"), r.get("의료수익")), axis=1)
    df["admin_cost_ratio_pct"] = df.apply(lambda r: pct(r.get("관리운영비"), r.get("의료수익")), axis=1)
    return df

# ---------------------------------------------------------------------
# 5. Build hospital × patient-experience-year analysis panel
# ---------------------------------------------------------------------

def build_analysis_panel(patient, financial_ratios, quality, hospital_chars):
    panel = patient.copy()
    panel["main_fin_year"] = panel["patient_exp_year"].map({2021: 2020, 2023: 2022})
    panel["same_fin_year"] = panel["patient_exp_year"]
    fin_main = financial_ratios.add_prefix("main_")
    fin_main = fin_main.rename(columns={"main_standard_hospital_name": "standard_hospital_name", "main_year": "main_fin_year"})
    panel = panel.merge(fin_main, on=["standard_hospital_name", "main_fin_year"], how="left")
    panel = panel.merge(quality, on="standard_hospital_name", how="left")
    panel = panel.merge(hospital_chars, on="standard_hospital_name", how="left")
    panel["year_2023_dummy"] = (panel["patient_exp_year"] == 2023).astype(int)
    return panel

if __name__ == "__main__":
    print("This appendix script is a reproducibility template. Set file paths before running.")
