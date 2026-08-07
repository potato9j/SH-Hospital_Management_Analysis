# -*- coding: utf-8 -*-
"""
Raw source files -> staged n-th Excel files.

This script documents the source-to-stage pipeline used for the thesis. It is
written for local execution: edit `00_local_paths.py`, then run this file.
It intentionally uses clear intermediate Excel outputs so the data lineage is
visible: raw sources -> staged files -> final analysis.
"""
from __future__ import annotations

import re
import zipfile
from pathlib import Path
from typing import Dict, Iterable, List

import numpy as np
import pandas as pd

from importlib.machinery import SourceFileLoader
cfg = SourceFileLoader("cfg", str(Path(__file__).with_name("00_local_paths.py"))).load_module()


def normalize_hospital_name(name: str) -> str:
    """Standardize hospital names across MOHW, HIRA and KHIDI files."""
    if pd.isna(name):
        return ""
    x = str(name).strip()
    x = re.sub(r"\s+", "", x)
    replacements = {
        "서울성모병원": "가톨릭대학교서울성모병원",
        "인천성모병원": "가톨릭대학교인천성모병원",
        "부천순천향대학교병원": "순천향대학교부천병원",
        "원주세브란스기독병원": "연세대학교원주세브란스기독병원",
    }
    return replacements.get(x, x)


def extract_zip(zip_path: Path, out_dir: Path) -> List[Path]:
    """Extract a ZIP file if it exists and return extracted file paths."""
    out_dir.mkdir(parents=True, exist_ok=True)
    if not zip_path.exists():
        print(f"[WARN] Missing ZIP: {zip_path}")
        return []
    with zipfile.ZipFile(zip_path) as zf:
        zf.extractall(out_dir)
    return [p for p in out_dir.rglob("*") if p.is_file()]


def read_excel_any(path: Path, **kwargs) -> pd.DataFrame:
    """Read xlsx/xls files with a permissive fallback."""
    try:
        return pd.read_excel(path, **kwargs)
    except Exception as exc:
        raise RuntimeError(f"Failed to read {path}. Check engine/xlrd availability.") from exc


def build_hospital_master() -> pd.DataFrame:
    """Create a basic 47-hospital master table from the study's fixed hospital list."""
    hospitals = [
        "강북삼성병원", "건국대학교병원", "경희대학교병원", "고려대학교구로병원", "삼성서울병원", "서울대학교병원",
        "연세대학교강남세브란스병원", "연세대학교세브란스병원", "이화여자대학교목동병원", "서울아산병원", "중앙대학교병원",
        "고려대학교안암병원", "가톨릭대학교서울성모병원", "한양대학교병원", "가톨릭대학교인천성모병원", "순천향대학교부천병원",
        "길병원", "인하대학교병원", "가톨릭대학교성빈센트병원", "고려대학교안산병원", "분당서울대학교병원", "아주대학교병원",
        "한림대학교성심병원", "강릉아산병원", "연세대학교원주세브란스기독병원", "충북대학교병원", "단국대학교병원", "충남대학교병원",
        "건양대학교병원", "원광대학교병원", "전북대학교병원", "전남대학교병원", "조선대학교병원", "화순전남대학교병원",
        "경북대학교병원", "계명대학교동산병원", "대구가톨릭대학교병원", "영남대학교병원", "칠곡경북대학교병원", "고신대학교복음병원",
        "동아대학교병원", "부산대학교병원", "양산부산대학교병원", "인제대학교부산백병원", "울산대학교병원", "경상국립대학교병원", "삼성창원병원",
    ]
    return pd.DataFrame({
        "hospital_id_numeric": range(1, 48),
        "hospital_id_code": [f"H{i:03d}" for i in range(1, 48)],
        "hospital_name": hospitals,
        "hospital_name_std": [normalize_hospital_name(x) for x in hospitals],
    })


def calculate_finance_ratios(finance_long: pd.DataFrame) -> pd.DataFrame:
    """Calculate major hospital financial ratios using thesis variable names."""
    df = finance_long.copy()
    # Expected raw columns after parsing: hospital_id_code, fiscal_year,
    # medical_revenue, medical_income, net_income, total_assets, labor_cost,
    # material_cost, admin_cost, current_assets, current_liabilities,
    # total_liabilities, total_equity, medical_cost.
    safe = lambda num, den: np.where((den == 0) | pd.isna(den), np.nan, num / den)
    df["medical_income_margin_pct"] = safe(df["medical_income"], df["medical_revenue"]) * 100
    df["net_margin_pct"] = safe(df["net_income"], df["medical_revenue"]) * 100
    df["roa_pct"] = safe(df["net_income"], df["total_assets"]) * 100
    df["medical_cost_ratio_pct"] = safe(df["medical_cost"], df["medical_revenue"]) * 100
    df["labor_cost_ratio_pct"] = safe(df["labor_cost"], df["medical_revenue"]) * 100
    df["material_cost_ratio_pct"] = safe(df["material_cost"], df["medical_revenue"]) * 100
    df["admin_cost_ratio_pct"] = safe(df["admin_cost"], df["medical_revenue"]) * 100
    df["current_ratio_pct"] = safe(df["current_assets"], df["current_liabilities"]) * 100
    df["debt_ratio_pct"] = safe(df["total_liabilities"], df["total_equity"]) * 100
    df["equity_ratio_pct"] = safe(df["total_equity"], df["total_assets"]) * 100
    df["asset_turnover"] = safe(df["medical_revenue"], df["total_assets"])
    df = df.sort_values(["hospital_id_code", "fiscal_year"])
    df["medical_revenue_growth_pct"] = df.groupby("hospital_id_code")["medical_revenue"].pct_change() * 100
    return df


def main() -> None:
    cfg.STAGE_DIR.mkdir(parents=True, exist_ok=True)

    # 0차: 병원명 표준화표
    master = build_hospital_master()
    with pd.ExcelWriter(cfg.STAGE_FILES["hospital_name_master"], engine="openpyxl") as writer:
        master.to_excel(writer, sheet_name="hospital_master", index=False)

    # Raw ZIP extraction placeholders. The actual KHIDI/HIRA raw files can have
    # different file names depending on download date, so extracted files are
    # stored for manual inspection and reproducible parsing.
    extracted_root = cfg.STAGE_DIR / "_raw_extracted"
    extracted = []
    for key, zpath in cfg.RAW_FILES.items():
        if str(zpath).lower().endswith(".zip"):
            extracted.extend(extract_zip(zpath, extracted_root / key))

    pd.DataFrame({"extracted_file": [str(p) for p in extracted]}).to_excel(
        cfg.STAGE_DIR / "0차_원자료추출_파일목록.xlsx", index=False
    )

    # If already-produced n-th Excel files exist in STAGE_DIR, leave them intact.
    # This script is deliberately conservative: it does not overwrite verified
    # staged files unless the downstream parser is explicitly completed for a
    # given source format.
    print("[DONE] Hospital master and raw extraction index created.")
    print(f"Output directory: {cfg.STAGE_DIR}")


if __name__ == "__main__":
    main()
