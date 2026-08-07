# -*- coding: utf-8 -*-
"""
Build evaluation-year matched HIRA appropriateness quality indices.

Core thesis logic:
- 2021 patient experience rows use 2021 appropriateness data.
- 2023 patient experience rows use 2023 appropriateness data.
- Grade conversion: 1등급=5, 2등급=4, ..., 5등급=1.
- 평가대상제외/등급제외 are treated as missing, not zero.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Dict, List

import numpy as np
import pandas as pd

from importlib.machinery import SourceFileLoader
cfg = SourceFileLoader("cfg", str(Path(__file__).with_name("00_local_paths.py"))).load_module()


def normalize_item_name(x: str) -> str:
    x = str(x).strip()
    x = x.replace("감영", "감염").replace("하기고", "하기도")
    if x in {"수술부위 감염예방 항생제", "수술적 예방적 항생제", "수술의 예방적 항생제 사용"}:
        return "수술의 예방적 항생제 사용"
    return x


def grade_to_score(x) -> float:
    """Convert HIRA grade text to score. Non-grade values become missing."""
    if pd.isna(x):
        return np.nan
    m = re.match(r"\s*([1-5])", str(x))
    if not m:
        return np.nan
    return 6 - int(m.group(1))


def read_quality_file(path: Path, patient_exp_year: int) -> tuple[pd.DataFrame, pd.DataFrame]:
    raw = pd.read_excel(path, header=None)
    categories = raw.iloc[0].astype(str).replace("nan", np.nan)
    headers = raw.iloc[1].astype(str).replace("nan", np.nan)
    item_cols = [i for i, h in enumerate(headers) if i >= 2 and pd.notna(h) and str(h).strip() != ""]

    rows = raw.iloc[2:].copy()
    rows = rows.rename(columns={0: "hospital_id_numeric", 1: "hospital_name_from_quality_file"})
    rows["hospital_id_numeric"] = pd.to_numeric(rows["hospital_id_numeric"], errors="coerce").astype("Int64")
    rows = rows[rows["hospital_id_numeric"].between(1, 47)]

    out = pd.DataFrame({
        "hospital_id_numeric": rows["hospital_id_numeric"].astype(int),
        "hospital_id_code": rows["hospital_id_numeric"].astype(int).map(lambda v: f"H{v:03d}"),
        "patient_exp_year": patient_exp_year,
        "hospital_name_from_quality_file": rows["hospital_name_from_quality_file"],
    })
    meta_rows = []
    for col in item_cols:
        item = normalize_item_name(headers.iloc[col])
        category = categories.iloc[col] if pd.notna(categories.iloc[col]) else "미분류"
        score_col = f"score__{item}"
        out[score_col] = rows.iloc[:, col].map(grade_to_score).to_numpy()
        meta_rows.append({
            "patient_exp_year": patient_exp_year,
            "category": category,
            "item_name_original": headers.iloc[col],
            "item_name_normalized": item,
            "score_column": score_col,
        })

    score_cols = [c for c in out.columns if c.startswith("score__")]
    out["year_matched_quality_index"] = out[score_cols].mean(axis=1, skipna=True)
    out["year_matched_valid_item_count"] = out[score_cols].notna().sum(axis=1)
    out["year_matched_top_grade_count"] = (out[score_cols] == 5).sum(axis=1)
    out["year_matched_top_grade_rate"] = out["year_matched_top_grade_count"] / out["year_matched_valid_item_count"]
    return out, pd.DataFrame(meta_rows)


def add_alternative_quality_indices(df: pd.DataFrame, meta: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    score_cols = [c for c in df.columns if c.startswith("score__")]
    coverage = pd.DataFrame({
        "item": [c.replace("score__", "") for c in score_cols],
        "N_valid_total": [df[c].notna().sum() for c in score_cols],
        "N_valid_2021": [df.loc[df["patient_exp_year"] == 2021, c].notna().sum() for c in score_cols],
        "N_valid_2023": [df.loc[df["patient_exp_year"] == 2023, c].notna().sum() for c in score_cols],
    }).sort_values(["N_valid_total", "item"], ascending=[False, True])

    common_items = coverage.loc[coverage["N_valid_total"] == len(df), "item"].tolist()
    high_items = coverage.loc[coverage["N_valid_total"] >= 90, "item"].tolist()
    df["year_matched_common12_index"] = df[[f"score__{x}" for x in common_items]].mean(axis=1, skipna=True)
    df["year_matched_high20_index"] = df[[f"score__{x}" for x in high_items]].mean(axis=1, skipna=True)

    # Balanced category index: average within category first, then average categories.
    item_to_cat = dict(zip(meta["item_name_normalized"], meta["category"]))
    categories = sorted(set(item_to_cat.values()))
    cat_cols = []
    for cat in categories:
        cols = [f"score__{item}" for item, c in item_to_cat.items() if c == cat and f"score__{item}" in df.columns]
        if cols:
            new_col = f"cat_mean__{cat}"
            df[new_col] = df[cols].mean(axis=1, skipna=True)
            cat_cols.append(new_col)
    df["year_matched_balanced_category_index"] = df[cat_cols].mean(axis=1, skipna=True)
    return df, coverage


def main() -> None:
    q21, meta21 = read_quality_file(cfg.RAW_FILES["hira_quality_2021"], 2021)
    q23, meta23 = read_quality_file(cfg.RAW_FILES["hira_quality_2023"], 2023)
    quality = pd.concat([q21, q23], ignore_index=True)
    meta = pd.concat([meta21, meta23], ignore_index=True)
    quality, coverage = add_alternative_quality_indices(quality, meta)

    out_path = cfg.STAGE_DIR / "quality_year_matched_indices.xlsx"
    with pd.ExcelWriter(out_path, engine="openpyxl") as writer:
        quality.to_excel(writer, sheet_name="quality_by_hospital_year", index=False)
        meta.to_excel(writer, sheet_name="quality_item_meta", index=False)
        coverage.to_excel(writer, sheet_name="quality_item_coverage", index=False)
    print(f"[DONE] Evaluation-year matched quality indices saved: {out_path}")


if __name__ == "__main__":
    main()
