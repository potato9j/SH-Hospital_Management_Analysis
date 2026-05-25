#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Publication-quality figures reproduced from the uploaded R visualization script.
Data source files are expected in /mnt/data.
Outputs: 600-dpi PNG and vector PDF files under /mnt/data/figures_R_output.
"""

from __future__ import annotations

import os
import re
import textwrap
import warnings
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import font_manager as fm
from matplotlib.patches import Rectangle, FancyArrowPatch
from matplotlib.lines import Line2D
from matplotlib.backends.backend_pdf import PdfPages
import statsmodels.api as sm

warnings.filterwarnings("ignore", category=UserWarning)

BASE_DIR = Path("/mnt/data")
OUT_DIR = BASE_DIR / "figures_R_output"
PNG_DIR = OUT_DIR / "png_600dpi"
PDF_DIR = OUT_DIR / "pdf_vector"
PREVIEW_DIR = OUT_DIR / "preview"
for d in (OUT_DIR, PNG_DIR, PDF_DIR, PREVIEW_DIR):
    d.mkdir(parents=True, exist_ok=True)

# ----------------------------------------------------------------------
# Font and plot settings
# ----------------------------------------------------------------------
FONT_CANDIDATES = [
    "/usr/share/fonts/truetype/nanum/NanumGothic.ttf",
    "/usr/share/fonts/truetype/nanum/NanumBarunGothic.ttf",
    "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc",
    "/usr/share/fonts/truetype/unfonts-core/UnDotum.ttf",
]
font_path = next((p for p in FONT_CANDIDATES if Path(p).exists()), None)
if font_path:
    fm.fontManager.addfont(font_path)
    font_name = fm.FontProperties(fname=font_path).get_name()
else:
    font_name = "DejaVu Sans"

mpl.rcParams.update({
    "font.family": font_name,
    "axes.unicode_minus": False,
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
    "figure.dpi": 150,
    "savefig.dpi": 600,
    "axes.titlesize": 14,
    "axes.titleweight": "bold",
    "axes.labelsize": 11,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 9,
    "axes.edgecolor": "0.15",
    "axes.linewidth": 0.8,
    "grid.color": "0.88",
    "grid.linewidth": 0.6,
})

# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------

def sanitize_filename(name: str) -> str:
    name = re.sub(r"[\\/:*?\"<>|]", "_", name)
    return name


def save_figure(fig: plt.Figure, basename: str) -> Dict[str, Path]:
    png_path = PNG_DIR / f"{basename}.png"
    pdf_path = PDF_DIR / f"{basename}.pdf"
    fig.savefig(png_path, dpi=600, bbox_inches="tight")
    fig.savefig(pdf_path, bbox_inches="tight")
    plt.close(fig)
    return {"png": png_path, "pdf": pdf_path}


def minimal_axes(ax: plt.Axes) -> None:
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.grid(True, axis="y")
    ax.set_axisbelow(True)


def to_numeric(df: pd.DataFrame, cols: Iterable[str]) -> pd.DataFrame:
    for c in cols:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce")
    return df


def load_excel_with_skip(filename: str, sheet: str, skiprows: int) -> pd.DataFrame:
    return pd.read_excel(BASE_DIR / filename, sheet_name=sheet, skiprows=skiprows)

# ----------------------------------------------------------------------
# Load data - equivalent to readxl::read_excel(..., skip = n)
# ----------------------------------------------------------------------
analysis_data = load_excel_with_skip(
    "9차_기술통계_상관분석_회귀분석_투입변수_최종선정.xlsx",
    "Analysis_Data_Used",
    2,
)
financial_summary = load_excel_with_skip(
    "2차_재무비율_산출결과.xlsx",
    "Summary",
    4,
)
m5_cluster_coeff = load_excel_with_skip(
    "10차_회귀분석_회귀진단.xlsx",
    "Coef_PrimaryCluster",
    2,
)
m5_influence_top = load_excel_with_skip(
    "10차_회귀분석_회귀진단.xlsx",
    "Influence_M5_Top",
    2,
)
sensitivity_key = load_excel_with_skip(
    "11차_민감도_분석.xlsx",
    "Key_Coefficient_Comparison",
    2,
)

analysis_numeric_cols = [
    "analysis_row_id", "patient_exp_year", "nurse", "doctor", "treatment", "environment", "rights", "overall",
    "patient_experience_index", "main_medical_income_margin_pct", "main_net_margin_pct", "main_roa_pct",
    "main_current_ratio_pct", "main_debt_ratio_pct", "main_equity_ratio_pct", "main_total_asset_turnover",
    "main_medical_revenue_growth_pct", "main_medical_expense_ratio_pct", "main_labor_cost_ratio_pct",
    "main_material_cost_ratio_pct", "main_admin_cost_ratio_pct", "latest_quality_index", "log_total_beds",
    "doctors_per_100_beds", "nursing_grade_main_num", "equipment_per_100_beds", "metro_area",
]
analysis_data = to_numeric(analysis_data, analysis_numeric_cols)
analysis_data["year_2023_dummy"] = np.where(analysis_data["patient_exp_year"] == 2023, 1, 0)

financial_numeric_cols = [c for c in financial_summary.columns if c != "year"] + ["year"]
financial_summary = to_numeric(financial_summary, financial_numeric_cols)
financial_summary = financial_summary.dropna(subset=["year"])

m5_cluster_coeff = to_numeric(m5_cluster_coeff, ["coef", "std_error", "ci95_low", "ci95_high"])
sensitivity_key = to_numeric(sensitivity_key, ["coef", "std_error", "ci_low", "ci_high", "n"])

created: List[Tuple[str, Path, Path]] = []

# ----------------------------------------------------------------------
# Figure 1. Research model
# ----------------------------------------------------------------------
def figure_01() -> Tuple[str, Dict[str, Path]]:
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.set_xlim(0, 5)
    ax.set_ylim(0.3, 4.7)
    ax.axis("off")
    nodes = [
        ("독립변수\n재무성과 + 비용구조", 1.0, 2.5, 1.75, 0.78),
        ("종속변수\n환자경험평가", 4.0, 2.5, 1.75, 0.78),
        ("설명변수\n적정성평가 기반\n의료질지수", 2.5, 4.0, 2.05, 0.90),
        ("통제변수\n병원특성", 2.5, 1.0, 1.75, 0.78),
    ]
    for label, x, y, w, h in nodes:
        rect = Rectangle((x - w / 2, y - h / 2), w, h,
                         facecolor="white", edgecolor="black", linewidth=1.0)
        ax.add_patch(rect)
        ax.text(x, y, label, ha="center", va="center", fontsize=11.5, linespacing=1.25)
    arrows = [
        ((1.875, 2.5), (3.125, 2.5)),
        ((2.5, 3.55), (3.48, 2.86)),
        ((2.5, 1.39), (3.48, 2.14)),
    ]
    for start, end in arrows:
        ax.add_patch(FancyArrowPatch(start, end, arrowstyle="-|>", mutation_scale=14,
                                     linewidth=1.0, color="black", shrinkA=0, shrinkB=0))
    fig.suptitle("그림 1. 연구모형", y=0.96, fontweight="bold", fontsize=14)
    return "figure_01_research_model_simple", save_figure(fig, "figure_01_research_model_simple")

# ----------------------------------------------------------------------
# Figure 2. Patient experience domain boxplot
# ----------------------------------------------------------------------
def figure_02() -> Tuple[str, Dict[str, Path]]:
    domains = ["nurse", "doctor", "treatment", "environment", "rights", "overall"]
    domain_kr = {
        "nurse": "간호사",
        "doctor": "의사",
        "treatment": "투약·치료과정",
        "environment": "병원환경",
        "rights": "환자권리보장",
        "overall": "전반적 평가",
    }
    years = [2021, 2023]
    fig, ax = plt.subplots(figsize=(10, 6))
    positions = np.arange(1, len(domains) + 1)
    width = 0.28
    offsets = {2021: -width/1.6, 2023: width/1.6}
    styles = {2021: "-", 2023: "--"}
    hatches = {2021: "", 2023: "////"}
    for yr in years:
        data = [analysis_data.loc[analysis_data["patient_exp_year"] == yr, d].dropna().values for d in domains]
        bp = ax.boxplot(
            data, positions=positions + offsets[yr], widths=width, patch_artist=True,
            manage_ticks=False, showfliers=True,
            medianprops={"color": "black", "linewidth": 1.1, "linestyle": styles[yr]},
            boxprops={"facecolor": "white", "edgecolor": "black", "linewidth": 1.0, "linestyle": styles[yr]},
            whiskerprops={"color": "black", "linewidth": 0.9, "linestyle": styles[yr]},
            capprops={"color": "black", "linewidth": 0.9, "linestyle": styles[yr]},
            flierprops={"marker": "o", "markersize": 3, "markerfacecolor": "white", "markeredgecolor": "0.35", "alpha": 0.7},
        )
        for patch in bp["boxes"]:
            patch.set_hatch(hatches[yr])
    ax.set_xticks(positions)
    ax.set_xticklabels([domain_kr[d] for d in domains], rotation=25, ha="right")
    ax.set_ylabel("점수")
    ax.set_xlabel("환자경험평가 영역")
    ax.set_title("그림 2. 2021·2023 환자경험평가 영역별 점수 분포")
    minimal_axes(ax)
    legend_handles = [
        Rectangle((0,0),1,1, facecolor="white", edgecolor="black", linestyle=styles[yr], hatch=hatches[yr], label=str(yr))
        for yr in years
    ]
    ax.legend(handles=legend_handles, title="평가연도", loc="lower center", bbox_to_anchor=(0.5, -0.28), ncol=2, frameon=False)
    fig.tight_layout()
    return "figure_02_patient_experience_domain_boxplot", save_figure(fig, "figure_02_patient_experience_domain_boxplot")

# ----------------------------------------------------------------------
# Figure 3. Financial ratios trend
# ----------------------------------------------------------------------
def figure_03() -> Tuple[str, Dict[str, Path]]:
    var_map = {
        "avg_medical_income_margin_pct": "의료수익의료이익률",
        "avg_net_margin_pct": "의료수익순이익률",
        "avg_labor_cost_ratio_pct": "인건비율",
        "avg_material_cost_ratio_pct": "재료비율",
        "avg_admin_cost_ratio_pct": "관리운영비율",
    }
    line_styles = ["-", "--", "-.", ":", (0, (3, 1, 1, 1))]
    markers = ["o", "s", "^", "D", "v"]
    grays = ["0.05", "0.20", "0.35", "0.50", "0.65"]
    fig, ax = plt.subplots(figsize=(10, 6))
    years = financial_summary["year"].astype(int).values
    for idx, (var, label) in enumerate(var_map.items()):
        ax.plot(
            years,
            financial_summary[var].values,
            label=label,
            linestyle=line_styles[idx],
            marker=markers[idx],
            linewidth=1.4,
            markersize=4.5,
            color=grays[idx],
        )
    ax.axhline(0, color="0.25", linewidth=0.7)
    ax.set_xticks(sorted(financial_summary["year"].dropna().astype(int).unique()))
    ax.set_xlabel("연도")
    ax.set_ylabel("비율(%)")
    ax.set_title("그림 3. 2020~2024년 주요 재무비율 평균 추이")
    minimal_axes(ax)
    ax.legend(title="지표", loc="lower center", bbox_to_anchor=(0.5, -0.34), ncol=3, frameon=False)
    fig.tight_layout()
    return "figure_03_financial_ratios_trend", save_figure(fig, "figure_03_financial_ratios_trend")

# ----------------------------------------------------------------------
# Figure 4. Scatter + linear regression CI
# ----------------------------------------------------------------------
def figure_04() -> Tuple[str, Dict[str, Path]]:
    df = analysis_data[["latest_quality_index", "patient_experience_index"]].dropna().copy()
    x = df["latest_quality_index"].astype(float).values
    y = df["patient_experience_index"].astype(float).values
    X = sm.add_constant(x)
    model = sm.OLS(y, X).fit()
    x_pred = np.linspace(np.nanmin(x), np.nanmax(x), 200)
    pred = model.get_prediction(sm.add_constant(x_pred)).summary_frame(alpha=0.05)
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.scatter(x, y, s=28, facecolor="0.35", edgecolor="white", linewidth=0.3, alpha=0.85)
    ax.plot(x_pred, pred["mean"].values, color="black", linewidth=1.2)
    ax.fill_between(x_pred, pred["mean_ci_lower"].values, pred["mean_ci_upper"].values, color="0.75", alpha=0.45, linewidth=0)
    ax.set_xlabel("최신가용 의료질지수")
    ax.set_ylabel("환자경험지수")
    ax.set_title("그림 4. 환자경험지수와 최신가용 의료질지수의 관계")
    minimal_axes(ax)
    fig.tight_layout()
    return "figure_04_quality_vs_patient_experience_scatter", save_figure(fig, "figure_04_quality_vs_patient_experience_scatter")

# ----------------------------------------------------------------------
# Figure 5. Standardized coefficients
# ----------------------------------------------------------------------
def figure_05() -> Tuple[str, Dict[str, Path]]:
    continuous_terms = [
        "main_medical_income_margin_pct",
        "main_labor_cost_ratio_pct",
        "latest_quality_index",
        "log_total_beds",
        "doctors_per_100_beds",
        "nursing_grade_main_num",
        "equipment_per_100_beds",
    ]
    term_labels = {
        "main_medical_income_margin_pct": "의료수익의료이익률",
        "main_labor_cost_ratio_pct": "인건비율",
        "latest_quality_index": "최신가용 의료질지수",
        "log_total_beds": "로그 병상수",
        "doctors_per_100_beds": "100병상당 의사수",
        "nursing_grade_main_num": "간호등급",
        "equipment_per_100_beds": "100병상당 의료장비수",
    }
    sd_y = analysis_data["patient_experience_index"].std(skipna=True, ddof=1)
    rows = []
    for _, row in m5_cluster_coeff.loc[(m5_cluster_coeff["model_id"] == "M5") & (m5_cluster_coeff["coef_name"].isin(continuous_terms))].iterrows():
        term = row["coef_name"]
        sd_x = analysis_data[term].std(skipna=True, ddof=1)
        std_beta = row["coef"] * sd_x / sd_y
        std_se = row["std_error"] * sd_x / sd_y
        rows.append({
            "term": term,
            "term_label": term_labels[term],
            "std_beta": std_beta,
            "std_se": std_se,
            "ci_low": std_beta - 1.96 * std_se,
            "ci_high": std_beta + 1.96 * std_se,
        })
    df = pd.DataFrame(rows).sort_values("std_beta")
    y_pos = np.arange(len(df))
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.axvline(0, color="0.25", linewidth=0.8)
    ax.errorbar(
        df["std_beta"], y_pos,
        xerr=[df["std_beta"] - df["ci_low"], df["ci_high"] - df["std_beta"]],
        fmt="o", markersize=5, color="black", ecolor="black", elinewidth=1.0, capsize=3,
    )
    ax.set_yticks(y_pos)
    ax.set_yticklabels(df["term_label"])
    ax.set_xlabel("표준화 회귀계수")
    ax.set_title("그림 5. 기준 통합모형 주요 변수의 표준화 회귀계수")
    minimal_axes(ax)
    ax.grid(True, axis="x")
    ax.grid(False, axis="y")
    fig.tight_layout()
    return "figure_05_m5_standardized_coefficient_plot", save_figure(fig, "figure_05_m5_standardized_coefficient_plot")

# ----------------------------------------------------------------------
# Figure 6. Sensitivity analysis coefficient comparison
# ----------------------------------------------------------------------
def figure_06() -> Tuple[str, Dict[str, Path]]:
    df = sensitivity_key.loc[sensitivity_key["term"] == "latest_quality_index"].copy()
    df = df.sort_values("model_id")
    x = np.arange(len(df))
    fig, ax = plt.subplots(figsize=(9, 6))
    ax.axhline(0, color="0.25", linewidth=0.8)
    ax.errorbar(
        x, df["coef"].values,
        yerr=[df["coef"].values - df["ci_low"].values, df["ci_high"].values - df["coef"].values],
        fmt="o", markersize=5, color="black", ecolor="black", elinewidth=1.0, capsize=4,
    )
    ax.set_xticks(x)
    ax.set_xticklabels(df["model_id"].values)
    ax.set_xlabel("민감도 분석 모형")
    ax.set_ylabel("회귀계수")
    ax.set_title("그림 6. 민감도 분석별 최신가용 의료질지수 계수 비교")
    minimal_axes(ax)
    ax.margins(x=0.08, y=0.12)
    fig.tight_layout()
    return "figure_06_sensitivity_quality_coefficient_plot", save_figure(fig, "figure_06_sensitivity_quality_coefficient_plot")

# ----------------------------------------------------------------------
# Appendix Figure A. Correlation heatmap
# ----------------------------------------------------------------------
def appendix_A() -> Tuple[str, Dict[str, Path]]:
    var_order = [
        "patient_experience_index",
        "main_medical_income_margin_pct",
        "main_labor_cost_ratio_pct",
        "latest_quality_index",
        "log_total_beds",
        "doctors_per_100_beds",
        "nursing_grade_main_num",
        "equipment_per_100_beds",
    ]
    labels = {
        "patient_experience_index": "환자경험지수",
        "main_medical_income_margin_pct": "의료수익의료이익률",
        "main_labor_cost_ratio_pct": "인건비율",
        "latest_quality_index": "최신가용 의료질지수",
        "log_total_beds": "로그 병상수",
        "doctors_per_100_beds": "100병상당 의사수",
        "nursing_grade_main_num": "간호등급",
        "equipment_per_100_beds": "100병상당 의료장비수",
    }
    corr = analysis_data[var_order].corr(method="pearson", min_periods=1)
    fig, ax = plt.subplots(figsize=(8.8, 7.6))
    im = ax.imshow(corr.values, vmin=-1, vmax=1, cmap="coolwarm")
    ax.set_xticks(np.arange(len(var_order)))
    ax.set_yticks(np.arange(len(var_order)))
    ax.set_xticklabels([labels[v] for v in var_order], rotation=45, ha="right")
    ax.set_yticklabels([labels[v] for v in var_order])
    for i in range(len(var_order)):
        for j in range(len(var_order)):
            val = corr.values[i, j]
            text_color = "white" if abs(val) > 0.65 else "black"
            ax.text(j, i, f"{val:.2f}", ha="center", va="center", fontsize=9, color=text_color)
    cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label("상관계수")
    ax.set_title("부록그림 A. 주요 변수 피어슨 상관계수 히트맵")
    ax.set_xlabel("")
    ax.set_ylabel("")
    fig.tight_layout()
    return "appendix_figure_A_correlation_heatmap", save_figure(fig, "appendix_figure_A_correlation_heatmap")

# ----------------------------------------------------------------------
# Appendix Figure B. Cook's distance
# ----------------------------------------------------------------------
def appendix_B() -> Tuple[str, Dict[str, Path]]:
    m5_cols = [
        "analysis_row_id",
        "hospital_id_code",
        "standard_hospital_name",
        "patient_exp_year",
        "patient_experience_index",
        "main_medical_income_margin_pct",
        "main_labor_cost_ratio_pct",
        "latest_quality_index",
        "log_total_beds",
        "doctors_per_100_beds",
        "nursing_grade_main_num",
        "equipment_per_100_beds",
        "metro_area",
    ]
    df = analysis_data[m5_cols].dropna().copy()
    y = df["patient_experience_index"].astype(float)
    X = pd.DataFrame({
        "const": 1.0,
        "main_medical_income_margin_pct": df["main_medical_income_margin_pct"].astype(float),
        "main_labor_cost_ratio_pct": df["main_labor_cost_ratio_pct"].astype(float),
        "latest_quality_index": df["latest_quality_index"].astype(float),
        "log_total_beds": df["log_total_beds"].astype(float),
        "doctors_per_100_beds": df["doctors_per_100_beds"].astype(float),
        "nursing_grade_main_num": df["nursing_grade_main_num"].astype(float),
        "equipment_per_100_beds": df["equipment_per_100_beds"].astype(float),
        "metro_area": df["metro_area"].astype(float),
        "year_2023_dummy": (df["patient_exp_year"] == 2023).astype(int),
    })
    fit = sm.OLS(y, X).fit()
    infl = fit.get_influence()
    df["cooks_d"] = infl.cooks_distance[0]
    threshold = 4 / len(df)
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.axhline(threshold, color="black", linestyle="--", linewidth=0.9, label=f"4/n = {threshold:.3f}")
    ax.vlines(df["analysis_row_id"], 0, df["cooks_d"], color="0.25", linewidth=0.8)
    ax.scatter(df["analysis_row_id"], df["cooks_d"], s=16, color="black")
    ax.set_xlabel("분석 행 번호")
    ax.set_ylabel("Cook's distance")
    ax.set_title("부록그림 B. 기준 통합모형 Cook's distance")
    minimal_axes(ax)
    ax.legend(loc="upper right", frameon=False)
    fig.tight_layout()
    return "appendix_figure_B_cooks_distance", save_figure(fig, "appendix_figure_B_cooks_distance")

# Generate all figures
for fn in [figure_01, figure_02, figure_03, figure_04, figure_05, figure_06, appendix_A, appendix_B]:
    name, paths = fn()
    created.append((name, paths["png"], paths["pdf"]))

# Create a compact visual contact sheet for quick review.
from PIL import Image, ImageDraw, ImageFont
contact_files = [png for _, png, _ in created]
thumbs = []
for p in contact_files:
    img = Image.open(p).convert("RGB")
    img.thumbnail((900, 650), Image.Resampling.LANCZOS)
    thumbs.append((Path(p), img.copy()))
cols = 2
pad = 30
label_h = 60
cell_w = 900 + 2 * pad
cell_h = 650 + label_h + 2 * pad
rows = (len(thumbs) + cols - 1) // cols
sheet = Image.new("RGB", (cols * cell_w, rows * cell_h), "white")
draw = ImageDraw.Draw(sheet)
try:
    label_font = ImageFont.truetype(str(Path("/usr/share/fonts/truetype/nanum/NanumGothic.ttf")), 22)
except Exception:
    label_font = None
for idx, (p, img) in enumerate(thumbs):
    r, c = divmod(idx, cols)
    x0 = c * cell_w + pad
    y0 = r * cell_h + pad
    draw.text((x0, y0), p.stem, fill="black", font=label_font)
    x_img = c * cell_w + pad + (900 - img.width) // 2
    y_img = r * cell_h + pad + label_h
    sheet.paste(img, (x_img, y_img))
contact_jpg = PREVIEW_DIR / "all_figures_contact_sheet.jpg"
contact_pdf = OUT_DIR / "all_figures_contact_sheet.pdf"
sheet.save(contact_jpg, quality=92)
sheet.save(contact_pdf, "PDF", resolution=150.0)

# Manifest
manifest = OUT_DIR / "figure_manifest.csv"
pd.DataFrame(created, columns=["figure_id", "png_600dpi", "pdf_vector"]).to_csv(manifest, index=False, encoding="utf-8-sig")

# README
readme = OUT_DIR / "README.txt"
readme.write_text(
    "논문용 그림 파일 생성 결과\n"
    "================================\n\n"
    "생성 방식: 업로드된 R 시각화 코드의 데이터 로딩, 전처리, 그림 구성 로직을 /mnt/data 원자료에 맞춰 재현했습니다.\n"
    "주의: 현재 실행 환경에는 Rscript가 없어 R 자체는 실행할 수 없었고, 동일 로직을 Python/matplotlib으로 재현해 산출했습니다.\n\n"
    "폴더 구성\n"
    "- png_600dpi/: 논문/과제 문서 삽입용 고해상도 PNG 파일\n"
    "- pdf_vector/: 확대해도 선명한 벡터 PDF 파일\n"
    "- all_figures_review.pdf: 전체 그림 검토용 PDF\n"
    "- figure_manifest.csv: 파일 목록\n\n"
    "생성 그림\n"
    + "\n".join([f"- {name}" for name, _, _ in created])
    + "\n",
    encoding="utf-8",
)

# Also write a path-corrected R script for the user to run locally if desired.
original_r = BASE_DIR / "붙여넣은 텍스트 (1).txt"
fixed_r = OUT_DIR / "18차_R_시각화_코드_경로수정본.R"
if original_r.exists():
    txt = original_r.read_text(encoding="utf-8", errors="replace")
    txt = re.sub(
        r'setwd\(".*?"\)',
        'setwd("/mnt/data")',
        txt,
        count=1,
        flags=re.DOTALL,
    )
    fixed_r.write_text(txt, encoding="utf-8")

print("Generated files:")
for name, png, pdf in created:
    print(f"{name}\t{png}\t{pdf}")
print(f"Contact sheet JPG\t{contact_jpg}")
print(f"Contact sheet PDF\t{contact_pdf}")
print(f"Manifest\t{manifest}")
print(f"README\t{readme}")
print(f"Fixed R script\t{fixed_r if fixed_r.exists() else 'not created'}")

