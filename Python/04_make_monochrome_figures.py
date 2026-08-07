# -*- coding: utf-8 -*-
"""Create thesis-style monochrome figures from the final numeric workbook."""
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

from importlib.machinery import SourceFileLoader
cfg = SourceFileLoader("cfg", str(Path(__file__).with_name("00_local_paths.py"))).load_module()

plt.rcParams["font.family"] = ["Malgun Gothic", "AppleGothic", "NanumGothic", "DejaVu Sans"]
plt.rcParams["axes.unicode_minus"] = False


def theme_gray(ax):
    ax.set_facecolor("white")
    ax.grid(True, axis="y", color="#E6E6E6", linewidth=0.8)
    ax.grid(True, axis="x", color="#F0F0F0", linewidth=0.5)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_color("#666666")
    ax.spines["bottom"].set_color("#666666")
    ax.tick_params(colors="#333333", labelsize=9)


def main() -> None:
    cfg.FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    xlsx = cfg.FINAL_RESULTS_XLSX
    analysis = pd.read_excel(xlsx, sheet_name="analysis_data_revised")
    # Figure 5: scatter
    fig, ax = plt.subplots(figsize=(7, 5))
    x = analysis["year_matched_quality_index"]
    y = analysis["patient_experience_index"]
    ax.scatter(x, y, s=16, color="#4D4D4D", alpha=0.85)
    z = np.polyfit(x.dropna(), y.loc[x.notna()], 1)
    xp = np.linspace(x.min(), x.max(), 100)
    yp = z[0] * xp + z[1]
    ax.plot(xp, yp, color="black", linewidth=1.2)
    ax.set_xlabel("평가연도 매칭 의료질지수")
    ax.set_ylabel("환자경험지수")
    theme_gray(ax)
    fig.tight_layout()
    fig.savefig(cfg.FIGURE_DIR / "figure5_year_matched_quality_scatter_mono.png", dpi=300)
    plt.close(fig)

    # Figure 6: standardized coefficients
    std = pd.read_excel(xlsx, sheet_name="T21_std_coef")
    if {"variable", "std_coef", "ci_low", "ci_high"}.issubset(std.columns):
        df = std.dropna(subset=["std_coef"]).copy()
    else:
        df = std.iloc[:, :4].copy()
        df.columns = ["variable", "std_coef", "ci_low", "ci_high"]
    fig, ax = plt.subplots(figsize=(7, 5))
    y_pos = np.arange(len(df))
    ax.errorbar(df["std_coef"], y_pos, xerr=[df["std_coef"]-df["ci_low"], df["ci_high"]-df["std_coef"]], fmt="o", color="black", ecolor="black", capsize=3)
    ax.axvline(0, color="#666666", linewidth=1)
    ax.set_yticks(y_pos)
    ax.set_yticklabels(df["variable"])
    ax.set_xlabel("표준화 회귀계수")
    theme_gray(ax)
    fig.tight_layout()
    fig.savefig(cfg.FIGURE_DIR / "figure6_standardized_coefficients_mono.png", dpi=300)
    plt.close(fig)

    print(f"[DONE] Figures saved to {cfg.FIGURE_DIR}")


if __name__ == "__main__":
    main()
