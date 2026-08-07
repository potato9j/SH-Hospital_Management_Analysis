# Pipeline overview

The project follows a three-layer pipeline.

## 1. Raw source files

Official raw source files are placed under `data/raw/`:

- `A_MOHW_ 상급종합병원 지정기관명단.zip`
- `B_KHIDI_재무상태표&손익계산서.zip`
- `C_HIRA_환자경험평가 자료.zip`
- `D_HIRA_병원 일반현황 자료.zip`
- `C2_HIRA_2021_세부적정성평가_47개병원_전체평가항목.xlsx`
- `C2_HIRA_2023_세부적정성평가_47개병원_전체평가항목.xls`

## 2. Staged n-th Excel files

The staged files are stored under `outputs/nth_excels/`. They document hospital-name standardization, financial parsing, financial ratios, patient experience data, quality indices, hospital characteristics and analysis panel construction.

## 3. Analysis and thesis outputs

Final numeric outputs are stored under `outputs/results/`:

- financial ratios
- descriptive statistics
- correlations
- OLS coefficients
- HC3 robust standard errors
- hospital-clustered standard errors
- VIF
- Cook's distance and leverage
- sensitivity analysis
- Random Effects and Two-Way Fixed Effects
- Hausman test
- common-item, high-coverage and balanced-category quality indices

The final submission workbook is `hospital_management_full_numeric_results.xlsx`.
