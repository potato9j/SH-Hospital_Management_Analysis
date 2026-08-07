# Thesis-style grayscale figures. Edit 00_local_paths.R, then run this file.
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
source(file.path(script_dir, "00_local_paths.R"))

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2)
})

theme_thesis <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "grey35"),
      axis.text = element_text(color = "grey20"),
      axis.title = element_text(color = "grey20"),
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )
}

make_figures <- function() {
  df <- readxl::read_excel(FILES$final_results, sheet = "analysis_data_revised")
  std <- readxl::read_excel(FILES$final_results, sheet = "T21_std_coef")
  infl <- readxl::read_excel(FILES$final_results, sheet = "influence_all")
  sens <- readxl::read_excel(FILES$final_results, sheet = "T27_sensitivity_summary")

  # Figure 5: scatter
  p5 <- ggplot(df, aes(x = year_matched_quality_index, y = patient_experience_index)) +
    geom_point(size = 1.7, color = "grey35") +
    geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey80", linewidth = 0.6) +
    labs(x = "평가연도 매칭 의료질지수", y = "환자경험지수") +
    theme_thesis()
  ggsave(file.path(FIGURE_DIR, "figure5_year_matched_quality_scatter_mono.png"), p5, width = 7, height = 5, dpi = 300)

  # Figure 7: Cook's distance lollipop. Column names are handled defensively.
  cook_col <- grep("cook", names(infl), ignore.case = TRUE, value = TRUE)[1]
  infl$analysis_no <- seq_len(nrow(infl))
  threshold <- 4 / nrow(infl)
  p7 <- ggplot(infl, aes(x = analysis_no, y = .data[[cook_col]])) +
    geom_segment(aes(xend = analysis_no, y = 0, yend = .data[[cook_col]]), color = "grey30", linewidth = 0.25) +
    geom_point(color = "black", size = 1.2) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "grey30") +
    annotate("text", x = Inf, y = threshold, label = paste0("4/n = ", sprintf("%.3f", threshold)), hjust = 1.1, vjust = -0.5, size = 3) +
    labs(x = "분석 행 번호", y = "Cook's distance") + theme_thesis()
  ggsave(file.path(FIGURE_DIR, "figure7_cooks_distance_mono.png"), p7, width = 7, height = 5, dpi = 300)
}

make_figures()
