# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot exposure levels and gaps between socioeconomic groups.
#
#' @Description: Read the saved observed exposure results and build named plot lists for
# education and income. Saving keeps the manuscript names. The imputed-reading figures are
# drawn by scripts/tables_images/imputation/figure_exposure_imputed.R and the 5 km figures
# by scripts/tables_images/robustness/buffer_5km/figure_exposure_5km.R.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read saved summaries.
#   II.  Process data: plot group differences and levels for education and income.
#   III. Save outputs: write the education figures, then the income figures.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the plotting functions and shared settings, then apply the paper's theme
source(here::here("src", "general_utilities", "plot", "exposure_figures.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))
set_paper_theme()

# Set the input folder and the output folder
dir_reg     <- here::here("data", "processed", "idw_regressions")
dir_figures <- here::here("results", "figures")

# Set the files containing group means and regression estimates with confidence intervals
files_ci_edu      <- here::here(dir_reg, 
                                sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
                                        exposure_buffers_km,
                                        analysis_year))
files_summary_edu <- here::here(dir_reg,
                                sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
                                        exposure_buffers_km,
                                        analysis_year))
files_ci_inc      <- here::here(dir_reg,
                                sprintf("exposure_ci_estimates_income_%dkm_%d.parquet",
                                        exposure_buffers_km,
                                        analysis_year))
files_summary_inc <- here::here(dir_reg,
                                sprintf("exposure_group_summaries_income_%dkm_%d.parquet",
                                        exposure_buffers_km, analysis_year))

# Read the observed-data estimates for education.
ci_edu      <- data.table::rbindlist(lapply(files_ci_edu, arrow::read_parquet))
summary_edu <- data.table::rbindlist(lapply(files_summary_edu, arrow::read_parquet))

# Keep income optional for manual education-only runs.
ci_inc <- summary_inc <- NULL
has_income <- all(file.exists(c(files_ci_inc, files_summary_inc)))
if (has_income) {
  ci_inc      <- data.table::rbindlist(lapply(files_ci_inc, arrow::read_parquet))
  summary_inc <- data.table::rbindlist(lapply(files_summary_inc, arrow::read_parquet))
}

# ========================================================================================
# II: Process data
# ========================================================================================
# Plot education-group gaps in exceedance hours, with confidence intervals
plots_ci_edu <- build_exposure_ci_figures(ci_dt = ci_edu[outcome != "avg"],
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# Plot mean PM concentrations by education group
plots_levels_edu <- build_exposure_level_figures(sum_dt = summary_edu,
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# Plot the same outcomes by income group for cities with income data
plots_ci_inc <- plots_levels_inc <- list()
if (has_income) {
  plots_ci_inc <- build_exposure_ci_figures(ci_dt = ci_inc[outcome != "avg"],
    tag = "income", city_labels = exposure_city_labels, city_files = exposure_city_files)

  plots_levels_inc <- build_exposure_level_figures(sum_dt = summary_inc,
    tag = "income", city_labels = exposure_city_labels, city_files = exposure_city_files)
}

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the education figures, then the income figures
save_exposure_plot_family(plots_ci_edu, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
save_exposure_plot_family(plots_levels_edu, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)

save_exposure_plot_family(plots_ci_inc, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
save_exposure_plot_family(plots_levels_inc, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
