# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot education-group exposure gaps and levels with imputed hourly readings.
#
#' @Description: Read the education tables saved by estimate_exposure_imputed.R and draw
# the figures of scripts/tables_images/exposure/generate_exposure_plots.R for the panels
# that combine observed and imputed readings: gaps in exceedance hours with confidence
# intervals and mean concentrations by education quintile. Only the manuscript's figures
# are saved, under the imputation names in results/figures/imputation/.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read the tables.
#   II.  Process data: plot the group gaps and levels.
#   III. Save outputs: write the gap figures, then the level figures.
#
#' @Date: November 2026
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
dir_imputed <- here::here("data", "processed", "idw_regressions_imputed")
dir_figures <- here::here("results", "figures", "imputation")

# Set the files containing group means and regression estimates with imputed readings
file_ci_imputed      <- here::here(dir_imputed,
  sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
          imputed_exposure_buffer_km, imputation_year))
file_summary_imputed <- here::here(dir_imputed,
  sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
          imputed_exposure_buffer_km, imputation_year))

# Read the education estimates and group means
ci_imputed      <- data.table::as.data.table(arrow::read_parquet(file_ci_imputed))
summary_imputed <- data.table::as.data.table(arrow::read_parquet(file_summary_imputed))

# ========================================================================================
# II: Process data
# ========================================================================================
# Plot education-group gaps in exceedance hours, with confidence intervals
plots_ci_imputed <- build_exposure_ci_figures(ci_dt = ci_imputed[outcome != "avg"],
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# Plot mean PM concentrations by education group
plots_levels_imputed <- build_exposure_level_figures(sum_dt = summary_imputed,
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the gap figures, then the level figures, under their imputation names
files_ci_imputed     <- save_exposure_plot_family(plots_ci_imputed, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = imputation_year, imputed = TRUE)
files_levels_imputed <- save_exposure_plot_family(plots_levels_imputed,
  out_dir = dir_figures, paper_files = exposure_paper_files, year = imputation_year,
  imputed = TRUE)
