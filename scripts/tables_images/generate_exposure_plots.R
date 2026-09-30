# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot exposure levels and gaps between socioeconomic groups.
#
#' @Description: Read the saved observed and imputed exposure results.
# Build named plot lists for education, income and imputed education. Saving preserves
# the existing manuscript names and the additional buffer and census-vintage figures.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read saved summaries.
#   II.  Process data: plot group differences and levels for each specification.
#   III. Save outputs: write observed education, income and imputed education figures.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions, their required packages and the theme for the paper's plots
source(here::here("src", "general_utilities", "plot", "exposure_figures.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))
set_paper_theme()

# Set input folders and the output folder
dir_reg     <- here::here("data", "processed", "idw_regressions")
dir_imputed <- here::here("data", "processed", "idw_regressions_imputed")
dir_figures <- here::here("results", "figures")

# Set files containing exposure information
files_ci_edu         <- 
  here::here(dir_reg, sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
                              exposure_buffers_km, analysis_year))
files_summary_edu    <- 
  here::here(dir_reg, sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
                              exposure_buffers_km, analysis_year))
files_ci_inc         <- 
  here::here(dir_reg, sprintf("exposure_ci_estimates_income_%dkm_%d.parquet",
                              exposure_buffers_km, analysis_year))
files_summary_inc    <- 
  here::here(dir_reg, sprintf("exposure_group_summaries_income_%dkm_%d.parquet",
                              exposure_buffers_km, analysis_year))
file_ci_imputed      <- 
  here::here(dir_imputed, sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
                                  imputed_exposure_buffer_km, imputation_year))
file_summary_imputed <- 
  here::here(dir_imputed, sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
                                  imputed_exposure_buffer_km, imputation_year))

# Read both observed-data buffers for education.
ci_edu      <- data.table::rbindlist(lapply(files_ci_edu, arrow::read_parquet))
summary_edu <- data.table::rbindlist(lapply(files_summary_edu, arrow::read_parquet))

# Keep income optional for manual education-only runs.
ci_inc <- summary_inc <- NULL
has_income <- all(file.exists(c(files_ci_inc, files_summary_inc)))
if (has_income) {
  ci_inc      <- data.table::rbindlist(lapply(files_ci_inc, arrow::read_parquet))
  summary_inc <- data.table::rbindlist(lapply(files_summary_inc, arrow::read_parquet))
}
ci_imputed      <- data.table::as.data.table(arrow::read_parquet(file_ci_imputed))
summary_imputed <- data.table::as.data.table(arrow::read_parquet(file_summary_imputed))

# ========================================================================================
# II: Process data
# ========================================================================================
# CI figures retain the exceedance-hour outcomes; mean estimates appear in tables.
plots_ci_edu <- build_exposure_ci_figures(ci_dt = ci_edu[outcome != "avg"],
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

plots_levels_edu <- build_exposure_level_figures(sum_dt = summary_edu,
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# Plots for income
plots_ci_inc <- plots_levels_inc <- list()
if (has_income) {
  plots_ci_inc <- build_exposure_ci_figures(ci_dt = ci_inc[outcome != "avg"],
    tag = "income", city_labels = exposure_city_labels, city_files = exposure_city_files)

  plots_levels_inc <- build_exposure_level_figures(sum_dt = summary_inc,
    tag = "income", city_labels = exposure_city_labels, city_files = exposure_city_files)
}

# Plots for imputed values
plots_ci_imputed <- build_exposure_ci_figures(ci_dt = ci_imputed[outcome != "avg"],
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

plots_levels_imputed <- build_exposure_level_figures(sum_dt = summary_imputed,
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# ========================================================================================
# III: Save outputs
# ========================================================================================
save_exposure_plot_family(plots_ci_edu, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
save_exposure_plot_family(plots_levels_edu, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)

save_exposure_plot_family(plots_ci_inc, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
save_exposure_plot_family(plots_levels_inc, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)

save_exposure_plot_family(plots_ci_imputed, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = imputation_year, imputed = TRUE)
save_exposure_plot_family(plots_levels_imputed, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = imputation_year, imputed = TRUE)
