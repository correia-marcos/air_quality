# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot exposure gaps and levels between socioeconomic groups at 5 km.
#
#' @Description: Read the 5 km tables of estimate_exposure_5km.R and draw the figures of
# scripts/tables_images/exposure/generate_exposure_plots.R at the robustness buffer:
# gaps in exceedance hours with confidence intervals and mean concentrations, by
# education and by income group. The figures are not printed in the manuscript.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read the tables.
#   II.  Process data: plot group gaps and levels for education and income.
#   III. Save outputs: write the education figures, then the income figures.
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
dir_reg     <- here::here("data", "processed", "idw_regressions")
dir_figures <- here::here("results", "figures", "robustness", "buffer_5km")

# Set the files containing group means and regression estimates at 5 km
file_ci_edu      <- here::here(dir_reg,
  sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
          robustness_buffer_km, analysis_year))
file_summary_edu <- here::here(dir_reg,
  sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
          robustness_buffer_km, analysis_year))
file_ci_inc      <- here::here(dir_reg,
  sprintf("exposure_ci_estimates_income_%dkm_%d.parquet",
          robustness_buffer_km, analysis_year))
file_summary_inc <- here::here(dir_reg,
  sprintf("exposure_group_summaries_income_%dkm_%d.parquet",
          robustness_buffer_km, analysis_year))

# Read the education and income estimates and group means
ci_edu      <- data.table::as.data.table(arrow::read_parquet(file_ci_edu))
summary_edu <- data.table::as.data.table(arrow::read_parquet(file_summary_edu))
ci_inc      <- data.table::as.data.table(arrow::read_parquet(file_ci_inc))
summary_inc <- data.table::as.data.table(arrow::read_parquet(file_summary_inc))

# ========================================================================================
# II: Process data
# ========================================================================================
# Plot education-group gaps in exceedance hours, with confidence intervals
plots_ci_edu_5km <- build_exposure_ci_figures(ci_dt = ci_edu[outcome != "avg"],
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# Plot mean PM concentrations by education group
plots_levels_edu_5km <- build_exposure_level_figures(sum_dt = summary_edu,
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# Plot the same outcomes by income group for the cities with income data
plots_ci_inc_5km <- build_exposure_ci_figures(ci_dt = ci_inc[outcome != "avg"],
  tag = "income", city_labels = exposure_city_labels, city_files = exposure_city_files)

plots_levels_inc_5km <- build_exposure_level_figures(sum_dt = summary_inc,
  tag = "income", city_labels = exposure_city_labels, city_files = exposure_city_files)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the education figures, then the income figures
files_ci_edu_5km     <- save_exposure_plot_family(plots_ci_edu_5km, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
files_levels_edu_5km <- save_exposure_plot_family(plots_levels_edu_5km,
  out_dir = dir_figures, paper_files = exposure_paper_files, year = analysis_year)

files_ci_inc_5km     <- save_exposure_plot_family(plots_ci_inc_5km, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
files_levels_inc_5km <- save_exposure_plot_family(plots_levels_inc_5km,
  out_dir = dir_figures, paper_files = exposure_paper_files, year = analysis_year)
