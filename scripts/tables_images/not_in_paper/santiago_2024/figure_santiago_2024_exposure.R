# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot Santiago exposure gaps and levels with the 2024 census communes.
#
#' @Description: Read the education tables saved by estimate_santiago_2024_exposure.R and
# draw the exposure analysis's figures for each buffer: gaps in exceedance hours with
# confidence intervals, and mean concentrations by education quintile. The figures are
# not in the paper; they keep their santiago_comuna_2024 names in
# results/figures/not_in_paper/santiago_2024/.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read the tables.
#   II.  Process data: plot the group gaps and levels for each buffer.
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
dir_reg     <- here::here("data", "processed", "idw_regressions", "santiago_2024")
dir_figures <- here::here("results", "figures", "not_in_paper", "santiago_2024")

# Set the files with regression estimates and group means for each buffer
files_ci      <- here::here(dir_reg,
  sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
          santiago_2024_buffers_km, analysis_year))
files_summary <- here::here(dir_reg,
  sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
          santiago_2024_buffers_km, analysis_year))

# Read both buffers' estimates and group means
ci_2024      <- data.table::rbindlist(lapply(files_ci, arrow::read_parquet))
summary_2024 <- data.table::rbindlist(lapply(files_summary, arrow::read_parquet))

# ========================================================================================
# II: Process data
# ========================================================================================
# Plot education-group gaps in exceedance hours, with confidence intervals
plots_ci_2024 <- build_exposure_ci_figures(ci_dt = ci_2024[outcome != "avg"],
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# Plot mean PM concentrations by education group
plots_levels_2024 <- build_exposure_level_figures(sum_dt = summary_2024,
  tag = "education", city_labels = exposure_city_labels, city_files = exposure_city_files)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the gap figures, then the level figures
files_ci_2024     <- save_exposure_plot_family(plots_ci_2024, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
files_levels_2024 <- save_exposure_plot_family(plots_levels_2024, out_dir = dir_figures,
  paper_files = exposure_paper_files, year = analysis_year)
