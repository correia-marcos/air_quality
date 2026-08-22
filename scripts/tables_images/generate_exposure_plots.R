# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Generate all exposure figures by socioeconomic group: regression gaps with
#   confidence intervals, and population-weighted concentration levels.
#
#' @Description: Reads the tidy artifacts written by estimate_exposure.R (the CI estimates 
#   and the raw group summaries, for education and for income) and draws two figure families: 
#   (1) regression gaps vs the base group with 95% CIs, per city/outcome/pollutant; 
#   (2) dual-axis PM10/PM2.5 mean concentration by group, per city. Plotting consumes the 
#   already-computed tables, so figures are always consistent with the regressions and summary
#   tables. The whole procedure runs once per buffer radius: 3 km is the paper's specification,
#   5 km is the robustness check, and the buffer appears in every output file name. Income
#   figures are produced only for the cities whose artifacts contain income (CDMX
#   and Sao Paulo).
#
#' @Summary:
#   I.  Import data: define paths, the buffer radii and the city labels that do
#   not depend on the buffer.
#   II. Process and save: one buffer at a time, read that buffer's artifacts and
#   draw both figure families per grouping.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_reg    <- here::here("data", "processed", "idw_regressions")
outdir_ci  <- here::here("results", "figures", "exposure_by_group", "ci")
outdir_lvl <- here::here("results", "figures", "exposure_by_group", "levels")

# The buffer radius and analysis year are the only choices that vary run to run
analysis_year <- 2023L
buffers_km    <- c(3L, 5L)

# City display labels and file-safe names (city matches the regression artifact)
city_labels <- c(Bogota = "Bogot\u00e1", CDMX = "Mexico City",
                 Santiago = "Santiago", `Sao Paulo` = "S\u00e3o Paulo",
                 `Santiago (comuna, 2024)` = "Santiago (commune, 2024 census)")
city_files  <- c(Bogota = "bogota", CDMX = "mexico_city",
                 Santiago = "santiago", `Sao Paulo` = "sao_paulo",
                 `Santiago (comuna, 2024)` = "santiago_comuna_2024")

# Create output folders
dir.create(outdir_ci, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_lvl, recursive = TRUE, showWarnings = FALSE)

# ============================================================================================
# II: Build and save figures
# ============================================================================================
# One full pass per buffer. Objects left in the environment hold the last buffer run.
for (buffer_km in buffers_km) {

  cat("\n=== Buffer:", buffer_km, "km ===\n")

  # Artifact paths written by estimate_exposure.R, one set per buffer
  ci_edu_pq      <- here::here(dir_reg, sprintf(
    "exposure_ci_estimates_education_%dkm_%d.parquet", buffer_km, analysis_year))
  ci_inc_pq      <- here::here(dir_reg, sprintf(
    "exposure_ci_estimates_income_%dkm_%d.parquet", buffer_km, analysis_year))
  summary_edu_pq <- here::here(dir_reg, sprintf(
    "exposure_group_summaries_education_%dkm_%d.parquet", buffer_km, analysis_year))
  summary_inc_pq <- here::here(dir_reg, sprintf(
    "exposure_group_summaries_income_%dkm_%d.parquet", buffer_km, analysis_year))

  # Stop early if the required education artifacts are missing
  if (!file.exists(ci_edu_pq)) {
    stop("CI estimates not found: ", ci_edu_pq)
  }
  if (!file.exists(summary_edu_pq)) {
    stop("Group summaries not found: ", summary_edu_pq)
  }

  # Read education artifacts eagerly for RStudio inspection
  ci_education      <- data.table::as.data.table(arrow::read_parquet(ci_edu_pq))
  summary_education <- data.table::as.data.table(arrow::read_parquet(summary_edu_pq))

  # Income artifacts cover only CDMX and Sao Paulo, so they are optional
  has_income     <- file.exists(ci_inc_pq) && file.exists(summary_inc_pq)
  ci_income      <- if (has_income) {
    data.table::as.data.table(arrow::read_parquet(ci_inc_pq))
  } else {
    NULL
  }
  summary_income <- if (has_income) {
    data.table::as.data.table(arrow::read_parquet(summary_inc_pq))
  } else {
    NULL
  }

  # Education figures, always produced
  save_exposure_ci_figures(ci_education, "education", outdir_ci, city_labels,
                           city_files, buffer_km = buffer_km)
  save_exposure_level_figures(summary_education, "education", outdir_lvl,
                              city_labels, city_files, buffer_km = buffer_km)

  # Income figures, only for the cities whose census carries income
  if (has_income) {
    save_exposure_ci_figures(ci_income, "income", outdir_ci, city_labels,
                             city_files, buffer_km = buffer_km)
    save_exposure_level_figures(summary_income, "income", outdir_lvl,
                                city_labels, city_files, buffer_km = buffer_km)
  }
}

cat("Saved exposure CI figures to:", outdir_ci, "\n")
cat("Saved exposure level figures to:", outdir_lvl, "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")
