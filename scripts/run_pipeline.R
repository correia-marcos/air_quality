# ============================================================================================
# IDB: Air monitoring — transitional runner; targets cutover awaits acceptance
# ============================================================================================
#' @Goal: Master orchestrator to execute the entire data pipeline sequentially.
#
#' @Description: This script allows coauthors and reviewers to reproduce the entire
# project by running a single file top-to-bottom. It sources individual module scripts
# in the strict dependency order required for the data architecture.
#
#' @Summary:
#   I. City preparation.
#   II. Analysis and temporal preparation.
#   III. Figures and tables.
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# ============================================================================================
# I: City preparation
# ============================================================================================

# ============================================================================================
# Download Raw Data
# ============================================================================================
# WARNING: These scripts fetch large datasets. If data/raw/ is already
# populated, you should skip this section to save time and bandwidth.

# source(here::here("scripts", "download_data", "download_bogota_data.R"))
# source(here::here("scripts", "download_data", "download_cdmx_data.R"))
# source(here::here("scripts", "download_data", "download_santiago_data.R"))
# source(here::here("scripts", "download_data", "download_sao_paulo_data.R"))
# source(here::here("scripts", "download_data", "download_merra2_data.R"))

# ============================================================================================
# Process City Data
# ============================================================================================
# These scripts format the raw inputs into standardized structures.
# They do not depend on each other and can technically be run in any order here.

source(here::here("scripts", "process_data", "process_bogota_data.R"))
source(here::here("scripts", "process_data", "process_cdmx_data.R"))
source(here::here("scripts", "process_data", "process_santiago_data.R"))
source(here::here("scripts", "process_data", "process_sao_paulo_data.R"))

# ============================================================================================
# Generate Distance Matrices
# ============================================================================================
# Calculates distances between census tracts and monitoring stations.
# Depends entirely on the outputs generated in Step 1.


# ============================================================================================
# II: Analysis and temporal preparation
# ============================================================================================
source(here::here("scripts", "process_data", "generate_distance_matrices.R"))

# ============================================================================================
# Outlier Detection
# ============================================================================================
# Flags anomalous pollution readings based on pre-defined thresholds.

source(here::here("scripts", "process_data", "detect_outliers.R"))

# ============================================================================================
# Estimate IDW Exposure
# ============================================================================================
# Estimates exposure using Inverse Distance Weighting.
# Can utilize outlier flags from Step 3 for sensitivity analysis.

source(here::here("scripts", "process_data", "estimate_idw.R"))

# ============================================================================================
# Exposure Regressions
# ============================================================================================
# Turns the geo-level exposure of Step 4 into quintile/decile gaps relative to the
# top group, with clustered confidence intervals. Produces the inputs of Figures 7-8.

source(here::here("scripts", "process_data", "estimate_exposure.R"))

# ============================================================================================
# Descriptive Tables
# ============================================================================================
# Station counts, missing-data shares, WHO exceedances and the census summary. Needs the
# cleaned panels from Step 3, the distance matrices from Step 2 and the processed census.

source(here::here("scripts", "process_data", "compute_descriptive_tables.R"))
source(here::here("scripts", "process_data", "compute_station_scatter_inputs.R"))
source(here::here("scripts", "process_data", "compute_distance_band_descriptives.R"))
source(here::here("scripts", "process_data", "impute_missing_hourly.R"))
source(here::here("scripts", "process_data", "estimate_exposure_imputed.R"))

# ============================================================================================
# Station-only temporal preparation
# ============================================================================================
# Average current observed metropolitan station measurements onto a complete hourly grid.
source(here::here("scripts", "process_data", "prepare_station_hourly.R"))

# ============================================================================================
# Tables & Images
# ============================================================================================
# Final publication artefacts. These read only from data/processed/ or data/interim/.


# ============================================================================================
# III: Figures and tables
# ============================================================================================
source(here::here("scripts", "tables_images", "render_station_tables.R"))
source(here::here("scripts", "tables_images", "render_missing_tables.R"))
source(here::here("scripts", "tables_images", "render_census_tables.R"))
source(here::here("scripts", "tables_images", "render_exposure_tables.R"))
source(here::here("scripts", "tables_images", "generate_exposure_plots.R"))
source(here::here("scripts", "tables_images", "plot_station_monitoring_figures.R"))
source(here::here("scripts", "tables_images", "figure_station_scatter.R"))
source(here::here("scripts", "tables_images", "figure_population_density_maps.R"))
source(here::here("scripts", "tables_images", "figure_pollution_quintile_maps.R"))
source(here::here("scripts", "tables_images", "figure_imputation_diagnostics.R"))
source(here::here("scripts", "tables_images", "figure_station_temporal.R"))
source(here::here("scripts", "tables_images", "figure_kernel_distributions.R"))
source(here::here("scripts", "tables_images", "figure_quintile_kernel_distributions.R"))


# Optional methodological analysis: `make resolution`. Re-keys the Bogota manzana exposure to
# five nested census geographies and re-estimates the quintile gap at each. Not part of `all`:
# the manuscript does not yet cite it, and it depends only on the exposure stage.
# source(here::here("scripts", "process_data", "build_bogota_localidad_crosswalk.R"))
# source(here::here("scripts", "process_data", "estimate_resolution_sensitivity.R"))
# source(here::here("scripts", "tables_images", "figure_resolution_sensitivity.R"))

# Optional supporting analyses: run `make merra2` after acquiring its extra inputs.
# source(here::here("scripts", "process_data", "generate_panel_air_quality.R"))
# source(here::here("scripts", "process_data", "prepare_station_temporal.R"))
# source(here::here("scripts", "process_data", "process_merra2_panels.R"))
# source(here::here("scripts", "tables_images", "figure_merra2_vs_stations.R"))
# source(here::here("scripts", "tables_images", "figure_aerosol_composition.R"))

# Optional context maps: `make context-maps` (terrain tiles may require networking).
# source(here::here("scripts", "tables_images", "figure_study_area_maps.R"))
