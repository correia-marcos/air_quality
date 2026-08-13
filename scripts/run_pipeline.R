# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Master orchestrator to execute the entire data pipeline sequentially.
# 
# @Description: This script allows coauthors and reviewers to reproduce the entire 
# project by running a single file top-to-bottom. It sources individual module scripts 
# in the strict dependency order required for the data architecture.
# 
# @Summary:
#   0.   Download all raw data (Ground Stations, Census, MERRA-2)
#   1.   Process and format city-specific data
#   2.   Generate distance matrices
#   3.   Detect and flag outliers
#   4.   Estimate exposure (IDW)
#   5.   Exposure regressions
#   6.   Descriptive tables
#   7.   MERRA-2 satellite track
#   8.   Tables and figures
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# Load `here` to ensure pathing is robust regardless of the working directory
library(here)

# ============================================================================================
# Step 0: Download Raw Data
# ============================================================================================
# WARNING: These scripts fetch large datasets. If data/raw/ is already 
# populated, you should skip this section to save time and bandwidth.

# source(here::here("scripts", "download_data", "download_bogota_data.R"))
# source(here::here("scripts", "download_data", "download_cdmx_data.R"))
# source(here::here("scripts", "download_data", "download_santiago_data.R"))
# source(here::here("scripts", "download_data", "download_sao_paulo_data.R"))
# source(here::here("scripts", "download_data", "download_merra2_data.R"))

# ============================================================================================
# Step 1: Process City Data
# ============================================================================================
# These scripts format the raw inputs into standardized structures. 
# They do not depend on each other and can technically be run in any order here.

source(here::here("scripts", "process_data", "process_bogota_data.R"))
source(here::here("scripts", "process_data", "process_cdmx_data.R"))
source(here::here("scripts", "process_data", "process_santiago_data.R"))
source(here::here("scripts", "process_data", "process_sao_paulo_data.R"))

# ============================================================================================
# Step 2: Generate Distance Matrices
# ============================================================================================
# Calculates distances between census tracts and monitoring stations.
# Depends entirely on the outputs generated in Step 1.

source(here::here("scripts", "process_data", "generate_distance_matrices.R"))

# ============================================================================================
# Step 3: Outlier Detection
# ============================================================================================
# Flags anomalous pollution readings based on pre-defined thresholds.

source(here::here("scripts", "process_data", "detect_outliers.R"))

# ============================================================================================
# Step 4: Estimate IDW Exposure
# ============================================================================================
# Estimates exposure using Inverse Distance Weighting. 
# Can utilize outlier flags from Step 3 for sensitivity analysis.

source(here::here("scripts", "process_data", "estimate_idw.R"))

# ============================================================================================
# Step 5: Exposure Regressions
# ============================================================================================
# Turns the geo-level exposure of Step 4 into quintile/decile gaps relative to the
# top group, with clustered confidence intervals. Produces the inputs of Figures 7-8.

source(here::here("scripts", "process_data", "estimate_exposure.R"))

# ============================================================================================
# Step 6: Descriptive Tables
# ============================================================================================
# Station counts, missing-data shares, WHO exceedances and the census summary. Needs the
# cleaned panels from Step 3, the distance matrices from Step 2 and the processed census.

source(here::here("scripts", "process_data", "compute_descriptive_tables.R"))
source(here::here("scripts", "process_data", "compute_station_scatter_inputs.R"))

# ============================================================================================
# Step 7: MERRA-2 Satellite Track
# ============================================================================================
# Independent of the station pipeline above. generate_panel_air_quality.R reads the raw
# .nc4 granules and takes hours, so it stays commented out unless the panels are missing.

# source(here::here("scripts", "process_data", "generate_panel_air_quality.R"))
source(here::here("scripts", "process_data", "process_merra2_panels.R"))

# ============================================================================================
# Step 8: Tables & Images
# ============================================================================================
# Final publication artefacts. These read only from data/processed/ or data/interim/.

source(here::here("scripts", "tables_images", "render_paper_tables.R"))
source(here::here("scripts", "tables_images", "generate_exposure_plots.R"))
source(here::here("scripts", "tables_images", "figure_exposure_by_quintile.R"))
source(here::here("scripts", "tables_images", "plot_station_monitoring_figures.R"))
source(here::here("scripts", "tables_images", "figure_merra2_vs_stations.R"))
source(here::here("scripts", "tables_images", "figure_aerosol_composition.R"))
source(here::here("scripts", "tables_images", "figure_study_area_maps.R"))