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
#   I.   Process and format city-specific data
#   II.  Generate distance matrices
#   III. Detect and flag outliers
#   IV.  Estimate exposure (IDW)
#   V.   [Placeholders for remaining processing]
#   VI.  [Placeholders for Tables and Figures]
# 
# @Date: April 2026
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

source(here::here("scripts", "process_data", "generate_distances_matrices.R"))

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

source(here::here("scripts", "process_data", "estimate_idw_exposure.R"))

# ============================================================================================
# Step 5: Additional Processing (Placeholders)
# ============================================================================================
# Add remaining data merging, MERRA-2 comparisons, and panel generation here.

# source(here::here("scripts", "process_data", "generate_panel_air_quality.R"))
# source(here::here("scripts", "process_data", "compare_pm25_merra2_stations.R"))
# ...

# ============================================================================================
# Step 6: Tables & Images (Placeholders)
# ============================================================================================
# Generate final publication artefacts. 
# Ensure these scripts only read from data/processed/ or data/interim/.

# source(here::here("scripts", "tables_images", "figure_pollution_average_by_hours.R"))
# ...