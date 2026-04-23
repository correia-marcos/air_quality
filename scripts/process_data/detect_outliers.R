# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: 
# 
# @Description: 
# 
# @Summary: 
#   I.   
#   II.  
#   III. 
# 
# @Date: January 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define the output general folders
dir_pollution    <- here::here("data", "raw", "monitoring_stations")
outdir_distances <- here::here("data", "processed", "distances_matrices")
outdir_results   <- here::here("data", "processed", "outlier_detection")

# Arrow (raw hourly) datasets — one hive-partitioned directory per city.
arrow_bogota_dir   <- here::here(dir_pollution, "bogota_metro_dataset")
arrow_cdmx_dir     <- here::here(dir_pollution, "cdmx_metro_dataset")
arrow_santiago_dir <- here::here(dir_pollution, "santiago_metro_dataset")
arrow_sp_dir       <- here::here(dir_pollution, "sao_paulo_metro_dataset")

# Station-to-station distance Parquets produced by generate_distances_matrices.R
dist_bogota   <- here::here(outdir_distances, "bogota_2018",
                            "matrix_station_distances.parquet")
dist_cdmx     <- here::here(outdir_distances, "cdmx_2020",
                            "matrix_station_distances.parquet")
dist_santiago <- here::here(outdir_distances, "santiago_2024",
                            "matrix_station_distances.parquet")
dist_sp       <- here::here(outdir_distances, "sao_paulo_2010",
                            "matrix_station_distances.parquet")

# ============================================================================================
# II and III: Process and save matrix as parquet
# ============================================================================================
# Create the folder of the results, if not yet created
dir.create(outdir_results, recursive = TRUE, showWarnings = FALSE)

# Apply function to calculate outliers for Bogota
bogota_cleaned <- detect_pollution_outliers(
  arrow_dir           = arrow_bogota_dir,
  station_dist_path   = dist_bogota,
  on_missing_temporal = "continue",
  on_missing_neighbor = "second",
  out_dir             = outdir_results,
  out_name            = "bogota_metro"
)

# Apply function to calculate outliers for CDMX
cdmx_cleaned <- detect_pollution_outliers(
  arrow_dir           = arrow_cdmx_dir,
  station_dist_path   = dist_cdmx,
  on_missing_temporal = "continue",
  on_missing_neighbor = "second",
  out_dir             = outdir_results,
  out_name            = "cdmx_metro"
)

# Apply function to calculate outliers for Santiago
santiago_cleaned <- detect_pollution_outliers(
  arrow_dir           = arrow_santiago_dir,
  station_dist_path   = dist_santiago,
  on_missing_temporal = "continue",
  on_missing_neighbor = "second",
  out_dir             = outdir_results,
  out_name            = "santiago_metro"
)

# Apply function to calculate outliers for São Paulo
sp_cleaned <- detect_pollution_outliers(
  arrow_dir           = arrow_sp_dir,
  station_dist_path   = dist_sp,
  on_missing_temporal = "continue",
  on_missing_neighbor = "second",
  out_dir             = outdir_results,
  out_name            = "sao_paulo_metro"
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")