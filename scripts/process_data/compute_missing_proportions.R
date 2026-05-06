# ====================================================================================
# IDB: Air monitoring
# ====================================================================================
# @Goal: Produce structural and algorithmic missing-data tables for each city.
#
# @Description: This script runs `compute_missing_proportions()` against both the raw and 
# cleaned Arrow pollution datasets. This dual-pass approach separates hardware failures 
# (structural missingness) from outlier removal (algorithmic missingness). Writes 
# Parquet files per city, per dimension (station/month/hour), per data stage.
#
# @Summary: 
#   I.   Import data: Define paths for both raw and cleaned Arrow datasets.
#   II.  Process Raw: Compute structural missingness from the raw datasets.
#   III. Process Clean: Compute post-outlier missingness from the cleaned datasets.
#
# @Date: April 2026
# @Author: Marcos
# ====================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ====================================================================================
# I: Import data
# ====================================================================================
# Define the general input and output folders
dir_raw        <- here::here("data", "raw", "monitoring_stations")
dir_clean      <- here::here("data", "processed", "outlier_detection")
outdir_missing <- here::here("data", "processed", "missing_proportions")

# Arrow (raw hourly) datasets — structural missingness
arrow_raw_dirs <- list(
  bogota          = here::here(dir_raw, "bogota_metro_dataset"),
  cdmx            = here::here(dir_raw, "cdmx_metro_dataset"),
  santiago        = here::here(dir_raw, "santiago_metro_dataset"),
  sao_paulo_metro = here::here(dir_raw, "sao_paulo_metro_dataset")
)

# Arrow (cleaned hourly) datasets — algorithmic missingness (post-outliers)
arrow_clean_dirs <- list(
  bogota          = here::here(dir_clean, "bogota_metro_clean"),
  cdmx            = here::here(dir_clean, "cdmx_metro_clean"),
  santiago        = here::here(dir_clean, "santiago_metro_clean"),
  sao_paulo_metro = here::here(dir_clean, "sao_paulo_metro_clean")
)

# ====================================================================================
# II: Process Raw Data (Structural Missingness)
# ====================================================================================
# Create the output folder if it does not yet exist
dir.create(outdir_missing, recursive = TRUE, showWarnings = FALSE)

# Loop through each city to calculate base missingness (hardware/transmission failures)
for (city in names(arrow_raw_dirs)) {
  
  if (!dir.exists(arrow_raw_dirs[[city]])) {
    message("[", city, "] Raw Arrow dataset not found — skipping.")
    next
  }
  
  compute_missing_proportions(
    arrow_dir   = arrow_raw_dirs[[city]],
    pollutants  = c("pm10", "pm25", "o3", "no2", "co"),
    dims        = c("station", "month", "hour", "day_of_week"),
    year_filter = 2023,
    out_dir     = outdir_missing,
    out_name    = paste0(city, "_raw")
  )
}

# ====================================================================================
# III: Process Cleaned Data (Algorithmic Missingness)
# ====================================================================================
# Loop through each city to calculate post-outlier missingness 
for (city in names(arrow_clean_dirs)) {
  
  if (!dir.exists(arrow_clean_dirs[[city]])) {
    message("[", city, "] Clean Arrow dataset not found — skipping.")
    next
  }
  
  compute_missing_proportions(
    arrow_dir   = arrow_clean_dirs[[city]],
    pollutants  = c("pm10", "pm25", "o3", "no2", "co"),
    dims        = c("station", "month", "hour", "day_of_week"),
    year_filter = 2023,
    out_dir     = outdir_missing,
    out_name    = paste0(city, "_clean")
  )
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
