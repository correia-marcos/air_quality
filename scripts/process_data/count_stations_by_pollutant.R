# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Count monitoring stations reporting each pollutant in each city-year.
# 
# @Description: Processes hive-partitioned Arrow datasets of raw ground station 
# data to compute the number of active stations per pollutant per year. Utilizes 
# out-of-core DuckDB SQL with algebraic balancing to calculate rigorous completeness 
# percentages. Feeds downstream table-generation scripts.
# 
# @Summary: 
#   I.   Import data: Define paths for raw Arrow datasets across all cities.
#   II.  Process: Loop through each city, applying the summarization function.
#   III. Save: Combine all city outputs into a long format and export as Parquet.
# 
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define the general input and output folders
dir_pollution <- here::here("data", "raw", "monitoring_stations")
outdir_counts <- here::here("data", "processed", "station_counts")

# Arrow (raw hourly) datasets — one hive-partitioned directory per city
arrow_bogota_dir   <- here::here(dir_pollution, "bogota_metro_dataset")
arrow_cdmx_dir     <- here::here(dir_pollution, "cdmx_metro_dataset")
arrow_santiago_dir <- here::here(dir_pollution, "santiago_metro_dataset")
arrow_sp_dir       <- here::here(dir_pollution, "sao_paulo_metro_dataset")

# Group directories into a named list for sequential processing
arrow_dirs <- list(
  bogota          = arrow_bogota_dir,
  cdmx            = arrow_cdmx_dir,
  santiago        = arrow_santiago_dir,
  sao_paulo_metro = arrow_sp_dir
)

# ============================================================================================
# II: Compute
# ============================================================================================
long_tables <- list()

# Loop through each city to calculate station counts per pollutant
for (city in names(arrow_dirs)) {
  if (!dir.exists(arrow_dirs[[city]])) {
    message("[", city, "] Arrow dataset not found — skipping.")
    next
  }
  # Calculate coverage using algebraic balancing for accurate valid percentages
  res <- summarize_stations_by_pollutant(
    arrow_dir     = arrow_dirs[[city]],
    city_label    = city,
    pollutants    = c("pm10", "pm25", "o3", "no2", "co"),
    min_valid_pct = 0.0
  )
  
  long_tables[[city]] <- res$long
}

# Combine all individual city results into a single long-format data.table
stations_long <- data.table::rbindlist(long_tables, fill = TRUE)

# ============================================================================================
# III: Save
# ============================================================================================
# Create the output folder if it does not yet exist
dir.create(outdir_counts, recursive = TRUE, showWarnings = FALSE)

# Save the finalized dataset as a Parquet file
save_raw_data_tidy_formatted(
  data          = stations_long,
  out_dir       = outdir_counts,
  out_name      = "stations_by_pollutant",
  write_rds     = FALSE,
  write_parquet = TRUE
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")