# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute annual PM concentrations vs. WHO AQG (2021) for each city-year.
#
# @Description: This script uses `compute_who_exceedances()` to derive per city-year-pollutant 
# averages. Crucially, uses a mean-of-means approach (averaging station means) rather 
# than a pooled grand mean to prevent spatial bias caused by differential sensor 
# uptime. Reads from the post-outlier detection datasets.
#
# @Summary: 
#   I.   Import data: Define paths for cleaned Arrow datasets across all cities.
#   II.  Process: Loop through each city, applying the exceedance function.
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
# Explicitly point to the CLEANED datasets from the outlier detection step
dir_clean          <- here::here("data", "processed", "outlier_detection")
outdir_exceedances <- here::here("data", "processed", "who_exceedances")

# Arrow (cleaned hourly) datasets — one hive-partitioned directory per city
arrow_bogota_dir   <- here::here(dir_clean, "bogota_metro_clean")
arrow_cdmx_dir     <- here::here(dir_clean, "cdmx_metro_clean")
arrow_santiago_dir <- here::here(dir_clean, "santiago_metro_clean")
arrow_sp_dir       <- here::here(dir_clean, "sao_paulo_metro_clean")

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
city_tables <- list()

# Loop through each city to calculate exceedances per pollutant
for (city in names(arrow_dirs)) {
  
  if (!dir.exists(arrow_dirs[[city]])) {
    message("[", city, "] Arrow dataset not found — skipping.")
    next
  }
  
  # Calculate exceedances using the mean-of-means spatial aggregation
  city_tables[[city]] <- compute_who_exceedances(
    arrow_dir   = arrow_dirs[[city]],
    city_label  = city,
    pollutants  = c("pm10", "pm25"),
    year_filter = NULL  # Process all available years
  )
}

if (length(city_tables) == 0L) {
  stop("No city Arrow datasets were found — nothing to aggregate.")
}

# Combine all individual city results into a single long-format data.table
all_cities <- data.table::rbindlist(city_tables, fill = TRUE)

# ============================================================================================
# III: Save
# ============================================================================================
# Create the output folder if it does not yet exist
dir.create(outdir_exceedances, recursive = TRUE, showWarnings = FALSE)

# Save the finalized dataset as a Parquet file
save_raw_data_tidy_formatted(
  data          = all_cities,
  out_dir       = outdir_exceedances,
  out_name      = "who_exceedances_all_cities",
  write_rds     = FALSE,
  write_parquet = TRUE,
  write_csv_gz  = FALSE
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
