# ====================================================================================
# IDB: Air monitoring
# ====================================================================================
# @Goal: Produce missing-data diagnostics and a LaTeX station-count table.
#
# @Description:
#   This script runs `compute_missing_proportions()` for PM10 and PM2.5 in each
#   city. It also creates a LaTeX table with the number of monitoring stations
#   with at least one valid PM10 or PM2.5 observation in the selected year.
#
# @Summary:
#   I.    Import functions and define paths.
#   II.   Define small script-level helpers.
#   III.  Compute structural missingness from raw hourly datasets.
#   IV.   Compute post-outlier missingness from cleaned hourly datasets.
#   V.    Count monitoring stations by pollutant and write a LaTeX table.
#
# @Date: April 2026
# @Updated_on: June 2026
# @Author: Marcos Paulo
# ====================================================================================

# Get all libraries and functions.
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ====================================================================================
# I: Import data
# ====================================================================================
# Define the general input and output folders.
dir_raw        <- here::here("data", "raw", "monitoring_stations")
dir_clean      <- here::here("data", "processed", "outlier_detection")
outdir_missing <- here::here("data", "processed", "missing_proportions")
outdir_tables  <- here::here("results", "tables", "station_counts")

# Main analysis parameters.
analysis_year <- 2023L
pollutants    <- c("pm10", "pm25")

# Arrow raw hourly datasets. These define the monitoring infrastructure table.
arrow_raw_dirs <- list(
  bogota          = here::here(dir_raw, "bogota_metro_dataset"),
  cdmx            = here::here(dir_raw, "cdmx_metro_dataset"),
  santiago        = here::here(dir_raw, "santiago_metro_dataset"),
  sao_paulo_metro = here::here(dir_raw, "sao_paulo_metro_dataset")
)

# Arrow cleaned hourly datasets. These define post-outlier missingness.
arrow_clean_dirs <- list(
  bogota          = here::here(dir_clean, "bogota_metro_clean"),
  cdmx            = here::here(dir_clean, "cdmx_metro_clean"),
  santiago        = here::here(dir_clean, "santiago_metro_clean"),
  sao_paulo_metro = here::here(dir_clean, "sao_paulo_metro_clean")
)

# Table labels and order used in the presentation/paper output.
city_labels <- data.table::data.table(
  city_id = c("santiago", "bogota", "cdmx", "sao_paulo_metro"),
  city    = c("Santiago", "Bogotá", "Mexico City", "São Paulo")
)

# ====================================================================================
# III: Process raw data: structural missingness
# ====================================================================================
dir.create(outdir_missing, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_tables, recursive = TRUE, showWarnings = FALSE)

for (city in names(arrow_raw_dirs)) {
  if (!dir.exists(arrow_raw_dirs[[city]])) {
    message("[", city, "] Raw Arrow dataset not found — skipping.")
    next
  }
  
  compute_missing_proportions(
    arrow_dir   = arrow_raw_dirs[[city]],
    pollutants  = pollutants,
    dims        = c("station", "month", "hour", "day_of_week"),
    year_filter = analysis_year,
    out_dir     = outdir_missing,
    out_name    = paste0(city, "_raw")
  )
}

# ====================================================================================
# IV: Process cleaned data: algorithmic missingness
# ====================================================================================
for (city in names(arrow_clean_dirs)) {
  if (!dir.exists(arrow_clean_dirs[[city]])) {
    message("[", city, "] Clean Arrow dataset not found — skipping.")
    next
  }
  
  compute_missing_proportions(
    arrow_dir   = arrow_clean_dirs[[city]],
    pollutants  = pollutants,
    dims        = c("station", "month", "hour", "day_of_week"),
    year_filter = analysis_year,
    out_dir     = outdir_missing,
    out_name    = paste0(city, "_clean")
  )
}

# ====================================================================================
# V: Generate LaTeX table: number of monitoring stations by pollutant
# ====================================================================================
station_count_list <- list()

for (city in names(arrow_raw_dirs)) {
  if (!dir.exists(arrow_raw_dirs[[city]])) {
    message("[", city, "] Raw Arrow dataset not found — skipping table count.")
    next
  }
  
  tmp <- count_stations_reporting(
    arrow_dir    = arrow_raw_dirs[[city]],
    pollutants   = pollutants,
    year_filter  = analysis_year,
    mem_gb       = 8
  )
  
  tmp[, city_id := city]
  station_count_list[[city]] <- tmp
}

station_counts <- data.table::rbindlist(station_count_list, fill = TRUE)
station_counts <- merge(city_labels, station_counts, by = "city_id", all.x = TRUE)

# Keep the presentation order: Santiago, Bogotá, Mexico City, São Paulo.
station_counts[, city_id := factor(city_id, levels = city_labels$city_id)]
data.table::setorder(station_counts, city_id)

station_counts <- station_counts[, .(city, pm10, pm25)]

# Save machine-readable and LaTeX versions of the station-count table.
arrow::write_parquet(
  station_counts,
  file.path(outdir_tables, "stations_by_pollutant_2023.parquet")
)

data.table::fwrite(
  station_counts,
  file.path(outdir_tables, "stations_by_pollutant_2023.csv")
)

write_station_count_latex(
  station_counts = station_counts,
  out_file = file.path(outdir_tables, "stations_by_pollutant_2023.tex")
)

cat("Script from the IDB project executed successfully in the Docker container!\n")
