# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Fill missing station-hour readings by OLS on the other stations' readings.
#
#' @Description: Builds the imputed panels behind the paper's robustness specification.
# For each city, every station's PM10 and PM2.5 series is regressed on the contemporaneous
# readings of the other stations, on indicators for those being missing, and on month,
# day-of-week and hour effects with their interactions; the fitted values fill the gaps.
# Reads the outlier-cleaned panels and writes one hive-partitioned Arrow dataset per city
# to data/processed/imputed_ols/, plus the 2023 fitted values that the imputation
# diagnostics figures plot.
#
#' @Summary:
#   I.   Import data: define the cleaned input panels and the output folder.
#   II.  Process: impute each city, one at a time.
#   III. Summary: report how many station-hours each city gained.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_pollution  <- here::here("data", "processed", "monitoring_stations_outliers")
outdir_imputed <- here::here("data", "processed", "imputed_ols")

# Define the cleaned Arrow dataset paths
arrow_bogota   <- here::here(dir_pollution, "bogota_metro_clean")
arrow_cdmx     <- here::here(dir_pollution, "cdmx_metro_clean")
arrow_santiago <- here::here(dir_pollution, "santiago_metro_clean")
arrow_sp       <- here::here(dir_pollution, "sao_paulo_metro_clean")

# The paper reports the imputed specification for 2023 only, so that is the year the
# panels are built for and the year whose fitted values the diagnostics figures plot.
analysis_year <- 2023L

# ============================================================================================
# II: Process and save
# ============================================================================================
dir.create(outdir_imputed, recursive = TRUE, showWarnings = FALSE)

# CDMX is keyed on the CETESB-style station code because two of its station names share a
# code; the other three cities identify a station by its name.
message("\n--- Processing Bogota ---")
res_bogota <- impute_missing_hourly_ols(
  arrow_dir  = arrow_bogota,
  out_dir    = outdir_imputed,
  out_name   = "bogota_imputed",
  pollutants = c("pm10", "pm25"),
  id_col     = "station",
  years      = analysis_year,
  diag_year  = analysis_year)

message("\n--- Processing CDMX ---")
res_cdmx <- impute_missing_hourly_ols(
  arrow_dir  = arrow_cdmx,
  out_dir    = outdir_imputed,
  out_name   = "cdmx_imputed",
  pollutants = c("pm10", "pm25"),
  id_col     = "station_code",
  years      = analysis_year,
  diag_year  = analysis_year)

message("\n--- Processing Santiago ---")
res_santiago <- impute_missing_hourly_ols(
  arrow_dir  = arrow_santiago,
  out_dir    = outdir_imputed,
  out_name   = "santiago_imputed",
  pollutants = c("pm10", "pm25"),
  id_col     = "station",
  years      = analysis_year,
  diag_year  = analysis_year)

message("\n--- Processing Sao Paulo ---")
res_sp <- impute_missing_hourly_ols(
  arrow_dir  = arrow_sp,
  out_dir    = outdir_imputed,
  out_name   = "sao_paulo_imputed",
  pollutants = c("pm10", "pm25"),
  id_col     = "station",
  years      = analysis_year,
  diag_year  = analysis_year)

# ============================================================================================
# III: Summary
# ============================================================================================
res_bogota$per_poll[,   city := "Bogota"]
res_cdmx$per_poll[,     city := "CDMX"]
res_santiago$per_poll[, city := "Santiago"]
res_sp$per_poll[,       city := "Sao Paulo"]

summary_tbl <- data.table::rbindlist(
  list(res_bogota$per_poll, res_cdmx$per_poll,
       res_santiago$per_poll, res_sp$per_poll), fill = TRUE)

message("\n--- Imputation summary: station-hours filled ---")
print(summary_tbl[, .(city, pollutant, n_imputed)])

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
