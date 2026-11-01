# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare observed metropolitan hourly PM2.5 series.
#
#' @Description: Summarize cleaned station partitions without satellite joins or imputation.
#
#' @Summary:
#   I.   Import data: source definitions, declare paths and open datasets.
#   II.  Process data: retain hourly city means and reporting counts.
#   III. Save outputs: write the four small Parquet checkpoints.
#
#' @Date: October 2026
#' @Author: Project contributors
# ========================================================================================


# ========================================================================================
# I: Import data
# ========================================================================================
source(here::here("src", "general_utilities", "process", "station_temporal.R"))
source(here::here("config", "analysis_settings.R"))

dir_stations <- here::here("data", "processed", "monitoring_stations_outliers")
outdir_hourly <- here::here("data", "processed", "station_hourly")
file_bogota <- here::here(outdir_hourly,
  sprintf("bogota_pm25_%d.parquet", analysis_year))
file_cdmx <- here::here(outdir_hourly,
  sprintf("cdmx_pm25_%d.parquet", analysis_year))
file_santiago <- here::here(outdir_hourly,
  sprintf("santiago_pm25_%d.parquet", analysis_year))
file_sao_paulo <- here::here(outdir_hourly,
  sprintf("sao_paulo_pm25_%d.parquet", analysis_year))

# Open file-backed station datasets; only the requested year is summarized.
stations_bogota <- arrow::open_dataset(here::here(dir_stations, "bogota_metro_clean"))
stations_cdmx <- arrow::open_dataset(here::here(dir_stations, "cdmx_metro_clean"))
stations_santiago <- arrow::open_dataset(here::here(dir_stations, "santiago_metro_clean"))
stations_sao_paulo <- arrow::open_dataset(here::here(dir_stations, "sao_paulo_metro_clean"))

# ========================================================================================
# II: Process data
# ========================================================================================
hourly_bogota <- summarize_city_hourly_pm25(station_data = stations_bogota,
  year = analysis_year)

hourly_cdmx <- summarize_city_hourly_pm25(station_data = stations_cdmx,
  year = analysis_year)

hourly_santiago <- summarize_city_hourly_pm25(station_data = stations_santiago,
  year = analysis_year)

hourly_sao_paulo <- summarize_city_hourly_pm25(station_data = stations_sao_paulo,
  year = analysis_year)


# ========================================================================================
# III: Save outputs
# ========================================================================================
file_bogota <- write_station_hourly(hourly = hourly_bogota, file = file_bogota)
file_cdmx <- write_station_hourly(hourly = hourly_cdmx, file = file_cdmx)
file_santiago <- write_station_hourly(hourly = hourly_santiago, file = file_santiago)
file_sao_paulo <- write_station_hourly(hourly = hourly_sao_paulo, file = file_sao_paulo)
