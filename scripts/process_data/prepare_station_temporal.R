# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare optional satellite and ground-station comparisons.
#' @Description: Convert saved aerosol means and join current observed city-hour summaries.
# This is separate from manuscript figures and uses no historical balanced station panels.
#' @Summary:
#   I. Import the aerosol panels and station-only hourly checkpoints.
#   II. Convert aerosol masses and join the two sources.
#   III. Write the converted and joined CSV checkpoints.
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
source(here::here("src", "general_utilities", "process", "merra2.R"))
source(here::here("config", "analysis_settings.R"))
dir_panels <- here::here("data", "interim", "cities_m2_aerosols")
dir_hourly <- here::here("data", "processed", "station_hourly")
outdir_pm25 <- here::here("data", "processed", "merra2_pm25")
outdir_series <- here::here("data", "processed", "merra2_stations_pm25")

panel_bogota <- read.csv(file.path(dir_panels, "bogota_panel.csv"))
hourly_bogota <- arrow::read_parquet(file.path(dir_hourly,
  sprintf("bogota_pm25_%d.parquet", analysis_year)))

panel_cdmx <- read.csv(file.path(dir_panels, "ciudad_mexico_panel.csv"))
hourly_cdmx <- arrow::read_parquet(file.path(dir_hourly,
  sprintf("cdmx_pm25_%d.parquet", analysis_year)))

panel_santiago <- read.csv(file.path(dir_panels, "santiago_panel.csv"))
hourly_santiago <- arrow::read_parquet(file.path(dir_hourly,
  sprintf("santiago_pm25_%d.parquet", analysis_year)))

panel_sp <- read.csv(file.path(dir_panels, "sao_paulo_panel.csv"))
hourly_sp <- arrow::read_parquet(file.path(dir_hourly,
  sprintf("sao_paulo_pm25_%d.parquet", analysis_year)))

# ========================================================================================
# II: Process data
# ========================================================================================
pm25_bogota <- convert_and_add_pm25(df = panel_bogota)
series_bogota <- join_station_hourly_merra2(hourly = hourly_bogota, merra2 = pm25_bogota)
pm25_cdmx <- convert_and_add_pm25(df = panel_cdmx)
series_cdmx <- join_station_hourly_merra2(hourly = hourly_cdmx, merra2 = pm25_cdmx)
pm25_santiago <- convert_and_add_pm25(df = panel_santiago)
series_santiago <- join_station_hourly_merra2(hourly = hourly_santiago, merra2 = pm25_santiago)
pm25_sp <- convert_and_add_pm25(df = panel_sp)
series_sp <- join_station_hourly_merra2(hourly = hourly_sp, merra2 = pm25_sp)

# ========================================================================================
# III: Save outputs
# ========================================================================================
dir.create(outdir_pm25, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_series, recursive = TRUE, showWarnings = FALSE)
write.csv(pm25_bogota, file.path(outdir_pm25, "bogota_pm25.csv"), row.names = FALSE)
write.csv(series_bogota, file.path(outdir_series,
  "bogota_pm25_stations_merra2.csv"), row.names = FALSE)
write.csv(pm25_cdmx, file.path(outdir_pm25, "ciudad_mexico_pm25.csv"), row.names = FALSE)
write.csv(series_cdmx, file.path(outdir_series,
  "ciudad_mexico_pm25_stations_merra2.csv"), row.names = FALSE)
write.csv(pm25_santiago, file.path(outdir_pm25, "santiago_pm25.csv"), row.names = FALSE)
write.csv(series_santiago, file.path(outdir_series,
  "santiago_pm25_stations_merra2.csv"), row.names = FALSE)
write.csv(pm25_sp, file.path(outdir_pm25, "sao_paulo_pm25.csv"), row.names = FALSE)
write.csv(series_sp, file.path(outdir_series,
  "sao_paulo_pm25_stations_merra2.csv"), row.names = FALSE)
