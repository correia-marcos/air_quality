# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Prepare the preserved city series for temporal and satellite figures.
#
#' @Description: Convert city MERRA-2 panels and join the original balanced station inputs.
# Preserve MERRA-2 timestamp support, station means and São Paulo metro selection.
# Write inspectable CSVs shared by manuscript temporal and optional satellite analyses.
#
#' @Summary:
#   I.   Read city aerosol panels and original balanced station inputs.
#   II.  Convert aerosol panels to PM2.5.
#   III. Merge the station means onto MERRA-2 timestamps and save.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_panels   <- here::here("data", "interim", "cities_m2_aerosols")
dir_stations <- here::here("data", "raw", "pollution_ground_stations")

outdir_pm25         <- here::here("data", "processed", "merra2_pm25")
outdir_m2_stations  <- here::here("data", "processed", "merra2_stations_pm25")

# City aerosol panels, written by generate_panel_air_quality.R
bogota_panel        <- read.csv(file.path(dir_panels, "bogota_panel.csv"))
ciudad_mexico_panel <- read.csv(file.path(dir_panels, "ciudad_mexico_panel.csv"))
santiago_panel      <- read.csv(file.path(dir_panels, "santiago_panel.csv"))
sao_paulo_panel     <- read.csv(file.path(dir_panels, "sao_paulo_panel.csv"))

# Ground-station readings. Each city names its datetime and PM2.5 columns differently, so
# those names are passed explicitly in section III rather than harmonized here.
bogota_stations        <- readRDS(file.path(
  dir_stations, "Bogota", "pollution_pm10_pm25_data_balanced_2023.rds"))
ciudad_mexico_stations <- readRDS(file.path(
  dir_stations, "Mexico_city", "pollution_pm25_data_balanced_2023.rds"))
santiago_stations      <- readRDS(file.path(
  dir_stations, "Santiago", "pollution_data_balanced_2023_pm25.rds"))
sao_paulo_stations     <- readRDS(file.path(
  dir_stations, "Sao_paulo", "pollution_data_balanced_2023_pm25.rds"))

# Sao Paulo's station file covers the whole state; section III cuts it to the metro area.
stations_in_sp_metro <- sf::st_read(here::here(
  "data", "raw", "cities_shapefiles", "Sao_Paulo_metro_stations"))

# ============================================================================================
# II: Convert city aerosol panels to PM2.5
# ============================================================================================
dir.create(outdir_pm25, recursive = TRUE, showWarnings = FALSE)

bogota_pm25        <- convert_and_add_pm25(bogota_panel)
ciudad_mexico_pm25 <- convert_and_add_pm25(ciudad_mexico_panel)
santiago_pm25      <- convert_and_add_pm25(santiago_panel)
sao_paulo_pm25     <- convert_and_add_pm25(sao_paulo_panel)

write.csv(bogota_pm25, file.path(outdir_pm25, "bogota_pm25.csv"), row.names = FALSE)
write.csv(ciudad_mexico_pm25, file.path(outdir_pm25, "ciudad_mexico_pm25.csv"),
          row.names = FALSE)
write.csv(santiago_pm25, file.path(outdir_pm25, "santiago_pm25.csv"), row.names = FALSE)
write.csv(sao_paulo_pm25, file.path(outdir_pm25, "sao_paulo_pm25.csv"), row.names = FALSE)

# ============================================================================================
# III: Merge with ground stations and save
# ============================================================================================
dir.create(outdir_m2_stations, recursive = TRUE, showWarnings = FALSE)

# Keep only the Sao Paulo stations that sit inside the metro area.
sao_paulo_stations <- sao_paulo_stations %>%
  filter(station_code %in% stations_in_sp_metro$sttn_cd)

bogota_pollution <- combine_station_merra2_pm25(
  station_df           = bogota_stations,
  station_datetime_col = "datetime",
  station_pm25_col     = "pm25",
  merra2_df            = bogota_pm25)

ciudad_mexico_pollution <- combine_station_merra2_pm25(
  station_df           = ciudad_mexico_stations,
  station_datetime_col = "datetime",
  station_pm25_col     = "pm25",
  merra2_df            = ciudad_mexico_pm25)

santiago_pollution <- combine_station_merra2_pm25(
  station_df           = santiago_stations,
  station_datetime_col = "date2_hour",
  station_pm25_col     = "pm25_validated",
  merra2_df            = santiago_pm25)

sao_paulo_pollution <- combine_station_merra2_pm25(
  station_df           = sao_paulo_stations,
  station_datetime_col = "datetime",
  station_pm25_col     = "pm25",
  merra2_df            = sao_paulo_pm25)

write.csv(bogota_pollution,
          file.path(outdir_m2_stations, "bogota_pm25_stations_merra2.csv"),
          row.names = FALSE)
write.csv(ciudad_mexico_pollution,
          file.path(outdir_m2_stations, "ciudad_mexico_pm25_stations_merra2.csv"),
          row.names = FALSE)
write.csv(santiago_pollution,
          file.path(outdir_m2_stations, "santiago_pm25_stations_merra2.csv"),
          row.names = FALSE)
write.csv(sao_paulo_pollution,
          file.path(outdir_m2_stations, "sao_paulo_pm25_stations_merra2.csv"),
          row.names = FALSE)

