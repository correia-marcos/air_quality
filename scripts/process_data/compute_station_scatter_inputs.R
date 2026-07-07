# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Build station-level data linking pollution outcomes to socioeconomic context.
#
# @Description:
#   This script computes station-level pollution outcomes for active monitoring stations
#   in 2023 and attaches socioeconomic characteristics from nearby or containing
#   geographic units. The resulting datasets are inputs for station-level scatterplots.
#
# @Summary:
#   I.   Import data: Define paths and read stations, geographic units, and census files.
#   II.  Process: Build station-level pollution-socioeconomic datasets for each city.
#
# @Date: June 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define general input and output folders
dir_cleaned    <- here::here("data", "processed", "outlier_detection")
dir_geospatial <- here::here("data", "raw", "geospatial_data")
dir_census     <- here::here("data", "interim", "census")
outdir_station <- here::here("data", "processed", "station_socio_exposure")

# Define cleaned Arrow dataset paths
arrow_bogota   <- here::here(dir_cleaned, "bogota_metro_clean")
arrow_cdmx     <- here::here(dir_cleaned, "cdmx_metro_clean")
arrow_santiago <- here::here(dir_cleaned, "santiago_metro_clean")
arrow_sp       <- here::here(dir_cleaned, "sao_paulo_metro_clean")

# Define station spatial files
gpkg_stations_bogota   <- here::here(dir_geospatial, "bogota",
                                     "bogota_2018_stations_buffer_metro.gpkg")
gpkg_stations_cdmx     <- here::here(dir_geospatial, "cdmx",
                                     "cdmx_stations_buffer_metro.gpkg")
gpkg_stations_santiago <- here::here(dir_geospatial, "santiago",
                                     "gran_santiago_stations_buffer_metro_2024.gpkg")
gpkg_stations_sp       <- here::here(dir_geospatial, "sao_paulo",
                                     "sao_paulo_stations_buffer_metro_2010.gpkg")

# Define geographic boundary files
gpkg_geo_bogota   <- here::here(dir_geospatial, "bogota",
                                "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_geo_cdmx     <- here::here(dir_geospatial, "cdmx",
                                "cdmx_area_metro.gpkg")
gpkg_geo_santiago <- here::here(dir_geospatial, "santiago",
                                "gran_santiago_area_2024.gpkg")
gpkg_geo_sp       <- here::here(dir_geospatial, "sao_paulo",
                                "sao_paulo_metro_2010_weighting_areas.gpkg")

# Define collapsed census files
census_bogota_csv   <- here::here(dir_census, "bogota_2018",
                                  "census_2018_metro_collapsed.csv")
census_cdmx_csv     <- here::here(dir_census, "cdmx_extended_2020",
                                  "collapse_metro_area_2020.csv")
census_santiago_csv <- here::here(dir_census, "santiago_2024",
                                  "census_santiago_collapsed_2024.csv")
census_sp_csv       <- here::here(dir_census, "sao_paulo_2010",
                                  "census_sp_collapsed_2010.csv")

# Read station spatial data
stations_bogota   <- sf::st_read(gpkg_stations_bogota, quiet = TRUE)
stations_cdmx     <- sf::st_read(gpkg_stations_cdmx, quiet = TRUE)
stations_santiago <- sf::st_read(gpkg_stations_santiago, quiet = TRUE)
stations_sp       <- sf::st_read(gpkg_stations_sp, quiet = TRUE)

# Read geographic units
geo_bogota   <- sf::st_read(gpkg_geo_bogota, quiet = TRUE)
geo_cdmx     <- sf::st_read(gpkg_geo_cdmx, quiet = TRUE)
geo_santiago <- sf::st_read(gpkg_geo_santiago, quiet = TRUE)
geo_sp       <- sf::st_read(gpkg_geo_sp, quiet = TRUE)

# Read collapsed census data
census_bogota   <- data.table::fread(census_bogota_csv)
census_cdmx     <- data.table::fread(census_cdmx_csv)
census_santiago <- data.table::fread(census_santiago_csv)
census_sp       <- data.table::fread(census_sp_csv)

# CDMX geo key: the gpkg stores CVE_MUN as the full 5-digit code ("09012"), but
census_cdmx[, CVE_MUN := canonical_geo_id(CVE_MUN, width = 5)]

# ============================================================================================
# II: Process station-level socioeconomic exposure data
# ============================================================================================
# Create output folder
dir.create(outdir_station, recursive = TRUE, showWarnings = FALSE)

# Define common WHO thresholds used in the paper
who_it <- list(
  pm10 = c(it1 = 150, it2 = 100),
  pm25 = c(it1 = 75,  it2 = 50))

# 1. Bogota
# --------------------------------------------------------------------------------------------
# Bogota uses very small geographic units, so we use a buffer-based context.
station_bogota <- build_station_scatter_inputs(
  arrow_dir         = arrow_bogota,
  stations_sf       = stations_bogota,
  geo_sf            = geo_bogota,
  census_col        = census_bogota,
  station_id_col    = "station_name",
  geo_id_col        = "GEO_ID",
  pop_col           = "weight",
  socio_vars        = c("education_mean"),
  year_filter       = 2023L,
  context_method    = "buffer",
  context_buffer_km = 3,
  geo_id_repair     = "bogota",
  bogota_max_suffix = 2L,
  bogota_broad_ids  = FALSE,
  pollutants        = c("pm10", "pm25"),
  who_it            = who_it,
  out_dir           = here::here(outdir_station, "bogota_2018"),
  out_name          = "bogota_2018_2023_3km",
  overwrite         = TRUE,
  return_data       = TRUE
)

# 2. CDMX
# --------------------------------------------------------------------------------------------
# Municipalities are large enough that the containing-unit definition is transparent.
station_cdmx <- build_station_scatter_inputs(
  arrow_dir         = arrow_cdmx,
  stations_sf       = stations_cdmx,
  geo_sf            = geo_cdmx,
  census_col        = census_cdmx,
  station_id_col    = "station",
  geo_id_col        = "CVE_MUN",
  pop_col           = "weight",
  socio_vars        = c("education_mean", "income_mean"),
  year_filter       = 2023L,
  context_method    = "containing_geo",
  context_buffer_km = 3,
  geo_id_repair     = "none",
  pollutants        = c("pm10", "pm25"),
  who_it            = who_it,
  out_dir           = here::here(outdir_station, "cdmx_2020"),
  out_name          = "cdmx_2020_2023",
  overwrite         = TRUE,
  return_data       = TRUE
)

# 3. Santiago
# --------------------------------------------------------------------------------------------
# Santiago uses census-tract-level geographic units, so containing-unit context is fine.
station_santiago <- build_station_scatter_inputs(
  arrow_dir         = arrow_santiago,
  stations_sf       = stations_santiago,
  geo_sf            = geo_santiago,
  census_col        = census_santiago,
  station_id_col    = "station_name",
  geo_id_col        = "CUT",
  pop_col           = "weight",
  socio_vars        = c("education_mean"),
  year_filter       = 2023L,
  context_method    = "containing_geo",
  context_buffer_km = 3,
  geo_id_repair     = "none",
  pollutants        = c("pm10", "pm25"),
  who_it            = who_it,
  out_dir           = here::here(outdir_station, "santiago_2024"),
  out_name          = "santiago_2024_2023",
  overwrite         = TRUE,
  return_data       = TRUE
)

# 4. Sao Paulo
# --------------------------------------------------------------------------------------------
# Sao Paulo uses weighting areas. We attach both education and income context.
station_sp <- build_station_scatter_inputs(
  arrow_dir         = arrow_sp,
  stations_sf       = stations_sp,
  geo_sf            = geo_sp,
  census_col        = census_sp,
  station_id_col    = "station_name",
  geo_id_col        = "code_weighting",
  pop_col           = "weight",
  socio_vars        = c("education_mean", "income_mean"),
  year_filter       = 2023L,
  context_method    = "containing_geo",
  context_buffer_km = 3,
  geo_id_repair     = "none",
  pollutants        = c("pm10", "pm25"),
  who_it            = who_it,
  out_dir           = here::here(outdir_station, "sao_paulo_2010"),
  out_name          = "sao_paulo_2010_2023",
  overwrite         = TRUE,
  return_data       = TRUE
)

cat("Station-level socioeconomic exposure data created successfully.\n")