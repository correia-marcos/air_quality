# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare geography, stations, pollution and census data for São Paulo.
#
#' @Description: Use the 2010 municipalities, tracts and weighting areas.
# Geography and station selection return spatial objects. Pollution and census functions
# also write their large datasets while processing. All sources must be available locally.
#
#' @Summary:
#   I.   Import data: source functions, declare paths and read station locations.
#   II.  Process data: prepare geography, select stations and harmonize measurements.
#   III. Save outputs: save geographic layers and selected stations.
#
#' @Date: January 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and their required packages.
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "reproducibility.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "spatial_files.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "processing.R"))

# Load the city configurations and use spherical geometry for sf operations.
load_city_modules()
sf::sf_use_s2(TRUE)
cfg <- sao_paulo_cfg

# Set the source folders using the city configuration.
dir_sources   <- cfg$dl_dir
dir_geography <- here::here(dir_sources, "metro_area")
dir_census    <- here::here(dir_sources, "census")
dir_pollution <- here::here(dir_sources, "ground_stations")

# Set the output folders for geography, pollution and the 2010 census.
out_geography <- here::here(cfg$out_dir, "geospatial_data", "sao_paulo")
out_pollution <- here::here(cfg$out_dir, "monitoring_stations")
out_census    <- here::here(cfg$out_dir, "census", "sao_paulo_2010")

# Name the preserved IBGE, geobr, census and station files.
file_municipalities <- here::here(dir_geography, "sp_municipios.zip")
file_tracts         <- here::here(dir_geography, "sp_setores_censitarios.zip")
file_weights        <- here::here(dir_geography, "sp_weighting_areas_2010.rds")
file_census         <- here::here(dir_census, "2010_population.parquet")
file_stations       <- here::here(dir_sources, "stations_metadata", "stations_metadata.csv")

# Check the sources before processing writes any output.
processing_files(c(file_municipalities, file_tracts, file_weights, file_stations,
                   dir_pollution), "São Paulo sources")
preflight_census_inputs("sao_paulo", c(census_2010 = file_census))

# Read station locations before selecting stations around the metropolitan area.
station_locations <- read.csv(file_stations)

# ========================================================================================
# II: Process data
# ========================================================================================
# Prepare the municipalities, census tracts and weighting areas from local sources.
municipalities  <- sao_paulo_prepare_metro_area(source_zip        = file_municipalities,
                                                level             = "mpio",
                                                keep_municipality = cfg$cities_in_metro)

tracts          <- sao_paulo_prepare_metro_area(source_zip        = file_tracts,
                                                level             = "setor_censitario",
                                                keep_municipality = cfg$cities_in_metro)

weighting_areas <- sao_paulo_prepare_weighting_areas(source_file       = file_weights,
                                                     keep_municipality = cfg$cities_in_metro)

stations        <- sp_filter_stations_in_metro(stations_sp = station_locations,
                                               metro_area  = municipalities,
                                               radius_km   = cfg$station_buffer_km,
                                               out_file    = NULL)

# These functions write their pollution and census datasets during processing.
pollution <- sp_process_stations_data_to_parquet(data_folder = dir_pollution,
                                                 stations_sf = stations,
                                                 tz          = cfg$processing_tz,
                                                 years       = cfg$years,
                                                 out_dir     = out_pollution,
                                                 out_name    = "sao_paulo_metro")

census    <- sp_process_census_2010(sf_data     = weighting_areas,
                                    source_file = file_census,
                                    out_dir     = out_census,
                                    return_data = FALSE)

# ========================================================================================
# III: Save outputs
# ========================================================================================
municipalities_file <- write_geopackage(
  x    = municipalities,
  path = here::here(out_geography, "sao_paulo_metro_2010.gpkg"))
tracts_file <- write_geopackage(
  x    = tracts,
  path = here::here(out_geography, "sao_paulo_metro_2010_census_tracts.gpkg"))
weighting_areas_file <- write_geopackage(
  x    = weighting_areas,
  path = here::here(out_geography, "sao_paulo_metro_2010_weighting_areas.gpkg"))
stations_file <- write_geopackage(
  x    = stations,
  path = here::here(out_geography, "sao_paulo_stations_buffer_metro_2010.gpkg"))
