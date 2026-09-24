# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare geography, stations, pollution and census data for Santiago.
#
#' @Description: Keep separate 2017 zones and 2024 communes.
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
cfg <- santiago_cfg

# Set the source folders; the two census vintages use separate geographic inputs.
dir_sources        <- cfg$dl_dir
dir_geography      <- here::here(dir_sources, "metro_area")
dir_geography_2017 <- here::here(dir_geography, "2017")
dir_census         <- here::here(dir_sources, "census")
dir_census_2024    <- here::here(dir_census, "2024")
dir_pollution      <- here::here(dir_sources, "ground_stations")

# Set the output folders, including the working copy of the 2017 census database.
out_geography   <- here::here(cfg$out_dir, "geospatial_data", "santiago")
out_pollution   <- here::here(cfg$out_dir, "monitoring_stations")
out_census_2017 <- here::here(cfg$out_dir, "census", "santiago_2017")
out_census_2024 <- here::here(cfg$out_dir, "census", "santiago_2024")
dir_work_2017   <- here::here(cfg$out_dir, "census_extracted", "santiago", "2017")

# Name the preserved geography, census and station files.
file_geography_2024 <- here::here(dir_geography, "Cartografia_censo2024_Pais.zip")
file_metro_2017     <- here::here(dir_geography_2017, "GRAN_SANTIAGO_13_metro.geojson")
file_zones_2017     <- here::here(dir_geography_2017, "GRAN_SANTIAGO_13_zonas.geojson")
file_count_2017     <- here::here(dir_geography_2017, "GRAN_SANTIAGO_13_count.json")
file_census_2017    <- here::here(dir_census, "2017", "censo2017.duckdb")
file_stations       <- here::here(dir_sources, "stations_metadata",
                                  "SINCA_metadata_stations_20260113_1616.csv")

# Check both vintages before processing writes any output.
required_sources <- c(file_geography_2024, file_metro_2017, file_zones_2017,
                      file_count_2017, file_stations, dir_pollution, file_census_2017)
processing_files(required_sources, "Santiago sources")
preflight_census_inputs("santiago", c(census_2017 = file_census_2017,
                                    census_2024 = dir_census_2024))

# Read the monitoring-station locations.
station_locations <- read.csv(file_stations)

# ========================================================================================
# II: Process data
# ========================================================================================
# Prepare both vintages; the 2017 zones determine which stations are selected.
zones_2017 <- santiago_prepare_metro_area_2017(metro_file = file_metro_2017,
                                               zones_file = file_zones_2017,
                                               count_file = file_count_2017)

metro_2024 <- santiago_prepare_metro_area_2024(source_zip        = file_geography_2024,
                                               type              = "gran_santiago",
                                               level             = "mpio",
                                               keep_municipality = cfg$cities_in_metro,
                                               dissolve_by       = "CUT")

stations  <- santiago_filter_stations_in_metro(stations_df = station_locations,
                                               metro_area  = zones_2017,
                                               radius_km   = cfg$station_buffer_km,
                                               out_file    = NULL)

# The following functions write pollution and census datasets during processing.
pollution   <- santiago_process_stations_data_to_parquet(data_folder = dir_pollution,
                                                         stations_sf = stations,
                                                         tz          = cfg$processing_tz,
                                                         years       = cfg$years,
                                                         out_dir     = out_pollution,
                                                         out_name    = "santiago_metro")

# The two census files
census_2017 <- santiago_process_census_2017(sf_data     = zones_2017,
                                            match_col   = "zona_id",
                                            source_db   = file_census_2017,
                                            work_dir    = dir_work_2017,
                                            out_dir     = out_census_2017,
                                            return_data = FALSE)

census_2024 <- santiago_process_census_2024(census_dir  = dir_census_2024,
                                            sf_data     = metro_2024,
                                            match_col   = "CUT",
                                            out_dir     = out_census_2024,
                                            overwrite   = TRUE,
                                            return_data = FALSE)

# ========================================================================================
# III: Save outputs
# ========================================================================================
zones_2017_file <- write_geopackage(
  x    = zones_2017,
  path = here::here(out_geography, "gran_santiago_zonas_2017.gpkg"))

metro_2024_file <- write_geopackage(
  x    = metro_2024,
  path = here::here(out_geography, "gran_santiago_area_2024.gpkg"))

stations_file <- write_geopackage(
  x    = stations,
  path = here::here(out_geography, "gran_santiago_stations_buffer_metro_2017.gpkg"))
