# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare geography, stations, pollution and census data for CDMX.
#
#' @Description: Use 2024 geography with the extended 2020 census.
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
cfg <- cdmx_cfg

# Set the source folders using the city configuration.
dir_sources   <- cfg$dl_dir
dir_geography <- here::here(dir_sources, "metro_area")
dir_census    <- here::here(dir_sources, "census")
dir_pollution <- here::here(dir_sources, "ground_stations")
dir_secondary <- here::here(dir_sources, "ground_stations_raw_missing_data")

# Set the output folders for geography, pollution and the extended 2020 census.
out_geography <- here::here(cfg$out_dir, "geospatial_data", "cdmx")
out_pollution <- here::here(cfg$out_dir, "monitoring_stations")
out_extracted <- here::here(cfg$out_dir, "census_extracted", "cdmx", "CPV2020_EXTENDED")
out_census    <- here::here(cfg$out_dir, "census", "cdmx_extended_2020")

# Name the input files used by the preparation functions.
file_geography <- here::here(dir_geography, "mg_2024_integrado.zip")
file_stations  <- here::here(dir_sources, "ground_stations_geolocation",
                             "all_station_location.csv")

# Check the sources before processing writes any output.
processing_files(c(file_geography, file_stations, dir_pollution, dir_secondary),
                 "CDMX sources")
preflight_census_inputs("cdmx", c(census_2020 = dir_census))

# Read station locations before correcting the documented spelling differences.
station_locations <- read.csv(file_stations)

# ========================================================================================
# II: Process data
# ========================================================================================
# Prepare the municipality and AGEB layers from the same 2024 archive.
municipalities <- cdmx_prepare_metro_area(source_zip        = file_geography,
                                          level             = "municipality",
                                          keep_municipality = cfg$cities_in_metro)

metro_area     <- cdmx_prepare_metro_area(source_zip        = file_geography,
                                          level             = "ageb",
                                          keep_municipality = cfg$cities_in_metro)

# Correct only the configured station spellings; retain distinct real stations.
station_locations <- cdmx_correct_station_names(locations   = station_locations,
                                                corrections = cfg$station_nme_map)
stations <- cdmx_filter_stations_in_metro(station_location = station_locations,
                                          metro_area       = metro_area,
                                          radius_km        = cfg$station_buffer_km,
                                          dissolve         = TRUE,
                                          out_file         = NULL)

# Merge pollution sources without deleting either source; outputs stay partitioned.
pollution <- cdmx_merge_pollution_data(primary_data_dir   = dir_pollution,
                                       secondary_data_dir = dir_secondary,
                                       stations_sf        = stations,
                                       tz                 = cfg$processing_tz,
                                       years              = cfg$years,
                                       cleanup            = FALSE,
                                       out_dir            = out_pollution,
                                       out_name           = "cdmx_metro")

# Extract and harmonize the extended census; these calls also write the census files.
extracted <- mexico_filter_census(census_dir = dir_census,
                                  out_dir    = out_extracted,
                                  overwrite  = TRUE)
archives <- list.files(dir_census, pattern = "Censo2020_CA_.*_csv\\.zip$",
                       ignore.case = TRUE)
census <- mexico_harmonize_census_data(extract_index = extracted,
                                       metro_codes   = cfg$cities_in_metro,
                                       out_dir       = out_census,
                                       return_data   = FALSE)

# ========================================================================================
# III: Save outputs
# ========================================================================================
municipalities_file <- write_geopackage(
  x    = municipalities,
  path = here::here(out_geography, "cdmx_area_metro_municipalities_2024.gpkg"))

metro_area_file <- write_geopackage(
  x    = metro_area,
  path = here::here(out_geography, "cdmx_area_metro_2024.gpkg"))

stations_file <- write_geopackage(
  x    = stations,
  path = here::here(out_geography, "cdmx_stations_buffer_metro.gpkg"))

