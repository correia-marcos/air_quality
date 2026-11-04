# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare geography, stations, pollution and census data for Bogotá.
#
#' @Description: Prepare the 2018 geography, the stations within its buffer, pollution and
# the 2018 census. Geography and station selection return spatial objects. Pollution and
# census functions also write their large datasets while processing. All sources must be
# available locally. The 2005 inputs used only by the legacy validation are prepared by
# scripts/process_data/not_in_paper/legacy_validation/prepare_bogota_2005_inputs.R.
#
#' @Summary:
#   I.   Import data: source functions, declare paths and read station locations.
#   II.  Process data: prepare geography, select stations and process pollution/census.
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

# Load the city configurations and use spherical geometry for sf operations
load_city_modules()
sf::sf_use_s2(TRUE)
cfg <- bogota_cfg

# Set the source folders using the city configuration
dir_sources    <- cfg$dl_dir
dir_geography  <- here::here(dir_sources, "metro_area")
dir_census     <- here::here(dir_sources, "census")
dir_pollution  <- here::here(dir_sources, "ground_stations")
dir_metadata   <- here::here(dir_sources, "stations_metadata")
dir_sisaire    <- here::here(dir_sources, "metro_ground_stations_hourly")

# Set the output folders for geography, pollution and the 2018 census
out_geography    <- here::here(cfg$out_dir, "geospatial_data", "bogota")
out_pollution    <- here::here(cfg$out_dir, "monitoring_stations")
out_extract_2018 <- here::here(cfg$out_dir, "census_extracted", "bogota", "CNPV_2018")
out_census_2018  <- here::here(cfg$out_dir, "census", "bogota_2018")

# Name the input files to be used as parameters in the functions below
file_municipalities  <- here::here(dir_geography, "SHP_MGN2018_INTGRD_MPIO.zip")
file_urban_tracts    <- here::here(dir_geography, "SHP_MGN2018_INTGRD_MANZ.zip")
file_rural_tracts    <- here::here(dir_geography, "SHP_MGN2018_INTGRD_SECCR.zip")
file_localities      <- here::here(dir_geography, "bogota_loca.gpkg")
file_stations        <- here::here(dir_sources, "ground_stations_geolocation",
                                   "bogota_stations_location.csv")

# Check the sources before processing writes any output; missing files are not downloaded.
required_sources <- c(file_municipalities, file_urban_tracts, file_rural_tracts,
                      file_localities,     file_stations,     dir_metadata,
                      dir_pollution,       dir_sisaire)
processing_files(required_sources, "Bogotá sources")

# The 2018 census needs both Bogotá and Cundinamarca archives.
preflight_census_inputs("bogota", c(census_2018 = dir_census))

# Read the monitoring-station locations.
station_locations <- read.csv(file_stations)
# ========================================================================================
# II: Process data
# ========================================================================================
# Prepare the two 2018 geographic layers from preserved DANE files.
metro_2018  <- bogota_prepare_metro_area(source_zips        = file_municipalities,
                                         level              = "mpio_localidad",
                                         mgn_year           = 2018,
                                         municipality_codes = cfg$city_code_metro,
                                         localities_file    = file_localities)

tracts_2018 <- bogota_prepare_metro_area(source_zips        = c(file_urban_tracts,
                                                                file_rural_tracts),
                                         level              = "manzana",
                                         mgn_year           = 2018,
                                         municipality_codes = cfg$city_code_metro)

# Select the stations within the buffer of the 2018 area.
stations_2018 <- bogota_filter_stations_in_metro(rmcab_df     = station_locations,
                                                 metadata_dir = dir_metadata,
                                                 metro_area   = metro_2018,
                                                 radius_km    = cfg$station_buffer_km,
                                                 out_file     = NULL)

# Build and save the pollution dataset; pollution is an Arrow connection to files on disk.
pollution <- bogota_process_stations_data_to_parquet(rmcab_folder   = dir_pollution,
                                                     sisaire_folder = dir_sisaire,
                                                     stations_sf    = stations_2018,
                                                     tz             = cfg$processing_tz,
                                                     years          = cfg$years,
                                                     out_dir        = out_pollution,
                                                     out_name       = "bogota_metro")

# Extract and process the 2018 census; these functions save files and return paths.
extracted_2018 <- bogota_filter_census_2018(census_folder = dir_census,
                                            out_dir       = out_extract_2018,
                                            overwrite     = TRUE)
census_2018    <- bogota_harmonize_census_2018_data(extract_paths = extracted_2018,
                                                    metro_codes   = cfg$city_code_metro,
                                                    out_dir       = out_census_2018,
                                                    return_data   = FALSE)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the geographic layers and selected stations in their processing order.
metro_2018_file    <- write_geopackage(
  x    = metro_2018,
  path = here::here(out_geography, "bogota_area_metro_2018.gpkg")
  )
tracts_2018_file   <- write_geopackage(
  x    = tracts_2018,
  path = here::here(out_geography, "bogota_area_metro_census_tracts_2018.gpkg")
  )
stations_2018_file <- write_geopackage(
  x    = stations_2018,
  path = here::here(out_geography, "bogota_2018_stations_buffer_metro.gpkg")
  )
