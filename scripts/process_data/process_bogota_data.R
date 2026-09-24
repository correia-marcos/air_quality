# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare geography, stations, pollution and census data for Bogotá.
#
#' @Description: Preserve both geographic vintages and all three census variants.
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
# Get all libraries and functions
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "reproducibility.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "spatial_files.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "processing.R"))

# Get the config lists and define the sf behavior
load_city_modules()
sf::sf_use_s2(TRUE)
cfg <- bogota_cfg

# Set the source folders; city definitions remain in the configuration cfg
dir_sources    <- cfg$dl_dir
dir_geography  <- here::here(dir_sources, "metro_area")
dir_census     <- here::here(dir_sources, "census")
dir_pollution  <- here::here(dir_sources, "ground_stations")
dir_metadata   <- here::here(dir_sources, "stations_metadata")
dir_sisaire    <- here::here(dir_sources, "metro_ground_stations_hourly")

# Set the output folders; city definitions remain in the configuration cfg
out_geography        <- here::here(cfg$out_dir, "geospatial_data", "bogota")
out_pollution        <- here::here(cfg$out_dir, "monitoring_stations")
out_extract_extended <- here::here(cfg$out_dir, "census_extracted", "bogota", "CG2005_EXTENDED")
out_census_extended  <- here::here(cfg$out_dir, "census", "bogota_extended_2005")
out_extract_basic    <- here::here(cfg$out_dir, "census_extracted", "bogota", "CG2005_BASIC")
out_census_basic     <- here::here(cfg$out_dir, "census", "bogota_basic_2005")
out_extract_2018     <- here::here(cfg$out_dir, "census_extracted", "bogota", "CNPV_2018")
out_census_2018      <- here::here(cfg$out_dir, "census", "bogota_2018")

# Set the specific files' location - easier to apply in functions
file_census_extended <- here::here(dir_census, "CG2005_AMPLIADO.zip")
file_census_basic    <- here::here(dir_census, "CG2005_BASICO.zip")
file_geography_2005  <- here::here(dir_geography, "SHP_MGN2005_COLOMBIA.zip")
file_municipalities  <- here::here(dir_geography, "SHP_MGN2018_INTGRD_MPIO.zip")
file_urban_tracts    <- here::here(dir_geography, "SHP_MGN2018_INTGRD_MANZ.zip")
file_rural_tracts    <- here::here(dir_geography, "SHP_MGN2018_INTGRD_SECCR.zip")
file_localities      <- here::here(dir_geography, "bogota_loca.gpkg")
file_stations        <- here::here(dir_sources, "ground_stations_geolocation",
                                   "bogota_stations_location.csv")

# Check every preserved source before processing starts; missing files are not downloaded.
required_sources <- c(file_geography_2005,  file_municipalities, file_urban_tracts,
                      file_rural_tracts,    file_localities,     file_stations,
                      dir_metadata,         dir_pollution,       dir_sisaire,
                      file_census_extended, file_census_basic)
processing_files(required_sources, "Bogotá sources")
preflight_census_inputs("bogota", c(census_2018 = dir_census))

# Read the monitoring-station locations.
station_locations <- read.csv(file_stations)

# ========================================================================================
# II: Process data
# ========================================================================================
# Prepare the five geographic layers from preserved DANE files.
municipalities_2005 <- bogota_prepare_metro_area(source_zips        = file_geography_2005,
                                                 level              = "mpio",
                                                 mgn_year           = 2005,
                                                 municipality_codes = cfg$city_code_metro)

metro_2005  <- bogota_prepare_metro_area(source_zips        = file_geography_2005,
                                         level              = "mpio_localidad",
                                         mgn_year           = 2005,
                                         municipality_codes = cfg$city_code_metro,
                                         localities_file    = file_localities)

tracts_2005 <- bogota_prepare_metro_area(source_zips        = file_geography_2005,
                                         level              = "manzana",
                                         mgn_year           = 2005,
                                         municipality_codes = cfg$city_code_metro)

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

# Select stations separately for 2018 and 2005; pollution uses the 2018 selection.
stations_2018 <- bogota_filter_stations_in_metro(rmcab_df     = station_locations,
                                                 metadata_dir = dir_metadata,
                                                 metro_area   = metro_2018,
                                                 radius_km    = cfg$station_buffer_km,
                                                 out_file     = NULL)

stations_2005 <- bogota_filter_stations_in_metro(rmcab_df     = station_locations,
                                                 metadata_dir = dir_metadata,
                                                 metro_area   = metro_2005,
                                                 radius_km    = cfg$station_buffer_km,
                                                 out_file     = NULL)

# Apply function to build pollution data; pollution already save dataset into memory
pollution <- bogota_process_stations_data_to_parquet(rmcab_folder   = dir_pollution,
                                                     sisaire_folder = dir_sisaire,
                                                     stations_sf    = stations_2018,
                                                     tz             = cfg$processing_tz,
                                                     years          = cfg$years,
                                                     out_dir        = out_pollution,
                                                     out_name       = "bogota_metro")

# Apply functions to build census data (extraction and processing) - save to memory as well
extracted_extended <- bogota_filter_census_2005(census_zip = file_census_extended,
                                                out_dir    = out_extract_extended,
                                                overwrite  = TRUE)
census_extended    <- bogota_harmonize_census_2005_data(extract_list = extracted_extended,
                                                        is_extended  = TRUE,
                                                        metro_codes  = cfg$city_code_metro,
                                                        out_dir      = out_census_extended,
                                                        return_data  = FALSE)

# Apply functions to build census data (extraction and processing) - save to memory as well
extracted_basic <- bogota_filter_census_2005(census_zip = file_census_basic,
                                             out_dir    = out_extract_basic,
                                             overwrite  = TRUE)
census_basic    <- bogota_harmonize_census_2005_data(extract_list = extracted_basic,
                                                     is_extended  = FALSE,
                                                     metro_codes  = cfg$city_code_metro,
                                                     out_dir      = out_census_basic,
                                                     return_data  = FALSE)

# Apply functions to build census data (extraction and processing) - save to memory as well
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
metro_2005_file          <- write_geopackage(
  x    = metro_2005,
  path = here::here(out_geography, "bogota_area_metro_2005.gpkg")
  )
municipalities_2005_file <- write_geopackage(
  x    = municipalities_2005,
  path = here::here(out_geography, "bogota_area_metro_municipalities_2005.gpkg")
  )
tracts_2005_file         <- write_geopackage(
  x    = tracts_2005,
  path = here::here(out_geography, "bogota_area_metro_census_tracts_2005.gpkg")
  )
metro_2018_file          <- write_geopackage(
  x    = metro_2018,
  path = here::here(out_geography, "bogota_area_metro_2018.gpkg")
  )
tracts_2018_file         <- write_geopackage(
  x    = tracts_2018,
  path = here::here(out_geography, "bogota_area_metro_census_tracts_2018.gpkg")
  )
stations_2018_file       <- write_geopackage(
  x    = stations_2018,
  path = here::here(out_geography, "bogota_2018_stations_buffer_metro.gpkg")
  )
stations_2005_file       <- write_geopackage(
  x    = stations_2005,
  path = here::here(out_geography, "bogota_2005_stations_buffer_metro.gpkg")
  )
