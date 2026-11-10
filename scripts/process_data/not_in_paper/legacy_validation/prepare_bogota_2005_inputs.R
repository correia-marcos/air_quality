# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare the 2005 Bogotá inputs that the legacy validation compares.
#
#' @Description: Build the 2005 metropolitan area (municipalities with Bogotá's localities),
# the stations within its buffer, their distance matrices and the extended 2005 census, at
# the paths the validation scripts read. The paper uses the 2018 geography and census,
# prepared by scripts/process_data/inputs/process_bogota_data.R. The census functions
# write while processing. All sources must be available locally.
#
#' @Summary:
#   I.   Import data: source functions, declare paths and read station locations.
#   II.  Process data: prepare the 2005 area, select stations, compute distances and
#        process the census.
#   III. Save outputs: save the area, the selected stations and the distances.
#
#' @Date: November 2026
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
source(here::here("src", "general_utilities", "process", "distances.R"))
source(here::here("config", "analysis_settings.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "processing.R"))

# Load the city configurations and use spherical geometry for sf operations
load_city_modules()
sf::sf_use_s2(TRUE)
cfg <- bogota_cfg

# Set the source folders using the city configuration
dir_sources   <- cfg$dl_dir
dir_geography <- here::here(dir_sources, "metro_area")
dir_census    <- here::here(dir_sources, "census")
dir_metadata  <- here::here(dir_sources, "stations_metadata")

# Set the output folders for geography and the extended 2005 census
out_geography        <- here::here(cfg$out_dir, "geospatial_data", "bogota")
out_extract_extended <- here::here(cfg$out_dir, "census_extracted", "bogota",
                                   "CG2005_EXTENDED")
out_census_extended  <- here::here(cfg$out_dir, "census", "bogota_extended_2005")
out_distances        <- here::here("data", "processed", "distances_matrices", "bogota_2005")

# Name the input files to be used as parameters in the functions below
file_census_extended <- here::here(dir_census, "CG2005_AMPLIADO.zip")
file_geography_2005  <- here::here(dir_geography, "SHP_MGN2005_COLOMBIA.zip")
file_localities      <- here::here(dir_geography, "bogota_loca.gpkg")
file_stations        <- here::here(dir_sources, "ground_stations_geolocation",
                                   "bogota_stations_location.csv")

# Check the sources before processing writes any output; missing files are not downloaded.
required_sources <- c(file_geography_2005, file_localities,      file_stations,
                      dir_metadata,        file_census_extended)
processing_files(required_sources, "Bogotá 2005 sources")

# Read the monitoring-station locations.
station_locations <- read.csv(file_stations)

# ========================================================================================
# II: Process data
# ========================================================================================
# Prepare the 2005 metropolitan area: municipalities, with Bogotá split into localities.
metro_2005 <- bogota_prepare_metro_area(source_zips        = file_geography_2005,
                                        level              = "mpio_localidad",
                                        mgn_year           = 2005,
                                        municipality_codes = cfg$city_code_metro,
                                        localities_file    = file_localities)

# Select the stations within the buffer of the 2005 area.
stations_2005 <- bogota_filter_stations_in_metro(rmcab_df     = station_locations,
                                                 metadata_dir = dir_metadata,
                                                 metro_area   = metro_2005,
                                                 radius_km    = cfg$station_buffer_km,
                                                 out_file     = NULL)

# Compute station-to-station and unit-to-station distances on the 2005 area, with the
# paper's distance settings; the legacy distance check compares them.
distances_2005 <- compute_distance_matrices(stations_sf = stations_2005,
    station_id_col       = "station_name",
    geo_sf               = metro_2005,
    geo_id_col           = "GEO_ID",
    distance_metric      = distance_metric,
    representative_point = distance_representative_point)

# Extract and process the extended 2005 census; these functions save files and return paths.
extracted_extended <- bogota_filter_census_2005(census_zip = file_census_extended,
                                                out_dir    = out_extract_extended,
                                                overwrite  = TRUE)
census_extended    <- bogota_harmonize_census_2005_data(extract_list = extracted_extended,
                                                        is_extended  = TRUE,
                                                        metro_codes  = cfg$city_code_metro,
                                                        out_dir      = out_census_extended,
                                                        return_data  = FALSE)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the 2005 area, its selected stations and their distance matrices.
metro_2005_file    <- write_geopackage(
  x    = metro_2005,
  path = here::here(out_geography, "bogota_area_metro_2005.gpkg")
  )
stations_2005_file <- write_geopackage(
  x    = stations_2005,
  path = here::here(out_geography, "bogota_2005_stations_buffer_metro.gpkg")
  )
distances_2005_files <- write_distance_matrices(
  result  = distances_2005,
  out_dir = out_distances
  )
