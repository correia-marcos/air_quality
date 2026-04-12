# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: 
# 
# @Description: 
# 
# @Summary: 
#   I.   
#   II.  
#   III. 
# 
# @Date: January 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define the output general folders
dir_pollution  <- here::here("data", "raw", "monitoring_stations")
dir_geospatial <- here::here("data", "raw", "geospatial_data")
dir_census     <- here::here("data", "interim", "census")
outdir_data    <- here::here("data", "processed", "distances_matrices")

# Define files' specifc location
gpkg_stations_bogota_2018     <- here::here(dir_geospatial, "bogota",
                                            "bogota_2018_stations_buffer_metro.gpkg")
gpkg_stations_cdmx            <- here::here(dir_geospatial, "cdmx",
                                            "cdmx_stations_buffer_metro.gpkg")
gpkg_stations_santiago_2024   <- here::here(dir_geospatial, "santiago",
                                            "gran_santiago_stations_buffer_metro_2024.gpkg")
gpkg_stations_sp_2010         <- here::here(dir_geospatial, "sao_paulo",
                                            "sao_paulo_stations_buffer_metro_2010.gpkg")

gpkg_bogota_2018_metro_area   <- here::here(dir_geospatial, "bogota",
                                            "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_bogota_2018_small        <- here::here(dir_geospatial, "bogota",
                                            "bogota_area_metro_2018.gpkg")
gpkg_cdmx_metro_area          <- here::here(dir_geospatial, "cdmx",
                                            "cdmx_area_metro.gpkg")
gpkg_santiago_2024_metro_area <- here::here(dir_geospatial, "santiago",
                                            "santiago_metro_area_2024.gpkg")
gpkg_sp_2010_metro_area       <- here::here(dir_geospatial, "sao_paulo",
                                            "sao_paulo_metro_2010_weighting_areas.gpkg")

arrow_bogota_dir              <- here::here(dir_pollution, "bogota_metro_dataset")
arrow_cdmx_dir                <- here::here(dir_pollution, "cdmx_metro_dataset")
arrow_santiago_sir            <- here::here(dir_pollution, "santiago_metro_dataset")
arrow_sp_dir                  <- here::here(dir_pollution, "sao_paulo_metro_dataset")

# Read the necessary files
bogota_stations_2018_sf      <- sf::st_read(gpkg_stations_bogota_2018)
cdmx_stations_sf             <- sf::st_read(gpkg_stations_cdmx)
santiago_stations_2024_sf    <- sf::st_read(gpkg_stations_santiago_2024)
sp_stations_2010_sf          <- sf::st_read(gpkg_stations_sp_2010)

bogota_metro_2018_sf         <- sf::st_read(gpkg_bogota_2018_metro_area)
bogota_metro_2018_small      <- sf::st_read(gpkg_bogota_2018_small)
cdmx_metro_sf                <- sf::st_read(gpkg_cdmx_metro_area)
santiago_metro_2024_sf       <- sf::st_read(gpkg_santiago_2024_metro_area)
sp_metro_2010_sf             <- sf::st_read(gpkg_sp_2010_metro_area)
# ============================================================================================
# II and III: Process and save matrix as parquet
# ============================================================================================
# Apply function to calculate distance matrices for Bogota
bogota_distances_stations <- compute_distance_matrices(
  stations_sf    = bogota_stations_2018_sf,
  station_id_col = "station_name",
  geo_sf         = NULL,
  out_dir        = outdir_data,
  out_name       = "bogota_2018"
)

bogota_distances_geo_id_stations <- compute_distance_matrices(
  stations_sf    = bogota_stations_2018_sf,
  station_id_col = "station_name",
  geo_sf         = bogota_metro_2018_small,
  geo_id_col     = "GEO_ID",
  out_dir        = outdir_data,
  out_name       = "bogota_2018"
)


