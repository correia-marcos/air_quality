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
dir_pollution    <- here::here("data", "raw", "monitoring_stations")
dir_geospatial   <- here::here("data", "raw", "geospatial_data")
dir_census       <- here::here("data", "interim", "census")
outdir_distances <- here::here("data", "processed", "distances_matrices")
outdir_results   <- here::here("data", "processed", "outlier_detection")

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
cdmx_metro_sf                <- sf::st_read(gpkg_cdmx_metro_area)
santiago_metro_2024_sf       <- sf::st_read(gpkg_santiago_2024_metro_area)
sp_metro_2010_sf             <- sf::st_read(gpkg_sp_2010_metro_area)

big_bogota_cnpv <- here::here(dir_census, "bogota_2018", "census_2018_metro_individual.csv")
bogota_cnpv_2018 <- vroom::vroom(big_bogota_cnpv, col_types = "cnnnnccc")

# ============================================================================================
# II and III: Process and save matrix as parquet
# ============================================================================================
# Apply function to calculate outliers for Bogota
bogota_cleaned <- detect_pollution_outliers(
  arrow_dir           = arrow_bogota_dir,
  station_dist_path   = here::here(outdir_distances, "bogota_2018_station_distances.parquet"),
  on_missing_temporal = "continue",
  on_missing_neighbor = "second",
  out_dir             = outdir_results,
  out_name            = "bogota_metro"
)

exp <- aggregate_idw_exposure(
  arrow_dir      = here::here(outdir_results, "bogota_metro_clean"),
  geo_sta_pq     = here::here(outdir_distances, "bogota_2018_geo_station_distances.parquet"),
  census_col     = bogota_cnpv_2018,
  geo_id_col     = "GEO_ID",
  pop_col        = "fe",
  edu_col        = "escolaridad",
  quintile_level = "individual",
  out_dir        = here::here("data", "processed", "idw_estimates", "bogota_2018"),
  out_name       = "bogota_2018"
)


hi <- plot_exposure_by_quintile(
  exposure_dir   = here::here("data", "processed", "idw_estimates", "bogota_2018"),
  out_name       = "bogota_2018",
  quintile_level = "individual",
  pop_col        = "fe",
  year_filter    = 2023)

hi$plot
