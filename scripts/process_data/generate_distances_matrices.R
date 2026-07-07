# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Produce station-to-station and station-to-geo distance matrices for every city.
# 
# @Description: This script calculates the spatial distances required for outlier detection
# and spatial interpolation (IDW). For each city, it computes two types of matrices: the
# distance between individual monitoring stations, and the distance from census geographic 
# units to all stations. Results are saved as Parquet files for memory efficiency.
# 
# @Summary: 
#   I.   Setup: Load dependencies, utility functions, and define directory paths.
#   II.  Import: Read raw geospatial boundaries (GPKG) and station location data.
#   III. Process: Compute and save distance matrices as Parquet files for all four cities.
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

# File path for station spatial location
gpkg_stations_bogota_2018 <- here::here(dir_geospatial, "bogota",
                                        "bogota_2018_stations_buffer_metro.gpkg")
gpkg_stations_cdmx        <- here::here(dir_geospatial, "cdmx",
                                        "cdmx_stations_buffer_metro.gpkg")
gpkg_stations_santiago_2024 <- here::here(dir_geospatial, "santiago",
                                          "gran_santiago_stations_buffer_metro_2024.gpkg")
gpkg_stations_sp_2010       <- here::here(dir_geospatial, "sao_paulo",
                                          "sao_paulo_stations_buffer_metro_2010.gpkg")

# File path for metro area spatial boundaries
gpkg_bogota_2018_metro_area <- here::here(dir_geospatial, "bogota",
                                          "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_cdmx_metro_area        <- here::here(dir_geospatial, "cdmx",
                                          "cdmx_area_metro.gpkg")
gpkg_santiago_2024_metro_area <- here::here(dir_geospatial, "santiago",
                                            "gran_santiago_area_2024.gpkg")
gpkg_sp_2010_metro_area       <- here::here(dir_geospatial, "sao_paulo",
                                            "sao_paulo_metro_2010_weighting_areas.gpkg")

# Read the necessary files for stations
bogota_stations_2018_sf   <- sf::st_read(gpkg_stations_bogota_2018)
cdmx_stations_sf          <- sf::st_read(gpkg_stations_cdmx)
santiago_stations_2024_sf <- sf::st_read(gpkg_stations_santiago_2024)
sp_stations_2010_sf       <- sf::st_read(gpkg_stations_sp_2010)

# Read the necessary files for metro boundaries
bogota_metro_2018_sf   <- sf::st_read(gpkg_bogota_2018_metro_area)
cdmx_metro_sf          <- sf::st_read(gpkg_cdmx_metro_area)
santiago_metro_2024_sf <- sf::st_read(gpkg_santiago_2024_metro_area)
sp_metro_2010_sf       <- sf::st_read(gpkg_sp_2010_metro_area)

# ============================================================================================
# II and III: Process and save matrices as parquet
# ============================================================================================
# Create the geographic distances output folder, if not created
dir.create(outdir_data, recursive = TRUE, showWarnings = FALSE)

# Calculate distance matrices for Bogota
bogota_distances <- compute_distance_matrices(
  stations_sf          = bogota_stations_2018_sf,
  station_id_col       = "station_name",
  geo_sf               = bogota_metro_2018_sf,
  geo_id_col           = "GEO_ID",
  distance_metric      = "aeqd",
  representative_point = "point_on_surface",
  out_dir              = here::here(outdir_data, "bogota_2018"),
  out_name             = "matrix"
)

# Calculate distance matrices for CDMX
cdmx_distances <- compute_distance_matrices(
  stations_sf          = cdmx_stations_sf,
  station_id_col       = "station",
  geo_sf               = cdmx_metro_sf,
  geo_id_col           = "CVE_MUN",
  distance_metric      = "aeqd",
  representative_point = "point_on_surface",
  out_dir              = here::here(outdir_data, "cdmx_2020"),
  out_name             = "matrix"
)

# Calculate distance matrices for Santiago
santiago_distances <- compute_distance_matrices(
  stations_sf          = santiago_stations_2024_sf,
  station_id_col       = "station_name",
  geo_sf               = santiago_metro_2024_sf,
  geo_id_col           = "CUT",
  distance_metric      = "aeqd",
  representative_point = "point_on_surface",
  out_dir              = here::here(outdir_data, "santiago_2024"),
  out_name             = "matrix"
)

# Calculate distance matrices for Sao Paulo
sao_paulo_distances <- compute_distance_matrices(
  stations_sf          = sp_stations_2010_sf,
  station_id_col       = "station_name",
  geo_sf               = sp_metro_2010_sf,
  geo_id_col           = "code_weighting",
  distance_metric      = "aeqd",
  representative_point = "point_on_surface",
  out_dir              = here::here(outdir_data, "sao_paulo_2010"),
  out_name             = "matrix"
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
