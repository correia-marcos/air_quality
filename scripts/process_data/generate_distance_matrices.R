# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Produce station-to-station and station-to-geo distance matrices for every city.
#
#' @Description: This script calculates distances for outlier detection and spatial
# interpolation (IDW). For each city, it computes two types of matrices: the
# distance between individual monitoring stations, and the distance from census geographic
# units to all stations. Results are saved as Parquet files. Santiago is run twice because
# the 2017 census identifies smaller geographic units than the 2024 census.
#
#' @Summary:
#   I.   Import data: station locations, geographic units, and distance settings.
#   II.  Process data: compute two distance tables for each city/geographic vintage.
#   III. Save outputs: write the five matrix pairs in the same order.
#
#' @Date: January 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "distances.R"))
source(here::here("config", "analysis_settings.R"))
sf::sf_use_s2(TRUE)

# Set the paths to the required folders
dir_geospatial <- here::here("data", "interim", "geospatial_data")
outdir_data    <- here::here("data", "processed", "distances_matrices")

# Set the paths to station locations selected for each city's study area
gpkg_stations_bogota_2018   <- here::here(dir_geospatial, "bogota",
                                          "bogota_2018_stations_buffer_metro.gpkg")
gpkg_stations_cdmx          <- here::here(dir_geospatial, "cdmx",
                                          "cdmx_stations_buffer_metro.gpkg")
gpkg_stations_santiago_2017 <- here::here(dir_geospatial, "santiago",
                                          "gran_santiago_stations_buffer_metro_2017.gpkg")
gpkg_stations_sp_2010       <- here::here(dir_geospatial, "sao_paulo",
                                          "sao_paulo_stations_buffer_metro_2010.gpkg")

# Set the paths to geographic units matched to each census context
gpkg_bogota_2018_metro_area   <- here::here(dir_geospatial, "bogota",
                                            "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_cdmx_metro_area          <- here::here(dir_geospatial, "cdmx",
                                            "cdmx_area_metro_municipalities_2024.gpkg")
gpkg_santiago_2017_zonas      <- here::here(dir_geospatial, "santiago",
                                            "gran_santiago_zonas_2017.gpkg")
gpkg_santiago_2024_metro_area <- here::here(dir_geospatial, "santiago",
                                            "gran_santiago_area_2024.gpkg")
gpkg_sp_2010_metro_area       <- here::here(dir_geospatial, "sao_paulo",
                                            "sao_paulo_metro_2010_weighting_areas.gpkg")

# Read the station locations
bogota_stations_2018_sf   <- sf::st_read(gpkg_stations_bogota_2018)
cdmx_stations_sf          <- sf::st_read(gpkg_stations_cdmx)
santiago_stations_2017_sf <- sf::st_read(gpkg_stations_santiago_2017)
sp_stations_2010_sf       <- sf::st_read(gpkg_stations_sp_2010)

# Read the geographic units
bogota_metro_2018_sf   <- sf::st_read(gpkg_bogota_2018_metro_area)
cdmx_metro_sf          <- sf::st_read(gpkg_cdmx_metro_area)
santiago_zonas_2017_sf <- sf::st_read(gpkg_santiago_2017_zonas)
santiago_metro_2024_sf <- sf::st_read(gpkg_santiago_2024_metro_area)
sp_metro_2010_sf       <- sf::st_read(gpkg_sp_2010_metro_area)

# ========================================================================================
# II: Process data
# ========================================================================================
# Compute distances for Bogotá (2018 census tracts)
bogota_distances <- compute_distance_matrices(stations_sf = bogota_stations_2018_sf,
    station_id_col       = "station_name",
    geo_sf               = bogota_metro_2018_sf,
    geo_id_col           = "GEO_ID",
    distance_metric      = distance_metric,
    representative_point = distance_representative_point)

# Compute distances for CDMX (2024 municipalities, used with the 2020 census)
cdmx_distances <- compute_distance_matrices(stations_sf = cdmx_stations_sf,
    station_id_col       = "station",
    geo_sf               = cdmx_metro_sf,
    geo_id_col           = "CVE_MUN",
    distance_metric      = distance_metric,
    representative_point = distance_representative_point)

# Compute distances for Santiago's main specification (2017 census zones)
santiago_zona_distances <- compute_distance_matrices(
    stations_sf          = santiago_stations_2017_sf,
    station_id_col       = "station_name",
    geo_sf               = santiago_zonas_2017_sf,
    geo_id_col           = "zona_id",
    distance_metric      = distance_metric,
    representative_point = distance_representative_point)

# Compute distances for Santiago's robustness check (2024 communes)
santiago_distances <- compute_distance_matrices(stations_sf = santiago_stations_2017_sf,
    station_id_col       = "station_name",
    geo_sf               = santiago_metro_2024_sf,
    geo_id_col           = "CUT",
    distance_metric      = distance_metric,
    representative_point = distance_representative_point)

# Compute distances for São Paulo (2010 weighting areas)
sao_paulo_distances <- compute_distance_matrices(stations_sf = sp_stations_2010_sf,
    station_id_col       = "station_name",
    geo_sf               = sp_metro_2010_sf,
    geo_id_col           = "code_weighting",
    distance_metric      = distance_metric,
    representative_point = distance_representative_point)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the two distance tables from each result list as Parquet files
bogota_files <- write_distance_matrices(result = bogota_distances,
                                        out_dir = here::here(outdir_data, "bogota_2018"))

cdmx_files <- write_distance_matrices(result = cdmx_distances,
                                      out_dir = here::here(outdir_data, "cdmx_2020"))

santiago_zona_files <- write_distance_matrices(result = santiago_zona_distances,
    out_dir = here::here(outdir_data, "santiago_2017"))

santiago_files <- write_distance_matrices(result = santiago_distances,
    out_dir = here::here(outdir_data, "santiago_2024"))

sao_paulo_files <- write_distance_matrices(result = sao_paulo_distances,
    out_dir = here::here(outdir_data, "sao_paulo_2010"))
