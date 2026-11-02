# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate Santiago exposure differences with the 2024 census communes.
#
#' @Description: Use the 2017 station selection, the cleaned 2023 panel and the 2024
# communes and census from process_santiago_2024_data.R. Compute commune-to-station
# distances, IDW exposure and education quintiles, then the group differences and
# coverage of the exposure analysis at 3 and 5 km. The paper's Santiago uses 2017 zones;
# this is a check on census vintage at coarser resolution. Distances and IDW are written
# as they are computed, because IDW reads the saved distances. Tables go to
# data/processed/idw_regressions/santiago_2024/.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths, read geography and census.
#   II.  Process and save data: compute and save distances, then IDW for every buffer.
#   III. Process data: estimate group differences and coverage for each buffer.
#   IV.  Save outputs: write the group tables for each buffer.
#
#' @Date: November 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and their required packages
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "distances.R"))
source(here::here("src", "general_utilities", "process", "idw_exposure.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))
source(here::here("config", "analysis_settings.R"))
sf::sf_use_s2(TRUE)

# Set input folders
dir_geospatial <- here::here("data", "interim", "geospatial_data", "santiago")
dir_census     <- here::here("data", "interim", "census", "santiago_2024")
dir_cleaned    <- here::here("data", "processed", "monitoring_stations_outliers")

# Set output folders; IDW writes under its own santiago_2024 folder
dir_distances <- here::here("data", "processed", "distances_matrices", "santiago_2024")
dir_idw       <- here::here("data", "processed", "idw_estimates")
dir_out       <- here::here("data", "processed", "idw_regressions", "santiago_2024")

# Set the station, commune, panel and census paths
gpkg_stations    <- here::here(dir_geospatial,
                               "gran_santiago_stations_buffer_metro_2017.gpkg")
gpkg_communes    <- here::here(dir_geospatial, "gran_santiago_area_2024.gpkg")
panel_santiago   <- here::here(dir_cleaned, "santiago_metro_clean",
                               paste0("year=", analysis_year))
file_micro       <- here::here(dir_census, "census_santiago_individual_2024.parquet")
file_geo         <- here::here(dir_census, "census_santiago_collapsed_2024.parquet")
distances_matrix <- here::here(dir_distances, "matrix_geo_station_distances.parquet")

# Read the stations, the 2024 communes and the 2024 census
stations_sf <- sf::st_read(gpkg_stations)
communes_sf <- sf::st_read(gpkg_communes)
micro_2024  <- arrow::read_parquet(file_micro)
geo_2024    <- arrow::read_parquet(file_geo)

# ========================================================================================
# II: Process and save data
# ========================================================================================
# Compute distances from the urban parts of the 2024 communes to the stations
distances_2024 <- compute_distance_matrices(stations_sf = stations_sf,
    station_id_col       = "station_name",
    geo_sf               = communes_sf,
    geo_id_col           = "CUT",
    distance_metric      = distance_metric,
    representative_point = distance_representative_point)

# Save the two distance tables; IDW reads the commune-to-station matrix from disk
distance_files <- write_distance_matrices(result  = distances_2024,
                                          out_dir = dir_distances)

# Estimate IDW exposure and education quintiles; buffers stay serial because their
# census-group files share the same destination
idw_santiago_2024 <- list()

for (buffer_km in santiago_2024_buffers_km) {
  idw_santiago_2024[[as.character(buffer_km)]] <- run_idw_city(
      city_label           = "Santiago (comuna 2024)",
      city_id              = "santiago_2024",
      arrow_dir            = panel_santiago,
      geo_sta_pq           = distances_matrix,
      geo_census           = geo_2024,
      micro_census         = micro_2024,
      socio_var            = "education",
      n_groups             = 5L,
      group_name           = "edu_quintile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      return_data          = FALSE)
}

# ========================================================================================
# III: Process data
# ========================================================================================
# Read the individual education quintiles and each buffer's IDW exposure just written
individual_2024 <- read_idw_artifact(dir_idw, "santiago_2024", "indiv_groups")

exposure_by_buffer <- list()
for (buffer_km in santiago_2024_buffers_km) {
  exposure_by_buffer[[as.character(buffer_km)]] <- read_idw_artifact(
      dir_idw, "santiago_2024", "idw_exposure", buffer_km)
}

# Estimate education-quintile differences and coverage for each buffer
estimates_by_buffer <- list()

for (buffer_km in santiago_2024_buffers_km) {
  santiago_2024 <- run_city_exposure(city           = "Santiago (comuna, 2024)",
                                     city_id        = "santiago_2024",
                                     exposure_dt    = exposure_by_buffer[[
                                       as.character(buffer_km)]],
                                     individual_dt  = individual_2024,
                                     geo_station_pq = distances_matrix,
                                     socio_var      = "education",
                                     group_col      = "edu_quintile",
                                     n_groups       = 5L,
                                     year           = analysis_year,
                                     buffer_km      = buffer_km)

  # Keep the education and coverage tables; this run has no income grouping
  tables <- stack_exposure_runs(edu_runs = list(santiago_2024), inc_runs = list())
  estimates_by_buffer[[as.character(buffer_km)]] <- tables[c(
    "ci_estimates_education", "group_summaries_education", "coverage")]
}

# ========================================================================================
# IV: Save outputs
# ========================================================================================
# Save the education and coverage tables for each buffer
group_files <- list()
for (buffer_km in santiago_2024_buffers_km) {
  group_files[[as.character(buffer_km)]] <- save_exposure_tables(
      tables = estimates_by_buffer[[as.character(buffer_km)]],
      out_dir = dir_out, buffer_km = buffer_km, year = analysis_year)
}
