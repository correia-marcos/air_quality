# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Estimate geographic exposure and socioeconomic groups with IDW.
#
#' @Description: Use cleaned 2023 station data, distances and census records.
# Compute education quintiles for all cities, income quintiles for CDMX and income
# deciles for São Paulo. Buffers are 3/5/20 km; 20 km is used only for density figures.
# run_idw_city() writes the large outputs and returns their paths. Income reuses the
# same interpolation. Each city retains its results for every buffer in a named list.
#
#' @Summary:
#   I.  Import data: source functions, declare paths and read census data.
#   II. Process and save data: estimate each city and retain the returned file paths.
#
#' @Date: September 2026
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
# Load the functions and their required packages
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "idw_exposure.R"))
source(here::here("config", "analysis_settings.R"))

# Set input folders and the output folder
dir_cleaned   <- here::here("data", "processed", "monitoring_stations_outliers")
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_census    <- here::here("data", "interim", "census")
dir_idw       <- here::here("data", "processed", "idw_estimates")

# The hourly station datasets remain on disk.
# Set the Bogotá input paths
panel_bogota         <- here::here(dir_cleaned, "bogota_metro_clean",
                                   paste0("year=", analysis_year))
distances_bogota     <- here::here(dir_distances, "bogota_2018",
                                   "matrix_geo_station_distances.parquet")
file_micro_bogota    <- here::here(dir_census, "bogota_2018",
                                   "census_2018_metro_individual.parquet")
file_geo_bogota      <- here::here(dir_census, "bogota_2018",
                                   "census_2018_metro_collapsed.parquet")

# Set the CDMX input paths
panel_cdmx           <- here::here(dir_cleaned, "cdmx_metro_clean",
                                   paste0("year=", analysis_year))
distances_cdmx       <- here::here(dir_distances, "cdmx_2020",
                                   "matrix_geo_station_distances.parquet")
file_micro_cdmx      <- here::here(dir_census, "cdmx_extended_2020",
                                   "census_metro_individual_2020.parquet")
file_geo_cdmx        <- here::here(dir_census, "cdmx_extended_2020",
                                   "collapse_metro_area_2020.parquet")

# Set the Santiago 2017 input paths
panel_santiago       <- here::here(dir_cleaned, "santiago_metro_clean",
                                   paste0("year=", analysis_year))
distances_santiago   <- here::here(dir_distances, "santiago_2017",
                                   "matrix_geo_station_distances.parquet")
file_micro_santiago  <- here::here(dir_census, "santiago_2017",
                                   "census_individual_2017.parquet")
file_geo_santiago    <- here::here(dir_census, "santiago_2017",
                                   "census_collapsed_2017.parquet")

# Set the São Paulo input paths
panel_sao_paulo      <- here::here(dir_cleaned, "sao_paulo_metro_clean",
                                   paste0("year=", analysis_year))
distances_sao_paulo  <- here::here(dir_distances, "sao_paulo_2010",
                                   "matrix_geo_station_distances.parquet")
file_micro_sao_paulo <- here::here(dir_census, "sao_paulo_2010",
                                   "census_sp_individual_2010.parquet")
file_geo_sao_paulo   <- here::here(dir_census, "sao_paulo_2010",
                                   "census_sp_collapsed_2010.parquet")

# Set the Santiago 2024 robustness input paths
panel_santiago_robustness      <- here::here(dir_cleaned, "santiago_metro_clean",
                                             paste0("year=", analysis_year))
distances_santiago_robustness  <- here::here(dir_distances, "santiago_2024",
                                             "matrix_geo_station_distances.parquet")
file_micro_santiago_robustness <- here::here(dir_census, "santiago_2024",
                                             "census_santiago_individual_2024.parquet")
file_geo_santiago_robustness   <- here::here(dir_census, "santiago_2024",
                                             "census_santiago_collapsed_2024.parquet")

# Read the required census data
micro_bogota    <- arrow::read_parquet(file_micro_bogota)
geo_bogota      <- arrow::read_parquet(file_geo_bogota)

micro_cdmx      <- arrow::read_parquet(file_micro_cdmx)
geo_cdmx        <- arrow::read_parquet(file_geo_cdmx)

micro_santiago  <- arrow::read_parquet(file_micro_santiago)
geo_santiago    <- arrow::read_parquet(file_geo_santiago)

micro_sao_paulo <- arrow::read_parquet(file_micro_sao_paulo)
geo_sao_paulo   <- arrow::read_parquet(file_geo_sao_paulo)

micro_santiago_robustness <- arrow::read_parquet(file_micro_santiago_robustness)
geo_santiago_robustness   <- arrow::read_parquet(file_geo_santiago_robustness)
# ==========================================================================================
# II: Process and save data
# ==========================================================================================
# Keep buffers serial: their census-group files share the same destination.
idw_bogota <- list()
idw_cdmx <- list()
idw_santiago <- list()
idw_santiago_robustness <- list()
idw_sao_paulo <- list()

for (buffer_km in idw_buffers_km) {

  # Bogotá: education quintiles.
  idw_bogota[[as.character(buffer_km)]] <- run_idw_city(city_label = "Bogota",
      city_id              = "bogota_2018",
      arrow_dir            = panel_bogota,
      geo_sta_pq           = distances_bogota,
      geo_census           = geo_bogota,
      micro_census         = micro_bogota,
      socio_var            = "education",
      n_groups             = 5L,
      group_name           = "edu_quintile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      return_data          = FALSE)

  # CDMX: education quintiles.
  idw_cdmx[[as.character(buffer_km)]] <- run_idw_city(city_label = "CDMX",
      city_id              = "cdmx_2020",
      arrow_dir            = panel_cdmx,
      geo_sta_pq           = distances_cdmx,
      geo_census           = geo_cdmx,
      micro_census         = micro_cdmx,
      socio_var            = "education",
      n_groups             = 5L,
      group_name           = "edu_quintile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      return_data          = FALSE)

  # Reuse exposure for income; retain both sets of returned paths.
  income <- run_idw_city(city_label = "CDMX",
      city_id              = "cdmx_2020",
      arrow_dir            = panel_cdmx,
      geo_sta_pq           = distances_cdmx,
      geo_census           = geo_cdmx,
      micro_census         = micro_cdmx,
      socio_var            = "income",
      n_groups             = 5L,
      group_name           = "income_quintile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      out_suffix           = "income",
      reuse_exposure       = TRUE,
      return_data          = FALSE)
  idw_cdmx[[as.character(buffer_km)]] <- list(
      education            = idw_cdmx[[as.character(buffer_km)]],
      income               = income)

  # Santiago (zona 2017): education quintiles.
  idw_santiago[[as.character(buffer_km)]] <- run_idw_city(
      city_label           = "Santiago (zona 2017)",
      city_id              = "santiago_2017",
      arrow_dir            = panel_santiago,
      geo_sta_pq           = distances_santiago,
      geo_census           = geo_santiago,
      micro_census         = micro_santiago,
      socio_var            = "education",
      n_groups             = 5L,
      group_name           = "edu_quintile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      return_data          = FALSE)

  # Santiago 2024: education quintiles for the deferred robustness specification.
  idw_santiago_robustness[[as.character(buffer_km)]] <- run_idw_city(
      city_label           = "Santiago (comuna 2024)",
      city_id              = "santiago_2024",
      arrow_dir            = panel_santiago_robustness,
      geo_sta_pq           = distances_santiago_robustness,
      geo_census           = geo_santiago_robustness,
      micro_census         = micro_santiago_robustness,
      socio_var            = "education",
      n_groups             = 5L,
      group_name           = "edu_quintile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      return_data          = FALSE)

  # Sao Paulo: education quintiles.
  idw_sao_paulo[[as.character(buffer_km)]] <- run_idw_city(city_label = "Sao Paulo",
      city_id              = "sao_paulo_2010",
      arrow_dir            = panel_sao_paulo,
      geo_sta_pq           = distances_sao_paulo,
      geo_census           = geo_sao_paulo,
      micro_census         = micro_sao_paulo,
      socio_var            = "education",
      n_groups             = 5L,
      group_name           = "edu_quintile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      return_data          = FALSE)

  # Reuse exposure for income; retain both sets of returned paths.
  income <- run_idw_city(city_label = "Sao Paulo",
      city_id              = "sao_paulo_2010",
      arrow_dir            = panel_sao_paulo,
      geo_sta_pq           = distances_sao_paulo,
      geo_census           = geo_sao_paulo,
      micro_census         = micro_sao_paulo,
      socio_var            = "income",
      n_groups             = 10L,
      group_name           = "income_decile",
      buffer_km            = buffer_km,
      distance_power       = idw_distance_power,
      outdir_exp           = dir_idw,
      out_suffix           = "income",
      reuse_exposure       = TRUE,
      return_data          = FALSE)
  idw_sao_paulo[[as.character(buffer_km)]] <- list(
      education            = idw_sao_paulo[[as.character(buffer_km)]],
      income               = income)
}
