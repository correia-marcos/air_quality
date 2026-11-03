# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate IDW exposure and socioeconomic groups at the 5 km robustness buffer.
#
#' @Description: Repeat the interpolation of scripts/process_data/exposure/estimate_idw.R
# at robustness_buffer_km, with the same cleaned 2023 panels, distances and census
# records: education quintiles for all cities, income quintiles for CDMX and income
# deciles for São Paulo. run_idw_city() writes the outputs and returns their paths; it
# also rewrites each city's individual groups, which do not depend on the buffer.
#
#' @Summary:
#   I.  Import data: source functions, declare paths and read census data.
#   II. Process and save data: estimate each city and retain the returned file paths.
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

# Read the required census data
micro_bogota    <- arrow::read_parquet(file_micro_bogota)
geo_bogota      <- arrow::read_parquet(file_geo_bogota)

micro_cdmx      <- arrow::read_parquet(file_micro_cdmx)
geo_cdmx        <- arrow::read_parquet(file_geo_cdmx)

micro_santiago  <- arrow::read_parquet(file_micro_santiago)
geo_santiago    <- arrow::read_parquet(file_geo_santiago)

micro_sao_paulo <- arrow::read_parquet(file_micro_sao_paulo)
geo_sao_paulo   <- arrow::read_parquet(file_geo_sao_paulo)

# ========================================================================================
# II: Process and save data
# ========================================================================================
# Bogotá: education quintiles.
idw_bogota_5km <- run_idw_city(city_label = "Bogota",
    city_id              = "bogota_2018",
    arrow_dir            = panel_bogota,
    geo_sta_pq           = distances_bogota,
    geo_census           = geo_bogota,
    micro_census         = micro_bogota,
    socio_var            = "education",
    n_groups             = 5L,
    group_name           = "edu_quintile",
    buffer_km            = robustness_buffer_km,
    distance_power       = idw_distance_power,
    outdir_exp           = dir_idw,
    return_data          = FALSE)

# CDMX: education quintiles, then income quintiles on the same exposure.
idw_cdmx_5km <- list()
idw_cdmx_5km$education <- run_idw_city(city_label = "CDMX",
    city_id              = "cdmx_2020",
    arrow_dir            = panel_cdmx,
    geo_sta_pq           = distances_cdmx,
    geo_census           = geo_cdmx,
    micro_census         = micro_cdmx,
    socio_var            = "education",
    n_groups             = 5L,
    group_name           = "edu_quintile",
    buffer_km            = robustness_buffer_km,
    distance_power       = idw_distance_power,
    outdir_exp           = dir_idw,
    return_data          = FALSE)

idw_cdmx_5km$income <- run_idw_city(city_label = "CDMX",
    city_id              = "cdmx_2020",
    arrow_dir            = panel_cdmx,
    geo_sta_pq           = distances_cdmx,
    geo_census           = geo_cdmx,
    micro_census         = micro_cdmx,
    socio_var            = "income",
    n_groups             = 5L,
    group_name           = "income_quintile",
    buffer_km            = robustness_buffer_km,
    distance_power       = idw_distance_power,
    outdir_exp           = dir_idw,
    out_suffix           = "income",
    reuse_exposure       = TRUE,
    return_data          = FALSE)

# Santiago (zona 2017): education quintiles.
idw_santiago_5km <- run_idw_city(city_label = "Santiago (zona 2017)",
    city_id              = "santiago_2017",
    arrow_dir            = panel_santiago,
    geo_sta_pq           = distances_santiago,
    geo_census           = geo_santiago,
    micro_census         = micro_santiago,
    socio_var            = "education",
    n_groups             = 5L,
    group_name           = "edu_quintile",
    buffer_km            = robustness_buffer_km,
    distance_power       = idw_distance_power,
    outdir_exp           = dir_idw,
    return_data          = FALSE)

# São Paulo: education quintiles, then income deciles on the same exposure.
idw_sao_paulo_5km <- list()
idw_sao_paulo_5km$education <- run_idw_city(city_label = "Sao Paulo",
    city_id              = "sao_paulo_2010",
    arrow_dir            = panel_sao_paulo,
    geo_sta_pq           = distances_sao_paulo,
    geo_census           = geo_sao_paulo,
    micro_census         = micro_sao_paulo,
    socio_var            = "education",
    n_groups             = 5L,
    group_name           = "edu_quintile",
    buffer_km            = robustness_buffer_km,
    distance_power       = idw_distance_power,
    outdir_exp           = dir_idw,
    return_data          = FALSE)

idw_sao_paulo_5km$income <- run_idw_city(city_label = "Sao Paulo",
    city_id              = "sao_paulo_2010",
    arrow_dir            = panel_sao_paulo,
    geo_sta_pq           = distances_sao_paulo,
    geo_census           = geo_sao_paulo,
    micro_census         = micro_sao_paulo,
    socio_var            = "income",
    n_groups             = 10L,
    group_name           = "income_decile",
    buffer_km            = robustness_buffer_km,
    distance_power       = idw_distance_power,
    outdir_exp           = dir_idw,
    out_suffix           = "income",
    reuse_exposure       = TRUE,
    return_data          = FALSE)
