# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate exposure differences and coverage at the 5 km robustness buffer.
#
#' @Description: Read the 5 km IDW estimates of estimate_idw_5km.R and the individual
# socioeconomic groups. run_city_exposure() returns the weighted summaries, regression
# estimates and coverage of scripts/process_data/exposure/estimate_exposure.R for the
# same city/group models. Save the combined tables to data/processed/idw_regressions/.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read estimates and populations.
#   II.  Process data: estimate group differences for each city and grouping.
#   III. Save outputs: write the combined 5 km tables.
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
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))
source(here::here("config", "analysis_settings.R"))

# Set input folders and the output folder
dir_idw  <- here::here("data", "processed", "idw_estimates")
dir_dist <- here::here("data", "processed", "distances_matrices")
dir_out  <- here::here("data", "processed", "idw_regressions")

# Set the geographic-unit-to-station distances used to measure coverage
dist_bogota   <- here::here(dir_dist, "bogota_2018",
                            "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_dist, "cdmx_2020",
                            "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_dist, "santiago_2017",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_dist, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")

# Read individual group membership; income exists only for CDMX and São Paulo.
individual_bogota   <- read_idw_artifact(dir_idw, "bogota_2018", "indiv_groups")
individual_cdmx     <- read_idw_artifact(dir_idw, "cdmx_2020", "indiv_groups")
individual_santiago <- read_idw_artifact(dir_idw, "santiago_2017", "indiv_groups")
individual_sp       <- read_idw_artifact(dir_idw, "sao_paulo_2010", "indiv_groups")
individual_cdmx_inc <- read_idw_artifact(dir_idw, "cdmx_2020", "indiv_groups",
                                         suffix = "_income")
individual_sp_inc   <- read_idw_artifact(dir_idw, "sao_paulo_2010", "indiv_groups",
                                         suffix = "_income")

# Read the 5 km exposure; the income models use these same estimates.
exposure_bogota   <- read_idw_artifact(dir_idw, "bogota_2018", "idw_exposure",
                                       robustness_buffer_km)
exposure_cdmx     <- read_idw_artifact(dir_idw, "cdmx_2020", "idw_exposure",
                                       robustness_buffer_km)
exposure_santiago <- read_idw_artifact(dir_idw, "santiago_2017", "idw_exposure",
                                       robustness_buffer_km)
exposure_sp       <- read_idw_artifact(dir_idw, "sao_paulo_2010", "idw_exposure",
                                       robustness_buffer_km)

# ========================================================================================
# II: Process data
# ========================================================================================
# Bogotá: education quintiles
bogota_5km <- run_city_exposure(city           = "Bogota",
                                city_id        = "bogota_2018",
                                exposure_dt    = exposure_bogota,
                                individual_dt  = individual_bogota,
                                geo_station_pq = dist_bogota,
                                socio_var      = "education",
                                group_col      = "edu_quintile",
                                n_groups       = 5L,
                                year           = analysis_year,
                                buffer_km      = robustness_buffer_km)

# CDMX: education quintiles
cdmx_5km <- run_city_exposure(city           = "CDMX",
                              city_id        = "cdmx_2020",
                              exposure_dt    = exposure_cdmx,
                              individual_dt  = individual_cdmx,
                              geo_station_pq = dist_cdmx,
                              socio_var      = "education",
                              group_col      = "edu_quintile",
                              n_groups       = 5L,
                              year           = analysis_year,
                              buffer_km      = robustness_buffer_km)

# Santiago: education quintiles, zonas censales 2017
santiago_5km <- run_city_exposure(city           = "Santiago",
                                  city_id        = "santiago_2017",
                                  exposure_dt    = exposure_santiago,
                                  individual_dt  = individual_santiago,
                                  geo_station_pq = dist_santiago,
                                  socio_var      = "education",
                                  group_col      = "edu_quintile",
                                  n_groups       = 5L,
                                  year           = analysis_year,
                                  buffer_km      = robustness_buffer_km)

# São Paulo: education quintiles
sao_paulo_5km <- run_city_exposure(city           = "Sao Paulo",
                                   city_id        = "sao_paulo_2010",
                                   exposure_dt    = exposure_sp,
                                   individual_dt  = individual_sp,
                                   geo_station_pq = dist_sp,
                                   socio_var      = "education",
                                   group_col      = "edu_quintile",
                                   n_groups       = 5L,
                                   year           = analysis_year,
                                   buffer_km      = robustness_buffer_km)

# CDMX: income quintiles
cdmx_inc_5km <- run_city_exposure(city           = "CDMX",
                                  city_id        = "cdmx_2020",
                                  exposure_dt    = exposure_cdmx,
                                  individual_dt  = individual_cdmx_inc,
                                  geo_station_pq = dist_cdmx,
                                  socio_var      = "income",
                                  group_col      = "income_quintile",
                                  n_groups       = 5L,
                                  year           = analysis_year,
                                  buffer_km      = robustness_buffer_km)

# São Paulo: income deciles
sao_paulo_inc_5km <- run_city_exposure(city           = "Sao Paulo",
                                       city_id        = "sao_paulo_2010",
                                       exposure_dt    = exposure_sp,
                                       individual_dt  = individual_sp_inc,
                                       geo_station_pq = dist_sp,
                                       socio_var      = "income",
                                       group_col      = "income_decile",
                                       n_groups       = 10L,
                                       year           = analysis_year,
                                       buffer_km      = robustness_buffer_km)

# Combine estimates and coverage across cities.
tables_5km <- stack_exposure_runs(
  edu_runs = list(bogota_5km, cdmx_5km, santiago_5km, sao_paulo_5km),
  inc_runs = list(cdmx_inc_5km, sao_paulo_inc_5km))

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the five 5 km tables as Parquet with CSV copies
files_5km <- save_exposure_tables(tables    = tables_5km,
                                  out_dir   = dir_out,
                                  buffer_km = robustness_buffer_km,
                                  year      = analysis_year)
