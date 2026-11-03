# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Estimate exposure differences and geographic coverage.
#
#' @Description: Read geographic IDW estimates and individual socioeconomic groups.
# run_city_exposure() returns weighted summaries, regression estimates and coverage.
# Run the city/group models at exposure_buffers_km, keep the city results in named lists
# and save combined tables in Section III. The 5 km buffer and the individual-level
# regression are robustness checks in scripts/process_data/robustness/.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read estimates and populations.
#   II.  Process data: estimate group differences for each city and grouping.
#   III. Save outputs: write the group tables for each buffer.
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

# Read exposure for each buffer; the income models use these same estimates.
exposure_bogota_by_buffer   <- list()
exposure_cdmx_by_buffer     <- list()
exposure_santiago_by_buffer <- list()
exposure_sp_by_buffer       <- list()
for (buffer_km in exposure_buffers_km) {
  exposure_bogota_by_buffer[[as.character(buffer_km)]] <- read_idw_artifact(
      dir_idw, "bogota_2018", "idw_exposure", buffer_km)
  exposure_cdmx_by_buffer[[as.character(buffer_km)]] <- read_idw_artifact(
      dir_idw, "cdmx_2020", "idw_exposure", buffer_km)
  exposure_santiago_by_buffer[[as.character(buffer_km)]] <- read_idw_artifact(
      dir_idw, "santiago_2017", "idw_exposure", buffer_km)
  exposure_sp_by_buffer[[as.character(buffer_km)]] <- read_idw_artifact(
      dir_idw, "sao_paulo_2010", "idw_exposure", buffer_km)
}

# ==========================================================================================
# II: Process data
# ==========================================================================================
# Keep the results for each buffer; the individual city calls remain visible
estimates_by_buffer <- list()
city_runs_by_buffer <- list()

# Select the first buffer when executing the loop body line by line in RStudio.
buffer_km <- exposure_buffers_km[1]

for (buffer_km in exposure_buffers_km) {
  exposure_bogota   <- exposure_bogota_by_buffer[[as.character(buffer_km)]]
  exposure_cdmx     <- exposure_cdmx_by_buffer[[as.character(buffer_km)]]
  exposure_santiago <- exposure_santiago_by_buffer[[as.character(buffer_km)]]
  exposure_sp       <- exposure_sp_by_buffer[[as.character(buffer_km)]]

  # Bogotá: education quintiles
  bogota <- run_city_exposure(city           = "Bogota",
                              city_id        = "bogota_2018",
                              exposure_dt    = exposure_bogota,
                              individual_dt  = individual_bogota,
                              geo_station_pq = dist_bogota,
                              socio_var      = "education",
                              group_col      = "edu_quintile",
                              n_groups       = 5L,
                              year           = analysis_year,
                              buffer_km      = buffer_km)

  # CDMX: education quintiles
  cdmx <- run_city_exposure(city           = "CDMX",
                            city_id        = "cdmx_2020",
                            exposure_dt    = exposure_cdmx,
                            individual_dt  = individual_cdmx,
                            geo_station_pq = dist_cdmx,
                            socio_var      = "education",
                            group_col      = "edu_quintile",
                            n_groups       = 5L,
                            year           = analysis_year,
                            buffer_km      = buffer_km)

  # Santiago: education quintiles, zonas censales 2017
  santiago <- run_city_exposure(city           = "Santiago",
                                city_id        = "santiago_2017",
                                exposure_dt    = exposure_santiago,
                                individual_dt  = individual_santiago,
                                geo_station_pq = dist_santiago,
                                socio_var      = "education",
                                group_col      = "edu_quintile",
                                n_groups       = 5L,
                                year           = analysis_year,
                                buffer_km      = buffer_km)

  # São Paulo: education quintiles
  sao_paulo <- run_city_exposure(city           = "Sao Paulo",
                                 city_id        = "sao_paulo_2010",
                                 exposure_dt    = exposure_sp,
                                 individual_dt  = individual_sp,
                                 geo_station_pq = dist_sp,
                                 socio_var      = "education",
                                 group_col      = "edu_quintile",
                                 n_groups       = 5L,
                                 year           = analysis_year,
                                 buffer_km      = buffer_km)

  # CDMX: income quintiles
  cdmx_inc <- run_city_exposure(city           = "CDMX",
                                city_id        = "cdmx_2020",
                                exposure_dt    = exposure_cdmx,
                                individual_dt  = individual_cdmx_inc,
                                geo_station_pq = dist_cdmx,
                                socio_var      = "income",
                                group_col      = "income_quintile",
                                n_groups       = 5L,
                                year           = analysis_year,
                                buffer_km      = buffer_km)

  # São Paulo: income deciles
  sao_paulo_inc <- run_city_exposure(city           = "Sao Paulo",
                                     city_id        = "sao_paulo_2010",
                                     exposure_dt    = exposure_sp,
                                     individual_dt  = individual_sp_inc,
                                     geo_station_pq = dist_sp,
                                     socio_var      = "income",
                                     group_col      = "income_decile",
                                     n_groups       = 10L,
                                     year           = analysis_year,
                                     buffer_km      = buffer_km)

  # Combine estimates and coverage across cities.
  tables <- stack_exposure_runs(
    edu_runs = list(bogota, cdmx, santiago, sao_paulo),
    inc_runs = list(cdmx_inc, sao_paulo_inc))

  city_runs_by_buffer[[as.character(buffer_km)]] <- list(
      bogota = bogota, cdmx = cdmx, santiago = santiago, sao_paulo = sao_paulo,
      cdmx_income = cdmx_inc, sao_paulo_income = sao_paulo_inc)
  estimates_by_buffer[[as.character(buffer_km)]] <- tables
}

# ==========================================================================================
# III: Save outputs
# ==========================================================================================
# Save the five combined tables for each buffer
group_files <- list()
for (buffer_km in exposure_buffers_km) {
  group_files[[as.character(buffer_km)]] <- save_exposure_tables(
      tables = estimates_by_buffer[[as.character(buffer_km)]],
      out_dir = dir_out, buffer_km = buffer_km, year = analysis_year)
}
