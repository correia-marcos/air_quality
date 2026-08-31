# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Re-estimate the paper's exposure results on the imputed hourly panels.
#
#' @Description: The robustness specification behind the manuscript's "_imp" figures. It
# repeats the main exposure chain — inverse-distance interpolation to geographic units,
# then the saturated regression of exposure on education quintile — using the panels that
# combine the observed readings with the values impute_missing_hourly.R predicted for the
# gaps. Only the paper's 3 km buffer and 2023 are run, because that is the only
# combination the manuscript reports for this specification. Outputs land in their own
# folders so the observed-data results are never overwritten.
#
#' @Summary:
#   I.   Import data: imputed panels, distance matrices and census microdata.
#   II.  Interpolate exposure to geographic units, one city at a time.
#   III. Estimate the group regressions and save the stacked tables.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_imputed   <- here::here("data", "processed", "imputed_ols")
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_census    <- here::here("data", "interim", "census")
outdir_exp    <- here::here("data", "processed", "idw_estimates_imputed")
outdir_reg    <- here::here("data", "processed", "idw_regressions_imputed")

# The manuscript reports this specification at the main buffer and year only.
analysis_year  <- 2023L
buffer_km      <- 3
distance_power <- 1

# Define the imputed Arrow dataset paths, restricted to the analysis year
arrow_bogota   <- here::here(dir_imputed, "bogota_imputed", "year=2023")
arrow_cdmx     <- here::here(dir_imputed, "cdmx_imputed", "year=2023")
arrow_santiago <- here::here(dir_imputed, "santiago_imputed", "year=2023")
arrow_sp       <- here::here(dir_imputed, "sao_paulo_imputed", "year=2023")

# Define geo-to-station distance matrix paths
dist_bogota   <- here::here(dir_distances, "bogota_2018",
                            "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_distances, "cdmx_2020",
                            "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_distances, "santiago_2017",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_distances, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")

# Define individual census paths
micro_bogota_pq   <- here::here(dir_census, "bogota_2018",
                                "census_2018_metro_individual.parquet")
micro_cdmx_pq     <- here::here(dir_census, "cdmx_extended_2020",
                                "census_metro_individual_2020.parquet")
micro_santiago_pq <- here::here(dir_census, "santiago_2017",
                                "census_individual_2017.parquet")
micro_sp_pq       <- here::here(dir_census, "sao_paulo_2010",
                                "census_sp_individual_2010.parquet")

# Define collapsed census paths
geo_bogota_pq   <- here::here(dir_census, "bogota_2018",
                              "census_2018_metro_collapsed.parquet")
geo_cdmx_pq     <- here::here(dir_census, "cdmx_extended_2020",
                              "collapse_metro_area_2020.parquet")
geo_santiago_pq <- here::here(dir_census, "santiago_2017",
                              "census_collapsed_2017.parquet")
geo_sp_pq       <- here::here(dir_census, "sao_paulo_2010",
                              "census_sp_collapsed_2010.parquet")

# Read census microdata
mi_bogota   <- data.table::as.data.table(arrow::read_parquet(micro_bogota_pq))
mi_cdmx     <- data.table::as.data.table(arrow::read_parquet(micro_cdmx_pq))
mi_santiago <- data.table::as.data.table(arrow::read_parquet(micro_santiago_pq))
mi_sp       <- data.table::as.data.table(arrow::read_parquet(micro_sp_pq))

# Read collapsed census data
geo_bogota   <- data.table::as.data.table(arrow::read_parquet(geo_bogota_pq))
geo_cdmx     <- data.table::as.data.table(arrow::read_parquet(geo_cdmx_pq))
geo_santiago <- data.table::as.data.table(arrow::read_parquet(geo_santiago_pq))
geo_sp       <- data.table::as.data.table(arrow::read_parquet(geo_sp_pq))

# ============================================================================================
# II: Interpolate exposure to geographic units
# ============================================================================================
dir.create(outdir_exp, recursive = TRUE, showWarnings = FALSE)

idw_bogota <- run_idw_city(
  city_label     = "Bogota",
  city_id        = "bogota_2018",
  arrow_dir      = arrow_bogota,
  geo_sta_pq     = dist_bogota,
  geo_census     = geo_bogota,
  micro_census   = mi_bogota,
  socio_var      = "education",
  n_groups       = 5L,
  group_name     = "edu_quintile",
  buffer_km      = buffer_km,
  outdir_exp     = outdir_exp,
  distance_power = distance_power)

idw_cdmx <- run_idw_city(
  city_label     = "CDMX",
  city_id        = "cdmx_2020",
  arrow_dir      = arrow_cdmx,
  geo_sta_pq     = dist_cdmx,
  geo_census     = geo_cdmx,
  micro_census   = mi_cdmx,
  socio_var      = "education",
  n_groups       = 5L,
  group_name     = "edu_quintile",
  buffer_km      = buffer_km,
  outdir_exp     = outdir_exp,
  distance_power = distance_power)

idw_santiago <- run_idw_city(
  city_label     = "Santiago",
  city_id        = "santiago_2017",
  arrow_dir      = arrow_santiago,
  geo_sta_pq     = dist_santiago,
  geo_census     = geo_santiago,
  micro_census   = mi_santiago,
  socio_var      = "education",
  n_groups       = 5L,
  group_name     = "edu_quintile",
  buffer_km      = buffer_km,
  outdir_exp     = outdir_exp,
  distance_power = distance_power)

idw_sp <- run_idw_city(
  city_label     = "Sao Paulo",
  city_id        = "sao_paulo_2010",
  arrow_dir      = arrow_sp,
  geo_sta_pq     = dist_sp,
  geo_census     = geo_sp,
  micro_census   = mi_sp,
  socio_var      = "education",
  n_groups       = 5L,
  group_name     = "edu_quintile",
  buffer_km      = buffer_km,
  outdir_exp     = outdir_exp,
  distance_power = distance_power)

# ============================================================================================
# III: Group regressions on the imputed exposure
# ============================================================================================
dir.create(outdir_reg, recursive = TRUE, showWarnings = FALSE)

exposure_bogota   <- read_idw_artifact(outdir_exp, "bogota_2018", "idw_exposure",
                                       buffer_km)
exposure_cdmx     <- read_idw_artifact(outdir_exp, "cdmx_2020", "idw_exposure",
                                       buffer_km)
exposure_santiago <- read_idw_artifact(outdir_exp, "santiago_2017", "idw_exposure",
                                       buffer_km)
exposure_sp       <- read_idw_artifact(outdir_exp, "sao_paulo_2010", "idw_exposure",
                                       buffer_km)

individual_bogota   <- read_idw_artifact(outdir_exp, "bogota_2018", "indiv_groups")
individual_cdmx     <- read_idw_artifact(outdir_exp, "cdmx_2020", "indiv_groups")
individual_santiago <- read_idw_artifact(outdir_exp, "santiago_2017", "indiv_groups")
individual_sp       <- read_idw_artifact(outdir_exp, "sao_paulo_2010", "indiv_groups")

bogota <- run_city_exposure(
  city = "Bogota", city_id = "bogota_2018",
  exposure_dt = exposure_bogota, individual_dt = individual_bogota,
  geo_station_pq = dist_bogota, socio_var = "education",
  group_col = "edu_quintile", n_groups = 5L,
  year = analysis_year, buffer_km = buffer_km)

cdmx <- run_city_exposure(
  city = "CDMX", city_id = "cdmx_2020",
  exposure_dt = exposure_cdmx, individual_dt = individual_cdmx,
  geo_station_pq = dist_cdmx, socio_var = "education",
  group_col = "edu_quintile", n_groups = 5L,
  year = analysis_year, buffer_km = buffer_km)

santiago <- run_city_exposure(
  city = "Santiago", city_id = "santiago_2017",
  exposure_dt = exposure_santiago, individual_dt = individual_santiago,
  geo_station_pq = dist_santiago, socio_var = "education",
  group_col = "edu_quintile", n_groups = 5L,
  year = analysis_year, buffer_km = buffer_km)

sao_paulo <- run_city_exposure(
  city = "Sao Paulo", city_id = "sao_paulo_2010",
  exposure_dt = exposure_sp, individual_dt = individual_sp,
  geo_station_pq = dist_sp, socio_var = "education",
  group_col = "edu_quintile", n_groups = 5L,
  year = analysis_year, buffer_km = buffer_km)

runs <- list(bogota, cdmx, santiago, sao_paulo)

tables <- list(
  ci_estimates_education   = stack_city_tables(runs, "ci"),
  group_summaries_education = stack_city_tables(runs, "summary"),
  coverage                 = stack_city_tables(runs, "coverage"))

cat("\nGeographic coverage behind each imputed regression:\n")
print(tables$coverage[, .(city, pollutant, n_geo_metro, n_geo_in_buffer,
                          n_geo_estimation,
                          share_pop = round(share_pop_estimation, 3))])

save_exposure_tables(tables, outdir_reg, buffer_km, analysis_year)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
