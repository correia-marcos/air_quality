# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Estimate education-group exposure using imputed hourly panels.
#
#' @Description: Use the 2023 imputed panels for the manuscript robustness check at
# 3 km. First run IDW and form education quintiles; then estimate exposure differences and
# coverage with the same functions as the observed-data analysis. IDW writes its large
# checkpoints during processing. Regression tables remain available before saving in III.
#
#' @Summary:
#   I.   Import data: source functions, declare paths and read census data.
#   II.  Process data: interpolate pollution and estimate education-group differences.
#   III. Save outputs: write regression estimates, group summaries and coverage.
#
#' @Date: September 2026
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "idw_exposure.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))
source(here::here("config", "analysis_settings.R"))

# Define input and output folders
dir_imputed   <- here::here("data", "processed", "imputed_ols")
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_census    <- here::here("data", "interim", "census")
outdir_exp    <- here::here("data", "processed", "idw_estimates_imputed")
outdir_reg    <- here::here("data", "processed", "idw_regressions_imputed")

# The manuscript reports this specification at the main buffer and year only.
year          <- imputation_year
buffer_km     <- imputed_exposure_buffer_km
distance_power <- idw_distance_power

# Define the imputed Arrow dataset paths, restricted to the analysis year
arrow_bogota   <- here::here(dir_imputed, "bogota_imputed", paste0("year=", year))
arrow_cdmx     <- here::here(dir_imputed, "cdmx_imputed", paste0("year=", year))
arrow_santiago <- here::here(dir_imputed, "santiago_imputed", paste0("year=", year))
arrow_sp       <- here::here(dir_imputed, "sao_paulo_imputed", paste0("year=", year))

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

# ==========================================================================================
# II: Process data
# ==========================================================================================
# IDW writes geographic exposure and individual group membership, returning their paths.

idw_bogota <- run_idw_city(city_label     = "Bogota",
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

idw_cdmx <- run_idw_city(city_label     = "CDMX",
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

idw_santiago <- run_idw_city(city_label     = "Santiago",
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

idw_sp <- run_idw_city(city_label     = "Sao Paulo",
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

# Read the checkpoints just produced for the exposure regressions.
exposure_bogota   <- arrow::read_parquet(idw_bogota$individual$exposure_path)
exposure_cdmx     <- arrow::read_parquet(idw_cdmx$individual$exposure_path)
exposure_santiago <- arrow::read_parquet(idw_santiago$individual$exposure_path)
exposure_sp       <- arrow::read_parquet(idw_sp$individual$exposure_path)

individual_bogota   <- arrow::read_parquet(idw_bogota$individual$individual_path)
individual_cdmx     <- arrow::read_parquet(idw_cdmx$individual$individual_path)
individual_santiago <- arrow::read_parquet(idw_santiago$individual$individual_path)
individual_sp       <- arrow::read_parquet(idw_sp$individual$individual_path)

# Estimate each city separately so its estimates and coverage remain easy to inspect.

bogota <- run_city_exposure(city           = "Bogota",
                            city_id        = "bogota_2018",
                            exposure_dt    = exposure_bogota,
                            individual_dt  = individual_bogota,
                            geo_station_pq = dist_bogota,
                            socio_var      = "education",
                            group_col      = "edu_quintile",
                            n_groups       = 5L,
                            year           = year,
                            buffer_km      = buffer_km)

cdmx <- run_city_exposure(city           = "CDMX",
                          city_id        = "cdmx_2020",
                          exposure_dt    = exposure_cdmx,
                          individual_dt  = individual_cdmx,
                          geo_station_pq = dist_cdmx,
                          socio_var      = "education",
                          group_col      = "edu_quintile",
                          n_groups       = 5L,
                          year           = year,
                          buffer_km      = buffer_km)

santiago <- run_city_exposure(city           = "Santiago",
                              city_id        = "santiago_2017",
                              exposure_dt    = exposure_santiago,
                              individual_dt  = individual_santiago,
                              geo_station_pq = dist_santiago,
                              socio_var      = "education",
                              group_col      = "edu_quintile",
                              n_groups       = 5L,
                              year           = year,
                              buffer_km      = buffer_km)

sao_paulo <- run_city_exposure(city           = "Sao Paulo",
                               city_id        = "sao_paulo_2010",
                               exposure_dt    = exposure_sp,
                               individual_dt  = individual_sp,
                               geo_station_pq = dist_sp,
                               socio_var      = "education",
                               group_col      = "edu_quintile",
                               n_groups       = 5L,
                               year           = year,
                               buffer_km      = buffer_km)

runs <- list(bogota = bogota, cdmx = cdmx, santiago = santiago, sao_paulo = sao_paulo)
tables <- list(ci_estimates_education    = stack_city_tables(runs, "ci"),
               group_summaries_education = stack_city_tables(runs, "summary"),
               coverage                  = stack_city_tables(runs, "coverage"))

# ==========================================================================================
# III: Save outputs
# ==========================================================================================
files <- save_exposure_tables(tables = tables, out_dir = outdir_reg,
                              buffer_km = buffer_km, year = year)
