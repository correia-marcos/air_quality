# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute air pollution exposure at the geographic and individual level using IDW.
#
# @Description: This script processes cleaned Arrow datasets of ground station data,
# pre-computed distance matrices, and census data. It applies IDW interpolation
# within 3km and 5km buffers using DuckDB for out-of-core aggregation. Education
# quintiles are produced for all four cities. Income groups are produced only for the
# two cities whose census carries income: deciles for Sao Paulo, but quintiles for
# CDMX, whose 63 municipalities leave too few clusters to identify 10 coefficients.
#
# @Summary:
#   I.   Import data: Define paths for Arrow datasets, matrices, and census files.
#   II.  Process: Apply IDW interpolation for each city, grouping, and buffer.
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define general input and output folders
dir_cleaned   <- here::here("data", "processed", "monitoring_stations_outliers")
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_census    <- here::here("data", "interim", "census")
outdir_exp    <- here::here("data", "processed", "idw_estimates")

# Define cleaned Arrow dataset paths
arrow_bogota   <- here::here(dir_cleaned, "bogota_metro_clean", "year=2023")
arrow_cdmx     <- here::here(dir_cleaned, "cdmx_metro_clean", "year=2023")
arrow_santiago <- here::here(dir_cleaned, "santiago_metro_clean", "year=2023")
arrow_sp       <- here::here(dir_cleaned, "sao_paulo_metro_clean", "year=2023")

# Define geo-to-station distance matrix paths
dist_bogota        <- here::here(dir_distances, "bogota_2018",
                                 "matrix_geo_station_distances.parquet")
dist_cdmx          <- here::here(dir_distances, "cdmx_2020",
                                 "matrix_geo_station_distances.parquet")
dist_santiago      <- here::here(dir_distances, "santiago_2017",
                                      "matrix_geo_station_distances.parquet")
dist_santiago_rob  <- here::here(dir_distances, "santiago_2024",
                                  "matrix_geo_station_distances.parquet")
dist_sp            <- here::here(dir_distances, "sao_paulo_2010",
                                 "matrix_geo_station_distances.parquet")

# Define individual census paths
micro_bogota_pq       <- here::here(dir_census, "bogota_2018",
                                    "census_2018_metro_individual.parquet")
micro_cdmx_pq         <- here::here(dir_census, "cdmx_extended_2020",
                                    "census_metro_individual_2020.parquet")
micro_santiago_pq     <- here::here(dir_census, "santiago_2017",
                                    "census_individual_2017.parquet")
micro_santiago_rob_pq <- here::here(dir_census, "santiago_2024",
                                    "census_santiago_individual_2024.parquet")
micro_sp_pq           <- here::here(dir_census, "sao_paulo_2010",
                                    "census_sp_individual_2010.parquet")

# Define collapsed census paths
geo_bogota_pq       <- here::here(dir_census, "bogota_2018",
                                  "census_2018_metro_collapsed.parquet")
geo_cdmx_pq         <- here::here(dir_census, "cdmx_extended_2020",
                                  "collapse_metro_area_2020.parquet")
geo_santiago_pq     <- here::here(dir_census, "santiago_2017",
                                  "census_collapsed_2017.parquet")
geo_santiago_rob_pq <- here::here(dir_census, "santiago_2024",
                                  "census_santiago_collapsed_2024.parquet")
geo_sp_pq           <- here::here(dir_census, "sao_paulo_2010",
                                  "census_sp_collapsed_2010.parquet")

# Read census microdata. Parquet carries its own schema, so the geographic keys
# arrive as character without a colClasses argument.
mi_bogota       <- data.table::as.data.table(arrow::read_parquet(micro_bogota_pq))
mi_cdmx         <- data.table::as.data.table(arrow::read_parquet(micro_cdmx_pq))
mi_santiago     <- data.table::as.data.table(arrow::read_parquet(micro_santiago_pq))
mi_santiago_rob <- data.table::as.data.table(arrow::read_parquet(micro_santiago_rob_pq))
mi_sp           <- data.table::as.data.table(arrow::read_parquet(micro_sp_pq))

# Read collapsed census data
geo_bogota       <- data.table::as.data.table(arrow::read_parquet(geo_bogota_pq))
geo_cdmx         <- data.table::as.data.table(arrow::read_parquet(geo_cdmx_pq))
geo_santiago     <- data.table::as.data.table(arrow::read_parquet(geo_santiago_pq))
geo_santiago_rob <- data.table::as.data.table(arrow::read_parquet(geo_santiago_rob_pq))
geo_sp           <- data.table::as.data.table(arrow::read_parquet(geo_sp_pq))

# ============================================================================================
# II: Process and save
# ============================================================================================
# Create output folder if needed
dir.create(outdir_exp, recursive = TRUE, showWarnings = FALSE)

# Define IDW specifications
buffers_km     <- c(3, 5)
distance_power <- 1

# Run all cities for the baseline and robustness buffers
for (buffer in buffers_km) {
  # 1. Bogota -- education quintiles
  res_bogota <- run_idw_city(
    city_label      = "Bogota",
    city_id         = "bogota_2018",
    arrow_dir       = arrow_bogota,
    geo_sta_pq      = dist_bogota,
    geo_census      = geo_bogota,
    micro_census    = mi_bogota,
    geo_id_col      = "GEO_ID",
    geo_pop_col     = "weight",
    geo_group_var   = "education_mean",
    micro_id_col    = "GEO_ID",
    micro_pop_col   = "fe",
    micro_group_var = "escolaridad",
    n_groups        = 5L,
    group_name      = "edu_quintile",
    buffer_km       = buffer,
    outdir_exp      = outdir_exp,
    distance_power  = distance_power)
  
  # 2. CDMX -- education quintiles
  res_cdmx <- run_idw_city(
    city_label      = "CDMX",
    city_id         = "cdmx_2020",
    arrow_dir       = arrow_cdmx,
    geo_sta_pq      = dist_cdmx,
    geo_census      = geo_cdmx,
    micro_census    = mi_cdmx,
    geo_id_col      = "CVE_MUN",
    geo_pop_col     = "weight",
    geo_group_var   = "education_mean",
    micro_id_col    = "CVE_MUN",
    micro_pop_col   = "FACTOR",
    micro_group_var = "escolaridad",
    n_groups        = 5L,
    group_name      = "edu_quintile",
    buffer_km       = buffer,
    outdir_exp      = outdir_exp,
    distance_power  = distance_power)
  
  # 3. Santiago -- education quintiles, zonas censales 2017 (main specification)
  res_santiago <- run_idw_city(
    city_label      = "Santiago (zona 2017)",
    city_id         = "santiago_2017",
    arrow_dir       = arrow_santiago,
    geo_sta_pq      = dist_santiago,
    geo_census      = geo_santiago,
    micro_census    = mi_santiago,
    geo_id_col      = "zona_id",
    geo_pop_col     = "weight",
    geo_group_var   = "education_mean",
    micro_id_col    = "zona_id",
    micro_pop_col   = "fe",
    micro_group_var = "educ_years",
    n_groups        = 5L,
    group_name      = "edu_quintile",
    buffer_km       = buffer,
    outdir_exp      = outdir_exp,
    distance_power  = distance_power)

  # 3b. Santiago -- education quintiles, commune level (2024 robustness)
  res_santiago_rob <- run_idw_city(
    city_label      = "Santiago (comuna 2024)",
    city_id         = "santiago_2024",
    arrow_dir       = arrow_santiago,
    geo_sta_pq      = dist_santiago_rob,
    geo_census      = geo_santiago_rob,
    micro_census    = mi_santiago_rob,
    geo_id_col      = "CUT",
    geo_pop_col     = "weight",
    geo_group_var   = "education_mean",
    micro_id_col    = "comuna",
    micro_pop_col   = "fe",
    micro_group_var = "educ_years",
    n_groups        = 5L,
    group_name      = "edu_quintile",
    buffer_km       = buffer,
    outdir_exp      = outdir_exp,
    distance_power  = distance_power)
  
  # 4. Sao Paulo -- education quintiles
  res_sp <- run_idw_city(
    city_label      = "Sao Paulo",
    city_id         = "sao_paulo_2010",
    arrow_dir       = arrow_sp,
    geo_sta_pq      = dist_sp,
    geo_census      = geo_sp,
    micro_census    = mi_sp,
    geo_id_col      = "code_weighting",
    geo_pop_col     = "weight",
    geo_group_var   = "education_mean",
    micro_id_col    = "code_weighting",
    micro_pop_col   = "weight",
    micro_group_var = "years_schooling",
    n_groups        = 5L,
    group_name      = "edu_quintile",
    buffer_km       = buffer,
    outdir_exp      = outdir_exp,
    distance_power  = distance_power)
  
  # ----------------------------------------------------------------------------------------
  # Income: ONLY CDMX and SP, whose census carries it. CDMX gets quintiles, not deciles,
  # because its ~10 surviving municipalities cannot identify 10 coefficients.
  # 5. CDMX -- income quintiles
  res_cdmx_income <- run_idw_city(
    city_label      = "CDMX",
    city_id         = "cdmx_2020",
    arrow_dir       = arrow_cdmx,
    geo_sta_pq      = dist_cdmx,
    geo_census      = geo_cdmx,
    micro_census    = mi_cdmx,
    geo_id_col      = "CVE_MUN",
    geo_pop_col     = "weight",
    geo_group_var   = "income",
    micro_id_col    = "CVE_MUN",
    micro_pop_col   = "FACTOR",
    micro_group_var = "income",
    n_groups        = 5L,
    group_name      = "income_quintile",
    buffer_km       = buffer,
    outdir_exp      = outdir_exp,
    out_suffix      = "income",
    distance_power  = distance_power)

  # 6. Sao Paulo -- income deciles
  res_sp_income <- run_idw_city(
    city_label      = "Sao Paulo",
    city_id         = "sao_paulo_2010",
    arrow_dir       = arrow_sp,
    geo_sta_pq      = dist_sp,
    geo_census      = geo_sp,
    micro_census    = mi_sp,
    geo_id_col      = "code_weighting",
    geo_pop_col     = "weight",
    geo_group_var   = "income",
    micro_id_col    = "code_weighting",
    micro_pop_col   = "weight",
    micro_group_var = "income",
    n_groups        = 10L,
    group_name      = "income_decile",
    buffer_km       = buffer,
    outdir_exp      = outdir_exp,
    out_suffix      = "income",
    distance_power  = distance_power)
}

cat("Script from the IDB project executed successfully in the Docker container!\n")