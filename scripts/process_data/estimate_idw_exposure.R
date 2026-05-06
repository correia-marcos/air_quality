# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute air pollution exposure at the geographic and individual level using IDW.
#
# @Description: This script processes cleaned hive-partitioned Arrow datasets of ground 
# station data, pre-computed spatial distance matrices, and census microdata. It applies 
# Inverse Distance Weighting (IDW) interpolation within 3km and 5km buffers, leveraging 
# DuckDB to execute out-of-core aggregations without exceeding active memory limits.
#
# @Summary:
#   I.   Import data: Define paths for cleaned Arrow datasets, matrices, and census files.
#   II.  Process: Apply IDW interpolation for Bogotá, CDMX, Santiago, and São Paulo.
#   III. Save: Export geographic yearly exposure and individual quintiles as Parquets.
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
dir_cleaned   <- here::here("data", "processed", "outlier_detection")
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_census    <- here::here("data", "interim", "census")
outdir_exp    <- here::here("data", "processed", "idw_estimates")

# Define the cleaned Arrow datasets paths
arrow_bogota_dir   <- here::here(dir_cleaned, "bogota_metro_clean")
arrow_cdmx_dir     <- here::here(dir_cleaned, "cdmx_metro_clean")
arrow_santiago_dir <- here::here(dir_cleaned, "santiago_metro_clean")
arrow_sp_dir       <- here::here(dir_cleaned, "sao_paulo_metro_clean")

# Define distance Matrices (Station-to-Geo) paths
bogota_geo_dist   <- here::here(dir_distances, "bogota_2018", 
                                "matrix_geo_station_distances.parquet")
cdmx_geo_dist     <- here::here(dir_distances, "cdmx_2020", 
                                "matrix_geo_station_distances.parquet")
santiago_geo_dist <- here::here(dir_distances, "santiago_2024", 
                                "matrix_geo_station_distances.parquet")
sp_geo_dist       <- here::here(dir_distances, "sao_paulo_2010", 
                                "matrix_geo_station_distances.parquet")

# Define Census Microdata Paths
bogota_census_csv   <- here::here(dir_census, "bogota_2018",
                                  "census_2018_metro_individual.csv")
cdmx_census_csv     <- here::here(dir_census, "cdmx_extended_2020", 
                                  "census_metro_individual_2020.csv")
santiago_census_csv <- here::here(dir_census, "santiago_2024", 
                                  "census_santiago_individual_2024.csv")
sp_census_csv       <- here::here(dir_census, "sao_paulo_2010", 
                                  "census_sp_individual_2010.csv")

# Read Census Data (data.table::fread auto-detects column types)
bogota_census   <- data.table::fread(bogota_census_csv)
cdmx_census     <- data.table::fread(cdmx_census_csv)
santiago_census <- data.table::fread(santiago_census_csv)
sp_census       <- data.table::fread(sp_census_csv)

# ============================================================================================
# II: Process and save
# ============================================================================================
# Create the folder of the results, if not yet created
dir.create(outdir_exp, recursive = TRUE, showWarnings = FALSE)

for (buffer in c(3, 5)) {
  # Bogotá — 2018 CNPV
  exp_bogota <- aggregate_idw_exposure(
    arrow_dir      = arrow_bogota_dir,
    geo_sta_pq     = bogota_geo_dist,
    census_col     = bogota_census,
    geo_id_col     = "GEO_ID",
    pop_col        = "fe",
    edu_col        = "escolaridad",
    quintile_level = "individual",
    buffer_km      = buffer,
    distance_power = 2,
    mem_gb         = 32,
    out_dir        = here::here(outdir_exp, "bogota_2018"),
    out_name       = sprintf("bogota_2018_%dkm", buffer)
  )
  message("Bogotá ", buffer, " km: ", exp_bogota$exposure_path)

  # CDMX — Censo 2020
  exp_cdmx <- aggregate_idw_exposure(
    arrow_dir      = arrow_cdmx_dir,
    geo_sta_pq     = cdmx_geo_dist,
    census_col     = cdmx_census,
    geo_id_col     = "CVE_MUN",
    pop_col        = "FACTOR",
    edu_col        = "escolaridad",
    quintile_level = "individual",
    buffer_km      = buffer,
    distance_power = 2,
    out_dir        = here::here(outdir_exp, "cdmx_2020"),
    out_name       = sprintf("cdmx_2020_%dkm", buffer)
  )
  message("CDMX ", buffer, " km: ", exp_cdmx$exposure_path)

  # Santiago — Censo 2024 INE
  exp_sgo <- aggregate_idw_exposure(
    arrow_dir      = arrow_santiago_dir,
    geo_sta_pq     = santiago_geo_dist,
    census_col     = santiago_census,
    geo_id_col     = "comuna",
    pop_col        = "fe",
    edu_col        = "educ_years",
    quintile_level = "individual",
    buffer_km      = buffer,
    distance_power = 2,
    out_dir        = here::here(outdir_exp, "santiago_2024"),
    out_name       = sprintf("santiago_2024_%dkm", buffer)
  )
  message("Santiago ", buffer, " km: ", exp_sgo$exposure_path)

  # São Paulo — Censo IBGE 2010
  exp_sp <- aggregate_idw_exposure(
    arrow_dir      = arrow_sp_dir,
    geo_sta_pq     = sp_geo_dist,
    census_col     = sp_census,
    geo_id_col     = "code_weighting",
    pop_col        = "weight",
    edu_col        = "years_schooling",
    quintile_level = "individual",
    buffer_km      = buffer,
    distance_power = 2,
    out_dir        = here::here(outdir_exp, "sao_paulo_2010"),
    out_name       = sprintf("sao_paulo_2010_%dkm", buffer)
  )
  message("São Paulo ", buffer, " km: ", exp_sp$exposure_path)
}

cat("Script from the IDB project executed successfully in the Docker container!\n")