# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute air pollution exposure at the geographic and individual level using IDW.
#
# @Description: This script processes cleaned Arrow datasets of ground station data,
# pre-computed distance matrices, and census data. It applies IDW interpolation
# within 3km and 5km buffers using DuckDB for out-of-core aggregation.
#
# @Summary:
#   I.   Import data: Define paths for Arrow datasets, matrices, and census files.
#   II.  Process: Apply IDW interpolation for each city and exposure level.
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

# Define cleaned Arrow dataset paths
arrow_bogota   <- here::here(dir_cleaned, "bogota_metro_clean")
arrow_cdmx     <- here::here(dir_cleaned, "cdmx_metro_clean")
arrow_santiago <- here::here(dir_cleaned, "santiago_metro_clean")
arrow_sp       <- here::here(dir_cleaned, "sao_paulo_metro_clean")

# Define geo-to-station distance matrix paths
dist_bogota   <- here::here(dir_distances, "bogota_2018",
                            "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_distances, "cdmx_2020",
                            "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_distances, "santiago_2024",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_distances, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")

# Define individual census paths
micro_bogota_csv   <- here::here(dir_census, "bogota_2018",
                                 "census_2018_metro_individual.csv")
micro_cdmx_csv     <- here::here(dir_census, "cdmx_extended_2020",
                                 "census_metro_individual_2020.csv")
micro_santiago_csv <- here::here(dir_census, "santiago_2024",
                                 "census_santiago_individual_2024.csv")
micro_sp_csv       <- here::here(dir_census, "sao_paulo_2010",
                                 "census_sp_individual_2010.csv")

# Define collapsed census paths
geo_bogota_csv   <- here::here(dir_census, "bogota_2018",
                               "census_2018_metro_collapsed.csv")
geo_cdmx_csv     <- here::here(dir_census, "cdmx_extended_2020",
                               "collapse_metro_area_2020.csv")
geo_santiago_csv <- here::here(dir_census, "santiago_2024",
                               "census_santiago_collapsed_2024.csv")
geo_sp_csv       <- here::here(dir_census, "sao_paulo_2010",
                               "census_sp_collapsed_2010.csv")

# Read census microdata
micro_bogota   <- data.table::fread(micro_bogota_csv)
micro_cdmx     <- data.table::fread(micro_cdmx_csv)
micro_santiago <- data.table::fread(micro_santiago_csv)
micro_sp       <- data.table::fread(micro_sp_csv)

# Read collapsed census data
geo_bogota   <- data.table::fread(geo_bogota_csv)
geo_cdmx     <- data.table::fread(geo_cdmx_csv)
geo_santiago <- data.table::fread(geo_santiago_csv)
geo_sp       <- data.table::fread(geo_sp_csv)

# ============================================================================================
# II: Process and save
# ============================================================================================
# Create output folder if needed
dir.create(outdir_exp, recursive = TRUE, showWarnings = FALSE)

# Define IDW specifications
buffers_km     <- c(3, 5)
distance_power <- 2

# Run all cities for the baseline and robustness buffers
for (buffer in buffers_km) {
  # 1. Bogota
  res_bogota <- run_idw_city(
    city_label     = "Bogota",
    city_id        = "bogota_2018",
    arrow_dir      = arrow_bogota,
    geo_sta_pq     = dist_bogota,
    geo_census     = geo_bogota,
    micro_census   = micro_bogota,
    geo_id_col     = "GEO_ID",
    geo_pop_col    = "weight",
    geo_edu_col    = "education_mean",
    micro_id_col   = "GEO_ID",
    micro_pop_col  = "fe",
    micro_edu_col  = "escolaridad",
    buffer_km      = buffer,
    outdir_exp     = outdir_exp,
    distance_power = distance_power)

  # # 2. CDMX
  # res_cdmx <- run_idw_city(
  #   city_label     = "CDMX",
  #   city_id        = "cdmx_2020",
  #   arrow_dir      = arrow_cdmx,
  #   geo_sta_pq     = dist_cdmx,
  #   geo_census     = geo_cdmx,
  #   micro_census   = micro_cdmx,
  #   geo_id_col     = "CVE_MUN",
  #   geo_pop_col    = "weight",
  #   geo_edu_col    = "education_mean",
  #   micro_id_col   = "CVE_MUN",
  #   micro_pop_col  = "FACTOR",
  #   micro_edu_col  = "escolaridad",
  #   buffer_km      = buffer,
  #   outdir_exp     = outdir_exp,
  #   distance_power = distance_power)
  # 
  # # 3. Santiago
  # res_santiago <- run_idw_city(
  #   city_label     = "Santiago",
  #   city_id        = "santiago_2024",
  #   arrow_dir      = arrow_santiago,
  #   geo_sta_pq     = dist_santiago,
  #   geo_census     = geo_santiago,
  #   micro_census   = micro_santiago,
  #   geo_id_col     = "CUT",
  #   geo_pop_col    = "weight",
  #   geo_edu_col    = "education_mean",
  #   micro_id_col   = "comuna",
  #   micro_pop_col  = "fe",
  #   micro_edu_col  = "educ_years",
  #   buffer_km      = buffer,
  #   outdir_exp     = outdir_exp,
  #   distance_power = distance_power)
  # 
  # # 4. Sao Paulo
  # res_sp <- run_idw_city(
  #   city_label     = "Sao Paulo",
  #   city_id        = "sao_paulo_2010",
  #   arrow_dir      = arrow_sp,
  #   geo_sta_pq     = dist_sp,
  #   geo_census     = geo_sp,
  #   micro_census   = micro_sp,
  #   geo_id_col     = "code_weighting",
  #   geo_pop_col    = "weight",
  #   geo_edu_col    = "education_mean",
  #   micro_id_col   = "code_weighting",
  #   micro_pop_col  = "weight",
  #   micro_edu_col  = "years_schooling",
  #   buffer_km      = buffer,
  #   outdir_exp     = outdir_exp,
  #   distance_power = distance_power)
}

cat("Script from the IDB project executed successfully in the Docker container!\n")