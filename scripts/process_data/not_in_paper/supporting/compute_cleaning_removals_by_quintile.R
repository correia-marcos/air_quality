# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Measure the share of reported readings removed by cleaning, by education quintile.
#
#' @Description: Read the cleaned station panels, the distance matrices and the individual
# census files. For each city, count the reported PM readings that the gross-value screen
# and the outlier procedure set to missing, grouped by education quintile with the method
# of the availability table (missing_quintile_method). A supplement to that table: it
# asks whether cleaning, rather than reporting, removes more readings in some quintiles.
# Save the stacked table to data/processed/missing_proportions/.
#
#' @Summary:
#   I.   Import data: source functions and settings, then set input and output paths.
#   II.  Process data: compute each city's shares and stack them.
#   III. Save outputs: write the table as Parquet with a CSV copy.
#
#' @Date: November 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the counting and grouping functions, plus their shared helpers
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "diagnostics.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))
# assign_socio_group() cuts education quintiles for the "idw" grouping by quintile
source(here::here("src", "general_utilities", "process", "idw_exposure.R"))

# The shared settings define the year, pollutants and grouping method
source(here::here("config", "analysis_settings.R"))

# Set the cleaned-panel, distance-matrix and census folders, and the output folder
dir_clean  <- here::here("data", "processed", "monitoring_stations_outliers")
dir_dist   <- here::here("data", "processed", "distances_matrices")
dir_census <- here::here("data", "interim", "census")
outdir     <- here::here("data", "processed", "missing_proportions")

# Set each city's cleaned panel, distances and individual census
clean_bogota   <- here::here(dir_clean, "bogota_metro_clean")
clean_cdmx     <- here::here(dir_clean, "cdmx_metro_clean")
clean_santiago <- here::here(dir_clean, "santiago_metro_clean")
clean_sp       <- here::here(dir_clean, "sao_paulo_metro_clean")

dist_bogota   <- here::here(dir_dist, "bogota_2018", "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_dist, "cdmx_2020", "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_dist, "santiago_2017",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_dist, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")

micro_bogota   <- here::here(dir_census, "bogota_2018",
                             "census_2018_metro_individual.parquet")
micro_cdmx     <- here::here(dir_census, "cdmx_extended_2020",
                             "census_metro_individual_2020.parquet")
micro_santiago <- here::here(dir_census, "santiago_2017",
                             "census_individual_2017.parquet")
micro_sp       <- here::here(dir_census, "sao_paulo_2010",
                             "census_sp_individual_2010.parquet")

# ========================================================================================
# II: Process data
# ========================================================================================
# Share of reported readings removed by cleaning, by education quintile
removals_bogota <- compute_cleaning_removals_by_quintile(
  city           = "Bogota",
  city_order     = 1L,
  pollution_dir  = clean_bogota,
  dist_pq        = dist_bogota,
  census_file    = micro_bogota,
  geo_id_col     = "geo_id",
  pollutants     = summary_pollutants,
  year           = analysis_year,
  method         = missing_quintile_method,
  buffer_km      = missing_quintile_buffer_km,
  distance_power = idw_distance_power)

removals_cdmx <- compute_cleaning_removals_by_quintile(
  city           = "Mexico City",
  city_order     = 2L,
  pollution_dir  = clean_cdmx,
  dist_pq        = dist_cdmx,
  census_file    = micro_cdmx,
  geo_id_col     = "geo_id",
  pollutants     = summary_pollutants,
  year           = analysis_year,
  method         = missing_quintile_method,
  buffer_km      = missing_quintile_buffer_km,
  distance_power = idw_distance_power)

removals_santiago <- compute_cleaning_removals_by_quintile(
  city           = "Santiago",
  city_order     = 3L,
  pollution_dir  = clean_santiago,
  dist_pq        = dist_santiago,
  census_file    = micro_santiago,
  geo_id_col     = "geo_id",
  pollutants     = summary_pollutants,
  year           = analysis_year,
  method         = missing_quintile_method,
  buffer_km      = missing_quintile_buffer_km,
  distance_power = idw_distance_power)

removals_sp <- compute_cleaning_removals_by_quintile(
  city           = "Sao Paulo",
  city_order     = 4L,
  pollution_dir  = clean_sp,
  dist_pq        = dist_sp,
  census_file    = micro_sp,
  geo_id_col     = "geo_id",
  pollutants     = summary_pollutants,
  year           = analysis_year,
  method         = missing_quintile_method,
  buffer_km      = missing_quintile_buffer_km,
  distance_power = idw_distance_power)

# Stack the four cities in table order
removals_by_quintile <- data.table::rbindlist(list(removals_bogota, removals_cdmx,
                                                   removals_santiago, removals_sp))

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the stacked table for render_cleaning_removals_table.R
removals_files <- save_table_parquet_csv(
  dt      = removals_by_quintile,
  out_dir = outdir,
  name    = paste0("cleaning_removals_by_education_quintile_", analysis_year))
