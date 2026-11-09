# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Flag anomalous hourly pollution readings for each city.
#
#' @Description: Read partitioned station data and the station distance matrices.
# Screen out impossible readings, then compare each high reading with its own adjacent
# hours and its nearest station. The function writes a complete cleaned dataset and
# returns its directory; years remain together. The method, worked by hand, is in
# doc/reference/outlier_cleaning.md.
#
#' @Summary:
#   I.   Import data: source functions and declare input and output paths.
#   II.  Process and save data: clean each city, then count and list the removals.
#   III. Save quality summaries: write the counts and screened records as CSV.
#
#' @Date: September 2026
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
# Load the functions and their required packages.
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "outliers.R"))
source(here::here("src", "general_utilities", "process", "pollution_quality.R"))
source(here::here("config", "analysis_settings.R"))

# Set the source folders and the output folder.
dir_pollution     <- here::here("data", "interim", "monitoring_stations")
dir_distances     <- here::here("data", "processed", "distances_matrices")
dir_outliers      <- here::here("data", "processed", "monitoring_stations_outliers")
dir_quality       <- here::here("data", "processed", "pollution_quality")

# Set paths to each complete station dataset and its station-distance matrix.
pollution_bogota   <- here::here(dir_pollution, "bogota_metro_dataset")
distances_bogota   <- here::here(dir_distances, "bogota_2018",
                                 "matrix_station_distances.parquet")

pollution_cdmx     <- here::here(dir_pollution, "cdmx_metro_dataset")
distances_cdmx     <- here::here(dir_distances, "cdmx_2020",
                                 "matrix_station_distances.parquet")

pollution_santiago <- here::here(dir_pollution, "santiago_metro_dataset")
distances_santiago <- here::here(dir_distances, "santiago_2017",
                                 "matrix_station_distances.parquet")

pollution_sao_paulo <- here::here(dir_pollution, "sao_paulo_metro_dataset")
distances_sao_paulo <- here::here(dir_distances, "sao_paulo_2010",
                                  "matrix_station_distances.parquet")

# Optional: Check the distances and pollution datasets (change city if you want)
pollution_data <- arrow::open_dataset(pollution_bogota) |> dplyr::collect()
distances_data <- arrow::open_dataset(distances_bogota) |> dplyr::collect()

# ==========================================================================================
# II: Process and save data
# ==========================================================================================
# Flag outliers and save the cleaned, year-partitioned dataset for Bogotá
cleaned_bogota <- detect_pollution_outliers(arrow_dir = pollution_bogota,
    station_dist_path    = distances_bogota,
    upper_bounds         = pollution_upper_bounds,
    pct_flag             = outlier_quantile,
    n_sd                 = outlier_sd_band,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    neighbor_eligibility = outlier_neighbor_eligibility,
    out_dir              = dir_outliers,
    out_name             = "bogota_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for CDMX
cleaned_cdmx <- detect_pollution_outliers(arrow_dir = pollution_cdmx,
    station_dist_path    = distances_cdmx,
    upper_bounds         = pollution_upper_bounds,
    pct_flag             = outlier_quantile,
    n_sd                 = outlier_sd_band,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    neighbor_eligibility = outlier_neighbor_eligibility,
    out_dir              = dir_outliers,
    out_name             = "cdmx_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for Santiago
cleaned_santiago <- detect_pollution_outliers(arrow_dir = pollution_santiago,
    station_dist_path    = distances_santiago,
    upper_bounds         = pollution_upper_bounds,
    pct_flag             = outlier_quantile,
    n_sd                 = outlier_sd_band,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    neighbor_eligibility = outlier_neighbor_eligibility,
    out_dir              = dir_outliers,
    out_name             = "santiago_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for São Paulo
cleaned_sao_paulo <- detect_pollution_outliers(arrow_dir = pollution_sao_paulo,
    station_dist_path    = distances_sao_paulo,
    upper_bounds         = pollution_upper_bounds,
    pct_flag             = outlier_quantile,
    n_sd                 = outlier_sd_band,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    neighbor_eligibility = outlier_neighbor_eligibility,
    out_dir              = dir_outliers,
    out_name             = "sao_paulo_metro",
    overwrite            = TRUE)

# Count hours by source validation and the two cleaning reasons.
# screen_reason:  0 within bounds, 1 above the bound, 2 negative, 3 missing.
# outlier_reason: NA not assessed, 0 retained, 1-4 removed (data dictionary, section 9).
quality_bogota    <- summarize_pollution_quality(cleaned_bogota)
quality_cdmx      <- summarize_pollution_quality(cleaned_cdmx)
quality_santiago  <- summarize_pollution_quality(cleaned_santiago)
quality_sao_paulo <- summarize_pollution_quality(cleaned_sao_paulo)

# List the readings the screen withheld as above the bound or negative (codes 1 and 2).
screening_records_bogota    <- collect_pollution_screening_records(cleaned_bogota)
screening_records_cdmx      <- collect_pollution_screening_records(cleaned_cdmx)
screening_records_santiago  <- collect_pollution_screening_records(cleaned_santiago)
screening_records_sao_paulo <- collect_pollution_screening_records(cleaned_sao_paulo)

# ==========================================================================================
# III: Save quality summaries
# ==========================================================================================
# Save the hour counts.
quality_bogota_file <- write_pollution_quality_summary(quality_bogota,
  here::here(dir_quality, "bogota_quality_summary.csv"))
quality_cdmx_file <- write_pollution_quality_summary(quality_cdmx,
  here::here(dir_quality, "cdmx_quality_summary.csv"))
quality_santiago_file <- write_pollution_quality_summary(quality_santiago,
  here::here(dir_quality, "santiago_quality_summary.csv"))
quality_sao_paulo_file <- write_pollution_quality_summary(quality_sao_paulo,
  here::here(dir_quality, "sao_paulo_quality_summary.csv"))

# Save the screened records.
screening_records_bogota_file <- write_pollution_quality_summary(screening_records_bogota,
  here::here(dir_quality, "bogota_review_records.csv"))
screening_records_cdmx_file <- write_pollution_quality_summary(screening_records_cdmx,
  here::here(dir_quality, "cdmx_review_records.csv"))
screening_records_santiago_file <- write_pollution_quality_summary(
  screening_records_santiago,
  here::here(dir_quality, "santiago_review_records.csv"))
screening_records_sao_paulo_file <- write_pollution_quality_summary(
  screening_records_sao_paulo,
  here::here(dir_quality, "sao_paulo_review_records.csv"))
