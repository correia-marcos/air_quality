# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Flag anomalous hourly pollution readings for each city.
#
#' @Description: Read partitioned station data and the station distance matrices.
# Compare each reading with its temporal and spatial neighbors. The function writes
# a complete cleaned dataset and returns its directory; years remain together.
#
#' @Summary:
#   I.  Import data: source functions and declare input and output paths.
#   II. Process and save data: retain the cleaned dataset paths for each city.
#   III. Save quality summaries: named station-year counts remain inspectable.
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
reviews_bogota    <- read_pollution_quality_reviews(pollution_review_path, "bogota")
reviews_cdmx      <- read_pollution_quality_reviews(pollution_review_path, "cdmx")
reviews_santiago  <- read_pollution_quality_reviews(pollution_review_path, "santiago")
reviews_sao_paulo <- read_pollution_quality_reviews(pollution_review_path, "sao_paulo")

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

# ==========================================================================================
# II: Process and save data
# ==========================================================================================
# Flag outliers and save the cleaned, year-partitioned dataset for Bogotá
cleaned_bogota <- detect_pollution_outliers(arrow_dir = pollution_bogota,
    upper_bounds         = pollution_upper_bounds,
    eligibility_cols     = pollution_eligibility_cols,
    review_decisions     = reviews_bogota,
    station_dist_path    = distances_bogota,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "bogota_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for CDMX
cleaned_cdmx <- detect_pollution_outliers(arrow_dir = pollution_cdmx,
    upper_bounds         = pollution_upper_bounds,
    eligibility_cols     = pollution_eligibility_cols,
    review_decisions     = reviews_cdmx,
    station_dist_path    = distances_cdmx,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "cdmx_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for Santiago
cleaned_santiago <- detect_pollution_outliers(arrow_dir = pollution_santiago,
    upper_bounds         = pollution_upper_bounds,
    eligibility_cols     = pollution_eligibility_cols,
    review_decisions     = reviews_santiago,
    station_dist_path    = distances_santiago,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "santiago_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for São Paulo
cleaned_sao_paulo <- detect_pollution_outliers(arrow_dir = pollution_sao_paulo,
    upper_bounds         = pollution_upper_bounds,
    eligibility_cols     = pollution_eligibility_cols,
    review_decisions     = reviews_sao_paulo,
    station_dist_path    = distances_sao_paulo,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "sao_paulo_metro",
    overwrite            = TRUE)

# Named count tables expose source status, quality holds and statistical removals.
quality_bogota    <- summarize_pollution_quality(cleaned_bogota)
quality_cdmx      <- summarize_pollution_quality(cleaned_cdmx)
quality_santiago  <- summarize_pollution_quality(cleaned_santiago)
quality_sao_paulo <- summarize_pollution_quality(cleaned_sao_paulo)

review_records_bogota    <- collect_pollution_review_records(cleaned_bogota)
review_records_cdmx      <- collect_pollution_review_records(cleaned_cdmx)
review_records_santiago  <- collect_pollution_review_records(cleaned_santiago)
review_records_sao_paulo <- collect_pollution_review_records(cleaned_sao_paulo)

# ==========================================================================================
# III: Save quality summaries
# ==========================================================================================
quality_bogota_file <- write_pollution_quality_summary(quality_bogota,
  here::here(dir_quality, "bogota_quality_summary.csv"))
quality_cdmx_file <- write_pollution_quality_summary(quality_cdmx,
  here::here(dir_quality, "cdmx_quality_summary.csv"))
quality_santiago_file <- write_pollution_quality_summary(quality_santiago,
  here::here(dir_quality, "santiago_quality_summary.csv"))
quality_sao_paulo_file <- write_pollution_quality_summary(quality_sao_paulo,
  here::here(dir_quality, "sao_paulo_quality_summary.csv"))

review_records_bogota_file <- write_pollution_quality_summary(review_records_bogota,
  here::here(dir_quality, "bogota_review_records.csv"))
review_records_cdmx_file <- write_pollution_quality_summary(review_records_cdmx,
  here::here(dir_quality, "cdmx_review_records.csv"))
review_records_santiago_file <- write_pollution_quality_summary(review_records_santiago,
  here::here(dir_quality, "santiago_review_records.csv"))
review_records_sao_paulo_file <- write_pollution_quality_summary(review_records_sao_paulo,
  here::here(dir_quality, "sao_paulo_review_records.csv"))
