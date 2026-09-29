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
source(here::here("config", "analysis_settings.R"))

# Set the source folders and the output folder.
dir_pollution <- here::here("data", "interim", "monitoring_stations")
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_outliers  <- here::here("data", "processed", "monitoring_stations_outliers")

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
    station_dist_path    = distances_bogota,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "bogota_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for CDMX
cleaned_cdmx <- detect_pollution_outliers(arrow_dir = pollution_cdmx,
    station_dist_path    = distances_cdmx,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "cdmx_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for Santiago
cleaned_santiago <- detect_pollution_outliers(arrow_dir = pollution_santiago,
    station_dist_path    = distances_santiago,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "santiago_metro",
    overwrite            = TRUE)

# Flag outliers and save the cleaned, year-partitioned dataset for São Paulo
cleaned_sao_paulo <- detect_pollution_outliers(arrow_dir = pollution_sao_paulo,
    station_dist_path    = distances_sao_paulo,
    on_missing_temporal  = outlier_missing_temporal,
    on_missing_neighbor  = outlier_missing_neighbor,
    out_dir              = dir_outliers,
    out_name             = "sao_paulo_metro",
    overwrite            = TRUE)
