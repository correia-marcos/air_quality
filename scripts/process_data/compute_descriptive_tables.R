# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Compute every descriptive statistic the paper reports about monitoring coverage,
#   data availability and the census populations behind the exposure estimates.
#
#' @Description: One script for the paper's descriptive layer, because all five families
# answer the same question — how much do we actually observe, and for whom. Each family
# reads the hourly Arrow panels or the census and writes a machine-readable artefact to
# data/processed/. No LaTeX is written here: the render_*_tables.R scripts turn these
# Parquet files into the .tex files, which keeps this script on the data side of the
# data/ -> results/ ratchet. Santiago uses the 2017 zonas censales throughout, matching
# the main exposure specification; the 2024 communes are a robustness vintage only.
#
#' @Summary:
#   I.   Import data: paths and analysis options, one variable per city.
#   II.  Missing proportions by station, month, hour and day of week (raw and cleaned).
#   III. Station counts by pollutant.
#   IV.  WHO exceedance factors.
#   IVb. Days and hours above the WHO interim targets, city-hour and station-hour.
#   V.   Data availability by education quintile.
#   VI.  Census summary.
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
dir_raw     <- here::here("data", "interim", "monitoring_stations")
dir_clean   <- here::here("data", "processed", "monitoring_stations_outliers")
dir_dist    <- here::here("data", "processed", "distances_matrices")
dir_census  <- here::here("data", "interim", "census")

outdir_missing  <- here::here("data", "processed", "missing_proportions")
outdir_counts   <- here::here("data", "processed", "station_counts")
outdir_who      <- here::here("data", "processed", "who_exceedances")
outdir_census   <- here::here("data", "processed", "census_summary")
outdir_exceed   <- here::here("data", "processed", "threshold_exceedances")

# Analysis options. "available" reports non-missing shares, "missing" the complement.
analysis_year <- 2023L
pollutants    <- c("pm10", "pm25")
report        <- "available"
missing_dims  <- c("station", "month", "hour", "day_of_week")

# Define raw and cleaned Arrow dataset paths
raw_bogota   <- here::here(dir_raw, "bogota_metro_dataset")
raw_cdmx     <- here::here(dir_raw, "cdmx_metro_dataset")
raw_santiago <- here::here(dir_raw, "santiago_metro_dataset")
raw_sp       <- here::here(dir_raw, "sao_paulo_metro_dataset")

clean_bogota   <- here::here(dir_clean, "bogota_metro_clean")
clean_cdmx     <- here::here(dir_clean, "cdmx_metro_clean")
clean_santiago <- here::here(dir_clean, "santiago_metro_clean")
clean_sp       <- here::here(dir_clean, "sao_paulo_metro_clean")

# Define geo-to-station distance matrix paths
dist_bogota   <- here::here(dir_dist, "bogota_2018",
                            "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_dist, "cdmx_2020",
                            "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_dist, "santiago_2017",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_dist, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")

# Define individual census paths
micro_bogota   <- here::here(dir_census, "bogota_2018",
                             "census_2018_metro_individual.parquet")
micro_cdmx     <- here::here(dir_census, "cdmx_extended_2020",
                             "census_metro_individual_2020.parquet")
micro_santiago <- here::here(dir_census, "santiago_2017",
                             "census_individual_2017.parquet")
micro_sp       <- here::here(dir_census, "sao_paulo_2010",
                             "census_sp_individual_2010.parquet")

# ============================================================================================
# II: Missing proportions by station, month, hour and day of week
# ============================================================================================
dir.create(outdir_missing, recursive = TRUE, showWarnings = FALSE)

# Raw panels give structural missingness: hours the network never reported.
compute_missing_proportions(
  arrow_dir   = raw_bogota,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "bogota_raw")

compute_missing_proportions(
  arrow_dir   = raw_cdmx,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "cdmx_raw")

compute_missing_proportions(
  arrow_dir   = raw_santiago,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "santiago_raw")

compute_missing_proportions(
  arrow_dir   = raw_sp,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "sao_paulo_metro_raw")

# Cleaned panels add what detect_outliers.R removed; the paper reports the raw panel.
compute_missing_proportions(
  arrow_dir   = clean_bogota,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "bogota_clean")

compute_missing_proportions(
  arrow_dir   = clean_cdmx,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "cdmx_clean")

compute_missing_proportions(
  arrow_dir   = clean_santiago,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "santiago_clean")

compute_missing_proportions(
  arrow_dir   = clean_sp,
  pollutants  = pollutants,
  dims        = missing_dims,
  year_filter = analysis_year,
  out_dir     = outdir_missing,
  out_name    = "sao_paulo_metro_clean")

# ============================================================================================
# III: Station counts by pollutant
# ============================================================================================
dir.create(outdir_counts, recursive = TRUE, showWarnings = FALSE)

# Counted on the raw panels: this describes the monitoring infrastructure that exists, not
# the subset that survives outlier removal.
counts_bogota <- count_stations_reporting(
  arrow_dir   = raw_bogota,
  pollutants  = pollutants,
  year_filter = analysis_year,
  mem_gb      = 8)

counts_cdmx <- count_stations_reporting(
  arrow_dir   = raw_cdmx,
  pollutants  = pollutants,
  year_filter = analysis_year,
  mem_gb      = 8)

counts_santiago <- count_stations_reporting(
  arrow_dir   = raw_santiago,
  pollutants  = pollutants,
  year_filter = analysis_year,
  mem_gb      = 8)

counts_sp <- count_stations_reporting(
  arrow_dir   = raw_sp,
  pollutants  = pollutants,
  year_filter = analysis_year,
  mem_gb      = 8)

# Presentation order and accented names for the paper's station-count table.
counts_santiago[, city := "Santiago"]
counts_bogota[,   city := "Bogotá"]
counts_cdmx[,     city := "Mexico City"]
counts_sp[,       city := "São Paulo"]

station_counts <- data.table::rbindlist(
  list(counts_santiago, counts_bogota, counts_cdmx, counts_sp))
station_counts <- station_counts[, .(city, pm10, pm25)]

save_table_parquet_csv(station_counts, outdir_counts,
                       paste0("stations_by_pollutant_", analysis_year))

# ============================================================================================
# IV: WHO exceedance factors
# ============================================================================================
dir.create(outdir_who, recursive = TRUE, showWarnings = FALSE)

# Mean-of-means across stations, not a pooled grand mean: pooling would let the stations
# with the most uptime dominate the city average. All years, not just analysis_year.
who_bogota <- compute_who_exceedances(
  arrow_dir   = clean_bogota,
  city_label  = "bogota",
  pollutants  = pollutants,
  year_filter = NULL)

who_cdmx <- compute_who_exceedances(
  arrow_dir   = clean_cdmx,
  city_label  = "cdmx",
  pollutants  = pollutants,
  year_filter = NULL)

who_santiago <- compute_who_exceedances(
  arrow_dir   = clean_santiago,
  city_label  = "santiago",
  pollutants  = pollutants,
  year_filter = NULL)

who_sp <- compute_who_exceedances(
  arrow_dir   = clean_sp,
  city_label  = "sao_paulo_metro",
  pollutants  = pollutants,
  year_filter = NULL)

who_exceedances <- data.table::rbindlist(
  list(who_bogota, who_cdmx, who_santiago, who_sp), fill = TRUE)

save_raw_data_tidy_formatted(
  data          = who_exceedances,
  out_dir       = outdir_who,
  out_name      = "who_exceedances_all_cities",
  write_rds     = FALSE,
  write_parquet = TRUE,
  write_csv_gz  = FALSE)

# ============================================================================================
# IVb: Days and hours above the WHO interim targets
# ============================================================================================
dir.create(outdir_exceed, recursive = TRUE, showWarnings = FALSE)

# Two series per city: the metro average hour by hour, and the individual stations. The
# gap between them is how much a city-wide average hides a local spike.
exceed_bogota <- compute_threshold_exceedance_days(
  arrow_dir = clean_bogota, city_label = "Bogota", year_filter = analysis_year,
  pollutants = pollutants)

exceed_santiago <- compute_threshold_exceedance_days(
  arrow_dir = clean_santiago, city_label = "Santiago", year_filter = analysis_year,
  pollutants = pollutants)

exceed_cdmx <- compute_threshold_exceedance_days(
  arrow_dir = clean_cdmx, city_label = "Mexico City", year_filter = analysis_year,
  pollutants = pollutants)

exceed_sp <- compute_threshold_exceedance_days(
  arrow_dir = clean_sp, city_label = "Sao Paulo", year_filter = analysis_year,
  pollutants = pollutants)

threshold_exceedances <- data.table::rbindlist(
  list(exceed_bogota, exceed_santiago, exceed_cdmx, exceed_sp))

save_table_parquet_csv(threshold_exceedances, outdir_exceed,
                       paste0("days_and_hours_", analysis_year))

# ============================================================================================
# V: Data availability by education quintile
# ============================================================================================
# Assigns each station the education quintile of its nearest census unit, then reports the
# share of non-missing hours by quintile. A smaller share in the lower quintiles means the
# exposure estimates are least reliable exactly where the paper's question bites.
quintile_bogota <- compute_missing_by_quintile(
  city          = "Bogota",
  city_order    = 1L,
  pollution_dir = clean_bogota,
  dist_pq       = dist_bogota,
  census_file   = micro_bogota,
  geo_id_col    = "geo_id",
  pollutants    = pollutants,
  year          = analysis_year,
  report        = report)

quintile_cdmx <- compute_missing_by_quintile(
  city          = "Mexico City",
  city_order    = 2L,
  pollution_dir = clean_cdmx,
  dist_pq       = dist_cdmx,
  census_file   = micro_cdmx,
  geo_id_col    = "geo_id",
  pollutants    = pollutants,
  year          = analysis_year,
  report        = report)

quintile_santiago <- compute_missing_by_quintile(
  city          = "Santiago",
  city_order    = 3L,
  pollution_dir = clean_santiago,
  dist_pq       = dist_santiago,
  census_file   = micro_santiago,
  geo_id_col    = "geo_id",
  pollutants    = pollutants,
  year          = analysis_year,
  report        = report)

quintile_sp <- compute_missing_by_quintile(
  city          = "Sao Paulo",
  city_order    = 4L,
  pollution_dir = clean_sp,
  dist_pq       = dist_sp,
  census_file   = micro_sp,
  geo_id_col    = "geo_id",
  pollutants    = pollutants,
  year          = analysis_year,
  report        = report)

missing_by_quintile <- data.table::rbindlist(
  list(quintile_bogota, quintile_cdmx, quintile_santiago, quintile_sp))

save_table_parquet_csv(missing_by_quintile, outdir_missing,
                       paste0("missing_by_education_quintile_", analysis_year))

# ============================================================================================
# VI: Census summary
# ============================================================================================
dir.create(outdir_census, recursive = TRUE, showWarnings = FALSE)

# Population totals and geographic-unit counts behind the exposure estimates. Units with a
# missing id or a non-positive weight are dropped, so the count is the estimation-relevant
# one rather than the file's row count.
summary_bogota <- compute_city_census_summary(
  census_path  = micro_bogota,
  city         = "Bogota",
  city_latex   = "Bogot\\'a",
  census_year  = 2018L,
  census_level = "Census tract",
  geo_id_col   = "geo_id",
  pop_col      = "person_weight")

summary_cdmx <- compute_city_census_summary(
  census_path  = micro_cdmx,
  city         = "Mexico City",
  city_latex   = "Mexico City",
  census_year  = 2020L,
  census_level = "Municipality",
  geo_id_col   = "geo_id",
  pop_col      = "person_weight")

summary_santiago <- compute_city_census_summary(
  census_path  = micro_santiago,
  city         = "Gran Santiago",
  city_latex   = "Gran Santiago",
  census_year  = 2017L,
  census_level = "Census tract",
  geo_id_col   = "geo_id",
  pop_col      = "person_weight")

summary_sp <- compute_city_census_summary(
  census_path  = micro_sp,
  city         = "Sao Paulo",
  city_latex   = "S\\~ao Paulo",
  census_year  = 2010L,
  census_level = "Weighting area",
  geo_id_col   = "geo_id",
  pop_col      = "person_weight")

census_summary <- data.table::rbindlist(
  list(summary_bogota, summary_cdmx, summary_santiago, summary_sp))

save_table_parquet_csv(census_summary, outdir_census, "census_summary")

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
