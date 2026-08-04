# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute every descriptive statistic the paper reports about monitoring coverage,
#   data availability and the census populations behind the exposure estimates.
#
# @Description: One script for the paper's descriptive layer, because all five families
# answer the same question — how much do we actually observe, and for whom. Each family
# reads the hourly Arrow panels or the collapsed census and writes a machine-readable
# artefact to data/processed/. No LaTeX is written here: render_paper_tables.R turns these
# Parquet files into the .tex files, which keeps this script on the data side of the
# data/ -> results/ ratchet.
#
# @Summary:
#   I.   Import data: paths, analysis options and one city specification table.
#   II.  Missing proportions by station, month, hour and day of week (raw and cleaned).
#   III. Station counts by pollutant.
#   IV.  WHO exceedance factors.
#   V.   Data availability by education quintile.
#   VI.  Census summary.
#
# @Date: August 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_raw     <- here::here("data", "raw", "monitoring_stations")
dir_clean   <- here::here("data", "processed", "monitoring_stations_outliers")
dir_dist    <- here::here("data", "processed", "distances_matrices")
dir_census  <- here::here("data", "interim", "census")

outdir_missing  <- here::here("data", "processed", "missing_proportions")
outdir_counts   <- here::here("data", "processed", "station_counts")
outdir_who      <- here::here("data", "processed", "who_exceedances")
outdir_census   <- here::here("data", "processed", "census_summary")

# Analysis options. "available" reports non-missing shares, "missing" the complement.
analysis_year <- 2023L
pollutants    <- c("pm10", "pm25")
report        <- "available"
missing_dims  <- c("station", "month", "hour", "day_of_week")

# One row per city. This is the shared infrastructure the five families all need; display
# labels stay next to the section that uses them, because the paper genuinely names the
# cities differently from table to table.
#
# Two geographic id columns, not one: Santiago's individual census carries `comuna`, which
# the collapse step renames to `CUT`. The Santiago rows use the 2024 commune-level census
# here, matching the distance matrix this diagnostic needs, not the 2017 zonas the main
# exposure specification uses.
city_specs <- data.table::data.table(
  city_id      = c("bogota", "cdmx", "santiago", "sao_paulo_metro"),
  census_id    = c("bogota_2018", "cdmx_extended_2020", "santiago_2024",
                   "sao_paulo_2010"),
  dist_id      = c("bogota_2018", "cdmx_2020", "santiago_2024", "sao_paulo_2010"),
  arrow_raw    = file.path(dir_raw,
                           c("bogota_metro_dataset", "cdmx_metro_dataset",
                             "santiago_metro_dataset", "sao_paulo_metro_dataset")),
  arrow_clean  = file.path(dir_clean,
                           c("bogota_metro_clean", "cdmx_metro_clean",
                             "santiago_metro_clean", "sao_paulo_metro_clean")),
  micro_id_col = c("GEO_ID", "CVE_MUN", "comuna", "code_weighting"),
  geo_id_col   = c("GEO_ID", "CVE_MUN", "CUT", "code_weighting"),
  pop_col      = c("weight", "weight", "weight", "weight"),
  census_year  = c(2018L, 2020L, 2024L, 2010L),
  census_level = c("Census tract", "Municipality", "Census tract", "Weighting area")
)

city_specs[, dist_pq := file.path(dir_dist, dist_id,
                                  "matrix_geo_station_distances.parquet")]

city_specs[, micro_census := file.path(
  dir_census, census_id,
  c("census_2018_metro_individual.parquet", "census_metro_individual_2020.parquet",
    "census_santiago_individual_2024.parquet", "census_sp_individual_2010.parquet"))]

city_specs[, geo_census := file.path(
  dir_census, census_id,
  c("census_2018_metro_collapsed.parquet", "collapse_metro_area_2020.parquet",
    "census_santiago_collapsed_2024.parquet", "census_sp_collapsed_2010.parquet"))]

# ============================================================================================
# II: Missing proportions by station, month, hour and day of week
# ============================================================================================
dir.create(outdir_missing, recursive = TRUE, showWarnings = FALSE)

# Raw panels give structural missingness: hours the network never reported.
for (i in seq_len(nrow(city_specs))) {
  compute_missing_proportions(
    arrow_dir   = city_specs$arrow_raw[i],
    pollutants  = pollutants,
    dims        = missing_dims,
    year_filter = analysis_year,
    out_dir     = outdir_missing,
    out_name    = paste0(city_specs$city_id[i], "_raw"))
}

# Cleaned panels give algorithmic missingness: what detect_outliers.R also removed.
for (i in seq_len(nrow(city_specs))) {
  compute_missing_proportions(
    arrow_dir   = city_specs$arrow_clean[i],
    pollutants  = pollutants,
    dims        = missing_dims,
    year_filter = analysis_year,
    out_dir     = outdir_missing,
    out_name    = paste0(city_specs$city_id[i], "_clean"))
}

# ============================================================================================
# III: Station counts by pollutant
# ============================================================================================
dir.create(outdir_counts, recursive = TRUE, showWarnings = FALSE)

# Counted on the raw panels: this describes the monitoring infrastructure that exists, not
# the subset that survives outlier removal.
station_counts <- data.table::rbindlist(lapply(
  seq_len(nrow(city_specs)),
  function(i) {
    dt <- count_stations_reporting(
      arrow_dir   = city_specs$arrow_raw[i],
      pollutants  = pollutants,
      year_filter = analysis_year,
      mem_gb      = 8)
    dt[, city_id := city_specs$city_id[i]]
    dt
  }
))

# Presentation order and accented names for the paper's station-count table.
count_labels <- data.table::data.table(
  city_id = c("santiago", "bogota", "cdmx", "sao_paulo_metro"),
  city    = c("Santiago", "Bogotá", "Mexico City", "São Paulo"))

station_counts <- merge(count_labels, station_counts, by = "city_id", all.x = TRUE)
station_counts[, city_id := factor(city_id, levels = count_labels$city_id)]
data.table::setorder(station_counts, city_id)
station_counts <- station_counts[, .(city, pm10, pm25)]

save_table_parquet_csv(station_counts, outdir_counts,
                       paste0("stations_by_pollutant_", analysis_year))

# ============================================================================================
# IV: WHO exceedance factors
# ============================================================================================
dir.create(outdir_who, recursive = TRUE, showWarnings = FALSE)

# Mean-of-means across stations, not a pooled grand mean: pooling would let the stations
# with the most uptime dominate the city average. All years, not just analysis_year.
who_exceedances <- data.table::rbindlist(lapply(
  seq_len(nrow(city_specs)),
  function(i) {
    compute_who_exceedances(
      arrow_dir   = city_specs$arrow_clean[i],
      city_label  = city_specs$city_id[i],
      pollutants  = pollutants,
      year_filter = NULL)
  }
), fill = TRUE)

save_raw_data_tidy_formatted(
  data          = who_exceedances,
  out_dir       = outdir_who,
  out_name      = "who_exceedances_all_cities",
  write_rds     = FALSE,
  write_parquet = TRUE,
  write_csv_gz  = FALSE)

# ============================================================================================
# V: Data availability by education quintile
# ============================================================================================
# Assigns each station the education quintile of its nearest census unit, then reports the
# share of non-missing hours by quintile. A smaller share in the lower quintiles means the
# exposure estimates are least reliable exactly where the paper's question bites.
quintile_labels <- c("Bogota", "Mexico City", "Santiago", "Sao Paulo")

missing_by_quintile <- data.table::rbindlist(lapply(
  seq_len(nrow(city_specs)),
  function(i) {
    compute_missing_by_quintile(
      city          = quintile_labels[i],
      city_order    = i,
      pollution_dir = city_specs$arrow_clean[i],
      dist_pq       = city_specs$dist_pq[i],
      census_file   = city_specs$micro_census[i],
      geo_id_col    = city_specs$micro_id_col[i],
      pollutants    = pollutants,
      year          = analysis_year,
      report        = report)
  }
))

save_table_parquet_csv(missing_by_quintile, outdir_missing,
                       paste0("missing_by_education_quintile_", analysis_year))

# ============================================================================================
# VI: Census summary
# ============================================================================================
dir.create(outdir_census, recursive = TRUE, showWarnings = FALSE)

# Population totals and geographic-unit counts behind the exposure estimates. Units with a
# missing id or a non-positive weight are dropped, so the count is the estimation-relevant
# one rather than the file's row count.
census_labels <- data.table::data.table(
  city       = c("Bogota", "Mexico City", "Gran Santiago", "Sao Paulo"),
  city_latex = c("Bogot\\'a", "Mexico City", "Gran Santiago", "S\\~ao Paulo"))

census_summary <- data.table::rbindlist(lapply(
  seq_len(nrow(city_specs)),
  function(i) {
    compute_city_census_summary(
      census_path  = city_specs$geo_census[i],
      city         = census_labels$city[i],
      city_latex   = census_labels$city_latex[i],
      census_year  = city_specs$census_year[i],
      census_level = city_specs$census_level[i],
      geo_id_col   = city_specs$geo_id_col[i],
      pop_col      = city_specs$pop_col[i])
  }
))

save_table_parquet_csv(census_summary, outdir_census, "census_summary")

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
