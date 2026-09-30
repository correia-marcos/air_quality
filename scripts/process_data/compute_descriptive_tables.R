# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Describe monitoring coverage, data availability and census populations.
#
#' @Description: Read the hourly station panels, distance matrices and individual census
# files to compute six groups of summaries. Coverage and hourly threshold summaries use
# the analysis year; WHO comparisons use every available year. Missingness uses stored
# station-hour rows, without filling gaps in the calendar. Santiago uses the 2017 zones.
# Save the tables for render_station_tables.R, render_missing_tables.R and
# render_census_tables.R; these scripts produce the manuscript's LaTeX tables.
#
#' @Summary:
#   I.   Import data: source functions and settings, then set input and output paths.
#   II.  Process data: compute each city's summaries and combine the city tables.
#   III. Save outputs: write the summaries as Parquet, with CSV copies where used.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the summary and saving functions, plus their shared helpers
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "diagnostics.R"))
source(here::here("src", "general_utilities", "process", "station_socio.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))

# The shared settings define the year, pollutants and availability measure
source(here::here("config", "analysis_settings.R"))

# Set the station-panel, distance-matrix and census folders
dir_raw    <- here::here("data", "interim", "monitoring_stations")
dir_clean  <- here::here("data", "processed", "monitoring_stations_outliers")
dir_dist   <- here::here("data", "processed", "distances_matrices")
dir_census <- here::here("data", "interim", "census")

# Set the output folders for the six groups of summaries
outdir_missing <- here::here("data", "processed", "missing_proportions")
outdir_counts  <- here::here("data", "processed", "station_counts")
outdir_who     <- here::here("data", "processed", "who_exceedances")
outdir_exceed  <- here::here("data", "processed", "threshold_exceedances")
outdir_census  <- here::here("data", "processed", "census_summary")

# Define the station datasets before and after outlier removal
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

# ========================================================================================
# II: Process data
# ========================================================================================
# Compute missing-reading percentages before and after outlier removal
missing_bogota_raw <- compute_missing_proportions(arrow_dir   = raw_bogota,
                                                  pollutants  = summary_pollutants,
                                                  dims        = missing_dimensions,
                                                  year_filter = analysis_year)

missing_cdmx_raw <- compute_missing_proportions(arrow_dir   = raw_cdmx,
                                                pollutants  = summary_pollutants,
                                                dims        = missing_dimensions,
                                                year_filter = analysis_year)

missing_santiago_raw <- compute_missing_proportions(arrow_dir   = raw_santiago,
                                                    pollutants  = summary_pollutants,
                                                    dims        = missing_dimensions,
                                                    year_filter = analysis_year)

missing_sp_raw <- compute_missing_proportions(arrow_dir   = raw_sp,
                                              pollutants  = summary_pollutants,
                                              dims        = missing_dimensions,
                                              year_filter = analysis_year)

missing_bogota_clean <- compute_missing_proportions(arrow_dir   = clean_bogota,
                                                    pollutants  = summary_pollutants,
                                                    dims        = missing_dimensions,
                                                    year_filter = analysis_year)

missing_cdmx_clean <- compute_missing_proportions(arrow_dir   = clean_cdmx,
                                                  pollutants  = summary_pollutants,
                                                  dims        = missing_dimensions,
                                                  year_filter = analysis_year)

missing_santiago_clean <- compute_missing_proportions(arrow_dir   = clean_santiago,
                                                      pollutants  = summary_pollutants,
                                                      dims        = missing_dimensions,
                                                      year_filter = analysis_year)

missing_sp_clean <- compute_missing_proportions(arrow_dir   = clean_sp,
                                                pollutants  = summary_pollutants,
                                                dims        = missing_dimensions,
                                                year_filter = analysis_year)

# Count stations with at least one reported value for each pollutant in the year
counts_bogota <- count_stations_reporting(arrow_dir   = raw_bogota,
                                          pollutants  = summary_pollutants,
                                          year_filter = analysis_year,
                                          mem_gb      = 8)

counts_cdmx <- count_stations_reporting(arrow_dir   = raw_cdmx,
                                        pollutants  = summary_pollutants,
                                        year_filter = analysis_year,
                                        mem_gb      = 8)

counts_santiago <- count_stations_reporting(arrow_dir   = raw_santiago,
                                            pollutants  = summary_pollutants,
                                            year_filter = analysis_year,
                                            mem_gb      = 8)

counts_sp <- count_stations_reporting(arrow_dir   = raw_sp,
                                      pollutants  = summary_pollutants,
                                      year_filter = analysis_year,
                                      mem_gb      = 8)

# Combine the station counts in the order used by the manuscript table
counts_santiago[, city := "Santiago"]
counts_bogota[,   city := "Bogotá"]
counts_cdmx[,     city := "Mexico City"]
counts_sp[,       city := "São Paulo"]

station_counts <- data.table::rbindlist(list(counts_santiago, counts_bogota,
                                            counts_cdmx, counts_sp))
station_counts <- station_counts[, .(city, pm10, pm25)]

# Average station annual means and compare them with WHO annual guidelines
# Keep every available year, including years before the main analysis
who_bogota <- compute_who_exceedances(arrow_dir   = clean_bogota,
                                      city_label  = "bogota",
                                      pollutants  = summary_pollutants,
                                      year_filter = NULL)

who_cdmx <- compute_who_exceedances(arrow_dir   = clean_cdmx,
                                    city_label  = "cdmx",
                                    pollutants  = summary_pollutants,
                                    year_filter = NULL)

who_santiago <- compute_who_exceedances(arrow_dir   = clean_santiago,
                                        city_label  = "santiago",
                                        pollutants  = summary_pollutants,
                                        year_filter = NULL)

who_sp <- compute_who_exceedances(arrow_dir   = clean_sp,
                                  city_label  = "sao_paulo_metro",
                                  pollutants  = summary_pollutants,
                                  year_filter = NULL)

who_exceedances <- data.table::rbindlist(list(who_bogota, who_cdmx,
                                              who_santiago, who_sp), fill = TRUE)

# Count days and hours above each threshold for city-hour and station-hour series
# This retains the existing hourly comparison with the 24-hour interim thresholds
exceed_bogota <- compute_threshold_exceedance_days(arrow_dir   = clean_bogota,
                                                   city_label  = "Bogota",
                                                   year_filter = analysis_year,
                                                   pollutants  = summary_pollutants)

exceed_santiago <- compute_threshold_exceedance_days(arrow_dir   = clean_santiago,
                                                     city_label  = "Santiago",
                                                     year_filter = analysis_year,
                                                     pollutants  = summary_pollutants)

exceed_cdmx <- compute_threshold_exceedance_days(arrow_dir   = clean_cdmx,
                                                 city_label  = "Mexico City",
                                                 year_filter = analysis_year,
                                                 pollutants  = summary_pollutants)

exceed_sp <- compute_threshold_exceedance_days(arrow_dir   = clean_sp,
                                               city_label  = "Sao Paulo",
                                               year_filter = analysis_year,
                                               pollutants  = summary_pollutants)

threshold_exceedances <- data.table::rbindlist(list(exceed_bogota,
                                            exceed_santiago, exceed_cdmx, exceed_sp))

# Assign stations the education quintile of their nearest census unit
# Summarize available readings among stored rows for stations with a matched unit
quintile_bogota <- compute_missing_by_quintile(city          = "Bogota",
                                               city_order    = 1L,
                                               pollution_dir = clean_bogota,
                                               dist_pq       = dist_bogota,
                                               census_file   = micro_bogota,
                                               geo_id_col    = "geo_id",
                                               pollutants    = summary_pollutants,
                                               year          = analysis_year,
                                               report        = availability_report)

quintile_cdmx <- compute_missing_by_quintile(city          = "Mexico City",
                                             city_order    = 2L,
                                             pollution_dir = clean_cdmx,
                                             dist_pq       = dist_cdmx,
                                             census_file   = micro_cdmx,
                                             geo_id_col    = "geo_id",
                                             pollutants    = summary_pollutants,
                                             year          = analysis_year,
                                             report        = availability_report)

quintile_santiago <- compute_missing_by_quintile(city          = "Santiago",
                                                 city_order    = 3L,
                                                 pollution_dir = clean_santiago,
                                                 dist_pq       = dist_santiago,
                                                 census_file   = micro_santiago,
                                                 geo_id_col    = "geo_id",
                                                 pollutants    = summary_pollutants,
                                                 year          = analysis_year,
                                                 report        = availability_report)

quintile_sp <- compute_missing_by_quintile(city          = "Sao Paulo",
                                           city_order    = 4L,
                                           pollution_dir = clean_sp,
                                           dist_pq       = dist_sp,
                                           census_file   = micro_sp,
                                           geo_id_col    = "geo_id",
                                           pollutants    = summary_pollutants,
                                           year          = analysis_year,
                                           report        = availability_report)

missing_by_quintile <- data.table::rbindlist(list(quintile_bogota, quintile_cdmx,
                                                  quintile_santiago, quintile_sp))

# Sum person weights and count geographic units in each individual census file
summary_bogota <- compute_city_census_summary(census_path  = micro_bogota,
                                              city         = "Bogota",
                                              city_latex   = "Bogot\\'a",
                                              census_year  = 2018L,
                                              census_level = "Census tract",
                                              geo_id_col   = "geo_id",
                                              pop_col      = "person_weight")

summary_cdmx <- compute_city_census_summary(census_path  = micro_cdmx,
                                            city         = "Mexico City",
                                            city_latex   = "Mexico City",
                                            census_year  = 2020L,
                                            census_level = "Municipality",
                                            geo_id_col   = "geo_id",
                                            pop_col      = "person_weight")

summary_santiago <- compute_city_census_summary(census_path  = micro_santiago,
                                                city         = "Gran Santiago",
                                                city_latex   = "Gran Santiago",
                                                census_year  = 2017L,
                                                census_level = "Census tract",
                                                geo_id_col   = "geo_id",
                                                pop_col      = "person_weight")

summary_sp <- compute_city_census_summary(census_path  = micro_sp,
                                          city         = "Sao Paulo",
                                          city_latex   = "S\\~ao Paulo",
                                          census_year  = 2010L,
                                          census_level = "Weighting area",
                                          geo_id_col   = "geo_id",
                                          pop_col      = "person_weight")

census_summary <- data.table::rbindlist(list(summary_bogota, summary_cdmx,
                                             summary_santiago, summary_sp))

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the four missingness tables for each city and panel
write_missing_proportions(tables   = missing_bogota_raw,
                          out_dir  = outdir_missing,
                          out_name = "bogota_raw")

write_missing_proportions(tables   = missing_cdmx_raw,
                          out_dir  = outdir_missing,
                          out_name = "cdmx_raw")

write_missing_proportions(tables   = missing_santiago_raw,
                          out_dir  = outdir_missing,
                          out_name = "santiago_raw")

write_missing_proportions(tables   = missing_sp_raw,
                          out_dir  = outdir_missing,
                          out_name = "sao_paulo_metro_raw")

write_missing_proportions(tables   = missing_bogota_clean,
                          out_dir  = outdir_missing,
                          out_name = "bogota_clean")

write_missing_proportions(tables   = missing_cdmx_clean,
                          out_dir  = outdir_missing,
                          out_name = "cdmx_clean")

write_missing_proportions(tables   = missing_santiago_clean,
                          out_dir  = outdir_missing,
                          out_name = "santiago_clean")

write_missing_proportions(tables   = missing_sp_clean,
                          out_dir  = outdir_missing,
                          out_name = "sao_paulo_metro_clean")

# Save station counts, annual WHO comparisons and hourly threshold summaries
save_table_parquet_csv(dt      = station_counts,
                       out_dir = outdir_counts,
                       name    = paste0("stations_by_pollutant_", analysis_year))

save_raw_data_tidy_formatted(data          = who_exceedances,
                             out_dir       = outdir_who,
                             out_name      = "who_exceedances_all_cities",
                             write_rds     = FALSE,
                             write_parquet = TRUE,
                             write_csv_gz  = FALSE)

save_table_parquet_csv(dt      = threshold_exceedances,
                       out_dir = outdir_exceed,
                       name    = paste0("days_and_hours_", analysis_year))

# Save availability by education quintile and the census population summaries
save_table_parquet_csv(dt      = missing_by_quintile,
                       out_dir = outdir_missing,
                       name    = paste0("missing_by_education_quintile_", analysis_year))

save_table_parquet_csv(dt      = census_summary,
                       out_dir = outdir_census,
                       name    = "census_summary")
