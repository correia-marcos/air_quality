# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Estimate raw exposure levels, normalized exposure regressions and the geographic
# coverage behind them.
#
#' @Description: This script provides the main results related to exposure inequality
# in the paper. It uses the geo-level IDW exposure and the geo-by-group population. For
# city it calls run_city_exposure(), which returns weighted exposure summaries by group,
# regression gaps relative to the top group with clustered confidence intervals, and a
# coverage table recording how many geographic units survive to estimation. Results are
# stacked across cities and saved as Parquet and CSV. The whole procedure runs once per
# buffer radius: 3 km is the paper's specification, 5 km is the robustness check, and the
# buffer appears in every output file name.
#
#' @Summary:
#   I.   Import data: define paths and the methodological options, one buffer at a time.
#   II.  Process and save: run every city, stack the families, write Parquet and CSV.
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
dir_idw  <- here::here("data", "processed", "idw_estimates")
dir_dist <- here::here("data", "processed", "distances_matrices")
dir_out  <- here::here("data", "processed", "idw_regressions")

# Methodological choices kept in one place for easy review. Every one is passed
# explicitly below, so the paper's specification is visible here and nowhere else.
analysis_year   <- 2023L
buffers_km      <- c(3L, 5L)
group_col       <- "edu_quintile"
group_values    <- 1:5
base_group      <- 5L
pollutants      <- c("pm10", "pm25")
conf_level      <- 0.95
se_type         <- "cluster_geo"
reg_unit        <- "geo_group"
normalized_gaps <- TRUE

# Outcome selectors: summaries use means + exceedance hours, regressions only IT1/IT2
summary_outcomes <- "^(avg|hrs_d)_"
ci_outcomes      <- "^hrs_d_.*_it[12]$"

# Income exists only for CDMX and Sao Paulo. CDMX gets quintiles because its ~10 surviving
# municipalities cannot identify the 10 coefficients deciles would need.
inc_group_col_cdmx    <- "income_quintile"
inc_group_values_cdmx <- 1:5
inc_base_group_cdmx   <- 5L
inc_group_col_sp      <- "income_decile"
inc_group_values_sp   <- 1:10
inc_base_group_sp     <- 10L

# Geo-to-station distance matrices, read by the coverage step. Same file for every buffer:
# the buffer filters this matrix, it does not change it.
dist_bogota       <- here::here(dir_dist, "bogota_2018",
                                "matrix_geo_station_distances.parquet")
dist_cdmx         <- here::here(dir_dist, "cdmx_2020",
                                "matrix_geo_station_distances.parquet")
dist_santiago     <- here::here(dir_dist, "santiago_2017",
                                "matrix_geo_station_distances.parquet")
dist_santiago_com <- here::here(dir_dist, "santiago_2024",
                                "matrix_geo_station_distances.parquet")
dist_sp           <- here::here(dir_dist, "sao_paulo_2010",
                                "matrix_geo_station_distances.parquet")

# ============================================================================================
# II and III: Process and save
# ============================================================================================
# Create the output folder before processing
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# One full pass per buffer. Objects left in the environment hold the last buffer run;
# re-run a single read to inspect the other.
for (buffer_km in buffers_km) {

  cat("\n=== Buffer:", buffer_km, "km ===\n")

  # Read each city's exposure and geo-by-group population eagerly for RStudio inspection
  exposure_bogota       <- read_idw_artifact(dir_idw, "bogota_2018", "idw_exposure",
                                             buffer_km)
  exposure_cdmx         <- read_idw_artifact(dir_idw, "cdmx_2020", "idw_exposure",
                                             buffer_km)
  exposure_santiago     <- read_idw_artifact(dir_idw, "santiago_2017", "idw_exposure",
                                             buffer_km)
  exposure_santiago_com <- read_idw_artifact(dir_idw, "santiago_2024", "idw_exposure",
                                             buffer_km)
  exposure_sp           <- read_idw_artifact(dir_idw, "sao_paulo_2010", "idw_exposure",
                                             buffer_km)

  individual_bogota       <- read_idw_artifact(dir_idw, "bogota_2018", "indiv_groups",
                                               buffer_km)
  individual_cdmx         <- read_idw_artifact(dir_idw, "cdmx_2020", "indiv_groups",
                                               buffer_km)
  individual_santiago     <- read_idw_artifact(dir_idw, "santiago_2017", "indiv_groups",
                                               buffer_km)
  individual_santiago_com <- read_idw_artifact(dir_idw, "santiago_2024", "indiv_groups",
                                               buffer_km)
  individual_sp           <- read_idw_artifact(dir_idw, "sao_paulo_2010", "indiv_groups",
                                               buffer_km)

  # Income inputs, which carry their own group column
  exposure_cdmx_inc   <- read_idw_artifact(dir_idw, "cdmx_2020", "idw_exposure",
                                           buffer_km, "_income")
  exposure_sp_inc     <- read_idw_artifact(dir_idw, "sao_paulo_2010", "idw_exposure",
                                           buffer_km, "_income")
  individual_cdmx_inc <- read_idw_artifact(dir_idw, "cdmx_2020", "indiv_groups",
                                           buffer_km, "_income")
  individual_sp_inc   <- read_idw_artifact(dir_idw, "sao_paulo_2010", "indiv_groups",
                                           buffer_km, "_income")

  # 1. Bogota -- education quintiles
  bogota <- run_city_exposure(
    city            = "Bogota",
    city_id         = "bogota_2018",
    exposure_dt     = exposure_bogota,
    individual_dt   = individual_bogota,
    geo_station_pq  = dist_bogota,
    socio_var       = "education",
    group_col       = group_col,
    group_values    = group_values,
    base_group      = base_group,
    group_type      = "quintile",
    year            = analysis_year,
    buffer_km       = buffer_km,
    pollutants      = pollutants,
    summary_pattern = summary_outcomes,
    ci_pattern      = ci_outcomes,
    conf_level      = conf_level,
    normalized      = normalized_gaps,
    regression_unit = reg_unit,
    se_type         = se_type)

  # 2. CDMX -- education quintiles
  cdmx <- run_city_exposure(
    city            = "CDMX",
    city_id         = "cdmx_2020",
    exposure_dt     = exposure_cdmx,
    individual_dt   = individual_cdmx,
    geo_station_pq  = dist_cdmx,
    socio_var       = "education",
    group_col       = group_col,
    group_values    = group_values,
    base_group      = base_group,
    group_type      = "quintile",
    year            = analysis_year,
    buffer_km       = buffer_km,
    pollutants      = pollutants,
    summary_pattern = summary_outcomes,
    ci_pattern      = ci_outcomes,
    conf_level      = conf_level,
    normalized      = normalized_gaps,
    regression_unit = reg_unit,
    se_type         = se_type)

  # 3. Santiago -- education quintiles, zonas censales 2017 (main specification)
  santiago <- run_city_exposure(
    city            = "Santiago",
    city_id         = "santiago_2017",
    exposure_dt     = exposure_santiago,
    individual_dt   = individual_santiago,
    geo_station_pq  = dist_santiago,
    socio_var       = "education",
    group_col       = group_col,
    group_values    = group_values,
    base_group      = base_group,
    group_type      = "quintile",
    year            = analysis_year,
    buffer_km       = buffer_km,
    pollutants      = pollutants,
    summary_pattern = summary_outcomes,
    ci_pattern      = ci_outcomes,
    conf_level      = conf_level,
    normalized      = normalized_gaps,
    regression_unit = reg_unit,
    se_type         = se_type)

  # 3b. Santiago -- education quintiles, commune level (2024 robustness)
  santiago_com <- run_city_exposure(
    city            = "Santiago (comuna, 2024)",
    city_id         = "santiago_2024",
    exposure_dt     = exposure_santiago_com,
    individual_dt   = individual_santiago_com,
    geo_station_pq  = dist_santiago_com,
    socio_var       = "education",
    group_col       = group_col,
    group_values    = group_values,
    base_group      = base_group,
    group_type      = "quintile",
    year            = analysis_year,
    buffer_km       = buffer_km,
    pollutants      = pollutants,
    summary_pattern = summary_outcomes,
    ci_pattern      = ci_outcomes,
    conf_level      = conf_level,
    normalized      = normalized_gaps,
    regression_unit = reg_unit,
    se_type         = se_type)

  # 4. Sao Paulo -- education quintiles
  sao_paulo <- run_city_exposure(
    city            = "Sao Paulo",
    city_id         = "sao_paulo_2010",
    exposure_dt     = exposure_sp,
    individual_dt   = individual_sp,
    geo_station_pq  = dist_sp,
    socio_var       = "education",
    group_col       = group_col,
    group_values    = group_values,
    base_group      = base_group,
    group_type      = "quintile",
    year            = analysis_year,
    buffer_km       = buffer_km,
    pollutants      = pollutants,
    summary_pattern = summary_outcomes,
    ci_pattern      = ci_outcomes,
    conf_level      = conf_level,
    normalized      = normalized_gaps,
    regression_unit = reg_unit,
    se_type         = se_type)

  # --------------------------------------------------------------------------------------
  # Income: only the CDMX and Sao Paulo censuses carry it, and they group it differently
  # 5. CDMX -- income quintiles
  cdmx_inc <- run_city_exposure(
    city            = "CDMX",
    city_id         = "cdmx_2020",
    exposure_dt     = exposure_cdmx_inc,
    individual_dt   = individual_cdmx_inc,
    geo_station_pq  = dist_cdmx,
    socio_var       = "income",
    group_col       = inc_group_col_cdmx,
    group_values    = inc_group_values_cdmx,
    base_group      = inc_base_group_cdmx,
    group_type      = "quintile",
    year            = analysis_year,
    buffer_km       = buffer_km,
    pollutants      = pollutants,
    summary_pattern = summary_outcomes,
    ci_pattern      = ci_outcomes,
    conf_level      = conf_level,
    normalized      = normalized_gaps,
    regression_unit = reg_unit,
    se_type         = se_type)

  # 6. Sao Paulo -- income deciles
  sao_paulo_inc <- run_city_exposure(
    city            = "Sao Paulo",
    city_id         = "sao_paulo_2010",
    exposure_dt     = exposure_sp_inc,
    individual_dt   = individual_sp_inc,
    geo_station_pq  = dist_sp,
    socio_var       = "income",
    group_col       = inc_group_col_sp,
    group_values    = inc_group_values_sp,
    base_group      = inc_base_group_sp,
    group_type      = "decile",
    year            = analysis_year,
    buffer_km       = buffer_km,
    pollutants      = pollutants,
    summary_pattern = summary_outcomes,
    ci_pattern      = ci_outcomes,
    conf_level      = conf_level,
    normalized      = normalized_gaps,
    regression_unit = reg_unit,
    se_type         = se_type)

  # Stack each table family across runs; education and income stay in separate artifacts
  # because their group definitions differ (1:5 versus 1:10).
  edu_runs <- list(bogota, cdmx, santiago, santiago_com, sao_paulo)
  inc_runs <- list(cdmx_inc, sao_paulo_inc)

  ci_all             <- stack_city_tables(edu_runs, "ci")
  summary_all        <- stack_city_tables(edu_runs, "summary")
  ci_income_all      <- stack_city_tables(inc_runs, "ci")
  summary_income_all <- stack_city_tables(inc_runs, "summary")
  coverage_all       <- stack_city_tables(c(edu_runs, inc_runs), "coverage")

  # Attach the cluster count each regression used. n_geo_estimation and n_clusters come
  # from independent paths, so a disagreement is a silent sample loss made visible.
  g_used <- data.table::rbindlist(list(ci_all, ci_income_all), fill = TRUE)[
    , .(n_clusters = max(n_clusters), n_units = max(n_units), n_coef = max(n_coef)),
    by = .(city_id, socioeconomic_var, pollutant)]

  coverage_all <- merge(coverage_all, g_used,
                        by = c("city_id", "socioeconomic_var", "pollutant"), all.x = TRUE)

  # Put the shared metadata columns first for readability
  first_cols <- c("city", "city_id", "year", "buffer_km",
                  "socioeconomic_var", "group_type")

  set_meta_cols_first(ci_all, first_cols)
  set_meta_cols_first(summary_all, first_cols)
  set_meta_cols_first(ci_income_all, first_cols)
  set_meta_cols_first(summary_income_all, first_cols)
  set_meta_cols_first(coverage_all, first_cols)

  # Print the coverage table, thinnest samples first. No threshold is applied: there is
  # no defensible cutoff for "too few clusters", so this reports and leaves it open.
  data.table::setorder(coverage_all, n_clusters)

  cat("\nGeographic coverage behind each regression (fewest clusters first):\n")
  print(coverage_all[, .(city, socioeconomic_var, pollutant, n_geo_metro, n_geo_in_buffer,
                         n_geo_estimation, n_clusters, n_coef,
                         share_pop = round(share_pop_estimation, 3))])

  # Buffer and year go in the file stem, so a result can never be read out of context
  stem <- function(what) sprintf("exposure_%s_%dkm_%d", what, buffer_km, analysis_year)

  save_table_parquet_csv(ci_all, dir_out, stem("ci_estimates_education"))
  save_table_parquet_csv(summary_all, dir_out, stem("group_summaries_education"))
  save_table_parquet_csv(ci_income_all, dir_out, stem("ci_estimates_income"))
  save_table_parquet_csv(summary_income_all, dir_out, stem("group_summaries_income"))
  save_table_parquet_csv(coverage_all, dir_out, stem("coverage"))
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
