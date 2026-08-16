# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Estimate raw exposure levels, normalized exposure regressions and the geographic
# coverage behind them.
#
#' @Description: This script provides the main results related to exposure inequality in
# the paper. It uses the geo-level IDW exposure and the geo-by-group population. For each
# city it calls run_city_exposure(), which returns weighted exposure summaries by group,
# regression gaps relative to the top group with clustered confidence intervals, and a
# coverage table recording how many geographic units survive to estimation. Results are
# stacked across cities and saved as Parquet and CSV. The whole procedure runs once per
# buffer radius: 3 km is the paper's specification, 5 km is the robustness check, and
# the buffer appears in every output file name.
#
#' @Summary:
#   I.   Import data: define paths, the methodological options, and the inputs that do
#        not depend on the buffer.
#   II.  Process: run every city and grouping, one buffer at a time.
#   III. Save: stack the table families, cross-check the cluster counts, write the files.
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
pollutants      <- c("pm10", "pm25")
conf_level      <- 0.95
se_type         <- "cluster_geo"
normalized_gaps <- TRUE

# Outcome selectors: summaries use means + exceedance hours, regressions only IT1/IT2
summary_outcomes <- "^(avg|hrs_d)_"
ci_outcomes      <- "^hrs_d_.*_it[12]$"

# Geo-to-station distance matrices, read by the coverage step.
dist_bogota       <- here::here(dir_dist, "bogota_2018",
                                "matrix_geo_station_distances.parquet")
dist_cdmx         <- here::here(dir_dist, "cdmx_2020",
                                "matrix_geo_station_distances.parquet")
dist_santiago     <- here::here(dir_dist, "santiago_2017",
                                "matrix_geo_station_distances.parquet")
dist_santiago_rob <- here::here(dir_dist, "santiago_2024",
                                "matrix_geo_station_distances.parquet")
dist_sp           <- here::here(dir_dist, "sao_paulo_2010",
                                "matrix_geo_station_distances.parquet")

# Geo-by-group population; income exists only for CDMX and SP.
individual_bogota       <- read_idw_artifact(dir_idw, "bogota_2018", "indiv_groups")
individual_cdmx         <- read_idw_artifact(dir_idw, "cdmx_2020", "indiv_groups")
individual_santiago     <- read_idw_artifact(dir_idw, "santiago_2017", "indiv_groups")
individual_santiago_rob <- read_idw_artifact(dir_idw, "santiago_2024", "indiv_groups")
individual_sp           <- read_idw_artifact(dir_idw, "sao_paulo_2010", "indiv_groups")
individual_cdmx_inc     <- read_idw_artifact(dir_idw, "cdmx_2020", "indiv_groups",
                                             suffix = "_income")
individual_sp_inc       <- read_idw_artifact(dir_idw, "sao_paulo_2010", "indiv_groups",
                                             suffix = "_income")

# ============================================================================================
# II and III: Process and save
# ============================================================================================
# Create the output folder before processing
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# One full pass per buffer. Objects left in the environment hold the last buffer run,
# and run_one() below reads buffer_km at call time, so it does too.
for (buffer_km in buffers_km) {

  cat("\n=== Buffer:", buffer_km, "km ===\n")

  # Read each city's exposure here. The income runs below reuse these same tables
  exposure_bogota       <- read_idw_artifact(dir_idw, "bogota_2018", "idw_exposure",
                                             buffer_km)
  exposure_cdmx         <- read_idw_artifact(dir_idw, "cdmx_2020", "idw_exposure",
                                             buffer_km)
  exposure_santiago     <- read_idw_artifact(dir_idw, "santiago_2017", "idw_exposure",
                                             buffer_km)
  exposure_santiago_rob <- read_idw_artifact(dir_idw, "santiago_2024", "idw_exposure",
                                             buffer_km)
  exposure_sp           <- read_idw_artifact(dir_idw, "sao_paulo_2010", "idw_exposure",
                                             buffer_km)

  # Bind this buffer and the specification above, so each city call below states only
  # what actually differs between cities: its data, its geography and its grouping.
  run_one <- function(city, city_id, exposure_dt, individual_dt, geo_station_pq,
                      socio_var, group_col, n_groups) {
    run_city_exposure(
      city            = city,
      city_id         = city_id,
      exposure_dt     = exposure_dt,
      individual_dt   = individual_dt,
      geo_station_pq  = geo_station_pq,
      socio_var       = socio_var,
      group_col       = group_col,
      n_groups        = n_groups,
      year            = analysis_year,
      buffer_km       = buffer_km,
      pollutants      = pollutants,
      summary_pattern = summary_outcomes,
      ci_pattern      = ci_outcomes,
      conf_level      = conf_level,
      normalized      = normalized_gaps,
      se_type         = se_type)
  }

  # 1. Bogota -- education quintiles
  bogota <- run_one(city = "Bogota", city_id = "bogota_2018",
                    exposure_dt = exposure_bogota, individual_dt = individual_bogota,
                    geo_station_pq = dist_bogota, socio_var = "education",
                    group_col = "edu_quintile", n_groups = 5L)

  # 2. CDMX -- education quintiles
  cdmx <- run_one(city = "CDMX", city_id = "cdmx_2020",
                  exposure_dt = exposure_cdmx, individual_dt = individual_cdmx,
                  geo_station_pq = dist_cdmx, socio_var = "education",
                  group_col = "edu_quintile", n_groups = 5L)

  # 3. Santiago -- education quintiles, zonas censales 2017 (main specification)
  santiago <- run_one(city = "Santiago", city_id = "santiago_2017",
                      exposure_dt = exposure_santiago,
                      individual_dt = individual_santiago,
                      geo_station_pq = dist_santiago, socio_var = "education",
                      group_col = "edu_quintile", n_groups = 5L)

  # 3b. Santiago -- education quintiles, commune level (2024 robustness)
  santiago_rob <- run_one(city = "Santiago (comuna, 2024)", city_id = "santiago_2024",
                          exposure_dt = exposure_santiago_rob,
                          individual_dt = individual_santiago_rob,
                          geo_station_pq = dist_santiago_rob, socio_var = "education",
                          group_col = "edu_quintile", n_groups = 5L)

  # 4. Sao Paulo -- education quintiles
  sao_paulo <- run_one(city = "Sao Paulo", city_id = "sao_paulo_2010",
                       exposure_dt = exposure_sp, individual_dt = individual_sp,
                       geo_station_pq = dist_sp, socio_var = "education",
                       group_col = "edu_quintile", n_groups = 5L)

  # 5. CDMX -- income quintiles
  cdmx_inc <- run_one(city = "CDMX", city_id = "cdmx_2020",
                      exposure_dt = exposure_cdmx, individual_dt = individual_cdmx_inc,
                      geo_station_pq = dist_cdmx, socio_var = "income",
                      group_col = "income_quintile", n_groups = 5L)

  # 6. Sao Paulo -- income deciles
  sao_paulo_inc <- run_one(city = "Sao Paulo", city_id = "sao_paulo_2010",
                           exposure_dt = exposure_sp,
                           individual_dt = individual_sp_inc,
                           geo_station_pq = dist_sp, socio_var = "income",
                           group_col = "income_decile", n_groups = 10L)

  # Stack each table family across runs; education and income stay in separate artifacts
  # because their group definitions differ (1:5 versus 1:10).
  edu_runs <- list(bogota, cdmx, santiago, santiago_rob, sao_paulo)
  inc_runs <- list(cdmx_inc, sao_paulo_inc)

  ci_all             <- stack_city_tables(edu_runs, "ci")
  summary_all        <- stack_city_tables(edu_runs, "summary")
  ci_income_all      <- stack_city_tables(inc_runs, "ci")
  summary_income_all <- stack_city_tables(inc_runs, "summary")
  coverage_all       <- stack_city_tables(c(edu_runs, inc_runs), "coverage")

  # Cluster count per regression, taken as the max across the IT1/IT2 outcomes.
  # n_geo_estimation and n_clusters come from independent paths, so a disagreement
  # between them is a silent sample loss made visible.
  g_used <- data.table::rbindlist(list(ci_all, ci_income_all), fill = TRUE)[
    , .(n_clusters = max(n_clusters), n_units = max(n_units), n_coef = max(n_coef)),
    by = .(city_id, socioeconomic_var, pollutant)]

  coverage_all <- merge(coverage_all, g_used,
                        by = c("city_id", "socioeconomic_var", "pollutant"), all.x = TRUE)

  # Print the coverage table, thinnest samples first. No threshold is applied: there is
  # no defensible cutoff for "too few clusters", so this reports and leaves it open.
  data.table::setorder(coverage_all, n_clusters)

  cat("\nGeographic coverage behind each regression (fewest clusters first):\n")
  print(coverage_all[, .(city, socioeconomic_var, pollutant, n_geo_metro, n_geo_in_buffer,
                         n_geo_estimation, n_clusters, n_coef,
                         share_pop = round(share_pop_estimation, 3))])

  # Buffer and year go in each file stem, so a result can never be read out of context
  save_exposure_tables(
    list(ci_estimates_education    = ci_all,
         group_summaries_education = summary_all,
         ci_estimates_income       = ci_income_all,
         group_summaries_income    = summary_income_all,
         coverage                  = coverage_all),
    dir_out, buffer_km, analysis_year)
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
