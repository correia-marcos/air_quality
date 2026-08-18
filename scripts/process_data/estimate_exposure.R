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
# coverage table recording how many geographic units survive to estimation. The paper's
# specification (pollutants, outcome patterns, conf_level, normalized, se_type) lives as
# defaults on run_city_exposure(); only city, geography and grouping vary here. Runs are
# stacked with stack_exposure_runs(), printed, and saved as Parquet and CSV. The whole
# procedure runs once per buffer radius: 3 km is the paper's specification, 5 km is the
# robustness check, and the buffer appears in every output file name.
#
#' @Summary:
#   I.   Import data: define paths and the inputs that do not depend on the buffer.
#   II.  Process: run every city and grouping, one buffer at a time.
#   III. Save: stack the table families, print coverage, write the files.
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

# The buffer radius and analysis year are the only choices that vary run to run
analysis_year <- 2023L
buffers_km    <- c(3L, 5L)

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

# One full pass per buffer. Objects left in the environment hold the last buffer run.
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

  # 1. Bogota -- education quintiles
  bogota <- run_city_exposure(
    city = "Bogota", city_id = "bogota_2018",
    exposure_dt = exposure_bogota, individual_dt = individual_bogota,
    geo_station_pq = dist_bogota, socio_var = "education",
    group_col = "edu_quintile", n_groups = 5L,
    year = analysis_year, buffer_km = buffer_km)

  # 2. CDMX -- education quintiles
  cdmx <- run_city_exposure(
    city = "CDMX", city_id = "cdmx_2020",
    exposure_dt = exposure_cdmx, individual_dt = individual_cdmx,
    geo_station_pq = dist_cdmx, socio_var = "education",
    group_col = "edu_quintile", n_groups = 5L,
    year = analysis_year, buffer_km = buffer_km)

  # 3. Santiago -- education quintiles, zonas censales 2017 (main specification)
  santiago <- run_city_exposure(
    city = "Santiago", city_id = "santiago_2017",
    exposure_dt = exposure_santiago, individual_dt = individual_santiago,
    geo_station_pq = dist_santiago, socio_var = "education",
    group_col = "edu_quintile", n_groups = 5L,
    year = analysis_year, buffer_km = buffer_km)

  # 3b. Santiago -- education quintiles, commune level (2024 robustness)
  santiago_rob <- run_city_exposure(
    city = "Santiago (comuna, 2024)", city_id = "santiago_2024",
    exposure_dt = exposure_santiago_rob, individual_dt = individual_santiago_rob,
    geo_station_pq = dist_santiago_rob, socio_var = "education",
    group_col = "edu_quintile", n_groups = 5L,
    year = analysis_year, buffer_km = buffer_km)

  # 4. Sao Paulo -- education quintiles
  sao_paulo <- run_city_exposure(
    city = "Sao Paulo", city_id = "sao_paulo_2010",
    exposure_dt = exposure_sp, individual_dt = individual_sp,
    geo_station_pq = dist_sp, socio_var = "education",
    group_col = "edu_quintile", n_groups = 5L,
    year = analysis_year, buffer_km = buffer_km)

  # 5. CDMX -- income quintiles
  cdmx_inc <- run_city_exposure(
    city = "CDMX", city_id = "cdmx_2020",
    exposure_dt = exposure_cdmx, individual_dt = individual_cdmx_inc,
    geo_station_pq = dist_cdmx, socio_var = "income",
    group_col = "income_quintile", n_groups = 5L,
    year = analysis_year, buffer_km = buffer_km)

  # 6. Sao Paulo -- income deciles
  sao_paulo_inc <- run_city_exposure(
    city = "Sao Paulo", city_id = "sao_paulo_2010",
    exposure_dt = exposure_sp, individual_dt = individual_sp_inc,
    geo_station_pq = dist_sp, socio_var = "income",
    group_col = "income_decile", n_groups = 10L,
    year = analysis_year, buffer_km = buffer_km)

  # Stack the run families and cross-check cluster counts across independent paths;
  # see stack_exposure_runs() @details.
  tables <- stack_exposure_runs(
    edu_runs = list(bogota, cdmx, santiago, santiago_rob, sao_paulo),
    inc_runs = list(cdmx_inc, sao_paulo_inc))

  # Print the coverage table, thinnest samples first. No threshold is applied: there is
  # no defensible cutoff for "too few clusters", so this reports and leaves it open.
  cat("\nGeographic coverage behind each regression (fewest clusters first):\n")
  print(tables$coverage[, .(city, socioeconomic_var, pollutant, n_geo_metro,
                            n_geo_in_buffer, n_geo_estimation, n_clusters, n_coef,
                            share_pop = round(share_pop_estimation, 3))])

  # Buffer and year go in each file stem, so a result can never be read out of context
  save_exposure_tables(tables, dir_out, buffer_km, analysis_year)
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
