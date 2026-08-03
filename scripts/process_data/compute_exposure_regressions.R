# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute raw exposure summaries, exposure-regression coefficients with confidence
#   intervals, and the geographic coverage behind them, for the four metropolitan areas.
#
# @Description: This script reads the geo-level IDW exposure and the geo-by-group
# population eagerly into memory, so each table can be inspected in RStudio. For every
# city it applies three functions: weighted exposure summaries by group, regression gaps
# relative to the top group with clustered confidence intervals, and a coverage table
# recording how many geographic units survive to estimation. Results are stacked across
# cities and saved as Parquet and CSV.
#
# @Summary:
#   I.   Import data: define paths/options and read each city's inputs into memory.
#   II.  Process: run the three functions per city, then stack across cities.
#   III. Save: write Parquet and CSV outputs.
#
# @Date: June 2026
# @Author: Marcos
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

# Methodological choices kept in one place for easy review
analysis_year   <- 2023L
buffer_km       <- 3L
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

# Readers for the two artifact families, whose names follow a fixed pattern:
# <city_id>/<city_id>_<buffer>km[_income]_<what>.parquet and <city_id>/matrix_*.parquet
read_idw <- function(city_id, what, suffix = "") {
  path <- here::here(dir_idw, city_id,
                     sprintf("%s_%dkm%s_%s.parquet", city_id, buffer_km, suffix, what))
  data.table::as.data.table(arrow::read_parquet(path))
}
dist_pq <- function(city_id) {
  here::here(dir_dist, city_id, "matrix_geo_station_distances.parquet")
}

# Read each city's exposure and geo-by-group population eagerly for RStudio inspection
exposure_bogota       <- read_idw("bogota_2018", "idw_exposure")
exposure_cdmx         <- read_idw("cdmx_2020", "idw_exposure")
exposure_santiago     <- read_idw("santiago_2017", "idw_exposure")
exposure_santiago_com <- read_idw("santiago_2024", "idw_exposure")
exposure_sp           <- read_idw("sao_paulo_2010", "idw_exposure")

individual_bogota       <- read_idw("bogota_2018", "indiv_groups")
individual_cdmx         <- read_idw("cdmx_2020", "indiv_groups")
individual_santiago     <- read_idw("santiago_2017", "indiv_groups")
individual_santiago_com <- read_idw("santiago_2024", "indiv_groups")
individual_sp           <- read_idw("sao_paulo_2010", "indiv_groups")

# Income inputs, which carry their own group column
exposure_cdmx_inc   <- read_idw("cdmx_2020", "idw_exposure", "_income")
exposure_sp_inc     <- read_idw("sao_paulo_2010", "idw_exposure", "_income")
individual_cdmx_inc <- read_idw("cdmx_2020", "indiv_groups", "_income")
individual_sp_inc   <- read_idw("sao_paulo_2010", "indiv_groups", "_income")

# Each city's population/expansion-weight column (differs by census source)
pop_bogota   <- "fe"
pop_cdmx     <- "FACTOR"
pop_santiago <- "fe"
pop_sp       <- "weight"

# ============================================================================================
# II: Process data
# ============================================================================================
# Create the output folder before processing
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# One call per city-grouping run. It computes the three tables and stamps the run labels
# on each, so the stacking below is a plain rbind and the labels can never disagree.
run_city <- function(city, city_id, exposure, individual, pop, socio,
                     group, values, base, type) {

  label <- function(dt) {
    dt[, `:=`(city = city, city_id = city_id, year = analysis_year,
              buffer_km = buffer_km, socioeconomic_var = socio, group_type = type)]
    dt[]
  }

  list(
    summary = label(compute_exposure_summaries(
      exposure_dt = exposure, individual_dt = individual, pop_col = pop,
      group_col = group, group_values = values, pollutants = pollutants,
      outcome_pattern = summary_outcomes, year_filter = analysis_year)),

    ci = label(compute_exposure_regressions(
      exposure_dt = exposure, individual_dt = individual, pop_col = pop,
      group_col = group, group_values = values, base_group = base,
      pollutants = pollutants, outcome_pattern = ci_outcomes,
      year_filter = analysis_year, conf_level = conf_level,
      normalized = normalized_gaps, regression_unit = reg_unit, se_type = se_type)),

    coverage = label(compute_exposure_coverage(
      exposure_dt = exposure, individual_dt = individual,
      geo_station_pq = dist_pq(city_id), pop_col = pop, group_col = group,
      group_values = values, pollutants = pollutants, buffer_km = buffer_km,
      year_filter = analysis_year)))
}

# Education quintiles, all four cities plus the Santiago commune-level robustness run
bogota <- run_city("Bogota", "bogota_2018", exposure_bogota, individual_bogota,
                   pop_bogota, "education", group_col, group_values, base_group,
                   "quintile")

cdmx <- run_city("CDMX", "cdmx_2020", exposure_cdmx, individual_cdmx,
                 pop_cdmx, "education", group_col, group_values, base_group,
                 "quintile")

santiago <- run_city("Santiago", "santiago_2017", exposure_santiago, individual_santiago,
                     pop_santiago, "education", group_col, group_values, base_group,
                     "quintile")

santiago_com <- run_city("Santiago (comuna, 2024)", "santiago_2024",
                         exposure_santiago_com, individual_santiago_com, pop_santiago,
                         "education", group_col, group_values, base_group, "quintile")

sao_paulo <- run_city("Sao Paulo", "sao_paulo_2010", exposure_sp, individual_sp,
                      pop_sp, "education", group_col, group_values, base_group,
                      "quintile")

# Income groups, kept separate because CDMX runs quintiles and Sao Paulo deciles
cdmx_inc <- run_city("CDMX", "cdmx_2020", exposure_cdmx_inc, individual_cdmx_inc,
                     pop_cdmx, "income", inc_group_col_cdmx, inc_group_values_cdmx,
                     inc_base_group_cdmx, "quintile")

sao_paulo_inc <- run_city("Sao Paulo", "sao_paulo_2010", exposure_sp_inc,
                          individual_sp_inc, pop_sp, "income", inc_group_col_sp,
                          inc_group_values_sp, inc_base_group_sp, "decile")

# Stack each table family across runs; education and income stay in separate artifacts
# because their group definitions differ (1:5 versus 1:10).
edu_runs <- list(bogota, cdmx, santiago, santiago_com, sao_paulo)
inc_runs <- list(cdmx_inc, sao_paulo_inc)

stack_runs <- function(runs, what) {
  data.table::rbindlist(lapply(runs, `[[`, what), fill = TRUE)
}

ci_all             <- stack_runs(edu_runs, "ci")
summary_all        <- stack_runs(edu_runs, "summary")
ci_income_all      <- stack_runs(inc_runs, "ci")
summary_income_all <- stack_runs(inc_runs, "summary")
coverage_all       <- stack_runs(c(edu_runs, inc_runs), "coverage")

# Attach the cluster count each regression actually used. n_geo_estimation and n_clusters
# come from independent paths, so a disagreement is a silent sample loss made visible.
g_used <- data.table::rbindlist(list(ci_all, ci_income_all), fill = TRUE)[
  , .(n_clusters = max(n_clusters), n_units = max(n_units), n_coef = max(n_coef)),
  by = .(city_id, socioeconomic_var, pollutant)]

coverage_all <- merge(coverage_all, g_used,
                      by = c("city_id", "socioeconomic_var", "pollutant"), all.x = TRUE)

# Put the shared metadata columns first for readability
first_cols <- c("city", "city_id", "year", "buffer_km",
                "socioeconomic_var", "group_type")

meta_first <- function(dt) {
  data.table::setcolorder(dt, c(first_cols, setdiff(names(dt), first_cols)))
}

meta_first(ci_all)
meta_first(summary_all)
meta_first(ci_income_all)
meta_first(summary_income_all)
meta_first(coverage_all)

# Print the coverage table, thinnest samples first. No threshold is applied: there is no
# defensible cutoff for "too few clusters", so this reports and leaves the judgement open.
data.table::setorder(coverage_all, n_clusters)

cat("\nGeographic coverage behind each regression (fewest clusters first):\n")
print(coverage_all[, .(city, socioeconomic_var, pollutant, n_geo_metro, n_geo_in_buffer,
                       n_geo_estimation, n_clusters, n_coef,
                       share_pop = round(share_pop_estimation, 3))])

# ============================================================================================
# III: Save data
# ============================================================================================
# Write each table as Parquet plus a CSV copy for coauthors who prefer spreadsheets
save_both <- function(dt, name) {
  arrow::write_parquet(dt, file.path(dir_out, paste0(name, ".parquet")))
  data.table::fwrite(dt, file.path(dir_out, paste0(name, ".csv")))
  cat("Saved:", file.path(dir_out, name), "\n")
}

save_both(ci_all, "exposure_ci_estimates_education_2023")
save_both(summary_all, "exposure_group_summaries_education_2023")
save_both(ci_income_all, "exposure_ci_estimates_income_2023")
save_both(summary_income_all, "exposure_group_summaries_income_2023")
save_both(coverage_all, "exposure_coverage_2023")

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
