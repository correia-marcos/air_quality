# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute raw quintile exposure summaries and exposure-regression coefficients
#   with confidence intervals for the four metropolitan areas.
#
# @Description:
#   For each city, reads the geo-level IDW exposure and the geo-by-group population
#   EAGERLY into memory (so each table can be inspected in RStudio), then applies three
#   functions: one for raw weighted exposure summaries by education quintile, one for
#   the regression gaps relative to the top quintile with confidence intervals, and one
#   recording how many geographic units survive to estimation (which is what sets the
#   cluster count). Results are combined across cities and saved as Parquet and CSV.
#
# @Summary:
#   I.   Import data: define paths/options and read each city's inputs into memory.
#   II.  Process: compute summaries, regressions and coverage per city, then combine.
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
dir_idw <- here::here("data", "processed", "idw_estimates")
dir_out <- here::here("data", "processed", "idw_regressions")

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

# Human-readable labels stamped onto every output row
socioeconomic_var <- "education"
group_type        <- "quintile"

# Income groups exist only for the two cities whose census carries income (CDMX and
# Sao Paulo), and the two cities cannot use the same grouping. Sao Paulo runs deciles
# against its 633 weighting areas. CDMX runs on 63 municipalities, of which only ~10
# keep a station inside the buffer, so 10 deciles ask for 10 coefficients from 10
# clusters: the sandwich has rank at most G - 1, so the variance is not identified and
# every interval came back NA. Quintiles halve the coefficient count. This is a rank
# condition, not a rule of thumb -- see exposure_coverage_2023 for the counts.
inc_socio_var         <- "income"
inc_group_col_cdmx    <- "income_quintile"
inc_group_values_cdmx <- 1:5
inc_base_group_cdmx   <- 5L
inc_group_type_cdmx   <- "quintile"
inc_group_col_sp      <- "income_decile"
inc_group_values_sp   <- 1:10
inc_base_group_sp     <- 10L
inc_group_type_sp     <- "decile"

# Outcome selectors: raw summaries use means + exceedance hours; the regressions
# use only the WHO IT1/IT2 hourly exceedance outcomes.
summary_outcomes <- "^(avg|hrs_d)_"
ci_outcomes      <- "^hrs_d_.*_it[12]$"

# Exposure and individual (geo-by-group) Parquet files per city
exp_bogota   <- here::here(dir_idw, "bogota_2018",
                           "bogota_2018_3km_idw_exposure.parquet")
exp_cdmx     <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_idw_exposure.parquet")
exp_santiago <- here::here(dir_idw, "santiago_2017",
                           "santiago_2017_3km_idw_exposure.parquet")
exp_sp       <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_idw_exposure.parquet")

ind_bogota   <- here::here(dir_idw, "bogota_2018",
                           "bogota_2018_3km_indiv_groups.parquet")
ind_cdmx     <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_indiv_groups.parquet")
ind_santiago <- here::here(dir_idw, "santiago_2017",
                           "santiago_2017_3km_indiv_groups.parquet")
ind_sp       <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_indiv_groups.parquet")

# Income group files (CDMX and Sao Paulo only); "_income" precedes the suffix.
exp_cdmx_inc <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_income_idw_exposure.parquet")
exp_sp_inc   <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_income_idw_exposure.parquet")

ind_cdmx_inc <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_income_indiv_groups.parquet")
ind_sp_inc   <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_income_indiv_groups.parquet")

# Santiago robustness: commune level, 2024 census (main run is zonas 2017)
exp_santiago_com <- here::here(dir_idw, "santiago_2024",
                               "santiago_2024_3km_idw_exposure.parquet")
ind_santiago_com <- here::here(dir_idw, "santiago_2024",
                               "santiago_2024_3km_indiv_groups.parquet")

# Read every city's exposure data eagerly (not lazily) for RStudio inspection
exposure_bogota   <- data.table::as.data.table(arrow::read_parquet(exp_bogota))
exposure_cdmx     <- data.table::as.data.table(arrow::read_parquet(exp_cdmx))
exposure_santiago <- data.table::as.data.table(arrow::read_parquet(exp_santiago))
exposure_sp       <- data.table::as.data.table(arrow::read_parquet(exp_sp))

# Read every city's geo-by-group population data eagerly
individual_bogota   <- data.table::as.data.table(arrow::read_parquet(ind_bogota))
individual_cdmx     <- data.table::as.data.table(arrow::read_parquet(ind_cdmx))
individual_santiago <- data.table::as.data.table(arrow::read_parquet(ind_santiago))

exposure_santiago_com   <- data.table::as.data.table(
  arrow::read_parquet(exp_santiago_com))
individual_santiago_com <- data.table::as.data.table(
  arrow::read_parquet(ind_santiago_com))
individual_sp       <- data.table::as.data.table(arrow::read_parquet(ind_sp))

# Read income group inputs eagerly for CDMX and Sao Paulo
exposure_cdmx_inc   <- data.table::as.data.table(arrow::read_parquet(exp_cdmx_inc))
exposure_sp_inc     <- data.table::as.data.table(arrow::read_parquet(exp_sp_inc))
individual_cdmx_inc <- data.table::as.data.table(arrow::read_parquet(ind_cdmx_inc))
individual_sp_inc   <- data.table::as.data.table(arrow::read_parquet(ind_sp_inc))

# Each city's population/expansion-weight column (differs by census source)
pop_bogota   <- "fe"
pop_cdmx     <- "FACTOR"
pop_santiago <- "fe"
pop_sp       <- "weight"

# Distance matrices, read by the coverage step to count the metro-wide geo units and
# how many of them keep a station inside the buffer. Paths only: the function reads them.
dir_dist      <- here::here("data", "processed", "distances_matrices")
dist_bogota   <- here::here(dir_dist, "bogota_2018",
                            "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_dist, "cdmx_2020",
                            "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_dist, "santiago_2017",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_dist, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")
dist_santiago_com <- here::here(dir_dist, "santiago_2024",
                                "matrix_geo_station_distances.parquet")

# ============================================================================================
# II: Process data
# ============================================================================================
# Create the output folder before processing
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# 1. Bogota
# --------------------------------------------------------------------------------------------
summary_bogota <- compute_exposure_summaries(
  exposure_dt   = exposure_bogota,
  individual_dt = individual_bogota,
  pop_col       = pop_bogota,
  group_col     = group_col,
  group_values  = group_values,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_bogota <- compute_exposure_regressions(
  exposure_dt   = exposure_bogota,
  individual_dt = individual_bogota,
  pop_col       = pop_bogota,
  group_col     = group_col,
  group_values  = group_values,
  base_group    = base_group,
  pollutants    = pollutants,
  outcome_pattern = ci_outcomes,
  year_filter   = analysis_year,
  conf_level    = conf_level,
  normalized    = normalized_gaps,
  regression_unit = reg_unit,
  se_type       = se_type)

# 2. CDMX
# --------------------------------------------------------------------------------------------
summary_cdmx <- compute_exposure_summaries(
  exposure_dt   = exposure_cdmx,
  individual_dt = individual_cdmx,
  pop_col       = pop_cdmx,
  group_col     = group_col,
  group_values  = group_values,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_cdmx <- compute_exposure_regressions(
  exposure_dt   = exposure_cdmx,
  individual_dt = individual_cdmx,
  pop_col       = pop_cdmx,
  group_col     = group_col,
  group_values  = group_values,
  base_group    = base_group,
  pollutants    = pollutants,
  outcome_pattern = ci_outcomes,
  year_filter   = analysis_year,
  conf_level    = conf_level,
  normalized    = normalized_gaps,
  regression_unit = reg_unit,
  se_type       = se_type)

# 3. Santiago
# --------------------------------------------------------------------------------------------
summary_santiago <- compute_exposure_summaries(
  exposure_dt   = exposure_santiago,
  individual_dt = individual_santiago,
  pop_col       = pop_santiago,
  group_col     = group_col,
  group_values  = group_values,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_santiago <- compute_exposure_regressions(
  exposure_dt   = exposure_santiago,
  individual_dt = individual_santiago,
  pop_col       = pop_santiago,
  group_col     = group_col,
  group_values  = group_values,
  base_group    = base_group,
  pollutants    = pollutants,
  outcome_pattern = ci_outcomes,
  year_filter   = analysis_year,
  conf_level    = conf_level,
  normalized    = normalized_gaps,
  regression_unit = reg_unit,
  se_type       = se_type)

# 3b. Santiago -- commune level, 2024 census (robustness)
# --------------------------------------------------------------------------------------------
summary_santiago_com <- compute_exposure_summaries(
  exposure_dt   = exposure_santiago_com,
  individual_dt = individual_santiago_com,
  pop_col       = pop_santiago,
  group_col     = group_col,
  group_values  = group_values,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_santiago_com <- compute_exposure_regressions(
  exposure_dt   = exposure_santiago_com,
  individual_dt = individual_santiago_com,
  pop_col       = pop_santiago,
  group_col     = group_col,
  group_values  = group_values,
  base_group    = base_group,
  pollutants    = pollutants,
  outcome_pattern = ci_outcomes,
  year_filter   = analysis_year,
  conf_level    = conf_level,
  normalized    = normalized_gaps,
  regression_unit = reg_unit,
  se_type       = se_type)

# 4. Sao Paulo
# --------------------------------------------------------------------------------------------
summary_sp <- compute_exposure_summaries(
  exposure_dt   = exposure_sp,
  individual_dt = individual_sp,
  pop_col       = pop_sp,
  group_col     = group_col,
  group_values  = group_values,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_sp <- compute_exposure_regressions(
  exposure_dt   = exposure_sp,
  individual_dt = individual_sp,
  pop_col       = pop_sp,
  group_col     = group_col,
  group_values  = group_values,
  base_group    = base_group,
  pollutants    = pollutants,
  outcome_pattern = ci_outcomes,
  year_filter   = analysis_year,
  conf_level    = conf_level,
  normalized    = normalized_gaps,
  regression_unit = reg_unit,
  se_type       = se_type)

# ----------------------------------------------------------------------------------------
# Income groups: CDMX quintiles, Sao Paulo deciles (see the note in section I)
# ----------------------------------------------------------------------------------------
# 5. CDMX -- income quintiles
summary_cdmx_inc <- compute_exposure_summaries(
  exposure_dt   = exposure_cdmx_inc,
  individual_dt = individual_cdmx_inc,
  pop_col       = pop_cdmx,
  group_col     = inc_group_col_cdmx,
  group_values  = inc_group_values_cdmx,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_cdmx_inc <- compute_exposure_regressions(
  exposure_dt   = exposure_cdmx_inc,
  individual_dt = individual_cdmx_inc,
  pop_col       = pop_cdmx,
  group_col     = inc_group_col_cdmx,
  group_values  = inc_group_values_cdmx,
  base_group    = inc_base_group_cdmx,
  pollutants    = pollutants,
  outcome_pattern = ci_outcomes,
  year_filter   = analysis_year,
  conf_level    = conf_level,
  normalized    = normalized_gaps,
  regression_unit = reg_unit,
  se_type       = se_type)

# 6. Sao Paulo -- income deciles
summary_sp_inc <- compute_exposure_summaries(
  exposure_dt   = exposure_sp_inc,
  individual_dt = individual_sp_inc,
  pop_col       = pop_sp,
  group_col     = inc_group_col_sp,
  group_values  = inc_group_values_sp,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_sp_inc <- compute_exposure_regressions(
  exposure_dt   = exposure_sp_inc,
  individual_dt = individual_sp_inc,
  pop_col       = pop_sp,
  group_col     = inc_group_col_sp,
  group_values  = inc_group_values_sp,
  base_group    = inc_base_group_sp,
  pollutants    = pollutants,
  outcome_pattern = ci_outcomes,
  year_filter   = analysis_year,
  conf_level    = conf_level,
  normalized    = normalized_gaps,
  regression_unit = reg_unit,
  se_type       = se_type)

# Stamp city labels, then combine the per-city tables into final datasets
label_city <- function(dt, city, city_id) {
  dt[, `:=`(city = city, city_id = city_id, year = analysis_year,
            buffer_km = buffer_km, socioeconomic_var = socioeconomic_var,
            group_type = group_type)]
  dt[]
}

ci_all <- data.table::rbindlist(list(
  label_city(ci_bogota,   "Bogota",    "bogota_2018"),
  label_city(ci_cdmx,     "CDMX",      "cdmx_2020"),
  label_city(ci_santiago, "Santiago",  "santiago_2017"),
  label_city(ci_sp,       "Sao Paulo", "sao_paulo_2010"),
  label_city(ci_santiago_com, "Santiago (comuna, 2024)", "santiago_2024")),
  fill = TRUE)

summary_all <- data.table::rbindlist(list(
  label_city(summary_bogota,   "Bogota",    "bogota_2018"),
  label_city(summary_cdmx,     "CDMX",      "cdmx_2020"),
  label_city(summary_santiago, "Santiago",  "santiago_2017"),
  label_city(summary_sp,       "Sao Paulo", "sao_paulo_2010"),
  label_city(summary_santiago_com, "Santiago (comuna, 2024)", "santiago_2024")),
  fill = TRUE)

# Put the shared metadata columns first for readability
first_cols <- c("city", "city_id", "year", "buffer_km",
                "socioeconomic_var", "group_type")

data.table::setcolorder(ci_all, c(first_cols, setdiff(names(ci_all), first_cols)))
data.table::setcolorder(summary_all,
                        c(first_cols, setdiff(names(summary_all), first_cols)))

# Income rows carry income labels, so use a dedicated labeler and keep them in separate
# tables since the group definitions differ (CDMX 1:5 quintiles, Sao Paulo 1:10 deciles).
# The grouping label is an argument, not a script scalar, because it now varies by city.
label_city_income <- function(dt, city, city_id, group_type_label) {
  dt[, `:=`(city = city, city_id = city_id, year = analysis_year,
            buffer_km = buffer_km, socioeconomic_var = inc_socio_var,
            group_type = group_type_label)]
  dt[]
}

ci_income_all <- data.table::rbindlist(list(
  label_city_income(ci_cdmx_inc, "CDMX", "cdmx_2020", inc_group_type_cdmx),
  label_city_income(ci_sp_inc, "Sao Paulo", "sao_paulo_2010", inc_group_type_sp)),
  fill = TRUE)

summary_income_all <- data.table::rbindlist(list(
  label_city_income(summary_cdmx_inc, "CDMX", "cdmx_2020", inc_group_type_cdmx),
  label_city_income(summary_sp_inc, "Sao Paulo", "sao_paulo_2010",
                    inc_group_type_sp)), fill = TRUE)

data.table::setcolorder(ci_income_all,
                        c(first_cols, setdiff(names(ci_income_all), first_cols)))
data.table::setcolorder(summary_income_all,
                        c(first_cols, setdiff(names(summary_income_all), first_cols)))

# --------------------------------------------------------------------------------------------
# Coverage: how many geographic units survive each stage before estimation
# --------------------------------------------------------------------------------------------
# The cluster count is decided long before the regression runs, by how many geo units
# keep a station inside the buffer and report the pollutant. Record that chain next to
# the estimates so a small G is a number in an artifact, not a late surprise.
coverage_all <- data.table::rbindlist(list(
  label_city(compute_exposure_coverage(
    exposure_bogota, individual_bogota, dist_bogota, pop_col = pop_bogota,
    group_col = group_col, group_values = group_values, pollutants = pollutants,
    buffer_km = buffer_km, year_filter = analysis_year), "Bogota", "bogota_2018"),
  label_city(compute_exposure_coverage(
    exposure_cdmx, individual_cdmx, dist_cdmx, pop_col = pop_cdmx,
    group_col = group_col, group_values = group_values, pollutants = pollutants,
    buffer_km = buffer_km, year_filter = analysis_year), "CDMX", "cdmx_2020"),
  label_city(compute_exposure_coverage(
    exposure_santiago, individual_santiago, dist_santiago, pop_col = pop_santiago,
    group_col = group_col, group_values = group_values, pollutants = pollutants,
    buffer_km = buffer_km, year_filter = analysis_year), "Santiago", "santiago_2017"),
  label_city(compute_exposure_coverage(
    exposure_sp, individual_sp, dist_sp, pop_col = pop_sp,
    group_col = group_col, group_values = group_values, pollutants = pollutants,
    buffer_km = buffer_km, year_filter = analysis_year), "Sao Paulo", "sao_paulo_2010"),
  label_city(compute_exposure_coverage(
    exposure_santiago_com, individual_santiago_com, dist_santiago_com,
    pop_col = pop_santiago, group_col = group_col, group_values = group_values,
    pollutants = pollutants, buffer_km = buffer_km, year_filter = analysis_year),
    "Santiago (comuna, 2024)", "santiago_2024"),
  label_city_income(compute_exposure_coverage(
    exposure_cdmx_inc, individual_cdmx_inc, dist_cdmx, pop_col = pop_cdmx,
    group_col = inc_group_col_cdmx, group_values = inc_group_values_cdmx,
    pollutants = pollutants, buffer_km = buffer_km, year_filter = analysis_year),
    "CDMX", "cdmx_2020", inc_group_type_cdmx),
  label_city_income(compute_exposure_coverage(
    exposure_sp_inc, individual_sp_inc, dist_sp, pop_col = pop_sp,
    group_col = inc_group_col_sp, group_values = inc_group_values_sp,
    pollutants = pollutants, buffer_km = buffer_km, year_filter = analysis_year),
    "Sao Paulo", "sao_paulo_2010", inc_group_type_sp)),
  fill = TRUE)

# Attach the G the regressions actually used, with the coefficient count it has to
# support. n_geo_estimation above and n_clusters here are computed by independent paths,
# so a disagreement between the two columns is a silent sample loss made visible.
ci_stack <- data.table::rbindlist(list(ci_all, ci_income_all), fill = TRUE)

g_used <- ci_stack[, .(n_clusters = max(n_clusters), n_units = max(n_units),
                       n_coef = max(n_coef)),
                   by = .(city_id, socioeconomic_var, pollutant)]

coverage_all <- merge(coverage_all, g_used,
                      by = c("city_id", "socioeconomic_var", "pollutant"), all.x = TRUE)

data.table::setcolorder(coverage_all,
                        c(first_cols, setdiff(names(coverage_all), first_cols)))

# Print the whole table, ordered so the thinnest samples come first. No threshold is
# applied: there is no defensible cutoff for "too few clusters", so the run reports the
# counts and the population share behind them and leaves the judgement to the reader.
data.table::setorder(coverage_all, n_clusters)

cat("\nGeographic coverage behind each regression (fewest clusters first):\n")
print(coverage_all[, .(city, socioeconomic_var, pollutant, n_geo_metro,
                       n_geo_in_buffer, n_geo_estimation, n_clusters, n_coef,
                       share_pop_estimation = round(share_pop_estimation, 3))])

# ============================================================================================
# III: Save data
# ============================================================================================
# Regression coefficients and confidence intervals
ci_file <- file.path(dir_out, "exposure_ci_estimates_education_2023.parquet")
arrow::write_parquet(ci_all, ci_file)

# Raw exposure summaries by education quintile
summary_file <- file.path(dir_out, "exposure_group_summaries_education_2023.parquet")
arrow::write_parquet(summary_all, summary_file)

# Lightweight CSV copies for coauthors who prefer spreadsheet checks
data.table::fwrite(ci_all,
                   file.path(dir_out, "exposure_ci_estimates_education_2023.csv"))
data.table::fwrite(summary_all,
                   file.path(dir_out, "exposure_group_summaries_education_2023.csv"))

# Income decile outputs (CDMX and Sao Paulo), kept separate from education
ci_income_file <- file.path(dir_out, "exposure_ci_estimates_income_2023.parquet")
arrow::write_parquet(ci_income_all, ci_income_file)

summary_income_file <- file.path(dir_out,
                                 "exposure_group_summaries_income_2023.parquet")
arrow::write_parquet(summary_income_all, summary_income_file)

data.table::fwrite(ci_income_all,
                   file.path(dir_out, "exposure_ci_estimates_income_2023.csv"))
data.table::fwrite(summary_income_all,
                   file.path(dir_out, "exposure_group_summaries_income_2023.csv"))

# Coverage: the geo-unit attrition behind every cluster count, education and income
coverage_file <- file.path(dir_out, "exposure_coverage_2023.parquet")
arrow::write_parquet(coverage_all, coverage_file)

data.table::fwrite(coverage_all,
                   file.path(dir_out, "exposure_coverage_2023.csv"))

cat("Saved education regression estimates to:", ci_file, "\n")
cat("Saved education raw group summaries to:", summary_file, "\n")
cat("Saved income regression estimates to:", ci_income_file, "\n")
cat("Saved income raw group summaries to:", summary_income_file, "\n")
cat("Saved geographic coverage diagnostics to:", coverage_file, "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")
