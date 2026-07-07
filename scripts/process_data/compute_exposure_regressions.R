# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compute raw quintile exposure summaries and exposure-regression coefficients
#   with confidence intervals for the four metropolitan areas.
#
# @Description:
#   For each city, reads the geo-level IDW exposure and the geo-by-group population
#   EAGERLY into memory (so each table can be inspected in RStudio), then applies two
#   functions: one for raw weighted exposure summaries by education quintile, and one
#   for the regression gaps relative to the top quintile with confidence intervals.
#   Results are combined across cities and saved as Parquet and CSV.
#
# @Summary:
#   I.   Import data: define paths/options and read each city's inputs into memory.
#   II.  Process: compute summaries and regressions per city, then combine.
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

# Income deciles are available only for the two cities whose census has income
# (CDMX and Sao Paulo). The grouping differs from education: 10 deciles, top
# decile as the reference group.
inc_group_col    <- "income_decile"
inc_group_values <- 1:10
inc_base_group   <- 10L
inc_socio_var    <- "income"
inc_group_type   <- "decile"

# Outcome selectors: raw summaries use means + exceedance hours; the regressions
# use only the WHO IT1/IT2 hourly exceedance outcomes.
summary_outcomes <- "^(avg|hrs_d)_"
ci_outcomes      <- "^hrs_d_.*_it[12]$"

# Exposure and individual (geo-by-group) Parquet files per city
exp_bogota   <- here::here(dir_idw, "bogota_2018",
                           "bogota_2018_3km_idw_exposure.parquet")
exp_cdmx     <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_idw_exposure.parquet")
exp_santiago <- here::here(dir_idw, "santiago_2024",
                           "santiago_2024_3km_idw_exposure.parquet")
exp_sp       <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_idw_exposure.parquet")

ind_bogota   <- here::here(dir_idw, "bogota_2018",
                           "bogota_2018_3km_indiv_groups.parquet")
ind_cdmx     <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_indiv_groups.parquet")
ind_santiago <- here::here(dir_idw, "santiago_2024",
                           "santiago_2024_3km_indiv_groups.parquet")
ind_sp       <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_indiv_groups.parquet")

# Income decile files (CDMX and Sao Paulo only); "_income" precedes the suffix.
exp_cdmx_inc <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_income_idw_exposure.parquet")
exp_sp_inc   <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_income_idw_exposure.parquet")

ind_cdmx_inc <- here::here(dir_idw, "cdmx_2020",
                           "cdmx_2020_3km_income_indiv_groups.parquet")
ind_sp_inc   <- here::here(dir_idw, "sao_paulo_2010",
                           "sao_paulo_2010_3km_income_indiv_groups.parquet")

# Read every city's exposure data eagerly (not lazily) for RStudio inspection
exposure_bogota   <- data.table::as.data.table(arrow::read_parquet(exp_bogota))
exposure_cdmx     <- data.table::as.data.table(arrow::read_parquet(exp_cdmx))
exposure_santiago <- data.table::as.data.table(arrow::read_parquet(exp_santiago))
exposure_sp       <- data.table::as.data.table(arrow::read_parquet(exp_sp))

# Read every city's geo-by-group population data eagerly
individual_bogota   <- data.table::as.data.table(arrow::read_parquet(ind_bogota))
individual_cdmx     <- data.table::as.data.table(arrow::read_parquet(ind_cdmx))
individual_santiago <- data.table::as.data.table(arrow::read_parquet(ind_santiago))
individual_sp       <- data.table::as.data.table(arrow::read_parquet(ind_sp))

# Read income decile inputs eagerly for CDMX and Sao Paulo
exposure_cdmx_inc   <- data.table::as.data.table(arrow::read_parquet(exp_cdmx_inc))
exposure_sp_inc     <- data.table::as.data.table(arrow::read_parquet(exp_sp_inc))
individual_cdmx_inc <- data.table::as.data.table(arrow::read_parquet(ind_cdmx_inc))
individual_sp_inc   <- data.table::as.data.table(arrow::read_parquet(ind_sp_inc))

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
# Income deciles (CDMX and Sao Paulo only)
# ----------------------------------------------------------------------------------------
# 5. CDMX -- income deciles
summary_cdmx_inc <- compute_exposure_summaries(
  exposure_dt   = exposure_cdmx_inc,
  individual_dt = individual_cdmx_inc,
  pop_col       = pop_cdmx,
  group_col     = inc_group_col,
  group_values  = inc_group_values,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_cdmx_inc <- compute_exposure_regressions(
  exposure_dt   = exposure_cdmx_inc,
  individual_dt = individual_cdmx_inc,
  pop_col       = pop_cdmx,
  group_col     = inc_group_col,
  group_values  = inc_group_values,
  base_group    = inc_base_group,
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
  group_col     = inc_group_col,
  group_values  = inc_group_values,
  pollutants    = pollutants,
  outcome_pattern = summary_outcomes,
  year_filter   = analysis_year)

ci_sp_inc <- compute_exposure_regressions(
  exposure_dt   = exposure_sp_inc,
  individual_dt = individual_sp_inc,
  pop_col       = pop_sp,
  group_col     = inc_group_col,
  group_values  = inc_group_values,
  base_group    = inc_base_group,
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
  label_city(ci_santiago, "Santiago",  "santiago_2024"),
  label_city(ci_sp,       "Sao Paulo", "sao_paulo_2010")), fill = TRUE)

summary_all <- data.table::rbindlist(list(
  label_city(summary_bogota,   "Bogota",    "bogota_2018"),
  label_city(summary_cdmx,     "CDMX",      "cdmx_2020"),
  label_city(summary_santiago, "Santiago",  "santiago_2024"),
  label_city(summary_sp,       "Sao Paulo", "sao_paulo_2010")), fill = TRUE)

# Put the shared metadata columns first for readability
first_cols <- c("city", "city_id", "year", "buffer_km",
                "socioeconomic_var", "group_type")

data.table::setcolorder(ci_all, c(first_cols, setdiff(names(ci_all), first_cols)))
data.table::setcolorder(summary_all,
                        c(first_cols, setdiff(names(summary_all), first_cols)))

# Income rows carry income labels (decile), so use a dedicated labeler and keep
# them in separate tables since the group definitions differ (1:5 vs 1:10).
label_city_income <- function(dt, city, city_id) {
  dt[, `:=`(city = city, city_id = city_id, year = analysis_year,
            buffer_km = buffer_km, socioeconomic_var = inc_socio_var,
            group_type = inc_group_type)]
  dt[]
}

ci_income_all <- data.table::rbindlist(list(
  label_city_income(ci_cdmx_inc, "CDMX",      "cdmx_2020"),
  label_city_income(ci_sp_inc,   "Sao Paulo", "sao_paulo_2010")), fill = TRUE)

summary_income_all <- data.table::rbindlist(list(
  label_city_income(summary_cdmx_inc, "CDMX",      "cdmx_2020"),
  label_city_income(summary_sp_inc,   "Sao Paulo", "sao_paulo_2010")), fill = TRUE)

data.table::setcolorder(ci_income_all,
                        c(first_cols, setdiff(names(ci_income_all), first_cols)))
data.table::setcolorder(summary_income_all,
                        c(first_cols, setdiff(names(summary_income_all), first_cols)))

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

cat("Saved education regression estimates to:", ci_file, "\n")
cat("Saved education raw group summaries to:", summary_file, "\n")
cat("Saved income regression estimates to:", ci_income_file, "\n")
cat("Saved income raw group summaries to:", summary_income_file, "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")
