# ======================================================================================
# IDB: Air monitoring
# ======================================================================================
# @Goal: Compute raw exposure summaries and exposure-regression confidence intervals.
#
# @Description:
#   Estimates education-quintile exposure summaries and regression-based confidence
#   intervals for the four Latin American metropolitan areas in the paper. The script
#   reads every city's inputs EAGERLY into memory first, so coauthors can inspect the
#   exact objects being modeled in RStudio before any computation runs. All modeling
#   logic lives in named functions in config_utils_process_data.R.
#
# @Summary:
#   I.   Import: define paths/options and read all city inputs into memory.
#   II.  Process: apply one city-level function to every city and combine.
#   III. Save: write Parquet and CSV outputs.
#
# @Date: May 2026
# @Author: Marcos
# ======================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ======================================================================================
# I: Import data
# ======================================================================================
# Define input and output folders.
dir_idw <- here::here("data", "processed", "idw_estimates")
dir_out <- here::here("data", "processed", "idw_regressions")

# Create the output folder before running anything.
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Methodological choices kept in one place for easy review.
analysis_year   <- 2023L
buffer_km       <- 3L
group_col       <- "edu_quintile"
group_values    <- 1:5
base_group      <- 5L
pollutants      <- c("pm10", "pm25")
conf_level      <- 0.95
main_se_type    <- "cluster_geo"
main_reg_unit   <- "geo_group"
normalized_gaps <- TRUE

# Human-readable labels stamped onto every output row (for clear tables/plots).
socioeconomic_var <- "education"
group_type        <- "quintile"

# Raw summaries: annual means and WHO-threshold exceedance-hour outcomes.
summary_outcomes <- "^(avg|hrs_d)_"

# Main regression figures: WHO IT1 and IT2 hourly exceedance outcomes only.
ci_outcomes <- "^hrs_d_.*_it[12]$"

# Per-city parameters, including each city's population/weight column.
city_specs <- data.table::data.table(
  city    = c("Bogota",          "CDMX",          "Santiago",          "Sao Paulo"),
  city_id = c("bogota_2018",     "cdmx_2020",     "santiago_2024",     "sao_paulo_2010"),
  prefix  = c("bogota_2018_3km", "cdmx_2020_3km", "santiago_2024_3km", "sao_paulo_2010_3km"),
  pop_col = c("fe",              "FACTOR",        "fe",                "weight")
)

# Build the input file paths inside the city table.
city_specs[, exposure_parquet := file.path(
  dir_idw, city_id, paste0(prefix, "_idw_exposure.parquet")
)]

city_specs[, individual_pq := file.path(
  dir_idw, city_id, paste0(prefix, "_indiv_quintiles.parquet")
)]

# Read EVERY city's data eagerly (not lazily) into named lists, so each table is a
# visible object that coauthors can open and check in RStudio before modeling.
exposure_by_city <- lapply(city_specs$exposure_parquet, function(path) {
  data.table::as.data.table(arrow::read_parquet(path))
})

individual_by_city <- lapply(city_specs$individual_pq, function(path) {
  data.table::as.data.table(arrow::read_parquet(path))
})

names(exposure_by_city)   <- city_specs$city_id
names(individual_by_city) <- city_specs$city_id

# Convenience handles for quick inspection of one city in the console.
exposure_data_inspect   <- exposure_by_city[["santiago_2024"]]
individual_data_inspect <- individual_by_city[["santiago_2024"]]

# ======================================================================================
# II: Process data
# ======================================================================================
# Run the same transparent city-level function for every city, passing the
# already-loaded in-memory tables (no second read inside the function).
city_results <- lapply(
  seq_len(nrow(city_specs)),
  function(i) {
    run_exposure_results_for_city(
      exposure_dt      = exposure_by_city[[i]],
      individual_dt    = individual_by_city[[i]],
      spec_row         = city_specs[i],
      analysis_year    = analysis_year,
      buffer_km        = buffer_km,
      group_col        = group_col,
      group_values     = group_values,
      base_group       = base_group,
      pollutants       = pollutants,
      summary_outcomes = summary_outcomes,
      ci_outcomes      = ci_outcomes,
      confidence_level = conf_level,
      main_reg_unit     = main_reg_unit,
      main_se_type      = main_se_type,
      normalized        = normalized_gaps,
      socioeconomic_var = socioeconomic_var,
      group_type        = group_type,
      validate_inputs   = TRUE
    )
  }
)

names(city_results) <- city_specs$city_id

# Combine city outputs into the final analysis datasets.
ci_all <- data.table::rbindlist(
  lapply(city_results, `[[`, "ci"),
  fill = TRUE
)

summary_all <- data.table::rbindlist(
  lapply(city_results, `[[`, "summary"),
  fill = TRUE
)

# Put the shared metadata columns first for readability.
first_cols <- c(
  "city", "city_id", "year", "buffer_km", "socioeconomic_var", "group_type"
)

data.table::setcolorder(
  ci_all, c(first_cols, setdiff(names(ci_all), first_cols))
)

data.table::setcolorder(
  summary_all, c(first_cols, setdiff(names(summary_all), first_cols))
)

# ======================================================================================
# III: Save data
# ======================================================================================
# Regression coefficients and confidence intervals.
ci_file <- file.path(dir_out, "exposure_ci_estimates_education_2023.parquet")
arrow::write_parquet(ci_all, ci_file)

# Raw exposure summaries by education quintile.
summary_file <- file.path(dir_out, "exposure_group_summaries_education_2023.parquet")
arrow::write_parquet(summary_all, summary_file)

# Lightweight CSV copies for coauthors who prefer spreadsheet checks.
data.table::fwrite(
  ci_all, file.path(dir_out, "exposure_ci_estimates_education_2023.csv")
)
data.table::fwrite(
  summary_all, file.path(dir_out, "exposure_group_summaries_education_2023.csv")
)

cat("Saved regression estimates to:", ci_file, "\n")
cat("Saved raw group summaries to:", summary_file, "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")