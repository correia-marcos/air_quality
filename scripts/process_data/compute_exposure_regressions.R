# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Estimate population-weighted exposure by educational quintiles.
#
# @Description: Runs Weighted Least Squares (WLS) regressions to calculate mean
# exposure levels and 95% confidence intervals across educational quintiles.
# Separates statistical modeling from visualization to optimize processing.
#
# @Summary:
#   I.   Import data: Define paths for IDW exposure and quintile Parquet files.
#   II.  Process: Fit WLS models for each city, pollutant, and outcome.
#   III. Save: Export the unified CI estimates table as a Parquet artifact.
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
dir_exp          <- here::here("data", "processed", "idw_estimates")
outdir_estimates <- here::here("data", "processed", "idw_regressions")

# Define processing parameters per city utilizing true individual microdata
specs <- list(
  list(
    label      = "Bogota", 
    dir        = here::here(dir_exp, "bogota_2018"),
    prefix     = "bogota_2018_3km", 
    geo_id_col = "geo_id",
    pop_col    = "fe", 
    mode       = "individual"
  ),
  list(
    label      = "Mexico City", 
    dir        = here::here(dir_exp, "cdmx_2020"),
    prefix     = "cdmx_2020_3km", 
    geo_id_col = "geo_id",
    pop_col    = "FACTOR", 
    mode       = "individual"
  ),
  list(
    label      = "Santiago", 
    dir        = here::here(dir_exp, "santiago_2024"),
    prefix     = "santiago_2024_3km",
    geo_id_col = "geo_id",
    pop_col    = "fe", 
    mode       = "individual"
  ),
  list(
    label      = "Sao Paulo", 
    dir        = here::here(dir_exp, "sao_paulo_2010"),
    prefix     = "sao_paulo_2010_3km", 
    geo_id_col = "code_weighting",
    pop_col    = "weight", 
    mode       = "individual"
  )
)

# ============================================================================================
# II: Compute
# ============================================================================================
ci_tables <- list()

for (s in specs) {
  exp_pq <- file.path(s$dir, paste0(s$prefix, "_idw_exposure.parquet"))
  ind_pq <- file.path(s$dir, paste0(s$prefix, "_indiv_quintiles.parquet"))
  
  if (!file.exists(exp_pq)) {
    message("[", s$label, "] Missing exposure Parquet — skipping (", exp_pq, ")")
    next
  }
  
  # Map the individual quintile file based on the processing mode
  pass_ind_pq <- if (s$mode == "individual") ind_pq else NULL
  
  # Execute WLS regressions to extract population-weighted estimates
  res <- compute_exposure_ci_regression(
    exposure_parquet = exp_pq,
    individual_pq    = pass_ind_pq,
    geo_id_col       = s$geo_id_col,
    pop_col          = s$pop_col,
    pollutants       = c("pm10", "pm25"),
    year_filter      = 2023,
    base_quintile    = 5L,
    normalized       = TRUE  # Legacy replication mode
  )
  
  # Append the city identifier for downstream plotting functions
  if (nrow(res) > 0) {
    res[, city := s$label]
    ci_tables[[s$label]] <- res
  }
}

# Bind the results into a single comprehensive data.table
all_ci_estimates <- data.table::rbindlist(ci_tables, fill = TRUE)

# ============================================================================================
# III: Save
# ============================================================================================
dir.create(outdir_estimates, recursive = TRUE, showWarnings = FALSE)

save_raw_data_tidy_formatted(
  data          = all_ci_estimates,
  out_dir       = outdir_estimates,
  out_name      = "exposure_ci_estimates_2023",
  write_rds     = FALSE,
  write_parquet = TRUE,
  write_csv_gz  = FALSE
)

cat("Script from the IDB project executed successfully in the Docker container!\n")