# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Fill missing station-hour readings using OLS regression imputation.
#
# @Description: For each city, this script uses an OLS model with temporal fixed effects and
# concurrent readings from all other stations to predict and fill NA values in
# PM10 and PM2.5. By default, it runs in `legacy_mode = TRUE` to perfectly
# replicate the original "shifting identity" spatial alignment.
#
# @Summary:
#   I.   Import data: Define paths for the input Arrow datasets.
#   II.  Process: Apply OLS imputation for Bogotá, CDMX, Santiago, and São Paulo.
#   III. Summary: Print total imputed cells and confirm successful execution.
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define general input and output folders
dir_pollution  <- here::here("data", "processed", "outlier_detection")
outdir_imputed <- here::here("data", "processed", "imputed_ols")

# Define the input Arrow datasets paths
arrow_bogota   <- here::here(dir_pollution, "bogota_metro_clean")
arrow_cdmx     <- here::here(dir_pollution, "cdmx_metro_clean")
arrow_santiago <- here::here(dir_pollution, "santiago_metro_clean")
arrow_sp       <- here::here(dir_pollution, "sao_paulo_metro_clean")

# ============================================================================================
# II: Process and save
# ============================================================================================
# Create the folder for the results, if not yet created
dir.create(outdir_imputed, recursive = TRUE, showWarnings = FALSE)

summaries <- list()

# 1. Bogotá
if (dir.exists(arrow_bogota)) {
  message("\n--- Processing Bogotá ---")
  res_bogota <- impute_missing_hourly_ols(
    arrow_dir  = arrow_bogota,
    out_dir    = outdir_imputed,
    out_name   = "bogota_imputed",
    pollutants = c("pm10", "pm25"),
    id_col     = "station"
  )
  if (!is.null(res_bogota$per_poll)) {
    summaries[["bogota"]] <- res_bogota$per_poll[, city := "Bogota"]
  }
}

# 2. CDMX
if (dir.exists(arrow_cdmx)) {
  message("\n--- Processing CDMX ---")
  res_cdmx <- impute_missing_hourly_ols(
    arrow_dir   = arrow_cdmx,
    out_dir     = outdir_imputed,
    out_name    = "cdmx_imputed",
    pollutants  = c("pm10", "pm25"),
    id_col      = "station_code",
    legacy_mode = TRUE
  )
  if (!is.null(res_cdmx$per_poll)) {
    summaries[["cdmx"]] <- res_cdmx$per_poll[, city := "CDMX"]
  }
}

# 3. Santiago
if (dir.exists(arrow_santiago)) {
  message("\n--- Processing Santiago ---")
  res_sgo <- impute_missing_hourly_ols(
    arrow_dir   = arrow_santiago,
    out_dir     = outdir_imputed,
    out_name    = "santiago_imputed",
    pollutants  = c("pm10", "pm25"),
    legacy_mode = TRUE
  )
  if (!is.null(res_sgo$per_poll)) {
    summaries[["santiago"]] <- res_sgo$per_poll[, city := "Santiago"]
  }
}

# 4. São Paulo
if (dir.exists(arrow_sp)) {
  message("\n--- Processing São Paulo ---")
  res_sp <- impute_missing_hourly_ols(
    arrow_dir   = arrow_sp,
    out_dir     = outdir_imputed,
    out_name    = "sao_paulo_imputed",
    pollutants  = c("pm10", "pm25"),
    legacy_mode = TRUE
  )
  if (!is.null(res_sp$per_poll)) {
    summaries[["sao_paulo"]] <- res_sp$per_poll[, city := "Sao Paulo"]
  }
}

# ============================================================================================
# III: Summary
# ============================================================================================
message("\n--- Imputation Summary ---")
if (length(summaries) > 0) {
  summary_tbl <- data.table::rbindlist(summaries, fill = TRUE)
  print(summary_tbl)
} else {
  message("No cities were processed. Please check your raw data directories.")
}

cat("\nScript from the IDB project executed successfully in the Docker container!\n")