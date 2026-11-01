# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Fill missing station-hour readings using the other stations.
#
#' @Description: Fit OLS separately for each station, pollutant and year, using
# contemporaneous readings at other stations and calendar effects. The window runs from
# the first finite reading through year-end; complete windows skip fitting. Earlier gaps
# and unsupported predictions remain missing. Observed readings are unchanged.
# The manuscript uses PM10 and PM2.5 in 2023. The function writes panels and fitted values,
# returning paths, counts and per_station fitting summaries for inspection in RStudio.
# See impute_missing_hourly_ols() for the model and the conditions that leave gaps unfilled.
#
#' @Summary:
#   I.   Import data: source the model, settings and input/output paths.
#   II.  Process data: impute each city and combine the returned counts.
#   III. Save outputs: write the small combined count table.
#
#' @Date: September 2026
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
source(here::here("src", "general_utilities", "process", "imputation.R"))
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("config", "analysis_settings.R"))

# Set the cleaned input datasets and the output folder
dir_pollution  <- here::here("data", "processed", "monitoring_stations_outliers")
outdir_imputed <- here::here("data", "processed", "imputed_ols")
panel_bogota   <- here::here(dir_pollution, "bogota_metro_clean")
panel_cdmx     <- here::here(dir_pollution, "cdmx_metro_clean")
panel_santiago <- here::here(dir_pollution, "santiago_metro_clean")
panel_sp       <- here::here(dir_pollution, "sao_paulo_metro_clean")
file_summary   <- here::here(outdir_imputed,
                             paste0("imputation_summary_", imputation_year, ".parquet"))

# ==========================================================================================
# II: Process data
# ==========================================================================================
# Each call writes the hourly dataset and returns paths, per-pollutant and per-year counts.
# CDMX uses station_code; the other cities use station.
res_bogota   <- impute_missing_hourly_ols(arrow_dir  = panel_bogota,
                                          out_dir    = outdir_imputed,
                                          out_name   = "bogota_imputed",
                                          pollutants = imputation_pollutants,
                                          id_col     = "station",
                                          years      = imputation_year,
                                          diag_year  = imputation_year)

res_cdmx     <- impute_missing_hourly_ols(arrow_dir  = panel_cdmx,
                                          out_dir    = outdir_imputed,
                                          out_name   = "cdmx_imputed",
                                          pollutants = imputation_pollutants,
                                          id_col     = "station_code",
                                          years      = imputation_year,
                                          diag_year  = imputation_year)

res_santiago <- impute_missing_hourly_ols(arrow_dir  = panel_santiago,
                                          out_dir    = outdir_imputed,
                                          out_name   = "santiago_imputed",
                                          pollutants = imputation_pollutants,
                                          id_col     = "station",
                                          years      = imputation_year,
                                          diag_year  = imputation_year)

res_sp       <- impute_missing_hourly_ols(arrow_dir  = panel_sp,
                                          out_dir    = outdir_imputed,
                                          out_name   = "sao_paulo_imputed",
                                          pollutants = imputation_pollutants,
                                          id_col     = "station",
                                          years      = imputation_year,
                                          diag_year  = imputation_year)

# Count station-hours filled by city and pollutant; hourly panels stay on disk.
summary_tbl <- data.table::rbindlist(list(Bogota = res_bogota$per_poll,
                                          CDMX = res_cdmx$per_poll,
                                          Santiago = res_santiago$per_poll,
                                          "Sao Paulo" = res_sp$per_poll), idcol = "city")

# ==========================================================================================
# III: Save outputs
# ==========================================================================================
arrow::write_parquet(summary_tbl, file_summary)
