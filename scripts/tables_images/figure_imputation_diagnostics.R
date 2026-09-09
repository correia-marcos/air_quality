# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Show what the hourly imputation predicted, and whether the filled hours differ.
#
#' @Description: Draws the manuscript's two imputation diagnostics from the fitted values
# written by impute_missing_hourly.R. The first family puts the linear prediction against
# the observed series, one panel per station, for each city and pollutant. The second
# divides each station's mean prediction over its missing hours by the mean of its
# observed hours, with stations ordered by the mean years of schooling of the area they
# sit in, which is what would reveal missingness concentrated in low-education areas.
#
#' @Summary:
#   I.   Setup: load dependencies, set the paper theme, define paths.
#   II.  Read the fitted values and the station-level socioeconomic data.
#   III. Prediction-versus-observed series, one figure per city and pollutant.
#   IV.  Predicted-missing to observed ratios, one figure per city and pollutant.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Setup
# ============================================================================================
# Define input and output folders
dir_imputed <- here::here("data", "processed", "imputed_ols")
dir_station <- here::here("data", "processed", "station_socio_exposure")
outdir_fig  <- here::here("results", "figures", "imputation")

dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

# Define the fitted-value paths written alongside each imputed panel
pred_bogota_pq   <- here::here(dir_imputed, "bogota_imputed_predictions.parquet")
pred_cdmx_pq     <- here::here(dir_imputed, "cdmx_imputed_predictions.parquet")
pred_santiago_pq <- here::here(dir_imputed, "santiago_imputed_predictions.parquet")
pred_sp_pq       <- here::here(dir_imputed, "sao_paulo_imputed_predictions.parquet")

# Define station-level socioeconomic exposure paths, the source of the education ordering
station_bogota_pq   <- here::here(dir_station, "bogota_2018",
                                  "bogota_2018_2023_3km_station_socio.parquet")
station_cdmx_pq     <- here::here(dir_station, "cdmx_2020",
                                  "cdmx_2020_2023_station_socio.parquet")
station_santiago_pq <- here::here(dir_station, "santiago_2017",
                                  "santiago_2017_2023_station_socio.parquet")
station_sp_pq       <- here::here(dir_station, "sao_paulo_2010",
                                  "sao_paulo_2010_2023_station_socio.parquet")

# ============================================================================================
# II: Read processed data
# ============================================================================================
pred_bogota   <- safe_read_parquet(pred_bogota_pq)
pred_cdmx     <- safe_read_parquet(pred_cdmx_pq)
pred_santiago <- safe_read_parquet(pred_santiago_pq)
pred_sp       <- safe_read_parquet(pred_sp_pq)

station_bogota   <- safe_read_parquet(station_bogota_pq)
station_cdmx     <- safe_read_parquet(station_cdmx_pq)
station_santiago <- safe_read_parquet(station_santiago_pq)
station_sp       <- safe_read_parquet(station_sp_pq)

# Sao Paulo stores education x1000 in the source census; correct it once per city.
station_bogota   <- rescale_station_education(station_bogota, "Bogota")
station_cdmx     <- rescale_station_education(station_cdmx, "Mexico City")
station_santiago <- rescale_station_education(station_santiago, "Gran Santiago")
station_sp       <- rescale_station_education(station_sp, "Sao Paulo")

# ============================================================================================
# III: Prediction versus observed series
# ============================================================================================
# The two pollutants differ only in which rows they read and what the manuscript calls
# the file, so the pollutant is the loop and the cities are written out. PM10 carries no
# pollutant token in the file name, because that is how the manuscript cites it.
for (pol in c("pm10", "pm25")) {

  pol_tag <- if (pol == "pm10") "" else "_pm25"

  plot_imputation_series(
    pred_dt    = pred_bogota,
    pollutant  = pol,
    city_label = "Bogotá",
    out_file   = file.path(outdir_fig, paste0("model2_bogota", pol_tag, ".pdf")))

  plot_imputation_series(
    pred_dt    = pred_cdmx,
    pollutant  = pol,
    city_label = "Mexico City",
    out_file   = file.path(outdir_fig, paste0("model2_mexico", pol_tag, ".pdf")))

  plot_imputation_series(
    pred_dt    = pred_santiago,
    pollutant  = pol,
    city_label = "Santiago",
    out_file   = file.path(outdir_fig, paste0("model2_santiago", pol_tag, ".pdf")))

  plot_imputation_series(
    pred_dt    = pred_sp,
    pollutant  = pol,
    city_label = "São Paulo",
    out_file   = file.path(outdir_fig, paste0("model2_saopaulo", pol_tag, ".pdf")))
}

# ============================================================================================
# IV: Predicted-missing to observed ratios
# ============================================================================================
for (pol in c("pm10", "pm25")) {

  pol_tag <- if (pol == "pm10") "" else "_pm25"

  plot_imputation_ratio_by_station(
    pred_dt    = pred_bogota,
    station_dt = station_bogota,
    pollutant  = pol,
    city_label = "Bogotá",
    out_file   = file.path(outdir_fig,
                           paste0("model2_bogota_scatter", pol_tag, ".pdf")))

  plot_imputation_ratio_by_station(
    pred_dt    = pred_cdmx,
    station_dt = station_cdmx,
    pollutant  = pol,
    city_label = "Mexico City",
    out_file   = file.path(outdir_fig,
                           paste0("model2_mexico_scatter", pol_tag, ".pdf")))

  plot_imputation_ratio_by_station(
    pred_dt    = pred_santiago,
    station_dt = station_santiago,
    pollutant  = pol,
    city_label = "Santiago",
    out_file   = file.path(outdir_fig,
                           paste0("model2_santiago_scatter", pol_tag, ".pdf")))

  plot_imputation_ratio_by_station(
    pred_dt    = pred_sp,
    station_dt = station_sp,
    pollutant  = pol,
    city_label = "São Paulo",
    out_file   = file.path(outdir_fig,
                           paste0("model2_saopaulo_scatter", pol_tag, ".pdf")))
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
