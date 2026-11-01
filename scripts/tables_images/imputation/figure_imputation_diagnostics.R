# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Compare imputation predictions with observed station readings.
#
#' @Description: Plot fitted and observed time series for each station. Then compare the
# predicted mean during missing hours with the mean during observed hours, ordering stations
# by nearby education. These are descriptive model diagnostics, not validation of readings
# that were never observed. The ratio tables and plots remain named objects before saving.
#
#' @Summary:
#   I.   Import data: read predictions and station education; declare figure paths.
#   II.  Process data: compute station ratios and create both plot families.
#   III. Save outputs: write the series plots, followed by the ratio plots.
#
#' @Date: September 2026
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
source(here::here("src", "general_utilities", "plot", "imputation_diagnostics.R"))
source(here::here("src", "general_utilities", "plot", "station_monitoring.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))
set_paper_theme()

# Set the input and output folders
dir_imputed <- here::here("data", "processed", "imputed_ols")
dir_station <- here::here("data", "processed", "station_socio_exposure")
dir_figures <- here::here("results", "figures", "imputation")

# The fitted values are saved beside the hourly imputed panels.
file_pred_bogota   <- here::here(dir_imputed, "bogota_imputed_predictions.parquet")
file_pred_cdmx     <- here::here(dir_imputed, "cdmx_imputed_predictions.parquet")
file_pred_santiago <- here::here(dir_imputed, "santiago_imputed_predictions.parquet")
file_pred_sp       <- here::here(dir_imputed, "sao_paulo_imputed_predictions.parquet")

# Station education orders the ratio plots.
file_station_bogota   <- here::here(dir_station, "bogota_2018",
                                   "bogota_2018_2023_3km_station_socio.parquet")
file_station_cdmx     <- here::here(dir_station, "cdmx_2020",
                                   "cdmx_2020_2023_station_socio.parquet")
file_station_santiago <- here::here(dir_station, "santiago_2017",
                                   "santiago_2017_2023_station_socio.parquet")
file_station_sp       <- here::here(dir_station, "sao_paulo_2010",
                                   "sao_paulo_2010_2023_station_socio.parquet")

# PM10 retains the manuscript filename without a pollutant suffix.
files_series_bogota <- c(pm10 = here::here(dir_figures, "model2_bogota.pdf"),
                         pm25 = here::here(dir_figures, "model2_bogota_pm25.pdf"))
files_ratio_bogota <- c(pm10 = here::here(dir_figures, "model2_bogota_scatter.pdf"),
                        pm25 = here::here(dir_figures, "model2_bogota_scatter_pm25.pdf"))

files_series_cdmx <- c(pm10 = here::here(dir_figures, "model2_mexico.pdf"),
                       pm25 = here::here(dir_figures, "model2_mexico_pm25.pdf"))
files_ratio_cdmx <- c(pm10 = here::here(dir_figures, "model2_mexico_scatter.pdf"),
                      pm25 = here::here(dir_figures, "model2_mexico_scatter_pm25.pdf"))

files_series_santiago <- c(pm10 = here::here(dir_figures, "model2_santiago.pdf"),
                           pm25 = here::here(dir_figures, "model2_santiago_pm25.pdf"))
files_ratio_santiago <- c(pm10 = here::here(dir_figures, "model2_santiago_scatter.pdf"),
    pm25 = here::here(dir_figures, "model2_santiago_scatter_pm25.pdf"))

files_series_sp <- c(pm10 = here::here(dir_figures, "model2_saopaulo.pdf"),
                     pm25 = here::here(dir_figures, "model2_saopaulo_pm25.pdf"))
files_ratio_sp <- c(pm10 = here::here(dir_figures, "model2_saopaulo_scatter.pdf"),
                    pm25 = here::here(dir_figures, "model2_saopaulo_scatter_pm25.pdf"))

# Read predictions and station summaries.
pred_bogota   <- arrow::read_parquet(file_pred_bogota)
pred_cdmx     <- arrow::read_parquet(file_pred_cdmx)
pred_santiago <- arrow::read_parquet(file_pred_santiago)
pred_sp       <- arrow::read_parquet(file_pred_sp)

station_bogota   <- arrow::read_parquet(file_station_bogota)
station_cdmx     <- arrow::read_parquet(file_station_cdmx)
station_santiago <- arrow::read_parquet(file_station_santiago)
station_sp       <- arrow::read_parquet(file_station_sp)

# Preserve the existing scale correction when education values exceed 100.
station_bogota   <- rescale_station_education(station_bogota, "Bogota")
station_cdmx     <- rescale_station_education(station_cdmx, "CDMX")
station_santiago <- rescale_station_education(station_santiago, "Santiago")
station_sp       <- rescale_station_education(station_sp, "Sao Paulo")

# ==========================================================================================
# II: Process data
# ==========================================================================================
# Keep results for both pollutants, with explicit city calls inside each loop.
ratios_bogota <- series_bogota <- plots_ratio_bogota <- list()
ratios_cdmx <- series_cdmx <- plots_ratio_cdmx <- list()
ratios_santiago <- series_santiago <- plots_ratio_santiago <- list()
ratios_sp <- series_sp <- plots_ratio_sp <- list()

pollutant <- imputation_pollutants[1]
for (pollutant in imputation_pollutants) {
  ratios_bogota[[pollutant]] <- summarize_imputation_ratios(pred_dt = pred_bogota,
      station_dt = station_bogota, pollutant = pollutant)
  ratios_cdmx[[pollutant]] <- summarize_imputation_ratios(pred_dt = pred_cdmx,
      station_dt = station_cdmx, pollutant = pollutant)
  ratios_santiago[[pollutant]] <- summarize_imputation_ratios(pred_dt = pred_santiago,
      station_dt = station_santiago, pollutant = pollutant)
  ratios_sp[[pollutant]] <- summarize_imputation_ratios(pred_dt = pred_sp,
      station_dt = station_sp, pollutant = pollutant)
}

for (pollutant in imputation_pollutants) {
  series_bogota[[pollutant]] <- plot_imputation_series(pred_dt = pred_bogota,
      pollutant = pollutant, city_label = "Bogotá")

  series_cdmx[[pollutant]] <- plot_imputation_series(pred_dt = pred_cdmx,
      pollutant = pollutant, city_label = "Mexico City")

  series_santiago[[pollutant]] <- plot_imputation_series(pred_dt = pred_santiago,
      pollutant = pollutant, city_label = "Santiago")

  series_sp[[pollutant]] <- plot_imputation_series(pred_dt = pred_sp,
      pollutant = pollutant, city_label = "São Paulo")
}

for (pollutant in imputation_pollutants) {
  plots_ratio_bogota[[pollutant]] <- plot_imputation_ratio_by_station(
      ratio_dt = ratios_bogota[[pollutant]], pollutant = pollutant,
      city_label = "Bogotá")

  plots_ratio_cdmx[[pollutant]] <- plot_imputation_ratio_by_station(
      ratio_dt = ratios_cdmx[[pollutant]], pollutant = pollutant,
      city_label = "Mexico City")

  plots_ratio_santiago[[pollutant]] <- plot_imputation_ratio_by_station(
      ratio_dt = ratios_santiago[[pollutant]], pollutant = pollutant,
      city_label = "Santiago")

  plots_ratio_sp[[pollutant]] <- plot_imputation_ratio_by_station(
      ratio_dt = ratios_sp[[pollutant]], pollutant = pollutant,
      city_label = "São Paulo")
}

# ==========================================================================================
# III: Save outputs
# ==========================================================================================
dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)

for (pollutant in imputation_pollutants) {
  ggplot2::ggsave(filename = files_series_bogota[[pollutant]],
                  plot = series_bogota[[pollutant]], width = 12, height = 8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")

  ggplot2::ggsave(filename = files_series_cdmx[[pollutant]],
                  plot = series_cdmx[[pollutant]], width = 12, height = 8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")

  ggplot2::ggsave(filename = files_series_santiago[[pollutant]],
                  plot = series_santiago[[pollutant]], width = 12, height = 8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")

  ggplot2::ggsave(filename = files_series_sp[[pollutant]],
                  plot = series_sp[[pollutant]], width = 12, height = 8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")
}

for (pollutant in imputation_pollutants) {
  ggplot2::ggsave(filename = files_ratio_bogota[[pollutant]],
                  plot = plots_ratio_bogota[[pollutant]], width = 8.5, height = 5.8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")

  ggplot2::ggsave(filename = files_ratio_cdmx[[pollutant]],
                  plot = plots_ratio_cdmx[[pollutant]], width = 8.5, height = 5.8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")

  ggplot2::ggsave(filename = files_ratio_santiago[[pollutant]],
                  plot = plots_ratio_santiago[[pollutant]], width = 8.5, height = 5.8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")

  ggplot2::ggsave(filename = files_ratio_sp[[pollutant]],
                  plot = plots_ratio_sp[[pollutant]], width = 8.5, height = 5.8,
                  dpi = 300, device = grDevices::cairo_pdf, limitsize = FALSE, bg = "white")
}
