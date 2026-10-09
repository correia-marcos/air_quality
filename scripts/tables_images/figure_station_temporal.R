# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot observed hourly PM2.5 profiles and IT2 episode durations.
#
#' @Description: Read city-hour station means and preserve missing hours between episodes.
#
#' @Summary:
#   I.   Import data: source definitions and read hourly station checkpoints.
#   II.  Process data: retain four ridgelines and the IT2 episode plot.
#   III. Save outputs: preserve the five manuscript filenames.
#
#' @Date: October 2026
#' @Author: Project contributors
# ========================================================================================


# ========================================================================================
# I: Import data
# ========================================================================================
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "plot", "timeseries_hourly.R"))
source(here::here("src", "general_utilities", "process", "station_temporal.R"))
source(here::here("src", "general_utilities", "plot", "exposure_figures.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))
set_paper_theme()

dir_series <- here::here("data", "processed", "station_hourly")
file_episode_data <- here::here(dir_series,
  sprintf("episodes_it2_%d.csv", analysis_year))
outdir_fig <- here::here("results", "figures", "temporal")

series_bogota <- arrow::read_parquet(here::here(dir_series,
  sprintf("bogota_pm25_%d.parquet", analysis_year)))
series_cdmx <- arrow::read_parquet(here::here(dir_series,
  sprintf("cdmx_pm25_%d.parquet", analysis_year)))
series_santiago <- arrow::read_parquet(here::here(dir_series,
  sprintf("santiago_pm25_%d.parquet", analysis_year)))
series_sao_paulo <- arrow::read_parquet(here::here(dir_series,
  sprintf("sao_paulo_pm25_%d.parquet", analysis_year)))

# ========================================================================================
# II: Process data
# ========================================================================================
ridge_bogota <- plot_hourly_ridgeline_pollution(df = series_bogota,
  region_name = "Bogota", pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

ridge_cdmx <- plot_hourly_ridgeline_pollution(df = series_cdmx,
  region_name = "Ciudad de México", pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

ridge_santiago <- plot_hourly_ridgeline_pollution(df = series_santiago,
  region_name = "Santiago", pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

ridge_sao_paulo <- plot_hourly_ridgeline_pollution(df = series_sao_paulo,
  region_name = "São Paulo", pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

episodes_data_it2 <- dplyr::bind_rows(
  compute_time_spans_above_target(series_bogota, "Bogota", "IT2"),
  compute_time_spans_above_target(series_cdmx, "Ciudad de México", "IT2"),
  compute_time_spans_above_target(series_santiago, "Santiago", "IT2"),
  compute_time_spans_above_target(series_sao_paulo, "São Paulo", "IT2"))
episodes_it2 <- plot_episode_spans_ridgeline(episodes_data_it2, target = "IT2") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

# ========================================================================================
# III: Save outputs
# ========================================================================================
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

file_bogota <- save_plot_pdf(plot_obj = ridge_bogota,
  path = here::here(outdir_fig, "bogota_ridge_plot.pdf"),
  width = 16, height = 9, bg = NULL, limitsize = FALSE)

file_cdmx <- save_plot_pdf(plot_obj = ridge_cdmx,
  path = here::here(outdir_fig, "ciudad_mexico_ridge_plot.pdf"),
  width = 16, height = 9, bg = NULL, limitsize = FALSE)

file_santiago <- save_plot_pdf(plot_obj = ridge_santiago,
  path = here::here(outdir_fig, "santiago_ridge_plot.pdf"),
  width = 16, height = 9, bg = NULL, limitsize = FALSE)

file_sao_paulo <- save_plot_pdf(plot_obj = ridge_sao_paulo,
  path = here::here(outdir_fig, "sao_paulo_ridge_plot.pdf"),
  width = 16, height = 9, bg = NULL, limitsize = FALSE)

file_episodes <- save_plot_pdf(plot_obj = episodes_it2,
  path = here::here(outdir_fig, "distribution_hours_above_IT2.pdf"),
  width = 16, height = 9, bg = NULL, limitsize = FALSE)

file_episode_data <- write_station_episodes(episodes_data_it2, file_episode_data)
