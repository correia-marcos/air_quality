# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot hourly PM2.5 profiles and pollution-episode durations.
#
#' @Description: Read the prepared station/MERRA-2 series. Compare hourly averages,
# plot station-reading distributions by hour and summarize consecutive exceedances.
# Preserve the existing threshold definitions; bar-chart error bars show one standard
# error. The manuscript's ridgelines and episode plots are saved without titles.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read inputs.
#   II.  Process data: build hourly profiles and episode-duration plots.
#   III. Save outputs: save the results in the same order.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the scientific functions and their shared helpers
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "plot", "timeseries_hourly.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))

# Use the manuscript font and theme
set_paper_theme()

# Set the prepared time-series folder and figure destination
dir_series <- here::here("data", "processed", "merra2_stations_pm25")
outdir_fig <- here::here("results", "figures", "temporal")

# Read one merged hourly series per city
series_bogota   <- read.csv(here::here(dir_series,
                                       "bogota_pm25_stations_merra2.csv"))
series_santiago <- read.csv(here::here(dir_series,
                                     "santiago_pm25_stations_merra2.csv"))
series_cdmx     <- read.csv(here::here(dir_series,
                                         "ciudad_mexico_pm25_stations_merra2.csv"))
series_sp       <- read.csv(here::here(dir_series,
                                           "sao_paulo_pm25_stations_merra2.csv"))

# ========================================================================================
# II: Process data
# ========================================================================================
# Compare hourly means, then show the station-reading distribution by hour
bar_bogota <- plot_hourly_avg_pollution(df          = series_bogota,
                                        region_name = "Bogota",
                                        plot_ci     = TRUE,
                                        bar_width   = 0.7)

ridge_bogota <- plot_hourly_ridgeline_pollution(df            = series_bogota,
                                                region_name   = "Bogota",
                                                pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

bar_santiago <- plot_hourly_avg_pollution(df          = series_santiago,
                                          region_name = "Santiago",
                                          plot_ci     = TRUE,
                                          bar_width   = 0.7)

ridge_santiago <- plot_hourly_ridgeline_pollution(df            = series_santiago,
                                                  region_name   = "Santiago",
                                                  pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

bar_cdmx <- plot_hourly_avg_pollution(df          = series_cdmx,
                                      region_name = "Ciudad de México",
                                      plot_ci     = TRUE,
                                      bar_width   = 0.7)

ridge_cdmx <- plot_hourly_ridgeline_pollution(df            = series_cdmx,
                                              region_name   = "Ciudad de México",
                                              pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

bar_sp <- plot_hourly_avg_pollution(df          = series_sp,
                                    region_name = "São Paulo",
                                    plot_ci     = TRUE,
                                    bar_width   = 0.7)

ridge_sp <- plot_hourly_ridgeline_pollution(df            = series_sp,
                                            region_name   = "São Paulo",
                                            pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

# Compare episode durations across cities for each interim target
city_series <- list("Bogota" = series_bogota, "Santiago" = series_santiago,
                    "Ciudad de México" = series_cdmx, "São Paulo" = series_sp)
episodes_it1 <- plot_time_spans_ridgeline(list_of_dfs   = city_series,
                                          target        = "IT1",
                                          pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

episodes_it2 <- plot_time_spans_ridgeline(list_of_dfs   = city_series,
                                          target        = "IT2",
                                          pollution_var = "pm25_stations") +
  ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save each city's bar and ridge plots, followed by the two episode plots
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "bogota_bar_plot.pdf"),
                plot      = bar_bogota,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "bogota_ridge_plot.pdf"),
                plot      = ridge_bogota,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "santiago_bar_plot.pdf"),
                plot      = bar_santiago,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "santiago_ridge_plot.pdf"),
                plot      = ridge_santiago,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "ciudad_mexico_bar_plot.pdf"),
                plot      = bar_cdmx,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "ciudad_mexico_ridge_plot.pdf"),
                plot      = ridge_cdmx,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "sao_paulo_bar_plot.pdf"),
                plot      = bar_sp,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "sao_paulo_ridge_plot.pdf"),
                plot      = ridge_sp,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "distribution_hours_above_IT1.pdf"),
                plot      = episodes_it1,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "distribution_hours_above_IT2.pdf"),
                plot      = episodes_it2,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 9, dpi = 300, limitsize = FALSE)
