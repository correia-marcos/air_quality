# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Visualize and compare PM2.5 time series data from MERRA-2 and ground station 
# measurements for selected Latin American cities.
# 
# @Description: This script imports processed time series data for MERRA-2 and ground-based
# PM2.5 measurements for four cities (Bogotá, Ciudad de México, Santiago,  and São Paulo). It
# then generates two sets of visualizations: one set using a rolling window (smoothed data) and
# another set with raw data. Each plot is annotated with the Pearson (or specified) correlation
# between the MERRA-2 and station series. Finally, the plots are saved as PDF files in the
# designated results folder.
# 
# @Summary: This program performs the following steps:
#   I.   Import and Load the processed time series data for each city.
#   II.  Generate time series plots for each city with both rolling and raw PM2.5 series 
#   III. Save the generated plots as PDF files.
# 
# @Date: Feb 2025
# @author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Open time-series dataframe of MERRA-2 and ground-based station measurements of PM 2.5
bogota_pm25        <- read.csv(here::here("data",
                                          "processed",
                                          "merra2_stations_pm25",
                                          "bogota_pm25_stations_merra2.csv"))
santiago_pm25      <- read.csv(here::here("data", 
                                          "processed",
                                          "merra2_stations_pm25",
                                          "santiago_pm25_stations_merra2.csv"))
ciudad_mexico_pm25 <- read.csv(here::here("data", 
                                          "processed",
                                          "merra2_stations_pm25",
                                          "ciudad_mexico_pm25_stations_merra2.csv"))
sao_paulo_pm25     <- read.csv(here::here("data", 
                                          "processed",
                                          "merra2_stations_pm25",
                                          "sao_paulo_pm25_stations_merra2.csv"))

# ============================================================================================
# II:Process data
# ============================================================================================
# Apply function to generate plot with rolling window
bogota_plot_series        <- plot_pm25_timeseries_smooth(
  df             = bogota_pm25,
  region_name    = "Bogota",
  apply_rolling  = TRUE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue")

santiago_plot_series      <- plot_pm25_timeseries_smooth(
  df             = santiago_pm25,
  region_name    = "Santiago",
  apply_rolling  = TRUE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue")

ciudad_mexico_plot_series <- plot_pm25_timeseries_smooth(
  df             = ciudad_mexico_pm25,
  region_name    = "Ciudad de México",
  apply_rolling  = TRUE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue")

sao_paulo_plot_series     <- plot_pm25_timeseries_smooth(
  df             = sao_paulo_pm25,
  region_name    = "São Paulo",
  apply_rolling  = TRUE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue")

# Apply function to generate plot with "raw" values (no moving windows)
bogota_raw_series <- plot_pm25_timeseries_smooth(
  df             = bogota_pm25,
  region_name    = "Bogota",
  apply_rolling  = FALSE,
  window_hours   = 24,       # This parameter is ignored when apply_rolling = FALSE
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue"
)

santiago_raw_series <- plot_pm25_timeseries_smooth(
  df             = santiago_pm25,
  region_name    = "Santiago",
  apply_rolling  = FALSE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue"
)

ciudad_mexico_raw_series <- plot_pm25_timeseries_smooth(
  df             = ciudad_mexico_pm25,
  region_name    = "Ciudad de México",
  apply_rolling  = FALSE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue"
)

sao_paulo_raw_series <- plot_pm25_timeseries_smooth(
  df             = sao_paulo_pm25,
  region_name    = "São Paulo",
  apply_rolling  = FALSE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue"
)

# ============================================================================================
# II: Save data
# ============================================================================================
# Save rolling window plots
ggsave(
  filename = here("results", "figures", "time_series", "bogota_moving_average_pm25.pdf"),
  plot     = bogota_plot_series,
  device   = cairo_pdf, 
  width    = 16, height = 9, dpi = 300
  )

ggsave(
  filename = here("results", "figures", "time_series", "ciudad_mexico_moving_average_pm25.pdf"),
  plot     = ciudad_mexico_plot_series,
  device   = cairo_pdf, 
  width    = 16, height = 9, dpi = 300
  )

ggsave(
  filename = here("results", "figures", "time_series", "santiago_moving_average_pm25.pdf"),
  plot     = santiago_plot_series,
  device   = cairo_pdf, 
  width    = 16, height = 9, dpi = 300
  )

ggsave(
  filename = here("results", "figures", "time_series", "sao_paulo_moving_average_pm25.pdf"), 
  plot     = sao_paulo_plot_series,
  device   = cairo_pdf, 
  width    = 16, height = 9, dpi = 300
  )

# Save raw plots
ggsave(
  filename = here("results", "figures", "time_series", "bogota_time_series_pm25.pdf"),
  plot     = bogota_raw_series,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
  )

ggsave(
  filename = here("results", "figures", "time_series", "ciudad_mexico_time_series_pm25.pdf"),
  plot     = ciudad_mexico_raw_series,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
  )

ggsave(
  filename = here("results", "figures", "time_series", "santiago_time_series_pm25.pdf"),
  plot     = santiago_raw_series,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
  )

ggsave(
  filename = here("results", "figures", "time_series", "sao_paulo_time_series_pm25.pdf"),
  plot     = sao_paulo_raw_series,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
  )
