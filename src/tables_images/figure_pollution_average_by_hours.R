# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Generate and save plots of hour-average PM 2.5 pollution level in four metro areas
# 
# @Description: This script aims o analyze each hour average (across the year) PM2.5 pollution 
# data (from MERRA-2 and ground stations) in the 4 Latin American cities, examining both mean 
# levels and the distribution of PM2.5 throughout the day.
# 
# @Summary: This program performs the following steps:
#   I.   Import and Load the processed time series data for each city from CSV files.
#   II.  Create bar plots and ridgeline plots for PM2.5 data using the custom functions 
#        'plot_hourly_avg_pollution' and 'plot_hourly_ridgeline_pollution'.
#   III. Save the generated plots as PDF files.
# 
# @Date: Mar 2025
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
# Apply function to generate a bar plot of pollution by hour for each city
bogota_bar        <- plot_hourly_avg_pollution(df = bogota_pm25,
                                               region_name = "Bogotá",
                                               plot_ci = TRUE,
                                               bar_width = 0.7)

santiago_bar      <- plot_hourly_avg_pollution(df = santiago_pm25,
                                               region_name = "Santiago",
                                               plot_ci = TRUE,
                                               bar_width = 0.7)

ciudad_mexico_bar <- plot_hourly_avg_pollution(df = ciudad_mexico_pm25,
                                               region_name = "Ciudad de México",
                                               plot_ci = TRUE,
                                               bar_width = 0.7)

sao_paulo_bar     <- plot_hourly_avg_pollution(df = sao_paulo_pm25,
                                               region_name = "São Paulo",
                                               plot_ci = TRUE,
                                               bar_width = 0.7)

# Apply function to create "Ridgeline" plots for each city and each pollution variable
bogota_ridge_plot        <- plot_hourly_ridgeline_pollution(df = bogota_pm25,
                                                            region_name = "Bogotá",
                                                            pollution_var = "pm25_stations")

santiago_ridge_plot      <- plot_hourly_ridgeline_pollution(df = santiago_pm25,
                                                            region_name = "Santiago",
                                                            pollution_var = "pm25_stations")

ciudad_mexico_ridge_plot <- plot_hourly_ridgeline_pollution(df = ciudad_mexico_pm25,
                                                            region_name = "Ciudad de México",
                                                            pollution_var = "pm25_stations")

sao_paulo_ridge_plot     <- plot_hourly_ridgeline_pollution(df = sao_paulo_pm25,
                                                            region_name = "São Paulo",
                                                            pollution_var = "pm25_stations")

# ============================================================================================
# II: Save data
# ============================================================================================

# Save bar plots
ggsave(
  filename = here::here("results", "figures", "hour_average", "bogota_bar_plot.pdf"),
  plot     = bogota_bar,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)

ggsave(
  filename = here::here("results", "figures", "hour_average", "santiago_bar_plot.pdf"),
  plot     = santiago_bar,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)

ggsave(
  filename = here::here("results", "figures", "hour_average", "ciudad_mexico_bar_plot.pdf"),
  plot     = ciudad_mexico_bar,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)

ggsave(
  filename = here::here("results", "figures", "hour_average", "sao_paulo_bar_plot.pdf"),
  plot     = sao_paulo_bar,
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)

# Save Ridgeline plots
ggsave(
  filename = here::here("results", "figures", "hour_average", "bogota_ridge_plot.pdf"),
  plot     = bogota_ridge_plot
  + labs(title = NULL)
  + theme(plot.title = element_blank()),
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)

ggsave(
  filename = here::here("results", "figures", "hour_average", "santiago_ridge_plot.pdf"),
  plot     = santiago_ridge_plot
  + labs(title = NULL)
  + theme(plot.title = element_blank()),
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)

ggsave(
  filename = here::here("results", "figures", "hour_average", "ciudad_mexico_ridge_plot.pdf"),
  plot     = ciudad_mexico_ridge_plot
  + labs(title = NULL)
  + theme(plot.title = element_blank()),
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)

ggsave(
  filename = here::here("results", "figures", "hour_average", "sao_paulo_ridge_plot.pdf"),
  plot     = sao_paulo_ridge_plot
  + labs(title = NULL)
  + theme(plot.title = element_blank()),
  device   = cairo_pdf,
  width    = 16, height = 9, dpi = 300
)
