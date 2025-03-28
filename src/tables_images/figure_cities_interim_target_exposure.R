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
#   II.  .
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
# II: Process data
# ============================================================================================
# Apply a function to calculate episodes of pollution higher than an IT (either IT1 or IT2)
# This is just for visual check
bogota_episodes        <- compute_time_spans_above_target(
  df            = bogota_pm25,
  city_name     = "bogota",
  target        = "IT2",
  pollution_var = "pm25_stations")

santiago_episodes      <- compute_time_spans_above_target(
  df            = santiago_pm25,
  city_name     = "santiago",
  target        = "IT2",
  pollution_var = "pm25_stations")

ciudad_mexico_episodes <- compute_time_spans_above_target(
  df            = ciudad_mexico_pm25,
  city_name     = "ciudad_mexico",
  target        = "IT2",
  pollution_var = "pm25_stations")

sao_paulo_episodes     <- compute_time_spans_above_target(
  df            = sao_paulo_pm25,
  city_name     = "sao_paulo",
  target        = "IT2",
  pollution_var = "pm25_stations")

# Create a named list of data frames
city_dfs <- list(
  "Bogotá"          = bogota_pm25,
  "Santiago"        = santiago_pm25,
  "Ciudad de México"= ciudad_mexico_pm25,
  "São Paulo"       = sao_paulo_pm25)

# Generate ridgeline for IT1 using the "pm25_stations" column
plot_time_spans_it2 <- plot_time_spans_ridgeline(
  list_of_dfs   = city_dfs,
  target        = "IT2",
  pollution_var = "pm25_stations")

plot_time_spans_it1 <- plot_time_spans_ridgeline(
  list_of_dfs   = city_dfs,
  target        = "IT1",
  pollution_var = "pm25_stations")

plot_time_spans_it2
plot_time_spans_it1

# ============================================================================================
# II: Save data
# ============================================================================================
# Save plot of time span distribution of high pollution episodes
ggsave(filename = here("results",
                       "figures",
                       "hour_above_iterim_target",
                       "distribution_hours_above_IT1.pdf"),
       plot     = plot_time_spans_it1 +
         labs(title = NULL) +
         theme(plot.title = element_blank()),
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

ggsave(filename = here("results",
                       "figures",
                       "hour_above_iterim_target",
                       "distribution_hours_above_IT2.pdf"),
       plot     = plot_time_spans_it2 +
         labs(title = NULL) +
         theme(plot.title = element_blank()),
       device   = cairo_pdf,
       width    = 16, 
       height   = 9,
       dpi      = 300)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")