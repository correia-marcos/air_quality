# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Assess and compare exposure to PM2.5 air pollution in four major Latin American
# metropolitan areas by comparing satellite-derived (MERRA-2) PM2.5 estimates with ground-based 
# station measurements.
# 
# @Description: This script combines pre-processed MERRA-2 PM2.5 data with ground station
# measurements for Bogotá, Ciudad de México, Santiago, and São Paulo. It then computes 
# correlations between these data sources at various time scales (hourly, daily, and monthly) to
# evaluate the consistency of pollution estimates.
# 
# @Summary: This program performs the following steps:
#   I.   Import necessary libraries, functions, and data
#   II.  Process data by merging MERRA-2 and ground station PM2.5 measurements for each city
#   III. Compute correlations between the two sources at different time scales and save them
# 
# @Date: Feb 2025
# @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Open station measure pollution data created previously by the IDB team
bogota_stations         <- readRDS(here::here("data",
                                              "raw",
                                              "pollution_station_measure",
                                              "Bogota",
                                              "pollution_pm10_pm25_data_balanced_2023.rds"))
ciudad_mexico_stations  <- readRDS(here::here("data",
                                              "raw",
                                              "pollution_station_measure",
                                              "Mexico_city",
                                              "pollution_pm25_data_balanced_2023.rds"))
santiago_stations       <- readRDS(here::here("data",
                                              "raw",
                                              "pollution_station_measure",
                                              "Santiago",
                                              "pollution_data_balanced_2023_pm25.rds"))
sao_paulo_stations      <- readRDS(here::here("data",
                                              "raw",
                                              "pollution_station_measure",
                                              "Sao_paulo",
                                              "pollution_data_balanced_2023_pm25.rds"))

# Open panel data for each city
bogota_merra2           <- read.csv(here::here("data",
                                               "processed",
                                               "merra2_pm25",
                                               "bogota_pm25.csv"))
ciudad_mexico_merra2    <- read.csv(here::here("data",
                                               "processed",
                                               "merra2_pm25",
                                               "ciudad_mexico_pm25.csv"))
santiago_merra2         <- read.csv(here::here("data",
                                               "processed",
                                               "merra2_pm25",
                                               "santiago_pm25.csv"))
sao_paulo_merra2        <- read.csv(here::here("data",
                                               "processed",
                                               "merra2_pm25",
                                               "sao_paulo_pm25.csv"))

# Open shapefile with the location of ground-based monitoring stations in Sao Paulo metro area
stations_in_sp_metro    <- sf::st_read(here::here("data",
                                                  "raw",
                                                  "cities",
                                                  "Sao_Paulo_metro_stations")) 

# ============================================================================================
# II:Process data
# ============================================================================================
# Filter values in Sao Paulo station data frame that are inside metro area
sao_paulo_stations <- sao_paulo_stations %>%
  filter(station_code %in% stations_in_sp_metro$sttn_cd)

# Apply function to combine MERRA-2 and ground station information of PM 2.5 levels
bogota_pollution        <- combine_station_merra2_pm25(
  station_df            = bogota_stations,
  station_datetime_col  = "datetime",
  station_pm25_col      = "pm25",
  merra2_df             = bogota_merra2)

ciudad_mexico_pollution <- combine_station_merra2_pm25(
  station_df            = ciudad_mexico_stations,
  station_datetime_col  = "datetime",
  station_pm25_col      = "pm25",
  merra2_df             = ciudad_mexico_merra2)

santiago_pollution      <- combine_station_merra2_pm25(
  station_df            = santiago_stations,
  station_datetime_col  = "date2_hour",
  station_pm25_col      = "pm25_validated",
  merra2_df             = santiago_merra2)

sao_paulo_pollution     <- combine_station_merra2_pm25(
  station_df            = sao_paulo_stations,
  station_datetime_col  = "datetime",
  station_pm25_col      = "pm25",
  merra2_df             = sao_paulo_merra2)

# Create a named list of merged data frames
city_pollution_list <- list(
  Bogota = bogota_pollution,
  "Ciudad_Mexico" = ciudad_mexico_pollution,
  Santiago = santiago_pollution,
  "Sao_Paulo" = sao_paulo_pollution)

# Compute correlations for each city at hourly, daily, and monthly time scales
correlation_results <- compute_correlations_for_cities(city_pollution_list)

# ============================================================================================
# II:Save data
# ============================================================================================
# Save processed dataframes
write.csv(correlation_results,
          file = here::here("data", "processed", "comparisons",
                            "correlation_pm25_stations_merra2.csv"),
          row.names = FALSE)

write.csv(bogota_pollution,
          file = here::here("data", "processed", "merra2_stations_pm25",
                            "bogota_pm25_stations_merra2.csv"),
          row.names = FALSE)

write.csv(ciudad_mexico_pollution,
          file = here::here("data", "processed",  "merra2_stations_pm25",
                            "ciudad_mexico_pm25_stations_merra2.csv"),
          row.names = FALSE)

write.csv(santiago_pollution,
          file = here::here("data", "processed",  "merra2_stations_pm25",
                            "santiago_pm25_stations_merra2.csv"),
          row.names = FALSE)

write.csv(sao_paulo_pollution,
          file = here::here("data", "processed",  "merra2_stations_pm25",
                            "sao_paulo_pm25_stations_merra2.csv"),
          row.names = FALSE)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")