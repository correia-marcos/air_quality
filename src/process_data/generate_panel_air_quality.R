# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process and extract atmospheric aerosol data for the 4 cities in the study
# 
# @Description: This script applies a pre-defined function to process MERRA-2 aerosol data 
# over the metropolitan areas of Bogota, Ciudad de Mexico, Santiago, and Sao Paulo. 
# It uses city-specific shapefiles to crop the data and produces panel datasets for 
# further analysis. The output includes time-series data at hourly resolution, segmented 
# by city, for key aerosol variables.
# 
# @Summary: This program performs the following steps:
#   I.   Import necessary libraries, functions, and data
#   II.  Process MERRA-2 atmospheric data for each city's region using shapefiles
#   III. Save processed datasets as CSV files for further analysis
# 
# @Date: Nov 2024
# @Author: Marcos Paulo
# ============================================================================================

# Get all required libraries and functions
source(here::here("src", "config", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Create list of all raster files from MERRA2
nc_files  <- list.files(here::here("data", "raw","merra2"), full.names = TRUE)

# Open city shapefiles
bogota               <- sf::st_read(here::here("data", "raw", "cities", "Bogota_metro"))
ciudad_mexico        <- sf::st_read(here::here("data", "raw", "cities", "Mexico_city"))
santiago             <- sf::st_read(here::here("data", "raw", "cities", "Santiago")) 
sao_paulo            <- sf::st_read(here::here("data", "raw", "cities", "Sao_Paulo"))

# ============================================================================================
# II: Process data
# ============================================================================================

# Apply function to crop data for each city and generate a panel - parallel possible
bogota_results <- process_merra2_region_hourly(
  shapefile = bogota,
  nc_files = nc_files,
  region_name = "Bogotá",
  num_cores = NULL,
  extraction_fun = "mean",
  parallel = TRUE)

ciudad_mexico_results <- process_merra2_city_hourly_parallel(
  shapefile = ciudad_mexico,
  nc_files = nc_files,
  region_name = "Ciudad de México",
  num_cores = NULL,
  extraction_fun = "mean",
  parallel = TRUE)

santiago_results <- process_merra2_city_hourly_parallel(
  shapefile = santiago,
  nc_files = nc_files,
  region_name = "Santiago",
  num_cores = NULL,
  extraction_fun = "mean",
  parallel = TRUE)

sao_paulo_results <- process_merra2_city_hourly_parallel(
  shapefile = sao_paulo,
  nc_files = nc_files,
  region_name = "São Paulo",
  num_cores = NULL,
  extraction_fun = "mean",
  parallel = TRUE)

# ============================================================================================
# III: Save data
# ============================================================================================
# Save processed dataframes
write.csv(bogota_results,
          file = here::here("data", "interim", "bogota_panel.csv"),
          row.names = FALSE)
write.csv(ciudad_mexico_results,
          file = here::here("data", "interim", "ciudad_mexico_panel.csv"), 
          row.names = FALSE)
write.csv(santiago_results,
          file = here::here("data", "interim", "santiago_panel.csv"),
          row.names = FALSE)
write.csv(sao_paulo_results,
          file = here::here("data", "interim", "sao_paulo_panel.csv"), 
          row.names = FALSE)
