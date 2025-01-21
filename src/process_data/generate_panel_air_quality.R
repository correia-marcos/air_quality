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
# Check RAM usage and define if the machine can use parallelization
ram_gb <- memuse::Sys.meminfo()[1]$totalram@size
use_parallel <- (ram_gb > 30)

# Apply function to crop data for each city and generate a panel - based on memory size
if (use_parallel) {
  bogota_results <- process_merra2_city_hourly_parallel(
    shapefile = bogota,
    nc_files = nc_files,
    city_name = "Bogotá",
    num_cores = NULL)
  
  ciudad_mexico_results <- process_merra2_city_hourly_parallel(
    shapefile = ciudad_mexico,
    nc_files = nc_files,
    city_name = "Ciudad de México",
    num_cores = NULL)
  
  santiago_results <- process_merra2_city_hourly_parallel(
    shapefile = santiago,
    nc_files = nc_files,
    city_name = "Santiago",
    num_cores = NULL)
  
  sao_paulo_results <- process_merra2_city_hourly_parallel(
    shapefile = sao_paulo,
    nc_files = nc_files,
    city_name = "São Paulo",
    num_cores = NULL)
} else {
  bogota_results <- process_merra2_city_hourly(
    shapefile = bogota,
    nc_files = nc_files,
    city_name = "Bogotá")
  
  ciudad_mexico_results <- process_merra2_city_hourly(
    shapefile = ciudad_mexico,
    nc_files = nc_files,
    city_name = "Ciudad de México")
  
  santiago_results <- process_merra2_city_hourly(
    shapefile = santiago,
    nc_files = nc_files,
    city_name = "Santiago")
  
  sao_paulo_results <- process_merra2_city_hourly(
    shapefile = sao_paulo,
    nc_files = nc_files,
    city_name = "São Paulo")
}

# ============================================================================================
# III: Save data
# ============================================================================================
# Save processed dataframes
write.csv(bogota_results,
          file = here::here("data", "processed", "bogota_panel.csv"),
          row.names = FALSE)
write.csv(ciudad_mexico_results,
          file = here::here("data", "processed", "ciudad_mexico_panel.csv"), 
          row.names = FALSE)
write.csv(santiago_results,
          file = here::here("data", "processed", "santiago_panel.csv"),
          row.names = FALSE)
write.csv(sao_paulo_results,
          file = here::here("data", "processed", "sao_paulo_panel.csv"), 
          row.names = FALSE)
