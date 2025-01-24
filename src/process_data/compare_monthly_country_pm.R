# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Enhance panel datasets with PM2.5 conversion and save finalized outputs
# 
# @Description: This script processes previously prepared panel datasets for Bogotá, Ciudad de 
# México, Santiago, and São Paulo by applying a function to calculate and add a new column 
# for PM2.5 concentrations. The processed datasets are saved for further analysis.
# 
# @Summary: This program performs the following steps:
#   I.   Import panel datasets for the 4 cities
#   II.  Apply a function to add a PM2.5 variable to each dataset
#   III. Save the finalized datasets in a new directory
# 
# @Date: Nov 2024
# @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Create list of all raster files from MERRA2
nc_files       <- list.files(here::here("data", "raw", "merra2"), full.names = TRUE)

# Open Countries and continent shapefiles
south_america  <- ne_countries(continent = "South America", returnclass = "sf")

# Open MERRA2 file of country wise monthly PM 2.5 concentration
raster_country <- rast(here::here("data", "raw", "merra2_test", "mask_worldCountry_MERRA2.nc"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Check RAM usage and define if the machine can use parallelization
ram_gb <- memuse::Sys.meminfo()[1]$totalram@size
use_parallel <- (ram_gb > 30)


