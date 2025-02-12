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
nc_files          <- list.files(here::here("data", "raw", "merra2"), full.names = TRUE)

# Open Countries and continent shapefiles
south_america     <- ne_countries(continent = "South America", returnclass = "sf")

# Open MERRA2 file of country wise monthly PM 2.5 concentration
nasa_dataset      <- here::here("data",
                                "raw",
                                "merra2_country_pm",
                                "MERRA2.avgM_2d_pm25_admin0x.v01.19800101-20221231.csv")
nasa_pm_country   <- read.csv(nasa_dataset, sep = ",", skip = 13)

# ============================================================================================
# II: Process data
# ============================================================================================
# Check RAM usage and define if the machine can use parallelization
ram_gb           <- memuse::Sys.meminfo()[1]$totalram@size
use_parallel     <- (ram_gb > 30)

# Filter the dataframe for Brazil
brazil_shapefile <- south_america %>% 
  dplyr::filter(sov_a3 == "BRA")

# Decide parallel or not, then process MERRA-2
if (use_parallel) {
  cat("System has more than 30GB RAM -> using parallel processing.\n")
  brazil_results  <- process_merra2_city_hourly_parallel(
    shapefile      = brazil_shapefile,
    nc_files       = nc_files,
    city_name      = "Brazil",
    num_cores      = NULL,  # or specify a number
    extraction_fun = "mean"
    )
} else {
  cat("System has 30GB RAM or less -> using serial processing.\n")
  brazil_results  <- process_merra2_city_hourly(
    shapefile      = brazil_shapefile,
    nc_files       = nc_files,
    city_name      = "Brazil",
    extraction_fun = "mean"
    )
}

# Apply function to generate a new column with PM 2.5 measure
brazil_pm25       <- convert_and_add_pm25(brazil_results)

# Compare PM 2.5 estimation with the one calculated by NASA
brazil_comparison <- compare_pm25_to_nasa(user_pm_data       = brazil_pm25,
                                          nasa_monthly_data  = nasa_pm_country,
                                          country_name       = "Brazil")
