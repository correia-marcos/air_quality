# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Compare monthly PM2.5 estimates derived from MERRA-2 data (IDB estimation) with NASA’s 
# PM2.5 measurements for selected countries in South and North America.
# 
# @Description: This script imports raw MERRA-2 netCDF files, country shapefiles, and a CSV file
# containing NASA's monthly PM2.5 estimates. It then processes the MERRA-2 data by extracting 
# hourly aerosol variables over each region, converts the hourly data into daily PM2.5
# estimates, and aggregates these to monthly averages. Finally, it compares the monthly 
# estimates from MERRA-2 with NASA's data for each selected country.
# 
# @Summary: This program performs the following steps:
#   I.   Import necessary libraries, functions, and data
#   II.  Process data for each country (Filter the global shapefile; process MERRA-2 netCDF 
# files to generate hourly panels of aerosol data, convert the hourly aerosol data to PM2.5;
# aggregate the hourly data to daily and then monthly averages, among others)
#   III. Save processed dataset as CSV file
# 
# @Date: Feb 2025
# @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Create list of all raster files from MERRA2
nc_files.     <- list.files(here::here("data", "raw", "merra2_country_pm"),
                            pattern    = "\\.nc4$",
                            full.names = TRUE)

# Open Countries and continent shapefiles
south_america <- ne_countries(continent = "South America", returnclass = "sf")
north_america <- ne_countries(continent = "North America", returnclass = "sf")

# Open MERRA2 file of country wise monthly PM 2.5 concentration
nasa_dataset        <- here::here("data",
                                  "raw",
                                  "merra2_country_pm",
                                  "MERRA2.avgM_2d_pm25_admin0x.v01.19800101-20221231.csv")

nasa_pm25_countries <- read.csv(nasa_dataset, sep = ",", skip = 13)

# ============================================================================================
# II:Process data
# ============================================================================================
# Apply nested function to gen a dataframe comparing IDB MERRA-2 estimation and the one from 
# NASA dataset
brazil_comparison    <- generate_region_comparison(
  shapefile         = south_america,
  filter_field      = "sov_a3",      # or "admin" if you prefer
  filter_value      = "BRA",
  region_name       = "Brazil",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE
)

chile_comparison     <- generate_region_comparison(
  shapefile         = south_america,
  filter_field      = "sov_a3",
  filter_value      = "CHL",
  region_name       = "Chile",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE
)

colombia_comparison  <- generate_region_comparison(
  shapefile         = south_america,
  filter_field      = "sov_a3",      
  filter_value      = "COL",
  region_name       = "Colombia",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE
)

mexico_comparison    <- generate_region_comparison(
  shapefile         = north_america.,
  filter_field      = "sov_a3",      
  filter_value      = "MEX",
  region_name       = "Mexico",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE
)

# Join all data frames together
countries_comparison <- rbind(brazil_comparison,
                              chile_comparison,
                              colombia_comparison,
                              mexico_comparison)
# ============================================================================================
# III: Save data
# ============================================================================================
# Save processed dataframe
write.csv(countries_comparison,
          file      = here::here("data",
                                 "processed",
                                 "comparisons",
                                 "countries_comparison_month_idb_nasa_merra2.csv"),
          row.names = FALSE)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")