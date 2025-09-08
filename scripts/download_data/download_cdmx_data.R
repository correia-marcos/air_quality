# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Download all required data for the metro area of the Ciudad de México (CDMX)
# 
# @Description: This functions uses previous created functions on city specific src  utilities 
# to download all the necessary data for the project. It is based on download three major 
# sources:
#     1 - Geo-referenced administrative data to construct the metro area of CDMX
#           obs: This function download data for the country and then filter metro area
#     2 - All ground station air pollution data inside the metro area
#     3 - CENSUS microdata for the country
# 
# @Summary: 
#   I.   Load all the sources in the correct order (order matters) for functions and variables
#   II.  Use the named list in city_specific/cdmx.R to define parameters for functions
#   III. Download all data (metro area -> stations -> census)
# 
# @Date: August 2025
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "cdmx.R"))

# ============================================================================================
# I: Download data
# ============================================================================================
# Show parameters imported on src/city_specific_cdmx.R
print(cdmx_cfg$id)
print(cdmx_cfg$years)
print(cdmx_cfg$dl_dir)
print(cdmx_cfg$out_dir)

# Apply function to download shapefiles for Bogotá metro area
metro_area <- cdmx_download_metro_area(
  base_url          = cdmx_cfg$base_url_shp,
  keep_municipality = cdmx_cfg$cities_in_metro,
  download_dir      = here::here(cdmx_cfg$dl_dir, "metro_area"),
  out_file          = here::here(cdmx_cfg$out_dir, "cities_shapefiles", "cdmx_metro.gpkg"),
  overwrite_zip     = FALSE,
  overwrite_gpkg    = TRUE,
  quiet             = FALSE
)

# Apply function to generate and save dataframe with stations and their location
station_in_cdmx <- cdmx_scrape_station_catalog(
  page_url      = cdmx_cfg$url_loc_stations_cdmx,
  out_dir       = here::here(cdmx_cfg$out_dir, "pollution_ground_stations", "Mexico_city"),
  out_name      = "cdmx_station_location",
  write_parquet = FALSE,   # or FALSE if you don’t want a file
  write_csv     = TRUE,
  write_rds     = FALSE
)

# Apply function to create Selenium server and download the data for Bogota
cdmx_download_sinaica_data(
  base_url      = cdmx_cfg$base_url_sinaica,
  years         = cdmx_cfg$years,
  container     = TRUE,
  max_attempts  = 5,
  timeout_page  = 300,
  timeout_dl    = 600,
  subdir        = here::here(cdmx_cfg$dl_dir, "Ground_stations")
)



# Apply function to download Census data for the metro area
census <- bogota_download_census_data(
  type            = "AMPLIADO",
  url             = bogota_cfg$base_url_census,
  download_folder = here::here(bogota_cfg$dl_dir, "census")
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")