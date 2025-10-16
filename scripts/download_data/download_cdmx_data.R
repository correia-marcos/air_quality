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

# Get all libraries and functions - config_utils_plot_tables to generate one LaTeX table
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "cdmx.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
mexico <- ne_states(country = "Mexico", returnclass = "sf")

# Show parameters imported on src/city_specific_cdmx.R
print(cdmx_cfg$id)
print(cdmx_cfg$years)
print(cdmx_cfg$dl_dir)
print(cdmx_cfg$out_dir)
print(cdmx_cfg$which_states)

# ============================================================================================
# II: Download data
# ============================================================================================
# Apply function to download shapefiles for Bogotá metro area
metro_area <- cdmx_download_metro_area(
  base_url          = cdmx_cfg$base_url_shp,
  keep_municipality = cdmx_cfg$cities_in_metro,
  download_dir      = here::here(cdmx_cfg$dl_dir,"metro_area"),
  overwrite_zip     = FALSE,
  overwrite_gpkg    = TRUE,
  quiet             = FALSE,
  out_file          = here::here(cdmx_cfg$out_dir, 
                                 "geospatial_data",
                                 "metro_areas",
                                 "cdmx_metro.gpkg"))

# Apply function to save a LaTeX table of the states that we must download stations data
table_states_to_download <- table_state_metro_distances(
  national_states_sf = mexico,
  metro_area_sf = metro_area,
  save_latex_table = TRUE,
  caption = "Administrative states and distance to metropolitan area (in Km)",
  out_file = here::here("results", "tables", "states_to_get_stations", "CDMX.tex"),
  overwrite_tex = TRUE
) # change cdmx_cfg$which_states if necessary! Depending on result

# Apply function to gen/save table with stations and their location - inside CDMX
station_in_cdmx <- cdmx_scrape_station_catalog(
  page_url      = cdmx_cfg$url_loc_stations_cdmx,
  out_dir       = here::here(cdmx_cfg$out_dir,
                             "geospatial_data",
                             "ground_stations",
                             "CDMX"),
  out_name      = "cdmx_station_location",
  write_parquet = FALSE,
  write_csv     = TRUE,
  write_rds     = FALSE,
  verbose       = TRUE)

# Apply function to gen/save remain stations location and information - out CDMX
all_stations <- cdmx_scrape_states_merge(
  station_in_cdmx = station_in_cdmx,
  base_url        = cdmx_cfg$url_loc_stations_others,
  states          = cdmx_cfg$which_states,
  out_dir         = here::here(cdmx_cfg$out_dir,
                               "geospatial_data",
                               "ground_stations",
                               "CDMX"),
  out_name        = "all_station_location",
  write_parquet   = FALSE,
  write_csv       = TRUE,
  write_rds       = FALSE,
  verbose         = TRUE)

# Apply function to create Selenium server and download the data for Mexico stations
download_logs_stations <- cdmx_download_sinaica_data(
  base_url                    = cdmx_cfg$base_url_sinaica,
  years                       = cdmx_cfg$years,
  container                   = TRUE,
  timeout_csv                 = 20,
  settle_before_csv_click_sec = 1,
  subdir                      = here::here(cdmx_cfg$dl_dir, "Ground_stations")
)

# Save the log of downloading for transparency - if at least 1000 downloads were made
if (nrow(download_logs_stations) >= 1000){
  write.csv(download_logs_stations, file = path(cdmx_cfg$dl_dir, "stations_log.csv"))
}

# Apply function to download 2023 pollution data for states missing this year
missing_data_stations <- cdmx_download_remaining_raw_sinaica(
  base_url         = cdmx_cfg$base_url_sinaica,
  subdir_existing  = here::here(cdmx_cfg$dl_dir, "Ground_stations"),
  out_subdir_raw   = here::here(cdmx_cfg$dl_dir, "Ground_stations_raw_missing_data"),
  year_check       = 2023L)

# Apply function to download Census data for the metro area
census_log <- cdmx_download_census_data(
  areas    = c("Ciudad de México", "Hidalgo", "México"),
  base_url = cdmx_cfg$base_url_census,
  out_dir  = here::here(cdmx_cfg$dl_dir, "census"),
  retries  = 5,
  quiet    = FALSE
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")