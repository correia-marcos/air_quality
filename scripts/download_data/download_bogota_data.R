# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Download all required data for the metro area of Bogota
# 
# @Description: This functions uses previous created functions on city specific src  utilities 
# to download all the necessary data for the project. It is based on download three major 
# sources:
#     1 - Geo-referenced administrative data to construct the metro area of Bogotá
#           obs: This function download data for the country and then filter metro area
#     2 - All ground station air pollution data inside the metro area
#     3 - CENSUS microdata for the country
# 
# @Summary: 
#   I.   Load all the sources in the correct order (order matters) for functions and variables
#   II.  Use the named list in city_specific/bogota.R to define parameters for functions
#   III. Download all data (metro area -> stations -> census)
# 
# @Date: August 2025
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions - config_utils_plot_tables to generate one LaTeX table
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "bogota.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
colombia <- ne_states(country = "Colombia", returnclass = "sf")

# Show parameters imported on src/city_specific_bogota.R
print(bogota_cfg$base_url_shp)
print(bogota_cfg$base_url_census)
print(bogota_cfg$base_url_rmcab)
print(bogota_cfg$url_station_shp)
print(bogota_cfg$years)
print(bogota_cfg$dl_dir)
print(bogota_cfg$out_dir)

# ============================================================================================
# I: Download data
# ============================================================================================
# Apply function to download shapefiles for Bogotá metro area
metro_area <- bogota_download_metro_area(
  level             = "mpio",
  base_url          = bogota_cfg$base_url_shp,
  keep_municipality = bogota_cfg$cities_in_metro,
  download_dir      = here::here(bogota_cfg$dl_dir, "metro_area"),
  out_file          = here::here("data", "raw", "geospatial_data", "bogota",
                                 "bogota_area_metro.gpkg"),
  overwrite_zip     = FALSE,
  overwrite_gpkg    = TRUE,
  quiet             = FALSE
  )

# Apply function to save a LaTeX table of the states that we must download stations data
table_states_to_download <- table_state_metro_distances(
  national_states_sf = colombia,
  metro_area_sf = metro_area,
  save_latex_table = TRUE,
  caption = "Administrative states and distance to metropolitan area (in Km)",
  out_file = here::here("results", "tables", "states_to_get_stations", "bogota.tex"),
  overwrite_tex = TRUE
) # change cdmx_cfg$which_states if necessary! Depending on result

# Apply function to generate and save dataframe with stations and their location in Bogota
rmcab_dir <- bogota_scrape_rmcab_station_table(
  page_url      = bogota_cfg$url_station_shp,
  parse_coords  = TRUE,
  harmonize_map = bogota_cfg$station_nme_map,
  dedupe        = TRUE,
  verbose       = TRUE,
  out_dir       = here::here(bogota_cfg$dl_dir, "ground_stations_geolocation"),
  out_name      = "bogota_stations_location",
  write_csv     = TRUE,
)

# Apply function to download excel files with stations and their geo-location metro area
logs_sisaire_metadata_boundary <- sisaire_download_department_metadata(
  base_url     = bogota_cfg$base_url_sisaire,
  timeout_page = 25,
  subdir       = file.path("bogota", "stations_metadata"))

# Apply function to create Selenium server and download the data for Bogota
download_logs_station_bogota <- bogota_download_station_data(
  base_url      = bogota_cfg$base_url_rmcab,
  start_year    = min(bogota_cfg$years),
  end_year      = max(bogota_cfg$years),
  timeout_page  = 30,
  timeout_btn   = 30,
  timeout_dl    = 400,
  subdir        = file.path("bogota", "ground_stations")
)

# Apply function to create Selenium server and download the data for metro bogota
download_logs_stations_metro_bogota <- sisaire_download_hourly_data(
  base_url     = bogota_cfg$base_url_sisaire,
  target_depts = bogota_cfg$which_states,
  years_range  = bogota_cfg$years,
  subdir       = file.path("bogota", "metro_ground_stations_hourly"))

write.csv(download_logs_stations_metro_bogota,
          file = here::here(bogota_cfg$dl_dir, "metro_stations_log.csv"),
          row.names = FALSE)

# Apply function to download Census data for the metro area
census <- bogota_download_census_data(
  type            = "AMPLIADO",
  url             = bogota_cfg$base_url_census,
  download_folder = here::here(bogota_cfg$dl_dir, "census"))

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")