# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Download all required data for the metro area of Santiago
# 
# @Description: This functions uses previous created functions on city specific src  utilities 
# to download all the necessary data for the project. It is based on download three major 
# sources:
#     1 - Geo-referenced administrative data to construct the metro area of Santiago
#           obs: This function download data for the country and then filter metro area
#     2 - All ground station air pollution data inside the metro area
#     3 - CENSUS microdata for the country
# 
# @Summary: 
#   I.   Load all the sources in the correct order (order matters) for functions and variables
#   II.  Use the named list in city_specific/santiago.R to define parameters for functions
#   III. Download all data (metro area -> stations -> census)
# 
# @Date: September 2025
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions - config_utils_plot_tables to generate one LaTeX table
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "santiago.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
chile <- ne_states(country = "Chile", returnclass = "sf")

# Show parameters imported on src/city_specific_santiago.R
print(santiago_cfg$base_url_shp)
print(santiago_cfg$years)
print(santiago_cfg$dl_dir)
print(santiago_cfg$out_dir)

# ============================================================================================
# I: Download data
# ============================================================================================
# Apply function to download shapefiles for Gran Santiago area
gran_santiago <- santiago_download_metro_area(
  type              = "gran_santiago",
  base_url          = santiago_cfg$base_url_shp,
  keep_municipality = santiago_cfg$cities_in_metro,
  download_dir      = here::here(santiago_cfg$dl_dir, "metro_area"),
  out_file          = here::here(santiago_cfg$out_dir, "geospatial_data",
                                 "santiago", "gran_santiago_area.gpkg"),
  overwrite_zip     = FALSE,
  container         = TRUE,
  overwrite_gpkg    = TRUE,
  quiet             = FALSE
)

# Apply function to download shapefiles for Santiago metro area
santiago_metro <- santiago_download_metro_area(
  type              = "metro_santiago",
  base_url          = santiago_cfg$base_url_shp,
  keep_municipality = santiago_cfg$cities_in_metro,
  download_dir      = here::here(santiago_cfg$dl_dir, "metro_area"),
  out_file          = here::here(santiago_cfg$out_dir, "geospatial_data",
                                 "santiago", "santiago_metro_area.gpkg"),
  overwrite_zip     = FALSE,
  container         = TRUE,
  overwrite_gpkg    = TRUE,
  quiet             = FALSE
)

# Apply function to save a LaTeX table of the states that we must download stations data
table_states_to_download <- table_state_metro_distances(
  national_states_sf = chile,
  metro_area_sf = santiago_metro,
  save_latex_table = TRUE,
  caption = "Administrative states and distance to metropolitan area (in Km)",
  out_file = here::here("results", "tables", "states_to_get_stations", "santiago.tex"),
  overwrite_tex = TRUE
) # change cdmx_cfg$which_states if necessary! Depending on result


# Apply function to generate and save dataframe with stations and their location in Bogota


# Apply function to download excel files with stations and their geo-location metro area
logs_sisaire_metadata_boundary <- sisaire_download_department_metadata(
  base_url     = bogota_cfg$base_url_sisaire,
  timeout_page = 25,
  subdir       = file.path("bogota", "stations_metadata"))

# Apply function to create Selenium server and download the data for Bogota

  
# Apply function to create Selenium server and download the data for metro bogota


# Apply function to download Census data for the metro area
census <- bogota_download_census_data(
  type            = "AMPLIADO",
  url             = bogota_cfg$base_url_census,
  download_folder = here::here(bogota_cfg$dl_dir, "census"))

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")