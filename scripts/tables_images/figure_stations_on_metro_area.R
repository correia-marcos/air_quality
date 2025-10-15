# ============================================================================================
# IDB: Air monitoring - NEED TO UPDATE!!!!!
# ============================================================================================
# @Goal: Visualize the location of four metropolitan areas on Latin American
#  NEED TO UPDATE!!!!!
# @Description: This script creates a high-resolution map of Latin America, highlighting the 
# metropolitan areas of Bogotá, Ciudad de México, Santiago, and São Paulo. 
# NEED TO UPDATE!!!!!
# @Summary: This program performs the following steps:
#   I.   Import shapefiles for city boundaries and continents NEED TO UPDATE!!!!!
#   II.  Process the data to exclude unnecessary regions and combine shapefiles
#   III. Create and export a map of Latin America with the highlighted cities
# NEED TO UPDATE!!!!!
# @Date: Sep 2025
# @author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define the location of datasets
dir_cdmx_stations_data   <- here::here("data", "raw","air_monitoring_stations",
                                       "cdmx_metro_buffer_stations_dataset")
dir_cdmx_area            <- here::here("data", "raw", "geospatial_data",
                                       "metro_areas", "cdmx_metro.gpkg")
dir_cdmx_station_loc     <- here::here("data", "interim", "spatial_filtered_stations",
                                       "CDMX_stations.gpkg")
# Open air pollution dataframes
cdmx_stations_data <- arrow::open_dataset(dir_cdmx_stations_data)

# Open spatial data
cdmx              <- sf::st_read(dir_cdmx_area)
stations_in_metro <- sf::st_read(dir_cdmx_station_loc)

# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function to generate interactive plot for CDMX
# 1) No filter; color by entity
cdmx_all_stations_entity_scheme <- plot_metro_area_interactive(
  metro_area_sf = cdmx,
  stations_sf   = stations_in_metro,
  filter_type   = "none",
  color_scheme  = "entity",
  city_name     = "Mexico City"
)

# 2) Keep stations with PM2.5 OR PM10 anywhere; color by entity
cdmx_has_pm_stations_entity_scheme <- plot_metro_area_interactive(
  metro_area_sf = cdmx,
  stations_sf   = stations_in_metro,
  pollution_ds  = cdmx_stations_data,
  filter_type   = "has_pm_any",
  color_scheme  = "entity",
  city_name     = "Mexico City"
)

# ============================================================================================
# II: Save data
# ============================================================================================
# Ensure output folder exists
outdir <- here("results", "figures", "maps_interactive")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save plot
htmlwidgets::saveWidget(
  cdmx_all_stations_entity_scheme,
  here::here(outdir, "cdmx_all_stations_by_entity.html"),
  selfcontained = TRUE)

htmlwidgets::saveWidget(
  cdmx_has_pm_stations_entity_scheme,
  here::here(outdir, "cdmx_pm10_pm25_stations_by_entity.html"),
  selfcontained = TRUE)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")
