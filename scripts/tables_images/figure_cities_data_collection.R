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
# Define states where data was collected
states_downloaded_mexico <- c("Guerrero", "Hidalgo", "México", "Michoacán", "Morelos",
                              "Querétaro", "Puebla", "Tlaxcala")

# Open metro and country areas
mexico       <- ne_states(country = "Mexico", returnclass = "sf")
cdmx         <- sf::st_read(here::here("data", "raw", "geospatial_data",
                                       "metro_areas", "cdmx_metro.gpkg"))
legacy_cdmx <- sf::st_read(here::here("data", "raw", "cities_shapefiles(old)", "Mexico_city"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function to generate plot of vector-only data
mexico_plot <- plot_metro_area_national_context(
  national_states_sf = mexico,
  metro_area_sf      = cdmx,
  which_states       = states_downloaded_mexico,
  map_mode           = "sf",
  city_name          = "Mexico City")

legacy_mexico_plot <- plot_metro_area_national_context(
  national_states_sf = mexico,
  metro_area_sf      = legacy_cdmx,
  which_states       = states_downloaded_mexico,
  map_mode           = "sf",
  city_name          = "Mexico City (old version)")

mexico_plot_stadia <- plot_metro_area_national_context(
  national_states_sf = mexico,
  metro_area_sf      = cdmx,
  which_states       = states_downloaded_mexico,
  state_name_col     = "name",
  map_mode           = "ggmap",
  basemap_type       = "stamen_terrain_background",
  city_name          = "Mexico City"
)

# ============================================================================================
# II: Save data
# ============================================================================================
# Ensure output folder exists
outdir <- here("results", "figures", "maps")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save plot
ggsave(here::here(outdir, "cdmx_metro_national_context.pdf"),
       mexico_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)

ggsave(here::here(outdir, "cdmx_metro_national_context_stadia.pdf"),
       mexico_plot_stadia,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")
