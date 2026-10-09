# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Inspect historical CDMX station membership in interactive maps.
#' @Description: Reads the historical input paths below and saves two HTML widgets.
# These paths need a separate provenance review; the script is outside the manuscript.
# This cleanup preserves the existing sample and does not replace missing inputs.
#
#' @Summary:
#   I. Import data.
#   II. Process data.
#   III. Save data.
#
#' @Date: Sep 2025
#' @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
# ============================================================================================
# I: Import data
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Define the location of datasets
dir_cdmx_stations_data   <- here::here("data", "raw","air_monitoring_stations",
                                       "cdmx_metro_buffer_stations_dataset")
dir_cdmx_area            <- here::here("data", "interim", "geospatial_data",
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
# III: Save data
# ============================================================================================
# Ensure output folder exists
outdir <- here::here("results", "figures", "maps")
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
