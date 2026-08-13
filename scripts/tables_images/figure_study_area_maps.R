# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Static context maps showing where the four metropolitan areas sit.
#
#' @Description: Two scales of the same orienting question. The continental map places
# Bogotá, Ciudad de México, Santiago and São Paulo within Latin America; the national map
# places the Mexico City metro area within Mexico and shades the eight states whose
# station data was downloaded, which is what makes the CDMX metro boundary legible. All
# outputs are static PDFs under results/figures/maps/; the interactive station map lives
# in figure_stations_on_metro_area.R.
#
#' @Summary:
#   I.   Import data: city and metro shapefiles, country and state boundaries.
#   II.  Continental map: the four cities within Latin America.
#   III. National map: the CDMX metro area within Mexico, plain and over a basemap.
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
dir_shapefiles <- here::here("data", "raw", "cities_shapefiles")
outdir_maps    <- here::here("results", "figures", "maps")

# Figure geometry, shared by every map here.
fig_width  <- 16
fig_height <- 9
fig_dpi    <- 300

# City metro boundaries for the continental map
bogota        <- sf::st_read(file.path(dir_shapefiles, "Bogota_metro"))
ciudad_mexico <- sf::st_read(file.path(dir_shapefiles, "Mexico_city"))
santiago      <- sf::st_read(file.path(dir_shapefiles, "Santiago"))
sao_paulo     <- sf::st_read(file.path(dir_shapefiles, "Sao_Paulo"))

# Country and state boundaries
north_america <- ne_countries(continent = "North America", returnclass = "sf")
south_america <- ne_countries(continent = "South America", returnclass = "sf")
mexico_states <- ne_states(country = "Mexico", returnclass = "sf")

# CDMX metro area under the current legal definition
cdmx_metro <- sf::st_read(here::here("data", "raw", "geospatial_data",
                                     "metro_areas", "cdmx_metro.gpkg"))

# The eight states whose monitoring data was downloaded for the CDMX metro area
states_downloaded_mexico <- c("Guerrero", "Hidalgo", "México", "Michoacán", "Morelos",
                              "Querétaro", "Puebla", "Tlaxcala")

dir.create(outdir_maps, recursive = TRUE, showWarnings = FALSE)

# ============================================================================================
# II: Continental map — the four cities within Latin America
# ============================================================================================
# Drop the three northern countries so the map frames Latin America, not the Americas.
countries_to_remove    <- c("Canada", "United States of America", "Greenland")
north_america_filtered <- north_america[!(north_america$admin %in% countries_to_remove), ]

latin_america <- rbind(south_america, north_america_filtered)

latin_america_map <- plot_latin_america_map(
  latin_america = latin_america,
  regions       = list(bogota, ciudad_mexico, santiago, sao_paulo),
  region_names  = c("Bogotá", "Ciudad de México", "Santiago", "São Paulo"),
  outline       = TRUE)

ggplot2::ggsave(file.path(outdir_maps, "latin_america_cities.pdf"),
                latin_america_map, device = cairo_pdf,
                width = fig_width, height = fig_height, dpi = fig_dpi)

# ============================================================================================
# III: National map — the CDMX metro area within Mexico
# ============================================================================================
# Two renderings of the same map: plain vector, and the same geometry over a terrain
# basemap for presentations. plot_metro_area_national_context() falls back to "sf" when
# ggmap is unavailable, so the second call degrades to the first rather than failing.
cdmx_context_plot <- plot_metro_area_national_context(
  national_states_sf = mexico_states,
  metro_area_sf      = cdmx_metro,
  which_states       = states_downloaded_mexico,
  map_mode           = "sf",
  city_name          = "Mexico City")

cdmx_context_stadia <- plot_metro_area_national_context(
  national_states_sf = mexico_states,
  metro_area_sf      = cdmx_metro,
  which_states       = states_downloaded_mexico,
  state_name_col     = "name",
  map_mode           = "ggmap",
  basemap_type       = "stamen_terrain_background",
  city_name          = "Mexico City")

ggplot2::ggsave(file.path(outdir_maps, "cdmx_metro_national_context.pdf"),
                cdmx_context_plot, device = cairo_pdf,
                width = fig_width, height = fig_height, dpi = fig_dpi)

ggplot2::ggsave(file.path(outdir_maps, "cdmx_metro_national_context_stadia.pdf"),
                cdmx_context_stadia, device = cairo_pdf,
                width = fig_width, height = fig_height, dpi = fig_dpi)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
