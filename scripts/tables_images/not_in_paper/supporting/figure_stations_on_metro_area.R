# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Interactive maps of the CDMX monitoring stations around the metro area.
#
#' @Description: Reads the CDMX metro municipalities, the stations within its buffer and the
# standardized hourly station panel (2000-2023, before cleaning). Draws two leaflet maps:
# every station, and the stations with any PM10 or PM2.5 reading. Stations are matched
# to the panel by name, because CDMX's code ATI belongs to two stations. Saves two
# self-contained HTML files in results/figures/not_in_paper/supporting/.
#
#' @Summary:
#   I.   Import data: source functions, declare paths, read the geography and panel.
#   II.  Process data: draw the two maps.
#   III. Save outputs: save the maps as HTML.
#
#' @Date: November 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Set the source files and the output folder.
dir_cdmx_geo    <- here::here("data", "interim", "geospatial_data", "cdmx")
file_cdmx_metro <- here::here(dir_cdmx_geo, "cdmx_area_metro_municipalities_2024.gpkg")
file_stations   <- here::here(dir_cdmx_geo, "cdmx_stations_buffer_metro.gpkg")
dir_panel       <- here::here("data", "interim", "monitoring_stations",
                              "cdmx_metro_dataset")
outdir          <- here::here("results", "figures", "not_in_paper", "supporting")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Read the metro area's municipalities and the stations within its buffer.
cdmx_metro    <- sf::st_read(file_cdmx_metro, quiet = TRUE)
cdmx_stations <- sf::st_read(file_stations, quiet = TRUE)

# Connect to the hourly panel on disk.
cdmx_panel <- arrow::open_dataset(dir_panel)

# ========================================================================================
# II: Process data
# ========================================================================================
# Draw every station, coloured by state.
cdmx_all_stations_map <- plot_metro_area_interactive(metro_area_sf = cdmx_metro,
                                                     stations_sf   = cdmx_stations,
                                                     filter_type   = "none",
                                                     color_scheme  = "entity",
                                                     city_name     = "Mexico City")

# Keep the stations with any PM10 or PM2.5 reading in the panel, coloured by state.
cdmx_pm_stations_map <- plot_metro_area_interactive(metro_area_sf = cdmx_metro,
                                                    stations_sf   = cdmx_stations,
                                                    pollution_ds  = cdmx_panel,
                                                    filter_type   = "has_pm_any",
                                                    color_scheme  = "entity",
                                                    city_name     = "Mexico City")

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save both maps as self-contained HTML files. saveWidget() inlines the JavaScript
# libraries (pandoc, bundled with RStudio) and then removes its temporary lib folder,
# which it finds only by an absolute path.
htmlwidgets::saveWidget(cdmx_all_stations_map,
                        here::here(outdir, "cdmx_all_stations_by_entity.html"),
                        selfcontained = TRUE,
                        libdir        = here::here(outdir, "lib"))

htmlwidgets::saveWidget(cdmx_pm_stations_map,
                        here::here(outdir, "cdmx_pm10_pm25_stations_by_entity.html"),
                        selfcontained = TRUE,
                        libdir        = here::here(outdir, "lib"))
