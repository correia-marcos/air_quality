# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Map the population density of each metropolitan area with its active stations.
#
#' @Description: Draws people per square kilometre by geographic unit for the four cities,
# with the monitoring stations that reported PM10 or PM2.5 in 2023 marked on top. Reads
# the geographic boundaries, the collapsed census and the raw hourly panels (the last only
# to decide which stations were active) and writes one figure per city under the name the
# manuscript cites. Santiago uses the 2017 zonas censales, the main specification.
#
#' @Summary:
#   I.   Setup: load dependencies, set the paper theme, define paths.
#   II.  Read the geographic, census and station inputs.
#   III. Draw and save one map per city.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Setup
# ============================================================================================
# Define input and output folders
dir_geospatial <- here::here("data", "raw", "geospatial_data")
dir_census     <- here::here("data", "interim", "census")
dir_raw        <- here::here("data", "raw", "monitoring_stations")
outdir_maps    <- here::here("results", "paper", "figures", "maps")

dir.create(outdir_maps, recursive = TRUE, showWarnings = FALSE)

# Figure geometry shared by the four maps.
fig_width  <- 8
fig_height <- 8
fig_dpi    <- 300

# Define geographic boundary files
gpkg_bogota   <- here::here(dir_geospatial, "bogota",
                            "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_cdmx     <- here::here(dir_geospatial, "cdmx",
                            "cdmx_area_metro_municipalities_2024.gpkg")
gpkg_santiago <- here::here(dir_geospatial, "santiago",
                            "gran_santiago_zonas_2017.gpkg")
gpkg_sp       <- here::here(dir_geospatial, "sao_paulo",
                            "sao_paulo_metro_2010_weighting_areas.gpkg")

# Define station spatial files
gpkg_stations_bogota   <- here::here(dir_geospatial, "bogota",
                                     "bogota_2018_stations_buffer_metro.gpkg")
gpkg_stations_cdmx     <- here::here(dir_geospatial, "cdmx",
                                     "cdmx_stations_buffer_metro.gpkg")
gpkg_stations_santiago <- here::here(dir_geospatial, "santiago",
                                     "gran_santiago_stations_buffer_metro_2017.gpkg")
gpkg_stations_sp       <- here::here(dir_geospatial, "sao_paulo",
                                     "sao_paulo_stations_buffer_metro_2010.gpkg")

# Define collapsed census paths
census_bogota_pq   <- here::here(dir_census, "bogota_2018",
                                 "census_2018_metro_collapsed.parquet")
census_cdmx_pq     <- here::here(dir_census, "cdmx_extended_2020",
                                 "collapse_metro_area_2020.parquet")
census_santiago_pq <- here::here(dir_census, "santiago_2017",
                                 "census_collapsed_2017.parquet")
census_sp_pq       <- here::here(dir_census, "sao_paulo_2010",
                                 "census_sp_collapsed_2010.parquet")

# Define raw Arrow dataset paths, read only to find the active stations
arrow_bogota   <- here::here(dir_raw, "bogota_metro_dataset")
arrow_cdmx     <- here::here(dir_raw, "cdmx_metro_dataset")
arrow_santiago <- here::here(dir_raw, "santiago_metro_dataset")
arrow_sp       <- here::here(dir_raw, "sao_paulo_metro_dataset")

# ============================================================================================
# II: Read spatial and census data
# ============================================================================================
geo_bogota   <- sf::st_read(gpkg_bogota, quiet = TRUE)
geo_cdmx     <- sf::st_read(gpkg_cdmx, quiet = TRUE)
geo_santiago <- sf::st_read(gpkg_santiago, quiet = TRUE)
geo_sp       <- sf::st_read(gpkg_sp, quiet = TRUE)

stations_bogota   <- sf::st_read(gpkg_stations_bogota, quiet = TRUE)
stations_cdmx     <- sf::st_read(gpkg_stations_cdmx, quiet = TRUE)
stations_santiago <- sf::st_read(gpkg_stations_santiago, quiet = TRUE)
stations_sp       <- sf::st_read(gpkg_stations_sp, quiet = TRUE)

census_bogota   <- arrow::read_parquet(census_bogota_pq)
census_cdmx     <- arrow::read_parquet(census_cdmx_pq)
census_santiago <- arrow::read_parquet(census_santiago_pq)
census_sp       <- arrow::read_parquet(census_sp_pq)

# ============================================================================================
# III: Draw and save the maps
# ============================================================================================
map_bogota <- plot_population_density_map(
  metro_sf    = geo_bogota,
  stations_sf = stations_bogota,
  arrow_dir   = arrow_bogota,
  census_df   = census_bogota,
  join_sf_col = "GEO_ID",
  join_df_col = "geo_id",
  station_col = "station_name",
  city_label  = "Bogotá")

ggplot2::ggsave(file.path(outdir_maps, "bogota_population_density_map.pdf"),
                plot = map_bogota, device = cairo_pdf,
                width = fig_width, height = fig_height, dpi = fig_dpi)

map_cdmx <- plot_population_density_map(
  metro_sf    = geo_cdmx,
  stations_sf = stations_cdmx,
  arrow_dir   = arrow_cdmx,
  census_df   = census_cdmx,
  join_sf_col = "CVE_MUN",
  join_df_col = "geo_id",
  station_col = "station",
  city_label  = "Mexico City")

ggplot2::ggsave(file.path(outdir_maps, "mexico_population_density_map.pdf"),
                plot = map_cdmx, device = cairo_pdf,
                width = fig_width, height = fig_height, dpi = fig_dpi)

map_santiago <- plot_population_density_map(
  metro_sf    = geo_santiago,
  stations_sf = stations_santiago,
  arrow_dir   = arrow_santiago,
  census_df   = census_santiago,
  join_sf_col = "zona_id",
  join_df_col = "geo_id",
  station_col = "station_name",
  city_label  = "Santiago")

ggplot2::ggsave(file.path(outdir_maps, "santiago_population_density_map.pdf"),
                plot = map_santiago, device = cairo_pdf,
                width = fig_width, height = fig_height, dpi = fig_dpi)

map_sp <- plot_population_density_map(
  metro_sf    = geo_sp,
  stations_sf = stations_sp,
  arrow_dir   = arrow_sp,
  census_df   = census_sp,
  join_sf_col = "code_weighting",
  join_df_col = "geo_id",
  station_col = "station_name",
  city_label  = "São Paulo")

ggplot2::ggsave(file.path(outdir_maps, "saopaulo_population_density_map.pdf"),
                plot = map_sp, device = cairo_pdf,
                width = fig_width, height = fig_height, dpi = fig_dpi)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
