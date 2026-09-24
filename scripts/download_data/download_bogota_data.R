# =======================================================================================
# IDB: Air monitoring
# =======================================================================================
#' @Goal: Download all required data for the metro area of Bogota
#
#' @Description: Use Bogotá's acquisition functions to preserve source files.
# Download three groups of inputs:
#     1 - Geo-referenced administrative data to construct the metro area of Bogotá
#           Preserve national sources; the processing script builds the metro layers.
#     2 - All ground station air pollution data inside the metro area
#     3 - CENSUS microdata for the country
#
#' @Summary:
#   I. Import data.
#   II. Download data.
#   III. Check acquisition outputs.
#
#' @Date: August 2025
#' @Author: Marcos
# =======================================================================================

# =======================================================================================
# I: Import data
# =======================================================================================
# Get all libraries and functions - config_utils_plot_tables to generate one LaTeX table
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src","city_specific", "registry.R"))
load_city_modules()

# Set location of required folders
dir_geography       <- here::here(bogota_cfg$dl_dir, "metro_area")
file_municipalities <- here::here(dir_geography, "SHP_MGN2018_INTGRD_MPIO.zip")
file_localities     <- here::here(dir_geography, "bogota_loca.gpkg")

colombia <- ne_states(country = "Colombia", returnclass = "sf")

# Show parameters imported on src/city_specific_bogota.R
print(bogota_cfg$which_states)

# =======================================================================================
# II: Download data
# =======================================================================================
# Acquire the source archives and locality layer; processing saves the metro layers.
geography_sources <- bogota_download_geography(base_url     = bogota_cfg$base_url_shp,
                                               download_dir = dir_geography)

# Prepare only the footprint needed to review which station-source regions to acquire.
metro_area_2018 <- bogota_prepare_metro_area(
    source_zips        = file_municipalities,
    level              = "mpio_localidad",
    mgn_year           = 2018,
    municipality_codes = bogota_cfg$city_code_metro,
    localities_file    = file_localities)

# Apply function to save a LaTeX table of the states that we must download stations data
table_states_to_download <- table_state_metro_distances(
  national_states_sf = colombia,
  metro_area_sf      = metro_area_2018,
  save_latex_table   = TRUE,
  caption            = "Administrative states and distance to metropolitan area (in Km)",
  out_file           = here::here("results", "tables", "station_source_regions_bogota.tex"),
  overwrite_tex      = TRUE
) # Review bogota_cfg$which_states against this diagnostic if needed.

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

# Apply function to download the 2005 Census data for the metro area - Basic one
census_basico   <- bogota_download_census_data(
  year            = 2005,
  type            = "BASICO",
  url             = bogota_cfg$base_url_census,
  download_folder = here::here(bogota_cfg$dl_dir, "census"))

# Apply function to download the 2005 Census data for the metro area - Extended one
census_ampliado_2005 <- bogota_download_census_data(
  year            = 2005,
  type            = "AMPLIADO",
  url             = bogota_cfg$base_url_census,
  download_folder = here::here(bogota_cfg$dl_dir, "census"))

# Apply function to download the 2018 Census data for the metro area - Extended one
census_ampliado_2018 <- bogota_download_census_data(
  year            = 2018,
  url             = bogota_cfg$base_new_census,
  download_folder = here::here(bogota_cfg$dl_dir, "census"))

# =======================================================================================
# III: Check acquisition outputs
# =======================================================================================
acquisition_results <- list(
  geography_sources = geography_sources,
  download_logs_station_bogota = download_logs_station_bogota,
  download_logs_stations_metro_bogota = download_logs_stations_metro_bogota,
  census_basico = census_basico,
  census_ampliado_2005 = census_ampliado_2005,
  census_ampliado_2018 = census_ampliado_2018)
str(acquisition_results, max.level = 1)
