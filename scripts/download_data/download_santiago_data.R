# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Download all required data for the metro area of Santiago
#
#' @Description: Use the city acquisition functions to preserve source files.
# Download geographic sources, station measurements and census microdata.
#     1 - Georeferenced administrative data to construct the metro area of Santiago
#           Preserve source files; processing builds the metropolitan layers.
#     2 - All ground station air pollution data inside the metro area
#     3 - Census microdata for the country
#
#' @Summary:
#   I. Import data.
#   II. Download data.
#   III. Check acquisition outputs.
#
#' @Date: September 2025
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
# Load acquisition functions and plotting helpers for the source-region table.
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "city_specific", "registry.R"))
load_city_modules()

# Set the folder for preserved geographic sources.
dir_geography <- here::here(santiago_cfg$dl_dir, "metro_area")

# Load the boundaries of chilean states - rnaturalearth
chile <- ne_states(country = "Chile", returnclass = "sf")

# Show the settings from the city configuration.
print(santiago_cfg$which_states)

# ==========================================================================================
# II: Download data
# ==========================================================================================
# Acquire the complete 2017 response set and the national 2024 archive.
geography_sources_2017 <- santiago_download_geography_2017(
    base_url      = santiago_cfg$base_url_dpa_17,
    conurbacion   = "GRAN SANTIAGO",
    region_prefix = "13",
    download_dir  = here::here(dir_geography, "2017"))

geography_source_2024 <- santiago_download_geography_2024(
    base_url     = santiago_cfg$base_url_shp,
    download_dir = dir_geography)

# Prepare only the 2017 footprint used to review the station-source regions.
zonas_2017 <- santiago_prepare_metro_area_2017(
    metro_file = geography_sources_2017[["metro"]],
    zones_file = geography_sources_2017[["zonas"]],
    count_file = geography_sources_2017[["count"]])

# The optional alternative boundary has its own preparation recipe:
# scripts/process_data/prepare_santiago_alternative_geography.R.

# Save a LaTeX table to review which regions should supply station data.
table_states_to_download <- table_state_metro_distances(
  national_states_sf = chile,
  metro_area_sf = zonas_2017,
  save_latex_table = TRUE,
  caption = "Administrative states and distance to metropolitan area (in Km)",
  out_file = here::here("results", "tables", "station_source_regions_santiago.tex"),
  overwrite_tex = TRUE
) # Review santiago_cfg$which_states against this diagnostic.

# Use Selenium to download hourly station data and save the acquisition log.
logs_sinca_stations_hourly_data <- santiago_download_pollution(
  base_url = santiago_cfg$base_url_sinca,
  subdir   = file.path("santiago", "ground_stations"))
write.csv(logs_sinca_stations_hourly_data, row.names = FALSE,
          file = here::here(santiago_cfg$dl_dir, "log_sinca_ground_stations.csv"))

# Use Selenium to download the station metadata.
logs_sinca_stations_metadata    <- santiago_download_station_info(
  base_url = santiago_cfg$base_url_sinca,
  subdir   = file.path("santiago", "stations_metadata"))

# Download the 2017 census geographic files.
census_2017 <- santiago_download_census_data(
  type            = "geo_location",
  year            = 2017,
  download_folder = here::here(santiago_cfg$dl_dir, "census"))

# Download the 2024 census microdata.
census_2024 <- santiago_download_census_data(
  type = "people",
  year = 2024,
  download_folder = here::here(santiago_cfg$dl_dir, "census"))

# Preserve the package-managed census source before analytical filtering.
census_source <- santiago_acquire_census_2017()

# ==========================================================================================
# III: Check acquisition outputs
# ==========================================================================================
acquisition_results <- list(
  geography_sources_2017 = geography_sources_2017,
  geography_source_2024 = geography_source_2024,
  logs_sinca_stations_hourly_data = logs_sinca_stations_hourly_data,
  logs_sinca_stations_metadata = logs_sinca_stations_metadata,
  census_2017 = census_2017,
  census_2024 = census_2024,
  census_source = census_source)
str(acquisition_results, max.level = 1)
