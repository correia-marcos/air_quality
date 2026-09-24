# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Download all required data for the metro area of the Ciudad de México (CDMX)
#
#' @Description: Use the city-specific functions to acquire three groups of sources:
#     1 - Georeferenced administrative data to construct the metro area of CDMX
#           Preserve source files; processing builds the metropolitan layers.
#     2 - All ground station air pollution data inside the metro area
#     3 - Census microdata for the country
#
#' @Summary:
#   I. Import data.
#   II. Download data.
#   III. Check acquisition outputs.
#
#' @Date: August 2025
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
# Load acquisition functions and plotting helpers for the source-region table.
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src","city_specific", "registry.R"))
load_city_modules()

# Set the folder for preserved geographic sources.
dir_geography <- here::here(cdmx_cfg$dl_dir, "metro_area")

# Load the boundaries of mexico states - rnaturalearth
mexico <- ne_states(country = "Mexico", returnclass = "sf")

# Show the settings from the city configuration.
print(cdmx_cfg$which_states)

# ==========================================================================================
# II: Download data
# ==========================================================================================
# Acquire the integrated archive; processing prepares and saves the metro layers.
geography_source <- cdmx_download_geography(base_url     = cdmx_cfg$base_url_shp,
                                            download_dir = dir_geography)

# Prepare only the footprint used to review the station-source regions.
metro_area <- cdmx_prepare_metro_area(source_zip        = geography_source,
                                      level             = "municipality",
                                      keep_municipality = cdmx_cfg$cities_in_metro)

# Save a LaTeX table to review which regions should supply station data.
table_states_to_download <- table_state_metro_distances(
  national_states_sf = mexico,
  metro_area_sf = metro_area,
  save_latex_table = TRUE,
  caption = "Administrative states and distance to metropolitan area (in Km)",
  out_file = here::here("results", "tables", "station_source_regions_CDMX.tex"),
  overwrite_tex = TRUE
) # Review cdmx_cfg$which_states against this diagnostic.

# Read and save the catalog of stations inside CDMX.
station_in_cdmx <- cdmx_scrape_station_catalog(
  page_url      = cdmx_cfg$url_loc_stations_cdmx,
  out_dir       = here::here(cdmx_cfg$dl_dir, "ground_stations_geolocation"),
  out_name      = "cdmx_station_location",
  write_parquet = FALSE,
  write_csv     = TRUE,
  write_rds     = FALSE,
  verbose       = TRUE)

# Add station locations from the other configured states.
all_stations <- cdmx_scrape_states_merge(
  station_in_cdmx = station_in_cdmx,
  base_url        = cdmx_cfg$url_loc_stations_others,
  states          = cdmx_cfg$which_states,
  out_dir         = here::here(cdmx_cfg$dl_dir, "ground_stations_geolocation"),
  out_name        = "all_station_location",
  write_parquet   = FALSE,
  write_csv       = TRUE,
  write_rds       = FALSE,
  verbose         = TRUE)

# Use Selenium to download the configured years of station measurements.
download_logs_stations <- cdmx_download_sinaica_data(
  base_url                    = cdmx_cfg$base_url_sinaica,
  years                       = cdmx_cfg$years,
  container                   = TRUE,
  timeout_csv                 = 20,
  settle_before_csv_click_sec = 1,
  subdir                      = here::here(cdmx_cfg$dl_dir, "Ground_stations")
)

# Save the acquisition log once it contains at least 1,000 entries.
if (nrow(download_logs_stations) >= 1000){
  write.csv(download_logs_stations, file = path(cdmx_cfg$dl_dir, "stations_log.csv"))
}

# Download 2023 pollution data for states missing that year.
missing_data_stations <- cdmx_download_remaining_raw_sinaica(
  base_url         = cdmx_cfg$base_url_sinaica,
  subdir_existing  = here::here(cdmx_cfg$dl_dir, "Ground_stations"),
  out_subdir_raw   = here::here(cdmx_cfg$dl_dir, "Ground_stations_raw_missing_data"),
  year_check       = 2023L)

# Download the census archives covering the metro area.
census_log <- cdmx_download_census_data(
  areas    = c("Ciudad de México", "Hidalgo", "México"),
  base_url = cdmx_cfg$base_url_census,
  out_dir  = here::here(cdmx_cfg$dl_dir, "census"),
  retries  = 5,
  quiet    = FALSE
)

# ==========================================================================================
# III: Check acquisition outputs
# ==========================================================================================
acquisition_results <- list(
  geography_source = geography_source,
  all_stations = all_stations,
  download_logs_stations = download_logs_stations,
  missing_data_stations = missing_data_stations,
  census_log = census_log)
str(acquisition_results, max.level = 1)
