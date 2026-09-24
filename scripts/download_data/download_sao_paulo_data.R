# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Download all required data for the metro area of São Paulo
#
#' @Description: Use the city acquisition functions to preserve source files.
# Download geographic sources, station measurements and census microdata.
#     1 - Georeferenced administrative data to construct the metro area of São Paulo
#           Preserve source files; processing builds the metropolitan layers.
#     2 - All ground station air pollution data inside the metro area
#     3 - Census microdata for the country
#
#' @Summary:
#   I. Import data.
#   II. Download data.
#   III. Check acquisition outputs.
#
#' @Date: November 2025
#' @Author: Marcos
# ==========================================================================================

# Load acquisition functions and plotting helpers for the source-region table.
# ==========================================================================================
# I: Import data
# ==========================================================================================
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "city_specific", "registry.R"))
load_city_modules()

# Set the folder for preserved geographic sources.
dir_geography <- here::here(sao_paulo_cfg$dl_dir, "metro_area")

# Load the boundaries of Brazilian states - rnaturalearth
brazil <- rnaturalearth::ne_states(country = "Brazil", returnclass = "sf")

# Show the settings from the city configuration.
print(sao_paulo_cfg$which_states)

# ==========================================================================================
# II: Download data
# ==========================================================================================
# Acquire the 2010 municipality and census-tract archives.
municipality_source <- sao_paulo_download_geography(
    level        = "mpio",
    base_url     = sao_paulo_cfg$base_url_shp,
    download_dir = dir_geography)

tract_source <- sao_paulo_download_geography(level        = "setor_censitario",
                                             base_url     = sao_paulo_cfg$base_url_shp,
                                             download_dir = dir_geography)

# Preserve the full weighting-area source; processing selects the metro subset.
weighting_source <- sao_paulo_download_weighting_areas(year         = 2010,
                                                      download_dir = dir_geography)

# Prepare only the footprint used to review the station-source regions.
sao_paulo_metro_2010 <- sao_paulo_prepare_metro_area(
    source_zip        = municipality_source,
    level             = "mpio",
    keep_municipality = sao_paulo_cfg$cities_in_metro)

# Save a LaTeX table to review which regions should supply station data.
table_states_to_download <- table_state_metro_distances(
  national_states_sf = brazil,
  metro_area_sf = sao_paulo_metro_2010,
  save_latex_table = TRUE,
  caption = "Administrative states and distance to metropolitan area (in Km)",
  out_file = here::here("results", "tables", "station_source_regions_sao_paulo.tex"),
  overwrite_tex = TRUE)

# Use Selenium to download hourly station data and save the acquisition log.
logs_qualar_stations_hourly_data <- sao_paulo_download_pollution(
  base_url = sao_paulo_cfg$base_url_qualar,
  subdir   = file.path("sao_paulo", "ground_stations"),
  years    = sao_paulo_cfg$years)
write.csv(logs_qualar_stations_hourly_data, row.names = FALSE,
          file = here::here(sao_paulo_cfg$dl_dir, "log_qualar_ground_stations.csv"))

# Use Selenium to download the station metadata.
logs_qualar_stations_metadata <- sao_paulo_download_metadata(
  base_url   = sao_paulo_cfg$base_url_qualar,
  search_url = sao_paulo_cfg$metadata_url_qualar,
  out_file   = here::here(sao_paulo_cfg$dl_dir,
                          "stations_metadata", "stations_metadata.csv"))

# Preserve the package-managed census source before analytical filtering.
census_source <- sao_paulo_acquire_census_2010()

# ==========================================================================================
# III: Check acquisition outputs
# ==========================================================================================
acquisition_results <- list(
  municipality_source = municipality_source,
  tract_source = tract_source,
  weighting_source = weighting_source,
  logs_qualar_stations_hourly_data = logs_qualar_stations_hourly_data,
  logs_qualar_stations_metadata = logs_qualar_stations_metadata,
  census_source = census_source)
str(acquisition_results, max.level = 1)
