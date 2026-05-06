# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process all downloaded data from the metro area of Ciudad de México ground stations. 
# The idea here is to transform the initial data CMDX's metro area - with all theirs specifics
# into a format that is standard for all cities we assess in the project.
# 
# @Description: This script transforms raw monitoring, geospatial, and census data into a 
#   project-standard format. It includes: (1) Spatial filtering of ground stations within a 
#   20km metropolitan buffer; (2) Consolidation of SIMAT and SINAICA raw measurements into 
#   Parquet format; (3) Extraction and harmonization of 2020 Mexican Census microdata (
#   Extended) to integrate socio-economic indicators into the analysis.
# 
# @Summary: 
#   I.   Setup: Load dependencies, utility functions, and city-specific config.
#   II.  Import: Read raw geospatial boundaries and station location files.
#   III. Pollution: Filter stations by buffer and convert data to Parquet.
#   IV.  Census: Extract and harmonize the Extended 2020 microdata.
# 
# @Date: Oct 2025
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "cdmx.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Define the output general folders
outdir_pollution  <- here::here(cdmx_cfg$out_dir, "monitoring_stations")
outdir_geospatial <- here::here(cdmx_cfg$out_dir, "geospatial_data")
outdir_stations   <- here::here(cdmx_cfg$dl_dir, "ground_stations_geolocation")

# Define the file's specific location
all_station_csv   <- here::here(outdir_stations, "all_station_location.csv")
cdmx_metro        <- here::here(outdir_geospatial, "cdmx", "cdmx_area_metro.gpkg")

# Open station location and other spatial data
station_location <- read.csv(all_station_csv)
metro_area       <- sf::st_read(cdmx_metro)

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function to filter the stations in the metro area + 20 km radius
stations_kept <- cdmx_filter_stations_in_metro(
  station_location = station_location,
  metro_area       = metro_area,
  radius_km        = 20,             # change if needed
  dissolve         = TRUE,
  verbose          = TRUE,
  out_file         = here::here(outdir_geospatial, "cdmx", "cdmx_stations_buffer_metro.gpkg"))

# Apply function to merge all downloaded file into a single tidy dataframe
cdmx_stations_data <- cdmx_merge_pollution_data(
  primary_data_dir     = here::here(cdmx_cfg$dl_dir, "ground_stations"),
  secondary_data_dir   = here::here(cdmx_cfg$dl_dir, "ground_stations_raw_missing_data"),
  stations_sf          = stations_kept,
  tz                   = "UTC",
  years                = cdmx_cfg$years,
  out_dir              = outdir_pollution,
  out_name             = "cdmx_metro",
)

# Apply function to unpack the census data (unzip and filter) then read and process
process_cdmx_census <- mexico_filter_census(
  census_dir = here::here(cdmx_cfg$dl_dir, "census"),
  out_dir    = here::here("data", "raw", "census", "cdmx", "CPV2020_EXTENDED"),
  overwrite  = FALSE,
  quiet      = FALSE
)
process_harmonize_census <- mexico_harmonize_census_data(
  extract_index = process_cdmx_census,
  metro_codes  = cdmx_cfg$cities_in_metro,
  out_dir      = here::here("data", "interim", "census", "cdmx_extended_2020"))

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")