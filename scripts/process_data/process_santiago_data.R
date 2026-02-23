# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process all downloaded data from the metro area of Santiago ground stations. 
# The idea here is to transform the downloaded Santiago's metro area data - with all theirs 
# specificities into a format that is standard for all cities we assess in the project.
# 
# @Description: This script transforms raw monitoring, geospatial, and census data into a 
#   project-standard format. It includes: (1) Spatial filtering of ground stations within a 
#   20km metropolitan buffer; (2) Consolidation of SINCA raw measurements into 
#   Parquet format; (3) Extraction and harmonization of 2017 and 2024 Chilean Census microdata 
#   to integrate socio-economic indicators into the analysis.
# 
# @Summary: 
#   I.   Setup: Load dependencies, utility functions, and city-specific config.
#   II.  Import: Read raw geospatial boundaries and station location files.
#   III. Pollution: Filter stations by buffer and convert data to Parquet.
#   IV.  Census: Extract and harmonize the 2017 / 2024 microdata.
# 
# @Date: Oct 2025
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "santiago.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Define the output general folders
outdir_pollution  <- here::here(santiago_cfg$out_dir, "monitoring_stations")
outdir_geospatial <- here::here(santiago_cfg$out_dir, "geospatial_data")
outdir_metadata   <- here::here(santiago_cfg$dl_dir, "stations_metadata")

# Define the file's specific location
santiago_stations_csv   <- here::here(outdir_metadata,
                                      "SINCA_metadata_stations_20260113_1616.csv")
santiago_metro_gpkg     <- here::here(outdir_geospatial, "santiago",
                                      "gran_santiago_area_2024.gpkg")
# Open station location and other spatial data
station_location <- read.csv(santiago_stations_csv)
metro_area       <- sf::st_read(santiago_metro_gpkg)

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function to filter the stations in the metro area + 20 km radius
stations_kept <- santiago_filter_stations_in_metro(
  stations_df   = station_location,
  radius_km     = 20,
  metro_area    = metro_area,
  out_file      = here::here(outdir_geospatial, "santiago",
                             "gran_santiago_stations_buffer_metro_2024.gpkg"))

# Apply function to merge all downloaded file of Bogota metro area into DUCKDB database
santiago_stations_data <- santiago_process_stations_data_to_parquet(
  data_folder = here::here(santiago_cfg$dl_dir, "ground_stations"),
  stations_sf = stations_kept,
  out_dir     = outdir_pollution,
  out_name    = "santiago_metro",
  tz          = "UTC",
  years       = santiago_cfg$years
)

# Apply function to process the census data of 2017 - through a package
process_harmonize_census <- santiago_process_census_2017(
  out_dir    = here::here("data", "interim", "census", "santiago_2017"),
  sf_data    = metro_area,
  match_col  = "CUT",
  quiet      = FALSE)

# Apply function to unpack the census data of 2024 (unzip, filter, read and process)
process_harmonize_census_2024 <- santiago_process_census_2024(
  census_dir = here::here(santiago_cfg$dl_dir, "census", "2024"),
  sf_data    = metro_area,
  match_col  = "CUT",
  out_dir    = here::here("data", "interim", "census", "santiago_2024")
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")
