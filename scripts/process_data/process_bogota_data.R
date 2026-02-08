# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process and standardize air quality and census data for Bogota.
# 
# @Description: This script transforms raw monitoring, geospatial, and census data into a 
#   project-standard format. It includes: (1) Spatial filtering of ground stations within a 
#   20km metropolitan buffer; (2) Consolidation of RMCAB and SISAIRE raw measurements into 
#   Parquet format; (3) Extraction and harmonization of 2005 Colombian Census microdata (Basic 
#   and Extended) to integrate socio-economic indicators into the analysis.
# 
# @Summary: 
#   I.   Setup: Load dependencies, utility functions, and city-specific config.
#   II.  Import: Read raw geospatial boundaries and station location files.
#   III. Pollution: Filter stations by buffer and convert data to Parquet.
#   IV.  Census: Extract and harmonize both Basic and Extended 2005 microdata.
# 
# @Date: January 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "bogota.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Define the output general folders
outdir_pollution  <- here::here(bogota_cfg$out_dir, "monitoring_stations")
outdir_geospatial <- here::here(bogota_cfg$out_dir, "geospatial_data")
outdir_stations   <- here::here(bogota_cfg$dl_dir, "ground_stations_geolocation")
outdir_metadata   <- here::here(bogota_cfg$dl_dir, "stations_metadata")

# Define the file's specific location
bogota_stations_csv   <- here::here(outdir_stations, "bogota_stations_location.csv")
bogota_metro_gpkg     <- here::here(outdir_geospatial, "bogota", "bogota_area_metro_2018.gpkg")

# Read the geospatial data
stations_bogota   <- read.csv(bogota_stations_csv)
bogota_metro      <- st_read(bogota_metro_gpkg)

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function to filter the stations in the metro area + 20 km radius
stations_kept <- bogota_filter_stations_in_metro(
  stations_df_1 = stations_bogota,
  metadata_dir  = outdir_metadata,
  radius_km     = 20,
  metro_area    = bogota_metro,
  out_file      = here::here(outdir_geospatial, "bogota",
                             "bogota_2018_stations_buffer_metro.gpkg"))

# Apply function to merge all downloaded file of Bogota metro area into DUCKDB database
bogota_stations_data <- bogota_process_stations_data_to_parquet(
  rmcab_folder   = here::here(bogota_cfg$dl_dir, "ground_stations"),
  sisaire_folder = here::here(bogota_cfg$dl_dir, "metro_ground_stations_hourly"),
  stations_sf    = stations_kept,
  tz             = "UTC",    # Important to be UTC so DuckDB don't misbehaves
  out_dir        = outdir_pollution,
  out_name       = "bogota_metro"
)

# Apply function to unpack the extended census data (unzip and filter) then read and process
process_extended <- bogota_filter_census(
  census_zip = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip"),
  out_dir    = here::here("data", "raw", "census", "bogota", "CG2005_EXTENDED"),
  overwrite  = TRUE,
  quiet      = FALSE)
process_harmonize_extended <- bogota_harmonize_census_data(
  extract_list = process_extended,
  metro_codes  = bogota_cfg$city_code_metro,
  out_dir      = here::here("data", "interim", "census", "bogota_extended"))

# Apply function to unpack the basic census data (unzip and filter) then read and process
process_basic <- bogota_filter_census(
  census_zip = here::here(bogota_cfg$dl_dir, "census", "CG2005_BASICO.zip"),
  out_dir    = here::here("data", "raw", "census", "bogota", "CG2005_BASIC"),
  overwrite  = FALSE,
  quiet      = FALSE
)
process_harmonize_basic <- bogota_harmonize_census_data(
  extract_list = process_basic,
  is_extended  = FALSE,
  metro_codes  = bogota_cfg$city_code_metro,
  out_dir      = here::here("data", "interim", "census", "bogota_basic"))

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")
