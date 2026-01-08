# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process all downloaded data from the Bogota city ground stations. 
# The idea here is to transform the initial data Bogota's metro area - with all theirs specifics
# into a format that is standard for all cities we assess in the project.
# 
# @Description: 
# 
# @Summary: 
#   I.   Load libraries, utility functions and necessary data
#   II.  
#   III. 
# 
# @Date: May 2025
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
bogota_metro_gpkg   <- here::here(outdir_geospatial, "bogota", "bogota_area_metro.gpkg")
bogota_stations_csv <- here::here(outdir_stations, "bogota_stations_location.csv")

# Read the geospatial data
bogota_metro      <- st_read(bogota_metro_gpkg)
stations_bogota   <- read.csv(bogota_stations_csv)

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function to filter the stations in the metro area + 20 km radius
stations_kept <- bogota_filter_stations_in_metro(
  stations_df_1 = stations_bogota,
  metadata_dir  = outdir_metadata,
  radius_km     = 20,
  metro_area    = bogota_metro,
  out_file      = here::here(outdir_geospatial, "bogota", "bogota_stations_buffer_metro.gpkg"))

# Apply function to merge all downloaded file into DUCKDB database
bogota_stations_data <- bogota_process_xlsx_to_parquet(
  downloads_folder = here::here(bogota_cfg$dl_dir, "ground_stations"),
  out_dir          = outdir_pollution,
  out_name         = "bogota_stations"
)

# Apply function to process census data (unzip, filter and harmonize)
res <- bogota_filter_harmonize_census(
  census_zip = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip"),
  out_dir    = here::here("data", "raw", "census", "bogota", "CG2005"),
  overwrite  = FALSE,
  quiet      = FALSE
)

