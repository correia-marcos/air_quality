# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process and standardize air quality and census data for São Paulo
# 
# @Description: This script transforms raw monitoring, geospatial, and census data into a 
#   project-standard format. It includes: (1) Spatial filtering of ground stations within a 
#   20km metropolitan buffer; (2) Consolidation of QUALAR raw measurements into 
#   Parquet format; (3) Extraction and harmonization of 2010 Brazilian Census microdata 
#   to integrate socio-economic indicators into the analysis.
# 
# @Summary: 
#   I.   Setup: Load dependencies, utility functions, and city-specific config.
#   II.  Import: Read raw geospatial boundaries and station location files.
#   III. Pollution: Filter stations by buffer and convert data to Parquet.
#   IV.  Census: Extract and harmonize the 2010 census microdata.
# 
# @Date: January 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "sao_paulo.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Define the output general folders
downloaded_data   <- here::here(sao_paulo_cfg$dl_dir, "ground_stations")
outdir_pollution  <- here::here(sao_paulo_cfg$out_dir, "monitoring_stations")
outdir_geospatial <- here::here(sao_paulo_cfg$out_dir, "geospatial_data")
outdir_metadata   <- here::here(sao_paulo_cfg$dl_dir, "stations_metadata")

# Define the file's specific location
sao_paulo_stations_csv <- here::here(outdir_metadata, "stations_metadata.csv")
sao_paulo_metro_gpkg   <- here::here(outdir_geospatial, "sao_paulo",
                                     "sao_paulo_metro_2010.gpkg")
sao_paulo_tracts_gpkg  <- here::here(outdir_geospatial, "sao_paulo",
                                     "sao_paulo_metro_2010_weighting_areas.gpkg")
# Read the geospatial data
stations_sp     <- read.csv(sao_paulo_stations_csv)
metro_area      <- st_read(sao_paulo_metro_gpkg)
weighting_areas <- st_read(sao_paulo_tracts_gpkg)

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function to filter the stations in the metro area + 20 km radius
stations_kept <- sp_filter_stations_in_metro(
  stations_sp   = stations_sp,
  radius_km     = 20,
  metro_area    = metro_area,
  out_file      = here::here(outdir_geospatial, "sao_paulo",
                             "sao_paulo_stations_buffer_metro_2010.gpkg"))

# Apply function to merge all downloaded file of Bogota metro area into DUCKDB database
sao_paulo_stations_data <- sp_process_stations_data_to_parquet(
  data_folder    = downloaded_data,
  stations_sf    = stations_kept,
  tz             = "UTC",    # Important to be UTC so DuckDB don't misbehaves
  out_dir        = outdir_pollution,
  out_name       = "sao_paulo_metro",
  years          = sao_paulo_cfg$years
)

# Apply function to process the census data of 2010 - through a package
process_harmonize_census <- sp_process_census_2010(
  out_dir    = here::here("data", "interim", "census", "sao_paulo_2010"),
  sf_data    = weighting_areas,
  quiet      = FALSE)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")
