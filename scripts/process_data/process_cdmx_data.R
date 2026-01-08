# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process all downloaded data from the metro area of Ciudad de México ground stations. 
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
cdmx_metro        <- here::here(outdir_geospatial, "cdmx", "cdmx_metro.gpkg")

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

# Get list with unique stations inside the buffer (20 km) metro area
stations_to_keep = unique(stations_kept$code)

# Apply function to merge all downloaded file into a single tidy dataframe
cdmx_stations_data <- cdmx_merge_pollution_csvs(
  downloads_folder     = here::here(cdmx_cfg$dl_dir, "Ground_stations"),
  tz                   = "America/Mexico_City",
  years                = cdmx_cfg$years,
  stations_keep_codes  = stations_to_keep,
  cleanup              = FALSE,
  out_dir              = outdir_pollution,
  out_name             = "cdmx_metro",
  write_parquet        = TRUE,
  write_rds            = FALSE,
  write_csv            = FALSE,
  verbose              = TRUE,
  engine               = "auto"
)


# ============================================================================================
# III: Save  data
# ============================================================================================
# Save the raw data of ground stations
