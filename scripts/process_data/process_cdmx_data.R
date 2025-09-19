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
# Ensure output folder exists
outdir <- here::here(cdmx_cfg$out_dir, "pollution_ground_stations", "Mexico_city")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

station_location  <- read.csv(here::here(cdmx_cfg$out_dir,
                                         "pollution_ground_stations",
                                         "Mexico_city",
                                         "all_station_location.csv"))
metro_area        <- sf::st_read(here::here(cdmx_cfg$out_dir,
                                            "cities_shapefiles",
                                            "cdmx_metro.gpkg"))
legacy_metro_area <- sf::st_read(here::here(cdmx_cfg$out_dir,
                                            "cities_shapefiles",
                                            "Mexico_city"))
# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function to merge all downloaded file into a single tidy dataframe
cdmx_stations_data <- cdmx_merge_pollution_csvs(
  downloads_folder = here::here(cdmx_cfg$dl_dir, "Ground_stations"),
  tz               = "UTC",
  years            = cdmx_cfg$years,
  cleanup          = FALSE,
  out_dir          = here::here(outdir, "full_data"),
  out_name         = "mexico_stations_2000_2023",
  write_parquet    = TRUE,
  write_rds        = FALSE,
  write_csv        = FALSE,
  verbose          = TRUE,
  engine           = "auto"
)

# Apply function to filter the stations in the metro area + 20 km radius
stations_kept <- cdmx_filter_stations_in_metro(
  station_location = station_location,
  metro_area       = metro_area,
  radius_km        = 20,             # change if needed
  lon_col          = "lon",
  lat_col          = "lat",
  stations_epsg    = 4326,           # your lon/lat columns CRS
  dissolve         = TRUE,
  verbose          = TRUE
)
legacy_stations_kept <- cdmx_filter_stations_in_metro(
  station_location = station_location,
  metro_area       = legacy_metro_area,
  radius_km        = 20,             # change if needed
  lon_col          = "lon",
  lat_col          = "lat",
  stations_epsg    = 4326,           # your lon/lat columns CRS
  dissolve         = TRUE,
  verbose          = TRUE
)

# ============================================================================================
# III: Save  data
# ============================================================================================
# Save the raw data of ground stations
station_metro_csv <- file.path(outdir, paste0("all_station_on_metro_area", ".csv"))
readr::write_csv(stations_kept, station_metro_csv)
