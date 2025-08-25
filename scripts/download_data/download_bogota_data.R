# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Download all available data from the Bogota city ground stations
# 
# @Description: From 2000 to 2023 (NEED TO FINISH DOCUMENTATION)
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
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src","city_specific","registry.R"))      
source(here::here("src","city_specific","bogota.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define parameters
print(bogota_cfg$base_url_rmcab)
# base_url                 <- "http://rmcab.ambientebogota.gov.co/Report/stationreport"
# start_year               <- 2000L
# end_year                 <- 2023L

# ============================================================================================
# II: Process and save data
# ============================================================================================
# Apply function to create Selenium server and download the data for Bogota
bogota_download_station_data(
  base_url      = bogota_cfg$base_url_rmcab,
  start_year    = min(bogota_cfg$years),
  end_year      = max(bogota_cfg$years),
  container     = TRUE,
  stations_idx  = NULL,
  max_attempts  = 3,
  timeout_page  = 30,
  timeout_btn   = 30,
  timeout_dl    = 400,
  subdir        = file.path("Ground_stations","Bogota")
)
