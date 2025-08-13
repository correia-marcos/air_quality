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
source(here::here("src", "config_utils_download_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define parameters
base_url                 <- "http://rmcab.ambientebogota.gov.co/Report/stationreport"
start_year               <- 2000L
end_year                 <- 2023L
downloads_folder         <- here::here("data","downloads")
# ============================================================================================
# II: Process and save data
# ============================================================================================
# Apply function to create Selenium server and download the data for Bogota
bogota_download_station_data(
  base_url   = base_url,
  start_year = start_year,
  end_year   = end_year,
  container     = TRUE,
  stations_idx  = NULL,
  max_attempts  = 3,
  timeout_page  = 30,
  timeout_btn   = 30,
  timeout_dl    = 240
)

# Ensure output folder exists
outdir <- here::here("data", "raw", "pollution_ground_stations", "Bogota")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Apply function to merge all downloaded file into a single tidy dataframe
bogota_data <- bogota_merge_downloads(downloads_folder = downloads_folder,
                                      out_dir          = outdir,
                                      out_name         = NULL,
                                      write_rds        = TRUE,
                                      write_parquet    = TRUE,
                                      write_csv_gz     = FALSE,
                                      cleanup          = FALSE,
                                      tz               = "America/Bogota")

# Check coverage
miss <- bogota_missing_matrix(bogota_data, years = start_year:end_year)
if (nrow(miss)) print(head(miss, 20))
