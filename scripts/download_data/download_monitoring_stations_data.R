# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal:
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
source(here::here("src", "config_utils_download_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define parameters
base_url                 <- "http://rmcab.ambientebogota.gov.co/Report/stationreport"
start_date               <- as_date("2010-01-01")
end_date                 <- as_date("2023-12-31")

# ============================================================================================
# II: Process data
# ============================================================================================
# Begin session on Selenium

# Set host & port before calling function
Sys.setenv(
  REMOTE_DRIVER_HOST = "selenium",
  REMOTE_DRIVER_PORT = 4444  # Use container port 4444, not host port 4445
)


# 2. Point to the in-Docker Selenium service
session <- selenium::SeleniumSession$new(
  browser = "firefox",     # you want Firefox
  host    = "selenium",    # Docker service name
  port    = 4444L          # internal Selenium port
)


session$navigate(base_url)

session1 <- session$find_element("css selector", "#stationFilter > div:nth-child(2)")
session1$click()

remDr <- remoteDriver(
  remoteServerAddr = "selenium",
  port = 4444L,
  browserName = "firefox",
  path = "/wd/hub",
  extraCapabilities = list(
    browserName = "firefox",
    platformName = "LINUX",
    "moz:firefoxOptions" = list(binary = "/opt/firefox/firefox")
  )
)
rd$open()  # now returns a valid sessionId

active_cb <- remDr$findElement("css", "#ActiveStations")


stop_selenium(ses)

# ============================================================================================
# III: Save data
# ============================================================================================
# Ensure output folder exists
outdir <- here("data", "raw", "pollution_ground_stations")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save table into a csv

