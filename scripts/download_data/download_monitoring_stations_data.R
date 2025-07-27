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
# Ensure output folder exists
outdir <- here("data", "raw", "pollution_ground_stations")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

ses <- start_selenium(
  jar_path = Sys.getenv("SELENIUM_JAR"),
  port     = 4445,    # you can pick any free port
  browser  = "chrome"
)


# make sure these match your docker-compose.yml
Sys.setenv(
  REMOTE_DRIVER_HOST = "selenium",
  REMOTE_DRIVER_PORT = 4444
)

# connect:
remDr <- start_selenium_docker(browser = "chrome")

remDr <- RSelenium::remoteDriver(
  remoteServerAddr = "selenium",
  port             = 4444L,
  browserName      = "chrome",
  path             = "/wd/hub"
)
remDr$open()

remDr <- ses$client

remDr$getStatus()
remDr$navigate(base_url)

active_cb <- remDr$findElement("css", "#ActiveStations")


stop_selenium(ses)

# ============================================================================================
# III: Save data
# ============================================================================================
# Save table into a csv

