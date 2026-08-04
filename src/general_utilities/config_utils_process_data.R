# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
# 
# @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "process_data" scripts.
# 
# @Date: Nov 2024
# @Author: Marcos Paulo
# ============================================================================================
# List of required packages
pkgs <- c(
  "archive",
  "arrow",
  "censobr",
  "data.table",
  "DBI",
  "doParallel",
  "dplyr",
  "duckdb",
  "exactextractr",
  "foreach",
  "geosphere",
  "here",
  "janitor",
  "lubridate",
  "memuse",
  "readr",
  "rio",
  "rlang",
  "rnaturalearth",
  "rnaturalearthdata",
  "sandwich",
  "sf",
  "stringi",
  "terra",
  "tibble",
  "tidyr",
  "tools",
  "XLConnect",
  "XML"
)

# Shared setup mechanism and leaf helpers (one copy for the whole project).
source(here::here("src", "general_utilities", "setup_packages.R"))
source(here::here("src", "general_utilities", "base_utils.R"))

ensure_installed(pkgs)
attach_packages(pkgs)
rm(pkgs)

# Pin the geometry engine.
suppressMessages(sf::sf_use_s2(TRUE))

# Function definitions, split by theme. Nothing below runs; each file only defines.
source(here::here("src", "general_utilities", "process", "merra2.R"))
source(here::here("src", "general_utilities", "process", "distances.R"))
source(here::here("src", "general_utilities", "process", "outliers.R"))
source(here::here("src", "general_utilities", "process", "idw_exposure.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "station_socio.R"))
source(here::here("src", "general_utilities", "process", "imputation.R"))
source(here::here("src", "general_utilities", "process", "diagnostics.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))


# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")