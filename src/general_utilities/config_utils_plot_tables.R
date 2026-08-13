# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Configuration file for setup of packages and functions used in the project
# 
#' @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "plot_table" scripts.
# 
#' @Date: Nov 2024
#' @Author: Marcos Paulo
# ============================================================================================

# List of required packages
pkgs <- c(
  "arrow",
  "cowplot",
  "data.table",
  "dplyr",
  "ggmap",
  "ggplot2",
  "ggspatial",
  "ggridges",
  "haven",
  "here",
  "htmltools",
  "kableExtra",
  "leaflet",
  "lubridate",
  "rlang",
  "rnaturalearth",
  "rnaturalearthdata",
  "rnaturalearthhires",
  "sp",
  "sf",
  "showtext",
  "terra",
  "tidyr",
  "viridisLite",
  "viridis",
  "zoo")

# Shared setup mechanism, leaf helpers and the paper theme (one copy project-wide).
source(here::here("src", "general_utilities", "setup_packages.R"))
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))

ensure_installed(pkgs)
attach_packages(pkgs)
rm(pkgs)

# Function definitions, split by theme. Nothing below runs; each file only defines.
source(here::here("src", "general_utilities", "plot", "maps.R"))
source(here::here("src", "general_utilities", "plot", "timeseries_hourly.R"))
source(here::here("src", "general_utilities", "plot", "exposure_figures.R"))
source(here::here("src", "general_utilities", "plot", "latex_tables.R"))
source(here::here("src", "general_utilities", "plot", "station_monitoring.R"))


# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")
