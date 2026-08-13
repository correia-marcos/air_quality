# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Configuration file for setup of packages and functions used in the project
# 
#' @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "validation_old_version" scripts.
# 
#' @Date: Aug 2025
#' @Author: Marcos Paulo
# ============================================================================================

# List of required packages
pkgs <- c(
  "arrow",
  "data.table",
  "DBI",
  "dplyr",
  "duckdb",
  "haven",
  "here",
  "lubridate",
  "purrr",
  "quarto",
  "readr",
  "sf",
  "stringi",
  "tibble",
  "tidyr",
  "vroom"
)

# Shared setup mechanism and leaf helpers (one copy for the whole project).
source(here::here("src", "general_utilities", "setup_packages.R"))
source(here::here("src", "general_utilities", "base_utils.R"))

ensure_installed(pkgs)
attach_packages(pkgs)
rm(pkgs)

# Function definitions, split by theme. Nothing below runs; each file only defines.
source(here::here("src", "general_utilities", "validation", "prepare_panels.R"))
source(here::here("src", "general_utilities", "validation", "compare_inputs.R"))
source(here::here("src", "general_utilities", "validation", "compare_results.R"))
source(here::here("src", "general_utilities", "validation", "progression.R"))


# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")