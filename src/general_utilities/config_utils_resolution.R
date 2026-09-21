# ============================================================================================
# IDB: Air monitoring — optional resolution analysis dependencies
# ============================================================================================
#' @Goal: Load only installed dependencies for the optional methodological workflow.
#' @Description: No installation, source acquisition, or manuscript execution occurs here.
#' @Summary: Load packages, shared analytical functions, and the paper theme.
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================
resolution_packages <- c("arrow", "data.table", "DBI", "duckdb", "dplyr", "sf",
                         "stringi", "sandwich", "ggplot2", "here")
for (package in resolution_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Missing installed dependency: ", package, ". Restore the project environment.")
  }
}
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressMessages(sf::sf_use_s2(TRUE))
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
for (file in c("geo_ids.R", "distances.R", "idw_exposure.R",
               "exposure_regressions.R", "resolution_sensitivity.R")) {
  source(here::here("src", "general_utilities", "process", file))
}
rm(resolution_packages, package, file)
