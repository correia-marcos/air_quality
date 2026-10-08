# ============================================================================================
# IDB: Air monitoring — three-city geographic-resolution inputs
# ============================================================================================
#' @Goal: Prepare frozen populations, city-specific crosswalks and Design B matrices.
#' @Description: Uses current derived inputs, records their hashes, and writes only
#'   optional resolution directories. Does not acquire or replace original sources.
#' @Summary: Explicit city inputs; population cells; geography audit; dissolved matrices.
#   I.   Import settings and functions.
#   II.  Prepare frozen inputs and inspect named populations/geographies.
#   III. Check the per-city preparation summaries.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

# ============================================================================================
# I: Import data
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
cities <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) stop("This preparation script takes no arguments.")
prepared <- list()
city <- cities[[1]]

# ============================================================================================
# II: Prepare data
# ============================================================================================
for (city in cities) {
  sources_result <- resolution_prepare_sources(
    city = city)
  census_dir <- sources_result$census_dir
  out <- sources_result$out
  processed <- sources_result$processed
  census_file <- sources_result$census_file
  id_col <- sources_result$id_col
  fine_width <- sources_result$fine_width
  levels <- sources_result$levels
  labels <- sources_result$labels
  widths <- sources_result$widths
  nesting <- sources_result$nesting
  paths <- sources_result$paths

  population_result <- resolution_prepare_population(
    city = city,
    census_dir = census_dir,
    out = out,
    processed = processed,
    census_file = census_file,
    id_col = id_col,
    fine_width = fine_width,
    paths = paths)
  census <- population_result$census
  population <- population_result$population
  cells <- population_result$cells
  geo <- population_result$geo
  denominators <- population_result$denominators

  crosswalks_result <- resolution_prepare_crosswalks(
    city = city,
    out = out,
    levels = levels,
    labels = labels,
    widths = widths,
    nesting = nesting,
    paths = paths,
    census = census,
    population = population,
    geo = geo)
  keys <- crosswalks_result$keys
  ladder <- crosswalks_result$ladder
  stations <- crosswalks_result$stations

  matrices_result <- resolution_prepare_matrices(
    city = city,
    out = out,
    processed = processed,
    levels = levels,
    paths = paths,
    population = population,
    cells = cells,
    geo = geo,
    keys = keys,
    stations = stations)
  audit <- matrices_result$audit

  prepared[[city]] <- list(population = population, denominators = denominators,
                           geography = audit, outputs = c(out, processed))
}

# ============================================================================================
# III: Check outputs
# ============================================================================================
print(lapply(prepared, function(x) x$outputs))
