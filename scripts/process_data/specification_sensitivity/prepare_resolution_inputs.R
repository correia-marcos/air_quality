# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Freeze the inputs of the A/B/C robustness designs and build their supports.
#
#' @Description: For each city in resolution_cities, record the hashes of the manuscript
# outputs used, build adult population cells, the coarser geographic supports and their
# full distance matrices. Frozen inputs go to data/interim/resolution_sensitivity/<city>/,
# matrices to data/processed/resolution_sensitivity/<city>/B/. Sources are never edited.
#
#' @Summary:
#   I.   Import data: source functions and settings, declare the output folders.
#   II.  Process and save data: freeze each city's inputs and prepare its supports.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions, their packages and the declared cities.
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

# Set the frozen-input folder and the prepared folder (the 3 km quintile root).
dir_frozen   <- here::here("data", "interim", "resolution_sensitivity")
dir_prepared <- resolution_buffer_root(3)

# ========================================================================================
# II: Process and save data
# ========================================================================================
# Every function below also writes the products it names; see its @details.
supports     <- list()
inputs       <- list()
manifests    <- list()
population   <- list()
geometry     <- list()
denominators <- list()
keys         <- list()
audit        <- list()

# Select the first city when executing the loop body line by line in RStudio.
city <- resolution_cities[1]

for (city in resolution_cities) {
  # Declare the city's fine unit, coarser supports and their nesting.
  supports[[city]] <- resolution_city_supports(city)

  # Name the manuscript-derived inputs, then freeze them; a changed vintage stops the run.
  inputs[[city]]    <- resolution_input_paths(city = city, supports = supports[[city]])
  manifests[[city]] <- resolution_freeze_inputs(
    city         = city,
    supports     = supports[[city]],
    paths        = inputs[[city]],
    frozen_dir   = file.path(dir_frozen, city),
    prepared_dir = file.path(dir_prepared, city))

  # Build the census units, all-adult population and education-reporting cells.
  population[[city]] <- resolution_population_cells(
    supports   = supports[[city]],
    paths      = inputs[[city]],
    frozen_dir = file.path(dir_frozen, city))

  # Read the fine polygons and reconcile their identifiers with the census.
  geometry[[city]] <- resolution_fine_geometry(
    city       = city,
    supports   = supports[[city]],
    paths      = inputs[[city]],
    census_ids = population[[city]]$census$geo_id,
    frozen_dir = file.path(dir_frozen, city))

  # Count processed residents and adults (and, for Santiago, those of the source).
  denominators[[city]] <- resolution_population_denominators(
    city         = city,
    supports     = supports[[city]],
    paths        = inputs[[city]],
    population   = population[[city]]$population,
    cells        = population[[city]]$cells,
    geometry     = geometry[[city]],
    frozen_dir   = file.path(dir_frozen, city),
    prepared_dir = file.path(dir_prepared, city))

  # Give every fine unit its parent at each support and check the nesting.
  keys[[city]] <- resolution_support_keys(
    city       = city,
    supports   = supports[[city]],
    paths      = inputs[[city]],
    census_ids = population[[city]]$census$geo_id,
    population = population[[city]]$population,
    geometry   = geometry[[city]],
    frozen_dir = file.path(dir_frozen, city))

  # Dissolve each support, compute its distance matrix and audit the geography.
  audit[[city]] <- resolution_support_matrices(
    city         = city,
    supports     = supports[[city]],
    paths        = inputs[[city]],
    population   = population[[city]]$population,
    cells        = population[[city]]$cells,
    geometry     = geometry[[city]],
    keys         = keys[[city]],
    frozen_dir   = file.path(dir_frozen, city),
    prepared_dir = file.path(dir_prepared, city))
}
