# ==========================================================================================
# IDB: Air monitoring
# ==========================================================================================
#' @Goal: Prepare the alternative 2024 Metro Santiago boundary.
#
#' @Description: Preserve the administrative-commune definition previously prepared by the
# download recipe. This is separate from the Gran Santiago layer used by the manuscript.
# Both use the same preserved national archive; this script never downloads data.
#
#' @Summary:
#   I.   Import data: source functions and declare the input and output paths.
#   II.  Process data: prepare the alternative metropolitan boundary.
#   III. Save outputs: save the geographic layer.
#
#' @Date: September 2026
#' @Author: Marcos
# ==========================================================================================

# ==========================================================================================
# I: Import data
# ==========================================================================================
# Load the city definitions and geographic preparation functions.
source(here::here("src", "general_utilities", "reproducibility.R"))
source(here::here("src", "general_utilities", "process", "spatial_files.R"))
source(here::here("src", "city_specific", "registry.R"))
load_city_modules()

# Use the same source and destination as the former download-recipe branch.
file_geography <- here::here(santiago_cfg$dl_dir, "metro_area",
                             "Cartografia_censo2024_Pais.zip")
file_output    <- here::here(santiago_cfg$out_dir, "geospatial_data", "santiago",
                             "santiago_metro_area_2024.gpkg")

# ==========================================================================================
# II: Process data
# ==========================================================================================
# Select the configured communes and merge repeated CUT identifiers.
santiago_metro <- santiago_prepare_metro_area_2024(
    source_zip        = file_geography,
    type              = "metro_santiago",
    level             = "mpio",
    keep_municipality = santiago_cfg$cities_in_metro,
    dissolve_by       = "CUT")

# ==========================================================================================
# III: Save outputs
# ==========================================================================================
metro_file <- write_geopackage(x = santiago_metro, path = file_output)
