# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare the 2024 Gran Santiago communes and the 2024 census for Santiago 2024.
#
#' @Description: Build the urban parts of the Gran Santiago communes from the 2024 census
# cartography and process the 2024 census at that level. The 2024 microdata identifies
# residence only by commune, so the paper uses the 2017 zones and census prepared by
# scripts/process_data/inputs/process_santiago_data.R. The census function writes while
# processing. All sources must be available locally.
#
#' @Summary:
#   I.   Import data: source functions, declare paths and check the sources.
#   II.  Process data: prepare the 2024 communes and process the 2024 census.
#   III. Save outputs: save the 2024 communes.
#
#' @Date: November 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and their required packages.
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "reproducibility.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "spatial_files.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "processing.R"))

# Load the city configurations and use spherical geometry for sf operations.
load_city_modules()
sf::sf_use_s2(TRUE)
cfg <- santiago_cfg

# Set the source folders.
dir_sources     <- cfg$dl_dir
dir_geography   <- here::here(dir_sources, "metro_area")
dir_census_2024 <- here::here(dir_sources, "census", "2024")

# Set the output folders.
out_geography   <- here::here(cfg$out_dir, "geospatial_data", "santiago")
out_census_2024 <- here::here(cfg$out_dir, "census", "santiago_2024")

# Name the 2024 census cartography.
file_geography_2024 <- here::here(dir_geography, "Cartografia_censo2024_Pais.zip")

# Check the sources before processing writes any output.
processing_files(c(file_geography_2024, dir_census_2024), "Santiago 2024 sources")
preflight_census_inputs("santiago", c(census_2024 = dir_census_2024))

# ========================================================================================
# II: Process data
# ========================================================================================
# Gran Santiago follows INE urban limits; keep_municipality applies to Metro Santiago.
metro_2024 <- santiago_prepare_metro_area_2024(source_zip        = file_geography_2024,
                                               type              = "gran_santiago",
                                               level             = "mpio",
                                               keep_municipality = cfg$cities_in_metro,
                                               dissolve_by       = "CUT")

# Process the 2024 census; the call saves individual records and commune summaries.
census_2024 <- santiago_process_census_2024(census_dir  = dir_census_2024,
                                            sf_data     = metro_2024,
                                            match_col   = "CUT",
                                            out_dir     = out_census_2024,
                                            overwrite   = TRUE,
                                            return_data = FALSE)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the 2024 communes.
metro_2024_file <- write_geopackage(
  x    = metro_2024,
  path = here::here(out_geography, "gran_santiago_area_2024.gpkg"))
