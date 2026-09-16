# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Build a crosswalk from Bogota census manzanas to the 40-unit locality/
# municipality layer, written as Parquet.
#
#' @Description: Reads the 57,032-manzana census tract layer and the 40-unit layer of
# Bogota D.C.'s 20 localidades plus its 20 surrounding municipios. Takes one guaranteed-
# internal representative point per manzana with st_point_on_surface() and spatially
# joins it to its parent polygon with st_within(). Writes one row per manzana with the
# manzana id, parent id, parent name and parent type to
# data/interim/geospatial_data/bogota/bogota_manzana_localidad_crosswalk.parquet.
#
#' @Summary:
#   I.   Import data: read the manzana and locality/municipio layers.
#   II.  Process: representative points, spatial join, select output columns.
#   III. Save: write the crosswalk as Parquet.
#
#' @Date: September 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
path_manzanas <- here::here("data", "interim", "geospatial_data", "bogota",
                            "bogota_area_metro_census_tracts_2018.gpkg")
path_localities <- here::here("data", "interim", "geospatial_data", "bogota",
                              "bogota_area_metro_2018.gpkg")
path_out <- here::here("data", "interim", "geospatial_data", "bogota",
                       "bogota_manzana_localidad_crosswalk.parquet")

manzanas   <- sf::st_read(path_manzanas, quiet = TRUE)
localities <- sf::st_read(path_localities, quiet = TRUE)

# ============================================================================================
# II: Process
# ============================================================================================
# Match CRS before any geometric operation between the two layers.
manzanas <- sf::st_transform(manzanas, crs = sf::st_crs(localities))

# One guaranteed-internal representative point per manzana (never st_centroid, which
# can fall outside a concave or multipart polygon).
manzana_points <- manzanas["GEO_ID"]
sf::st_geometry(manzana_points) <- suppressWarnings(
  sf::st_point_on_surface(sf::st_geometry(manzanas))
)

# Assign each manzana point to the locality/municipio polygon it falls within.
joined <- sf::st_join(manzana_points, localities["GEO_ID"], join = sf::st_within,
                      left = TRUE, suffix = c("_manzana", "_loc"))

crosswalk <- data.table::as.data.table(sf::st_drop_geometry(joined))
loc_lookup <- data.table::as.data.table(sf::st_drop_geometry(localities))[
  , .(GEO_ID, MPIO_CNMBR, TIPO)]

crosswalk <- merge(crosswalk, loc_lookup, by.x = "GEO_ID_loc", by.y = "GEO_ID",
                   all.x = TRUE)

crosswalk <- crosswalk[, .(
  geo_id   = as.character(GEO_ID_manzana),
  loc_id   = as.character(GEO_ID_loc),
  loc_name = MPIO_CNMBR,
  loc_type = TIPO
)]

# ============================================================================================
# III: Save
# ============================================================================================
dir.create(dirname(path_out), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(crosswalk, path_out)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
