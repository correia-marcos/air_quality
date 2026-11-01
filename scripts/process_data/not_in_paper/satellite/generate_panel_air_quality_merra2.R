# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Extract hourly MERRA-2 aerosol means for the four study areas.
#
#' @Description: Read preserved raster files and the current generated city boundaries.
# Average each aerosol species over the city polygon, retaining one row per date/hour.
# Keep the four panels in memory, then save them for prepare_station_temporal.R.
# This optional route is separate from the manuscript station summaries.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read the preserved inputs.
#   II.  Process data: extract a named hourly aerosol panel for each city.
#   III. Save outputs: write the city CSV checkpoints in the same order.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the extraction function and its shared settings
source(here::here("src", "general_utilities", "process", "merra2.R"))
source(here::here("config", "merra2_settings.R"))
sf::sf_use_s2(TRUE)

# Set the preserved raster and generated geography folders, plus the panel destination
dir_merra2     <- here::here("data", "raw", "merra2_aerosol_products")
dir_geography  <- here::here("data", "interim", "geospatial_data")
outdir_panels  <- here::here("data", "interim", "cities_m2_aerosols")

# List the source raster files and read the four generated metropolitan boundaries
nc_files <- list.files(dir_merra2, pattern = "[.]nc4$", full.names = TRUE)
if (!length(nc_files)) stop("Acquire MERRA-2 aerosol files before optional extraction.")
geo_bogota <- sf::st_read(here::here(dir_geography, "bogota",
  "bogota_area_metro_2018.gpkg"), quiet = TRUE)
geo_cdmx <- sf::st_read(here::here(dir_geography, "cdmx",
  "cdmx_area_metro_2024.gpkg"), quiet = TRUE)
geo_santiago <- sf::st_read(here::here(dir_geography, "santiago",
  "gran_santiago_area_2024.gpkg"), quiet = TRUE)
geo_sp <- sf::st_read(here::here(dir_geography, "sao_paulo",
  "sao_paulo_metro_2010.gpkg"), quiet = TRUE)

# ========================================================================================
# II: Process data
# ========================================================================================
# Extract each city mean; the function returns a table without saving it
panel_bogota <- process_merra2_region_hourly(shapefile      = geo_bogota,
                                             nc_files       = nc_files,
                                             region_name    = "Bogotá",
                                             num_cores      = merra2_num_cores,
                                             extraction_fun = merra2_extraction_fun,
                                             parallel       = merra2_parallel)

panel_cdmx <- process_merra2_region_hourly(shapefile      = geo_cdmx,
                                           nc_files       = nc_files,
                                           region_name    = "Ciudad de México",
                                           num_cores      = merra2_num_cores,
                                           extraction_fun = merra2_extraction_fun,
                                           parallel       = merra2_parallel)

panel_santiago <- process_merra2_region_hourly(shapefile      = geo_santiago,
                                               nc_files       = nc_files,
                                               region_name    = "Santiago",
                                               num_cores      = merra2_num_cores,
                                               extraction_fun = merra2_extraction_fun,
                                               parallel       = merra2_parallel)

panel_sp <- process_merra2_region_hourly(shapefile      = geo_sp,
                                         nc_files       = nc_files,
                                         region_name    = "São Paulo",
                                         num_cores      = merra2_num_cores,
                                         extraction_fun = merra2_extraction_fun,
                                         parallel       = merra2_parallel)


# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the four aerosol panels for conversion to PM2.5
dir.create(outdir_panels, recursive = TRUE, showWarnings = FALSE)

write.csv(panel_bogota, here::here(outdir_panels, "bogota_panel.csv"),
          row.names = FALSE)

write.csv(panel_cdmx, here::here(outdir_panels, "ciudad_mexico_panel.csv"),
          row.names = FALSE)

write.csv(panel_santiago, here::here(outdir_panels, "santiago_panel.csv"),
          row.names = FALSE)

write.csv(panel_sp, here::here(outdir_panels, "sao_paulo_panel.csv"),
          row.names = FALSE)
