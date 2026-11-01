# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Map population density and active monitoring stations.
#
#' @Description: Use the collapsed census and prepared geographic boundaries for each
# city. Select stations reporting PM10 or PM2.5 in the original hourly panel during
# the analysis year. Santiago uses 2017 census zones; CDMX pairs the 2020 census with
# 2024 municipalities. Keep each map in memory before saving the manuscript PDFs.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read inputs.
#   II.  Process data: build one named map per city.
#   III. Save outputs: save the results in the same order.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the scientific functions and their shared helpers
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "plot", "maps.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))

# Use the manuscript font and theme
set_paper_theme()

sf::sf_use_s2(TRUE)

# Define input and output folders
dir_geospatial <- here::here("data", "interim", "geospatial_data")
dir_census     <- here::here("data", "interim", "census")
dir_raw        <- here::here("data", "interim", "monitoring_stations")
outdir_maps    <- here::here("results", "figures", "maps")

# Define geographic boundary files
gpkg_bogota   <- here::here(dir_geospatial, "bogota",
                            "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_cdmx     <- here::here(dir_geospatial, "cdmx",
                            "cdmx_area_metro_municipalities_2024.gpkg")
gpkg_santiago <- here::here(dir_geospatial, "santiago", "gran_santiago_zonas_2017.gpkg")
gpkg_sp       <- here::here(dir_geospatial, "sao_paulo",
                            "sao_paulo_metro_2010_weighting_areas.gpkg")

# Define station spatial files
gpkg_stations_bogota   <- here::here(dir_geospatial, "bogota",
                                     "bogota_2018_stations_buffer_metro.gpkg")
gpkg_stations_cdmx     <- here::here(dir_geospatial, "cdmx",
                                     "cdmx_stations_buffer_metro.gpkg")
gpkg_stations_santiago <- here::here(dir_geospatial, "santiago",
                                     "gran_santiago_stations_buffer_metro_2017.gpkg")
gpkg_stations_sp       <- here::here(dir_geospatial, "sao_paulo",
                                     "sao_paulo_stations_buffer_metro_2010.gpkg")

# Define collapsed census paths
census_bogota_pq   <- here::here(dir_census, "bogota_2018",
                                 "census_2018_metro_collapsed.parquet")
census_cdmx_pq     <- here::here(dir_census, "cdmx_extended_2020",
                                 "collapse_metro_area_2020.parquet")
census_santiago_pq <- here::here(dir_census, "santiago_2017",
                                 "census_collapsed_2017.parquet")
census_sp_pq       <- here::here(dir_census, "sao_paulo_2010",
                                 "census_sp_collapsed_2010.parquet")

# Define raw Arrow dataset paths, read only to find the active stations
arrow_bogota   <- here::here(dir_raw, "bogota_metro_dataset")
arrow_cdmx     <- here::here(dir_raw, "cdmx_metro_dataset")
arrow_santiago <- here::here(dir_raw, "santiago_metro_dataset")
arrow_sp       <- here::here(dir_raw, "sao_paulo_metro_dataset")

# Read the geographic units, station locations and collapsed census tables
geo_bogota   <- sf::st_read(gpkg_bogota, quiet = TRUE)
geo_cdmx     <- sf::st_read(gpkg_cdmx, quiet = TRUE)
geo_santiago <- sf::st_read(gpkg_santiago, quiet = TRUE)
geo_sp       <- sf::st_read(gpkg_sp, quiet = TRUE)

stations_bogota   <- sf::st_read(gpkg_stations_bogota, quiet = TRUE)
stations_cdmx     <- sf::st_read(gpkg_stations_cdmx, quiet = TRUE)
stations_santiago <- sf::st_read(gpkg_stations_santiago, quiet = TRUE)
stations_sp       <- sf::st_read(gpkg_stations_sp, quiet = TRUE)

census_bogota   <- arrow::read_parquet(census_bogota_pq)
census_cdmx     <- arrow::read_parquet(census_cdmx_pq)
census_santiago <- arrow::read_parquet(census_santiago_pq)
census_sp       <- arrow::read_parquet(census_sp_pq)

# ========================================================================================
# II: Process data
# ========================================================================================
# Join census population to land area and add the active stations
map_bogota <- plot_population_density_map(metro_sf    = geo_bogota,
                                          stations_sf = stations_bogota,
                                          arrow_dir   = arrow_bogota,
                                          census_df   = census_bogota,
                                          join_sf_col = "GEO_ID",
                                          join_df_col = "geo_id",
                                          station_col = "station_name",
                                          year_filter = analysis_year,
                                          city_label  = "Bogotá")

map_cdmx <- plot_population_density_map(metro_sf    = geo_cdmx,
                                        stations_sf = stations_cdmx,
                                        arrow_dir   = arrow_cdmx,
                                        census_df   = census_cdmx,
                                        join_sf_col = "CVE_MUN",
                                        join_df_col = "geo_id",
                                        station_col = "station",
                                        year_filter = analysis_year,
                                        city_label  = "Mexico City")

map_santiago <- plot_population_density_map(metro_sf    = geo_santiago,
                                            stations_sf = stations_santiago,
                                            arrow_dir   = arrow_santiago,
                                            census_df   = census_santiago,
                                            join_sf_col = "zona_id",
                                            join_df_col = "geo_id",
                                            station_col = "station_name",
                                            year_filter = analysis_year,
                                            city_label  = "Santiago")

map_sp <- plot_population_density_map(metro_sf    = geo_sp,
                                      stations_sf = stations_sp,
                                      arrow_dir   = arrow_sp,
                                      census_df   = census_sp,
                                      join_sf_col = "code_weighting",
                                      join_df_col = "geo_id",
                                      station_col = "station_name",
                                      year_filter = analysis_year,
                                      city_label  = "São Paulo")

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the four maps with the manuscript filenames
dir.create(outdir_maps, recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_maps, "bogota_population_density_map.pdf"),
                plot      = map_bogota,
                device    = grDevices::cairo_pdf,
                width     = 8, height = 8, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_maps, "mexico_population_density_map.pdf"),
                plot      = map_cdmx,
                device    = grDevices::cairo_pdf,
                width     = 8, height = 8, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_maps, "santiago_population_density_map.pdf"),
                plot      = map_santiago,
                device    = grDevices::cairo_pdf,
                width     = 8, height = 8, dpi = 300, limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_maps, "saopaulo_population_density_map.pdf"),
                plot      = map_sp,
                device    = grDevices::cairo_pdf,
                width     = 8, height = 8, dpi = 300, limitsize = FALSE)
