# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Describe the population living near monitoring stations.
#
#' @Description: Use the nearest-station distance of each geographic unit's representative
# point to summarize its census population within 1, 3, 5, 10 and 20 km. These cumulative
# groups are compared with the metropolitan total. Keep each city's results in memory,
# then save one combined Parquet table and a CSV copy for render_census_tables.R.
# Santiago uses 2017 zones; CDMX pairs its 2020 census with 2024 municipality boundaries.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read geography.
#   II.  Process data: measure unit areas and summarize each city's distance groups.
#   III. Save outputs: write the combined descriptive table as Parquet and CSV.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the area, summary and saving functions, plus their shared helpers
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "diagnostics.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))

# The shared settings define the radii and the census indicators used for each city
source(here::here("config", "analysis_settings.R"))

# Set the distance, census and geography folders and the output folder
dir_dist       <- here::here("data", "processed", "distances_matrices")
dir_census     <- here::here("data", "interim", "census")
dir_geospatial <- here::here("data", "interim", "geospatial_data")
outdir_bands   <- here::here("data", "processed", "distance_band_descriptives")

# Define geo-to-station distance matrix paths
dist_bogota   <- here::here(dir_dist, "bogota_2018",
                            "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_dist, "cdmx_2020",
                            "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_dist, "santiago_2017",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_dist, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")

# Define individual census paths
micro_bogota   <- here::here(dir_census, "bogota_2018",
                             "census_2018_metro_individual.parquet")
micro_cdmx     <- here::here(dir_census, "cdmx_extended_2020",
                             "census_metro_individual_2020.parquet")
micro_santiago <- here::here(dir_census, "santiago_2017",
                             "census_individual_2017.parquet")
micro_sp       <- here::here(dir_census, "sao_paulo_2010",
                             "census_sp_individual_2010.parquet")

# Define geographic boundary files, the source of land area
gpkg_bogota   <- here::here(dir_geospatial, "bogota",
                            "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_cdmx     <- here::here(dir_geospatial, "cdmx",
                            "cdmx_area_metro_municipalities_2024.gpkg")
gpkg_santiago <- here::here(dir_geospatial, "santiago",
                            "gran_santiago_zonas_2017.gpkg")
gpkg_sp       <- here::here(dir_geospatial, "sao_paulo",
                            "sao_paulo_metro_2010_weighting_areas.gpkg")

# Read the geographic units used to measure area; census records remain on disk
geo_bogota   <- sf::st_read(gpkg_bogota, quiet = TRUE)
geo_cdmx     <- sf::st_read(gpkg_cdmx, quiet = TRUE)
geo_santiago <- sf::st_read(gpkg_santiago, quiet = TRUE)
geo_sp       <- sf::st_read(gpkg_sp, quiet = TRUE)

# ========================================================================================
# II: Process data
# ========================================================================================
# Measure each unit's area in square kilometers using its city's UTM projection
area_bogota   <- compute_geo_area_km2(geo_sf = geo_bogota, geo_id_col = "GEO_ID")
area_cdmx     <- compute_geo_area_km2(geo_sf = geo_cdmx, geo_id_col = "CVE_MUN")
area_santiago <- compute_geo_area_km2(geo_sf = geo_santiago, geo_id_col = "zona_id")
area_sp       <- compute_geo_area_km2(geo_sf = geo_sp, geo_id_col = "code_weighting")

# Read the required census columns and summarize all residents in each distance group
bands_bogota <- compute_distance_band_summary(dist_pq     = dist_bogota,
                                              census_path = micro_bogota,
                                              area_dt     = area_bogota,
                                              city        = "Bogota",
                                              unit_label  = "census tracts",
                                              share_vars  = band_shares_bogota,
                                              mean_vars   = band_means_bogota,
                                              radii_km    = distance_band_radii_km)

bands_cdmx <- compute_distance_band_summary(dist_pq     = dist_cdmx,
                                            census_path = micro_cdmx,
                                            area_dt     = area_cdmx,
                                            city        = "Mexico City",
                                            unit_label  = "municipalities",
                                            share_vars  = band_shares_cdmx,
                                            mean_vars   = band_means_cdmx,
                                            radii_km    = distance_band_radii_km)

bands_santiago <- compute_distance_band_summary(dist_pq     = dist_santiago,
                                                census_path = micro_santiago,
                                                area_dt     = area_santiago,
                                                city        = "Santiago",
                                                unit_label  = "census tracts",
                                                share_vars  = band_shares_santiago,
                                                mean_vars   = band_means_santiago,
                                                radii_km    = distance_band_radii_km)

bands_sp <- compute_distance_band_summary(dist_pq     = dist_sp,
                                          census_path = micro_sp,
                                          area_dt     = area_sp,
                                          city        = "Sao Paulo",
                                          unit_label  = "weighting areas",
                                          share_vars  = band_shares_sp,
                                          mean_vars   = band_means_sp,
                                          radii_km    = distance_band_radii_km)

# Combine the four city tables in manuscript order
distance_bands <- data.table::rbindlist(list(bands_bogota, bands_cdmx,
                                            bands_santiago, bands_sp), fill = TRUE)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the Parquet checkpoint and a CSV copy for reading in a spreadsheet
save_table_parquet_csv(dt      = distance_bands,
                       out_dir = outdir_bands,
                       name    = "distance_band_descriptives")
