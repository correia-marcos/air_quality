# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Describe who lives within each distance band of a monitoring station.
#
#' @Description: For every city, groups geographic units by the distance from their
# representative point to the nearest monitoring station, then reports the population and
# its socioeconomic composition within 1, 3, 5, 10 and 20 km alongside the metropolitan
# total. Reads the distance matrices, the individual census and the geographic boundaries
# (for land area) and writes one long Parquet per city to data/processed/. The
# render_census_tables.R script turns these into the paper's two descriptive tables.
#
#' @Summary:
#   I.   Import data: paths, the radii, and the census variables each city carries.
#   II.  Land area per geographic unit, measured on a local UTM grid.
#   III. Distance-band summaries, one city at a time.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_dist       <- here::here("data", "processed", "distances_matrices")
dir_census     <- here::here("data", "interim", "census")
dir_geospatial <- here::here("data", "interim", "geospatial_data")
outdir_bands   <- here::here("data", "processed", "distance_band_descriptives")

# The radii the paper reports, in kilometres.
radii_km <- c(1, 3, 5, 10, 20)

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

# ============================================================================================
# II: Land area per geographic unit
# ============================================================================================
# Areas are measured on each city's own UTM grid, where a square metre is a square metre.
area_bogota   <- compute_geo_area_km2(sf::st_read(gpkg_bogota, quiet = TRUE), "GEO_ID")
area_cdmx     <- compute_geo_area_km2(sf::st_read(gpkg_cdmx, quiet = TRUE), "CVE_MUN")
area_santiago <- compute_geo_area_km2(sf::st_read(gpkg_santiago, quiet = TRUE),
                                      "zona_id")
area_sp       <- compute_geo_area_km2(sf::st_read(gpkg_sp, quiet = TRUE),
                                      "code_weighting")

# ============================================================================================
# III: Distance-band summaries
# ============================================================================================
dir.create(outdir_bands, recursive = TRUE, showWarnings = FALSE)

# The variable list differs by city because the censuses do: Bogota records no household
# head or ethnicity, Santiago carries age only in its raw column, and income exists only
# for Mexico City and Sao Paulo.
bands_bogota <- compute_distance_band_summary(
  dist_pq     = dist_bogota,
  census_path = micro_bogota,
  area_dt     = area_bogota,
  city        = "Bogota",
  unit_label  = "census tracts",
  share_vars  = c("Share of adults" = "adult",
                  "Share of women" = "women",
                  "Share of employed" = "employed",
                  "Share with no education" = "no_education",
                  "Share with graduate education" = "graduate_educ"),
  mean_vars   = c("Mean age" = "age",
                  "Mean years of schooling" = "educ_years"),
  radii_km    = radii_km)

bands_cdmx <- compute_distance_band_summary(
  dist_pq     = dist_cdmx,
  census_path = micro_cdmx,
  area_dt     = area_cdmx,
  city        = "Mexico City",
  unit_label  = "municipalities",
  share_vars  = c("Share of adults" = "adult",
                  "Share of women" = "women",
                  "Share of HH women" = "hh_head_women",
                  "Share of indigenous" = "indigena",
                  "Share of employed" = "employed",
                  "Share with no education" = "no_education",
                  "Share with graduate education" = "graduate_educ"),
  mean_vars   = c("Mean age" = "age",
                  "Mean years of schooling" = "educ_years",
                  "Mean income" = "income"),
  radii_km    = radii_km)

bands_santiago <- compute_distance_band_summary(
  dist_pq     = dist_santiago,
  census_path = micro_santiago,
  area_dt     = area_santiago,
  city        = "Santiago",
  unit_label  = "census tracts",
  share_vars  = c("Share of adults" = "adult",
                  "Share of women" = "women",
                  "Share of HH women" = "hh_head_women",
                  "Share of indigenous" = "indigena",
                  "Share of employed" = "employed",
                  "Share with no education" = "no_education",
                  "Share with graduate education" = "graduate_educ"),
  mean_vars   = c("Mean age" = "raw_p09",
                  "Mean years of schooling" = "educ_years"),
  radii_km    = radii_km)

bands_sp <- compute_distance_band_summary(
  dist_pq     = dist_sp,
  census_path = micro_sp,
  area_dt     = area_sp,
  city        = "Sao Paulo",
  unit_label  = "weighting areas",
  share_vars  = c("Share of adults" = "adult",
                  "Share of women" = "women",
                  "Share of whites" = "white",
                  "Share of blacks" = "black_pardo",
                  "Share of formal employees" = "formal_emp",
                  "Share of informal employees" = "informal_emp",
                  "Share with no education" = "no_education",
                  "Share with graduate education" = "graduate_educ"),
  mean_vars   = c("Mean age" = "age",
                  "Mean years of schooling" = "educ_years",
                  "Mean income" = "income"),
  radii_km    = radii_km)

distance_bands <- data.table::rbindlist(
  list(bands_bogota, bands_cdmx, bands_santiago, bands_sp), fill = TRUE)

save_table_parquet_csv(distance_bands, outdir_bands, "distance_band_descriptives")

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
