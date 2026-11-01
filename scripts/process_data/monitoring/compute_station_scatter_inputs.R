# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Link station pollution outcomes to socioeconomic context.
#
#' @Description: Summarize cleaned hourly readings in the analysis year and attach the
# collapsed census context. Bogotá uses a 3 km buffer; the other cities use the
# containing municipality, census zone or weighting area. Retain unmatched active
# stations and their match indicator. These four tables feed monitoring and scatter plots.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read inputs.
#   II.  Process data: summarize pollution, attach spatial context and join station tables.
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
source(here::here("src", "general_utilities", "process", "station_socio.R"))
source(here::here("config", "analysis_settings.R"))

sf::sf_use_s2(TRUE)

# Set the input folders and the destination for station summaries
dir_cleaned    <- here::here("data", "processed", "monitoring_stations_outliers")
dir_geospatial <- here::here("data", "interim", "geospatial_data")
dir_census     <- here::here("data", "interim", "census")
outdir_station <- here::here("data", "processed", "station_socio_exposure")

# Define cleaned Arrow dataset paths
arrow_bogota   <- here::here(dir_cleaned, "bogota_metro_clean")
arrow_cdmx     <- here::here(dir_cleaned, "cdmx_metro_clean")
arrow_santiago <- here::here(dir_cleaned, "santiago_metro_clean")
arrow_sp       <- here::here(dir_cleaned, "sao_paulo_metro_clean")

# Define station spatial files
gpkg_stations_bogota   <- here::here(dir_geospatial, "bogota",
                                     "bogota_2018_stations_buffer_metro.gpkg")
gpkg_stations_cdmx     <- here::here(dir_geospatial, "cdmx",
                                     "cdmx_stations_buffer_metro.gpkg")
gpkg_stations_santiago <- here::here(dir_geospatial, "santiago",
                                     "gran_santiago_stations_buffer_metro_2017.gpkg")
gpkg_stations_sp       <- here::here(dir_geospatial, "sao_paulo",
                                     "sao_paulo_stations_buffer_metro_2010.gpkg")

# Define geographic boundary files
gpkg_geo_bogota   <- here::here(dir_geospatial, "bogota",
                                "bogota_area_metro_census_tracts_2018.gpkg")
gpkg_geo_cdmx     <- here::here(dir_geospatial, "cdmx",
                                "cdmx_area_metro_municipalities_2024.gpkg")
gpkg_geo_santiago <- here::here(dir_geospatial, "santiago",
                                "gran_santiago_zonas_2017.gpkg")
gpkg_geo_sp       <- here::here(dir_geospatial, "sao_paulo",
                                "sao_paulo_metro_2010_weighting_areas.gpkg")

# Define collapsed census files
census_bogota_pq   <- here::here(dir_census, "bogota_2018",
                                 "census_2018_metro_collapsed.parquet")
census_cdmx_pq     <- here::here(dir_census, "cdmx_extended_2020",
                                 "collapse_metro_area_2020.parquet")
census_santiago_pq <- here::here(dir_census, "santiago_2017",
                                 "census_collapsed_2017.parquet")
census_sp_pq       <- here::here(dir_census, "sao_paulo_2010",
                                 "census_sp_collapsed_2010.parquet")

# Read station spatial data
stations_bogota   <- sf::st_read(gpkg_stations_bogota, quiet = TRUE)
stations_cdmx     <- sf::st_read(gpkg_stations_cdmx, quiet = TRUE)
stations_santiago <- sf::st_read(gpkg_stations_santiago, quiet = TRUE)
stations_sp       <- sf::st_read(gpkg_stations_sp, quiet = TRUE)

# Read geographic units
geo_bogota   <- sf::st_read(gpkg_geo_bogota, quiet = TRUE)
geo_cdmx     <- sf::st_read(gpkg_geo_cdmx, quiet = TRUE)
geo_santiago <- sf::st_read(gpkg_geo_santiago, quiet = TRUE)
geo_sp       <- sf::st_read(gpkg_geo_sp, quiet = TRUE)

# Read collapsed census data.
census_bogota   <- data.table::as.data.table(arrow::read_parquet(census_bogota_pq))
census_cdmx     <- data.table::as.data.table(arrow::read_parquet(census_cdmx_pq))
census_santiago <- data.table::as.data.table(arrow::read_parquet(census_santiago_pq))
census_sp       <- data.table::as.data.table(arrow::read_parquet(census_sp_pq))

# Set the four station-summary filenames
out_bogota   <- here::here(outdir_station, "bogota_2018",
                           "bogota_2018_2023_3km_station_socio.parquet")
out_cdmx     <- here::here(outdir_station, "cdmx_2020",
                           "cdmx_2020_2023_station_socio.parquet")
out_santiago <- here::here(outdir_station, "santiago_2017",
                           "santiago_2017_2023_station_socio.parquet")
out_sp       <- here::here(outdir_station, "sao_paulo_2010",
                           "sao_paulo_2010_2023_station_socio.parquet")

# ========================================================================================
# II: Process data
# ========================================================================================
# Summarize each active station: annual concentrations and hours above each threshold
pollution_bogota   <- compute_station_pollution_summary(arrow_dir   = arrow_bogota,
                                                        year_filter = analysis_year,
                                                        station_col = "station",
                                                        pollutants  = summary_pollutants,
                                                        who_it      = station_who_thresholds)

pollution_cdmx     <- compute_station_pollution_summary(arrow_dir   = arrow_cdmx,
                                                        year_filter = analysis_year,
                                                        station_col = "station",
                                                        pollutants  = summary_pollutants,
                                                        who_it      = station_who_thresholds)

pollution_santiago <- compute_station_pollution_summary(arrow_dir   = arrow_santiago,
                                                        year_filter = analysis_year,
                                                        station_col = "station",
                                                        pollutants  = summary_pollutants,
                                                        who_it      = station_who_thresholds)

pollution_sp       <- compute_station_pollution_summary(arrow_dir   = arrow_sp,
                                                        year_filter = analysis_year,
                                                        station_col = "station",
                                                        pollutants  = summary_pollutants,
                                                        who_it      = station_who_thresholds)

# Attach the census context, preserving each city's geographic definition
context_bogota   <- compute_station_socio_context(stations_sf    = stations_bogota,
                                                  geo_sf         = geo_bogota,
                                                  census_col     = census_bogota,
                                                  station_id_col = "station_name",
                                                  geo_sf_id_col  = "GEO_ID",
                                                  socio_vars     = "education_mean",
                                                  context_method = "buffer",
                                                  buffer_km      = station_context_buffer_km)

context_cdmx     <- compute_station_socio_context(stations_sf    = stations_cdmx,
                                                  geo_sf         = geo_cdmx,
                                                  census_col     = census_cdmx,
                                                  station_id_col = "station",
                                                  geo_sf_id_col  = "CVE_MUN",
                                                  socio_vars     = c("education_mean",
                                                                     "income_mean"),
                                                  context_method = "containing_geo")

context_santiago <- compute_station_socio_context(stations_sf    = stations_santiago,
                                                  geo_sf         = geo_santiago,
                                                  census_col     = census_santiago,
                                                  station_id_col = "station_name",
                                                  geo_sf_id_col  = "zona_id",
                                                  socio_vars     = "education_mean",
                                                  context_method = "containing_geo")

context_sp       <- compute_station_socio_context(stations_sf    = stations_sp,
                                                  geo_sf         = geo_sp,
                                                  census_col     = census_sp,
                                                  station_id_col = "station_name",
                                                  geo_sf_id_col  = "code_weighting",
                                                  socio_vars     = c("education_mean",
                                                                     "income_mean"),
                                                  context_method = "containing_geo")

# Keep every active station and mark whether its socioeconomic context matched
station_bogota   <- join_station_scatter_inputs(pollution_dt = pollution_bogota,
                                                context_dt   = context_bogota,
                                                socio_vars   = "education_mean",
                                                year_filter  = analysis_year)
  
station_cdmx     <- join_station_scatter_inputs(pollution_dt = pollution_cdmx,
                                                context_dt   = context_cdmx,
                                                socio_vars   = c("education_mean",
                                                                 "income_mean"),
                                                year_filter  = analysis_year)

station_santiago <- join_station_scatter_inputs(pollution_dt = pollution_santiago,
                                                context_dt   = context_santiago,
                                                socio_vars   = "education_mean",
                                                year_filter  = analysis_year)

station_sp       <- join_station_scatter_inputs(pollution_dt = pollution_sp,
                                                context_dt   = context_sp,
                                                socio_vars   = c("education_mean",
                                                                 "income_mean"),
                                                year_filter  = analysis_year)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save each city table for the figure recipes
dir.create(dirname(out_bogota), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(station_bogota, out_bogota)

dir.create(dirname(out_cdmx), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(station_cdmx, out_cdmx)

dir.create(dirname(out_santiago), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(station_santiago, out_santiago)

dir.create(dirname(out_sp), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(station_sp, out_sp)
