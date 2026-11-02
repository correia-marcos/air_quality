# Offline stage adapters. Transformations remain in the city modules.

#' @param cfg City configuration.
#' @param inputs Named preserved geographic sources.
#' @param quiet Suppress progress messages.
#' @return The two named 2018 geographic files: metropolitan area and census tracts.
bogota_prepare_geography <- function(cfg, inputs, quiet = FALSE) {
  expected <- basename(city_processing_inputs("bogota", cfg)$geography)
  processing_files(inputs, "Bogotá geography sources")
  if (!all(expected %in% basename(inputs))) stop("Missing Bogotá geographic source files")
  files <- city_geographic_outputs("bogota", cfg)
  levels <- c("mpio_localidad", "manzana")
  years <- c(2018, 2018)
  archives <- list("SHP_MGN2018_INTGRD_MPIO.zip",
    c("SHP_MGN2018_INTGRD_MANZ.zip", "SHP_MGN2018_INTGRD_SECCR.zip"))
  for (i in seq_along(files)) {
    geography <- bogota_prepare_metro_area(
        source_zips = inputs[basename(inputs) %in% archives[[i]]],
        level = levels[i], mgn_year = years[i],
        municipality_codes = cfg$city_code_metro,
        localities_file = inputs[basename(inputs) == "bogota_loca.gpkg"],
        quiet = quiet)
    write_geopackage(x = geography, path = files[i])
  }
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Named preserved geographic sources.
#' @param quiet Suppress progress messages.
#' @return Municipality and AGEB geographic files.
cdmx_prepare_geography <- function(cfg, inputs, quiet = FALSE) {
  processing_files(inputs, "CDMX geographic sources")
  files <- city_geographic_outputs("cdmx", cfg)
  for (i in seq_along(files)) {
    geography <- cdmx_prepare_metro_area(source_zip = unname(inputs["zip"]),
        level = c("municipality", "ageb")[i], keep_municipality = cfg$cities_in_metro,
        quiet = quiet)
    write_geopackage(x = geography, path = files[i])
  }
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Named preserved geographic sources.
#' @param quiet Suppress progress messages.
#' @return The 2017 census zones.
santiago_prepare_geography <- function(cfg, inputs, quiet = FALSE) {
  processing_files(inputs, "Santiago geographic sources")
  files <- city_geographic_outputs("santiago", cfg)
  zones <- santiago_prepare_metro_area_2017(metro_file = unname(inputs["metro_2017"]),
      zones_file = unname(inputs["zones_2017"]), count_file = unname(inputs["count_2017"]),
      quiet = quiet)
  write_geopackage(x = zones, path = files["zones_2017"])
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Named preserved geographic sources.
#' @param quiet Suppress progress messages.
#' @return Municipalities and weighting areas for 2010.
sao_paulo_prepare_geography <- function(cfg, inputs, quiet = FALSE) {
  processing_files(inputs, "São Paulo geographic sources")
  files <- city_geographic_outputs("sao_paulo", cfg)
  municipalities <- sao_paulo_prepare_metro_area(
      source_zip = unname(inputs["municipalities"]), level = "mpio",
      keep_municipality = cfg$cities_in_metro, quiet = quiet)
  write_geopackage(x = municipalities, path = files["municipalities"])
  weights <- sao_paulo_prepare_weighting_areas(source_file = unname(inputs["weights"]),
      keep_municipality = cfg$cities_in_metro, quiet = quiet)
  write_geopackage(x = weights, path = files["weights"])
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Station locations and metadata sources.
#' @param geography Upstream geographic files.
#' @param quiet Suppress progress messages.
#' @return The 2018 filtered-station product.
bogota_prepare_stations <- function(cfg, inputs, geography, quiet = FALSE) {
  processing_files(inputs, "station sources")
  processing_files(geography, "geography")
  locations <- read.csv(inputs["locations"])
  files <- city_station_outputs("bogota", cfg)
  metro <- sf::st_read(geography["metro_2018"], quiet = quiet)
  stations <- bogota_filter_stations_in_metro(
    rmcab_df = locations, metadata_dir = inputs["metadata"],
    radius_km = cfg$station_buffer_km,
    metro_area = metro, out_file = files["stations_2018"])
  processing_files(files)
}

#' @param cfg City configuration, including the station-name corrections.
#' @param inputs Station location source.
#' @param geography Upstream AGEB and municipality geography.
#' @param quiet Suppress progress messages.
#' @return Filtered stations using the original name corrections and 20 km buffer.
cdmx_prepare_stations <- function(cfg, inputs, geography, quiet = FALSE) {
  processing_files(inputs, "station sources")
  processing_files(geography, "geography")
  locations <- read.csv(inputs["locations"])
  locations <- cdmx_correct_station_names(locations, cfg$station_nme_map)
  metro <- sf::st_read(geography["metro"], quiet = quiet)
  files <- city_station_outputs("cdmx", cfg)
  stations <- cdmx_filter_stations_in_metro(station_location = locations,
    metro_area = metro, radius_km = cfg$station_buffer_km,
    dissolve = TRUE, verbose = !quiet,
    out_file = files["stations"])
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Station metadata source.
#' @param geography Upstream geography; station selection uses 2017 zones.
#' @param quiet Suppress progress messages.
#' @return Stations selected using the 2017 geographic definition.
santiago_prepare_stations <- function(cfg, inputs, geography, quiet = FALSE) {
  processing_files(inputs, "station sources")
  processing_files(geography, "geography")
  locations <- read.csv(inputs["locations"])
  metro <- sf::st_read(geography["zones_2017"], quiet = quiet)
  files <- city_station_outputs("santiago", cfg)
  stations <- santiago_filter_stations_in_metro(stations_df = locations,
    radius_km = cfg$station_buffer_km, metro_area = metro,
    out_file = files["stations"], quiet = quiet)
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Station metadata source.
#' @param geography Upstream municipality, tract and weighting-area geography.
#' @param quiet Suppress progress messages.
#' @return Filtered stations within the municipal metropolitan buffer.
sao_paulo_prepare_stations <- function(cfg, inputs, geography, quiet = FALSE) {
  processing_files(inputs, "station sources")
  processing_files(geography, "geography")
  locations <- read.csv(inputs["locations"])
  metro <- sf::st_read(geography["municipalities"], quiet = quiet)
  files <- city_station_outputs("sao_paulo", cfg)
  stations <- sp_filter_stations_in_metro(stations_sp = locations,
    radius_km = cfg$station_buffer_km,
    metro_area = metro, out_file = files["stations"])
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs RMCAB and SISAIRE source directories.
#' @param stations Upstream filtered stations, with the 2018 selection canonical.
#' @param quiet Suppress progress messages.
#' @return Partitioned dataset and the RMCAB/SISAIRE comparison sidecar directory.
bogota_prepare_pollution <- function(cfg, inputs, stations, quiet = FALSE) {
  processing_files(inputs, "pollution sources")
  processing_files(stations, "filtered stations")
  station_data <- sf::st_read(stations["stations_2018"], quiet = quiet)
  dataset <- bogota_process_stations_data_to_parquet(
    rmcab_folder = inputs["primary"], sisaire_folder = inputs["secondary"],
    stations_sf = station_data, tz = cfg$processing_tz,
    years = cfg$years, verbose = !quiet,
    out_dir = here::here(cfg$out_dir, "monitoring_stations"), out_name = "bogota_metro")
  city_pollution_outputs("bogota", cfg)
}

#' @param cfg City configuration, including processing years.
#' @param inputs Primary and supplementary pollution source directories.
#' @param stations Upstream filtered stations.
#' @param quiet Suppress progress messages.
#' @return Complete year-partitioned dataset path, without serializing an Arrow handle.
cdmx_prepare_pollution <- function(cfg, inputs, stations, quiet = FALSE) {
  processing_files(inputs, "pollution sources")
  processing_files(stations, "filtered stations")
  station_data <- sf::st_read(stations["stations"], quiet = quiet)
  dataset <- cdmx_merge_pollution_data(primary_data_dir = inputs["primary"],
    secondary_data_dir = inputs["secondary"], stations_sf = station_data,
    tz = cfg$processing_tz, years = cfg$years, verbose = !quiet, cleanup = FALSE,
    include_source_metadata = TRUE,
    out_dir = here::here(cfg$out_dir, "monitoring_stations"), out_name = "cdmx_metro")
  city_pollution_outputs("cdmx", cfg)
}

#' @param cfg City configuration, including processing years.
#' @param inputs Pollution source directory.
#' @param stations Upstream filtered stations.
#' @param quiet Suppress progress messages.
#' @return Complete year-partitioned dataset path.
santiago_prepare_pollution <- function(cfg, inputs, stations, quiet = FALSE) {
  processing_files(inputs, "pollution sources")
  processing_files(stations, "filtered stations")
  station_data <- sf::st_read(stations["stations"], quiet = quiet)
  dataset <- santiago_process_stations_data_to_parquet(
    data_folder = inputs["primary"], stations_sf = station_data, tz = cfg$processing_tz,
    years = cfg$years, out_dir = here::here(cfg$out_dir, "monitoring_stations"),
    out_name = "santiago_metro", verbose = !quiet)
  city_pollution_outputs("santiago", cfg)
}

#' @param cfg City configuration, including processing years.
#' @param inputs Pollution source directory.
#' @param stations Upstream filtered stations.
#' @param quiet Suppress progress messages.
#' @return Complete year-partitioned dataset path.
sao_paulo_prepare_pollution <- function(cfg, inputs, stations, quiet = FALSE) {
  processing_files(inputs, "pollution sources")
  processing_files(stations, "filtered stations")
  station_data <- sf::st_read(stations["stations"], quiet = quiet)
  dataset <- sp_process_stations_data_to_parquet(
    data_folder = inputs["primary"], stations_sf = station_data, tz = cfg$processing_tz,
    years = cfg$years, out_dir = here::here(cfg$out_dir, "monitoring_stations"),
    out_name = "sao_paulo_metro", verbose = !quiet)
  city_pollution_outputs("sao_paulo", cfg)
}

#' @param cfg City configuration.
#' @param inputs Preserved 2018 census directory.
#' @param geography Unused: Bogota census preparation selects municipality codes.
#' @param quiet Suppress progress messages.
#' @return The extracted 2018 checkpoint and the individual and collapsed census files.
bogota_prepare_census <- function(cfg, inputs, geography = NULL, quiet = FALSE) {
  preflight_census_inputs("bogota", inputs)
  extracted_dir <- here::here(cfg$out_dir, "census_extracted", "bogota", "CNPV_2018")
  extracted <- bogota_filter_census_2018(census_folder = inputs["census_2018"],
    out_dir = extracted_dir, overwrite = TRUE, quiet = quiet)
  out_dir <- here::here(cfg$out_dir, "census", "bogota_2018")
  census <- bogota_harmonize_census_2018_data(extract_paths = extracted,
    metro_codes = cfg$city_code_metro, out_dir = out_dir, quiet = quiet,
    return_data = FALSE)
  processing_files(c(census_2018_extracted = extracted_dir,
    census_2018_individual = here::here(out_dir, "census_2018_metro_individual.parquet"),
    census_2018_collapsed = here::here(out_dir, "census_2018_metro_collapsed.parquet")))
}

#' @param cfg City configuration.
#' @param inputs Preserved extended 2020 census directory.
#' @param geography Unused: census preparation selects municipality codes.
#' @param quiet Suppress progress messages.
#' @return Extracted checkpoints and individual/collapsed census files.
cdmx_prepare_census <- function(cfg, inputs, geography = NULL, quiet = FALSE) {
  preflight_census_inputs("cdmx", inputs)
  extracted_dir <- here::here(cfg$out_dir, "census_extracted", "cdmx", "CPV2020_EXTENDED")
  extracted <- mexico_filter_census(census_dir = inputs["census_2020"],
    out_dir = extracted_dir, overwrite = TRUE, quiet = quiet)
  archives <- list.files(inputs["census_2020"],
    pattern = "Censo2020_CA_.*_csv\\.zip$", ignore.case = TRUE)
  if (nrow(extracted) != length(archives) || !all(file.exists(extracted$file))) {
    stop("Incomplete extended census extraction")
  }
  out_dir <- here::here(cfg$out_dir, "census", "cdmx_extended_2020")
  census <- mexico_harmonize_census_data(extract_index = extracted,
    metro_codes = cfg$cities_in_metro, out_dir = out_dir, quiet = quiet,
    return_data = FALSE)
  processing_files(c(extracted = extracted_dir,
    individual = here::here(out_dir, "census_metro_individual_2020.parquet"),
    collapsed = here::here(out_dir, "collapse_metro_area_2020.parquet")))
}

#' @param cfg City configuration.
#' @param inputs Preserved 2017 census database.
#' @param geography Upstream 2017 census zones.
#' @param quiet Suppress progress messages.
#' @return The 2017 census products and the reproducible database working copy.
santiago_prepare_census <- function(cfg, inputs, geography, quiet = FALSE) {
  preflight_census_inputs("santiago", inputs)
  processing_files(geography, "geography")
  zones <- sf::st_read(geography["zones_2017"], quiet = quiet)
  out_2017 <- here::here(cfg$out_dir, "census", "santiago_2017")
  work_dir <- here::here(cfg$out_dir, "census_extracted", "santiago", "2017")
  census_2017 <- santiago_process_census_2017(sf_data = zones, match_col = "zona_id",
    out_dir = out_2017, source_db = inputs["census_2017"], work_dir = work_dir,
    quiet = quiet,
    return_data = FALSE)
  processing_files(c(working_copy = here::here(work_dir, "censo2017.duckdb"),
    individual_2017 = here::here(out_2017, "census_individual_2017.parquet"),
    collapsed_2017 = here::here(out_2017, "census_collapsed_2017.parquet")))
}

#' @param cfg City configuration.
#' @param inputs Preserved population Parquet.
#' @param geography Upstream 2010 weighting areas.
#' @param quiet Suppress progress messages.
#' @return Both individual and collapsed 2010 census products.
sao_paulo_prepare_census <- function(cfg, inputs, geography, quiet = FALSE) {
  processing_files(inputs, "census sources")
  processing_files(geography, "geography")
  weights <- sf::st_read(geography["weights"], quiet = quiet)
  out_dir <- here::here(cfg$out_dir, "census", "sao_paulo_2010")
  census <- sp_process_census_2010(sf_data = weights, out_dir = out_dir,
    source_file = inputs["census_2010"], quiet = quiet,
    return_data = FALSE)
  processing_files(c(individual = here::here(out_dir,
    "census_sp_individual_2010.parquet"),
    collapsed = here::here(out_dir, "census_sp_collapsed_2010.parquet")))
}
