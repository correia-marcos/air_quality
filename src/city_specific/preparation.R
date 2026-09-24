# Offline stage adapters. Transformations remain in the city modules.

#' @param cfg City configuration.
#' @param inputs Named preserved geographic sources.
#' @param quiet Suppress progress messages.
#' @return Five named geographic files, including both census vintages.
bogota_prepare_geography <- function(cfg, inputs, quiet = FALSE) {
  expected <- basename(city_processing_inputs("bogota", cfg)$geography)
  processing_files(inputs, "Bogotá geography sources")
  if (!all(expected %in% basename(inputs))) stop("Missing Bogotá geographic source files")
  files <- city_geographic_outputs("bogota", cfg)
  levels <- c("mpio_localidad", "mpio", "manzana", "mpio_localidad", "manzana")
  years <- c(2005, 2005, 2005, 2018, 2018)
  archives <- list("SHP_MGN2005_COLOMBIA.zip", "SHP_MGN2005_COLOMBIA.zip",
    "SHP_MGN2005_COLOMBIA.zip", "SHP_MGN2018_INTGRD_MPIO.zip",
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
#' @return Separate 2017 zones and 2024 municipal geography.
santiago_prepare_geography <- function(cfg, inputs, quiet = FALSE) {
  processing_files(inputs, "Santiago geographic sources")
  files <- city_geographic_outputs("santiago", cfg)
  zones <- santiago_prepare_metro_area_2017(metro_file = unname(inputs["metro_2017"]),
      zones_file = unname(inputs["zones_2017"]), count_file = unname(inputs["count_2017"]),
      quiet = quiet)
  metro <- santiago_prepare_metro_area_2024(source_zip = unname(inputs["zip"]),
      type = "gran_santiago", level = "mpio", keep_municipality = cfg$cities_in_metro,
      dissolve_by = "CUT", quiet = quiet)
  write_geopackage(x = zones, path = files["zones_2017"])
  write_geopackage(x = metro, path = files["metro_2024"])
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Named preserved geographic sources.
#' @param quiet Suppress progress messages.
#' @return Municipalities, census tracts and weighting areas for 2010.
sao_paulo_prepare_geography <- function(cfg, inputs, quiet = FALSE) {
  processing_files(inputs, "São Paulo geographic sources")
  files <- city_geographic_outputs("sao_paulo", cfg)
  for (i in 1:2) {
    geography <- sao_paulo_prepare_metro_area(
        source_zip = unname(inputs[c("municipalities", "tracts")][i]),
        level = c("mpio", "setor_censitario")[i], keep_municipality = cfg$cities_in_metro,
        quiet = quiet)
    write_geopackage(x = geography, path = files[i])
  }
  weights <- sao_paulo_prepare_weighting_areas(source_file = unname(inputs["weights"]),
      keep_municipality = cfg$cities_in_metro, quiet = quiet)
  write_geopackage(x = weights, path = files["weights"])
  processing_files(files)
}

#' @param cfg City configuration.
#' @param inputs Station locations and metadata sources.
#' @param geography Upstream geographic files.
#' @param quiet Suppress progress messages.
#' @return Both 2018 and 2005 filtered-station products.
bogota_prepare_stations <- function(cfg, inputs, geography, quiet = FALSE) {
  processing_files(inputs, "station sources")
  processing_files(geography, "geography")
  locations <- read.csv(inputs["locations"])
  files <- city_station_outputs("bogota", cfg)
  stations <- list()
  for (year in c(2018, 2005)) {
    metro <- sf::st_read(geography[paste0("metro_", year)], quiet = quiet)
    stations[[as.character(year)]] <- bogota_filter_stations_in_metro(
      rmcab_df = locations, metadata_dir = inputs["metadata"],
      radius_km = cfg$station_buffer_km,
      metro_area = metro, out_file = files[paste0("stations_", year)])
  }
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
#' @param inputs Named census archives/directories; aliases may select a vintage.
#' @param geography Unused: Bogota census preparation selects municipality codes.
#' @param quiet Suppress progress messages.
#' @return All extracted checkpoints and individual/collapsed census files.
bogota_prepare_census <- function(cfg, inputs, geography = NULL, quiet = FALSE) {
  preflight_census_inputs("bogota", inputs)
  files <- character()
  for (variant in intersect(c("extended", "basic"), names(inputs))) {
    extracted_dir <- here::here(cfg$out_dir, "census_extracted", "bogota",
      paste0("CG2005_", toupper(variant)))
    extracted <- bogota_filter_census_2005(census_zip = inputs[variant],
      out_dir = extracted_dir, overwrite = TRUE, quiet = quiet)
    out_dir <- here::here(cfg$out_dir, "census", paste0("bogota_", variant, "_2005"))
    census <- bogota_harmonize_census_2005_data(extract_list = extracted,
      is_extended = variant == "extended", metro_codes = cfg$city_code_metro,
      out_dir = out_dir, quiet = quiet,
      return_data = FALSE)
    outputs <- c(extracted = extracted_dir,
      individual = here::here(out_dir, paste0("census_metro_individual_", variant,
                                            ".parquet")),
      collapsed = here::here(out_dir, paste0("collapse_metro_area_", variant,
                                           ".parquet")))
    names(outputs) <- paste0(variant, "_2005_", names(outputs))
    files <- c(files, outputs)
  }
  if ("census_2018" %in% names(inputs)) {
    extracted_dir <- here::here(cfg$out_dir, "census_extracted", "bogota", "CNPV_2018")
    extracted <- bogota_filter_census_2018(census_folder = inputs["census_2018"],
      out_dir = extracted_dir, overwrite = TRUE, quiet = quiet)
    out_dir <- here::here(cfg$out_dir, "census", "bogota_2018")
    census <- bogota_harmonize_census_2018_data(extract_paths = extracted,
      metro_codes = cfg$city_code_metro, out_dir = out_dir, quiet = quiet,
      return_data = FALSE)
    files <- c(files, census_2018_extracted = extracted_dir,
      census_2018_individual = here::here(out_dir,
        "census_2018_metro_individual.parquet"),
      census_2018_collapsed = here::here(out_dir, "census_2018_metro_collapsed.parquet"))
  }
  processing_files(files)
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
#' @param inputs Preserved 2017 database and 2024 source directory.
#' @param geography Upstream geography, with distinct keys for each vintage.
#' @param quiet Suppress progress messages.
#' @return Both census vintages and the reproducible database working copy.
santiago_prepare_census <- function(cfg, inputs, geography, quiet = FALSE) {
  preflight_census_inputs("santiago", inputs)
  processing_files(geography, "geography")
  zones <- sf::st_read(geography["zones_2017"], quiet = quiet)
  metro <- sf::st_read(geography["metro_2024"], quiet = quiet)
  out_2017 <- here::here(cfg$out_dir, "census", "santiago_2017")
  out_2024 <- here::here(cfg$out_dir, "census", "santiago_2024")
  work_dir <- here::here(cfg$out_dir, "census_extracted", "santiago", "2017")
  census_2017 <- santiago_process_census_2017(sf_data = zones, match_col = "zona_id",
    out_dir = out_2017, source_db = inputs["census_2017"], work_dir = work_dir,
    quiet = quiet,
    return_data = FALSE)
  census_2024 <- santiago_process_census_2024(census_dir = inputs["census_2024"],
    sf_data = metro, match_col = "CUT", out_dir = out_2024,
    overwrite = TRUE, quiet = quiet,
    return_data = FALSE)
  members <- utils::unzip(here::here(unname(inputs["census_2024"]),
    "chile_census_2024_people.zip"), list = TRUE)$Name
  csv <- grep("\\.csv$", members, value = TRUE, ignore.case = TRUE)[1]
  processing_files(c(working_copy = here::here(work_dir, "censo2017.duckdb"),
    extracted_2024 = here::here(out_2024, basename(csv)),
    individual_2017 = here::here(out_2017, "census_individual_2017.parquet"),
    collapsed_2017 = here::here(out_2017, "census_collapsed_2017.parquet"),
    individual_2024 = here::here(out_2024, "census_santiago_individual_2024.parquet"),
    collapsed_2024 = here::here(out_2024, "census_santiago_collapsed_2024.parquet")))
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
