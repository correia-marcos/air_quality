# Offline preparation contracts shared by manual scripts and targets.

#' @param paths Cached file paths; targets does not preserve their names.
#' @param contract Named paths declaring each file's role for the consuming stage.
#' @return Named paths in contract order; rejects a different or incomplete file set.
processing_file_roles <- function(paths, contract) {
  index <- match(normalizePath(contract, mustWork = TRUE),
                 normalizePath(paths, mustWork = TRUE))
  if (anyNA(index) || length(paths) != length(contract)) {
    stop("Cached files do not match the declared processing contract")
  }
  setNames(unname(paths[index]), names(contract))
}

#' @param paths Named files/directories required by a stage.
#' @param label Stage or input description for an actionable error.
#' @return The same paths, after checking existence and nonempty directories.
processing_files <- function(paths, label = "processing outputs") {
  if (!is.character(paths) || !length(paths) || anyNA(paths)) {
    stop("No files declared for ", label)
  }
  missing <- !file.exists(paths)
  directories <- which(dir.exists(paths))
  for (i in directories) {
    missing[i] <- !length(list.files(paths[i], recursive = TRUE, all.files = TRUE,
                                    no.. = TRUE))
  }
  if (any(missing)) {
    stop("Missing or empty ", label, ": ", paste(paths[missing], collapse = ", "),
         ". Supply preserved inputs; processing does not acquire data.")
  }
  paths
}

#' @param id Stable city slug.
#' @param cfg City paths and scientific configuration.
#' @return Named source paths grouped by processing stage; no files are created.
city_processing_inputs <- function(id, cfg) {
  root <- cfg$dl_dir
  metro <- here::here(root, "metro_area")
  census <- here::here(root, "census")
  locations <- here::here(root, "ground_stations_geolocation")
  metadata <- here::here(root, "stations_metadata")
  primary <- here::here(root, "ground_stations")
  switch(id,
    bogota = list(
      geography = c(zip_2005 = here::here(metro, "SHP_MGN2005_COLOMBIA.zip"),
        zip_mpio = here::here(metro, "SHP_MGN2018_INTGRD_MPIO.zip"),
        zip_manz = here::here(metro, "SHP_MGN2018_INTGRD_MANZ.zip"),
        zip_seccr = here::here(metro, "SHP_MGN2018_INTGRD_SECCR.zip"),
        localities = here::here(metro, "bogota_loca.gpkg")),
      stations_filter = c(locations = here::here(locations,
        "bogota_stations_location.csv"), metadata = metadata),
      pollution_parquet = c(primary = primary,
        secondary = here::here(root, "metro_ground_stations_hourly")),
      census = c(extended = here::here(census, "CG2005_AMPLIADO.zip"),
        basic = here::here(census, "CG2005_BASICO.zip"), census_2018 = census)),
    cdmx = list(
      geography = c(zip = here::here(metro, "mg_2024_integrado.zip")),
      stations_filter = c(locations = here::here(locations,
        "all_station_location.csv")),
      pollution_parquet = c(primary = primary,
        secondary = here::here(root, "ground_stations_raw_missing_data")),
      census = c(census_2020 = census)),
    santiago = list(
      geography = c(zip = here::here(metro, "Cartografia_censo2024_Pais.zip"),
        metro_2017 = here::here(metro, "2017", "GRAN_SANTIAGO_13_metro.geojson"),
        zones_2017 = here::here(metro, "2017", "GRAN_SANTIAGO_13_zonas.geojson"),
        count_2017 = here::here(metro, "2017", "GRAN_SANTIAGO_13_count.json")),
      stations_filter = c(locations = here::here(metadata,
        "SINCA_metadata_stations_20260113_1616.csv")),
      pollution_parquet = c(primary = primary),
      census = c(census_2017 = here::here(census, "2017", "censo2017.duckdb"),
        census_2024 = here::here(census, "2024"))),
    sao_paulo = list(
      geography = c(municipalities = here::here(metro, "sp_municipios.zip"),
        tracts = here::here(metro, "sp_setores_censitarios.zip"),
        weights = here::here(metro, "sp_weighting_areas_2010.rds")),
      stations_filter = c(locations = here::here(metadata, "stations_metadata.csv")),
      pollution_parquet = c(primary = primary),
      census = c(census_2010 = here::here(census, "2010_population.parquet"))),
    stop("Unknown city: ", id))
}

#' @param inputs Explicit named stage paths, or NULL for configured sources.
#' @param id,cfg City slug and configuration.
#' @return Complete named input contract with supplied stage overrides.
resolve_city_inputs <- function(id, cfg, inputs = NULL) {
  defaults <- city_processing_inputs(id, cfg)
  if (is.null(inputs)) return(defaults)
  if (!is.list(inputs) || is.null(names(inputs)) || anyDuplicated(names(inputs)) ||
      any(!names(inputs) %in% names(defaults))) stop("Unknown input stage")
  for (stage in names(inputs)) {
    paths <- inputs[[stage]]
    if (!is.character(paths) || is.null(names(paths)) || anyDuplicated(names(paths)) ||
        !setequal(names(paths), names(defaults[[stage]]))) {
      stop("Supply every named source for stage: ", stage)
    }
    defaults[[stage]] <- paths[names(defaults[[stage]])]
  }
  defaults
}

#' @param id City slug.
#' @param steps Common stage names or the historical Bogota census aliases.
#' @return Ordered prerequisite closure and selected census variants.
city_processing_steps <- function(id, steps) {
  order <- c("geography", "stations_filter", "pollution_parquet", "census")
  aliases <- if (id == "bogota") c("census_2005", "census_2018") else character()
  if (!is.character(steps) || !length(steps) || anyNA(steps) ||
      any(!steps %in% c(order, aliases))) stop("Unknown or empty processing steps")
  variants <- if ("census" %in% steps) c("census_2005", "census_2018") else
    intersect(steps, aliases)
  if (length(intersect(steps, aliases))) steps <- c(steps, "census")
  if ("pollution_parquet" %in% steps) steps <- c(steps, "stations_filter")
  if ("stations_filter" %in% steps ||
      ("census" %in% steps && id %in% c("santiago", "sao_paulo"))) {
    steps <- c(steps, "geography")
  }
  list(stages = order[order %in% steps], variants = variants)
}

#' @param id City slug.
#' @param inputs Declared census files/directories for the selected variants.
#' @return Inputs after source completeness checks; no extraction or writing occurs.
preflight_census_inputs <- function(id, inputs) {
  processing_files(inputs, "census sources")
  if (id == "bogota" && "census_2018" %in% names(inputs)) {
    for (department in c("Bogot", "Cundinamarca")) {
      files <- list.files(inputs["census_2018"],
        pattern = paste0("2018.*", department, ".*\\.zip$"), ignore.case = TRUE)
      if (!length(files)) stop("Missing preserved 2018 census archive: ", department)
    }
  }
  if (id == "cdmx") {
    archives <- list.files(inputs["census_2020"],
      pattern = "Censo2020_CA_.*_csv\\.zip$", ignore.case = TRUE, full.names = TRUE)
    if (!length(archives)) stop("Missing preserved Censo2020_CA_*_csv.zip archives")
    for (archive in archives) {
      members <- utils::unzip(archive, list = TRUE)$Name
      if (!any(grepl("Personas[0-9]+\\.[Cc][Ss][Vv]$", members))) {
        stop("Census archive has no Personas CSV: ", archive)
      }
    }
  }
  if (id == "santiago") {
    archive <- here::here(unname(inputs["census_2024"]), "chile_census_2024_people.zip")
    processing_files(archive, "2024 census archive")
    if (!any(grepl("\\.csv$", utils::unzip(archive, list = TRUE)$Name,
                   ignore.case = TRUE))) stop("2024 census archive has no CSV")
  }
  inputs
}

#' @param id,cfg City slug and output configuration.
#' @return Named geographic and station outputs, without creating files.
city_geographic_outputs <- function(id, cfg) {
  names <- switch(id,
    bogota = c(metro_2005 = "bogota_area_metro_2005.gpkg",
      municipalities_2005 = "bogota_area_metro_municipalities_2005.gpkg",
      tracts_2005 = "bogota_area_metro_census_tracts_2005.gpkg",
      metro_2018 = "bogota_area_metro_2018.gpkg",
      tracts_2018 = "bogota_area_metro_census_tracts_2018.gpkg"),
    cdmx = c(municipalities = "cdmx_area_metro_municipalities_2024.gpkg",
      metro = "cdmx_area_metro_2024.gpkg"),
    santiago = c(zones_2017 = "gran_santiago_zonas_2017.gpkg",
      metro_2024 = "gran_santiago_area_2024.gpkg"),
    sao_paulo = c(municipalities = "sao_paulo_metro_2010.gpkg",
      tracts = "sao_paulo_metro_2010_census_tracts.gpkg",
      weights = "sao_paulo_metro_2010_weighting_areas.gpkg"))
  setNames(here::here(cfg$out_dir, "geospatial_data", id, unname(names)), names(names))
}

#' @param id,cfg City slug and output configuration.
#' @return Named filtered-station paths, without creating files.
city_station_outputs <- function(id, cfg) {
  files <- switch(id,
    bogota = c(stations_2018 = "bogota_2018_stations_buffer_metro.gpkg",
      stations_2005 = "bogota_2005_stations_buffer_metro.gpkg"),
    cdmx = c(stations = "cdmx_stations_buffer_metro.gpkg"),
    santiago = c(stations = "gran_santiago_stations_buffer_metro_2017.gpkg"),
    sao_paulo = c(stations = "sao_paulo_stations_buffer_metro_2010.gpkg"))
  setNames(here::here(cfg$out_dir, "geospatial_data", id, unname(files)), names(files))
}

#' @param id,cfg City slug and output configuration.
#' @return Validated complete partitioned dataset and persistent diagnostic sidecars.
city_pollution_outputs <- function(id, cfg) {
  root <- here::here(cfg$out_dir, "monitoring_stations")
  dataset <- here::here(root, paste0(id, "_metro_dataset"))
  paths <- c(dataset = dataset)
  if (id == "bogota") paths <- c(paths,
    comparison = here::here(root, "bogota_rmcab_sisaire_comparison"))
  processing_files(paths)
  partitions <- list.files(dataset, pattern = "\\.parquet$", recursive = TRUE)
  if (!length(partitions) || any(!grepl("^year=[0-9]{4}/", partitions))) {
    stop("Expected nonempty year-partitioned Parquet dataset: ", dataset)
  }
  paths
}

#' @param id,cfg City slug and configuration.
#' @param steps Selected stages including prerequisites.
#' @param inputs Explicit source paths or NULL for configured sources.
#' @param quiet Suppress progress messages.
#' @return All stage-owned output paths; errors propagate immediately.
run_city_processing <- function(id, cfg, steps, inputs = NULL, quiet = FALSE) {
  selection <- city_processing_steps(id, steps)
  inputs <- resolve_city_inputs(id, cfg, inputs)
  if (id == "bogota" && "census" %in% selection$stages) {
    keys <- c(if ("census_2005" %in% selection$variants) c("extended", "basic"),
              if ("census_2018" %in% selection$variants) "census_2018")
    inputs$census <- inputs$census[keys]
  }
  for (stage in selection$stages) {
    processing_files(inputs[[stage]], paste(id, stage, "inputs"))
    if (stage == "census") preflight_census_inputs(id, inputs$census)
  }
  functions <- switch(id,
    bogota = list(bogota_prepare_geography, bogota_prepare_stations,
                  bogota_prepare_pollution, bogota_prepare_census),
    cdmx = list(cdmx_prepare_geography, cdmx_prepare_stations,
                cdmx_prepare_pollution, cdmx_prepare_census),
    santiago = list(santiago_prepare_geography, santiago_prepare_stations,
                    santiago_prepare_pollution, santiago_prepare_census),
    sao_paulo = list(sao_paulo_prepare_geography, sao_paulo_prepare_stations,
                     sao_paulo_prepare_pollution, sao_paulo_prepare_census))
  names(functions) <- c("geography", "stations_filter", "pollution_parquet", "census")
  outputs <- list()
  for (stage in selection$stages) {
    if (!quiet) message("[", id, "] ", stage)
    args <- list(cfg = cfg, inputs = inputs[[stage]], quiet = quiet)
    if (stage %in% c("stations_filter", "census")) {
      args$geography <- outputs$geography
    }
    if (stage == "pollution_parquet") args$stations <- outputs$stations_filter
    outputs[[stage]] <- if (quiet) suppressMessages(do.call(functions[[stage]], args)) else
      do.call(functions[[stage]], args)
  }
  outputs
}
