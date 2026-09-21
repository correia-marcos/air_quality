# ============================================================================================
# IDB: Air monitoring — three-city geographic-resolution inputs
# ============================================================================================
#' @Goal: Prepare frozen populations, city-specific crosswalks and Design B matrices.
#' @Description: Uses current derived inputs, records their hashes, and writes only
#'   optional resolution directories. Does not acquire or replace original sources.
#' @Summary: Explicit city inputs; population cells; geography audit; dissolved matrices.
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_resolution.R"))

cities <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) stop("This preparation script takes no arguments.")
for (city in cities) {
  message("Preparing resolution inputs: ", city)
  city_name <- sub("_[0-9]+$", "", city)
  geo_dir <- here::here("data", "interim", "geospatial_data", city_name)
  census_dir <- here::here("data", "interim", "census", city)
  idw_dir <- here::here("data", "processed", "idw_estimates", city)
  out <- here::here("data", "interim", "resolution_sensitivity", city)
  processed <- here::here("data", "processed", "resolution_sensitivity", city)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(processed, recursive = TRUE, showWarnings = FALSE)

  # City-specific geography is explicit; no common hierarchy is imposed.
  if (city == "bogota_2018") {
    geo_file <- "bogota_area_metro_census_tracts_2018.gpkg"
    station_file <- "bogota_2018_stations_buffer_metro.gpkg"
    census_file <- "census_2018_metro_collapsed.parquet"
    id_col <- "GEO_ID"; fine_width <- 22L
    levels <- c("fine", "seccion", "sector", "localidad", "municipio")
    labels <- c("Fine census unit", "Seccion", "Sector", "Localidad/municipio",
                "Municipio")
    widths <- c(22L, 20L, 18L, NA_integer_, 5L)
    nesting <- list(c("fine", "seccion"), c("seccion", "sector"),
                    c("sector", "municipio"), c("fine", "localidad"),
                    c("localidad", "municipio"))
    missing_sources <- character()
  } else if (city == "santiago_2017") {
    geo_file <- "gran_santiago_zonas_2017.gpkg"
    station_file <- "gran_santiago_stations_buffer_metro_2017.gpkg"
    census_file <- "census_collapsed_2017.parquet"
    id_col <- "zona_id"; fine_width <- 11L
    levels <- c("fine", "distrito", "comuna")
    labels <- c("Zona censal", "Distrito censal", "Comuna")
    widths <- c(11L, 7L, 5L)
    nesting <- list(c("fine", "distrito"), c("distrito", "comuna"))
    missing_sources <- file.path("data/downloads/santiago/metro_area/2017",
      paste0("GRAN_SANTIAGO_13_", c("metro.geojson", "zonas.geojson", "count.json")))
  } else {
    geo_file <- "sao_paulo_metro_2010_weighting_areas.gpkg"
    station_file <- "sao_paulo_stations_buffer_metro_2010.gpkg"
    census_file <- "census_sp_collapsed_2010.parquet"
    id_col <- "code_weighting"; fine_width <- 13L
    levels <- c("fine", "municipio")
    labels <- c("Area de ponderacao", "Municipio")
    widths <- c(13L, 7L)
    nesting <- list(c("fine", "municipio"))
    missing_sources <- "data/downloads/sao_paulo/metro_area/sp_weighting_areas_2010.rds"
  }
  paths <- c(geometry = file.path(geo_dir, geo_file),
    stations = file.path(geo_dir, station_file),
    census = file.path(census_dir, census_file),
    groups = file.path(idw_dir, paste0(city, "_indiv_groups.parquet")),
    exposure = file.path(idw_dir, paste0(city, "_3km_idw_exposure.parquet")),
    distances = here::here("data", "processed", "distances_matrices", city,
                           "matrix_geo_station_distances.parquet"),
    pollution = here::here("data", "processed", "monitoring_stations_outliers",
                           paste0(city_name, "_metro_clean"), "year=2023",
                             "data.parquet"))
  if (city == "bogota_2018") paths <- c(paths, locality = file.path(geo_dir,
    "bogota_manzana_localidad_crosswalk.parquet"))
  if (any(!file.exists(paths))) stop("Missing input: ", paths[!file.exists(paths)][1])
  manifest <- data.table(input = names(paths), path = sub(paste0(here::here(), "/"),
    "", paths, fixed = TRUE), bytes = file.info(paths)$size,
    md5 = unname(tools::md5sum(paths)))
  # Refuse an implicit change of input vintage on a later run.
  manifest_path <- file.path(out, "input_manifest.csv")
  if (file.exists(manifest_path) &&
      !isTRUE(all.equal(fread(manifest_path), manifest, check.attributes = FALSE))) {
    stop("Frozen input manifest changed for ", city, ". Review the input vintage first.")
  }
  resolution_write(manifest, file.path(out, "input_manifest"))
  resolution_write(data.table(path = missing_sources,
    exists = file.exists(here::here(missing_sources))), file.path(out, "source_status"))

  census <- as.data.table(arrow::read_parquet(paths["census"]))
  individual <- as.data.table(arrow::read_parquet(paths["groups"]))
  stopifnot(all(individual$adult == 1), !anyDuplicated(census$geo_id),
            all(nchar(census$geo_id) == fine_width))
  population <- individual[, .(pop = sum(person_weight, na.rm = TRUE)), by = geo_id]
  cells <- individual[!is.na(edu_quintile) & is.finite(person_weight) & person_weight > 0,
    .(person_weight = sum(person_weight)), by = .(geo_id, edu_quintile)]
  rm(individual); gc(verbose = FALSE)
  resolution_write(population, file.path(out, "adult_population"))
  resolution_write(cells, file.path(out, "individual_quintile_cells"))
  arrow::write_parquet(census, file.path(out, "census_education.parquet"))
  geo <- sf::st_read(paths["geometry"], quiet = TRUE)
  raw_id <- as.character(geo[[id_col]])
  geo$geo_id <- reconcile_geo_ids(raw_id, census$geo_id, quiet = TRUE)
  if (city == "bogota_2018") {
    # Preserve natural rural prefixes even for geometric units without census adults.
    geo$geo_id <- stringi::stri_pad_right(geo$geo_id, 22L, pad = "0")
  }
  stopifnot(!anyDuplicated(geo$geo_id), all(nchar(geo$geo_id) == fine_width),
            all(sf::st_is_valid(geo)), !any(sf::st_is_empty(geo)))
  if (city == "sao_paulo_2010") {
    stopifnot(all(substr(geo$geo_id, 1, 7) == as.character(geo$code_muni)))
  }
  ledger <- data.table(raw_id = raw_id, geo_id = geo$geo_id,
                       census_match = geo$geo_id %chin% census$geo_id)
  resolution_write(ledger, file.path(out, "identifier_ledger"))
  # Report source and processed populations separately from pollution coverage.
  person_path <- file.path(census_dir, sub("collapsed", "individual", census_file,
                                          fixed = TRUE))
  population_paths <- c(processed_people = person_path)
  if (city == "santiago_2017") population_paths <- c(population_paths,
    source_census = here::here("data", "downloads", "santiago", "census", "2017",
                              "censo2017.duckdb"))
  population_manifest <- data.table(input = names(population_paths),
    path = sub(paste0(here::here(), "/"), "", population_paths, fixed = TRUE),
    md5 = unname(tools::md5sum(population_paths)))
  pop_manifest_file <- file.path(out, "population_manifest.csv")
  if (file.exists(pop_manifest_file)) {
    stopifnot(isTRUE(all.equal(fread(pop_manifest_file), population_manifest,
                              check.attributes = FALSE)))
  }
  resolution_write(population_manifest, file.path(out, "population_manifest"))
  people <- as.data.table(arrow::read_parquet(person_path,
    col_select = c("adult", "person_weight")))
  denominators <- data.table(city_id = city,
    processed_residents = sum(people$person_weight, na.rm = TRUE),
    processed_adults = people[adult == 1, sum(person_weight, na.rm = TRUE)],
    education_reporting_adults = sum(cells$person_weight),
    source_residents = NA_real_, source_adults = NA_real_,
    source_count_status = "not_separately_rebuilt_from_raw",
    adult_rule = "age_25_plus",
    weights = if (city == "sao_paulo_2010") "IBGE_expansion_weights" else "unit_weights")
  stopifnot(abs(denominators$processed_adults - sum(population$pop)) < 1e-5)
  rm(people); gc(verbose = FALSE)
  if (city == "santiago_2017") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = population_paths["source_census"],
                          read_only = TRUE)
    ids_sql <- paste(DBI::dbQuoteString(con, geo$geo_id), collapse = ",")
    raw_counts <- DBI::dbGetQuery(con, paste0(
      "SELECT COUNT(*) AS residents, ",
      "SUM(CASE WHEN p.p09 >= 25 THEN 1 ELSE 0 END) AS adults ",
      "FROM personas p JOIN hogares h USING (hogar_ref_id) ",
      "JOIN viviendas v USING (vivienda_ref_id) ",
      "JOIN zonas z USING (zonaloc_ref_id) WHERE z.geocodigo IN (", ids_sql, ")"))
    DBI::dbDisconnect(con, shutdown = TRUE)
    denominators[, `:=`(source_residents = as.numeric(raw_counts$residents),
      source_adults = as.numeric(raw_counts$adults),
      source_count_status = "read_only_2017_source_query_selected_zones")]
  }
  resolution_write(denominators, file.path(processed, "population_denominators"))

  resolution_write(population[!geo_id %chin% geo$geo_id],
                   file.path(out, "adults_without_fine_geometry"))
  all_ids <- union(geo$geo_id, census$geo_id)
  keys <- rbindlist(lapply(seq_along(levels), function(i) {
    data.table(geo_id = all_ids, level = levels[i],
      parent_id = if (is.na(widths[i])) NA_character_ else
        substr(all_ids, 1L, widths[i]))
  }))
  if (city == "bogota_2018") {
    cw <- as.data.table(arrow::read_parquet(paths["locality"]))
    cw[, geo_id := reconcile_geo_ids(geo_id, census$geo_id, quiet = TRUE)]
    cw[, geo_id := stringi::stri_pad_right(geo_id, 22L, pad = "0")]
    stopifnot(!anyDuplicated(cw$geo_id))
    keys[level == "localidad", parent_id := cw$loc_id[match(geo_id, cw$geo_id)]]
  }
  nest <- resolution_validate_keys(keys, nesting)
  stopifnot(all(nest$crossing_children == 0))
  resolution_write(nest, file.path(out, "nesting"))
  if (city == "bogota_2018") resolution_write(resolution_validate_keys(keys,
    list(c("seccion", "localidad"), c("sector", "localidad"))),
    file.path(out, "non_nested_branches"))
  resolution_write(keys, file.path(out, "crosswalk"))
  ladder <- data.table(level = levels, label = labels, fine_to_coarse = seq_along(levels),
    bootstrap = levels %chin% c("seccion", "sector", "distrito"),
    secondary_inference = levels %chin% c("fine", "seccion", "sector", "distrito"))
  resolution_write(ladder, file.path(out, "levels"))
  stations <- sf::st_read(paths["stations"], quiet = TRUE)
  fixed_crs <- NULL
  fine_points <- NULL
  audit <- list()
  for (lv in levels) {
    message(city, ": geometry and matrix at ", lv)
    k <- keys[level == lv, .(geo_id, parent_id)]
    polygons <- if (lv == "fine") geo["geo_id"] else resolution_dissolve(geo, k)
    sf::st_write(polygons, file.path(out, paste0(lv, "_polygons.gpkg")),
                 delete_dsn = TRUE, quiet = TRUE)
    bdir <- file.path(processed, "B", lv)
    matrix <- compute_distance_matrices(stations, "station_name", polygons, "geo_id",
      bdir, "matrix", evaluation_crs = fixed_crs, return_points = TRUE, quiet = TRUE)
    if (lv == "fine") {
      fixed_crs <- matrix$evaluation_crs
      fine_points <- matrix$representative_points
      writeLines(fixed_crs$wkt, file.path(out, "evaluation_crs.wkt"))
      old <- as.data.table(arrow::read_parquet(paths["distances"]))
      old[, geo_id := reconcile_geo_ids(geo_id, geo$geo_id, quiet = TRUE)]
      if (city == "bogota_2018") {
        old[, geo_id := stringi::stri_pad_right(geo_id, 22L, pad = "0")]
      }
      fresh <- merge(old, matrix$geo_station_matrix, by = c("geo_id", "station_id"))
      error <- max(abs(fresh$distance_km.x - fresh$distance_km.y))
      stopifnot(nrow(fresh) == nrow(old), error < 1e-8)
      resolution_write(data.table(check = "fresh_fine_distance_anchor", max_error = error,
        pairs = nrow(fresh), passed = TRUE), file.path(out, "distance_anchor"))
    }
    points <- matrix$representative_points
    sf::st_write(points, file.path(out, paste0(lv, "_points.gpkg")),
                 delete_dsn = TRUE, quiet = TRUE)
    # Internal-point validity is checked in the same planar lon/lat construction.
    projected_poly <- sf::st_transform(polygons, fixed_crs)
    projected_point <- sf::st_transform(points, fixed_crs)
    intersections <- sf::st_intersects(projected_point, projected_poly)
    own_polygon <- match(points$geo_id, polygons$geo_id)
    inside <- vapply(seq_along(intersections), function(i) {
      own_polygon[i] %in% intersections[[i]]
    }, logical(1))
    stopifnot(all(inside))
    kp <- k[!is.na(parent_id) & geo_id %chin% fine_points$geo_id]
    from <- sf::st_transform(fine_points[match(kp$geo_id, fine_points$geo_id), ],
      fixed_crs)
    to <- projected_point[match(kp$parent_id, projected_point$geo_id), ]
    displacement <- as.numeric(sf::st_distance(from, to, by_element = TRUE)) / 1000
    movement <- data.table(geo_id = kp$geo_id, parent_id = kp$parent_id,
                           displacement_km = displacement)
    resolution_write(movement, file.path(out, paste0(lv, "_movement")))
    child_area <- data.table(geo_id = geo$geo_id,
      child_area_km2 = as.numeric(sf::st_area(sf::st_transform(geo, fixed_crs))) / 1e6)
    child_area <- merge(child_area, k[!is.na(parent_id)], by = "geo_id")
    area_sums <- child_area[, .(sum_child_area_km2 = sum(child_area_km2)),
                             by = parent_id]
    area <- data.table(parent_id = polygons$geo_id,
      area_km2 = as.numeric(sf::st_area(projected_poly)) / 1e6)
    area <- merge(area, area_sums, by = "parent_id", all.x = TRUE)
    area[, overlap_or_repair_km2 := sum_child_area_km2 - area_km2]
    pp <- merge(population, k, by = "geo_id", all.x = TRUE)
    cp <- merge(cells, k, by = "geo_id", all.x = TRUE)
    unit <- pp[!is.na(parent_id), .(adult_population = sum(pop)), by = parent_id]
    unit <- merge(unit, cp[!is.na(parent_id),
      .(education_population = sum(person_weight)),
                           by = parent_id], by = "parent_id", all.x = TRUE)
    unit <- merge(area, unit, by = "parent_id", all = TRUE)
    unit[is.na(adult_population), adult_population := 0]
    unit[is.na(education_population), education_population := 0]
    resolution_write(unit, file.path(out, paste0(lv, "_unit_population")))
    p <- unit[adult_population > 0, adult_population]
    audit[[lv]] <- data.table(city_id = city, level = lv, geometry_units = nrow(polygons),
      adult_units = length(p), adults = sum(p),
        education_adults = sum(unit$education_population),
      missing_parent_units = pp[is.na(parent_id), .N],
      missing_parent_adults = pp[is.na(parent_id), sum(pop)],
      population_min = min(p), population_p10 = unname(quantile(p, .1)),
      population_median = median(p), population_mean = mean(p),
      population_p90 = unname(quantile(p, .9)), population_max = max(p),
      population_weighted_size = sum(p^2) / sum(p),
        max_population_share = max(p) / sum(p),
      area_p10_km2 = unname(quantile(unit$area_km2, .1, na.rm = TRUE)),
      area_median_km2 = median(unit$area_km2, na.rm = TRUE),
      area_p90_km2 = unname(quantile(unit$area_km2, .9, na.rm = TRUE)),
      area_union_difference_km2 = sum(area$overlap_or_repair_km2),
       source_year = as.integer(sub(".*_", "", city)),
      geometry_valid = all(sf::st_is_valid(polygons)),
        representative_points_internal = all(inside))
  }
  resolution_write(rbindlist(audit), file.path(processed, "geographic_audit"))
}
