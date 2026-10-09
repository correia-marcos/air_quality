# sources for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_prepare_sources <- function(city) {
  census_file <- NULL
  id_col <- NULL
  fine_width <- NULL
  levels <- NULL
  labels <- NULL
  widths <- NULL
  nesting <- NULL
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
  list(
    census_dir = census_dir,
    out = out,
    processed = processed,
    census_file = census_file,
    id_col = id_col,
    fine_width = fine_width,
    levels = levels,
    labels = labels,
    widths = widths,
    nesting = nesting,
    paths = paths
  )
}

# population for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param census_dir Named input or setting from the preceding operation.
#' @param out Named input or setting from the preceding operation.
#' @param processed Named input or setting from the preceding operation.
#' @param census_file Named input or setting from the preceding operation.
#' @param id_col Named input or setting from the preceding operation.
#' @param fine_width Named input or setting from the preceding operation.
#' @param paths Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_prepare_population <- function(
  city,
  census_dir,
  out,
  processed,
  census_file,
  id_col,
  fine_width,
  paths) {
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
  list(
    census = census,
    population = population,
    cells = cells,
    geo = geo,
    denominators = denominators
  )
}

# crosswalks for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param out Named input or setting from the preceding operation.
#' @param levels Named input or setting from the preceding operation.
#' @param labels Named input or setting from the preceding operation.
#' @param widths Named input or setting from the preceding operation.
#' @param nesting Named input or setting from the preceding operation.
#' @param paths Named input or setting from the preceding operation.
#' @param census Named input or setting from the preceding operation.
#' @param population Named input or setting from the preceding operation.
#' @param geo Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_prepare_crosswalks <- function(
  city,
  out,
  levels,
  labels,
  widths,
  nesting,
  paths,
  census,
  population,
  geo) {
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
  list(
    keys = keys,
    ladder = ladder,
    stations = stations
  )
}

# matrices for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param out Named input or setting from the preceding operation.
#' @param processed Named input or setting from the preceding operation.
#' @param levels Named input or setting from the preceding operation.
#' @param paths Named input or setting from the preceding operation.
#' @param population Named input or setting from the preceding operation.
#' @param cells Named input or setting from the preceding operation.
#' @param geo Named input or setting from the preceding operation.
#' @param keys Named input or setting from the preceding operation.
#' @param stations Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_prepare_matrices <- function(
  city,
  out,
  processed,
  levels,
  paths,
  population,
  cells,
  geo,
  keys,
  stations) {
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
    matrix <- compute_distance_matrices(
      stations_sf = stations, station_id_col = "station_name",
      geo_sf = polygons, geo_id_col = "geo_id",
      evaluation_crs = fixed_crs, return_points = TRUE, quiet = TRUE)
    matrix_files <- write_distance_matrices(result = matrix, out_dir = bdir)
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
  list(
    audit = audit
  )
}

# inputs for the preserved resolution specification.
#' @param resolution_args Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_inputs <- function(resolution_args) {
  set.seed(20260910)

  # Define input and output folders
  dir_idw    <- here::here("data", "processed", "idw_estimates")
  dir_census <- here::here("data", "interim", "census")
  dir_geo    <- here::here("data", "interim", "geospatial_data", "bogota")
  dir_out    <- here::here("data", "processed", "resolution_sensitivity")
  reference_dir <- sub("^--output-dir=", "", grep("^--output-dir=", resolution_args,
                                               value = TRUE))
  if (length(reference_dir)) dir_out <- here::here(reference_dir)

  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

  # The specification this analysis holds fixed; only the geography below varies
  city_id       <- "bogota_2018"
  analysis_year <- 2023L
  buffer_km     <- 3L
  n_groups      <- 5L
  base_group    <- 5L
  n_boot        <- 500L

  # The manzana-level artifacts written by estimate_idw.R
  exposure_manzana <- read_idw_artifact(dir_idw, city_id, "idw_exposure", buffer_km)
  individual       <- read_idw_artifact(dir_idw, city_id, "indiv_groups")

  # The collapsed census supplies the level-aggregated education of Design C
  census_collapsed <- data.table::as.data.table(arrow::read_parquet(
    here::here(dir_census, city_id, "census_2018_metro_collapsed.parquet")))

  # Manzana -> localidad/municipio, the one level the DANE code cannot express
  crosswalk <- data.table::as.data.table(arrow::read_parquet(
    here::here(dir_geo, "bogota_manzana_localidad_crosswalk.parquet")))

  exposure_manzana <- exposure_manzana[year == analysis_year]

  # The spatial layer stores rural-sector ids at their natural width and the census zero-fills
  # them to 22 characters, so the crosswalk arrives spelling every rural unit differently from
  # the exposure table. Reconciling against the census recovers them; without this the ladder
  # silently loses every clase-3 unit at the localidad rung only.
  crosswalk[, geo_id := reconcile_geo_ids(geo_id, census_collapsed$geo_id,
    label = "crosswalk")]

  # The regressions weight by adult population, so the ladder aggregates by the same quantity.
  manzana_pop <- individual[, .(pop = sum(person_weight, na.rm = TRUE)), by = geo_id]

  # Individuals collapse to geo-by-quintile cells inside the estimator anyway, so carrying one
  # row per cell instead of 5.7 million person rows changes no estimate. V4 proves it.
  indiv_cells <- individual[
    !is.na(edu_quintile) & !is.na(person_weight) & person_weight > 0,
    .(person_weight = sum(person_weight)), by = .(geo_id, edu_quintile)]
  list(
    dir_out = dir_out,
    reference_dir = reference_dir,
    city_id = city_id,
    analysis_year = analysis_year,
    buffer_km = buffer_km,
    n_groups = n_groups,
    base_group = base_group,
    n_boot = n_boot,
    exposure_manzana = exposure_manzana,
    individual = individual,
    census_collapsed = census_collapsed,
    crosswalk = crosswalk,
    manzana_pop = manzana_pop,
    indiv_cells = indiv_cells
  )
}

# ladder for the preserved resolution specification.
#' @param exposure_manzana Named input or setting from the preceding operation.
#' @param crosswalk Named input or setting from the preceding operation.
#' @param manzana_pop Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_ladder <- function(
  exposure_manzana,
  crosswalk,
  manzana_pop) {
  lv <- NULL
  w <- NULL

  # Prefix widths of the 22-character DANE code. Widths 6-14 are nominally nested but put 81% of
  # metro adults in a single unit (all of urban Bogota D.C. carries zeros there), so they are
  # excluded; see doc/ai/ and the plan's data audit for the counts.
  ladder <- data.table::data.table(
    level    = c("manzana", "seccion_urbana", "sector_urbano", "localidad", "municipio"),
    width    = c(22L, 20L, 18L, NA_integer_, 5L),
    n_expect = c(50287L, 3723L, 1146L, 40L, 21L))

  # One parent key per manzana per level. The localidad column comes from the spatial crosswalk;
  # every other level is a prefix, so nesting is exact and no unit can straddle a parent.
  keys <- unique(exposure_manzana[, .(geo_id)])
  keys <- merge(keys, crosswalk[, .(geo_id, loc_id)], by = "geo_id", all.x = TRUE)

  for (i in seq_len(nrow(ladder))) {
    lv <- ladder$level[i]
    w  <- ladder$width[i]
    keys[, (lv) := if (is.na(w)) loc_id else substr(geo_id, 1L, w)]
  }

  keys[, loc_id := NULL]

  # Every ladder key must be a well-formed 22-character code before it is truncated: an unpadded
  # rural id would become its own singleton parent at widths 18 and 20 and distort those rungs.
  # A handful survive reconcile_geo_ids() in the exposure file; they match no census row and so
  # carry no population, but they are removed here rather than left to truncate raggedly.
  ragged <- exposure_manzana[nchar(geo_id) != 22L, geo_id]

  if (length(ragged) > 0L) {
    message("Dropping ", length(ragged),
      " exposure unit(s) whose id is not 22 characters (",
            manzana_pop[geo_id %chin% ragged, sum(pop)], " adults).")
    exposure_manzana <- exposure_manzana[nchar(geo_id) == 22L]
    keys <- keys[nchar(geo_id) == 22L]
  }

  # A manzana whose representative point falls outside the 40-unit layer has no localidad. It is
  # dropped from every level, not just that one, so the estimation sample cannot change with the
  # geography being tested.
  no_parent <- keys[!complete.cases(keys), geo_id]

  if (length(no_parent) > 0L) {
    lost_pop <- manzana_pop[geo_id %chin% no_parent, sum(pop)]
    message("Dropping ", length(no_parent), " manzana(s) with no localidad parent (",
            lost_pop, " adults) from every level.")
    keys <- keys[!geo_id %chin% no_parent]
    exposure_manzana <- exposure_manzana[!geo_id %chin% no_parent]
  }
  list(
    ladder = ladder,
    keys = keys,
    lv = lv,
    w = w,
    exposure_manzana = exposure_manzana
  )
}

# classification for the preserved resolution specification.
#' @param n_groups Named input or setting from the preceding operation.
#' @param individual Named input or setting from the preceding operation.
#' @param census_collapsed Named input or setting from the preceding operation.
#' @param crosswalk Named input or setting from the preceding operation.
#' @param ladder Named input or setting from the preceding operation.
#' @param lv Named input or setting from the preceding operation.
#' @param w Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_classification <- function(
  n_groups,
  individual,
  census_collapsed,
  crosswalk,
  ladder,
  lv,
  w) {
  cells <- NULL

  # education_mean is a mean over the adults who reported education, so aggregating it weights by
  # pop_educ_known, not pop_total. See doc/audits/census_processing/education_mean_weight_shadowing.md
  census_keys <- copy(census_collapsed[, .(geo_id, pop_total, pop_educ_known,
    education_mean)])
  census_keys <- merge(census_keys, crosswalk[, .(geo_id, loc_id)], by = "geo_id",
    all.x = TRUE)

  design_c_cells <- vector("list", nrow(ladder))
  names(design_c_cells) <- ladder$level

  for (i in seq_len(nrow(ladder))) {
    lv <- ladder$level[i]
    w  <- ladder$width[i]

    census_keys[, .key := if (is.na(w)) loc_id else substr(geo_id, 1L, w)]

    # data.table's j is not sequential, so pop_educ_known here is the per-manzana vector, not
    # the group total that a dplyr::summarise() would have shadowed it with.
    lvl_census <- census_keys[!is.na(.key), .(
      pop_total      = sum(pop_total, na.rm = TRUE),
      pop_educ_known = sum(pop_educ_known, na.rm = TRUE),
      education_mean = stats::weighted.mean(education_mean, pop_educ_known,
        na.rm = TRUE)),
      by = .key]

    # assign_socio_group() breaks ties on a column named geo_id, so the parent key takes that
    # name here: at this level the parent *is* the geographic unit.
    data.table::setnames(lvl_census, ".key", "geo_id")

    # Same cut the geo-level path of run_idw_city() uses: equal-population bins of the unit mean.
    assign_socio_group(lvl_census, "education_mean", "pop_total", n_groups,
      "edu_quintile_geo")

    # Every adult inherits the quintile of the unit they live in.
    cells <- merge(individual[!is.na(person_weight) & person_weight > 0, .(geo_id,
      person_weight)],
                   census_keys[, .(geo_id, .key)], by = "geo_id")
    cells <- merge(cells, lvl_census[, .(.key = geo_id, edu_quintile_geo)], by = ".key")

    design_c_cells[[lv]] <- cells[
      !is.na(edu_quintile_geo),
      .(person_weight = sum(person_weight)),
      by = .(geo_id, edu_quintile = edu_quintile_geo)]
  }

  census_keys[, .key := NULL]

  # A manzana where no adult reported education has education_mean = NaN and so receives no
  # quintile; once it merges into a parent with a valid mean it acquires one. Left alone that
  # would let Design C's sample grow as the geography coarsens, which is the one thing the
  # ladder must not do. Keeping only manzanas classified at every level removes the drift.
  c_common <- Reduce(intersect, lapply(design_c_cells, function(x) unique(x$geo_id)))

  message("Design C: keeping ", length(c_common),
    " manzana(s) classified at every level (",
          data.table::uniqueN(unlist(lapply(design_c_cells, `[[`, "geo_id"))) -
            length(c_common), " dropped for level-dependent classification).")

  design_c_cells <- lapply(design_c_cells, function(x) x[geo_id %chin% c_common])
  list(
    design_c_cells = design_c_cells,
    lv = lv,
    w = w,
    cells = cells
  )
}

# estimates for the preserved resolution specification.
#' @param city_id Named input or setting from the preceding operation.
#' @param analysis_year Named input or setting from the preceding operation.
#' @param buffer_km Named input or setting from the preceding operation.
#' @param n_groups Named input or setting from the preceding operation.
#' @param base_group Named input or setting from the preceding operation.
#' @param exposure_manzana Named input or setting from the preceding operation.
#' @param manzana_pop Named input or setting from the preceding operation.
#' @param indiv_cells Named input or setting from the preceding operation.
#' @param ladder Named input or setting from the preceding operation.
#' @param keys Named input or setting from the preceding operation.
#' @param lv Named input or setting from the preceding operation.
#' @param design_c_cells Named input or setting from the preceding operation.
#' @param cells Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_estimates <- function(
  city_id,
  analysis_year,
  buffer_km,
  n_groups,
  base_group,
  exposure_manzana,
  manzana_pop,
  indiv_cells,
  ladder,
  keys,
  lv,
  design_c_cells,
  cells) {

  # The paper's regression outcomes: annual means plus IT1/IT2 exceedance hours.
  outcome_cols <- grep("^(avg_(pm10|pm25)|hrs_d_(pm10|pm25)_it[12])$",
                       names(exposure_manzana), value = TRUE)

  # One outcome at a time. Each outcome has its own covered manzanas (37,416 for PM10, 37,326 for
  # PM2.5), and fixing that sample per outcome is what keeps the ladder comparing like with like
  # while still reproducing the published per-outcome sample at the manzana rung.
  #
  # Re-keying geo_id to the parent is what makes the level real: the estimator then collapses to
  # parent-by-quintile cells and clusters on the parent, which is the unit exposure now varies at.

  ci_list <- list()

  for (design in c("A", "C")) {
    for (lv in ladder$level) {
      cells <- if (design == "A") indiv_cells else design_c_cells[[lv]]

      for (oc in outcome_cols) {
        ci_list[[length(ci_list) + 1L]] <- resolution_reference_run(
          oc, lv, cells, design, exposure_manzana, manzana_pop, keys,
          analysis_year, city_id, buffer_km, n_groups, base_group)
      }
    }
    cat("Design", design, "complete.\n")
  }

  ci_all <- data.table::rbindlist(ci_list, fill = TRUE)
  list(
    outcome_cols = outcome_cols,
    cells = cells,
    ci_all = ci_all
  )
}

# diagnostics for the preserved resolution specification.
#' @param exposure_manzana Named input or setting from the preceding operation.
#' @param manzana_pop Named input or setting from the preceding operation.
#' @param indiv_cells Named input or setting from the preceding operation.
#' @param ladder Named input or setting from the preceding operation.
#' @param keys Named input or setting from the preceding operation.
#' @param lv Named input or setting from the preceding operation.
#' @param w Named input or setting from the preceding operation.
#' @param cells Named input or setting from the preceding operation.
#' @param outcome_cols Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_diagnostics <- function(
  exposure_manzana,
  manzana_pop,
  indiv_cells,
  ladder,
  keys,
  lv,
  w,
  cells,
  outcome_cols) {

  # D1: the share of population-weighted exposure variance that survives each aggregation. This
  # is the mechanism variable and it is monotone by construction, unlike the gap itself.
  variance_retained <- data.table::rbindlist(lapply(outcome_cols, function(oc) {

    covered <- exposure_manzana[!is.na(get(oc)), .(geo_id, y = get(oc))]
    covered <- merge(covered, manzana_pop, by = "geo_id")
    covered <- merge(covered, keys, by = "geo_id")

    wvar <- function(x, w) {
      mu <- stats::weighted.mean(x, w)
      sum(w * (x - mu)^2) / sum(w)
    }

    base_var <- wvar(covered$y, covered$pop)

    data.table::rbindlist(lapply(ladder$level, function(lv) {
      agg <- covered[, .(y = stats::weighted.mean(y, pop), pop = sum(pop)),
                     by = c(lv)]

      data.table::data.table(
        outcome            = oc,
        resolution_level   = lv,
        n_resolution_units = nrow(agg),
        var_manzana        = base_var,
        var_level          = wvar(agg$y, agg$pop),
        share_retained     = wvar(agg$y, agg$pop) / base_var)
    }))
  }), fill = TRUE)

  # D5: the gap the aggregation moved is the differential within-parent sorting of each quintile.
  # Because the parent mean is population-weighted, the within-parent deviations average to zero
  # overall, so anything left is quintile-specific sorting.
  decomposition <- data.table::rbindlist(lapply(outcome_cols, function(oc) {

    covered <- exposure_manzana[!is.na(get(oc)), .(geo_id, y = get(oc))]
    covered <- merge(covered, manzana_pop, by = "geo_id")
    covered <- merge(covered, keys, by = "geo_id")

    cells <- merge(indiv_cells, covered, by = "geo_id")

    data.table::rbindlist(lapply(ladder$level, function(lv) {
      covered[, .key := get(lv)]
      covered[, y_level := stats::weighted.mean(y, pop), by = .key]

      d <- merge(cells[, .(geo_id, edu_quintile, person_weight)],
                 covered[, .(geo_id, y, y_level)], by = "geo_id")

      d[, .(outcome        = oc,
            resolution_level = lv,
            mean_within_dev = stats::weighted.mean(y - y_level, person_weight),
            mean_y          = stats::weighted.mean(y, person_weight),
            mean_y_level    = stats::weighted.mean(y_level, person_weight)),
        by = edu_quintile]
    }))
  }), fill = TRUE)
  list(
    variance_retained = variance_retained,
    decomposition = decomposition
  )
}

# bootstrap for the preserved resolution specification.
#' @param n_boot Named input or setting from the preceding operation.
#' @param exposure_manzana Named input or setting from the preceding operation.
#' @param manzana_pop Named input or setting from the preceding operation.
#' @param indiv_cells Named input or setting from the preceding operation.
#' @param ladder Named input or setting from the preceding operation.
#' @param keys Named input or setting from the preceding operation.
#' @param cells Named input or setting from the preceding operation.
#' @param outcome_cols Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_bootstrap <- function(
  n_boot,
  exposure_manzana,
  manzana_pop,
  indiv_cells,
  ladder,
  keys,
  cells,
  outcome_cols) {

  # Per-level clustered intervals answer "is this level's gap non-zero". They cannot answer
  # "does the gap move with resolution", because every level is computed from the same manzanas.
  # Resampling manzanas and recomputing the whole ladder inside each replicate does.
  #
  # For a saturated group model with within-group normalized weights the fitted group mean is the
  # population-weighted mean of the outcome, so the gap is a ratio of weighted means. V-boot below
  # checks that shortcut against compute_exposure_regressions() before it is trusted.
  #' manz  one row per drawn manzana: .draw, y, pop, and one column per ladder level.
  #' cells one row per drawn manzana and quintile: .draw, edu_quintile, person_weight.
  #' The two tables join on .draw rather than geo_id, so a manzana drawn twice contributes
  #' exactly twice on both sides instead of four times through a cartesian join.

  boot_list <- vector("list", length(outcome_cols))
  names(boot_list) <- outcome_cols

  for (oc in outcome_cols) {

    covered <- exposure_manzana[!is.na(get(oc)), .(geo_id, y = get(oc))]
    covered <- merge(covered, manzana_pop, by = "geo_id")
    covered <- merge(covered, keys, by = "geo_id")

    # The observed sample is the draw that takes every manzana exactly once.
    covered[, .draw := .I]
    base_cells <- merge(covered[, .(geo_id, .draw)], indiv_cells, by = "geo_id",
                        allow.cartesian = TRUE)[, .(.draw, edu_quintile, person_weight)]

    point <- resolution_reference_gaps(covered, base_cells, ladder)
    data.table::setnames(point, "gap", "gap_point")

    reps <- data.table::rbindlist(lapply(seq_len(n_boot), function(b) {

      # Resample manzanas, not cells or people: the manzana is the unit exposure is measured on.
      # Parent keys are left untouched so the nesting survives the draw.
      idx  <- sample(nrow(covered), nrow(covered), replace = TRUE)
      manz <- covered[idx]
      manz[, .draw := .I]

      cells <- merge(manz[, .(geo_id, .draw)], indiv_cells, by = "geo_id",
                     allow.cartesian = TRUE)[, .(.draw, edu_quintile, person_weight)]

      out <- resolution_reference_gaps(manz, cells, ladder)
      out[, .boot := b]
      out
    }), fill = TRUE)

    # Split the column name the way .exposure_parse_outcome() does, so these rows join the
    # regression table on (outcome, pollutant) instead of on a label only this loop understands.
    poll <- if (grepl("pm25", oc, fixed = TRUE)) "pm25" else "pm10"

    boot_list[[oc]] <- merge(reps, point, by = c("resolution_level", "group"))[
      , `:=`(outcome = sub(paste0("_", poll), "", oc), pollutant = poll)]
  }

  boot_reps <- data.table::rbindlist(boot_list, fill = TRUE)

  # The quantity of interest is how far each level's gap sits from the manzana gap, computed
  # within a replicate so the two share the draw and their dependence is preserved.
  anchor <- boot_reps[resolution_level == "manzana",
                      .(outcome, pollutant, group, .boot, gap_anchor = gap,
                        gap_anchor_point = gap_point)]

  boot_reps <- merge(boot_reps, anchor, by = c("outcome", "pollutant", "group", ".boot"))
  boot_reps[, delta := gap - gap_anchor]

  boot_ci <- boot_reps[, .(
    gap_point       = data.table::first(gap_point),
    delta_point     = data.table::first(gap_point) - data.table::first(gap_anchor_point),
    gap_boot_low    = stats::quantile(gap, 0.025, na.rm = TRUE),
    gap_boot_high   = stats::quantile(gap, 0.975, na.rm = TRUE),
    delta_boot_low  = stats::quantile(delta, 0.025, na.rm = TRUE),
    delta_boot_high = stats::quantile(delta, 0.975, na.rm = TRUE),
    n_boot          = .N),
    by = .(outcome, pollutant, resolution_level, group)]

  cat("Bootstrap complete.\n")
  list(
    boot_ci = boot_ci
  )
}

# verification for the preserved resolution specification.
#' @param city_id Named input or setting from the preceding operation.
#' @param analysis_year Named input or setting from the preceding operation.
#' @param buffer_km Named input or setting from the preceding operation.
#' @param census_collapsed Named input or setting from the preceding operation.
#' @param crosswalk Named input or setting from the preceding operation.
#' @param ladder Named input or setting from the preceding operation.
#' @param lv Named input or setting from the preceding operation.
#' @param w Named input or setting from the preceding operation.
#' @param ci_all Named input or setting from the preceding operation.
#' @param boot_ci Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_verification <- function(
  city_id,
  analysis_year,
  buffer_km,
  census_collapsed,
  crosswalk,
  ladder,
  lv,
  w,
  ci_all,
  boot_ci) {

  # V-boot: the weighted-mean shortcut the bootstrap uses must reproduce the estimator's own
  # point estimates. If it does not, every interval above is describing a different statistic.
  check_boot <- merge(
    boot_ci[, .(outcome, pollutant, resolution_level, group, gap_point)],
    ci_all[design == "A", .(outcome, pollutant, resolution_level, group, estimate)],
    by = c("outcome", "pollutant", "resolution_level", "group"))

  cat("\nV-boot: ", nrow(check_boot),
    " coefficients compared, max |shortcut - estimator| = ",
      if (nrow(check_boot) > 0L) {
        format(max(abs(check_boot$gap_point - check_boot$estimate)))
      }
      else "NOT CHECKED - labels did not align", "\n", sep = "")

  stopifnot(nrow(check_boot) == 150L,
            max(abs(check_boot$gap_point - check_boot$estimate)) < 1e-8)

  # V2/V3/V5: the ladder must conserve population and units, and hold the sample fixed.
  # n_units_metro counts the whole metro area and must reproduce the data audit's ladder;
  # n_units_covered counts only the units a station reaches inside the buffer. `keys` is built
  # from the exposure file, so the metro count comes from the census instead.
  census_ladder <- merge(census_collapsed[, .(geo_id, pop_total)],
                         crosswalk[, .(geo_id, loc_id)], by = "geo_id", all.x = TRUE)

  for (i in seq_len(nrow(ladder))) {
    w <- ladder$width[i]
    census_ladder[, (ladder$level[i]) := if (is.na(w)) loc_id else substr(geo_id, 1L, w)]
  }

  ladder_check <- data.table::rbindlist(lapply(ladder$level, function(lv) {
    data.table::data.table(
      resolution_level = lv,
      n_units_metro    = data.table::uniqueN(census_ladder[[lv]], na.rm = TRUE),
      n_units_covered  = ci_all[design == "A" & resolution_level == lv,
                                max(n_resolution_units)],
      pop_metro        = census_ladder[, sum(pop_total, na.rm = TRUE)],
      pop_estimation   = ci_all[design == "A" & resolution_level == lv,
                                max(pop_estimation)])
  }), fill = TRUE)

  ladder_check <- merge(ladder_check, ladder[, .(resolution_level = level, n_expect)],
                        by = "resolution_level", sort = FALSE)

  cat("\nV3 ladder (unit counts and estimation population by level):\n")
  print(ladder_check)

  # V5 must hold within every design and outcome separately: each has its own covered manzanas,
  # but within one the same people must appear at all five levels.
  v5 <- ci_all[, .(n_distinct_pop = data.table::uniqueN(round(pop_estimation, 6)),
                   pop = min(pop_estimation)),
               by = .(design, outcome, pollutant)]

  cat("\nV5 sample invariance across levels, by design and outcome:\n")
  print(v5[order(design, pollutant, outcome)])

  if (any(v5$n_distinct_pop != 1L)) {
    stop("V5 failed: the estimation sample changes with the geography being tested for ",
         v5[n_distinct_pop != 1L, paste(design, outcome, pollutant, collapse = "; ")])
  }

  # Design A and Design C do not share a sample, and should not: A classifies only adults who
  # reported education, C classifies everyone living in a unit with a defined area mean. Report
  # the gap so it is a stated fact rather than a silent difference between the two panels.
  cat("\nDesign A vs C estimation population (they differ by construction):\n")
  print(dcast(v5, outcome + pollutant ~ design, value.var = "pop"))

  cat("\nV7 clusters supporting each fit:\n")
  print(ci_all[design == "A" & group == 1L,
               .(resolution_level, outcome, pollutant, n_clusters, n_coef)])

  # V4 anchor: the manzana rung must reproduce the published Bogota 3 km estimates.
  published_path <- here::here("data", "processed", "idw_regressions",
                               sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
                                       buffer_km, analysis_year))

  if (file.exists(published_path)) {
    published <- data.table::as.data.table(arrow::read_parquet(published_path))
    published <- published[city_id == "bogota_2018", .(outcome, pollutant, group,
                                                       published = estimate)]

    anchor_cmp <- merge(ci_all[design == "A" & resolution_level == "manzana",
                               .(outcome, pollutant, group, estimate)],
                        published, by = c("outcome", "pollutant", "group"))

    anchor_cmp[, abs_diff := abs(estimate - published)]

    cat("\nV4 anchor vs published (", nrow(anchor_cmp), " coefficients): max |diff| = ",
        format(max(anchor_cmp$abs_diff)), "\n", sep = "")
    print(anchor_cmp[order(-abs_diff)][1:5])
    stopifnot(nrow(anchor_cmp) == 30L, max(anchor_cmp$abs_diff) < 1e-8)
  } else {
    cat("\nV4 anchor NOT RUN: published table absent at", published_path, "\n")
  }
  list(
    ladder_check = ladder_check
  )
}

# save for the preserved resolution specification.
#' @param dir_out Named input or setting from the preceding operation.
#' @param reference_dir Named input or setting from the preceding operation.
#' @param analysis_year Named input or setting from the preceding operation.
#' @param buffer_km Named input or setting from the preceding operation.
#' @param ci_all Named input or setting from the preceding operation.
#' @param variance_retained Named input or setting from the preceding operation.
#' @param decomposition Named input or setting from the preceding operation.
#' @param boot_ci Named input or setting from the preceding operation.
#' @param ladder_check Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_reference_save <- function(
  dir_out,
  reference_dir,
  analysis_year,
  buffer_km,
  ci_all,
  variance_retained,
  decomposition,
  boot_ci,
  ladder_check) {

  meta_cols <- c("city_id", "year", "buffer_km", "design", "resolution_level")
  set_meta_cols_first(ci_all, meta_cols)

  save_table_parquet_csv(ci_all, dir_out,
                         sprintf("resolution_ci_bogota_%dkm_%d", buffer_km,
                           analysis_year))
  save_table_parquet_csv(boot_ci, dir_out,
                         sprintf("resolution_bootstrap_bogota_%dkm_%d", buffer_km,
                           analysis_year))
  save_table_parquet_csv(variance_retained, dir_out,
                         sprintf("resolution_variance_bogota_%dkm_%d", buffer_km,
                           analysis_year))
  save_table_parquet_csv(decomposition, dir_out,
                         sprintf("resolution_decomposition_bogota_%dkm_%d", buffer_km,
                                 analysis_year))
  save_table_parquet_csv(ladder_check, dir_out,
                         sprintf("resolution_coverage_bogota_%dkm_%d", buffer_km,
                           analysis_year))

  if (length(reference_dir)) {
  # Compare all historical numerical products, including all 500-draw interval summaries.
  reference_comparison <- rbindlist(lapply(c("ci", "bootstrap", "variance",
                                            "decomposition", "coverage"), function(kind) {
    name <- sprintf("resolution_%s_bogota_%dkm_%d.parquet", kind, buffer_km,
      analysis_year)
    old_path <- here::here("data", "processed", "resolution_sensitivity", name)
    new_path <- file.path(dir_out, name)
    if (!file.exists(old_path)) return(data.table(artifact = kind,
      status = "saved_reference_unavailable", max_numeric_error = NA_real_))
    old <- as.data.table(arrow::read_parquet(old_path))
    new <- as.data.table(arrow::read_parquet(new_path))
    same <- isTRUE(all.equal(old, new, tolerance = 1e-9, check.attributes = FALSE))
    numeric_cols <- names(old)[vapply(old, is.numeric, logical(1))]
    errors <- vapply(numeric_cols, function(col) {
      values <- abs(old[[col]] - new[[col]])
      if (all(is.na(values))) 0 else max(values, na.rm = TRUE)
    }, numeric(1))
    stopifnot(same)
    data.table(artifact = kind, status = "passed", rows = nrow(new),
      max_numeric_error = max(errors), saved_md5 = unname(tools::md5sum(old_path)))
  }))
  resolution_write(reference_comparison, file.path(dir_out, "reference_comparison"))
  }
  cat("Bogota reference sensitivity completed.\n")
  invisible(NULL)
}

#' @param buffer_km IDW eligibility distance of the multicity run.
#' @param grouping Individual education grouping: "edu_quintile" (frozen quintiles),
#   "edu_quintile_split", "edu_level" (six bands) or "edu_group3" (three groups).
#' @return Processed root: the 3 km root, or a separate buffer_<b>km folder beneath it.
#   Every other grouping uses its own folder with the same layout.
#' @details Other buffers and groupings never overwrite the 3 km quintile products or
#   its frozen B matrices.
resolution_buffer_root <- function(buffer_km, grouping = "edu_quintile") {
  folders <- c(edu_quintile = "", edu_quintile_split = "quintile_split",
               edu_level = "education_level", edu_group3 = "education_group3")
  grouping <- match.arg(grouping, names(folders))
  root <- here::here("data", "processed", "resolution_sensitivity")
  if (nzchar(folders[[grouping]])) root <- file.path(root, folders[[grouping]])
  if (buffer_km == 3) root else file.path(root, paste0("buffer_", buffer_km, "km"))
}

#' @param groups_path The frozen individual groups file (input manifest "groups").
#' @param quintile_cells Frozen fine-unit quintile cells of the same adults.
#' @param grouping "edu_quintile_split", "edu_level" or "edu_group3".
#' @param bands Six education-band columns, lowest level first.
#' @param group_of_level Three-group number of each band (edu_group3 only).
#' @return List: cells (fine geo_id x group person weights of reporting adults) and
#   max_error, the largest relative difference from the quintile unit totals.
#' @details Levels come from assign_education_level() and three groups merge them;
#   split quintiles come from split_socio_group_cells(). Each unit's
#   education-reporting weight must match its frozen quintile total: same people and
#   weights, only the group labels change. The tolerance absorbs summation order.
resolution_group_cells <- function(groups_path, quintile_cells, grouping, bands,
                                   group_of_level = NULL) {
  individual <- as.data.table(arrow::read_parquet(groups_path, col_select =
    tidyselect::all_of(c("geo_id", "person_weight", "adult", "educ_years", bands))))
  stopifnot(all(individual$adult == 1))
  if (grouping == "edu_quintile_split") {
    cells <- split_socio_group_cells(individual, "educ_years", "person_weight", 5L,
                                     grouping)[person_weight > 0]
  } else {
    assign_education_level(individual, bands)
    if (grouping == "edu_group3") individual[, edu_group3 := group_of_level[edu_level]]
    cells <- individual[!is.na(get(grouping)) & is.finite(person_weight) &
      person_weight > 0, .(person_weight = sum(person_weight)), by = c("geo_id", grouping)]
  }
  totals <- merge(cells[, .(level = sum(person_weight)), by = geo_id],
                  quintile_cells[, .(quintile = sum(person_weight)), by = geo_id],
                  by = "geo_id", all = TRUE)
  max_error <- max(abs(totals$level - totals$quintile) / totals$quintile)
  stopifnot(!anyNA(totals), max_error < 1e-12)
  list(cells = cells, max_error = max_error)
}

# inputs for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param buffer_km IDW eligibility distance; 3 km uses the frozen manifest exposure.
#' @param grouping Individual education grouping; see resolution_buffer_root().
#' @param education_bands Band columns of the attainment groupings (education_level_bands).
#' @param education_group_map Band-to-group map of edu_group3 (education_group3_of_level).
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
#   Every buffer reuses the frozen 3 km crosswalks, cells and full B distance matrices
#   (matrix_root); another buffer reads the saved fine exposure of that buffer and
#   records its hash in exposure_manifest. Other groupings write their own cells; B
#   exposures come from the quintile run of the same buffer (exposure_root). C uses
#   area quintiles of mean schooling for both quintile groupings and the area median
#   for attainment groups. groups are those populated in the city's cells.
resolution_multicity_inputs <- function(city, buffer_km = 3, grouping = "edu_quintile",
                                        education_bands = NULL,
                                        education_group_map = NULL) {
  message("Estimating resolution sensitivity: ", city, ", ", buffer_km, " km, ",
          grouping)
  city_name <- sub("_[0-9]+$", "", city)
  inputs <- here::here("data", "interim", "resolution_sensitivity", city)
  out <- file.path(resolution_buffer_root(buffer_km, grouping), city)
  matrix_root <- file.path(resolution_buffer_root(3), city, "B")
  exposure_root <- file.path(resolution_buffer_root(buffer_km), city, "B")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  manifest <- fread(file.path(inputs, "input_manifest.csv"))
  stopifnot(identical(unname(tools::md5sum(here::here(manifest$path))), manifest$md5))
  paths <- setNames(here::here(manifest$path), manifest$input)
  if (buffer_km != 3) {
    paths["exposure"] <- here::here("data", "processed", "idw_estimates", city,
      paste0(city, "_", buffer_km, "km_idw_exposure.parquet"))
    resolution_write(data.table(input = "exposure", buffer_km = buffer_km,
      path = sub(paste0(here::here(), "/"), "", paths["exposure"], fixed = TRUE),
      md5 = unname(tools::md5sum(paths["exposure"]))),
      file.path(out, "exposure_manifest"))
  }
  keys <- as.data.table(arrow::read_parquet(file.path(inputs, "crosswalk.parquet")))
  population <- as.data.table(arrow::read_parquet(file.path(inputs,
    "adult_population.parquet")))
  cells <- as.data.table(arrow::read_parquet(file.path(inputs,
    "individual_quintile_cells.parquet")))
  checks <- list()
  if (grouping != "edu_quintile") {
    group_cells <- resolution_group_cells(paths["groups"], cells, grouping,
      education_bands, education_group_map)
    cells <- group_cells$cells
    resolution_write(cells, file.path(out, "individual_group_cells"))
    checks[[1L]] <- data.table(check = "group_cells_match_quintile_totals",
      level = "fine", outcome = NA_character_, max_error = group_cells$max_error,
      passed = TRUE)
  }
  group_col <- grouping
  groups <- sort(unique(cells[[group_col]]))
  census <- as.data.table(arrow::read_parquet(file.path(inputs,
    "census_education.parquet")))
  ladder <- fread(file.path(inputs, "levels.csv"))
  exposure <- as.data.table(arrow::read_parquet(paths["exposure"]))[year == 2023]
  if (city == "bogota_2018") {
    exposure[, geo_id := stringi::stri_pad_right(geo_id, 22L, pad = "0")]
  }
  outcomes <- grep("^(avg_(pm10|pm25)|hrs_d_(pm10|pm25)_it[12])$",
                    names(exposure), value = TRUE)
  for (oc in outcomes) set(exposure, j = oc, value = as.numeric(exposure[[oc]]))
  panel <- as.data.table(arrow::read_parquet(paths["pollution"]))
  panel[, station := normalize_station(station)]
  source_dir <- dirname(dirname(paths["pollution"]))
  writeLines("incomplete", file.path(out, "STATUS.txt"))
  profiles <- list(); contrasts <- list(); variances <- list()
  composition <- list(); differences <- list()
  bootstraps <- list(); transitions <- list()
  matrices <- list(); reconciliations <- list(); coverage <- list(); movements <- list()
  exposure_changes <- list()
  good_keys <- keys[, .(complete = all(!is.na(parent_id))),
    by = geo_id][complete == TRUE, geo_id]
  # Quintiles cut area mean schooling; attainment groups take each area's median.
  area_col <- sub("^edu_", "area_", group_col)
  if (grouping %chin% c("edu_quintile", "edu_quintile_split")) {
    c_map <- resolution_classify(census, keys)
    setnames(c_map, "area_quintile", area_col)
  } else c_map <- resolution_classify_median(cells, keys, group_col)
  c_common <- c_map[, .(complete = all(!is.na(get(area_col)))),
    by = geo_id][complete == TRUE, geo_id]
  resolution_write(c_map, file.path(out, "C_classification"))
  list(
    inputs = inputs,
    out = out,
    paths = paths,
    keys = keys,
    population = population,
    cells = cells,
    group_col = group_col,
    groups = groups,
    ladder = ladder,
    exposure = exposure,
    outcomes = outcomes,
    panel = panel,
    source_dir = source_dir,
    checks = checks,
    profiles = profiles,
    contrasts = contrasts,
    variances = variances,
    composition = composition,
    differences = differences,
    bootstraps = bootstraps,
    transitions = transitions,
    matrices = matrices,
    reconciliations = reconciliations,
    movements = movements,
    exposure_changes = exposure_changes,
    good_keys = good_keys,
    c_map = c_map,
    c_common = c_common,
    matrix_root = matrix_root,
    exposure_root = exposure_root,
    buffer_km = buffer_km
  )
}

# rebuild b for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param lv Named input or setting from the preceding operation.
#' @param reuse_b Named input or setting from the preceding operation.
#' @param inputs Named input or setting from the preceding operation.
#' @param out Named input or setting from the preceding operation.
#' @param paths Named input or setting from the preceding operation.
#' @param keys Named input or setting from the preceding operation.
#' @param cells Named input or setting from the preceding operation.
#' @param exposure Named input or setting from the preceding operation.
#' @param outcomes Named input or setting from the preceding operation.
#' @param source_dir Named input or setting from the preceding operation.
#' @param checks Named input or setting from the preceding operation.
#' @param matrix_root Frozen 3 km B folder holding each level's full distance matrix.
#' @param buffer_km IDW eligibility distance passed to aggregate_idw_exposure().
#' @param mem_gb DuckDB memory limit; a computing resource that never changes estimates.
#' @param grouping "edu_quintile" rebuilds B by IDW; other groupings reuse it.
#' @param exposure_root Quintile B folder of the same buffer, read by the level run.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
#   Interpolation never sees groups (run_idw_city() @details), so other groupings copy
#   the quintile run's B exposure after checking that its pollution, matrix and IDW
#   code hashes are current, and records the source in exposure_source.csv.
resolution_multicity_rebuild_b <- function(
  city,
  lv,
  reuse_b,
  inputs,
  out,
  paths,
  keys,
  cells,
  exposure,
  outcomes,
  source_dir,
  checks,
  matrix_root,
  buffer_km = 3,
  mem_gb = 4,
  grouping = "edu_quintile",
  exposure_root = NULL) {
  a <- NULL
  z <- NULL
  error <- NULL
  message(city, ": estimating ", lv)
  k <- keys[level == lv, .(geo_id, parent_id)]
  bdir <- file.path(out, "B", lv)
  bfile <- file.path(bdir, "resolution_idw_exposure.parquet")
  b_signature <- file.path(bdir, "exposure_inputs.csv")
  matrix_file <- file.path(matrix_root, lv, "matrix_geo_station_distances.parquet")
  signature_paths <- c(paths["pollution"], matrix_file,
    here::here("src", "general_utilities", "process", "idw_exposure.R"),
    file.path(inputs, "individual_quintile_cells.parquet"))
  signature <- data.table(path = signature_paths,
                          md5 = unname(tools::md5sum(signature_paths)))
  if (grouping != "edu_quintile") {
    # The first three signature rows are the interpolation inputs; cells are not.
    source_file <- file.path(exposure_root, lv, "resolution_idw_exposure.parquet")
    source_signature <- fread(file.path(exposure_root, lv, "exposure_inputs.csv"))
    if (!isTRUE(all.equal(source_signature[1:3], signature[1:3],
                          check.attributes = FALSE))) {
      stop("B exposure inputs changed for ", city, " ", lv,
           ": rerun the quintile B at this buffer first.")
    }
    dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(source_file, bfile, overwrite = TRUE))
    fwrite(data.table(source = source_file, md5 = unname(tools::md5sum(source_file))),
           file.path(bdir, "exposure_source.csv"))
    checks[[length(checks) + 1L]] <- data.table(check = "B_exposure_reused_current_inputs",
      level = lv, outcome = NA_character_, max_error = 0, passed = TRUE)
  } else {
    groups <- merge(cells, k[!is.na(parent_id)], by = "geo_id")
    groups <- groups[, .(person_weight = sum(person_weight)),
                       by = .(geo_id = parent_id, edu_quintile)]
    groups[, adult := 1L]
    can_reuse <- reuse_b && file.exists(bfile) && file.exists(b_signature) &&
      isTRUE(all.equal(fread(b_signature), signature, check.attributes = FALSE))
    if (!can_reuse) {
      aggregate_idw_exposure(arrow_dir = source_dir,
        geo_sta_pq = matrix_file, buffer_km = buffer_km,
        census_col = groups, group_var = "edu_quintile", quintile_level = "individual",
        precomputed_group_col = "edu_quintile", target_years = 2023L,
        mem_gb = mem_gb, n_threads = 2L, out_dir = bdir, out_name = "resolution",
        overwrite = TRUE, quiet = FALSE)
      fwrite(signature, b_signature)
    }
    saved_groups <- as.data.table(arrow::read_parquet(file.path(bdir,
      "resolution_indiv_groups.parquet")))
    saved_groups <- saved_groups[, .(geo_id, edu_quintile, person_weight)]
    expected_groups <- groups[, .(geo_id, edu_quintile, person_weight)]
    setorder(saved_groups, geo_id, edu_quintile)
    setorder(expected_groups, geo_id, edu_quintile)
    stopifnot(identical(saved_groups, expected_groups))
    checks[[length(checks) + 1L]] <- data.table(check = "B_frozen_groups_and_weights",
      level = lv, outcome = NA_character_, max_error = 0, passed = TRUE)
  }
  b <- as.data.table(arrow::read_parquet(bfile))[year == 2023]
  for (oc in outcomes) set(b, j = oc, value = as.numeric(b[[oc]]))
  if (lv == "fine") {
    cmp <- merge(exposure[, c("geo_id", outcomes), with = FALSE],
                 b[, c("geo_id", outcomes), with = FALSE], by = "geo_id", all = TRUE)
    for (oc in outcomes) {
      a <- cmp[[paste0(oc, ".x")]]; z <- cmp[[paste0(oc, ".y")]]
      error <- max(abs(a - z), na.rm = TRUE)
      same_na <- identical(is.na(a), is.na(z))
      checks[[length(checks) + 1L]] <- data.table(check = "fresh_IDW_anchor",
        level = lv,
        outcome = oc, max_error = error, passed = error < 1e-8 && same_na)
      stopifnot(error < 1e-8, same_na)
    }
  }
  list(
    k = k,
    bdir = bdir,
    checks = checks,
    b = b,
    a = a,
    z = z,
    error = error
  )
}

# contrasts for the preserved resolution specification.
#' @param lv Named input or setting from the preceding operation.
#' @param out Named input or setting from the preceding operation.
#' @param population Named input or setting from the preceding operation.
#' @param cells Named input or setting from the preceding operation.
#' @param ladder Named input or setting from the preceding operation.
#' @param exposure Named input or setting from the preceding operation.
#' @param outcomes Named input or setting from the preceding operation.
#' @param checks Named input or setting from the preceding operation.
#' @param profiles Named input or setting from the preceding operation.
#' @param contrasts Named input or setting from the preceding operation.
#' @param variances Named input or setting from the preceding operation.
#' @param composition Named input or setting from the preceding operation.
#' @param differences Named input or setting from the preceding operation.
#' @param bootstraps Named input or setting from the preceding operation.
#' @param transitions Named input or setting from the preceding operation.
#' @param exposure_changes Named input or setting from the preceding operation.
#' @param good_keys Named input or setting from the preceding operation.
#' @param c_map Named input or setting from the preceding operation.
#' @param c_common Named input or setting from the preceding operation.
#' @param k Named input or setting from the preceding operation.
#' @param b Named input or setting from the preceding operation.
#' @param a Named input or setting from the preceding operation.
#' @param error Named input or setting from the preceding operation.
#' @param group_col Individual group column: "edu_quintile" or "edu_level".
#' @param groups The city's groups, lowest first; gaps are first minus last.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_multicity_contrasts <- function(
  lv,
  out,
  population,
  cells,
  ladder,
  exposure,
  outcomes,
  checks,
  profiles,
  contrasts,
  variances,
  composition,
  differences,
  bootstraps,
  transitions,
  exposure_changes,
  good_keys,
  c_map,
  c_common,
  k,
  b,
  a,
  error,
  group_col = "edu_quintile",
  groups = 1:5) {
  boot <- NULL
  # Every estimate in this run uses its own grouping.
  estimate <- function(x, assignment, inference = FALSE) {
    resolution_estimate(x, assignment, inference, group_col = group_col,
                        groups = groups)
  }
  for (oc in outcomes) {
    fine <- merge(exposure[is.finite(get(oc)), .(geo_id, y = get(oc))],
                  population, by = "geo_id")
    fine <- fine[geo_id %chin% good_keys]
    a <- resolution_aggregate(fine, k)
    native <- merge(k[!is.na(parent_id)], b[is.finite(get(oc)),
      .(parent_id = geo_id, y = get(oc))], by = "parent_id")
    common <- intersect(native$geo_id, fine$geo_id)
    base <- fine[, .(geo_id, parent_id = geo_id, y)]
    common_b <- native[geo_id %chin% common]
    common_a <- a[geo_id %chin% common]
    common_0 <- base[geo_id %chin% common]
    # Baseline is unrestricted by the comparison level's B coverage.
    estimates <- list(A = estimate(cells, a,
      ladder[level == lv, secondary_inference]),
      B_native = estimate(cells, native,
        ladder[level == lv, secondary_inference]),
      B_common = estimate(cells, common_b),
      baseline_common = estimate(cells, common_0),
      A_common = estimate(cells, common_a))
    original <- estimate(cells, base)
    area_col <- sub("^edu_", "area_", group_col)
    class <- c_map[level == lv & geo_id %chin% c_common,
                   c("geo_id", area_col, "parent_id"), with = FALSE]
    cc <- merge(cells, class, by = "geo_id")
    cc <- merge(cc, base[, .(geo_id, y)], by = "geo_id")
    transition <- cc[, .(population = sum(person_weight)), by = c(group_col, area_col)]
    setnames(transition, group_col, sub("^edu_", "individual_", group_col))
    transition[, `:=`(level = lv, outcome = oc)]
    transitions[[length(transitions) + 1L]] <- transition
    cc[, (group_col) := get(area_col)]
    c_assignment <- unique(cc[, .(geo_id, parent_id, y)])
    estimates$C <- estimate(cc[, c("geo_id", group_col, "person_weight"), with = FALSE],
                           c_assignment)
    for (design in names(estimates)) {
      e <- estimates[[design]]
      e$profile[, `:=`(design = design, level = lv, outcome = oc)]
      e$contrast[, `:=`(design = design, level = lv, outcome = oc)]
      sd0 <- sqrt(resolution_variance(fine$y, fine$pop))
      e$contrast[, standardized_gap := if (is.finite(sd0) && sd0 > 0) {
        gap / sd0
      } else NA_real_]
      profiles[[length(profiles) + 1L]] <- e$profile
      contrasts[[length(contrasts) + 1L]] <- e$contrast
    }
    common_values <- merge(common_a[, .(geo_id, original = y0, aggregated = y)],
      common_b[, .(geo_id, rebuilt = y)], by = "geo_id")
    common_weights <- cells[geo_id %chin% common,
      .(weight = sum(person_weight)), by = geo_id]
    common_values <- merge(common_values, common_weights, by = "geo_id")
    exposure_changes[[length(exposure_changes) + 1L]] <- common_values[, .(
      level = lv, outcome = oc, population = sum(weight),
      mean_B_minus_fine = weighted.mean(rebuilt - original, weight),
      mean_absolute_B_minus_fine = weighted.mean(abs(rebuilt - original), weight),
      rmse_B_minus_fine = sqrt(weighted.mean((rebuilt - original)^2, weight)),
      mean_B_minus_A = weighted.mean(rebuilt - aggregated, weight),
      mean_absolute_B_minus_A = weighted.mean(abs(rebuilt - aggregated), weight),
      rmse_B_minus_A = sqrt(weighted.mean((rebuilt - aggregated)^2, weight)))]
    # Exact cell membership and weights, not merely population totals, are invariant.
    baseline_cells <- cells[geo_id %chin% base$geo_id]
    a_cells <- cells[geo_id %chin% a$geo_id]
    invariant <- identical(baseline_cells, a_cells)
    stopifnot(invariant)
    checks[[length(checks) + 1L]] <- data.table(check = "A_cell_weight_invariance",
      level = lv, outcome = oc, max_error = 0, passed = invariant)
    v0 <- resolution_variance(a$y0, a$pop)
    v1 <- resolution_variance(a$y, a$pop)
    within <- sum(a$pop * (a$y0 - a$y)^2) / sum(a$pop)
    stopifnot(abs(v0 - v1 - within) < 1e-7 * max(1, v0))
    variances[[length(variances) + 1L]] <- data.table(level = lv, outcome = oc,
      baseline_variance = v0, retained_variance = v1, within_variance = within,
      share_retained = if (v0 > 0) v1 / v0 else NA_real_,
      aggregation_population = sum(a$pop))
    gaps <- vapply(estimates, function(e) e$contrast$gap, numeric(1))
    g0 <- original$contrast$gap
    parts <- c(native_selection = gaps["B_native"] - gaps["B_common"],
      procedure_common = gaps["B_common"] - gaps["baseline_common"],
      baseline_selection = gaps["baseline_common"] - g0)
    total <- gaps["B_native"] - g0
    error <- total - sum(parts)
    if (is.finite(error)) stopifnot(abs(error) < 1e-8)
    composition[[length(composition) + 1L]] <- data.table(level = lv, outcome = oc,
      native_change = total, native_selection = unname(parts[1]),
      procedure_common = unname(parts[2]), baseline_selection = unname(parts[3]),
      aggregation_common = gaps["A_common"] - gaps["baseline_common"],
      additional_B_common = gaps["B_common"] - gaps["A_common"],
      reconciliation_error = error, population_fine = original$contrast$population,
      population_native = estimates$B_native$contrast$population,
      population_common = estimates$B_common$contrast$population)
    for (design in c("A", "B_common")) {
      pair <- if (design == "A") a[, .(geo_id, parent_id, y0, y1 = y)] else {
        merge(common_b[, .(geo_id, parent_id, y1 = y)],
              common_0[, .(geo_id, y0 = y)], by = "geo_id")
      }
      point <- if (design == "A") gaps["A"] - g0 else
        gaps["B_common"] - gaps["baseline_common"]
      delta <- data.table(design = design, level = lv, outcome = oc, delta = point,
        lower = NA_real_, upper = NA_real_, n_boot = 0L, valid_replicates = 0L,
        n_clusters = uniqueN(pair$parent_id), seed = 20260910L,
        interval_type = "descriptive")
      feasibility <- estimate(cells, pair[, .(geo_id, parent_id, y = y1)])
      supported <- feasibility$contrast$effective_clusters >= 30 &&
        feasibility$contrast$max_cluster_share < .2 && delta$n_clusters >= 50
      if (ladder[level == lv, bootstrap] && isTRUE(supported)) {
        boot <- resolution_bootstrap(cells, pair, group_col = group_col, groups = groups)
        boot$replicates[, `:=`(design = design, level = lv, outcome = oc)]
        bootstraps[[length(bootstraps) + 1L]] <- boot$replicates
        delta[, (names(boot$interval)) := boot$interval]
      } else if (ladder[level == lv, bootstrap]) {
        delta[, interval_type := "insufficient_cluster_support"]
      }
      differences[[length(differences) + 1L]] <- delta
    }
    # Assignments suffice to reconstruct estimates without duplicating individual rows.
    assignment <- merge(a[, .(geo_id, parent_id, A = y, baseline = y0)],
                         native[, .(geo_id, parent_B = parent_id, B = y)],
                         by = "geo_id", all = TRUE)
    dir.create(file.path(out, "assignments"), showWarnings = FALSE)
    arrow::write_parquet(assignment, file.path(out, "assignments", paste0(lv, "_", oc,
                                                                         ".parquet")))
  }
  list(
    composition = composition,
    differences = differences,
    bootstraps = bootstraps,
    transitions = transitions,
    exposure_changes = exposure_changes,
    profiles = profiles,
    contrasts = contrasts,
    checks = checks,
    variances = variances,
    boot = boot
  )
}

# matrix checks for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param lv Named input or setting from the preceding operation.
#' @param inputs Named input or setting from the preceding operation.
#' @param out Named input or setting from the preceding operation.
#' @param population Named input or setting from the preceding operation.
#' @param panel Named input or setting from the preceding operation.
#' @param matrices Named input or setting from the preceding operation.
#' @param reconciliations Named input or setting from the preceding operation.
#' @param movements Named input or setting from the preceding operation.
#' @param bdir Named input or setting from the preceding operation.
#' @param b Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
#' @param profiles Compact results accumulated over preceding levels.
#' @param contrasts Compact results accumulated over preceding levels.
#' @param variances Compact results accumulated over preceding levels.
#' @param composition Compact results accumulated over preceding levels.
#' @param differences Compact results accumulated over preceding levels.
#' @param bootstraps Compact results accumulated over preceding levels.
#' @param transitions Compact results accumulated over preceding levels.
#' @param exposure_changes Compact results accumulated over preceding levels.
#' @param checks Compact results accumulated over preceding levels.
#' @param matrix_root Frozen 3 km B folder holding each level's full distance matrix.
#' @param buffer_km Eligibility distance for the streamed diagnostics.
resolution_multicity_matrix_checks <- function(
  city,
  lv,
  inputs,
  out,
  population,
  panel,
  matrices,
  reconciliations,
  movements,
  bdir,
  b,
  profiles,
  contrasts,
  variances,
  composition,
  differences,
  bootstraps,
  transitions,
  exposure_changes,
  checks,
  matrix_root,
  buffer_km = 3) {
  u <- NULL
  oc <- NULL
  exact <- NULL
  passed <- NULL
  distance <- as.data.table(arrow::read_parquet(file.path(matrix_root, lv,
    "matrix_geo_station_distances.parquet")))
  unit_pop <- as.data.table(arrow::read_parquet(file.path(inputs,
    paste0(lv, "_unit_population.parquet"))))
  movement <- as.data.table(arrow::read_parquet(file.path(inputs,
    paste0(lv, "_movement.parquet"))))
  movement <- merge(movement, population, by = "geo_id")
  movements[[lv]] <- data.table(level = lv,
    population = sum(movement$pop), mean_km = weighted.mean(movement$displacement_km,
    movement$pop), median_km = median(movement$displacement_km),
    p90_km = unname(quantile(movement$displacement_km, .9)),
    max_km = max(movement$displacement_km))
  for (poll in c("pm10", "pm25")) {
    message(city, " ", lv, ": streaming ", poll, " matrix diagnostics")
    m <- resolution_matrix_diagnostics(distance, panel, poll, buffer_km = buffer_km)
    u <- merge(m$units, unit_pop, by.x = "geo_id", by.y = "parent_id", all.x = TRUE)
    u[, `:=`(level = lv, pollutant = poll)]
    arrow::write_parquet(u, file.path(bdir, paste0(poll, "_matrix_units.parquet")))
    arrow::write_parquet(m$edges, file.path(bdir, paste0(poll,
      "_eligible_edges.parquet")))
    resolution_write(m$stations, file.path(bdir, paste0(poll, "_station_manifest")))
    fine_stations <- if (lv == "fine") m$stations[eligible == TRUE, station_id] else {
      fread(file.path(out, "B", "fine", paste0(poll, "_station_manifest.csv")))[
        eligible == TRUE, station_id]
    }
    stations_now <- m$stations[eligible == TRUE, station_id]
    matrices[[length(matrices) + 1L]] <- data.table(level = lv, pollutant = poll,
      geography_units = uniqueN(distance$geo_id),
        catalog_stations = uniqueN(distance$station_id),
      matrix_pairs = nrow(distance), active_stations = sum(m$stations$active),
      contributing_stations = length(stations_now),
      catalog_eligible_stations = uniqueN(m$edges$station_id),
      catalog_covered_units = uniqueN(m$edges$geo_id),
      catalog_unit_coverage_share = uniqueN(m$edges$geo_id) / uniqueN(distance$geo_id),
      observed_hours_mean = mean(u[observed_hours > 0, observed_hours]),
      observed_hours_p10 = unname(quantile(u[observed_hours > 0, observed_hours], .1)),
      observed_hours_p90 = unname(quantile(u[observed_hours > 0, observed_hours], .9)),
      stations_gained = paste(setdiff(stations_now, fine_stations), collapse = ";"),
      stations_lost = paste(setdiff(fine_stations, stations_now), collapse = ";"),
      covered_units = sum(u$observed_hours > 0),
      covered_adult_units = sum(u$observed_hours > 0 & u$adult_population > 0),
      unit_coverage_share = mean(u$observed_hours > 0),
      covered_adults = sum(u[observed_hours > 0, adult_population]),
      covered_education_adults = sum(u[observed_hours > 0, education_population]),
      adult_coverage_share = sum(u[observed_hours > 0,
        adult_population]) / sum(population$pop),
      mean_eligible_stations = mean(u$eligible_stations),
      median_nearest_active_km = median(u$nearest_active_km),
      median_static_neff = median(u$static_neff, na.rm = TRUE),
      mean_hourly_neff = mean(u$hourly_neff_mean, na.rm = TRUE),
      missing_rows = sum(u$eligible_stations == 0),
      max_row_sum_error = max(u$hourly_row_sum_error, na.rm = TRUE))
    compare <- merge(u[observed_hours > 0], b, by = "geo_id", all.x = TRUE)
    for (suffix in c("avg", "it1", "it2")) {
      oc <- if (suffix == "avg") paste0("avg_", poll) else paste0("hrs_d_", poll, "_",
        suffix)
      errors <- abs(compare[[paste0(suffix, "_reconstructed")]] - compare[[oc]])
      exact <- !anyNA(errors) && all(errors < 1e-8)
      within_boundary <- if (suffix == "avg") exact else {
        values <- compare[[oc]]
        all(!is.na(values) & values >= compare[[paste0(suffix, "_lower")]] &
              values <= compare[[paste0(suffix, "_upper")]])
      }
      passed <- exact || within_boundary
      reconciliations[[length(reconciliations) + 1L]] <- data.table(level = lv,
        outcome = oc, units = length(errors), max_error = max(errors),
        differing_units = sum(errors > 1e-8), exact = exact,
        boundary_reconciled = within_boundary, passed = passed)
      stopifnot(passed)
    }
  }
  # Checkpoint compact outputs after each level; a failed run remains marked incomplete.
  tables <- list(profiles = profiles, contrasts = contrasts, variances = variances,
    composition = composition, differences = differences, bootstraps = bootstraps,
    transitions = transitions, matrices = matrices, reconciliations = reconciliations,
    checks = checks, exposure_changes = exposure_changes)
  for (name in names(tables)) {
    tab <- rbindlist(tables[[name]], fill = TRUE)
    tab[, city_id := city]
    resolution_write(tab, file.path(out, name))
  }
  list(
    reconciliations = reconciliations,
    movements = movements,
    u = u,
    matrices = matrices,
    oc = oc,
    exact = exact,
    passed = passed
  )
}

# verify city for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param all_tables Named input or setting from the preceding operation.
#' @param inputs Named input or setting from the preceding operation.
#' @param out Named input or setting from the preceding operation.
#' @param population Named input or setting from the preceding operation.
#' @param cells Named input or setting from the preceding operation.
#' @param exposure Named input or setting from the preceding operation.
#' @param outcomes Named input or setting from the preceding operation.
#' @param profiles Named input or setting from the preceding operation.
#' @param contrasts Named input or setting from the preceding operation.
#' @param variances Named input or setting from the preceding operation.
#' @param movements Named input or setting from the preceding operation.
#' @param oc Named input or setting from the preceding operation.
#' @param good_keys Fine units with a parent at every level: the Design A sample.
#' @param group_col Individual group column: "edu_quintile" or "edu_level".
#' @param groups The city's groups; the anchor contrasts the first with the last.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
#   The regression anchor uses the Design A sample, so units lacking a parent at some
#   level (covered only at wider buffers) enter neither side of the comparison.
resolution_multicity_verify_city <- function(
  city,
  all_tables,
  inputs,
  out,
  population,
  cells,
  exposure,
  outcomes,
  profiles,
  contrasts,
  variances,
  movements,
  oc,
  good_keys,
  group_col = "edu_quintile",
  groups = 1:5) {
  resolution_write(rbindlist(movements), file.path(out, "representative_point_movement"))
  c_profiles <- rbindlist(profiles)[design == "C"]
  c_population <- c_profiles[, .(population = sum(population)), by = .(level, outcome)]
  stopifnot(all(c_population[, .(n = uniqueN(round(population, 6))),
    by = outcome]$n == 1L))
  # A variance must decline along validated branches, not across the locality branch.
  variance <- rbindlist(variances)
  nesting <- fread(file.path(inputs, "nesting.csv"))
  for (i in seq_len(nrow(nesting))) for (oc in outcomes) {
    v_child <- variance[level == nesting$child_level[i] & outcome == oc,
      retained_variance]
    v_parent <- variance[level == nesting$parent_level[i] & outcome == oc,
      retained_variance]
    stopifnot(v_parent <= v_child + 1e-8)
  }
  # The maintained regression and the direct mean contrast must agree at fine support.
  regression <- compute_exposure_regressions(exposure[geo_id %chin% good_keys], cells,
    group_col = group_col,
    group_values = groups, base_group = groups[length(groups)], year_filter = 2023L,
    normalized = FALSE, quiet = TRUE,
    outcome_pattern = "^(avg_(pm10|pm25)|hrs_d_(pm10|pm25)_it[12])$")
  direct <- rbindlist(contrasts)[design == "A" & level == "fine"]
  # Construct column names explicitly because replacement strings are scalar in sub().
  regression[, outcome_col := ifelse(outcome == "avg", paste0("avg_", pollutant),
                                    sub("hrs_d", paste0("hrs_d_", pollutant), outcome)),
             by = .(outcome, pollutant)]
  joined <- merge(regression[group == groups[1]], direct, by.x = "outcome_col",
                  by.y = "outcome")
  stopifnot(nrow(joined) == length(outcomes),
    max(abs(joined$estimate - joined$gap)) < 1e-8)
  resolution_write(data.table(check = "weighted_mean_regression_anchor", passed = TRUE,
    max_error = max(abs(joined$estimate - joined$gap))), file.path(out,
      "regression_anchor"))
  all_tables[[city]] <- rbindlist(contrasts)
  writeLines(c("complete", "conditional on saved 2023 pollution data",
    "Derived geometry inputs; not a clean-source reproduction."), file.path(out,
      "STATUS.txt"))
  list(
    nesting = nesting,
    direct = direct,
    all_tables = all_tables
  )
}

# reference anchor for the preserved resolution specification.
#' @param z Named input or setting from the preceding operation.
#' @param direct Named input or setting from the preceding operation.
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_multicity_reference_anchor <- function(
  z,
  direct) {
  # Compare the whole Bogota A ladder to the historical saved calculation.
  if (file.exists(here::here("data", "processed", "resolution_sensitivity",
                            "resolution_ci_bogota_3km_2023.csv"))) {
  root <- here::here("data", "processed", "resolution_sensitivity")
  p <- fread(file.path(root, "bogota_2018", "profiles.csv"))[design == "A"]
  p[, reference_mean := mean[edu_quintile == 5L], by = .(level, outcome)]
  p[, direct := mean / reference_mean - 1]
  level_map <- c(fine = "manzana", seccion = "seccion_urbana", sector = "sector_urbano",
                 localidad = "localidad", municipio = "municipio")
  p[, resolution_level := unname(level_map[level])]
  p[, pollutant := ifelse(grepl("pm25", outcome), "pm25", "pm10")]
  p[, outcome_label := gsub("_pm10|_pm25", "", outcome)]
  p[, group := edu_quintile]
  old <- fread(file.path(root, "resolution_ci_bogota_3km_2023.csv"))[
    design == "A"]
  z <- merge(p, old, by.x = c("resolution_level", "pollutant", "outcome_label", "group"),
             by.y = c("resolution_level", "pollutant", "outcome", "group"))
  stopifnot(nrow(z) == 150, max(abs(z$estimate - z$direct)) < 1e-8)
  resolution_write(data.table(check = "Bogota_A_all_levels_reference",
    coefficients = nrow(z),
    max_error = max(abs(z$estimate - z$direct)), passed = TRUE),
    file.path(root, "bogota_2018", "all_levels_reference_anchor"))
  print(z[, .(coefficients = .N, max_error = max(abs(estimate - direct)))])
  } else {
    stop("The saved Bogota resolution reference is required for the multicity anchor.")
  }
  list(
    z = z
  )
}

# save tables for the preserved resolution specification.
#' @param city Named input or setting from the preceding operation.
#' @param lv Named input or setting from the preceding operation.
#' @param cities Named input or setting from the preceding operation.
#' @param exposure Named input or setting from the preceding operation.
#' @param z Named input or setting from the preceding operation.
#' @param boot Named input or setting from the preceding operation.
#' @param u Named input or setting from the preceding operation.
#' @param oc Named input or setting from the preceding operation.
#' @param exact Named input or setting from the preceding operation.
#' @param passed Named input or setting from the preceding operation.
#' @param nesting Named input or setting from the preceding operation.
#' @param buffer_km Eligibility distance; other buffers write resolution_multicity_<b>km_*.
#' @param grouping Other groupings write resolution_multicity_<folder>_[<b>km_]*, with
#   the folder of resolution_buffer_root().
#' @return Named intermediate objects; existing diagnostic writes are preserved.
#' @details See doc/RESOLUTION_SENSITIVITY.md for populations and A/B/C definitions.
resolution_multicity_save_tables <- function(
  city,
  lv,
  cities,
  exposure,
  z,
  boot,
  u,
  oc,
  exact,
  passed,
  nesting,
  buffer_km = 3,
  grouping = "edu_quintile") {
  table_dir <- here::here("results", "tables")
  root <- resolution_buffer_root(buffer_km, grouping)
  folder <- basename(resolution_buffer_root(3, grouping))
  prefix <- paste0("resolution_multicity_",
    if (grouping != "edu_quintile") paste0(folder, "_"),
    if (buffer_km != 3) paste0(buffer_km, "km_"))
  for (name in c("geographic_audit", "population_denominators", "profiles", "contrasts",
                 "variances", "composition",
                 "differences", "matrices", "checks", "reconciliations", "transitions",
                 "exposure_changes", "representative_point_movement")) {
    tables <- lapply(cities, function(city) {
      # Geography and denominators do not depend on the buffer; prepare writes them once.
      source_root <- if (name %in% c("geographic_audit", "population_denominators")) {
        resolution_buffer_root(3)
      } else root
      x <- fread(file.path(source_root, city, paste0(name, ".csv")))
      x[, city_id := city]
      x
    })
    fwrite(rbindlist(tables, fill = TRUE), file.path(table_dir,
                                                   paste0(prefix, name, ".csv")))
  }

  # Compact audit products are produced from completed, frozen city products.
  verification_summary <- list()
  patterns <- list()
  for (city in cities) {
    city_dir <- file.path(root, city)
    input_dir <- here::here("data", "interim", "resolution_sensitivity", city)
    check <- fread(file.path(city_dir, "checks.csv"))
    recon <- fread(file.path(city_dir, "reconciliations.csv"))
    boot <- fread(file.path(city_dir, "differences.csv"))
    dist <- fread(file.path(input_dir, "distance_anchor.csv"))
    nesting <- fread(file.path(input_dir, "nesting.csv"))
    source_status <- fread(file.path(input_dir, "source_status.csv"))
    verification_summary[[city]] <- data.table(city_id = city,
      check = c("frozen_population_and_IDW", "matrix_exposure_reconciliation",
                "fine_distance_anchor", "nested_branches", "paired_bootstrap",
                "preserved_source_geometry"),
      status = c(if (all(check$passed)) "passed" else "failed",
        if (all(recon$passed) && all(recon$exact)) "passed" else
          if (all(recon$passed)) "qualified_threshold_boundaries" else "failed",
        if (all(dist$passed)) "passed" else "failed",
        if (all(nesting$crossing_children == 0)) "passed" else "failed",
        if (all(boot[n_boot > 0, valid_replicates == n_boot])) {
          if (any(boot$n_boot > 0)) "passed" else "descriptive_only"
        } else "failed",
        if (all(source_status$exists)) "available_or_not_declared_missing" else
          "missing_source_snapshots"),
      detail = c(paste(nrow(check), "checks"),
        paste(sum(!recon$exact), "checks require floating-point boundary envelopes"),
        paste("maximum distance error (km)", max(dist$max_error)),
        paste(nrow(nesting), "directed nesting checks"),
        paste(sum(boot$n_boot > 0), "paired intervals; 999 draws each"),
        paste(sum(!source_status$exists), "declared source paths absent")))
    level_table <- fread(file.path(input_dir, "levels.csv"))
    for (lv in level_table$level) {
      matrix_dir <- file.path(city_dir, "B", lv)
      d <- as.data.table(arrow::read_parquet(file.path(resolution_buffer_root(3), city,
        "B", lv, "matrix_geo_station_distances.parquet")))
      stopifnot(nrow(d) == uniqueN(d$geo_id) * uniqueN(d$station_id),
        !anyDuplicated(d[, .(geo_id, station_id)]),
        all(is.finite(d$distance_km)), all(d$distance_km >= 0))
      exposure <- as.data.table(arrow::read_parquet(file.path(matrix_dir,
        "resolution_idw_exposure.parquet")))
      for (poll in c("pm10", "pm25")) {
        u <- as.data.table(arrow::read_parquet(file.path(matrix_dir,
          paste0(poll, "_matrix_units.parquet"))))
        observed <- u[observed_hours > 0]
        stopifnot(all(abs(observed$static_row_sum - 1) < 1e-12),
          all(observed$hourly_row_sum_error < 1e-12),
          setequal(observed$geo_id, exposure[is.finite(get(paste0("avg_", poll))),
            geo_id]))
        hours <- exposure[[paste0("total_hrs_", poll)]][match(observed$geo_id,
                                                              exposure$geo_id)]
        stopifnot(all(observed$observed_hours == as.numeric(hours)))
      }
    }
    verification_summary[[city]] <- rbind(verification_summary[[city]],
      data.table(city_id = city, check = "matrix_dimensions_normalization_hours",
        status = "passed", detail = paste(nrow(level_table),
          "supports; both pollutants")))
    x <- fread(file.path(city_dir, "contrasts.csv"))
    branches <- if (city == "bogota_2018") {
      list(code_hierarchy = c("fine", "seccion", "sector", "municipio"),
           locality = c("fine", "localidad", "municipio"))
    } else if (city == "santiago_2017") {
      list(code_hierarchy = c("fine", "distrito", "comuna"))
    } else list(code_hierarchy = c("fine", "municipio"))
    for (branch in names(branches)) for (design_name in c("A", "B_native")) {
      for (oc in unique(x$outcome)) {
        z <- x[design == design_name & outcome == oc & level %chin% branches[[branch]]]
        z <- z[match(branches[[branch]], level)]
        change <- diff(z$gap)
        patterns[[length(patterns) + 1L]] <- data.table(city_id = city,
          branch = branch, design = design_name, outcome = oc,
          population_fixed = design_name == "A", levels = nrow(z),
          non_monotonic = if (anyNA(change)) NA else
            any(change > 1e-8) && any(change < -1e-8),
          finest_gap = z$gap[1], coarsest_gap = z$gap[nrow(z)],
          minimum_step = min(change), maximum_step = max(change))
      }
    }
  }
  fwrite(rbindlist(verification_summary), file.path(table_dir,
    paste0(prefix, "verification.csv")))
  fwrite(rbindlist(patterns), file.path(table_dir, paste0(prefix, "patterns.csv")))
  # Record the exact source files used for this optional execution, without a clean-tree claim.
  code_paths <- c("scripts/process_data/prepare_resolution_inputs.R",
    "scripts/process_data/estimate_resolution_sensitivity.R",
    "scripts/tables_images/figure_resolution_sensitivity.R",
    "src/general_utilities/config_utils_resolution.R",
    "src/general_utilities/process/resolution_sensitivity.R",
    "src/general_utilities/process/resolution_workflow.R",
    "src/general_utilities/plot/resolution_workflow.R",
    "src/general_utilities/process/idw_exposure.R",
    "src/general_utilities/process/distances.R",
    "src/general_utilities/process/exposure_regressions.R",
    "config/analysis_settings.R")
  code_manifest <- data.table(path = code_paths, md5 = unname(tools::md5sum(code_paths)))
  fwrite(code_manifest, file.path(table_dir, paste0(prefix, "code_manifest.csv")))
  writeLines(capture.output(sessionInfo()), file.path(root, "session_info.txt"))

  list(
    verification_summary = verification_summary,
    patterns = patterns,
    exposure = exposure
  )
}
