# ==========================================================================================
# IDB: Air monitoring — robustness designs A/B/C of the specification-sensitivity analysis
# ==========================================================================================
#' @Goal: Prepare frozen city inputs and estimate the A/B/C designs for one specification.
#' @Description: Preparation freezes the manuscript-derived inputs of each city, builds its
#   adult population cells, coarser supports and their distance matrices. Estimation then
#   compares exposure aggregation (A), exposure rebuilt by IDW at each support (B) and
#   area classification (C) for one declared specification and city. Functions write
#   the products they name; definitions are in doc/RESOLUTION_SENSITIVITY.md.
#' @Summary:
#   Paths:       resolution_buffer_root
#   Preparation: resolution_city_supports, resolution_input_paths,
#                resolution_freeze_inputs, resolution_population_cells,
#                resolution_fine_geometry,
#                resolution_population_denominators, resolution_support_keys,
#                resolution_support_matrices
#   Estimation:  resolution_group_cells, resolution_city_inputs,
#                resolution_area_classification, resolution_support_exposure,
#                resolution_compare_designs, resolution_monitoring_diagnostics,
#                resolution_verify_city, run_resolution_city
#   Combination: resolution_reference_anchor, resolution_combine_cities,
#                resolution_verification_summary, resolution_gap_patterns,
#                write_resolution_tables, write_resolution_provenance
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: resolution_buffer_root
#
#' @param buffer_km IDW eligibility distance of the run.
#' @param grouping  "edu_quintile" (frozen quintiles), "edu_quintile_split", "edu_level"
#   (six bands) or "edu_group3" (three groups).
#' @param root      Project root that holds data/; the canonical root by default.
#
#' @return  the processed folder of the run: the 3 km quintile root, a grouping folder
#   beneath it, and a buffer_<b>km folder for buffers other than 3 km.
#
#' @details
#   Other buffers and groupings never overwrite the 3 km quintile products or the frozen
#   B matrices that preparation writes in the 3 km quintile root.
# ------------------------------------------------------------------------------------------
resolution_buffer_root <- function(buffer_km, grouping = "edu_quintile",
                                   root = here::here()) {
  folders <- c(edu_quintile = "", edu_quintile_split = "quintile_split",
               edu_level = "education_level", edu_group3 = "education_group3")
  grouping <- match.arg(grouping, names(folders))
  out <- file.path(root, "data", "processed", "resolution_sensitivity")
  if (nzchar(folders[[grouping]])) out <- file.path(out, folders[[grouping]])
  if (buffer_km == 3) out else file.path(out, paste0("buffer_", buffer_km, "km"))
}

# ------------------------------------------------------------------------------------------
# Function: resolution_city_supports
#
#' @param city string; "bogota_2018", "santiago_2017" or "sao_paulo_2010".
#
#' @return  list; the city's source file names, fine identifier column and width, the
#   supports (levels, labels, prefix widths), their directed nesting pairs and the
#   original source snapshots expected to be preserved.
#
#' @details
#   City geography is explicit; no common hierarchy is imposed. Bogota's localidad has
#   no prefix width: its parent comes from the manzana-localidad crosswalk.
# ------------------------------------------------------------------------------------------
resolution_city_supports <- function(city) {
  if (city == "bogota_2018") {
    list(geo_file = "bogota_area_metro_census_tracts_2018.gpkg",
         station_file = "bogota_2018_stations_buffer_metro.gpkg",
         census_file = "census_2018_metro_collapsed.parquet",
         id_col = "GEO_ID", fine_width = 22L,
         levels = c("fine", "seccion", "sector", "localidad", "municipio"),
         labels = c("Fine census unit", "Seccion", "Sector", "Localidad/municipio",
                    "Municipio"),
         widths = c(22L, 20L, 18L, NA_integer_, 5L),
         nesting = list(c("fine", "seccion"), c("seccion", "sector"),
                        c("sector", "municipio"), c("fine", "localidad"),
                        c("localidad", "municipio")),
         missing_sources = character())
  } else if (city == "santiago_2017") {
    list(geo_file = "gran_santiago_zonas_2017.gpkg",
         station_file = "gran_santiago_stations_buffer_metro_2017.gpkg",
         census_file = "census_collapsed_2017.parquet",
         id_col = "zona_id", fine_width = 11L,
         levels = c("fine", "distrito", "comuna"),
         labels = c("Zona censal", "Distrito censal", "Comuna"),
         widths = c(11L, 7L, 5L),
         nesting = list(c("fine", "distrito"), c("distrito", "comuna")),
         missing_sources = file.path("data/downloads/santiago/metro_area/2017",
           paste0("GRAN_SANTIAGO_13_", c("metro.geojson", "zonas.geojson",
                                         "count.json"))))
  } else if (city == "sao_paulo_2010") {
    list(geo_file = "sao_paulo_metro_2010_weighting_areas.gpkg",
         station_file = "sao_paulo_stations_buffer_metro_2010.gpkg",
         census_file = "census_sp_collapsed_2010.parquet",
         id_col = "code_weighting", fine_width = 13L,
         levels = c("fine", "municipio"),
         labels = c("Area de ponderacao", "Municipio"),
         widths = c(13L, 7L),
         nesting = list(c("fine", "municipio")),
         missing_sources =
           "data/downloads/sao_paulo/metro_area/sp_weighting_areas_2010.rds")
  } else stop("Unknown resolution city: ", city)
}

# ------------------------------------------------------------------------------------------
# Function: resolution_input_paths
#
#' @param city     string; city id.
#' @param supports list; from resolution_city_supports().
#
#' @return  named character vector of the manuscript-derived input paths: geometry,
#   stations, census, groups, exposure (3 km), distances, pollution (2023 partition) and,
#   for Bogota, the manzana-localidad crosswalk.
# ------------------------------------------------------------------------------------------
resolution_input_paths <- function(city, supports) {
  city_name <- sub("_[0-9]+$", "", city)
  geo_dir <- here::here("data", "interim", "geospatial_data", city_name)
  census_dir <- here::here("data", "interim", "census", city)
  idw_dir <- here::here("data", "processed", "idw_estimates", city)
  paths <- c(geometry = file.path(geo_dir, supports$geo_file),
    stations = file.path(geo_dir, supports$station_file),
    census = file.path(census_dir, supports$census_file),
    groups = file.path(idw_dir, paste0(city, "_indiv_groups.parquet")),
    exposure = file.path(idw_dir, paste0(city, "_3km_idw_exposure.parquet")),
    distances = here::here("data", "processed", "distances_matrices", city,
                           "matrix_geo_station_distances.parquet"),
    pollution = here::here("data", "processed", "monitoring_stations_outliers",
                           paste0(city_name, "_metro_clean"), "year=2023",
                           "data.parquet"))
  if (city == "bogota_2018") paths <- c(paths, locality = file.path(geo_dir,
    "bogota_manzana_localidad_crosswalk.parquet"))
  paths
}

# ------------------------------------------------------------------------------------------
# Function: resolution_freeze_inputs
#
#' @param city         string; city id.
#' @param supports     list; from resolution_city_supports().
#' @param paths        named character; from resolution_input_paths().
#' @param frozen_dir   string; the city's frozen-input folder (data/interim/...).
#' @param prepared_dir string; the city's prepared folder in the 3 km quintile root.
#
#' @return  data.table; the input manifest (input, relative path, bytes, MD5).
#
#' @details
#   Writes input_manifest and source_status. A manifest that already exists must match,
#   so a later run cannot silently change the input vintage.
# ------------------------------------------------------------------------------------------
resolution_freeze_inputs <- function(city, supports, paths, frozen_dir, prepared_dir) {
  dir.create(frozen_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(prepared_dir, recursive = TRUE, showWarnings = FALSE)
  if (any(!file.exists(paths))) stop("Missing input: ", paths[!file.exists(paths)][1])
  manifest <- data.table(input = names(paths), path = sub(paste0(here::here(), "/"),
    "", paths, fixed = TRUE), bytes = file.info(paths)$size,
    md5 = unname(tools::md5sum(paths)))
  # see @details: an existing manifest must match
  manifest_path <- file.path(frozen_dir, "input_manifest.csv")
  if (file.exists(manifest_path) &&
      !isTRUE(all.equal(fread(manifest_path), manifest, check.attributes = FALSE))) {
    stop("Frozen input manifest changed for ", city, ". Review the input vintage first.")
  }
  resolution_write(manifest, file.path(frozen_dir, "input_manifest"))
  resolution_write(data.table(path = supports$missing_sources,
    exists = file.exists(here::here(supports$missing_sources))),
    file.path(frozen_dir, "source_status"))
  manifest
}

# ------------------------------------------------------------------------------------------
# Function: resolution_population_cells
#
#' @param supports   list; from resolution_city_supports().
#' @param paths      named character; from resolution_freeze_inputs().
#' @param frozen_dir string; the city's frozen-input folder.
#
#' @return  list: census (collapsed census units), population (geo_id, pop: all adults)
#   and cells (geo_id, edu_quintile, person_weight: education-reporting adults).
#
#' @details
#   Writes adult_population, individual_quintile_cells and census_education.parquet.
#   Adults are the 25-plus population of the manuscript groups file.
# ------------------------------------------------------------------------------------------
resolution_population_cells <- function(supports, paths, frozen_dir) {
  census <- as.data.table(arrow::read_parquet(paths["census"]))
  individual <- as.data.table(arrow::read_parquet(paths["groups"]))
  stopifnot(all(individual$adult == 1), !anyDuplicated(census$geo_id),
            all(nchar(census$geo_id) == supports$fine_width))
  population <- individual[, .(pop = sum(person_weight, na.rm = TRUE)), by = geo_id]
  cells <- individual[!is.na(edu_quintile) & is.finite(person_weight) & person_weight > 0,
    .(person_weight = sum(person_weight)), by = .(geo_id, edu_quintile)]
  rm(individual); gc(verbose = FALSE)
  resolution_write(population, file.path(frozen_dir, "adult_population"))
  resolution_write(cells, file.path(frozen_dir, "individual_quintile_cells"))
  arrow::write_parquet(census, file.path(frozen_dir, "census_education.parquet"))
  list(census = census, population = population, cells = cells)
}

# ------------------------------------------------------------------------------------------
# Function: resolution_fine_geometry
#
#' @param city       string; city id.
#' @param supports   list; from resolution_city_supports().
#' @param paths      named character; from resolution_freeze_inputs().
#' @param census_ids character; census geo_ids, the spelling the geometry is reconciled to.
#' @param frozen_dir string; the city's frozen-input folder.
#
#' @return  sf; the fine polygons with a reconciled geo_id column.
#
#' @details
#   Writes identifier_ledger (raw id, reconciled id, census match). Bogota pads rural
#   ids to 22 characters even for units without census adults, so every natural rural
#   prefix is preserved at every support.
# ------------------------------------------------------------------------------------------
resolution_fine_geometry <- function(city, supports, paths, census_ids, frozen_dir) {
  geo <- sf::st_read(paths["geometry"], quiet = TRUE)
  raw_id <- as.character(geo[[supports$id_col]])
  geo$geo_id <- reconcile_geo_ids(raw_id, census_ids, quiet = TRUE)
  if (city == "bogota_2018") {
    geo$geo_id <- stringi::stri_pad_right(geo$geo_id, 22L, pad = "0")
  }
  stopifnot(!anyDuplicated(geo$geo_id), all(nchar(geo$geo_id) == supports$fine_width),
            all(sf::st_is_valid(geo)), !any(sf::st_is_empty(geo)))
  if (city == "sao_paulo_2010") {
    stopifnot(all(substr(geo$geo_id, 1, 7) == as.character(geo$code_muni)))
  }
  ledger <- data.table(raw_id = raw_id, geo_id = geo$geo_id,
                       census_match = geo$geo_id %chin% census_ids)
  resolution_write(ledger, file.path(frozen_dir, "identifier_ledger"))
  geo
}

# ------------------------------------------------------------------------------------------
# Function: resolution_population_denominators
#
#' @param city         string; city id.
#' @param supports     list; from resolution_city_supports().
#' @param paths        named character; from resolution_freeze_inputs().
#' @param population   data.table; geo_id, pop (all adults).
#' @param cells        data.table; education-reporting adult cells.
#' @param geometry     sf; from resolution_fine_geometry().
#' @param frozen_dir   string; the city's frozen-input folder.
#' @param prepared_dir string; the city's prepared folder.
#
#' @return  data.table; one row of processed and source resident and adult counts.
#
#' @details
#   Writes population_manifest (frozen like the input manifest) and
#   population_denominators. Source counts are rebuilt only for Santiago, by a read-only
#   query of the 2017 census for the selected zones.
# ------------------------------------------------------------------------------------------
resolution_population_denominators <- function(city, supports, paths, population, cells,
                                               geometry, frozen_dir, prepared_dir) {
  person_path <- file.path(dirname(paths["census"]), sub("collapsed", "individual",
                                                        supports$census_file,
                                                        fixed = TRUE))
  population_paths <- c(processed_people = unname(person_path))
  if (city == "santiago_2017") population_paths <- c(population_paths,
    source_census = here::here("data", "downloads", "santiago", "census", "2017",
                              "censo2017.duckdb"))
  population_manifest <- data.table(input = names(population_paths),
    path = sub(paste0(here::here(), "/"), "", population_paths, fixed = TRUE),
    md5 = unname(tools::md5sum(population_paths)))
  pop_manifest_file <- file.path(frozen_dir, "population_manifest.csv")
  if (file.exists(pop_manifest_file)) {
    stopifnot(isTRUE(all.equal(fread(pop_manifest_file), population_manifest,
                              check.attributes = FALSE)))
  }
  resolution_write(population_manifest, file.path(frozen_dir, "population_manifest"))
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
    ids_sql <- paste(DBI::dbQuoteString(con, geometry$geo_id), collapse = ",")
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
  resolution_write(denominators, file.path(prepared_dir, "population_denominators"))
  denominators
}

# ------------------------------------------------------------------------------------------
# Function: resolution_support_keys
#
#' @param city       string; city id.
#' @param supports   list; from resolution_city_supports().
#' @param paths      named character; from resolution_freeze_inputs().
#' @param census_ids character; census geo_ids.
#' @param population data.table; geo_id, pop (all adults).
#' @param geometry   sf; from resolution_fine_geometry().
#' @param frozen_dir string; the city's frozen-input folder.
#
#' @return  data.table; long crosswalk geo_id, level, parent_id over all supports.
#
#' @details
#   Prefix supports take the first width characters of the fine id; Bogota's localidad
#   comes from the crosswalk. Writes adults_without_fine_geometry, nesting (every
#   directed pair must have zero crossing children), non_nested_branches (Bogota),
#   crosswalk and levels (support labels and inference flags).
# ------------------------------------------------------------------------------------------
resolution_support_keys <- function(city, supports, paths, census_ids, population,
                                    geometry, frozen_dir) {
  resolution_write(population[!geo_id %chin% geometry$geo_id],
                   file.path(frozen_dir, "adults_without_fine_geometry"))
  all_ids <- union(geometry$geo_id, census_ids)
  keys <- rbindlist(lapply(seq_along(supports$levels), function(i) {
    data.table(geo_id = all_ids, level = supports$levels[i],
      parent_id = if (is.na(supports$widths[i])) NA_character_ else
        substr(all_ids, 1L, supports$widths[i]))
  }))
  if (city == "bogota_2018") {
    cw <- as.data.table(arrow::read_parquet(paths["locality"]))
    cw[, geo_id := reconcile_geo_ids(geo_id, census_ids, quiet = TRUE)]
    cw[, geo_id := stringi::stri_pad_right(geo_id, 22L, pad = "0")]
    stopifnot(!anyDuplicated(cw$geo_id))
    keys[level == "localidad", parent_id := cw$loc_id[match(geo_id, cw$geo_id)]]
  }
  nest <- resolution_validate_keys(keys, supports$nesting)
  stopifnot(all(nest$crossing_children == 0))
  resolution_write(nest, file.path(frozen_dir, "nesting"))
  if (city == "bogota_2018") resolution_write(resolution_validate_keys(keys,
    list(c("seccion", "localidad"), c("sector", "localidad"))),
    file.path(frozen_dir, "non_nested_branches"))
  resolution_write(keys, file.path(frozen_dir, "crosswalk"))
  levels <- supports$levels
  ladder <- data.table(level = levels, label = supports$labels,
    fine_to_coarse = seq_along(levels),
    bootstrap = levels %chin% c("seccion", "sector", "distrito"),
    secondary_inference = levels %chin% c("fine", "seccion", "sector", "distrito"))
  resolution_write(ladder, file.path(frozen_dir, "levels"))
  keys
}

# ------------------------------------------------------------------------------------------
# Function: resolution_support_matrices
#
#' @param city         string; city id.
#' @param supports     list; from resolution_city_supports().
#' @param paths        named character; from resolution_freeze_inputs().
#' @param population   data.table; geo_id, pop (all adults).
#' @param cells        data.table; education-reporting adult cells.
#' @param geometry     sf; from resolution_fine_geometry().
#' @param keys         data.table; from resolution_support_keys().
#' @param frozen_dir   string; the city's frozen-input folder.
#' @param prepared_dir string; the city's prepared folder (B/<level>/ matrices).
#
#' @return  data.table; the geographic audit, one row per support.
#
#' @details
#   For each support: dissolve the fine polygons (resolution_dissolve()), compute the
#   full unit-to-station distance matrix with the fine evaluation CRS
#   (compute_distance_matrices(), write_distance_matrices()), check that representative
#   points are internal, and record point movement, unit areas and populations. The
#   fine matrix must reproduce the manuscript distances (< 1e-8 km). Writes polygons,
#   points, movement and unit population files, evaluation_crs.wkt, distance_anchor and
#   geographic_audit.
# ------------------------------------------------------------------------------------------
resolution_support_matrices <- function(city, supports, paths, population, cells,
                                        geometry, keys, frozen_dir, prepared_dir) {
  stations <- sf::st_read(paths["stations"], quiet = TRUE)
  fixed_crs <- NULL
  fine_points <- NULL
  audit <- list()
  for (lv in supports$levels) {
    message(city, ": geometry and matrix at ", lv)
    k <- keys[level == lv, .(geo_id, parent_id)]
    polygons <- if (lv == "fine") geometry["geo_id"] else resolution_dissolve(geometry, k)
    sf::st_write(polygons, file.path(frozen_dir, paste0(lv, "_polygons.gpkg")),
                 delete_dsn = TRUE, quiet = TRUE)
    bdir <- file.path(prepared_dir, "B", lv)
    matrix <- compute_distance_matrices(
      stations_sf = stations, station_id_col = "station_name",
      geo_sf = polygons, geo_id_col = "geo_id",
      evaluation_crs = fixed_crs, return_points = TRUE, quiet = TRUE)
    matrix_files <- write_distance_matrices(result = matrix, out_dir = bdir)
    if (lv == "fine") {
      fixed_crs <- matrix$evaluation_crs
      fine_points <- matrix$representative_points
      writeLines(fixed_crs$wkt, file.path(frozen_dir, "evaluation_crs.wkt"))
      old <- as.data.table(arrow::read_parquet(paths["distances"]))
      old[, geo_id := reconcile_geo_ids(geo_id, geometry$geo_id, quiet = TRUE)]
      if (city == "bogota_2018") {
        old[, geo_id := stringi::stri_pad_right(geo_id, 22L, pad = "0")]
      }
      fresh <- merge(old, matrix$geo_station_matrix, by = c("geo_id", "station_id"))
      error <- max(abs(fresh$distance_km.x - fresh$distance_km.y))
      stopifnot(nrow(fresh) == nrow(old), error < 1e-8)
      resolution_write(data.table(check = "fresh_fine_distance_anchor", max_error = error,
        pairs = nrow(fresh), passed = TRUE), file.path(frozen_dir, "distance_anchor"))
    }
    points <- matrix$representative_points
    sf::st_write(points, file.path(frozen_dir, paste0(lv, "_points.gpkg")),
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
    resolution_write(movement, file.path(frozen_dir, paste0(lv, "_movement")))
    child_area <- data.table(geo_id = geometry$geo_id,
      child_area_km2 = as.numeric(sf::st_area(sf::st_transform(geometry,
                                                                fixed_crs))) / 1e6)
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
    resolution_write(unit, file.path(frozen_dir, paste0(lv, "_unit_population")))
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
  audit <- rbindlist(audit)
  resolution_write(audit, file.path(prepared_dir, "geographic_audit"))
  audit
}

# ------------------------------------------------------------------------------------------
# Function: resolution_group_cells
#
#' @param groups_path    string; the frozen individual groups file (manifest "groups").
#' @param quintile_cells data.table; frozen fine-unit quintile cells of the same adults.
#' @param grouping       string; "edu_quintile_split", "edu_level" or "edu_group3".
#' @param bands          character; six education-band columns, lowest level first.
#' @param group_of_level integer; three-group number of each band (edu_group3 only).
#
#' @return  list: cells (fine geo_id x group person weights of reporting adults) and
#   max_error, the largest relative difference from the quintile unit totals.
#
#' @details
#   Levels come from assign_education_level() and three groups merge them; split
#   quintiles come from split_socio_group_cells(). Each unit's education-reporting
#   weight must match its frozen quintile total: same people and weights, only the
#   group labels change. The tolerance absorbs summation order.
# ------------------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------------------
# Function: resolution_city_inputs
#
#' @param specification list; from resolution_specification().
#' @param city          string; city id.
#' @param education_bands     character; band columns of the attainment groupings.
#' @param education_group_map integer; band-to-group map of edu_group3.
#
#' @return  list; the city's frozen inputs for this specification: keys (crosswalk),
#   population, cells of the specification's grouping (group_col, groups), census,
#   ladder, fine exposure and outcomes, cleaned 2023 panel, good_keys (fine units with
#   a parent at every support), the initial checks and the folders of the run.
#
#' @details
#   Frozen input hashes must still match. Buffers other than 3 km read the manuscript
#   fine exposure of that buffer and record its hash in exposure_manifest. Other
#   groupings write their own individual_group_cells (resolution_group_cells()).
#   Marks the city run incomplete in STATUS.txt until resolution_verify_city().
# ------------------------------------------------------------------------------------------
resolution_city_inputs <- function(specification, city, education_bands = NULL,
                                   education_group_map = NULL) {
  buffer_km <- specification$buffer_km
  grouping <- specification$grouping
  message("Estimating resolution sensitivity: ", city, ", ", buffer_km, " km, ",
          grouping)
  frozen_dir <- file.path(specification$frozen_root, city)
  out <- file.path(specification$processed_root, city)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  manifest <- fread(file.path(frozen_dir, "input_manifest.csv"))
  stopifnot(identical(unname(tools::md5sum(here::here(manifest$path))), manifest$md5))
  paths <- setNames(here::here(manifest$path), manifest$input)
  if (buffer_km != 3) {
    paths["exposure"] <- here::here("data", "processed", "idw_estimates", city,
      paste0(city, "_", buffer_km, "km_idw_exposure.parquet"))
    if (!file.exists(paths["exposure"])) {
      stop("No manuscript exposure at ", buffer_km, " km: ", paths["exposure"])
    }
    resolution_write(data.table(input = "exposure", buffer_km = buffer_km,
      path = sub(paste0(here::here(), "/"), "", paths["exposure"], fixed = TRUE),
      md5 = unname(tools::md5sum(paths["exposure"]))),
      file.path(out, "exposure_manifest"))
  }
  keys <- as.data.table(arrow::read_parquet(file.path(frozen_dir, "crosswalk.parquet")))
  population <- as.data.table(arrow::read_parquet(file.path(frozen_dir,
    "adult_population.parquet")))
  cells <- as.data.table(arrow::read_parquet(file.path(frozen_dir,
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
  census <- as.data.table(arrow::read_parquet(file.path(frozen_dir,
    "census_education.parquet")))
  ladder <- fread(file.path(frozen_dir, "levels.csv"))
  exposure <- as.data.table(arrow::read_parquet(paths["exposure"]))[year == 2023]
  if (city == "bogota_2018") {
    exposure[, geo_id := stringi::stri_pad_right(geo_id, 22L, pad = "0")]
  }
  outcomes <- grep("^(avg_(pm10|pm25)|hrs_d_(pm10|pm25)_it[12])$",
                    names(exposure), value = TRUE)
  for (oc in outcomes) set(exposure, j = oc, value = as.numeric(exposure[[oc]]))
  panel <- as.data.table(arrow::read_parquet(paths["pollution"]))
  panel[, station := normalize_station(station)]
  writeLines("incomplete", file.path(out, "STATUS.txt"))
  good_keys <- keys[, .(complete = all(!is.na(parent_id))),
    by = geo_id][complete == TRUE, geo_id]
  list(city = city, buffer_km = buffer_km, grouping = grouping,
       frozen_dir = frozen_dir, out = out,
       matrix_root = file.path(specification$prepared_root, city, "B"),
       exposure_root = file.path(specification$exposure_root, city, "B"),
       # The frozen year=2023 partition only, as in the manuscript.
       source_dir = dirname(paths["pollution"]),
       paths = paths, keys = keys, population = population, cells = cells,
       group_col = grouping, groups = sort(unique(cells[[grouping]])),
       census = census, ladder = ladder, exposure = exposure, outcomes = outcomes,
       panel = panel, good_keys = good_keys, checks = checks)
}

# ------------------------------------------------------------------------------------------
# Function: resolution_area_classification
#
#' @param inputs list; from resolution_city_inputs().
#
#' @return  data.table; long fine-unit to area-group mapping at every support (C).
#
#' @details
#   Both quintile groupings cut area mean schooling into quintiles (resolution_classify());
#   attainment groups take each area's weighted median group
#   (resolution_classify_median()). Writes C_classification.
# ------------------------------------------------------------------------------------------
resolution_area_classification <- function(inputs) {
  area_col <- sub("^edu_", "area_", inputs$group_col)
  if (inputs$grouping %chin% c("edu_quintile", "edu_quintile_split")) {
    c_map <- resolution_classify(inputs$census, inputs$keys)
    setnames(c_map, "area_quintile", area_col)
  } else c_map <- resolution_classify_median(inputs$cells, inputs$keys, inputs$group_col)
  resolution_write(c_map, file.path(inputs$out, "C_classification"))
  c_map
}

# ------------------------------------------------------------------------------------------
# Function: resolution_support_exposure
#
#' @param inputs        list; from resolution_city_inputs().
#' @param lv            string; the support.
#' @param duckdb_mem_gb numeric; DuckDB memory limit, a computing resource only.
#' @param reuse_b       logical; reuse a quintile B rebuild whose input hashes match.
#
#' @return  list: exposure (annual exposure of the support's units, Design B) and checks
#   (data.table rows of the checks performed).
#
#' @details
#   Quintile runs rebuild IDW at the support (aggregate_idw_exposure()) from the frozen
#   full distance matrix and record the input hashes in exposure_inputs.csv. Other
#   groupings copy the quintile run's exposure at the same buffer after checking that
#   its pollution, matrix and IDW-code hashes are current: interpolation never sees
#   groups. At the fine support the rebuild must reproduce the manuscript exposure.
# ------------------------------------------------------------------------------------------
resolution_support_exposure <- function(inputs, lv, duckdb_mem_gb = 4, reuse_b = FALSE) {
  checks <- list()
  message(inputs$city, ": estimating ", lv)
  k <- inputs$keys[level == lv, .(geo_id, parent_id)]
  bdir <- file.path(inputs$out, "B", lv)
  bfile <- file.path(bdir, "resolution_idw_exposure.parquet")
  b_signature <- file.path(bdir, "exposure_inputs.csv")
  matrix_file <- file.path(inputs$matrix_root, lv, "matrix_geo_station_distances.parquet")
  signature_paths <- c(inputs$paths["pollution"], matrix_file,
    here::here("src", "general_utilities", "process", "idw_exposure.R"),
    file.path(inputs$frozen_dir, "individual_quintile_cells.parquet"))
  signature <- data.table(path = signature_paths,
                          md5 = unname(tools::md5sum(signature_paths)))
  if (inputs$grouping != "edu_quintile") {
    # The first three signature rows are the interpolation inputs; cells are not.
    source_file <- file.path(inputs$exposure_root, lv, "resolution_idw_exposure.parquet")
    source_signature <- fread(file.path(inputs$exposure_root, lv, "exposure_inputs.csv"))
    if (!isTRUE(all.equal(source_signature[1:3], signature[1:3],
                          check.attributes = FALSE))) {
      stop("B exposure inputs changed for ", inputs$city, " ", lv,
           ": rerun the quintile B at this buffer first.")
    }
    dir.create(bdir, recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(source_file, bfile, overwrite = TRUE))
    fwrite(data.table(source = source_file, md5 = unname(tools::md5sum(source_file))),
           file.path(bdir, "exposure_source.csv"))
    checks[[length(checks) + 1L]] <- data.table(check = "B_exposure_reused_current_inputs",
      level = lv, outcome = NA_character_, max_error = 0, passed = TRUE)
  } else {
    groups <- merge(inputs$cells, k[!is.na(parent_id)], by = "geo_id")
    groups <- groups[, .(person_weight = sum(person_weight)),
                       by = .(geo_id = parent_id, edu_quintile)]
    groups[, adult := 1L]
    can_reuse <- reuse_b && file.exists(bfile) && file.exists(b_signature) &&
      isTRUE(all.equal(fread(b_signature), signature, check.attributes = FALSE))
    if (!can_reuse) {
      aggregate_idw_exposure(arrow_dir = inputs$source_dir,
        geo_sta_pq = matrix_file, buffer_km = inputs$buffer_km,
        census_col = groups, group_var = "edu_quintile", quintile_level = "individual",
        precomputed_group_col = "edu_quintile", target_years = 2023L,
        mem_gb = duckdb_mem_gb, n_threads = 2L,
        duckdb_temp_dir = file.path(dirname(tempdir()), "resolution_b_duckdb_spill"),
        out_dir = bdir, out_name = "resolution",
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
  for (oc in inputs$outcomes) set(b, j = oc, value = as.numeric(b[[oc]]))
  if (lv == "fine") {
    cmp <- merge(inputs$exposure[, c("geo_id", inputs$outcomes), with = FALSE],
                 b[, c("geo_id", inputs$outcomes), with = FALSE], by = "geo_id",
                 all = TRUE)
    for (oc in inputs$outcomes) {
      a <- cmp[[paste0(oc, ".x")]]; z <- cmp[[paste0(oc, ".y")]]
      error <- max(abs(a - z), na.rm = TRUE)
      same_na <- identical(is.na(a), is.na(z))
      checks[[length(checks) + 1L]] <- data.table(check = "fresh_IDW_anchor",
        level = lv,
        outcome = oc, max_error = error, passed = error < 1e-8 && same_na)
      stopifnot(error < 1e-8, same_na)
    }
  }
  list(exposure = b, checks = checks)
}

# ------------------------------------------------------------------------------------------
# Function: resolution_compare_designs
#
#' @param inputs     list; from resolution_city_inputs().
#' @param lv         string; the support.
#' @param b_exposure data.table; the support's Design B exposure.
#' @param c_map      data.table; from resolution_area_classification().
#
#' @return  named list of the support's tables, each a list of data.tables in outcome
#   order: profiles, contrasts, variances, composition, differences, bootstraps,
#   transitions, exposure_changes and checks.
#
#' @details
#   For each outcome: A aggregates fine exposure over the support with all-adult weights
#   (resolution_aggregate()); B_native uses the rebuilt exposure; B_common, A_common and
#   baseline_common share the pairwise common sample; C reclassifies people by area
#   group with the finest exposure fixed. Group profiles and gaps come from
#   resolution_estimate(); supported paired changes from resolution_bootstrap(). Also
#   checks A cell invariance, the variance decomposition and the composition identity.
#   Writes assignments/<support>_<outcome>.parquet.
# ------------------------------------------------------------------------------------------
resolution_compare_designs <- function(inputs, lv, b_exposure, c_map) {
  profiles <- list(); contrasts <- list(); variances <- list()
  composition <- list(); differences <- list()
  bootstraps <- list(); transitions <- list(); exposure_changes <- list()
  checks <- list()
  group_col <- inputs$group_col
  groups <- inputs$groups
  cells <- inputs$cells
  ladder <- inputs$ladder
  k <- inputs$keys[level == lv, .(geo_id, parent_id)]
  area_col <- sub("^edu_", "area_", group_col)
  c_common <- c_map[, .(complete = all(!is.na(get(area_col)))),
    by = geo_id][complete == TRUE, geo_id]
  # Every estimate in this run uses its own grouping.
  estimate <- function(x, assignment, inference = FALSE) {
    resolution_estimate(x, assignment, inference, group_col = group_col,
                        groups = groups)
  }
  for (oc in inputs$outcomes) {
    fine <- merge(inputs$exposure[is.finite(get(oc)), .(geo_id, y = get(oc))],
                  inputs$population, by = "geo_id")
    fine <- fine[geo_id %chin% inputs$good_keys]
    a <- resolution_aggregate(fine, k)
    native <- merge(k[!is.na(parent_id)], b_exposure[is.finite(get(oc)),
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
    dir.create(file.path(inputs$out, "assignments"), showWarnings = FALSE)
    arrow::write_parquet(assignment, file.path(inputs$out, "assignments",
                                               paste0(lv, "_", oc, ".parquet")))
  }
  list(profiles = profiles, contrasts = contrasts, variances = variances,
       composition = composition, differences = differences, bootstraps = bootstraps,
       transitions = transitions, exposure_changes = exposure_changes, checks = checks)
}

# ------------------------------------------------------------------------------------------
# Function: resolution_monitoring_diagnostics
#
#' @param inputs     list; from resolution_city_inputs().
#' @param lv         string; the support.
#' @param b_exposure data.table; the support's Design B exposure.
#
#' @return  list: matrices (one row per pollutant), reconciliations (one row per
#   outcome) and movement (the support's representative-point displacement summary).
#
#' @details
#   Streams the full matrix with resolution_matrix_diagnostics() at the run's buffer and
#   requires the streamed annual outcomes to reconcile with the B exposure, exactly or
#   within floating-point threshold envelopes. Writes <pollutant>_matrix_units,
#   _eligible_edges and _station_manifest under B/<support>/. Coarser supports compare
#   their contributing stations with the fine support's manifest.
# ------------------------------------------------------------------------------------------
resolution_monitoring_diagnostics <- function(inputs, lv, b_exposure) {
  matrices <- list()
  reconciliations <- list()
  bdir <- file.path(inputs$out, "B", lv)
  distance <- as.data.table(arrow::read_parquet(file.path(inputs$matrix_root, lv,
    "matrix_geo_station_distances.parquet")))
  unit_pop <- as.data.table(arrow::read_parquet(file.path(inputs$frozen_dir,
    paste0(lv, "_unit_population.parquet"))))
  movement <- as.data.table(arrow::read_parquet(file.path(inputs$frozen_dir,
    paste0(lv, "_movement.parquet"))))
  movement <- merge(movement, inputs$population, by = "geo_id")
  movement <- data.table(level = lv,
    population = sum(movement$pop), mean_km = weighted.mean(movement$displacement_km,
    movement$pop), median_km = median(movement$displacement_km),
    p90_km = unname(quantile(movement$displacement_km, .9)),
    max_km = max(movement$displacement_km))
  for (poll in c("pm10", "pm25")) {
    message(inputs$city, " ", lv, ": streaming ", poll, " matrix diagnostics")
    m <- resolution_matrix_diagnostics(distance, inputs$panel, poll,
                                       buffer_km = inputs$buffer_km)
    u <- merge(m$units, unit_pop, by.x = "geo_id", by.y = "parent_id", all.x = TRUE)
    u[, `:=`(level = lv, pollutant = poll)]
    arrow::write_parquet(u, file.path(bdir, paste0(poll, "_matrix_units.parquet")))
    arrow::write_parquet(m$edges, file.path(bdir, paste0(poll,
      "_eligible_edges.parquet")))
    resolution_write(m$stations, file.path(bdir, paste0(poll, "_station_manifest")))
    fine_stations <- if (lv == "fine") m$stations[eligible == TRUE, station_id] else {
      fread(file.path(inputs$out, "B", "fine", paste0(poll, "_station_manifest.csv")))[
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
        adult_population]) / sum(inputs$population$pop),
      mean_eligible_stations = mean(u$eligible_stations),
      median_nearest_active_km = median(u$nearest_active_km),
      median_static_neff = median(u$static_neff, na.rm = TRUE),
      mean_hourly_neff = mean(u$hourly_neff_mean, na.rm = TRUE),
      missing_rows = sum(u$eligible_stations == 0),
      max_row_sum_error = max(u$hourly_row_sum_error, na.rm = TRUE))
    compare <- merge(u[observed_hours > 0], b_exposure, by = "geo_id", all.x = TRUE)
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
  list(matrices = matrices, reconciliations = reconciliations, movement = movement)
}

# ------------------------------------------------------------------------------------------
# Function: resolution_verify_city
#
#' @param inputs    list; from resolution_city_inputs().
#' @param tables    named list; the city's result lists (profiles, contrasts, variances).
#' @param movements named list; per-support movement summaries.
#
#' @return  data.table; the regression anchor check (one row).
#
#' @details
#   C must hold its population fixed across supports; A's retained variance must decline
#   along every validated nesting pair; and at the fine support the direct mean contrast
#   must equal the maintained regression (compute_exposure_regressions()) on the Design A
#   sample. Writes representative_point_movement and regression_anchor, then marks the
#   city run complete in STATUS.txt.
# ------------------------------------------------------------------------------------------
resolution_verify_city <- function(inputs, tables, movements) {
  out <- inputs$out
  groups <- inputs$groups
  resolution_write(rbindlist(movements), file.path(out, "representative_point_movement"))
  c_profiles <- rbindlist(tables$profiles)[design == "C"]
  c_population <- c_profiles[, .(population = sum(population)), by = .(level, outcome)]
  stopifnot(all(c_population[, .(n = uniqueN(round(population, 6))),
    by = outcome]$n == 1L))
  # A variance must decline along validated branches, not across the locality branch.
  variance <- rbindlist(tables$variances)
  nesting <- fread(file.path(inputs$frozen_dir, "nesting.csv"))
  for (i in seq_len(nrow(nesting))) for (oc in inputs$outcomes) {
    v_child <- variance[level == nesting$child_level[i] & outcome == oc,
      retained_variance]
    v_parent <- variance[level == nesting$parent_level[i] & outcome == oc,
      retained_variance]
    stopifnot(v_parent <= v_child + 1e-8)
  }
  # The maintained regression and the direct mean contrast must agree at fine support.
  regression <- compute_exposure_regressions(
    inputs$exposure[geo_id %chin% inputs$good_keys], inputs$cells,
    group_col = inputs$group_col,
    group_values = groups, base_group = groups[length(groups)], year_filter = 2023L,
    normalized = FALSE, quiet = TRUE,
    outcome_pattern = "^(avg_(pm10|pm25)|hrs_d_(pm10|pm25)_it[12])$")
  direct <- rbindlist(tables$contrasts)[design == "A" & level == "fine"]
  # Construct column names explicitly because replacement strings are scalar in sub().
  regression[, outcome_col := ifelse(outcome == "avg", paste0("avg_", pollutant),
                                    sub("hrs_d", paste0("hrs_d_", pollutant), outcome)),
             by = .(outcome, pollutant)]
  joined <- merge(regression[group == groups[1]], direct, by.x = "outcome_col",
                  by.y = "outcome")
  stopifnot(nrow(joined) == length(inputs$outcomes),
    max(abs(joined$estimate - joined$gap)) < 1e-8)
  anchor <- data.table(check = "weighted_mean_regression_anchor", passed = TRUE,
    max_error = max(abs(joined$estimate - joined$gap)))
  resolution_write(anchor, file.path(out, "regression_anchor"))
  writeLines(c("complete", "conditional on saved 2023 pollution data",
    "Derived geometry inputs; not a clean-source reproduction."), file.path(out,
      "STATUS.txt"))
  anchor
}

# ------------------------------------------------------------------------------------------
# Function: run_resolution_city
#
#' @param specification       list; from resolution_specification().
#' @param city                string; city id.
#' @param education_bands     character; band columns of the attainment groupings.
#' @param education_group_map integer; band-to-group map of edu_group3.
#' @param duckdb_mem_gb       numeric; DuckDB memory limit of the B rebuild.
#' @param reuse_b             logical; reuse a matching quintile B rebuild.
#
#' @return  named list of the city's result tables, as written: profiles, contrasts,
#   variances, composition, differences, bootstraps, transitions, matrices,
#   reconciliations, checks and exposure_changes, each with city_id.
#
#' @details
#   Estimates the A/B/C designs for one specification and city:
#   resolution_city_inputs() reads the frozen inputs; resolution_area_classification()
#   assigns area groups (C); then, for each support, resolution_support_exposure()
#   rebuilds B, resolution_compare_designs() estimates the designs and
#   resolution_monitoring_diagnostics() reconciles the monitoring matrix; finally
#   resolution_verify_city() checks the city. Writes every table in the specification's
#   processed folder (data/processed/resolution_sensitivity/...) under the city id.
# ------------------------------------------------------------------------------------------
run_resolution_city <- function(specification, city, education_bands = NULL,
                                education_group_map = NULL, duckdb_mem_gb = 4,
                                reuse_b = FALSE) {
  inputs <- resolution_city_inputs(specification, city, education_bands,
                                   education_group_map)
  c_map <- resolution_area_classification(inputs)
  table_names <- c("profiles", "contrasts", "variances", "composition", "differences",
                   "bootstraps", "transitions", "matrices", "reconciliations", "checks",
                   "exposure_changes")
  tables <- setNames(rep(list(list()), length(table_names)), table_names)
  tables$checks <- inputs$checks
  movements <- list()
  for (lv in inputs$ladder$level) {
    support <- resolution_support_exposure(inputs, lv, duckdb_mem_gb, reuse_b)
    designs <- resolution_compare_designs(inputs, lv, support$exposure, c_map)
    monitoring <- resolution_monitoring_diagnostics(inputs, lv, support$exposure)
    tables$checks <- c(tables$checks, support$checks, designs$checks)
    for (name in setdiff(names(designs), "checks")) {
      tables[[name]] <- c(tables[[name]], designs[[name]])
    }
    tables$matrices <- c(tables$matrices, monitoring$matrices)
    tables$reconciliations <- c(tables$reconciliations, monitoring$reconciliations)
    movements[[lv]] <- monitoring$movement
  }
  written <- list()
  for (name in table_names) {
    tab <- rbindlist(tables[[name]], fill = TRUE)
    tab[, city_id := city]
    resolution_write(tab, file.path(inputs$out, name))
    written[[name]] <- tab
  }
  resolution_verify_city(inputs, tables, movements)
  written
}

# ------------------------------------------------------------------------------------------
# Function: resolution_reference_anchor
#
#' @param specification      list; the 3 km quintile specification.
#' @param archived_reference string; the archived Bogota reference table
#   (resolution_ci_bogota_3km_2023.csv).
#
#' @return  data.table; the anchor check (coefficients compared, maximum error).
#
#' @details
#   The retired Bogota reference re-keyed the manzana exposure to five supports. Its
#   Design A ladder must equal this run's Bogota A profiles, all 150 normalized
#   coefficients within 1e-8. Writes bogota_2018/all_levels_reference_anchor.
# ------------------------------------------------------------------------------------------
resolution_reference_anchor <- function(specification, archived_reference) {
  if (!file.exists(archived_reference)) {
    stop("The archived Bogota resolution reference is required for the anchor.")
  }
  city_dir <- file.path(specification$processed_root, "bogota_2018")
  p <- fread(file.path(city_dir, "profiles.csv"))[design == "A"]
  p[, reference_mean := mean[edu_quintile == 5L], by = .(level, outcome)]
  p[, direct := mean / reference_mean - 1]
  level_map <- c(fine = "manzana", seccion = "seccion_urbana", sector = "sector_urbano",
                 localidad = "localidad", municipio = "municipio")
  p[, resolution_level := unname(level_map[level])]
  p[, pollutant := ifelse(grepl("pm25", outcome), "pm25", "pm10")]
  p[, outcome_label := gsub("_pm10|_pm25", "", outcome)]
  p[, group := edu_quintile]
  old <- fread(archived_reference)[design == "A"]
  z <- merge(p, old, by.x = c("resolution_level", "pollutant", "outcome_label", "group"),
             by.y = c("resolution_level", "pollutant", "outcome", "group"))
  stopifnot(nrow(z) == 150, max(abs(z$estimate - z$direct)) < 1e-8)
  anchor <- data.table(check = "Bogota_A_all_levels_reference",
    coefficients = nrow(z),
    max_error = max(abs(z$estimate - z$direct)), passed = TRUE)
  resolution_write(anchor, file.path(city_dir, "all_levels_reference_anchor"))
  anchor
}

# ------------------------------------------------------------------------------------------
# Function: resolution_combine_cities
#
#' @param specification list; from resolution_specification().
#
#' @return  named list of 13 combined tables, one row set per city.
#
#' @details
#   Reads each city's saved CSV so the combined tables keep the types of the saved
#   files. Geography and denominators do not depend on the buffer; they come from the
#   prepared 3 km quintile root. Every declared city must be complete.
# ------------------------------------------------------------------------------------------
resolution_combine_cities <- function(specification) {
  complete <- vapply(specification$cities, function(city) {
    status <- file.path(specification$processed_root, city, "STATUS.txt")
    file.exists(status) && readLines(status, n = 1L) == "complete"
  }, logical(1))
  if (!all(complete)) {
    stop("Incomplete resolution run: ", paste(specification$cities[!complete],
                                              collapse = ", "))
  }
  combined <- list()
  for (name in c("geographic_audit", "population_denominators", "profiles", "contrasts",
                 "variances", "composition",
                 "differences", "matrices", "checks", "reconciliations", "transitions",
                 "exposure_changes", "representative_point_movement")) {
    tables <- lapply(specification$cities, function(city) {
      source_root <- if (name %in% c("geographic_audit", "population_denominators")) {
        specification$prepared_root
      } else specification$processed_root
      x <- fread(file.path(source_root, city, paste0(name, ".csv")))
      x[, city_id := city]
      x
    })
    combined[[name]] <- rbindlist(tables, fill = TRUE)
  }
  combined
}

# ------------------------------------------------------------------------------------------
# Function: resolution_verification_summary
#
#' @param specification list; from resolution_specification().
#
#' @return  data.table; seven verification statuses per city.
#
#' @details
#   Summarizes the saved checks (frozen population and IDW anchors, matrix-exposure
#   reconciliation, fine distance anchor, nesting, paired bootstraps, preserved source
#   snapshots) and re-checks every saved matrix: full dimensions, unit normalization and
#   observed hours equal to the B exposure's reported hours.
# ------------------------------------------------------------------------------------------
resolution_verification_summary <- function(specification) {
  verification <- list()
  for (city in specification$cities) {
    city_dir <- file.path(specification$processed_root, city)
    input_dir <- file.path(specification$frozen_root, city)
    check <- fread(file.path(city_dir, "checks.csv"))
    recon <- fread(file.path(city_dir, "reconciliations.csv"))
    boot <- fread(file.path(city_dir, "differences.csv"))
    dist <- fread(file.path(input_dir, "distance_anchor.csv"))
    nesting <- fread(file.path(input_dir, "nesting.csv"))
    source_status <- fread(file.path(input_dir, "source_status.csv"))
    verification[[city]] <- data.table(city_id = city,
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
      d <- as.data.table(arrow::read_parquet(file.path(specification$prepared_root, city,
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
    verification[[city]] <- rbind(verification[[city]],
      data.table(city_id = city, check = "matrix_dimensions_normalization_hours",
        status = "passed", detail = paste(nrow(level_table),
          "supports; both pollutants")))
  }
  rbindlist(verification)
}

# ------------------------------------------------------------------------------------------
# Function: resolution_gap_patterns
#
#' @param specification list; from resolution_specification().
#
#' @return  data.table; per city, branch, design (A, B_native) and outcome: whether the
#   gap path from fine to coarse support is non-monotonic, and its end points and steps.
#
#' @details
#   Branches follow each city's code hierarchy; Bogota's locality is a separate branch.
# ------------------------------------------------------------------------------------------
resolution_gap_patterns <- function(specification) {
  patterns <- list()
  for (city in specification$cities) {
    x <- fread(file.path(specification$processed_root, city, "contrasts.csv"))
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
  rbindlist(patterns)
}

# ------------------------------------------------------------------------------------------
# Function: write_resolution_tables
#
#' @param specification list; from resolution_specification().
#' @param combined      named list; from resolution_combine_cities().
#' @param verification  data.table; from resolution_verification_summary().
#' @param patterns      data.table; from resolution_gap_patterns().
#
#' @return  character vector of the written CSV paths, invisibly.
#
#' @details
#   Files are <prefix><name>.csv in the specification's table folder
#   (results/tables/specification_sensitivity/robustness_designs/).
# ------------------------------------------------------------------------------------------
write_resolution_tables <- function(specification, combined, verification, patterns) {
  dir.create(specification$table_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- c(combined, list(verification = verification, patterns = patterns))
  files <- file.path(specification$table_dir,
                     paste0(specification$prefix, names(tables), ".csv"))
  for (i in seq_along(tables)) fwrite(tables[[i]], files[i])
  invisible(files)
}

# ------------------------------------------------------------------------------------------
# Function: write_resolution_provenance
#
#' @param specification list; from resolution_specification().
#' @param code_files    character; repository-relative paths of the code used.
#
#' @return  character vector of the two written paths, invisibly.
#
#' @details
#   Records the MD5 of each code file (<prefix>code_manifest.csv in the table folder) and
#   the R session (session_info.txt in the processed folder), without a clean-tree claim.
# ------------------------------------------------------------------------------------------
write_resolution_provenance <- function(specification, code_files) {
  manifest_file <- file.path(specification$table_dir,
                             paste0(specification$prefix, "code_manifest.csv"))
  session_file <- file.path(specification$processed_root, "session_info.txt")
  fwrite(data.table(path = code_files, md5 = unname(tools::md5sum(here::here(code_files)))),
         manifest_file)
  writeLines(capture.output(sessionInfo()), session_file)
  invisible(c(manifest_file, session_file))
}
