# ============================================================================================
# IDB: Air monitoring — reusable geographic-resolution diagnostics
# ============================================================================================
#' @Goal: Compare exposure aggregation, exposure reconstruction, and SES classification.
#' @Description: Pure transformations except the explicit artifact writer. Individual
#   education groups, adult aggregation weights, and estimation weights remain distinct.
#' @Summary: Crosswalks; assignments; profiles; paired inference; monitoring diagnostics.
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

#' @param x Data table to persist.
#' @param path File stem without an extension.
#' @return Invisible path.
#' @details Writes reviewable CSV alongside compact Parquet.
resolution_write <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(x, paste0(path, ".parquet"))
  data.table::fwrite(x, paste0(path, ".csv"), na = "NA")
  invisible(path)
}

#' @param keys Long crosswalk with geo_id, level, parent_id.
#' @param nesting Optional list of directed child/parent level pairs.
#' @return Audit by directed pair.
#' @details Missing parents are retained and reported.
resolution_validate_keys <- function(keys, nesting = list()) {
  if (anyNA(keys$geo_id) || anyDuplicated(keys[, .(geo_id, level)])) {
    stop("Crosswalk must contain one nonmissing fine ID per level.")
  }
  if (any(!is.na(keys$parent_id) & !nzchar(keys$parent_id))) {
    stop("Empty parent identifier.")
  }
  data.table::rbindlist(lapply(nesting, function(pair) {
    child <- keys[level == pair[1], .(geo_id, child = parent_id)]
    parent <- keys[level == pair[2], .(geo_id, parent = parent_id)]
    x <- merge(child, parent, by = "geo_id")
    x <- x[!is.na(child) & !is.na(parent)]
    count <- x[, .(n_parents = data.table::uniqueN(parent)), by = child]
    data.table::data.table(child_level = pair[1], parent_level = pair[2],
                           n_children = nrow(count),
                           crossing_children = sum(count$n_parents > 1L))
  }), fill = TRUE)
}

#' @param x Values.
#' @param w Population weights; only finite positive weights contribute.
#' @return Population variance, or NA for empty support.
#' @details Uses population, not sample, normalization.
resolution_variance <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  x <- x[keep]; w <- w[keep]
  sum(w * (x - stats::weighted.mean(x, w))^2) / sum(w)
}

#' @param fine Fine units with geo_id, y, pop.
#' @param keys Single-level crosswalk with geo_id,parent_id.
#' @return Assignment retaining fine IDs and weights, with original y0 and assigned y.
#' @details Aggregate annual outcomes over covered all-adult population, not SES cells.
resolution_aggregate <- function(fine, keys) {
  if (anyDuplicated(fine$geo_id) || anyDuplicated(keys$geo_id)) {
    stop("Aggregation requires unique fine units and crosswalks.")
  }
  x <- merge(fine[is.finite(y) & is.finite(pop) & pop > 0],
             keys[!is.na(parent_id), .(geo_id, parent_id)], by = "geo_id")
  data.table::setnames(x, "y", "y0")
  x[, y := stats::weighted.mean(y0, pop), by = parent_id]
  x[]
}

#' @param cells Fine geo_id, edu_quintile and person_weight.
#' @param assignment Fine geo_id,y,parent_id.
#' @param inference Whether sufficiently supported geographic-cluster CIs are requested.
#' @return List of full five-group profile and a one-row contrast/feasibility table.
#' @details HC1 matches the existing saturated weighted group regression. Missing
#   endpoint groups or a zero Q5 mean produce NA, never an arbitrary denominator.
resolution_estimate <- function(cells, assignment, inference = FALSE) {
  if (anyDuplicated(assignment$geo_id)) stop("Duplicate exposure assignment.")
  x <- merge(cells, assignment[, .(geo_id, parent_id, y)], by = "geo_id")
  x <- x[is.finite(y) & is.finite(person_weight) & person_weight > 0 &
           edu_quintile %in% 1:5 & !is.na(parent_id)]
  p <- x[, .(population = sum(person_weight),
             mean = stats::weighted.mean(y, person_weight)), by = edu_quintile]
  p <- merge(data.table::data.table(edu_quintile = 1:5), p,
             by = "edu_quintile", all.x = TRUE)
  p[is.na(population), population := 0]
  p[, share := if (sum(population) > 0) population / sum(population) else NA_real_]
  pop <- sum(p$population)
  cluster_pop <- x[, .(w = sum(person_weight)), by = parent_id]
  g <- nrow(cluster_pop)
  effective <- if (pop > 0) 1 / sum((cluster_pop$w / pop)^2) else 0
  max_share <- if (pop > 0) max(cluster_pop$w) / pop else NA_real_
  gap <- p$mean[1] - p$mean[5]
  normalized <- if (is.finite(p$mean[5]) && p$mean[5] != 0) {
    100 * (p$mean[1] / p$mean[5] - 1)
  } else NA_real_
  status <- if (!is.finite(gap)) "missing_endpoint_group" else "descriptive"
  se <- low <- high <- NA_real_
  # Geographic cells reproduce the existing regression's row count and HC1 factor.
  z <- x[, .(w = sum(person_weight), y = stats::weighted.mean(y, person_weight)),
         by = .(parent_id, edu_quintile)]
  n <- nrow(z)
  if (inference && all(p$population > 0) && g >= 50L && effective >= 30 &&
      max_share < 0.2 && n > 5L) {
    z <- merge(z, p[, .(edu_quintile, population, mean)], by = "edu_quintile")
    z[, score := w * (y - mean) / population *
         data.table::fifelse(edu_quintile == 1, 1,
           data.table::fifelse(edu_quintile == 5, -1, 0))]
    scores <- z[, .(score = sum(score)), by = parent_id]
    se <- sqrt(sum(scores$score^2) * g / (g - 1) * (n - 1) / (n - 5))
    low <- gap - stats::qt(0.975, g - 1) * se
    high <- gap + stats::qt(0.975, g - 1) * se
    status <- "conditional_cluster_HC1"
  } else if (inference && is.finite(gap)) status <- "insufficient_cluster_support"
  list(profile = p, contrast = data.table::data.table(
    gap = gap, normalized_pct = normalized, q1 = p$mean[1], q5 = p$mean[5],
    population = pop, n_units = data.table::uniqueN(x$geo_id), n_clusters = g,
    effective_clusters = effective, max_cluster_share = max_share,
    populated_groups = sum(p$population > 0), n_cells = n, se = se,
    lower = low, upper = high, inference_status = status))
}

#' @param cells Fine-unit quintile population cells.
#' @param pair Fine geo_id,parent_id,y0,y1 assignments.
#' @param draws Optional parent-index matrix, one column per replication.
#' @param n_boot Number of replications.
#' @param seed Reproducible seed; the cluster is the comparison parent.
#' @return Replicate estimates and percentile intervals for paired absolute differences.
#' @details Whole-parent resampling leaves each parent's population-weighted A mean
#   unchanged. Precollapsing numerators is algebraically identical to recomputing it
#   separately for each drawn parent, with unique draw identifiers (no cross-products).
resolution_bootstrap <- function(cells, pair, n_boot = 999L, seed = 20260910L,
                                 draws = NULL) {
  if (anyDuplicated(pair$geo_id)) stop("Duplicate paired assignment.")
  x <- merge(cells, pair, by = "geo_id")
  x <- x[edu_quintile %in% c(1L, 5L) & person_weight > 0 &
           is.finite(y0) & is.finite(y1) & !is.na(parent_id)]
  parents <- sort(unique(pair$parent_id[!is.na(pair$parent_id)]))
  if (length(parents) < 2L) stop("Paired inference requires multiple parent clusters.")
  sums <- x[, .(w = sum(person_weight), a = sum(person_weight * y0),
                b = sum(person_weight * y1)), by = .(parent_id, edu_quintile)]
  mat <- matrix(0, length(parents), 6L)
  for (q in c(1L, 5L)) {
    z <- sums[edu_quintile == q]
    offset <- if (q == 1L) 0L else 3L
    mat[match(z$parent_id, parents), offset + 1:3] <- as.matrix(z[, .(w, a, b)])
  }
  if (is.null(draws)) {
    set.seed(seed)
    draws <- replicate(n_boot, sample.int(length(parents), replace = TRUE))
  }
  if (anyNA(draws) || any(draws < 1 | draws > length(parents))) {
    stop("Invalid bootstrap cluster indices.")
  }
  values <- vapply(seq_len(ncol(draws)), function(i) {
    s <- colSums(mat[draws[, i], , drop = FALSE])
    if (s[1] == 0 || s[4] == 0) return(c(NA_real_, NA_real_, NA_real_))
    g0 <- s[2] / s[1] - s[5] / s[4]
    g1 <- s[3] / s[1] - s[6] / s[4]
    c(g0, g1, g1 - g0)
  }, numeric(3))
  reps <- data.table::data.table(replicate = seq_len(ncol(draws)),
                                 baseline = values[1, ], comparison = values[2, ],
                                 delta = values[3, ])
  valid <- is.finite(reps$delta)
  ci <- if (all(valid)) stats::quantile(reps$delta, c(.025, .975)) else c(NA, NA)
  list(replicates = reps, interval = data.table::data.table(
    lower = ci[1], upper = ci[2], n_boot = ncol(draws), valid_replicates = sum(valid),
    n_clusters = length(parents), seed = seed,
    interval_type = if (all(valid)) "conditional_geographic_cluster_bootstrap" else
      "unavailable_empty_bootstrap_group"))
}

#' @param census geo_id,pop_total,pop_educ_known,education_mean.
#' @param keys Long crosswalk.
#' @return Long fine-ID to area-quintile mapping, including unclassified units.
#' @details Schooling means use reporting adults; area group cuts use total adults.
resolution_classify <- function(census, keys) {
  data.table::rbindlist(lapply(unique(keys$level), function(lv) {
    x <- merge(census, keys[level == lv], by = "geo_id")
    areas <- x[!is.na(parent_id), .(
      pop_total = sum(pop_total),
      education_mean = if (sum(pop_educ_known[is.finite(education_mean)]) > 0) {
        stats::weighted.mean(education_mean, pop_educ_known, na.rm = TRUE)
      } else NA_real_), by = .(geo_id = parent_id)]
    assign_socio_group(areas, "education_mean", "pop_total", 5L, "area_quintile")
    merge(keys[level == lv], areas[, .(parent_id = geo_id, area_quintile)],
          by = "parent_id", all.x = TRUE)
  }))
}

#' @param distances Long full geography-station matrix.
#' @param panel Normalized station,datetime,pm10,pm25.
#' @param pollutant Outcome column.
#' @param buffer_km Fixed eligibility distance.
#' @return Per-unit metrics, eligible edges, station manifest, and annual reconciliation.
#' @details Streams one unit at a time; never stores a geography-by-station-by-hour
#   tensor. Dynamic weights use only observed stations at each hour, as in core IDW.
#   Boundary envelopes diagnose floating-point threshold comparisons only; they never
#   replace or round the core estimator's annual outcomes.
resolution_matrix_diagnostics <- function(distances, panel, pollutant,
                                          buffer_km = 3) {
  if (anyDuplicated(distances[, .(geo_id, station_id)])) {
    stop("Duplicate geography-station distance pair.")
  }
  if (anyDuplicated(panel[, .(station, datetime)])) {
    stop("Duplicate station-hour in cleaned panel.")
  }
  d <- data.table::copy(distances)
  d[, station_id := normalize_station(station_id)]
  times <- sort(unique(panel$datetime))
  stations <- sort(unique(d$station_id))
  active <- unique(panel[is.finite(get(pollutant)), station])
  values <- matrix(NA_real_, length(times), length(stations),
                   dimnames = list(NULL, stations))
  z <- panel[station %in% stations]
  values[cbind(match(z$datetime, times), match(z$station, stations))] <- z[[pollutant]]
  eligible <- d[is.finite(distance_km) & distance_km > 0 & distance_km <= buffer_km]
  eligible[, `:=`(inverse_distance = 1 / distance_km,
                  active = station_id %in% active)]
  edges <- eligible[active == TRUE]
  edges[, static_weight := inverse_distance / sum(inverse_distance), by = geo_id]
  metrics <- d[, .(catalog_stations = .N,
    missing_distances = sum(!is.finite(distance_km)),
                   zero_distances = sum(distance_km == 0, na.rm = TRUE),
                   nearest_catalog_km = min(distance_km, na.rm = TRUE),
                   nearest_active_km = if (any(station_id %in% active)) {
                     min(distance_km[station_id %in% active], na.rm = TRUE)
                   } else NA_real_), by = geo_id]
  units <- split(edges, by = "geo_id", keep.by = TRUE)
  thresholds <- if (pollutant == "pm10") c(150, 100) else c(75, 50)
  summaries <- lapply(units, function(e) {
    v <- values[, e$station_id, drop = FALSE]
    observed <- is.finite(v)
    inv <- e$inverse_distance
    denom <- as.vector(observed %*% inv)
    good <- denom > 0
    v[!observed] <- 0
    y <- as.vector(v %*% inv)[good] / denom[good]
    ss <- as.vector(observed %*% (inv^2))[good] / denom[good]^2
    largest <- numeric(nrow(observed))
    for (j in seq_along(inv)) largest <- pmax(largest, observed[, j] * inv[j])
    maxw <- largest[good] / denom[good]
    data.table::data.table(
      geo_id = e$geo_id[1], eligible_stations = nrow(e),
      eligible_distance_mean = mean(e$distance_km),
      static_neff = 1 / sum(e$static_weight^2), static_max_weight = max(e$static_weight),
      static_row_sum = sum(e$static_weight), observed_hours = sum(good),
      zero_weight_hours = sum(!good), hourly_neff_mean = mean(1 / ss),
      hourly_neff_p10 = unname(stats::quantile(1 / ss, .1)),
      hourly_neff_p90 = unname(stats::quantile(1 / ss, .9)),
      hourly_max_weight_mean = mean(maxw),
      hourly_row_sum_error = if (any(good)) {
        max(abs(rowSums(sweep(sweep(observed[good, , drop = FALSE], 2, inv, `*`),
                             1, denom[good], `/`)) - 1))
      } else NA_real_,
      avg_reconstructed = if (length(y)) mean(y) else NA_real_,
      it1_reconstructed = if (length(y)) sum(y >= thresholds[1]) else NA_real_,
      it2_reconstructed = if (length(y)) sum(y >= thresholds[2]) else NA_real_,
      it1_boundary_hours = sum(abs(y - thresholds[1]) <=
        64 * .Machine$double.eps * thresholds[1]),
      it2_boundary_hours = sum(abs(y - thresholds[2]) <=
        64 * .Machine$double.eps * thresholds[2]),
      it1_lower = sum(y >= thresholds[1] * (1 + 64 * .Machine$double.eps)),
      it1_upper = sum(y >= thresholds[1] * (1 - 64 * .Machine$double.eps)),
      it2_lower = sum(y >= thresholds[2] * (1 + 64 * .Machine$double.eps)),
      it2_upper = sum(y >= thresholds[2] * (1 - 64 * .Machine$double.eps)))
  })
  dynamic <- data.table::rbindlist(summaries)
  if (nrow(dynamic)) metrics <- merge(metrics, dynamic, by = "geo_id", all.x = TRUE)
  else {
    metrics[, `:=`(eligible_stations = 0L, observed_hours = 0L,
                    zero_weight_hours = length(times))]
  }
  metrics[is.na(eligible_stations), `:=`(eligible_stations = 0L, observed_hours = 0L,
                                       zero_weight_hours = length(times))]
  manifest <- data.table::data.table(station_id = stations,
    active = stations %in% active, eligible = stations %in% edges$station_id)
  eligible <- merge(eligible, edges[, .(geo_id, station_id, static_weight)],
                    by = c("geo_id", "station_id"), all.x = TRUE)
  list(units = metrics, edges = eligible, stations = manifest)
}

#' @param geo Fine sf polygons with geo_id.
#' @param keys Unique geo_id,parent_id crosswalk.
#' @return Dissolved sf polygons with geo_id equal to parent_id.
#' @details Validates and unions in the existing local UTM grid before WGS84 points.
resolution_dissolve <- function(geo, keys) {
  if (anyDuplicated(geo$geo_id) || anyDuplicated(keys$geo_id)) {
    stop("Dissolve requires unique fine identifiers.")
  }
  x <- merge(geo["geo_id"], keys[!is.na(parent_id)], by = "geo_id")
  if (!nrow(x)) stop("No polygons have a parent identifier.")
  x <- sf::st_make_valid(sf::st_transform(x, utm_epsg(x)))
  out <- stats::aggregate(x["geo_id"], list(parent_id = x$parent_id), length)
  out$geo_id <- as.character(out$parent_id)
  out <- out["geo_id"]
  if (any(sf::st_is_empty(out)) || !all(sf::st_is_valid(out))) {
    stop("Invalid or empty dissolved geometry.")
  }
  out
}

#' @param outcome_col Outcome; level parent key; cells quintile population cells.
#' @param design Reference label; remaining arguments explicitly supply reference inputs.
#' @return Historical normalized regression table.
#' @details Preserves the original Bogota A and joint-C calculation for verification.
resolution_reference_run <- function(outcome_col, level, cells, design,
                                     exposure_manzana, manzana_pop, keys,
                                     analysis_year, city_id, buffer_km,
                                     n_groups = 5L, base_group = 5L) {

  covered <- exposure_manzana[!is.na(get(outcome_col)), .(geo_id, y = get(outcome_col))]
  covered <- merge(covered, manzana_pop, by = "geo_id")
  covered <- merge(covered, keys[, c("geo_id", level), with = FALSE], by = "geo_id")

  data.table::setnames(covered, level, ".key")

  # Population-weighted mean over the covered manzanas of each parent. This is the only step
  # that discards information: within-parent variation in y is replaced by the parent mean.
  lvl_exposure <- covered[, .(y = stats::weighted.mean(y, pop, na.rm = TRUE)), by = .key]
  data.table::setnames(lvl_exposure, c(".key", "y"), c("geo_id", outcome_col))
  lvl_exposure[, year := analysis_year]

  # Individuals are restricted to the covered manzanas first, then re-keyed, so the estimation
  # sample is the same people at every level rather than growing as units merge.
  lvl_cells <- merge(cells, covered[, .(geo_id, .key)], by = "geo_id")
  lvl_cells <- lvl_cells[, .(person_weight = sum(person_weight)),
                         by = .(geo_id = .key, edu_quintile)]

  ci <- compute_exposure_regressions(
    exposure_dt     = lvl_exposure,
    individual_dt   = lvl_cells,
    group_col       = "edu_quintile",
    group_values    = seq_len(n_groups),
    base_group      = base_group,
    pollutants      = c("pm10", "pm25"),
    outcome_pattern = paste0("^", outcome_col, "$"),
    year_filter     = analysis_year,
    conf_level      = 0.95,
    normalized      = TRUE,
    se_type         = "cluster_geo",
    quiet           = TRUE)

  if (nrow(ci) == 0L) return(NULL)

  ci[, `:=`(city_id = city_id, year = analysis_year, buffer_km = buffer_km,
            design = design, resolution_level = level,
            n_resolution_units = data.table::uniqueN(lvl_exposure$geo_id),
            pop_estimation = sum(lvl_cells$person_weight))]
  ci[]
}

#' @param manz Draw-indexed units; cells draw-indexed quintile cells; levels_dt ladder.
#' @param base_group Reference quintile, historically five.
#' @return Historical normalized means by group and resolution.
#' @details Repeated sampled units join by draw ID, never by a duplicated geographic ID.
resolution_reference_gaps <- function(manz, cells, levels_dt, base_group = 5L) {
  data.table::rbindlist(lapply(levels_dt$level, function(lv) {

    m <- manz[, .(.draw, y, pop, .key = get(lv))]

    # The parent mean is weighted over manzanas, never over quintile cells: a manzana that
    # hosts four quintiles is still one manzana.
    m[, y_level := stats::weighted.mean(y, pop), by = .key]

    d <- merge(cells, m[, .(.draw, y_level)], by = ".draw")

    g <- d[, .(mu = stats::weighted.mean(y_level, person_weight)), by = edu_quintile]
    base_mu <- g[edu_quintile == base_group, mu]

    data.table::data.table(resolution_level = lv, group = g$edu_quintile,
                           gap = g$mu / base_mu - 1)
  }))
}
