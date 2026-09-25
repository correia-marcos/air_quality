# ==========================================================================================
# IDB: Air monitoring — tables for the review of the resolution-sensitivity analysis
# ==========================================================================================
#' @Goal: Summaries that explain the resolution results to coauthors.
#' @Description: Pure transformations of the frozen resolution inputs and saved outputs.
#   Monitoring summaries describe distances to stations and never enter exposure
#   estimation; exposure samples are reported as they are, never forced to match.
#' @Summary:
#   1. resolution_weighted_quantile
#   2. resolution_level_weights
#   3. resolution_unit_composition
#   4. resolution_sample_shares
#   5. resolution_monitoring_units
#   6. resolution_monitoring_summary
#   7. resolution_bogota_class_breakdown
#   8. resolution_buffer_comparison
#' @Date: September 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: resolution_weighted_quantile
#
#' @param x     numeric values.
#' @param w     nonnegative weights.
#' @param probs probabilities.
#
#' @return  numeric vector of weighted quantiles, NA for empty support.
#
#' @details
#   Smallest value whose cumulative weight share reaches each probability, the
#   convention of .exposure_weighted_median() in exposure_regressions.R.
# ------------------------------------------------------------------------------------------
resolution_weighted_quantile <- function(x, w, probs) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(rep(NA_real_, length(probs)))
  x <- x[keep]; w <- w[keep]
  ord <- order(x)
  share <- cumsum(w[ord]) / sum(w)
  vapply(probs, function(p) x[ord][which(share >= p - 1e-12)[1L]], numeric(1))
}

# ------------------------------------------------------------------------------------------
# Function: resolution_level_weights
#
#' @param population Fine geo_id and all-adult pop.
#' @param cells      Fine geo_id, edu_quintile and person_weight (education-reporting).
#' @param keys       Long crosswalk geo_id, level, parent_id.
#' @param level      Level to aggregate to.
#
#' @return  data.table geo_id (parent unit), group ("All adults", "Q1".."Q5"), weight.
#
#' @details
#   "All adults" uses every adult aged 25+; quintile groups use education-reporting
#   adults with their frozen individual quintile. Fine units without a parent drop out.
# ------------------------------------------------------------------------------------------
resolution_level_weights <- function(population, cells, keys, level) {
  target <- level
  k <- keys[keys$level == target & !is.na(parent_id), .(geo_id, parent_id)]
  all_adults <- merge(population, k, by = "geo_id")[
    , .(group = "All adults", weight = sum(pop)), by = .(geo_id = parent_id)]
  quintiles <- merge(cells, k, by = "geo_id")[
    , .(weight = sum(person_weight)), by = .(geo_id = parent_id, edu_quintile)]
  quintiles[, group := paste0("Q", edu_quintile)]
  rbind(all_adults, quintiles[, .(geo_id, group, weight)])
}

# ------------------------------------------------------------------------------------------
# Function: resolution_unit_composition
#
#' @param cells         Fine geo_id, edu_quintile, person_weight.
#' @param keys          Long crosswalk geo_id, level, parent_id.
#' @param level         Parent level, e.g. "comuna".
#' @param census        Fine geo_id, pop_educ_known, education_mean.
#' @param a_fine_ids    Fine units in the Design A fixed sample.
#' @param b_parent_ids  Parent units with native B exposure.
#
#' @return  data.table, one row per parent unit and individual quintile: population,
#   share of the unit's education-reporting adults, unit adults, schooling mean, the
#   share of the unit's adults inside the A sample, and whether B covers the unit.
#
#' @details
#   Shows how city-wide individual quintiles are distributed inside each parent
#   unit and which units each design reaches. Schooling means weight fine-unit means
#   by education-reporting adults, as resolution_classify() does.
# ------------------------------------------------------------------------------------------
resolution_unit_composition <- function(cells, keys, level, census, a_fine_ids,
                                        b_parent_ids) {
  target <- level
  k <- keys[keys$level == target & !is.na(parent_id), .(geo_id, parent_id)]
  x <- merge(cells, k, by = "geo_id")
  x[, in_a := geo_id %in% a_fine_ids]
  groups <- x[, .(population = sum(person_weight)), by = .(parent_id, edu_quintile)]
  units <- x[, .(unit_population = sum(person_weight),
                 a_share = sum(person_weight[in_a]) / sum(person_weight)),
             by = parent_id]
  schooling <- merge(census[, .(geo_id, pop_educ_known, education_mean)], k,
                     by = "geo_id")[is.finite(education_mean) & pop_educ_known > 0,
    .(education_mean = stats::weighted.mean(education_mean, pop_educ_known)),
    by = parent_id]
  full <- data.table::CJ(parent_id = units$parent_id, edu_quintile = 1:5)
  out <- merge(full, groups, by = c("parent_id", "edu_quintile"), all.x = TRUE)
  out[is.na(population), population := 0]
  out <- merge(out, units, by = "parent_id")
  out <- merge(out, schooling, by = "parent_id", all.x = TRUE)
  out[, `:=`(share = population / unit_population, level = target,
             b_covered = parent_id %in% b_parent_ids)]
  out[]
}

# ------------------------------------------------------------------------------------------
# Function: resolution_sample_shares
#
#' @param profiles Saved profiles (design, level, outcome, edu_quintile, population).
#' @param cells    Education-reporting quintile cells of the whole city.
#' @param outcome  Outcome whose estimation samples are described.
#
#' @return  data.table with one row per sample: population and Q1..Q5 shares.
#
#' @details
#   For design C the group is the area quintile, not the individual quintile. The
#   first row is every education-reporting adult, before any exposure selection.
# ------------------------------------------------------------------------------------------
resolution_sample_shares <- function(profiles, cells, outcome) {
  target <- outcome
  city <- cells[, .(design = "All adults", level = "all",
                    population = sum(person_weight)), by = edu_quintile]
  x <- rbind(city, profiles[profiles$outcome == target,
    .(design, level, population, edu_quintile)])
  x[, total := sum(population), by = .(design, level)]
  wide <- data.table::dcast(x, design + level + total ~ paste0("Q", edu_quintile),
                            value.var = "population")
  for (q in paste0("Q", 1:5)) data.table::set(wide, j = q, value = wide[[q]] / wide$total)
  data.table::setnames(wide, "total", "population")
  wide[]
}

# ------------------------------------------------------------------------------------------
# Function: resolution_monitoring_units
#
#' @param distances  Long geo_id, station_id, distance_km matrix for one support.
#' @param active_ids Stations with at least one observed 2023 reading of the pollutant.
#' @param radii_km   Radii for station counts.
#' @param buffer_km  IDW eligibility distance used for the exposure-coverage flag.
#
#' @return  data.table per unit: nearest active-station distance, active stations with
#   distance <= each radius (n_within_<r>km), zero-distance pairs and covered_idw.
#
#' @details
#   Counts follow the manuscript monitoring figure (distance <= radius, zero included,
#   build_station_distance_trend_data()). covered_idw instead applies the IDW rule
#   0 < distance <= buffer_km, so it marks units with an exposure estimate.
# ------------------------------------------------------------------------------------------
resolution_monitoring_units <- function(distances, active_ids, radii_km, buffer_km = 3) {
  d <- distances[station_id %in% active_ids & is.finite(distance_km)]
  out <- d[, .(nearest_km = min(distance_km),
               zero_distance_pairs = sum(distance_km == 0),
               covered_idw = any(distance_km > 0 & distance_km <= buffer_km)),
           by = geo_id]
  for (r in radii_km) {
    counts <- d[, .(n = sum(distance_km <= r)), by = geo_id]
    out[counts, on = "geo_id", (paste0("n_within_", r, "km")) := i.n]
  }
  out[]
}

# ------------------------------------------------------------------------------------------
# Function: resolution_monitoring_summary
#
#' @param units    Output of resolution_monitoring_units(); may add observed_hours.
#' @param weights  Output of resolution_level_weights().
#' @param radii_km Radii present in units.
#' @param hours    Hours in the year, for the observed-hour share.
#
#' @return  data.table per group: population, units, weighted p10/median/p90 nearest
#   distance, mean station count within 3 km (units must carry n_within_3km), the
#   weighted share with at least one station within each radius, the IDW-covered
#   population share and, when units carry observed_hours, the mean share of hours
#   with an observed eligible station among covered people.
#
#' @details
#   Population-weighted, so levels with very different unit counts stay comparable;
#   the manuscript figure instead ranks unweighted units.
# ------------------------------------------------------------------------------------------
resolution_monitoring_summary <- function(units, weights, radii_km, hours = 8760) {
  x <- merge(weights[weight > 0], units, by = "geo_id")
  has_hours <- "observed_hours" %in% names(x)
  out <- x[, .(
    population = sum(weight), units = data.table::uniqueN(geo_id),
    nearest_p10_km = resolution_weighted_quantile(nearest_km, weight, .1),
    nearest_median_km = resolution_weighted_quantile(nearest_km, weight, .5),
    nearest_p90_km = resolution_weighted_quantile(nearest_km, weight, .9),
    mean_stations_3km = stats::weighted.mean(n_within_3km, weight),
    idw_covered_share = sum(weight[covered_idw]) / sum(weight),
    observed_hour_share = if (has_hours && any(covered_idw)) {
      stats::weighted.mean(observed_hours[covered_idw] / hours, weight[covered_idw])
    } else NA_real_), by = group]
  # Share of each group's adults with at least one active station within each radius.
  for (r in radii_km) {
    n_col <- paste0("n_within_", r, "km")
    shares <- x[, .(value = sum(weight[get(n_col) > 0]) / sum(weight)), by = group]
    out[shares, on = "group", (paste0("share_within_", r, "km")) := i.value]
  }
  out[]
}

# ------------------------------------------------------------------------------------------
# Function: resolution_bogota_class_breakdown
#
#' @param keys       Bogota long crosswalk.
#' @param population Fine geo_id and all-adult pop.
#
#' @return  data.table per DANE class (position 6): populated fine units, distinct
#   seccion and sector parents, and adults.
#
#' @details
#   Class 1 is cabecera (urban blocks), 2 centro poblado and 3 rural disperso. Only
#   classes 1 and 2 aggregate at the 20- and 18-character prefixes.
# ------------------------------------------------------------------------------------------
resolution_bogota_class_breakdown <- function(keys, population) {
  w <- data.table::dcast(keys[level %in% c("fine", "seccion", "sector")],
                         geo_id ~ level, value.var = "parent_id")
  w <- merge(w, population[pop > 0], by = "geo_id")
  w[, .(fine_units = .N, seccion_units = data.table::uniqueN(seccion),
        sector_units = data.table::uniqueN(sector), adults = sum(pop)),
    by = .(dane_class = substr(geo_id, 6, 6))][order(dane_class)]
}

# ------------------------------------------------------------------------------------------
# Function: resolution_buffer_comparison
#
#' @param base        Saved contrasts of the baseline buffer.
#' @param alternative Saved contrasts of the alternative buffer.
#' @param labels      Suffixes for the two buffers, e.g. c("3km", "20km").
#
#' @return  data.table per city, design, level and outcome with gap, normalized gap,
#   population, units and clusters under both buffers, and the gap change.
# ------------------------------------------------------------------------------------------
resolution_buffer_comparison <- function(base, alternative, labels = c("3km", "20km")) {
  cols <- c("gap", "normalized_pct", "q1", "q5", "population", "n_units", "n_clusters")
  keys <- c("city_id", "design", "level", "outcome")
  x <- merge(base[, c(keys, cols), with = FALSE], alternative[, c(keys, cols),
             with = FALSE], by = keys, all = TRUE, suffixes = paste0("_", labels))
  x[, gap_change := get(paste0("gap_", labels[2])) - get(paste0("gap_", labels[1]))]
  x[]
}
