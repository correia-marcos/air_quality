# ==========================================================================================
# IDB: Air monitoring — Design A specification panel and its variance decomposition
# ==========================================================================================
#' @Goal: One row per Design A specification, baselines and the exact variance shares.
#' @Description: The panel joins the Design A grid tables into one analysable table whose
#   fields are labelled by role (identifier, choice, sample, mechanism, estimate,
#   reference). The variance decomposition attributes the spread of a gap over the
#   balanced grid of choices to each choice and their interactions.
#' @Summary:
#   1. specification_panel
#   2. specification_add_baselines
#   3. specification_variance_decomposition
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: specification_panel
#
#' @param contrasts       data.table; design_a_grid_contrasts.
#' @param profiles        data.table; design_a_grid_profiles.
#' @param samples         data.table; design_a_grid_samples.
#' @param support_sizes   data.table; design_a_grid_support_sizes.
#' @param station_weights data.table; design_a_grid_station_weights.
#' @param decomposition   data.table; design_a_grid_decomposition.
#' @param ladders         data.table; city_id, level, label and fine_to_coarse.
#' @param contrast_labels named character; headline contrast of each grouping.
#' @param n_groups        named integer; defined number of groups of each grouping.
#
#' @return  data.table with one row per city, outcome, grouping, buffer, exponent and
#   support: identifiers, choices, sample and mechanism descriptors, estimates and the
#   fixed-sample decomposition against (3 km, exponent 1).
#
#' @details
#   Every input row is a Design A estimate on its own sample, so the panel is a complete
#   factorial within each city. Sample and mechanism descriptors depend on the pollutant
#   (avg and threshold outcomes of one pollutant share a sample). The endpoint shares come
#   from the profiles: the population shares of the first and last group in the sample.
# ------------------------------------------------------------------------------------------
specification_panel <- function(contrasts, profiles, samples, support_sizes,
                                station_weights, decomposition, ladders,
                                contrast_labels, n_groups) {
  keys <- c("city_id", "grouping", "spec", "level", "outcome")
  x <- copy(contrasts)[, !c("variance_identity_error", "sample_matches_assignment")]
  x[, pollutant := sub("^.*_(pm10|pm25).*$", "\\1", outcome)]
  x[, `:=`(
    census_year   = as.integer(sub("^.*_", "", city_id)),
    metric        = data.table::fcase(grepl("^avg", outcome), "annual_mean",
                                      grepl("it1$", outcome), "hours_it1",
                                      grepl("it2$", outcome), "hours_it2"),
    outcome_unit  = data.table::fifelse(grepl("^avg", outcome), "concentration",
                                        "hours per year"),
    contrast_label = contrast_labels[grouping],
    n_groups      = n_groups[grouping],
    sample_key    = paste(city_id, pollutant, paste0(buffer_km, "km"), sep = "|"))]

  # Endpoint-group shares of each sample, from the profiles
  ends <- profiles[, .(low_group_share = share[group == min(group)],
                       high_group_share = share[group == max(group)]), by = keys]
  x <- merge(x, ends, by = keys)

  # Supports: label, rank (1 = finest) and branch
  x <- merge(x, ladders[, .(city_id, level, level_label = label,
                            level_rank = fine_to_coarse)], by = c("city_id", "level"))
  x[, branch := data.table::fifelse(city_id == "bogota_2018" & level == "localidad",
                                    "localidad", "main")]

  # Sample, support-size and station-weight descriptors of the pollutant's sample
  by_spec <- c("city_id", "buffer_km", "distance_power", "pollutant")
  x <- merge(x, samples[, .(city_id, buffer_km, distance_power, pollutant,
    sample_population, sample_fine_units, sample_share_city, overlap_with_reference,
    share_in_reference)], by = by_spec)
  x <- merge(x, support_sizes[, .(city_id, spec, pollutant, level, n_level_units,
    pw_unit_area_km2, median_unit_area_km2, pw_unit_population)],
    by = c("city_id", "spec", "pollutant", "level"))
  x <- merge(x, station_weights, by = by_spec)

  # Fixed-sample decomposition against the reference (3 km, exponent 1)
  x <- merge(x, decomposition[, .(city_id, grouping, spec, level, outcome,
    gap_ref_exposure = gap_ref_own, delta_exposure_total = total_change,
    delta_exposure_fixed_sample = exposure_change,
    delta_exposure_composition = sample_expansion - sample_contraction,
    common_population)], by = keys)

  x[, spec_id := paste(city_id, outcome, grouping, paste0(buffer_km, "km"),
                       paste0("p", distance_power), level, sep = "|")]
  setcolorder(x, c("spec_id", "city_id", "census_year", "outcome", "pollutant", "metric",
                   "outcome_unit", "grouping", "n_groups", "contrast_label", "buffer_km",
                   "distance_power", "level", "level_label", "level_rank", "branch"))
  setorder(x, city_id, outcome, grouping, buffer_km, distance_power, level_rank)
  x[]
}

# ------------------------------------------------------------------------------------------
# Function: specification_add_baselines
#
#' @param panel data.table; the output of specification_panel().
#
#' @return  the panel with the paper-baseline and finest-support references added.
#
#' @details
#   The paper baseline of a city and outcome is quintiles, the finest support, 3 km and
#   exponent 1: the manuscript estimate. delta_vs_paper is the total departure from it,
#   and n_choices_vs_paper counts the choices that differ (grouping, buffer, exponent,
#   support), so rows with one change give the one-at-a-time view. gap_paper_sd divides
#   every gap by the paper baseline's all-adult SD of fine exposure: one fixed scale per
#   city and outcome. Groupings change the estimand, so their deltas compare different
#   contrasts. The finest-support reference holds the people fixed (Design A).
# ------------------------------------------------------------------------------------------
specification_add_baselines <- function(panel) {
  x <- copy(panel)
  paper <- x[grouping == "edu_quintile" & level_rank == 1 & buffer_km == 3 &
               distance_power == 1, .(city_id, outcome, gap_paper = gap, sd_paper = sd0)]
  stopifnot(!anyDuplicated(paper[, .(city_id, outcome)]))
  x <- merge(x, paper, by = c("city_id", "outcome"))
  x[, `:=`(
    delta_vs_paper       = gap - gap_paper,
    n_choices_vs_paper   = (grouping != "edu_quintile") + (buffer_km != 3) +
                           (distance_power != 1) + (level_rank != 1),
    sign_change_vs_paper = sign(gap) != sign(gap_paper),
    gap_paper_sd         = gap / sd_paper)]
  fine <- x[level_rank == 1, .(city_id, outcome, grouping, buffer_km, distance_power,
                               gap_ref_fine = gap)]
  x <- merge(x, fine, by = c("city_id", "outcome", "grouping", "buffer_km",
                             "distance_power"))
  x[, delta_resolution := gap - gap_ref_fine]
  setorder(x, city_id, outcome, grouping, buffer_km, distance_power, level_rank)
  x[]
}

# ------------------------------------------------------------------------------------------
# Function: specification_variance_decomposition
#
#' @param x        data.table; one city and outcome, one row per grid point.
#' @param factors  character; choice columns (each treated as a factor).
#' @param response string; the column decomposed, e.g. "gap".
#
#' @return  list: terms (one row per main effect and interaction: sum of squares and share
#   of the total) and indices (per factor: first-order and total share), plus balanced
#   and the residual of the decomposition.
#
#' @details
#   On a complete grid with one row per combination of choices, the saturated factorial
#   ANOVA splits the total sum of squares exactly into orthogonal main-effect and
#   interaction terms: the functional-ANOVA (Sobol) decomposition with each chosen value
#   weighted equally. The first-order share of a choice is its main-effect share; its
#   total share adds every interaction that contains it. The shares describe the chosen
#   grid; denser values of a choice weight that region more. There is no sampling model:
#   rows are deterministic transformations of the same data, so no test or interval.
#   An incomplete grid is refused, because sequential sums of squares then depend on
#   the order of the terms.
# ------------------------------------------------------------------------------------------
specification_variance_decomposition <- function(x, factors, response = "gap") {
  x <- copy(x)
  for (f in factors) set(x, j = f, value = factor(x[[f]]))
  cells <- x[, .N, by = factors]
  balanced <- nrow(cells) == prod(vapply(factors, function(f) nlevels(x[[f]]), 1L)) &&
    all(cells$N == 1L) && all(is.finite(x[[response]]))
  if (!balanced) stop("The grid is incomplete; the decomposition needs every cell once.")

  fit <- stats::aov(stats::reformulate(paste(factors, collapse = " * "), response),
                    data = x)
  table <- summary(fit)[[1]]
  terms <- data.table(term = trimws(rownames(table)), sum_sq = table[["Sum Sq"]])
  terms <- terms[term != "Residuals"]
  total <- sum((x[[response]] - mean(x[[response]]))^2)
  terms[, `:=`(share = sum_sq / total, order = lengths(strsplit(term, ":", fixed = TRUE)))]

  indices <- data.table(factor = factors,
    first_order = vapply(factors, function(f) terms[term == f, share], numeric(1)),
    total = vapply(factors, function(f) {
      terms[vapply(strsplit(term, ":", fixed = TRUE), function(t) f %in% t, logical(1)),
            sum(share)]
    }, numeric(1)))
  list(terms = terms, indices = indices, balanced = balanced,
       residual = total - sum(terms$sum_sq))
}
