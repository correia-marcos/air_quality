# ==========================================================================================
# IDB: Air monitoring — Design A of the specification-sensitivity analysis
# ==========================================================================================
#' @Goal: Design A estimates on a supplied sample and the fixed-sample decomposition.
#' @Description: Design A keeps people, groups and weights fixed and replaces each fine
#   unit's exposure by its support's mean over covered fine units, weighted by all adults
#   (resolution_aggregate()). Group means then use education-reporting weights
#   (resolution_estimate()). These functions reproduce the A arm of
#   resolution_multicity_contrasts() for any IDW specification and any sample, so the
#   buffer x exponent grid and its fixed-sample decomposition share one rule.
#' @Summary:
#   1. design_a_sample
#   2. design_a_estimate
#   3. design_a_decomposition
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: design_a_sample
#
#' @param exposure   data.table; fine exposure with geo_id and the outcome column.
#' @param population data.table; geo_id and pop (all adults: the aggregation weights).
#' @param good_keys  character; fine units with a parent at every support.
#' @param outcome    string; outcome column, e.g. "avg_pm10".
#
#' @return  character vector of the fine geo_ids in the Design A sample, sorted.
#
#' @details
#   The rule of resolution_multicity_contrasts(): fine units with a finite exposure and a
#   parent at every support, with the positive-population filter of
#   resolution_aggregate(). Exposure is finite when at least one station within the
#   buffer reported in the year, so the sample depends on the buffer and never on the IDW
#   exponent. Avg and threshold outcomes of one pollutant share it.
# ------------------------------------------------------------------------------------------
design_a_sample <- function(exposure, population, good_keys, outcome) {
  covered <- exposure[is.finite(get(outcome)), geo_id]
  ids <- population[geo_id %chin% covered & geo_id %chin% good_keys &
                      is.finite(pop) & pop > 0, geo_id]
  sort(unique(ids))
}

# ------------------------------------------------------------------------------------------
# Function: design_a_estimate
#
#' @param exposure   data.table; fine exposure with geo_id and the outcome column.
#' @param sample_ids character; fine units kept, over which support means are taken.
#' @param population data.table; geo_id and pop.
#' @param cells      data.table; fine geo_id, the group column and person_weight.
#' @param keys       data.table; this support's geo_id and parent_id.
#' @param outcome    string; outcome column.
#' @param group_col  string; individual group column, e.g. "edu_quintile".
#' @param groups     the city's populated groups, lowest first; the gap is first minus last.
#' @param inference  logical; HC1 where the screens of resolution_estimate() pass.
#
#' @return  list: contrast (one row), profile (one row per group) and assignment (fine
#   units with y0, the fine exposure, and y, the support mean).
#
#' @details
#   Endpoint means are always low_mean and high_mean, whatever the grouping. The contrast
#   adds sd0 (the all-adult SD of fine exposure on this sample), standardized_gap
#   (gap / sd0, the existing definition), share_variance_retained and the error of the
#   between/within identity v0 = v1 + within, which must be near zero.
# ------------------------------------------------------------------------------------------
design_a_estimate <- function(exposure, sample_ids, population, cells, keys, outcome,
                              group_col = "edu_quintile", groups = 1:5,
                              inference = FALSE) {
  fine <- merge(exposure[geo_id %chin% sample_ids, .(geo_id, y = get(outcome))],
                population, by = "geo_id")
  assignment <- resolution_aggregate(fine, keys)
  estimate <- resolution_estimate(cells, assignment, inference, group_col = group_col,
                                  groups = groups)
  if ("q1" %in% names(estimate$contrast)) {
    data.table::setnames(estimate$contrast, c("q1", "q5"), c("low_mean", "high_mean"))
  }

  # Exposure variance before and after averaging over the support.
  sd0 <- sqrt(resolution_variance(fine$y, fine$pop))
  v0 <- resolution_variance(assignment$y0, assignment$pop)
  v1 <- resolution_variance(assignment$y, assignment$pop)
  within <- sum(assignment$pop * (assignment$y0 - assignment$y)^2) / sum(assignment$pop)
  estimate$contrast[, `:=`(
    standardized_gap = if (is.finite(sd0) && sd0 > 0) gap / sd0 else NA_real_,
    sd0 = sd0,
    share_variance_retained = if (is.finite(v0) && v0 > 0) v1 / v0 else NA_real_,
    variance_identity_error = v0 - v1 - within)]
  c(estimate, list(assignment = assignment))
}

# ------------------------------------------------------------------------------------------
# Function: design_a_decomposition
#
#' @param gap_own         numeric; G(s; S_s), the specification on its own sample.
#' @param gap_ref_own     numeric; G(r; S_r), the reference on its own sample.
#' @param gap_spec_common numeric; G(s; S_c), the specification on the common sample.
#' @param gap_ref_common  numeric; G(r; S_c), the reference on the common sample.
#
#' @return  data.table with total_change, exposure_change, sample_expansion,
#   sample_contraction and identity_error, one row per input element.
#
#' @details
#   G(s; S) is the Design A gap with exposure specification s (buffer, exponent)
#   aggregated over sample S; r is the reference (3 km, exponent 1) at the same city,
#   grouping, support and outcome; S_c = S_s intersected with S_r. Then
#     G(s; S_s) - G(r; S_r) = [G(s; S_c) - G(r; S_c)]   exposure construction, people fixed
#                           + [G(s; S_s) - G(s; S_c)]   people added by s's larger buffer
#                           - [G(r; S_r) - G(r; S_c)]   people the reference has and s lacks
#   Eligibility is monotone in the buffer, so samples are nested and one sample term is
#   zero. The other ordering would need exposure for units the smaller buffer does not
#   cover, which does not exist; this path is the only computable one. The identity is
#   exact; identity_error records floating-point residue only.
# ------------------------------------------------------------------------------------------
design_a_decomposition <- function(gap_own, gap_ref_own, gap_spec_common, gap_ref_common) {
  x <- data.table::data.table(
    total_change       = gap_own - gap_ref_own,
    exposure_change    = gap_spec_common - gap_ref_common,
    sample_expansion   = gap_own - gap_spec_common,
    sample_contraction = gap_ref_own - gap_ref_common)
  x[, identity_error := total_change -
      (exposure_change + sample_expansion - sample_contraction)]
  x[]
}
