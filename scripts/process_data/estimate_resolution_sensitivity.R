# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Measure how the estimated education gradient in air-pollution exposure in Bogota
# changes with the census geography used to assign residential exposure.
#
#' @Description: Bogota's 2018 CNPV identifier is a 22-character hierarchical DANE code, so
# the same adults can be represented at five nested resolutions (50,287 manzanas -> 3,723
# secciones urbanas -> 1,146 sectores urbanos -> 40 localidades/municipios -> 21 municipios).
# This script re-keys the already-computed 3 km manzana exposure to each level and re-estimates
# the paper's own normalized quintile gap, holding the census, metro definition, pollution
# year, station set, buffer, IDW parameters and education definition fixed. Two designs are
# run: A aggregates the exposure and keeps individual quintiles; C keeps manzana exposure and
# cuts quintiles on level-aggregated education. Outputs land in
# data/processed/resolution_sensitivity/ and feed figure_resolution_sensitivity.R.
#
#' @Summary:
#   I.    Setup: load dependencies, read the manzana exposure, census and crosswalk.
#   II.   Build the resolution ladder: one parent key per level, per manzana.
#   III.  Design C inputs: quintiles cut on level-aggregated education.
#   IV.   Estimate: one regression per level x design x outcome, on a fixed sample.
#   V.    Diagnostics: variance retained, coverage, and the aggregation decomposition.
#   VI.   Paired bootstrap for the change in the gap across levels.
#   VII.  Verify and save.
#
#' @Date: September 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

set.seed(20260910)

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_idw    <- here::here("data", "processed", "idw_estimates")
dir_census <- here::here("data", "interim", "census")
dir_geo    <- here::here("data", "interim", "geospatial_data", "bogota")
dir_out    <- here::here("data", "processed", "resolution_sensitivity")

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
crosswalk[, geo_id := reconcile_geo_ids(geo_id, census_collapsed$geo_id, label = "crosswalk")]

# The regressions weight by adult population, so the ladder aggregates by the same quantity.
manzana_pop <- individual[, .(pop = sum(person_weight, na.rm = TRUE)), by = geo_id]

# Individuals collapse to geo-by-quintile cells inside the estimator anyway, so carrying one
# row per cell instead of 5.7 million person rows changes no estimate. V4 proves it.
indiv_cells <- individual[
  !is.na(edu_quintile) & !is.na(person_weight) & person_weight > 0,
  .(person_weight = sum(person_weight)), by = .(geo_id, edu_quintile)]

# ============================================================================================
# II: Build the resolution ladder
# ============================================================================================
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
  message("Dropping ", length(ragged), " exposure unit(s) whose id is not 22 characters (",
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

# ============================================================================================
# III: Design C inputs -- quintiles cut on level-aggregated education
# ============================================================================================
# education_mean is a mean over the adults who reported education, so aggregating it weights by
# pop_educ_known, not pop_total. See doc/audits/census_processing/education_mean_weight_shadowing.md
census_keys <- copy(census_collapsed[, .(geo_id, pop_total, pop_educ_known, education_mean)])
census_keys <- merge(census_keys, crosswalk[, .(geo_id, loc_id)], by = "geo_id", all.x = TRUE)

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
    education_mean = stats::weighted.mean(education_mean, pop_educ_known, na.rm = TRUE)),
    by = .key]

  # assign_socio_group() breaks ties on a column named geo_id, so the parent key takes that
  # name here: at this level the parent *is* the geographic unit.
  data.table::setnames(lvl_census, ".key", "geo_id")

  # Same cut the geo-level path of run_idw_city() uses: equal-population bins of the unit mean.
  assign_socio_group(lvl_census, "education_mean", "pop_total", n_groups, "edu_quintile_geo")

  # Every adult inherits the quintile of the unit they live in.
  cells <- merge(individual[!is.na(person_weight) & person_weight > 0, .(geo_id, person_weight)],
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

message("Design C: keeping ", length(c_common), " manzana(s) classified at every level (",
        data.table::uniqueN(unlist(lapply(design_c_cells, `[[`, "geo_id"))) -
          length(c_common), " dropped for level-dependent classification).")

design_c_cells <- lapply(design_c_cells, function(x) x[geo_id %chin% c_common])

# ============================================================================================
# IV: Estimate the gaps at every level
# ============================================================================================
# The paper's regression outcomes: annual means plus IT1/IT2 exceedance hours.
outcome_cols <- grep("^(avg_(pm10|pm25)|hrs_d_(pm10|pm25)_it[12])$",
                     names(exposure_manzana), value = TRUE)

# One outcome at a time. Each outcome has its own covered manzanas (37,416 for PM10, 37,326 for
# PM2.5), and fixing that sample per outcome is what keeps the ladder comparing like with like
# while still reproducing the published per-outcome sample at the manzana rung.
#
# Re-keying geo_id to the parent is what makes the level real: the estimator then collapses to
# parent-by-quintile cells and clusters on the parent, which is the unit exposure now varies at.
resolution_run <- function(outcome_col, level, cells, design) {

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

ci_list <- list()

for (design in c("A", "C")) {
  for (lv in ladder$level) {
    cells <- if (design == "A") indiv_cells else design_c_cells[[lv]]

    for (oc in outcome_cols) {
      ci_list[[length(ci_list) + 1L]] <- resolution_run(oc, lv, cells, design)
    }
  }
  cat("Design", design, "complete.\n")
}

ci_all <- data.table::rbindlist(ci_list, fill = TRUE)

# ============================================================================================
# V: Diagnostics
# ============================================================================================
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

# ============================================================================================
# VI: Paired bootstrap for the change in the gap across levels
# ============================================================================================
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
ladder_gaps <- function(manz, cells, levels_dt) {
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

  point <- ladder_gaps(covered, base_cells, ladder)
  data.table::setnames(point, "gap", "gap_point")

  reps <- data.table::rbindlist(lapply(seq_len(n_boot), function(b) {

    # Resample manzanas, not cells or people: the manzana is the unit exposure is measured on.
    # Parent keys are left untouched so the nesting survives the draw.
    idx  <- sample(nrow(covered), nrow(covered), replace = TRUE)
    manz <- covered[idx]
    manz[, .draw := .I]

    cells <- merge(manz[, .(geo_id, .draw)], indiv_cells, by = "geo_id",
                   allow.cartesian = TRUE)[, .(.draw, edu_quintile, person_weight)]

    out <- ladder_gaps(manz, cells, ladder)
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

# ============================================================================================
# VII: Verify
# ============================================================================================
# V-boot: the weighted-mean shortcut the bootstrap uses must reproduce the estimator's own
# point estimates. If it does not, every interval above is describing a different statistic.
check_boot <- merge(
  boot_ci[, .(outcome, pollutant, resolution_level, group, gap_point)],
  ci_all[design == "A", .(outcome, pollutant, resolution_level, group, estimate)],
  by = c("outcome", "pollutant", "resolution_level", "group"))

cat("\nV-boot: ", nrow(check_boot), " coefficients compared, max |shortcut - estimator| = ",
    if (nrow(check_boot) > 0L) format(max(abs(check_boot$gap_point - check_boot$estimate)))
    else "NOT CHECKED - labels did not align", "\n", sep = "")

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
} else {
  cat("\nV4 anchor NOT RUN: published table absent at", published_path, "\n")
}

# ============================================================================================
# VIII: Save
# ============================================================================================
meta_cols <- c("city_id", "year", "buffer_km", "design", "resolution_level")
set_meta_cols_first(ci_all, meta_cols)

save_table_parquet_csv(ci_all, dir_out,
                       sprintf("resolution_ci_bogota_%dkm_%d", buffer_km, analysis_year))
save_table_parquet_csv(boot_ci, dir_out,
                       sprintf("resolution_bootstrap_bogota_%dkm_%d", buffer_km, analysis_year))
save_table_parquet_csv(variance_retained, dir_out,
                       sprintf("resolution_variance_bogota_%dkm_%d", buffer_km, analysis_year))
save_table_parquet_csv(decomposition, dir_out,
                       sprintf("resolution_decomposition_bogota_%dkm_%d", buffer_km,
                               analysis_year))
save_table_parquet_csv(ladder_check, dir_out,
                       sprintf("resolution_coverage_bogota_%dkm_%d", buffer_km, analysis_year))

cat("Script from the IDB project executed successfully in the Docker container!\n")
