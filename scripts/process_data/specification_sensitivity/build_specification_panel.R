# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Build the Design A specification panel: one row per specification estimate.
#
#' @Description: Joins the Design A grid tables (estimate_design_a_grid.R) into one table
# for Bogotá 2018, Santiago 2017 and São Paulo 2010: each row is the lowest-minus-highest
# education gap for one outcome, grouping, IDW buffer, IDW exponent and geographic
# support, with sample and mechanism descriptors, the fixed-sample decomposition and
# changes from the paper baseline. Checks the grid against the A/B/C robustness run at
# 3 and 20 km and against the manuscript regression. Writes the panel, its group
# profiles and checks to data/processed/resolution_sensitivity/specification_panel/
# and results/tables/specification_sensitivity/panel/.
#
#' @Summary:
#   I.   Import data: functions, the Design A grid tables and the anchor estimates.
#   II.  Process data: build the panel, add baselines, compare with the anchors.
#   III. Save outputs: panel, profiles and checks.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and settings
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

# Settings: cities, headline contrast and defined group count of each grouping
cities          <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
contrast_labels <- c(edu_quintile       = "Q1 - Q5",
                     edu_quintile_split = "Q1 - Q5 (split ties)",
                     edu_level          = "None - Graduate",
                     edu_group3         = "Below secondary - Bachelor's+")
n_groups        <- c(edu_quintile = 5L, edu_quintile_split = 5L, edu_level = 6L,
                     edu_group3 = 3L)

# Set the folders
dir_inputs <- here::here("data", "interim", "resolution_sensitivity")
dir_grid   <- here::here("data", "processed", "resolution_sensitivity", "design_a_grid")
dir_out    <- here::here("data", "processed", "resolution_sensitivity",
                         "specification_panel")
dir_tables <- here::here("results", "tables", "specification_sensitivity", "panel")

# Read the Design A grid tables
grid <- lapply(setNames(nm = c("contrasts", "profiles", "samples", "support_sizes",
                               "station_weights", "decomposition")), function(name) {
  as.data.table(arrow::read_parquet(file.path(dir_grid,
                                              paste0("design_a_grid_", name, ".parquet"))))
})
ladders <- rbindlist(lapply(cities, function(city) {
  fread(file.path(dir_inputs, city, "levels.csv"))[, city_id := city]
}))
input_manifest_md5 <- vapply(setNames(nm = cities), function(city) {
  unname(tools::md5sum(file.path(dir_inputs, city, "input_manifest.csv")))
}, character(1))

# Read the Design A rows of the robustness run (exponent 1, 3 and 20 km, all groupings)
robustness_a <- rbindlist(lapply(c("edu_quintile", "edu_quintile_split", "edu_level",
                                   "edu_group3"), function(g) {
  rbindlist(lapply(c(3, 20), function(b) rbindlist(lapply(cities, function(city) {
    x <- fread(file.path(resolution_buffer_root(b, g), city, "contrasts.csv"))
    x <- x[design == "A"]
    if ("q1" %in% names(x)) setnames(x, c("q1", "q5"), c("low_mean", "high_mean"))
    x[, .(city_id = city, grouping = g, buffer_km = b, level, outcome, gap,
          normalized_pct, low_mean, high_mean, population, n_clusters)]
  }))))
}))

# Read the manuscript regression: normalized lowest-group estimates at 3 km, all outcomes
manuscript <- fread(here::here("data", "processed", "idw_regressions",
                               "exposure_ci_estimates_education_3km_2023.csv"))

# ========================================================================================
# II: Process data
# ========================================================================================
# 1. One row per specification: estimates, descriptors and the decomposition
specification_panel_dt <- specification_panel(contrasts       = grid$contrasts,
                                              profiles        = grid$profiles,
                                              samples         = grid$samples,
                                              support_sizes   = grid$support_sizes,
                                              station_weights = grid$station_weights,
                                              decomposition   = grid$decomposition,
                                              ladders         = ladders,
                                              contrast_labels = contrast_labels,
                                              n_groups        = n_groups)

# 2. References: the paper baseline and the finest support
specification_panel_dt <- specification_add_baselines(specification_panel_dt)
specification_panel_dt[, input_manifest_md5 := input_manifest_md5[city_id]]

# Group profiles keyed by the same specification identifier
specification_profiles <- merge(
  specification_panel_dt[, .(spec_id, city_id, grouping, spec, level, outcome)],
  grid$profiles, by = c("city_id", "grouping", "spec", "level", "outcome"))

# 3. Parity: the grid's exponent-1 rows equal Design A of the robustness run
parity <- merge(robustness_a,
  specification_panel_dt[distance_power == 1 & buffer_km %in% c(3, 20),
    .(city_id, grouping, buffer_km, level, outcome, gap_grid = gap,
      normalized_grid = normalized_pct, low_grid = low_mean, high_grid = high_mean,
      population_grid = sample_population, clusters_grid = n_clusters)],
  by = c("city_id", "grouping", "buffer_km", "level", "outcome"), all = TRUE)
parity[, `:=`(
  max_error = pmax(abs(gap - gap_grid), abs(low_mean - low_grid),
                   abs(high_mean - high_grid), na.rm = TRUE),
  same_people = abs(population - population_grid) <= 1e-10 * population &
    n_clusters == clusters_grid)]
parity[, passed := same_people & max_error <= 1e-10 + 1e-8 * abs(gap)]

# 4. Regression anchor: the paper baseline equals the manuscript's group-1 estimate
anchor <- manuscript[group == 1, .(city_id,
  outcome = fifelse(outcome == "avg", paste0("avg_", pollutant),
                    paste0("hrs_d_", pollutant, "_", sub("^hrs_d_", "", outcome))),
  manuscript_normalized = estimate, manuscript_clusters = n_clusters)]
paper_rows <- specification_panel_dt[n_choices_vs_paper == 0,
  .(city_id, outcome, panel_normalized = normalized_pct / 100,
    panel_clusters = n_clusters)]
anchor <- merge(anchor, paper_rows, by = c("city_id", "outcome"))
anchor[, `:=`(max_error = abs(manuscript_normalized - panel_normalized),
              same_people = manuscript_clusters == panel_clusters)]
anchor[, passed := same_people & max_error <= 1e-10 + 1e-8 * abs(manuscript_normalized)]

# 5. Panel checks: keys, references and the cross-run anchors
panel_checks <- data.table(
  check = c("unique_spec_id", "rows", "one_paper_baseline_per_city_outcome",
            "zero_delta_at_paper_baseline", "zero_delta_at_fine_reference",
            "decomposition_sums_to_total", "parity_with_robustness_design_a",
            "manuscript_regression_anchor", "missing_gaps"),
  value = c(!anyDuplicated(specification_panel_dt$spec_id),
    nrow(specification_panel_dt),
    specification_panel_dt[n_choices_vs_paper == 0, .N == uniqueN(paste(city_id, outcome))],
    specification_panel_dt[n_choices_vs_paper == 0, all(delta_vs_paper == 0)],
    specification_panel_dt[level_rank == 1, all(delta_resolution == 0)],
    specification_panel_dt[, max(abs(delta_exposure_total - delta_exposure_fixed_sample -
                                     delta_exposure_composition), na.rm = TRUE) < 1e-10],
    all(parity$passed),
    all(anchor$passed),
    sum(!is.finite(specification_panel_dt$gap))),
  detail = c("spec_id is unique", "rows in the panel", "quintiles, finest, 3 km, p = 1",
             "delta_vs_paper", "delta_resolution", "max |total - exposure - composition|",
             paste(sum(!parity$passed, na.rm = TRUE), "of", nrow(parity), "rows differ"),
             paste(sum(!anchor$passed), "of", nrow(anchor), "outcomes differ"),
             "gaps not defined (an empty endpoint group)"))
print(panel_checks)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the panel, its profiles, the anchor comparisons and the checks
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
outputs <- list(specification_panel = specification_panel_dt,
                specification_profiles = specification_profiles,
                parity_with_robustness = parity, manuscript_anchor = anchor,
                panel_checks = panel_checks)
for (name in names(outputs)) {
  resolution_write(outputs[[name]], file.path(dir_out, name))
  fwrite(outputs[[name]], file.path(dir_tables, paste0(name, ".csv")))
}
