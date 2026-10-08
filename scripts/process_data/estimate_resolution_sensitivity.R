# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Estimate geographic-resolution sensitivity outside the manuscript pipeline.
#
#' @Description: Default reference mode preserves the historical Bogota A/joint-C
# calculation and 500-draw bootstrap. --scope=multicity runs exposure aggregation A,
# reconstructed IDW B, and classification-only C for Bogota, Santiago, and Sao Paulo.
# --buffer-km=20 repeats multicity at another IDW distance in its own output folder;
# --city=<id> runs one city, and the combined tables then cover every completed city.
# --grouping= replaces the frozen quintiles (multicity only): edu_group3 (three
# attainment groups), edu_level (six harmonized bands) or edu_quintile_split (quintiles
# with tied schooling split proportionally). These reuse the quintile run's B exposures.
# --duckdb-mem-gb=<n> raises the DuckDB memory limit of the B rebuild (default 4).
# Definitions, populations, inference, outputs, and limitations are documented in
# doc/RESOLUTION_SENSITIVITY.md.
#
#' @Summary:
#   I.   Import settings; select the preserved reference or multicity definition.
#   II.  Estimate named operations; inspect reference objects or per-city results.
#   III. Check the saved products and comparison diagnostics.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

# ============================================================================================
# I: Import data
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

resolution_args <- commandArgs(trailingOnly = TRUE)
resolution_scope <- sub("^--scope=", "", grep("^--scope=", resolution_args, value = TRUE))
if (!length(resolution_scope)) resolution_scope <- "reference"
if (length(resolution_scope) != 1L ||
    !resolution_scope %in% c("reference", "multicity")) stop("Invalid --scope.")
all_cities <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
if (any(!grepl(paste0("^--scope=|^--output-dir=|^--reuse-b$|^--buffer-km=[0-9]+$|",
                      "^--city=|^--duckdb-mem-gb=[0-9]+$|",
                      "^--grouping=(edu_quintile|edu_quintile_split|edu_level|",
                      "edu_group3)$"), resolution_args))) {
  stop("Supported options: --scope=reference|multicity, --output-dir=, --reuse-b, ",
       "--buffer-km=<km>, --city=<id>, --duckdb-mem-gb=<n>, ",
       "--grouping=edu_quintile|edu_quintile_split|edu_level|edu_group3 (multicity only)")
}
mem_gb <- as.numeric(sub("^--duckdb-mem-gb=", "",
                         grep("^--duckdb-mem-gb=", resolution_args, value = TRUE)))
if (!length(mem_gb)) mem_gb <- 4
cities <- sub("^--city=", "", grep("^--city=", resolution_args, value = TRUE))
if (!length(cities)) cities <- all_cities
if (!all(cities %in% all_cities)) stop("Unknown --city.")
buffer_km <- as.numeric(sub("^--buffer-km=", "",
                            grep("^--buffer-km=", resolution_args, value = TRUE)))
if (!length(buffer_km)) buffer_km <- 3

# Individual education groups: frozen quintiles, or one of the alternative groupings.
grouping <- sub("^--grouping=", "", grep("^--grouping=", resolution_args, value = TRUE))
if (!length(grouping)) grouping <- "edu_quintile"
if ((buffer_km != 3 || !setequal(cities, all_cities) || grouping != "edu_quintile") &&
    resolution_scope != "multicity") {
  stop("--buffer-km, --city and --grouping need --scope=multicity.")
}
set.seed(20260910)

# ============================================================================================
# II: Estimate and inspect
# ============================================================================================
if (resolution_scope == "reference") {
  inputs_result <- resolution_reference_inputs(
    resolution_args = resolution_args)
  dir_out <- inputs_result$dir_out
  reference_dir <- inputs_result$reference_dir
  city_id <- inputs_result$city_id
  analysis_year <- inputs_result$analysis_year
  buffer_km <- inputs_result$buffer_km
  n_groups <- inputs_result$n_groups
  base_group <- inputs_result$base_group
  n_boot <- inputs_result$n_boot
  exposure_manzana <- inputs_result$exposure_manzana
  individual <- inputs_result$individual
  census_collapsed <- inputs_result$census_collapsed
  crosswalk <- inputs_result$crosswalk
  manzana_pop <- inputs_result$manzana_pop
  indiv_cells <- inputs_result$indiv_cells

  ladder_result <- resolution_reference_ladder(
    exposure_manzana = exposure_manzana,
    crosswalk = crosswalk,
    manzana_pop = manzana_pop)
  ladder <- ladder_result$ladder
  keys <- ladder_result$keys
  lv <- ladder_result$lv
  w <- ladder_result$w
  exposure_manzana <- ladder_result$exposure_manzana

  classification_result <- resolution_reference_classification(
    n_groups = n_groups,
    individual = individual,
    census_collapsed = census_collapsed,
    crosswalk = crosswalk,
    ladder = ladder,
    lv = lv,
    w = w)
  design_c_cells <- classification_result$design_c_cells
  lv <- classification_result$lv
  w <- classification_result$w
  cells <- classification_result$cells

  estimates_result <- resolution_reference_estimates(
    city_id = city_id,
    analysis_year = analysis_year,
    buffer_km = buffer_km,
    n_groups = n_groups,
    base_group = base_group,
    exposure_manzana = exposure_manzana,
    manzana_pop = manzana_pop,
    indiv_cells = indiv_cells,
    ladder = ladder,
    keys = keys,
    lv = lv,
    design_c_cells = design_c_cells,
    cells = cells)
  outcome_cols <- estimates_result$outcome_cols
  cells <- estimates_result$cells
  ci_all <- estimates_result$ci_all

  diagnostics_result <- resolution_reference_diagnostics(
    exposure_manzana = exposure_manzana,
    manzana_pop = manzana_pop,
    indiv_cells = indiv_cells,
    ladder = ladder,
    keys = keys,
    lv = lv,
    w = w,
    cells = cells,
    outcome_cols = outcome_cols)
  variance_retained <- diagnostics_result$variance_retained
  decomposition <- diagnostics_result$decomposition

  bootstrap_result <- resolution_reference_bootstrap(
    n_boot = n_boot,
    exposure_manzana = exposure_manzana,
    manzana_pop = manzana_pop,
    indiv_cells = indiv_cells,
    ladder = ladder,
    keys = keys,
    cells = cells,
    outcome_cols = outcome_cols)
  boot_ci <- bootstrap_result$boot_ci

  verification_result <- resolution_reference_verification(
    city_id = city_id,
    analysis_year = analysis_year,
    buffer_km = buffer_km,
    census_collapsed = census_collapsed,
    crosswalk = crosswalk,
    ladder = ladder,
    lv = lv,
    w = w,
    ci_all = ci_all,
    boot_ci = boot_ci)
  ladder_check <- verification_result$ladder_check

  save_result <- resolution_reference_save(
    dir_out = dir_out,
    reference_dir = reference_dir,
    analysis_year = analysis_year,
    buffer_km = buffer_km,
    ci_all = ci_all,
    variance_retained = variance_retained,
    decomposition = decomposition,
    boot_ci = boot_ci,
    ladder_check = ladder_check)

} else {
  reuse_b <- "--reuse-b" %in% resolution_args
  all_tables <- list()
  results_by_city <- list()
  for (city in cities) {
    inputs_result <- resolution_multicity_inputs(
      city = city,
      buffer_km = buffer_km,
      grouping = grouping,
      education_bands = education_level_bands,
      education_group_map = education_group3_of_level)
    inputs <- inputs_result$inputs
    out <- inputs_result$out
    paths <- inputs_result$paths
    keys <- inputs_result$keys
    population <- inputs_result$population
    cells <- inputs_result$cells
    group_col <- inputs_result$group_col
    groups <- inputs_result$groups
    ladder <- inputs_result$ladder
    exposure <- inputs_result$exposure
    outcomes <- inputs_result$outcomes
    panel <- inputs_result$panel
    source_dir <- inputs_result$source_dir
    checks <- inputs_result$checks
    profiles <- inputs_result$profiles
    contrasts <- inputs_result$contrasts
    variances <- inputs_result$variances
    composition <- inputs_result$composition
    differences <- inputs_result$differences
    bootstraps <- inputs_result$bootstraps
    transitions <- inputs_result$transitions
    matrices <- inputs_result$matrices
    reconciliations <- inputs_result$reconciliations
    movements <- inputs_result$movements
    exposure_changes <- inputs_result$exposure_changes
    good_keys <- inputs_result$good_keys
    c_map <- inputs_result$c_map
    c_common <- inputs_result$c_common
    matrix_root <- inputs_result$matrix_root
    exposure_root <- inputs_result$exposure_root

    for (lv in ladder$level) {
      rebuild_b_result <- resolution_multicity_rebuild_b(
        city = city,
        lv = lv,
        reuse_b = reuse_b,
        inputs = inputs,
        out = out,
        paths = paths,
        keys = keys,
        cells = cells,
        exposure = exposure,
        outcomes = outcomes,
        source_dir = source_dir,
        checks = checks,
        matrix_root = matrix_root,
        buffer_km = buffer_km,
        mem_gb = mem_gb,
        grouping = grouping,
        exposure_root = exposure_root)
      k <- rebuild_b_result$k
      bdir <- rebuild_b_result$bdir
      checks <- rebuild_b_result$checks
      b <- rebuild_b_result$b
      a <- rebuild_b_result$a
      z <- rebuild_b_result$z
      error <- rebuild_b_result$error

      contrasts_result <- resolution_multicity_contrasts(
        lv = lv,
        out = out,
        population = population,
        cells = cells,
        ladder = ladder,
        exposure = exposure,
        outcomes = outcomes,
        checks = checks,
        profiles = profiles,
        contrasts = contrasts,
        variances = variances,
        composition = composition,
        differences = differences,
        bootstraps = bootstraps,
        transitions = transitions,
        exposure_changes = exposure_changes,
        good_keys = good_keys,
        c_map = c_map,
        c_common = c_common,
        k = k,
        b = b,
        a = a,
        error = error,
        group_col = group_col,
        groups = groups)
      composition <- contrasts_result$composition
      differences <- contrasts_result$differences
      bootstraps <- contrasts_result$bootstraps
      transitions <- contrasts_result$transitions
      exposure_changes <- contrasts_result$exposure_changes
      profiles <- contrasts_result$profiles
      contrasts <- contrasts_result$contrasts
      checks <- contrasts_result$checks
      variances <- contrasts_result$variances
      boot <- contrasts_result$boot

      matrix_checks_result <- resolution_multicity_matrix_checks(
        city = city,
        lv = lv,
        inputs = inputs,
        out = out,
        population = population,
        panel = panel,
        matrices = matrices,
        reconciliations = reconciliations,
        movements = movements,
        bdir = bdir,
        b = b,
        profiles = profiles,
        contrasts = contrasts,
        variances = variances,
        composition = composition,
        differences = differences,
        bootstraps = bootstraps,
        transitions = transitions,
        exposure_changes = exposure_changes,
        checks = checks,
        matrix_root = matrix_root,
        buffer_km = buffer_km)
      reconciliations <- matrix_checks_result$reconciliations
      movements <- matrix_checks_result$movements
      u <- matrix_checks_result$u
      matrices <- matrix_checks_result$matrices
      oc <- matrix_checks_result$oc
      exact <- matrix_checks_result$exact
      passed <- matrix_checks_result$passed

    }
    verify_city_result <- resolution_multicity_verify_city(
      city = city,
      all_tables = all_tables,
      inputs = inputs,
      out = out,
      population = population,
      cells = cells,
      exposure = exposure,
      outcomes = outcomes,
      profiles = profiles,
      contrasts = contrasts,
      variances = variances,
      movements = movements,
      oc = oc,
      good_keys = good_keys,
      group_col = group_col,
      groups = groups)
    nesting <- verify_city_result$nesting
    direct <- verify_city_result$direct
    all_tables <- verify_city_result$all_tables
    results_by_city[[city]] <- list(contrasts = contrasts, profiles = profiles,
      matrices = matrices, reconciliations = reconciliations, checks = checks)

  }
  # The historical Bogota anchor exists only for the 3 km quintile specification.
  if (buffer_km == 3 && grouping == "edu_quintile" && "bogota_2018" %in% cities) {
    reference_anchor_result <- resolution_multicity_reference_anchor(
      z = z,
      direct = direct)
    z <- reference_anchor_result$z
  }

  # Combined tables cover every city whose run at this buffer is complete.
  complete_cities <- all_cities[vapply(all_cities, function(x) {
    status <- file.path(resolution_buffer_root(buffer_km, grouping), x, "STATUS.txt")
    file.exists(status) && readLines(status, n = 1L) == "complete"
  }, logical(1))]

  save_tables_result <- resolution_multicity_save_tables(
    city = city,
    lv = lv,
    cities = complete_cities,
    exposure = exposure,
    z = z,
    boot = boot,
    u = u,
    oc = oc,
    exact = exact,
    passed = passed,
    nesting = nesting,
    buffer_km = buffer_km,
    grouping = grouping)
  verification_summary <- save_tables_result$verification_summary
  patterns <- save_tables_result$patterns
  exposure <- save_tables_result$exposure

}

# ============================================================================================
# III: Check outputs
# ============================================================================================
if (resolution_scope == "reference") {
  print(ladder_check)
  print(utils::head(ci_all))
} else {
  print(verification_summary)
  str(results_by_city, max.level = 1)
}
