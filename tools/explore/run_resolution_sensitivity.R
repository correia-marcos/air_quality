# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Run the A/B/C robustness designs on chosen values, outside the saved results.
#
#' @Description: Explores undeclared buffers, groupings or cities, or reruns the declared
# variants (--declared), with the same functions as the recipes in
# scripts/{process_data,tables_images}/specification_sensitivity/. Writes only under
# data/exploration/<run-id>/, which is git-ignored; never replaces saved results.
#
#' @Summary:
#   I.   Settings: read the options and declare the run's folders.
#   II.  Execute: optionally re-prepare inputs, estimate, combine and plot.
#
#   Options:
#     --run-id=<id>         required; output root data/exploration/<id>/
#     --declared            run every variant in resolution_specifications
#     --buffer-km=<km>      with --grouping, one undeclared variant (needs a manuscript
#     --grouping=<g>        exposure file at that buffer); groupings other than
#                           edu_quintile also run the quintile variant first
#     --cities=<a,b>        subset of resolution_cities
#     --prepare             re-prepare frozen inputs and matrices inside the run root
#     --figures             plot the 3 km variants of the run
#     --duckdb-mem-gb=<n>   DuckDB memory limit of the B rebuild
#     --reuse-b             reuse quintile B rebuilds whose input hashes match
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Settings
# ========================================================================================
args <- commandArgs(trailingOnly = TRUE)
known <- paste0("^--(run-id|buffer-km|grouping|cities|duckdb-mem-gb)=|",
                "^--(declared|prepare|figures|reuse-b)$")
if (any(!grepl(known, args))) stop("Unknown option: ", args[!grepl(known, args)][1])
option <- function(name) sub(paste0("^--", name, "="), "",
                             grep(paste0("^--", name, "="), args, value = TRUE))
run_id <- option("run-id")
if (length(run_id) != 1L || !nzchar(run_id)) stop("--run-id=<id> is required.")

# Load the functions, their packages and the declared variants.
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "general_utilities", "plot", "specification_sensitivity",
                  "resolution_workflow.R"))
source(here::here("config", "analysis_settings.R"))

# Declare the run root; inputs are re-prepared there only with --prepare.
run_root   <- here::here("data", "exploration", run_id)
input_root <- if ("--prepare" %in% args) run_root else here::here()
cities <- if (length(option("cities"))) strsplit(option("cities"), ",")[[1]] else
  resolution_cities
stopifnot(all(cities %in% resolution_cities))
mem_gb <- if (length(option("duckdb-mem-gb"))) as.numeric(option("duckdb-mem-gb")) else
  resolution_duckdb_mem_gb

# Select the variants: the declared ones, or one undeclared buffer and grouping.
if ("--declared" %in% args) {
  variants <- resolution_specifications
} else {
  buffer_km <- as.numeric(option("buffer-km"))
  grouping  <- option("grouping")
  if (length(buffer_km) != 1L || length(grouping) != 1L) {
    stop("Use --declared, or both --buffer-km=<km> and --grouping=<g>.")
  }
  variants <- list()
  variants[[paste0("quintile_", buffer_km, "km")]] <-
    list(buffer_km = buffer_km, grouping = "edu_quintile")
  if (grouping != "edu_quintile") {
    variants[[paste0(sub("^edu_", "", grouping), "_", buffer_km, "km")]] <-
      list(buffer_km = buffer_km, grouping = grouping)
  }
}
specifications <- lapply(names(variants), resolution_specification,
                         specifications = variants, cities = cities,
                         root = run_root, input_root = input_root)
names(specifications) <- names(variants)
stopifnot(all(startsWith(vapply(specifications, `[[`, "", "processed_root"), run_root)))

file_reference <- here::here("data", "processed", "resolution_sensitivity",
                             "resolution_ci_bogota_3km_2023.csv")
code_files <- c(
  "tools/explore/run_resolution_sensitivity.R",
  "src/general_utilities/config_utils_resolution.R",
  "src/general_utilities/process/specification_sensitivity/resolution_sensitivity.R",
  "src/general_utilities/process/specification_sensitivity/resolution_specification.R",
  "src/general_utilities/process/specification_sensitivity/resolution_workflow.R",
  "src/general_utilities/plot/specification_sensitivity/resolution_workflow.R",
  "src/general_utilities/process/idw_exposure.R",
  "src/general_utilities/process/distances.R",
  "src/general_utilities/process/exposure_regressions.R",
  "config/analysis_settings.R")
set.seed(20260910)

# ========================================================================================
# II: Execute
# ========================================================================================
# Re-prepare the frozen inputs and matrices inside the run root.
if ("--prepare" %in% args) {
  dir_frozen   <- file.path(input_root, "data", "interim", "resolution_sensitivity")
  dir_prepared <- resolution_buffer_root(3, root = input_root)
  for (city in cities) {
    supports <- resolution_city_supports(city)
    inputs <- resolution_input_paths(city, supports)
    resolution_freeze_inputs(city, supports, inputs, file.path(dir_frozen, city),
                             file.path(dir_prepared, city))
    population <- resolution_population_cells(supports, inputs,
                                              file.path(dir_frozen, city))
    geometry <- resolution_fine_geometry(city, supports, inputs,
                                         population$census$geo_id,
                                         file.path(dir_frozen, city))
    resolution_population_denominators(city, supports, inputs, population$population,
                                       population$cells, geometry,
                                       file.path(dir_frozen, city),
                                       file.path(dir_prepared, city))
    keys <- resolution_support_keys(city, supports, inputs, population$census$geo_id,
                                    population$population, geometry,
                                    file.path(dir_frozen, city))
    resolution_support_matrices(city, supports, inputs, population$population,
                                population$cells, geometry, keys,
                                file.path(dir_frozen, city),
                                file.path(dir_prepared, city))
  }
}

# Estimate, anchor, combine and save each variant.
for (id in names(specifications)) {
  for (city in cities) {
    run_resolution_city(specification = specifications[[id]], city = city,
                        education_bands = education_level_bands,
                        education_group_map = education_group3_of_level,
                        duckdb_mem_gb = mem_gb, reuse_b = "--reuse-b" %in% args)
  }
  if (specifications[[id]]$reference_anchor && "bogota_2018" %in% cities) {
    resolution_reference_anchor(specifications[[id]], file_reference)
  }
  write_resolution_tables(specifications[[id]],
                          resolution_combine_cities(specifications[[id]]),
                          resolution_verification_summary(specifications[[id]]),
                          resolution_gap_patterns(specifications[[id]]))
  write_resolution_provenance(specifications[[id]], code_files)
}

# Plot the 3 km variants with the recipe's group labels.
if ("--figures" %in% args) {
  set_paper_theme(base_size = 11)
  for (id in names(specifications)[vapply(specifications, `[[`, 0, "buffer_km") == 3]) {
    labels <- resolution_grouping_labels[[specifications[[id]]$grouping]]
    tables <- resolution_figure_tables(specifications[[id]])
    plots <- c(plot_resolution_designs(tables, specifications[[id]]$grouping,
                                       labels$group_labels, labels$group_name,
                                       labels$gap_label, labels$profile_note),
               plot_resolution_monitoring(tables, labels$gap_label))
    save_resolution_figures(plots, specifications[[id]])
  }
}
cat("Exploration run written to", run_root, "\n")
