# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate the A/B/C robustness designs for every declared specification.
#
#' @Description: For each variant in resolution_specifications (IDW buffer x education
# grouping) and each city, compare exposure aggregation A, exposure rebuilt by IDW B and
# area classification C from the frozen inputs. City products go to
# data/processed/resolution_sensitivity/; combined tables to results/tables/
# specification_sensitivity/robustness_designs/. Definitions: doc/RESOLUTION_SENSITIVITY.md.
#
#' @Summary:
#   I.   Import data: source functions and settings, validate the specifications.
#   II.  Process data: estimate each specification and city, then combine the cities.
#   III. Save outputs: write the combined tables and the code provenance.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions, their packages and the declared variants.
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

# Validate the declared variants; each fixes its output folders and file prefix.
specifications <- lapply(names(resolution_specifications), resolution_specification,
                         specifications  = resolution_specifications,
                         cities         = resolution_cities)
names(specifications) <- names(resolution_specifications)

# Set the archived Bogota reference that the 3 km quintile run must reproduce.
file_reference <- here::here("data", "processed", "resolution_sensitivity",
                             "resolution_ci_bogota_3km_2023.csv")

# List the code recorded with every run's tables.
code_files <- c(
  "scripts/process_data/specification_sensitivity/prepare_resolution_inputs.R",
  "scripts/process_data/specification_sensitivity/estimate_resolution_sensitivity.R",
  "scripts/tables_images/specification_sensitivity/figure_resolution_sensitivity.R",
  "src/general_utilities/config_utils_resolution.R",
  "src/general_utilities/process/specification_sensitivity/resolution_sensitivity.R",
  "src/general_utilities/process/specification_sensitivity/resolution_specification.R",
  "src/general_utilities/process/specification_sensitivity/resolution_workflow.R",
  "src/general_utilities/plot/specification_sensitivity/resolution_workflow.R",
  "src/general_utilities/process/idw_exposure.R",
  "src/general_utilities/process/distances.R",
  "src/general_utilities/process/exposure_regressions.R",
  "config/analysis_settings.R")

# Paired bootstraps set their own seed; this fixes any other draw.
set.seed(20260910)

# ========================================================================================
# II: Process data
# ========================================================================================
# run_resolution_city() also writes each city's tables; see its @details.
robustness   <- list()
anchors      <- list()
combined     <- list()
verification <- list()
patterns     <- list()

# Select the first specification and city when executing line by line in RStudio.
id   <- names(specifications)[1]
city <- resolution_cities[1]

for (id in names(specifications)) {
  # Estimate designs A, B and C for each city under this specification.
  for (city in resolution_cities) {
    robustness[[id]][[city]] <- run_resolution_city(
      specification       = specifications[[id]],
      city                = city,
      education_bands     = education_level_bands,
      education_group_map = education_group3_of_level,
      duckdb_mem_gb       = resolution_duckdb_mem_gb)
  }

  # Check the 3 km quintile run against the archived Bogota reference ladder.
  if (specifications[[id]]$reference_anchor) {
    anchors[[id]] <- resolution_reference_anchor(specification      = specifications[[id]],
                                                 archived_reference = file_reference)
  }

  # Combine the cities' saved tables and summarize their checks and gap patterns.
  combined[[id]]     <- resolution_combine_cities(specifications[[id]])
  verification[[id]] <- resolution_verification_summary(specifications[[id]])
  patterns[[id]]     <- resolution_gap_patterns(specifications[[id]])
}

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save each specification's combined tables, then its code provenance.
table_files      <- list()
provenance_files <- list()
for (id in names(specifications)) {
  table_files[[id]] <- write_resolution_tables(specification = specifications[[id]],
                                               combined      = combined[[id]],
                                               verification  = verification[[id]],
                                               patterns      = patterns[[id]])
  provenance_files[[id]] <- write_resolution_provenance(
    specification = specifications[[id]],
    code_files    = code_files)
}
