# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal  : Run all validation comparisons for Bogotá and render a single city-level report.
#
# @Description: This script compares the new automated pipeline against the legacy dataset. 
#   Artefacts are written to: results/validation_old_version/bogota/{data_type}/
# 
# @Details: This script is the single entry point for comparing every data layer
# of the Bogotá pipeline against the coauthor's legacy dataset. Comparison functions are called
# in pipeline order (raw → interim → processed → results). All artefacts are written to:
#     results/validation_old_version/bogota/
# A single self-contained HTML report is rendered from bogota_report.qmd.
#
# @Summary: 
#   I.   Setup: Load dependencies and configurations.
#   II.  Compare Data: Run modular validation functions for ground stations, outliers, etc.
#   III. Render Report: Generate the final Quarto HTML document.
#
# @Date  : April 2026
# @Author: Marcos Paulo
# ============================================================================================

# Load all utility functions and city configs
source(here::here("src", "general_utilities","config_utils_validation_old_version.R"))
source(here::here("src", "general_utilities","config_utils_process_data.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "bogota.R"))

# ============================================================================================
# I: Setup
# ============================================================================================
# Root folder for all Bogotá comparison artefacts
city_id   <- "bogota"
cfg       <- city_cfg(city_id)

# Attach the validation/comparison sublist to cfg
cfg$compare <- build_compare_cfg(city_id)

# ============================================================================================
# II: Process and Compare Data
# ============================================================================================
# 1. Compare Raw Ground Stations
gs_results <- compare_ground_stations(
  cfg      = cfg,
  out_root = cfg$compare$city_dir)

# 2. Compare Metro area
metro_results <- compare_metro_area(
  cfg           = cfg,
  out_root      = cfg$compare$city_dir,
  station_audit = if (!is.null(gs_results)) gs_results$station_audit else NULL)

# 3, Compare Census data
census_results <- compare_census(
  cfg      = cfg,
  out_root = cfg$compare$city_dir)

# 4. Compare station-to-station and Station-to-locality distances
idw_results <- compare_idw(
  cfg           = cfg,
  out_root      = cfg$compare$city_dir,
  station_audit = if (!is.null(gs_results)) gs_results$station_audit else NULL
)

# 5. Compare Outlier-corrected pollution panel
outlier_results <- compare_outlier_procedure(
  cfg           = cfg,
  out_root      = cfg$compare$city_dir,
  station_audit = if (!is.null(gs_results)) gs_results$station_audit else NULL
)

# 10: Console summary
message("\n", strrep("=", 60))
message(sprintf("%s — Execution Summary", toupper(city_id)))
message(strrep("=", 60))

# Group the summary tables into a named list
summaries <- list(
  "Ground Stations"   = if (!is.null(gs_results)) gs_results$diff_summary else NULL,
  "Metro Area"        = if (!is.null(metro_results)) metro_results$summary else NULL,
  "Census" = if (!is.null(census_results)) census_results$collapsed_summary else NULL,
  "Distance Matrices" = if (!is.null(idw_results)) idw_results$station_dist_summary else NULL,
  "Outlier Detection" = if (!is.null(outlier_results)) outlier_results$step_summary else NULL
)

# Loop through and print cleanly
for (name in names(summaries)) {
  df <- summaries[[name]]
  if (!is.null(df)) {
    message(sprintf("\n--- %s ---", name))
    
    # Optional: Keep your specific row count logic for Ground Stations
    if (name == "Ground Stations") {
      message("Rows only in legacy : ", format(nrow(gs_results$only_legacy), big.mark = ","))
      message("Rows only in new    : ", format(nrow(gs_results$only_new), big.mark = ","))
    }
    
    # as.data.frame strips the noisy tibble headers/data types from the console output
    print(as.data.frame(df), row.names = FALSE)
  }
}

# =============================================================================================
# III: Render city report
# =============================================================================================
# Quarto writes the HTML next to the .qmd; we copy it to results/ afterward.
tryCatch({
  qmd_path = cfg$compare$qmd_path
  html_dest = cfg$compare$html_dest
  html_name = basename(cfg$compare$html_dest)
  
  quarto::quarto_render(
    input          = qmd_path,
    output_file    = html_name,   # filename only — Quarto does not allow paths
    execute_params = list(
      city_id  = city_id,
      out_root = cfg$compare$out_root
    )
  )
  # Move rendered HTML to results folder
  html_beside_qmd <- file.path(dirname(qmd_path), html_name)
  if (normalizePath(html_beside_qmd) != normalizePath(html_dest)) {
    file.copy(html_beside_qmd, html_dest, overwrite = TRUE)
    file.remove(html_beside_qmd)
  }
  message("\nReport: ", html_dest)
},
error = function(e) warning(
  "Report rendering failed:\n", conditionMessage(e)
))

cat("Script from the IDB project executed successfully!\n")