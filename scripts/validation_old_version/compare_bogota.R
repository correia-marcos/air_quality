# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal  : Run all validation comparisons for Bogotá and render a single city-level report.
#
# @Description: This script is the single entry point for comparing every data layer
# of the Bogotá pipeline against the coauthor's legacy dataset. Comparison functions are called
# in pipeline order (raw → interim → processed → results). All artefacts are written to:
#     results/validation_rep_package/bogota/{data_type}/
# A single self-contained HTML report is rendered from bogota_report.qmd.
#
# @Data types compared (add a section as each comparison is implemented):
#   [x] Ground stations — raw hourly pollution data
#   [x] Census          — demographic microdata (placeholder)
#   [x] Metro area      — geospatial boundaries (placeholder)
#   [x] Distances       — station-to-locality distance matrices (placeholder)
#   [x] Processed panel — outlier-corrected Parquet dataset (placeholder)
#
# @Date  : April 2026
# @Author: Marcos Paulo
# ============================================================================================

# Load all utility functions and city configs
source(here::here("src", "general_utilities","config_utils_validation_old_version.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "bogota.R"))

# ============================================================================================
# I: Import Data and parameters
# ============================================================================================
# Root folder for all Bogotá comparison artefacts
out_root  <- here::here("results", "validation_legacy")
city_id   <- "bogota"
cfg       <- city_cfg(city_id)

# Quarto report lives next to this script; output goes to results/
qmd_path  <- here::here("results", "validation_old_version", "reports", "bogota_report.qmd")
html_name <- "bogota_validation_report.html"
html_dest <- file.path(out_root, city_id, html_name)

# Tolerances (0 = exact match required)
tolerances <- c(pm10 = 0, pm25 = 0, ozone = 0, co = 0, no2 = 0)

# ============================================================================
# II: Ground station comparison
# ============================================================================
message("\n", strrep("=", 60))
message("Bogotá — Ground stations")
message(strrep("=", 60))

gs_results <- tryCatch(
  compare_ground_stations(
    cfg              = cfg,
    out_root         = out_root,
    focus_pollutants = cfg$compare$focus_pollutants,
    pipeline_tz      = cfg$compare$pipeline_tz,
    tol              = tolerances,
    quiet            = FALSE
  ),
  error = function(e) {
    warning("Ground station comparison failed:\n", conditionMessage(e))
    NULL
  }
)

# ============================================================================
# III: Census comparison (placeholder — implement when ready)
# ============================================================================
# census_results <- compare_census(cfg = cfg, out_root = out_root)

# ============================================================================
# IV: Metro area comparison (placeholder)
# ============================================================================
# metro_results <- compare_metro_area(cfg = cfg, out_root = out_root)

# ============================================================================
# V: Console summary
# ============================================================================
message("\n", strrep("-", 60))
message("Bogotá summary")
message(strrep("-", 60))

if (!is.null(gs_results)) {
  message("Ground stations:")
  message(
    "  Rows only in legacy : ",
    format(nrow(gs_results$only_legacy), big.mark = ",")
  )
  message(
    "  Rows only in new    : ",
    format(nrow(gs_results$only_new), big.mark = ",")
  )
  print(gs_results$diff_summary)
}

# ============================================================================
# VI: Render city report
# ============================================================================
# Quarto writes the HTML next to the .qmd; we copy it to results/ afterward.
tryCatch({
  quarto::quarto_render(
    input          = qmd_path,
    output_file    = html_name,   # filename only — Quarto does not allow paths
    execute_params = list(
      city_id  = city_id,
      out_root = out_root
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