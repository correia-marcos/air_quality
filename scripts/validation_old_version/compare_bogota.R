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
#   [x] Metro area      — geospatial boundaries
#   [x] Census          — demographic microdata (Extended 2005)
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
out_root  <- here::here("results", "validation_old_version")
city_id   <- "bogota"
cfg       <- city_cfg(city_id)

# Important paths
dir_geospatial   <- here::here("data", "raw", "geospatial_data")
dir_census       <- here::here("data", "interim", "census")
outdir_distances <- here::here("data", "processed", "distances_matrices")

# Quarto report lives next to this script; output goes to results/
qmd_path  <- here::here(out_root, "bogota", "bogota_report.qmd")
html_name <- "bogota_validation_report.html"
html_dest <- file.path(out_root, city_id, html_name)

# Tolerances (0 = exact match required)
tolerances <- c(pm10 = 0, pm25 = 0, ozone = 0, co = 0, no2 = 0)

# ---------------------------------------
# a: Ground station comparison
# ---------------------------------------
message("\n", strrep("=", 60))
message("Bogotá — Ground stations")
message(strrep("=", 60))

gs_results <- tryCatch(
  compare_ground_stations(cfg              = cfg,
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

# ---------------------------------------
# b: Metro area comparison
# ---------------------------------------
message("\n", strrep("=", 60))
message("Bogotá — Metro area boundaries")
message(strrep("=", 60))

metro_results <- tryCatch(
  compare_metro_area(cfg               = cfg,
                     out_root          = out_root,
                     new_metro_gpkg    = here::here(dir_geospatial, "bogota",
                                                    "bogota_area_metro_2018.gpkg"),
                     new_stations_gpkg = here::here(dir_geospatial, "bogota",
                                                    "bogota_2018_stations_buffer_metro.gpkg"),
                     legacy_shp_dir    = here::here("data", "_legacy",
                                                    "cities_shapefiles", "Bogota_metro"),
                     station_audit     = 
                       if (!is.null(gs_results)) gs_results$station_audit else NULL,
                     buffer_km         = 20,
                     quiet             = FALSE
  ),
  error = function(e) {
    warning("Metro area comparison failed:\n", conditionMessage(e))
    NULL
  }
)

# ---------------------------------------
# c: Census comparison
# ---------------------------------------
message("\n", strrep("=", 60))
message("Bogotá — Census (Extended 2005)")
message(strrep("=", 60))

census_results <- tryCatch(
  compare_census(
    cfg              = cfg,
    out_root         = out_root,
    new_collapsed    = here::here(dir_census, "bogota_extended_2005",
                                  "collapse_metro_area_extended.csv"),
    new_individual   = here::here(dir_census, "bogota_extended_2005",
                                  "census_metro_individual_extended.csv"),
    legacy_collapsed = here::here("data", "_legacy", "census",
                                  "collapse_bogota_metro.csv"),
    legacy_individual = here::here("data", "_legacy", "census",
                                   "census_bogota_metro.csv"),
    join_key         = "GEO_ID",
    tol              = 0.001,
    quiet            = FALSE
  ),
  error = function(e) {
    warning("Census comparison failed:\n", conditionMessage(e))
    NULL
  }
)

# ---------------------------------------
# d: Station-to-locality distances
# ---------------------------------------
message("\n", strrep("=", 60))
message("Bogota — Distance matrices")
message(strrep("=", 60))

idw_results <- tryCatch(
  compare_idw(
    cfg                 = cfg,
    out_root            = out_root,
    new_station_dist    = here::here(outdir_distances,
                                     "bogota_2018_station_distances.parquet"),
    new_geo_dist        = here::here(outdir_distances,
                                     "bogota_2018_geo_station_distances.parquet"),
    legacy_station_dist = here::here("data", "_legacy", "distances", "bogota",
                                     "stations_distance_bogota_v2.csv"),
    legacy_geo_dist     = here::here("data", "_legacy", "distances", "bogota",
                                     "dt_distances.rds"),
    station_audit       = if (!is.null(gs_results)) gs_results$station_audit else NULL,
    tol_km              = 0.5,
    quiet               = FALSE
  ),
  error = function(e) {
    warning("Distance comparison failed:\n", conditionMessage(e))
    NULL
  }
)

# ---------------------------------------
# e: Outlier-corrected pollution panel
# ---------------------------------------
message("\n", strrep("=", 60))
message("Bogota — Outlier detection")
message(strrep("=", 60))

outlier_results <- tryCatch(
  compare_outlier_procedure(
    cfg               = cfg,
    out_root          = out_root,
    new_clean_dir     = here::here("data", "processed", "outlier_detection",
                                   "bogota_metro_clean"),
    new_raw_dir       = here::here("data", "raw", "monitoring_stations",
                                   "bogota_metro_dataset"),
    legacy_clean_path = here::here("data", "_legacy", "outlier", "bogota",
                                   "pollution_data_balanced_2023.rds"),
    legacy_raw_path   = cfg$compare$legacy_single_csv,
    pollutants        = c("pm10", "pm25"),
    compare_years     = cfg$compare$compare_years,
    station_audit     = if (!is.null(gs_results)) gs_results$station_audit else NULL,
    quiet             = FALSE
  ),
  error = function(e) {
    warning("Outlier comparison failed:\n", conditionMessage(e))
    NULL
  }
)

# ---------------------------------------
# f: Console summary
# ---------------------------------------
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

if (!is.null(metro_results)) {
  message("\nMetro area:")
  print(metro_results$summary)
}

if (!is.null(census_results)) {
  message("\nCensus (collapsed):")
  print(census_results$collapsed_summary)
}

if (!is.null(idw_results)) {
  message("\nDistance matrices:")
  print(idw_results$station_dist_summary)
}

if (!is.null(outlier_results)) {
  message("\nOutlier detection:")
  print(outlier_results$step_summary)
}

# =============================================================================================
# III: Render city report
# =============================================================================================
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