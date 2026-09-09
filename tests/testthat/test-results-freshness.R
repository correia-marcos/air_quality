# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Guard that no tracked result is older than the data it was built from.
#
#' @Description: In August 2026 the regression inputs were regenerated while the figure
# layer kept June files, and nothing said so. This test declares the pipeline's
# input-directory -> output-directory pairs and asserts, for each, that the newest input
# file is not newer than the oldest output file. A stale stage becomes a failing test
# naming the stage to re-run. Pairs where either side is absent are skipped: absence means
# "not built here yet", which the pipeline itself reports.
#
#' @Summary:
#   I.   Declare the input -> output pairs, one per pipeline stage.
#   II.  Compare newest-input vs oldest-output modification times per pair.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

# Newest / oldest file mtime under one or more directories; NA when nothing is there.
dir_mtime <- function(dirs, fun) {
  files <- list.files(here::here(dirs), recursive = TRUE, full.names = TRUE)
  files <- files[!file.info(files)$isdir]
  if (length(files) == 0L) return(as.POSIXct(NA))
  fun(file.info(files)$mtime)
}

# One row per stage: everything under `inputs` feeds everything under `output`.
freshness_stages <- list(
  list(stage  = "detect_outliers.R",
       inputs = file.path("data", "interim", "monitoring_stations"),
       output = file.path("data", "processed", "monitoring_stations_outliers")),
  list(stage  = "estimate_idw.R",
       inputs = c(file.path("data", "processed", "monitoring_stations_outliers"),
                  file.path("data", "processed", "distances_matrices")),
       output = file.path("data", "processed", "idw_estimates")),
  list(stage  = "estimate_exposure.R",
       inputs = file.path("data", "processed", "idw_estimates"),
       output = file.path("data", "processed", "idw_regressions")),
  list(stage  = "generate_exposure_plots.R",
       inputs = file.path("data", "processed", "idw_regressions"),
       output = file.path("results", "figures", "exposure")),
  list(stage  = "compute_station_scatter_inputs.R",
       inputs = file.path("data", "processed", "monitoring_stations_outliers"),
       output = file.path("data", "processed", "station_socio_exposure")),
  list(stage  = "plot_station_monitoring_figures.R (education scatters)",
       inputs = file.path("data", "processed", "station_socio_exposure"),
       output = file.path("results", "figures", "monitoring")),
  list(stage  = "plot_station_monitoring_figures.R (distance panels)",
       inputs = file.path("data", "processed", "station_socio_exposure"),
       output = file.path("results", "figures", "monitoring")),
  list(stage  = "impute_missing_hourly.R",
       inputs = file.path("data", "processed", "monitoring_stations_outliers"),
       output = file.path("data", "processed", "imputed_ols")),
  list(stage  = "estimate_exposure_imputed.R",
       inputs = file.path("data", "processed", "imputed_ols"),
       output = file.path("data", "processed", "idw_regressions_imputed")),
  list(stage  = "figure_station_scatter.R",
       inputs = file.path("data", "processed", "station_socio_exposure"),
       output = file.path("results", "figures", "monitoring")),
  list(stage  = "figure_kernel_distributions.R",
       inputs = file.path("data", "processed", "monitoring_stations_outliers"),
       output = file.path("results", "figures", "temporal")),
  list(stage  = "render_missing_tables.R",
       inputs = file.path("data", "processed", "missing_proportions"),
       output = file.path("results", "tables")),
  list(stage  = "render_census_tables.R",
       inputs = file.path("data", "processed", "census_summary"),
       output = file.path("results", "tables")),
  list(stage  = "figure_population_density_maps.R",
       inputs = file.path("data", "interim", "census"),
       output = file.path("results", "figures", "maps"))
)

test_that("no stage output is older than its inputs", {
  for (st in freshness_stages) {
    newest_in  <- dir_mtime(st$inputs, max)
    # Topic folders mix producers. Only this producer's selected manuscript files
    # may stand in for its outputs; unrelated historical figures cannot date the stage.
    producer <- sub(" .*", "", st$stage)
    manifest <- artifact_manifest(here::here("config/paper_artifacts.csv"))
    selected <- manifest$source_path[basename(manifest$producer_script) == producer]
    if (length(selected)) {
      paths <- here::here(selected)
      oldest_out <- if (all(file.exists(paths))) min(file.info(paths)$mtime) else as.POSIXct(NA)
    } else oldest_out <- dir_mtime(st$output, min)
    if (is.na(newest_in) || is.na(oldest_out)) {
      if (identical(getOption("airmonitoring.test_mode"), "release")) {
        fail(paste("Missing freshness input/output for", st$stage))
      } else skip(paste("Missing freshness input/output for", st$stage))
      next
    }
    expect_lte(as.numeric(newest_in), as.numeric(oldest_out),
               label = paste0("newest input mtime for stage '", st$stage, "'"),
               expected.label = paste0("its oldest output mtime (stale: re-run ",
                                       st$stage, ")"))
  }
})
