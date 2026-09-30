# File ownership and manual-stage inputs. The graph names its upstream targets explicitly.

# Locate a data-family directory through the files actually supplied by the caller.
#' @param inputs Explicit upstream files or directories; alternate roots are supported.
#' @param family Directory component identifying the data family, such as census.
#' @return One absolute directory derived from inputs; never a canonical-path fallback.
pipeline_input_root <- function(inputs, family) {
  paths <- normalizePath(inputs, winslash = "/", mustWork = FALSE)
  roots <- vapply(strsplit(paths, "/", fixed = TRUE), function(parts) {
    at <- which(parts == family)
    if (!length(at)) return(NA_character_)
    paste(parts[seq_len(tail(at, 1L))], collapse = "/")
  }, character(1))
  roots <- unique(roots[!is.na(roots)])
  if (length(roots) != 1L) {
    stop("Supply exactly one ", family, " input root; found ", length(roots))
  }
  roots
}

#' @param cfg City configuration used by the manuscript graph.
#' @return Configuration with the canonical interim root required by downstream readers.
#' @details Manual city wrappers support separate output roots. The manuscript graph uses
#   the repository layout, isolated through Compose mounts during verification.
manuscript_city_config <- function(cfg) {
  root <- normalizePath(here::here("data", "interim"), mustWork = FALSE)
  if (!identical(normalizePath(cfg$out_dir, mustWork = FALSE), root)) {
    stop("The manuscript graph requires the canonical data/interim root. ",
         "Use isolated verification mounts, or a manual city wrapper for custom outputs.")
  }
  cfg
}

#' @param stage Maintained manuscript stage name.
#' @return Input roots for running a standalone script outside targets.
pipeline_stage_inputs <- function(stage) {
  processed <- here::here("data", "processed")
  interim <- here::here("data", "interim")
  roots <- switch(stage,
    estimate_exposure = here::here(processed, c("idw_estimates", "distances_matrices")),
    compute_descriptive_tables = c(here::here(interim, c("monitoring_stations",
      "census")),
      here::here(processed, c("monitoring_stations_outliers", "distances_matrices"))),
    compute_distance_band_descriptives = c(here::here(processed, "distances_matrices"),
      here::here(interim, c("census", "geospatial_data"))),
    compute_station_scatter_inputs = c(here::here(processed,
      "monitoring_stations_outliers"),
      here::here(interim, c("census", "geospatial_data"))),
    impute_missing_hourly = here::here(processed, "monitoring_stations_outliers"),
    estimate_exposure_imputed = c(here::here(processed, c("imputed_ols",
      "distances_matrices")), here::here(interim, "census")),
    generate_panel_air_quality = here::here("data", "raw",
      c("merra2_aerosol_products", "cities_shapefiles")),
    prepare_station_temporal = c(here::here(interim, "cities_m2_aerosols"),
      here::here("data", "raw", c("pollution_ground_stations", "cities_shapefiles"))),
    generate_exposure_plots = here::here(processed,
      c("idw_regressions", "idw_regressions_imputed")),
    figure_imputation_diagnostics = here::here(processed,
      c("imputed_ols", "station_socio_exposure")),
    plot_station_monitoring_figures = c(here::here(processed,
      c("distances_matrices", "station_socio_exposure")), here::here(interim, "census")),
    figure_station_scatter = here::here(processed, "station_socio_exposure"),
    figure_population_density_maps = here::here(interim,
      c("geospatial_data", "census", "monitoring_stations")),
    figure_pollution_quintile_maps = here::here(interim,
      c("geospatial_data", "census", "monitoring_stations")),
    figure_kernel_distributions = here::here(processed, "monitoring_stations_outliers"),
    figure_quintile_kernel_distributions = here::here(processed, "idw_estimates"),
    figure_station_temporal = here::here(processed, "merra2_stations_pm25"),
    render_station_tables = here::here(processed,
      c("station_counts", "who_exceedances", "threshold_exceedances")),
    render_missing_tables = here::here(processed, "missing_proportions"),
    render_census_tables = here::here(processed,
      c("census_summary", "distance_band_descriptives")),
    render_exposure_tables = here::here(processed, "idw_regressions"),
    stop("Unknown manuscript stage: ", stage))
  roots
}

#' @param stage Maintained preparation/analysis stage name.
#' @return Exclusively owned output directories, including all files and sidecars.
pipeline_output_roots <- function(stage) {
  roots <- switch(stage,
    estimate_exposure = "idw_regressions",
    compute_descriptive_tables = c("missing_proportions", "station_counts",
      "who_exceedances", "census_summary", "threshold_exceedances"),
    compute_distance_band_descriptives = "distance_band_descriptives",
    compute_station_scatter_inputs = "station_socio_exposure",
    impute_missing_hourly = "imputed_ols",
    estimate_exposure_imputed = c("idw_estimates_imputed", "idw_regressions_imputed"),
    prepare_station_temporal = c("merra2_pm25", "merra2_stations_pm25"),
    generate_panel_air_quality = "cities_m2_aerosols",
    NULL)
  if (is.null(roots)) return(character())
  layer <- if (stage == "generate_panel_air_quality") "interim" else "processed"
  here::here("data", layer, roots)
}

#' @param stage Manuscript stage name.
#' @param written Paths returned by the stage's writers, including unselected artifacts.
#' @return Verified output paths with all required manuscript files accounted for.
pipeline_stage_outputs <- function(stage, written = character()) {
  roots <- pipeline_output_roots(stage)
  if (length(roots)) return(processing_files(roots))
  manifest <- artifact_manifest(here::here("config", "paper_artifacts.csv"))
  required <- here::here(manifest$source_path[
    basename(manifest$producer_script) == paste0(stage, ".R")])
  written <- unique(written)
  if (!all(required %in% written)) {
    stop("Stage did not report required manuscript files: ",
         paste(setdiff(required, written), collapse = ", "))
  }
  processing_files(written)
}

#' @param artifacts Upstream figure and table outputs.
#' @param manifest_file Canonical export mapping tracked as a file target.
#' @return Exported manuscript files and integrity manifest under data/processed/paper.
#' @details Required files must be reported by current targets, even if old copies exist.
prepare_paper_export <- function(artifacts, manifest_file) {
  processing_files(artifacts, "manuscript artifacts")
  manifest <- artifact_manifest(manifest_file)
  required <- here::here(manifest$source_path)
  missing <- !normalizePath(required, mustWork = FALSE) %in%
    normalizePath(artifacts, mustWork = TRUE)
  if (any(missing)) {
    stop("Current targets did not report manuscript artifacts: ",
         paste(manifest$source_path[missing], collapse = ", "))
  }
  destination <- here::here("data", "processed", "paper")
  exported <- export_paper_artifacts(manifest, here::here(), destination,
                                     dry_run = FALSE, overwrite = TRUE)
  inventory <- here::here(destination, "export-manifest.csv")
  utils::write.csv(exported, inventory, row.names = FALSE)
  processing_files(c(exported$destination, inventory))
}
