# Reusable stage names in dependency order; optional workflows remain outside this list.
#' @return Explicitly loaded manuscript preparation and rendering adapter names.
manuscript_stage_names <- function() c(
  "compute_descriptive_tables",
  "compute_distance_band_descriptives",
  "compute_station_scatter_inputs",
  "impute_missing_hourly",
  "estimate_exposure_imputed",
  "generate_panel_air_quality",
  "prepare_station_temporal",
  "generate_exposure_plots",
  "figure_imputation_diagnostics",
  "plot_station_monitoring_figures",
  "figure_station_scatter",
  "figure_population_density_maps",
  "figure_pollution_quintile_maps",
  "figure_kernel_distributions",
  "figure_quintile_kernel_distributions",
  "figure_station_temporal",
  "render_station_tables",
  "render_missing_tables",
  "render_census_tables",
  "render_exposure_tables")
