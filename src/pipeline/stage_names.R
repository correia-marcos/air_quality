# Reusable stage names in dependency order; optional workflows remain outside this list.
#' @return Explicitly loaded manuscript preparation and rendering adapter names.
manuscript_stage_names <- function() c(
  "compute_descriptive_tables",
  "compute_station_scatter_inputs",
  "generate_panel_air_quality",
  "prepare_station_temporal",
  "plot_station_monitoring_figures",
  "figure_station_scatter",
  "figure_population_density_maps",
  "figure_pollution_quintile_maps",
  "figure_kernel_distributions",
  "figure_quintile_kernel_distributions",
  "figure_station_temporal")
