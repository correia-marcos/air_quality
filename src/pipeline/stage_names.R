# Reusable stage names in dependency order; optional workflows remain outside this list.
#' @return Explicitly loaded manuscript preparation and rendering adapter names.
manuscript_stage_names <- function() c(
  "generate_panel_air_quality",
  "prepare_station_temporal")
