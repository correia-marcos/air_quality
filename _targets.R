# Manuscript dependencies: scientific objects and their saved checkpoints.
# Source reusable definitions only; none of these calls installs packages or runs analysis.
source(here::here("src", "general_utilities", "base_utils.R"), local = TRUE)
source(here::here("src", "general_utilities", "reproducibility.R"), local = TRUE)
source(here::here("src", "general_utilities", "theme_paper.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "geo_ids.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "station_temporal.R"),
       local = TRUE)
source(here::here("src", "general_utilities", "process", "distances.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "outliers.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "pollution_quality.R"),
       local = TRUE)
source(here::here("src", "general_utilities", "process", "idw_exposure.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "station_socio.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "imputation.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "diagnostics.R"), local = TRUE)
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"),
       local = TRUE)
source(here::here("src", "general_utilities", "plot", "maps.R"), local = TRUE)
source(here::here("src", "general_utilities", "plot", "timeseries_hourly.R"), local = TRUE)
source(here::here("src", "general_utilities", "plot", "exposure_figures.R"), local = TRUE)
source(here::here("src", "general_utilities", "plot", "latex_tables.R"), local = TRUE)
source(here::here("src", "general_utilities", "plot", "station_monitoring.R"), local = TRUE)
source(here::here("src", "general_utilities", "plot", "imputation_diagnostics.R"),
       local = TRUE)
source(here::here("src", "general_utilities", "plot", "concentration_distributions.R"),
       local = TRUE)
source(here::here("src", "city_specific", "registry.R"), local = TRUE)
load_city_modules()
source(here::here("config", "analysis_settings.R"), local = TRUE)
targets::tar_option_set(error = "stop", seed = manuscript_seed,
  packages = c("dplyr", "data.table", "lubridate", "ggplot2", "tidyr", "ggridges"))

# Each module returns ordinary declarations; no scientific recipe is executed here.
c(
  source(here::here("config", "targets", "bogota.R"), local = TRUE)$value,
  source(here::here("config", "targets", "cdmx.R"), local = TRUE)$value,
  source(here::here("config", "targets", "santiago.R"), local = TRUE)$value,
  source(here::here("config", "targets", "sao_paulo.R"), local = TRUE)$value,
  source(here::here("config", "targets", "distance_idw.R"), local = TRUE)$value,
  source(here::here("config", "targets", "exposure.R"), local = TRUE)$value,
  source(here::here("config", "targets", "descriptives.R"), local = TRUE)$value,
  source(here::here("config", "targets", "station_context.R"), local = TRUE)$value,
  source(here::here("config", "targets", "imputation.R"), local = TRUE)$value,
  source(here::here("config", "targets", "monitoring_figures.R"), local = TRUE)$value,
  source(here::here("config", "targets", "spatial_figures.R"), local = TRUE)$value,
  source(here::here("config", "targets", "distribution_figures.R"), local = TRUE)$value,
  source(here::here("config", "targets", "station_temporal.R"), local = TRUE)$value,
  source(here::here("config", "targets", "tables_export.R"), local = TRUE)$value
)
