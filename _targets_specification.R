# Specification-sensitivity dependencies, kept apart from the manuscript graph (_targets.R):
# this analysis reads frozen copies of manuscript products and must never rebuild them.
# Source reusable definitions only; none of these calls installs packages or runs analysis.
source(here::here("src", "general_utilities", "config_utils_resolution.R"), local = TRUE)
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"), local = TRUE)
source(here::here("src", "general_utilities", "plot", "specification_sensitivity",
                  "resolution_workflow.R"), local = TRUE)
source(here::here("config", "analysis_settings.R"), local = TRUE)
targets::tar_option_set(error = "stop", seed = 20260910L,
  packages = c("data.table", "ggplot2"))

source(here::here("config", "targets", "specification_sensitivity.R"), local = TRUE)$value
