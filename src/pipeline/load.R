# Explicit loading for graph inspection and execution; never install packages.
#' @param envir Environment receiving functions and fixed city configurations.
#' @param kind Which package set a manual stage needs.
#' @param packages Attach already installed packages; FALSE permits graph inspection.
#' @return NULL invisibly; sources only declared reusable modules.
load_manuscript_functions <- function(envir = parent.frame(), kind = "all",
                                      packages = FALSE) {
  base <- here::here("src", "general_utilities")
  for (file in c("base_utils.R", "reproducibility.R", "theme_paper.R")) {
    sys.source(here::here(base, file), envir)
  }
  for (file in c("geo_ids", "merra2", "distances", "outliers", "idw_exposure",
                 "station_socio", "imputation", "diagnostics", "exposure_regressions")) {
    sys.source(here::here(base, "process", paste0(file, ".R")), envir)
  }
  for (file in c("maps", "timeseries_hourly", "exposure_figures", "latex_tables",
                 "station_monitoring", "imputation_diagnostics",
                 "concentration_distributions")) {
    sys.source(here::here(base, "plot", paste0(file, ".R")), envir)
  }
  sys.source(here::here("src", "city_specific", "registry.R"), envir)
  envir$load_city_modules(envir)
  for (file in c("packages", "stage_names", "contracts")) {
    sys.source(here::here("src", "pipeline", paste0(file, ".R")), envir)
  }
  for (stage in envir$manuscript_stage_names()) {
    sys.source(here::here("src", "pipeline", "stages", paste0(stage, ".R")), envir)
  }
  if (packages) {
    required <- envir$pipeline_packages(kind)
    missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) stop("Restore the project library before processing: ",
                              paste(missing, collapse = ", "))
    for (package in required) {
      suppressPackageStartupMessages(library(package, character.only = TRUE))
    }
    if (kind %in% c("process", "all")) suppressMessages(sf::sf_use_s2(TRUE))
  }
  invisible(NULL)
}
