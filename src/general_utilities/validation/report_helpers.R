# Attach the existing report dependencies without installing packages.
#' @return NULL invisibly; changes only the package search path.
load_validation_report_packages <- function() {
  packages <- c("here", "arrow", "dplyr", "ggplot2", "knitr", "kableExtra",
                "scales", "lubridate", "sf", "leaflet")
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Restore the report dependency before rendering: ", package)
    }
    library(package, character.only = TRUE)
  }
  invisible(NULL)
}

# Read one optional comparison artifact for an interactive report chunk.
#' @param city_dir Declared directory containing the city's comparison products.
#' @param subdir Comparison family within city_dir.
#' @param name Artifact basename without the Parquet extension.
#' @return A table when present, otherwise NULL (report status remains pending).
read_validation_parquet <- function(city_dir, subdir, name) {
  path <- file.path(city_dir, subdir, paste0(name, ".parquet"))
  if (file.exists(path)) arrow::read_parquet(path) else NULL
}

# Format completion status without changing the comparison data.
#' @param ok Whether the comparison artifact is present.
#' @param label_ok,label_pending Labels for the two states.
#' @return HTML span used by the Quarto status table; writes nothing.
validation_status_badge <- function(ok, label_ok = "Complete",
                                    label_pending = "Pending") {
  color <- if (ok) "#2e7d32" else "#9e9e9e"
  label <- if (ok) label_ok else label_pending
  sprintf(paste0('<span style="background:%s;color:white;padding:2px 8px;',
                 'border-radius:4px;font-size:.85em">%s</span>'), color, label)
}
