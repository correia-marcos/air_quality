#' @param kind Processing, plotting, or the complete manuscript pipeline.
#' @return Required packages in the original attachment order; installs nothing.
pipeline_packages <- function(kind = c("all", "process", "plot")) {
  kind <- match.arg(kind)
  process <- c("archive", "arrow", "censobr", "data.table", "DBI", "doParallel",
    "dplyr", "duckdb", "exactextractr", "foreach", "geosphere", "here", "janitor",
    "lubridate", "memuse", "readr", "rio", "rlang", "rnaturalearth",
    "rnaturalearthdata", "sandwich", "sf", "stringi", "terra", "tibble", "tidyr",
    "tools", "XLConnect", "XML")
  plot <- c("arrow", "cowplot", "data.table", "dplyr", "ggmap", "ggplot2", "ggspatial",
    "ggridges", "haven", "here", "htmltools", "kableExtra", "leaflet", "lubridate",
    "rlang", "rnaturalearth", "rnaturalearthdata", "rnaturalearthhires", "sp", "sf",
    "showtext", "terra", "tidyr", "viridisLite", "viridis", "zoo")
  switch(kind, process = process, plot = plot, all = unique(c(process, plot)))
}
