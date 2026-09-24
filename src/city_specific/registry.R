# ============================================================================================
# IDB: Air monitoring — Registry module
# ============================================================================================
#' @Goal   : register + retrieve city modules (cfg + functions)
#' @Date   : Aug 2025
#' @Author : Marcos Paulo
# ============================================================================================

if (!exists(".city_registry", inherits = FALSE)) {
  .city_registry <- new.env(parent = emptyenv())
}

#' @param id Stable lowercase city slug.
#' @param cfg City configuration containing download and derived-output roots.
#' @param download Optional acquisition entry point; processing never calls it.
#' @param process Complete processing wrapper with the common argument contract.
#' @return TRUE invisibly after a validated, unique registration.
register_city <- function(id, cfg, download = NULL, process) {
  stopifnot(is.character(id), length(id) == 1L, !is.na(id), nzchar(id),
            is.list(cfg), is.function(process),
            is.null(download) || is.function(download))
  if (!grepl("^[a-z][a-z0-9_]*$", id) ||
      !all(c("dl_dir", "out_dir") %in% names(cfg)) ||
      !identical(names(formals(process)), c("cfg", "steps", "inputs", "quiet"))) {
    stop("Invalid city configuration or processing interface: ", id)
  }
  if (exists(id, envir = .city_registry, inherits = FALSE)) {
    stop("City already registered: ", id)
  }
  .city_registry[[id]] <- list(
    cfg       = cfg,
    download  = download,
    process   = process
  )
  invisible(TRUE)
}

#' @param id Registered city slug.
#' @return Configuration and supported functions; unknown slugs fail explicitly.
get_city <- function(id) {
  stopifnot(is.character(id), length(id) == 1L, !is.na(id))
  mod <- .city_registry[[id]]
  if (is.null(mod)) stop("Unknown city: ", id)
  mod
}

#' @return Sorted registered city slugs.
list_cities <- function() sort(ls(.city_registry))

# Convenience dispatchers; reader-facing recipes expose the individual stages.
#' @param id Registered city slug.
#' @return City configuration without performing acquisition or processing.
city_cfg     <- function(id)             get_city(id)$cfg
#' @param id Registered city slug.
#' @param ... Arguments for the separate acquisition wrapper.
#' @return Acquisition result; an unsupported capability raises an error.
city_download <- function(id, ...) {
  mod <- get_city(id)
  if (is.null(mod$download)) stop("No acquisition wrapper registered for: ", id)
  mod$download(cfg = mod$cfg, ...)
}

#' @param id Registered city slug.
#' @param steps Processing stages; prerequisites run first.
#' @param inputs Named stage lists of preserved files or source directories.
#' @param quiet Suppress progress messages.
#' @return Named stage lists of all owned output paths.
city_process <- function(id, steps = c("geography", "stations_filter",
                                     "pollution_parquet", "census"),
                         inputs = NULL, quiet = FALSE) {
  mod <- get_city(id)
  mod$process(cfg = mod$cfg, steps = steps, inputs = inputs, quiet = quiet)
}

#' @param envir Environment receiving reusable functions and configurations.
#' @return Loaded city slugs invisibly; no acquisition or processing is executed.
load_city_modules <- function(envir = parent.frame()) {
  sys.source(here::here("src", "general_utilities", "process", "spatial_files.R"), envir)
  sys.source(here::here("src", "city_specific", "processing.R"), envir = envir)
  sys.source(here::here("src", "city_specific", "preparation.R"), envir = envir)
  for (id in c("bogota", "cdmx", "santiago", "sao_paulo")) {
    if (!id %in% list_cities()) {
      sys.source(here::here("src", "city_specific", paste0(id, ".R")), envir)
    }
  }
  invisible(list_cities())
}
