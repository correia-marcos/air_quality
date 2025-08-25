# ============================================================================================
# IDB: Air monitoring — Registry module
# ============================================================================================
# @Goal   : register + retrieve city modules (cfg + functions)
# @Date   : Aug 2025
# @Author : Marcos Paulo
# ============================================================================================

.city_registry <- new.env(parent = emptyenv())

register_city <- function(id, cfg, download = NULL, process = NULL,
                          read_raw = NULL, normalize = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  .city_registry[[id]] <- list(
    cfg       = cfg,
    download  = download,
    process   = process,
    read_raw  = read_raw,
    normalize = normalize
  )
  invisible(TRUE)
}

get_city <- function(id) {
  mod <- .city_registry[[id]]
  if (is.null(mod)) stop("Unknown city: ", id)
  mod
}

list_cities <- function() sort(ls(.city_registry))

# Convenience dispatchers (so scripts stay one-liners)
city_cfg     <- function(id)             get_city(id)$cfg
city_download<- function(id, ...)        get_city(id)$download(cfg = city_cfg(id), ...)
city_process <- function(id, ...)        get_city(id)$process(cfg = city_cfg(id), ...)