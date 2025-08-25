# ============================================================================================
# IDB: Air monitoring — Ciudad De México (CDMX) module
# ============================================================================================
# @Goal   : CDMX-specific parameters, download/process wrappers, and any site-specific code
# @Date   : Aug 2025
# @Author : Marcos Paulo
# Obs: Expect the caller to have already sourced:
#   - src/config_utils_download_data.R  (selenium helpers, waits, clicking helpers, etc.)
#   - src/config_utils_process_data.R   (merge, tidy, QA, parquet writing, etc.)
#   - src/cities/registry.R
# ============================================================================================

# Parameters (single source)

cdmx_cfg <- list(
  id       = "cdmx",
  tz       = "America/Mexico_City",
  years    = 2000L:2024L,
  dl_dir   = here::here("data", "downloads", "CDMX"),
  out_dir  = here::here("data", "raw",       "CDMX")
)

# ============================================================================================
#  CDMX-specific functions 
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: download_cdmx_pollution_data
# @Arg       : years       — integer vector; years to download (e.g. 2000:2024)
# @Arg       : dir         — string; directory to save files
# @Arg       : compressed  — logical; if TRUE downloads “.csv.gz”; otherwise “.csv”
# @Arg       : quiet       — logical; if TRUE, suppress download progress messages
# @Output    : tibble with columns:
#                 • year      (int)
#                 • file_path (chr; local path or NA on failure)
# @Purpose   : Download annual hourly pollution CSVs from CDMX open data (“contaminantes”).
#              Sources follow the pattern:
#              http://datosabiertos.aire.cdmx.gob.mx:8080/Opendata/anuales_horarios/
#                contaminantes_<YYYY>.csv
#              and the gz variant under .../anuales_horarios_gz/.
# @Written_on: 13/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
download_cdmx_pollution_data <- function(years,
                                         dir = ".",
                                         compressed = FALSE,
                                         quiet = FALSE) {
  # 1) validate inputs
  stopifnot(is.numeric(years), length(years) >= 1)
  years <- as.integer(years)
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  if (any(years < 1986L | years > this_year)) {
    stop("Years must be between 1986 and ", this_year, ".")
  }
  stopifnot(is.character(dir), length(dir) == 1L)
  stopifnot(is.logical(compressed), length(compressed) == 1L)
  stopifnot(is.logical(quiet), length(quiet) == 1L)
  
  # 2) prepare output directory
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  
  # 3) base URLs (don’t use file.path for URLs; separators differ on Windows)
  base_csv <- "http://datosabiertos.aire.cdmx.gob.mx:8080/Opendata/anuales_horarios"
  base_gz  <- "http://datosabiertos.aire.cdmx.gob.mx:8080/Opendata/anuales_horarios_gz"
  base     <- if (compressed) base_gz else base_csv
  ext      <- if (compressed) "csv.gz" else "csv"
  
  # tiny helper: retry download.file with backoff
  .dl_try <- function(url, dest, quiet, tries = 3) {
    for (k in seq_len(tries)) {
      err <- try(utils::download.file(url, destfile = dest, mode = "wb", quiet = quiet),
                 silent = TRUE)
      if (!inherits(err, "try-error")) return(TRUE)
      if (k < tries) Sys.sleep(min(30, 2^k))
    }
    FALSE
  }
  
  # 4) iterate years
  rows <- lapply(years, function(yr) {
    fname <- sprintf("contaminantes_%d.%s", yr, ext)
    url   <- sprintf("%s/%s", base, fname)
    dest  <- file.path(dir, fname)
    
    ok <- .dl_try(url, dest, quiet = quiet, tries = 3)
    if (!ok || !file.exists(dest)) {
      warning(sprintf("Failed to download %s", url), call. = FALSE)
      dest <- NA_character_
    } else {
      dest <- normalizePath(dest, winslash = "/", mustWork = FALSE)
    }
    
    list(year = yr, file_path = dest)
  })
  
  # 5) assemble output
  out <- tibble::tibble(
    year      = vapply(rows, `[[`, integer(1), "year"),
    file_path = vapply(rows, `[[`, character(1), "file_path")
  )
  message("Downloaded ", sum(!is.na(out$file_path)), " of ", nrow(out), " files.")
  out
}