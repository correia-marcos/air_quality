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
  id              = "Mexico City",
  tz              = "America/Mexico_City",
  base_url_shp    = "https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=794551132173",
  years           = 2000L:2023L,
  dl_dir          = here::here("data", "downloads", "CDMX"),
  out_dir         = here::here("data", "raw"),
  cities_in_metro = c(09002, 09003, 09004, 09005, 09006, 09007, 09008, 09009, 09010, 09011,
                      09012, 09013, 09014, 09015, 09016, 09017, 13013, 13069, 15002, 15011,
                      15013, 15020, 15022, 15023, 15024, 15025, 15028, 15029, 15030, 15031,
                      15033, 15035, 15037, 15038, 15039, 15044, 15046, 15053, 15057, 15058,
                      15059, 15060, 15069, 15070, 15075, 15081, 15083, 15084, 15089, 15091,
                      15092, 15093, 15095, 15099, 15100, 15103, 15104, 15108, 15109, 15120, 
                      15121, 15122, 15125)
  )

# ============================================================================================
#  CDMX-specific functions - downloading and its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: cdmx_download_metro_area
# @Arg       : base_url          — ficha URL (with ?upc=...) OR a direct .zip URL
# @Arg       : keep_municipality — vector of CVEGEO municipality keys to keep
# @Arg       : download_dir      — where to save the ZIP
# @Arg       : out_file          — where to write the cropped GeoPackage
# @Arg       : overwrite_zip     — logical; re-download if ZIP exists
# @Arg       : overwrite_gpkg    — logical; overwrite output GeoPackage if exists
# @Arg       : quiet             — logical; suppress progress
# @Output    : Writes a GeoPackage with CDMX metro municipalities; returns the sf (invisibly).
# @Purpose   : Download INEGI MGI 2024, read municipal layer, filter by CVEGEO, save.
# @Written_on: 02/09/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
cdmx_download_metro_area <- function(
    base_url          = cdmx_cfg$base_url_shp,
    keep_municipality = cdmx_cfg$cities_in_metro,
    download_dir      = here::here("data", "downloads", "Administrative", "Mexico"),
    out_file          = here::here("data", "raw", "admin", "Mexico", "cdmx_metro.gpkg"),
    overwrite_zip     = FALSE,
    overwrite_gpkg    = TRUE,
    quiet             = FALSE
) {
  # 0) Locale: make filename handling as UTF-8-friendly as possible
  Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  
  # 1) Guarantee folders exist
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  # 2) Normalize the target CVEGEO keys (digits only, character) and make all them 5 chr
  keep_chr <- gsub("[^0-9]", "", as.character(keep_municipality))
  keep_chr <- sprintf("%05s", keep_chr)
  
  # 3) Small helper to probe HTTP existence quickly
  .url_exists <- function(u) {
    ua <- httr::user_agent(
      sprintf("R (%s) / IDB-AirMonitoring",
              paste(R.version$platform, R.version$version.string))
    )
    resp <- try(
      httr::RETRY("HEAD", u, ua, times = 3, quiet = TRUE, httr::timeout(20)),
      silent = TRUE
    )
    if (inherits(resp, "try-error")) return(FALSE)
    sc <- httr::status_code(resp)
    isTRUE(sc >= 200 && sc < 400)
  }
  
  # 4) Resolve ficha page → direct integrated ZIP URL (or accept direct .zip)
  get_zip_url <- function(u) {
    if (grepl("\\.zip$", u, ignore.case = TRUE)) return(u)
    
    base_host <- "https://www.inegi.org.mx"
    if (!quiet) message("🔎 Scraping ficha page for integrated ZIP…")
    
    upc <- {
      m <- regexpr("upc=([0-9]+)", u, perl = TRUE)
      if (m > 0) sub(".*upc=([0-9]+).*", "\\1", u) else NA_character_
    }
    
    href <- character(0)
    pg <- try(xml2::read_html(u), silent = TRUE)
    if (!inherits(pg, "try-error")) {
      href <- rvest::html_attr(rvest::html_elements(pg, "a[href]"), "href")
      href <- href[!is.na(href)]
    }
    
    j <- grep("integrado\\.zip$", href, ignore.case = TRUE)
    if (length(j)) {
      rel <- href[j[1]]
      return(if (grepl("^https?://", rel)) rel else paste0(base_host, rel))
    }
    
    if (!is.na(upc) && nzchar(upc)) {
      guess <- paste0(
        base_host, "/contenidos/productos/prod_serv/contenidos/",
        "espanol/bvinegi/productos/geografia/marcogeo/",
        upc, "/mg_2024_integrado.zip"
      )
      if (.url_exists(guess)) return(guess)
    }
    
    stop("Could not locate 'integrado.zip'. Pass a direct .zip URL.")
  }
  
  zip_url  <- get_zip_url(base_url)
  zip_name <- basename(zip_url)
  zip_path <- file.path(download_dir, zip_name)
  
  # 5) Download integrated ZIP (robust with retry)
  if (!file.exists(zip_path) || isTRUE(overwrite_zip)) {
    if (!quiet) message("⬇️  Downloading: ", zip_url)
    ua <- httr::user_agent(
      sprintf("R (%s) / IDB-AirMonitoring",
              paste(R.version$platform, R.version$version.string))
    )
    req <- httr::RETRY(
      "GET", zip_url, ua,
      httr::write_disk(zip_path, overwrite = TRUE),
      httr::progress(type = if (quiet) "none" else "down"),
      times = 5, terminate_on = c(200L), quiet = quiet, httr::timeout(60*30)
    )
    if (httr::status_code(req) != 200L) {
      stop("HTTP ", httr::status_code(req), " downloading: ", zip_url)
    }
    if (!quiet) message("✅ Saved: ", zip_path)
  } else if (!quiet) {
    message("↪︎ ZIP already present (skip): ", zip_path)
  }
  
  # 6) Prepare extraction directory
  exdir <- file.path(tempdir(), tools::file_path_sans_ext(zip_name))
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  
  # 7) List entries (avoid extracting files with bad encodings)
  list_names <- NULL
  lst <- try(utils::unzip(zip_path, list = TRUE), silent = TRUE)
  if (!inherits(lst, "try-error")) {
    list_names <- lst$Name
  } else {
    zz <- try(
      system2("unzip", c("-Z", "-1", zip_path), stdout = TRUE, stderr = TRUE),
      silent = TRUE
    )
    if (!inherits(zz, "try-error")) list_names <- zz
  }
  if (is.null(list_names) || !length(list_names)) {
    stop("Could not list ZIP entries (encoding issue?).")
  }
  
  # 8) Select only municipal shapefile parts under 'conjunto_de_datos'
  mun_pat  <- "(?i)(^|/)conjunto_de_datos/[^/]*/?0?0?mun\\."
  mun_pat  <- paste0(mun_pat, "(shp|dbf|shx|prj|cpg)$")
  sel <- grep(mun_pat, list_names, perl = TRUE, useBytes = TRUE)
  if (!length(sel)) {
    mun_pat2 <- "(?i)(^|/)conjunto_de_datos/0?0?mun\\."
    mun_pat2 <- paste0(mun_pat2, "(shp|dbf|shx|prj|cpg)$")
    sel <- grep(mun_pat2, list_names, perl = TRUE, useBytes = TRUE)
  }
  if (!length(sel)) {
    stop("No municipal files (*mun.{shp,dbf,shx,prj,cpg}) found in ZIP.")
  }
  need_files <- list_names[sel]
  
  # 9) Extract only municipal files (fallback to system 'unzip' if needed)
  ext_try <- try(
    utils::unzip(zipfile = zip_path, exdir = exdir,
                 files = need_files, overwrite = TRUE),
    silent = TRUE
  )
  if (inherits(ext_try, "try-error")) {
    args <- c("-qq", zip_path, need_files, "-d", exdir)
    z2 <- try(system2("unzip", args, stdout = TRUE, stderr = TRUE),
              silent = TRUE)
    if (inherits(z2, "try-error")) {
      stop("Failed to extract municipal shapefile from ZIP (encoding).")
    }
  }
  
  # 10) Locate the .shp path and read as sf
  shp_rel  <- need_files[grepl("(?i)\\.shp$", need_files, useBytes = TRUE)]
  if (!length(shp_rel)) stop("SHP part of municipal layer not found.")
  shp_path <- file.path(exdir, shp_rel[1])
  
  g <- suppressMessages(
    sf::st_read(shp_path, quiet = TRUE, stringsAsFactors = FALSE)
  )
  if (!"CVEGEO" %in% names(g)) {
    stop("Expected column 'CVEGEO' not found in the municipal layer.")
  }
  
  # 11) Filter by CVEGEO
  g <- dplyr::mutate(
    g, CVEGEO = gsub("[^0-9]", "", as.character(.data$CVEGEO))
  )
  g_sel <- dplyr::filter(g, .data$CVEGEO %in% keep_chr)
  if (nrow(g_sel) == 0L) {
    stop("Filter produced 0 rows. Check your keep_municipality CVEGEO values.")
  }
  
  # 12) Write GeoPackage
  if (file.exists(out_file) && !overwrite_gpkg) {
    if (!quiet) message("↪︎ Output exists and overwrite_gpkg=FALSE: ", out_file)
  } else {
    if (!quiet) message("💾 Writing GeoPackage → ", out_file)
    if (file.exists(out_file) && overwrite_gpkg) unlink(out_file, force = TRUE)
    suppressMessages(sf::st_write(g_sel, out_file, quiet = TRUE))
  }
  
  # 13) Friendly summary
  if (!quiet) {
    message("✅ Done. Selected ", nrow(g_sel), " municipalities.")
  }
  
  invisible(g_sel)
}


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
