# ============================================================================================
# IDB: Air monitoring — Bogotá module
# ============================================================================================
# @Goal   : Bogotá-specific parameters, download/process wrappers, and any site-specific code
# @Date   : Aug 2025
# @Author : Marcos Paulo
# Obs: Expect the caller to have already sourced:
#   - src/config_utils_download_data.R  (selenium helpers, waits, clicking helpers, etc.)
#   - src/config_utils_process_data.R   (merge, tidy, QA, parquet writing, etc.)
#   - src/cities/registry.R
# 
# Others obs:
# Definition of the metropolitan area comes from SDP (2022):
# — Secretaría Distrital de Planeacion (2022). Bogotá región: Un solo territorio. Dirección de 
# Integración Regional, Nacional e Internacional.
# ============================================================================================

# Parameters (single source)
bogota_cfg <- list(
  id              = "bogota",
  tz              = "America/Bogota",
  url_station_shp = "https://www.ambientebogota.gov.co/estaciones-rmcab",
  base_url_shp    = "https://www.dane.gov.co/files/geoportal-provisional",
  base_url_rmcab  = "http://rmcab.ambientebogota.gov.co/Report/stationreport",
  base_url_census = "https://microdatos.dane.gov.co/index.php/catalog/421/get-microdata",
  years           = 2000L:2023L,
  dl_dir          = here::here("data", "downloads", "Bogota"),
  out_dir         = here::here("data", "raw"),
  cities_in_metro = c("Bogotá DC", "Bojacá", "Cajicá", "Chía", "Cota", "El Rosal", "Facatativá",
                      "Funza", "Fusagasugá", "Gachancipá", "La Calera", "Madrid", "Mosquera",
                      "Sibaté", "Soacha", "Sopó", "Subachoque", "Tabio", "Tenjo", "Tocancipá",
                      "Zipaquirá"),
  station_nme_map = c("Centro de alto rendimiento" = "Centro de Alto Rendimiento",
                      "Las Ferias"                 = "Las Ferias",
                      "Carvajal-Sevillana"         = "Carvajal-Sevillana")
)

# ============================================================================================
#  Bogotá-specific functions - downloading and its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: bogota_download_metro_area
# @Arg       : level             — "mpio" (default) or "depto"
# @Arg       : base_url          — base URL of DANE provisional geoportal files
# @Arg       : keep_municipality — character vector of municipality names to keep
#                                  (e.g., bogota_cfg$cities_in_metro)
# @Arg       : download_dir      — where to save the ZIP 
#                                  (default: data/downloads/Administrative/Colombia)
# @Arg       : out_file          — where to write the cropped GeoPackage
# @Arg       : overwrite_zip     — logical; re-download if ZIP exists (default FALSE)
# @Arg       : overwrite_gpkg    — logical; overwrite output GeoPackage if exists 
#                                  (default TRUE)
# @Arg       : quiet             — logical; suppress progress (default FALSE)
# @Output    : Writes a GeoPackage with Bogotá metro municipalities; returns (invisibly) the 
#              sf object.
# @Purpose   : Download MGN2018 admin boundaries and crop to the Bogotá metro area 
# municipalities.
# @Written_on: 20/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_download_metro_area <- function(
    level             = c("mpio", "depto"),
    base_url          = bogota_cfg$base_url_shp,
    keep_municipality = bogota_cfg$cities_in_metro,
    download_dir      = here::here("data", "downloads", "Administrative", "Colombia"),
    out_file          = here::here("data", "raw", "admin", "Colombia", "bogota_metro.gpkg"),
    overwrite_zip     = FALSE,
    overwrite_gpkg    = TRUE,
    quiet             = FALSE
) {
  level <- match.arg(tolower(level), c("mpio","depto"))
  
  # 0) Bogotá metro municipalities (canonical names) from config
  bogota_metro_names <- bogota_cfg$cities_in_metro
  
  # Small helper: normalize Spanish names → ASCII, upper, strip non-alphanumeric
  norm_key <- function(x) {
    x <- as.character(x)
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- toupper(x)
    gsub("[^A-Z0-9]", "", x)
  }
  bogota_key <- norm_key(bogota_metro_names)
  # Treat BOGOTA D.C. variants
  bogota_key <- unique(c(bogota_key, norm_key("Bogota D.C."), norm_key("Bogota DC")))
  
  # 1) Choose ZIP by level
  zip_name <- if (level == "mpio"){
    "SHP_MGN2018_INTGRD_MPIO.zip"
  } else {
      "SHP_MGN2018_INTGRD_DEPTO.zip"
    }
  zip_url  <- file.path(base_url, zip_name)
  
  # 2) Ensure folders
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- dirname(out_file)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(download_dir, zip_name)
  
  # 3) Download (robust)
  if (!file.exists(zip_path) || isTRUE(overwrite_zip)) {
    if (!quiet) message("⬇️  Downloading ", zip_name, " …")
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
  
  # 4) Extract to a temp folder and locate the SHP
  exdir <- file.path(tempdir(), tools::file_path_sans_ext(zip_name))
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zipfile = zip_path, exdir = exdir, overwrite = TRUE)
  shp <- list.files(exdir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  if (length(shp) == 0) stop("No .shp found inside ZIP: ", zip_path)
  shp <- shp[1]
  
  # 5) Read with sf
  suppressMessages({
    g <- sf::st_read(shp, quiet = TRUE, stringsAsFactors = FALSE)
  })
  if (!quiet) message("🗺️  Loaded layer: ", basename(shp), "  (", nrow(g), " features)")

  # ---- CHANGED: fixed name column + code-based prefilter to resolve duplicates ----
  # Prefer the known fields if present
  has_name  <- "MPIO_CNMBR"   %in% names(g)
  has_dpto  <- "DPTO_CCDGO"   %in% names(g)
  
  # 6) Municipio-level selection
  if (level == "mpio") {
    if (!has_name) {
      stop("Expected column 'MPIO_CNMBR' not found in the municipio layer.")
    }
    
    # Coerce department code to integer safely (for filtering)
    if (!has_dpto) {
      stop("Expected column 'DPTO_CCDGO' not found; cannot disambiguate duplicate names.")
    }
    dpto_code <- suppressWarnings(as.integer(g[["DPTO_CCDGO"]]))
    
    # Keep only Bogotá D.C. (11) + Cundinamarca (25) to avoid name collisions like MOSQUERA (52)
    keep <- !is.na(dpto_code) & dpto_code %in% c(11L, 25L)
    g2 <- g[keep, , drop = FALSE]
    
    # Normalize names and match
    nm_values <- iconv(g2[["MPIO_CNMBR"]], from = "", to = "UTF-8")
    key_col   <- norm_key(nm_values)
    
    sel <- key_col %in% bogota_key
    g_sel <- g2[sel, , drop = FALSE]
    
    if (nrow(g_sel) == 0) {
      stop(
        "No municipalities matched within DPTO 11/25. ",
        "Examples: ", paste(utils::head(unique(nm_values), 8), collapse = " | "),
        "\nCheck accents/punctuation or update bogota_cfg$cities_in_metro."
      )
    }
    if (!quiet) {
      message("🔎 Matched ",
              nrow(g_sel),
              " / ",
              nrow(g),
              " features (post-filter to DPTO 11/25).")
    }
    
  } else {
    # 7) Departamento-level: just return Bogotá D.C. (11) + Cundinamarca (25)
    if (!has_dpto) {
      stop("Expected column 'DPTO_CCDGO' not found in the department layer.")
    }
    dpto_code <- suppressWarnings(as.integer(g[["DPTO_CCDGO"]]))
    sel <- !is.na(dpto_code) & dpto_code %in% c(11L, 25L)
    g_sel <- g[sel, , drop = FALSE]
    if (nrow(g_sel) == 0) {
      stop("Departamento match failed for codes 11 and 25.")
    }
    if (!quiet) {
      message("🔎 Selected departamentos by code: 11 (Bogotá D.C.) + 25 (Cundinamarca).")
    }
  }
  
  # 8) (Optional) dissolve to single polygon if desired
  # g_out <- sf::st_union(g_sel)  # <- uncomment if you want one multipart feature
  g_out <- g_sel
  
  # 9) Write GeoPackage
  if (file.exists(out_file) && !overwrite_gpkg) {
    if (!quiet) message("↪︎ Output exists and overwrite_gpkg=FALSE: ", out_file)
  } else {
    if (!quiet) message("💾 Writing GeoPackage → ", out_file)
    if (file.exists(out_file) && overwrite_gpkg) unlink(out_file, force = TRUE)
    sf::st_write(g_out, out_file, quiet = TRUE)
  }
  
  invisible(g_out)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_scrape_rmcab_station_table
# @Arg       : page_url        — string; the url of the page to scrape
# @Arg       : parse_coords    — logical; parse DMS lat/lon to decimal degrees
#                                (default TRUE)
# @Arg       : harmonize_map   — named chr vec (optional) to rename station display names
# @Arg       : dedupe          — logical; drop duplicate rows across repeated tables
#                                (default TRUE)
# @Arg       : verbose         — logical; print progress
#                                (default TRUE)
# @Arg       : out_dir         — string; directory to write outputs (created if missing)
# @Arg       : out_name        — string; base filename *without* extension
# @Arg       : write_rds       — logical; write .rds (default FALSE)
# @Arg       : write_parquet   — logical; write .parquet via {arrow} (default TRUE)
# @Arg       : write_csv       — logical; write .csv (default FALSE)
# @Output    : tibble with columns:
#                station, code, latitude_dms, longitude_dms, lat, lon,
#                altitude_m, height_m, locality, zone_type, station_type, address
# @Purpose   : Scrape RMCAB station directory robustly (nested tables, repeated headers),
#              and return clean metadata with decimal coords. Also save the table in the
#              required formats and location. 
# @Written_on: 29/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_scrape_rmcab_station_table <- function(
    page_url,
    parse_coords  = TRUE,
    harmonize_map = c(),
    dedupe        = TRUE,
    verbose       = TRUE,
    out_dir,
    out_name,
    write_rds     = FALSE,
    write_parquet = TRUE,
    write_csv     = FALSE
) {
  # Ensure output folder exists
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- helpers -----------------------------------------------------------------
  .clean_txt <- function(x) {
    x %>%
      stringr::str_replace_all("\u00A0", " ") %>%      # nbsp → space
      stringr::str_replace_all("[’′]", "'") %>%        # curly/prime apostrophes → '
      stringr::str_replace_all("[“”]", "\"") %>%       # fancy quotes → "
      stringr::str_squish() %>%
      trimws()
  }
  
  .parse_dms_vec <- function(x) {
    x <- .clean_txt(x)
    # e.g., 4°47'01.5"N | 74° 5'36.46"W | 4°44'13.9"N (seconds optional)
    re <- "^\\s*(\\d{1,3})\\D+(\\d{1,2})\\D*(\\d{1,2}(?:[\\.,]\\d+)?)?\\D*([NSEWnsew])\\s*$"
    m  <- stringr::str_match(x, re)
    out <- rep(NA_real_, length(x))
    ok  <- !is.na(m[,1])
    if (any(ok)) {
      deg <- as.numeric(m[ok,2])
      min <- as.numeric(m[ok,3])
      sec <- suppressWarnings(as.numeric(stringr::str_replace(m[ok,4], ",",".")))
      sec[is.na(sec)] <- 0
      dec <- deg + min/60 + sec/3600
      sign <- ifelse(m[ok,5] %in% c("S","s","W","w"), -1, 1)
      out[ok] <- sign * dec
    }
    out
  }
  
  .is_header_row <- function(vals10) {
    # second header row has exactly these labels (case/accents vary)
    labs <- tolower(paste(vals10, collapse = " | "))
    any(
      stringr::str_detect(labs, "caracter") &&
        stringr::str_detect(labs, "sigla")     &&
        stringr::str_detect(labs, "latitud")   &&
        stringr::str_detect(labs, "longitud")
    )
  }
  
  # ---- scrape ------------------------------------------------------------------
  if (isTRUE(verbose)) message("Fetching: ", page_url)
  # 1) Read the html
  doc <- xml2::read_html(page_url)
  
  # 2) Find tables whose text mentions the title
  tbls <- rvest::html_elements(doc, "table")
  has_title <- purrr::map_lgl(tbls, ~ stringr::str_detect(
    tolower(rvest::html_text(.x)), "ubicaci[oó]n\\s+estaciones\\s+rmcab"))
  candidates <- tbls[has_title]
  if (!length(candidates)) stop("No matching table found on the page.")
  
  if (isTRUE(verbose)) message("Found ", length(candidates), " candidate table(s). Parsing…")
  
  # 3) Map candidates of the information we want in the url
  rows_all <- purrr::map(candidates, function(tbl) {
    # Take only DIRECT child tds to avoid nested tables
    trs <- rvest::html_elements(tbl, xpath = ".//tr")
    purrr::map(trs, function(tr) {
      tds <- rvest::html_elements(tr, xpath = "./td")
      vals <- rvest::html_text2(tds)
      vals <- .clean_txt(vals)
      # Keep only data rows with exactly 10 direct cells
      if (length(vals) != 10) return(NULL)
      if (.is_header_row(vals)) return(NULL)   # skip label header
      # Also skip the single-cell title row handled above (won't pass length==10 anyway)
      vals
    }) %>% purrr::compact()
  }) %>% purrr::list_flatten()
  
  if (!length(rows_all)) stop("Parsed 0 data rows — the site may have changed.")
  
  # 4) Generate a table with the information collected
  df <- purrr::map_dfr(rows_all, function(vals) {
    tibble::tibble(
      station       = vals[[1]],
      code          = vals[[2]],
      latitude_dms  = vals[[3]],
      longitude_dms = vals[[4]],
      altitude_m    = vals[[5]],
      height_m      = vals[[6]],
      locality      = vals[[7]],
      zone_type     = vals[[8]],
      station_type  = vals[[9]],
      address       = vals[[10]]
    )
  })
  
  # 5) Clean/convert numbers from the table
  df <- df %>%
    dplyr::mutate(
      altitude_m = readr::parse_number(altitude_m),
      height_m   = readr::parse_number(height_m)
    )
  
  # 6) Harmonize names if requested
  if (length(harmonize_map)) {
    df$station <- dplyr::recode(df$station, !!!harmonize_map, .default = df$station)
  }
  
  # 7) Generate new columns with Coordinates in better units
  if (isTRUE(parse_coords)) {
    df <- df %>% dplyr::mutate(lat = .parse_dms_vec(latitude_dms),
                               lon = .parse_dms_vec(longitude_dms))
    bad <- which(is.na(df$lat) | is.na(df$lon))
    if (length(bad) && isTRUE(verbose)) {
      message("⚠️  Could not parse coords for rows: ",
              paste(bad, collapse = ", "),
              " (station(s): ", paste(df$station[bad], collapse = "; "), ")")
    }
  } else {
    df <- df %>% dplyr::mutate(lat = NA_real_, lon = NA_real_)
  }
  
  # 8) De-duplicate duplicate tables
  if (isTRUE(dedupe)) {
    df <- df %>%
      dplyr::distinct(station, code, latitude_dms, longitude_dms, .keep_all = TRUE)
  }
  
  # 9) Select and arrange the table with the needed information
  df %>%
    dplyr::select(station, code, latitude_dms, longitude_dms, lat, lon,
                  altitude_m, height_m, locality, zone_type, station_type, address) %>%
    dplyr::arrange(station) %>%
    { if (isTRUE(verbose))
      message("Rows: ", nrow(.), " | Unique stations: ",
              dplyr::n_distinct(.$station), " | With coords: ",
              sum(!is.na(.$lat) & !is.na(.$lon))); .
    }
  # 10) Save table in the requested formats - first, write paths
  paths <- list(rds = NA_character_, parquet = NA_character_, csv = NA_character_)
  
  if (isTRUE(write_rds)) {
    rds_path <- file.path(out_dir, paste0(out_name, ".rds"))
    saveRDS(df, rds_path, compress = "xz")
    paths$rds <- normalizePath(rds_path, winslash = "/", mustWork = FALSE)
    if (verbose) message("💾 Wrote RDS → ", paths$rds)
  }
  
  if (isTRUE(write_parquet)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for Parquet output.
           Install it (e.g., renv::install('arrow')).")
    }
    pq_path <- file.path(out_dir, paste0(out_name, ".parquet"))
    arrow::write_parquet(df, pq_path, compression = "zstd")
    paths$parquet <- normalizePath(pq_path, winslash = "/", mustWork = FALSE)
    if (verbose) message("🧱 Wrote Parquet → ", paths$parquet)
  }
  
  if (isTRUE(write_csv)) {
    csv_path <- file.path(out_dir, paste0(out_name, ".csv"))
    write.csv(df, file = csv_path, row.names = FALSE)
    paths$csv <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)
    if (verbose) message("📝 Wrote CSV.GZ → ", paths$csv)
  }
  
  return(df)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_get_station_info
# @Arg       : base_url      — string; URL of the station‐report form page
# @Output    : tibble with columns:
#                 • stationId   (chr)
#                 • DisplayName (chr)
#                 • monitors    (list‐col; each entry a list of monitor‐objects)
# @Purpose   : scrape the page’s <script> blocks, extract *every*
#              `all_stations = […]` assignment, take the *last* (full) JSON,
#              parse it, and return station metadata.
# @Written_on: 20/05/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_get_station_info <- function(base_url) {
  # 1) grab all the embedded script text
  all_js <- read_html(base_url) %>%
    html_nodes("script") %>%
    html_text() %>%
    paste(collapse = "\n")
  
  # 2) find *every* `all_stations = [...] ;` and capture the [...]
  pat     <- "(?s)all_stations\\s*=\\s*(\\[.*?\\])\\s*;"
  matches <- str_match_all(all_js, pat)[[1]]
  
  # 3) must have at least two: an initial "empty" and the real payload
  if (nrow(matches) < 2) {
    stop("Couldn’t find a second `all_stations = […]` block. Has the site changed?")
  }
  
  # 4) Pick the Last JSON block the true payload is in the last match, second column
  json_txt <- matches[nrow(matches), 2]
  
  # 5) parse JSON → list of station‐objects
  stations_list <- jsonlite::fromJSON(json_txt, simplifyDataFrame = FALSE)
  
  # 6) build and return a tibble
  out <- tibble(
    stationId   = map_chr(stations_list, ~ as.character(.x$stationId)),
    DisplayName = map_chr(stations_list, ~ as.character(.x$DisplayName)),
    monitors    = map(stations_list, "monitors"),
    location    = map(stations_list, "location"),
    owner       = map_chr(stations_list, ~ as.character(.x$owner))
  )
  
  message("Found ", nrow(out), " stations: ", paste(out$DisplayName, collapse = ", "))
  return(out)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_download_station_data
# @Arg       : base_url       — string; URL of the station‐report form page
# @Arg       : start_year     — integer; first year to download (e.g. 2000)
# @Arg       : end_year       — integer; last year to download (e.g. 2023)
# @Arg       : container      — logical; TRUE if running inside Docker/Selenium compose
# @Arg       : stations_idx   — integer vector|NULL; which <li> indices to download (NULL=all)
# @Arg       : max_attempts   — integer; retries per (station, year) (default 3)
# @Arg       : timeout_page   — integer; seconds to wait page ready (default 30)
# @Arg       : timeout_btn    — integer; seconds to wait buttons visible (default 30)
# @Arg       : timeout_dl     — integer; seconds to wait per download (default 240)
# @Arg       : subdir         — string|NULL; if provided, move each finished file into this
#                                subfolder under downloads_folder ("Ground_stations/Bogota")
# @Output    : writes XLSX files using the site’s random filenames; returns (invisibly)
#              a log tibble with columns: station, year, part, status, file
# @Purpose   : Same as before; also optionally moves finished files into a city-specific folder.
#              For each year it also downloads the missing last day:
#              31-12-yr 00:00 → 01-01-(yr+1) 00:00.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_download_station_data <- function(base_url,
                                         start_year,
                                         end_year,
                                         container     = TRUE,
                                         stations_idx  = NULL,
                                         max_attempts  = 3,
                                         timeout_page  = 30,
                                         timeout_btn   = 30,
                                         timeout_dl    = 240,
                                         subdir        = NULL) {
  
  # 0) target folder (host-mapped root where Selenium drops files)
  downloads_folder <- Sys.getenv("DOWNLOADS_DIR", here::here("data","downloads"))
  dir.create(downloads_folder, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Downloads dir: ", downloads_folder)
  
  # NEW: optional target subfolder for organization
  target_dir <- if (!is.null(subdir)) {
    td <- file.path(downloads_folder, subdir)
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    message("📂 Target subdir : ", td)
    td
  } else {
    NULL
  }
  
  # 1) Define selenium endpoint
  if (!container) {
    message("🚀 Starting local Selenium on 4445…")
    cid <- system(
      "docker run -d -p 4445:4444 --shm-size=2g selenium/standalone-firefox:4.34.0-20250717",
      intern = TRUE
    )
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern = TRUE),
                silent = TRUE),
            add = TRUE)
    selenium_host <- "localhost"; selenium_port <- 4445L
  } else {
    selenium_host <- "selenium";  selenium_port <- 4444L
  }
  
  # 2) Define browser preferences
  download_dir_container <- if (container) "/home/seluser/Downloads" else downloads_folder
  caps <- list(
    browserName = "firefox",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList"     = 2L,
        "browser.download.dir"            = download_dir_container,
        "browser.download.useDownloadDir" = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" =
          paste(
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            "application/octet-stream",
            sep = ","
          )
      )
    ),
    timeouts = list(implicit = 0L, pageLoad = 60000L, script = 60000L)
  )
  
  # 3) session
  session <- selenium::SeleniumSession$new(
    browser      = "firefox",
    host         = selenium_host,
    port         = selenium_port,
    capabilities = caps
  )
  on.exit(session$close(), add = TRUE)
  
  # 4) open once and discover stations
  session$navigate(base_url)
  wait_ready(session, timeout_page)
  ul <- wait_for(session, "css selector", "#StationsMonitorsList > ul", timeout_btn)
  items <- ul$find_elements("css selector", "li.k-item")
  count <- length(items)
  if (is.null(stations_idx)) stations_idx <- seq_len(count)
  
  # helper to get station name at index i
  read_station_name <- function(i) {
    ul2 <- wait_for(session, "css selector", "#StationsMonitorsList > ul", timeout_btn)
    lis <- ul2$find_elements("css selector", "li.k-item")
    lis[[i]]$get_text()[[1]]
  }
  
  # 5) loop stations
  log <- list()
  
  for (i in stations_idx) {
    # re-open fresh for stability on each station
    session$navigate(base_url)
    wait_ready(session, timeout_page)
    station_name <- read_station_name(i)
    message(sprintf("\n📍 Station [%d/%d]: %s", i, count, station_name))
    
    # Iterate through years
    for (yr in seq(as.integer(start_year), as.integer(end_year))) {
      # --------------------------- YEAR RANGE DOWNLOAD ---------------------------
      attempt <- 0L
      repeat {
        attempt <- attempt + 1L
        message(sprintf("   📥 Jan 01 %d (attempt %d/%d)…",
                        yr, attempt, max_attempts))
        newest_path2 <- NA_character_
        try({
          # reload base page each attempt (avoids stale state)
          session$navigate(base_url)
          wait_ready(session, timeout_page)
          
          # Report period → Personalizado
          wait_for(session, "css selector",
                   "#select-reportperiod > li:nth-child(6)", timeout_btn)$click()
          
          # Personalized period tab is already selected at this point
          click_date_input(session, "startDate", 1, 1,  yr)
          click_time_input(session, "startTime", 0, 0)
          click_end_date(session, year = yr, day = 31, month = 12)
          click_time_input(session, "endTime",   23, 0)
          # Safety: re-apply end time to prevent the "snap back to 01:00" bug
          click_time_input(session, "endTime",   23, 0)
          
          # Find station id
          lis  <- wait_for(session, "css selector", "#StationsMonitorsList > ul", timeout_btn)$
            find_elements("css selector", "li.k-item")
          item <- lis[[i]]
          chk  <- item$find_element("xpath", ".//input[contains(@class,'k-checkbox')]")
          cb_id <- chk$get_attribute("id")[[1]]
          
          # Ensure station checkbox ON
          js_toggle <- sprintf(
            "var cb = document.getElementById('%s');
             cb.scrollIntoView({block:'center'});
             cb.checked = !cb.checked;
             cb.dispatchEvent(new Event('change',{bubbles:true}));",
            cb_id)
          session$execute_script(js_toggle)
          
          # Click again on the last day of data - server bug
          click_end_date(session, year = yr, day = 31, month = 12)
          
          # Snapshot current files, then click
          before <- list.files(
            downloads_folder,
            pattern = "^StationsReport.*\\.xlsx$",
            full.names = TRUE
          )
          
          # Mostrar → Excel
          wait_for(session, "xpath", '//*[@id="buttonsWrapper"]/input[2]', timeout_btn)$click()
          robust_click_excel(session, timeout_btn)
          
          # Wait for a NEW file, then (optionally) move it into subdir
          src <- wait_for_new_download(
            downloads_folder,
            before   = before,
            pattern  = "^StationsReport.*\\.xlsx$",
            quiet_sec= 2,
            timeout  = timeout_dl
          )
          if (!is.null(target_dir)) {
            dest <- file.path(target_dir, basename(src))
            ok_mv <- try(file.rename(src, dest), silent = TRUE)
            if (inherits(ok_mv, "try-error") || !isTRUE(ok_mv)) {
              file.copy(src, dest, overwrite = TRUE); unlink(src)
            }
            newest_path2 <- dest
          } else {
            newest_path2 <- src
          }
          message("      ✅ ", basename(newest_path2))
        }, silent = TRUE) -> res2
        
        if (!inherits(res2, "try-error")) {
          log[[length(log) + 1L]] <- tibble::tibble(
            station = station_name, year = yr, part = "year",   # FIXED label
            status  = "ok",         file = newest_path2
          )
          break
        } else if (attempt < max_attempts) {
          back <- min(30, 2 ^ attempt)
          message(sprintf("      ⚠️  Failed; backoff %ds, retrying…", back))
          Sys.sleep(back)
        } else {
          message("      ❌ Failed after max attempts (year).")
          log[[length(log) + 1L]] <- tibble::tibble(
            station = station_name, year = yr, part = "year",
            status  = "failed",     file = NA_character_
          )
          break
        }
      }
      
      # ------------------------ EXTRA DAY (DEC 31) DOWNLOAD ----------------------
      attempt <- 0L
      repeat {
        attempt <- attempt + 1L
        message(sprintf("   📥 Dec 31 %d (attempt %d/%d)…",
                        yr, attempt, max_attempts))
        newest_path2 <- NA_character_
        try({
          session$navigate(base_url)
          wait_ready(session, timeout_page)
          
          wait_for(session, "css selector",
                   "#select-reportperiod > li:nth-child(6)", timeout_btn)$click()
          
          # Find station id again
          lis  <- wait_for(session, "css selector", "#StationsMonitorsList > ul", timeout_btn)$
            find_elements("css selector", "li.k-item")
          item <- lis[[i]]
          chk  <- item$find_element("xpath", ".//input[contains(@class,'k-checkbox')]")
          cb_id <- chk$get_attribute("id")[[1]]
          
          # Ensure station checkbox ON
          js_toggle <- sprintf(
            "var cb = document.getElementById('%s');
             cb.scrollIntoView({block:'center'});
             cb.checked = !cb.checked;
             cb.dispatchEvent(new Event('change',{bubbles:true}));",
            cb_id)
          session$execute_script(js_toggle)
          
          # 31-12-yr 00:00 to 01-01-(yr+1) 00:00
          click_date_input(session, "startDate", 31, 12, yr)
          click_time_input(session, "startTime", 0,  0)
          click_date_input(session, "endDate",    1,  1, yr + 1L)
          click_time_input(session, "endTime",    0,  0)
          
          # Safety re-apply
          click_time_input(session, "endTime",    0,  0)
          click_date_input(session, "endDate",    1,  1, yr + 1L)
          
          # Snapshot current files
          before <- list.files(
            downloads_folder,
            pattern = "^StationsReport.*\\.xlsx$",
            full.names = TRUE
          )
          
          # Mostrar → Excel
          wait_for(session, "xpath", '//*[@id="buttonsWrapper"]/input[2]', timeout_btn)$click()
          robust_click_excel(session, timeout_btn)
          
          src <- wait_for_new_download(
            downloads_folder,
            before   = before,
            pattern  = "^StationsReport.*\\.xlsx$",
            quiet_sec= 2,
            timeout  = timeout_dl
          )
          if (!is.null(target_dir)) {
            dest <- file.path(target_dir, basename(src))
            ok_mv <- try(file.rename(src, dest), silent = TRUE)
            if (inherits(ok_mv, "try-error") || !isTRUE(ok_mv)) {
              file.copy(src, dest, overwrite = TRUE); unlink(src)
            }
            newest_path2 <- dest
          } else {
            newest_path2 <- src
          }
          message("      ✅ ", basename(newest_path2))
        }, silent = TRUE) -> res2
        
        if (!inherits(res2, "try-error")) {
          log[[length(log)+1]] <- tibble::tibble(
            station = station_name, year = yr, part = "dec31",
            status  = "ok", file = newest_path2
          )
          break
        } else if (attempt < max_attempts) {
          back <- min(30, 2 ^ attempt)
          message(sprintf("      ⚠️  Failed; backoff %ds, retrying…", back))
          Sys.sleep(back)
        } else {
          message("      ❌ Failed after max attempts (dec31).")
          log[[length(log)+1]] <- tibble::tibble(
            station = station_name, year = yr, part = "dec31",
            status  = "failed", file = NA_character_
          )
          break
        }
      }
      # --------------------------------------------------------------------------
    }
  }
  
  out <- dplyr::bind_rows(log)
  invisible(out)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_find_resource_census
# @Arg       : url    — string; DANE catalog page. Works with either:
#                        "https://microdatos.dane.gov.co/index.php/catalog/421/get-microdata"
#                        or "https://microdatos.dane.gov.co/index.php/catalog/421"
# @Arg       : type   — "BASICO" or "AMPLIADO"
# @Output    : list(label, filename, href) for the requested resource
# @Purpose   : Extract the 'mostrarModal("FILE.zip","https://.../download/ID")' link.
#              We resolve relative → absolute URLs and trim whitespace.
# @Written_on: 21/08/2025
# --------------------------------------------------------------------------------------------
bogota_find_resource_census <- function(url, type = c("BASICO","AMPLIADO")) {
  type <- match.arg(toupper(type), c("BASICO","AMPLIADO"))
  
  # Try the provided URL; if it ends with /get-microdata, also try the base catalog
  base_try <- sub("/get-microdata/?$", "", url)
  candidates <- unique(c(url, base_try))
  
  # Helper to parse one page for the onclick() links
  parse_one <- function(u) {
    pg <- rvest::read_html(u)
    onclicks <- pg |>
      rvest::html_elements(
        xpath = "//input[@type='image' and contains(@onclick,'mostrarModal')]"
      ) |>
      rvest::html_attr("onclick")
    
    if (!length(onclicks)) return(NULL)
    
    # Regex: group 1 = filename.zip, group 2 = /index.php/catalog/421/download/ID (or absolute)
    m <- stringr::str_match(
      onclicks,
      "mostrarModal\\('\\s*([^']+?\\.zip)\\s*'\\s*,\\s*'\\s*([^']+?/download/\\d+)\\s*'"
    )
    m <- m[stats::complete.cases(m), , drop = FALSE]
    if (!nrow(m)) return(NULL)
    
    df <- tibble::tibble(
      filename = trimws(m[,2]),
      href_raw = trimws(m[,3])
    )
    # Ensure absolute URL against this page
    df$href <- vapply(df$href_raw, function(h) {
      # xml2::url_absolute handles absolute+relative properly
      xml2::url_absolute(h, u)
    }, character(1))
    df$label <- tools::file_path_sans_ext(df$filename)
    df
  }
  
  # Parse until we find entries
  all_df <- NULL
  for (u in candidates) {
    all_df <- try(parse_one(u), silent = TRUE)
    if (inherits(all_df, "try-error")) next
    if (!is.null(all_df) && nrow(all_df)) break
  }
  if (is.null(all_df) || !nrow(all_df)) {
    stop("No download inputs found; page markup may have changed or requires login/captcha.")
  }
  
  wanted <- paste0("CG2005_", type)
  cand   <- all_df[stringr::str_detect(all_df$label,
                                       stringr::fixed(wanted, ignore_case = TRUE)), ]
  if (!nrow(cand)) {
    stop("Could not find resource labeled ", wanted,
         ". Available: ", paste(all_df$label, collapse = ", "))
  }
  
  cand <- cand[1, ]
  list(label = cand$label, filename = cand$filename, href = cand$href)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_download_census_data
# @Arg       : type            — "BASICO" or "AMPLIADO"
# @Arg       : url             — catalog page (default: get-microdata version)
# @Arg       : download_folder — where to save ZIP (e.g., here('data','downloads','Census'))
# @Arg       : overwrite       — logical; re-download if file exists (default FALSE)
# @Arg       : retries         — integer; max HTTP retries (default 5)
# @Arg       : quiet           — logical; suppress progress (default FALSE)
# @Output    : tibble with columns: type, file_path, bytes, status
# @Purpose   : Resolve the direct download link and fetch CG2005 microdata ZIP robustly.
#              If server enforces reCAPTCHA (HTTP 401/403), returns status 'captcha_required'.
# @Written_on: 21/08/2025
# --------------------------------------------------------------------------------------------
bogota_download_census_data <- function(
    type,
    url             = "https://microdatos.dane.gov.co/index.php/catalog/421/get-microdata",
    download_folder = here::here('data', 'downloads', 'Census'),
    overwrite       = FALSE,
    retries         = 5,
    quiet           = FALSE
) {
  dir.create(download_folder, recursive = TRUE, showWarnings = FALSE)
  
  # I) Resolve which resource to fetch
  res  <- bogota_find_resource_census(url = url, type = type)
  dest <- file.path(download_folder, res$filename)
  
  if (file.exists(dest) && !isTRUE(overwrite)) {
    if (!quiet) message("↪︎ Already present: ", basename(dest), " (skip).")
    sz <- suppressWarnings(file.size(dest))
    return(tibble::tibble(
      type      = toupper(type),
      file_path = normalizePath(dest),
      bytes     = sz,
      status    = "cached"
    ))
  }
  
  # II) Robust download (direct GET to the /download/<id> link)
  ua <- httr::user_agent(
    sprintf("R (%s) / IDB-AirMonitoring", paste(R.version$platform, R.version$version.string))
  )
  rq <- httr::RETRY(
    verb = "GET",
    url  = res$href,
    ua,
    httr::add_headers(Referer = url),
    httr::write_disk(path = dest, overwrite = TRUE),
    httr::progress(type = if (quiet) "none" else "down"),
    times = as.integer(retries),
    terminate_on = c(200L),
    quiet = quiet,
    httr::timeout(60 * 120)  # up to 2h wall time; BASICO is ~2.4 GB
  )
  
  code <- httr::status_code(rq)
  
  # III) Handle captcha/authorization scenarios gracefully
  if (code %in% c(401L, 403L)) {
    if (file.exists(dest)) unlink(dest)
    warning("Server returned ", code, " (likely requires reCAPTCHA). ",
            "Open the page in a browser and use the 'Descargar' button, then try again.")
    return(tibble::tibble(
      type      = toupper(type),
      file_path = NA_character_,
      bytes     = NA_real_,
      status    = "captcha_required"
    ))
  }
  
  if (code != 200L) {
    if (file.exists(dest)) unlink(dest)
    warning("Download failed with HTTP ", code, " for ", res$href)
    return(tibble::tibble(
      type      = toupper(type),
      file_path = NA_character_,
      bytes     = NA_real_,
      status    = paste0("http_", code)
    ))
  }
  
  # IV) Basic validation
  bytes <- suppressWarnings(file.size(dest))
  if (is.na(bytes) || bytes < 1e6) {
    warning("Downloaded file seems too small: ", dest, " (", bytes, " bytes)")
  }
  
  if (!quiet) {
    message("✅ Downloaded ", basename(dest),
            " (", format(structure(bytes, class = "object_size")), ")")
  }
  
  tibble::tibble(
    type      = toupper(type),
    file_path = normalizePath(dest),
    bytes     = bytes,
    status    = "ok"
  )
}


# ============================================================================================
#  Bogotá-specific functions - processing data ans its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: bogota_normalize_varname
# @Arg       : x           — character; original column name
# @Output    : character; normalized (ASCII, snake_case) variable name
# @Purpose   : Standardize column names from Bogotá XLSX exports; handles accents,
#              spaces/punct, and common Spanish labels (PM2.5/PM10, Ozono, etc.).
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_normalize_varname <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[\\s\\./\\-\\(\\)\\[\\]\\{\\}\\+:;]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  
  x <- gsub("^pm2_?5$", "pm25", x)
  x <- gsub("^pm10$", "pm10", x)
  x <- gsub("^ozono$", "ozone", x)
  x <- gsub("^temp(\\b|_).*", "temp", x)
  x <- gsub("^pres(\\b|_).*", "pressure", x)
  x <- gsub("^hr$", "rh", x)              # humedad relativa
  x <- gsub("^rad(\\b|_).*", "radiation", x)
  x
}


# --------------------------------------------------------------------------------------------
# Function: bogota_read_one_xlsx
# @Arg       : path        — string; full path to a single STATION_YEAR.xlsx
# @Arg       : tz          — string; Olson timezone for datetime parsing 
#                            (default "America/Bogota")
# @Arg       : verbose     — logical; TRUE prints a brief parsing summary (default FALSE)
# @Output    : tibble with columns:
#                 • datetime (POSIXct, tz)
#                 • <pollutant/meteorological variables> (dbl)
#                 • station  (chr)
#                 • year     (int)
# @Purpose   : Read one Bogotá XLSX export that uses a 4-row header:
#              row1 = metadata; row2 = "DateTime" + station in [2,2];
#              row3 = variable names; row4 = units; data start at row5; "----" are NA.
#              Robust to '24:00' (converted to '00:00' + 1 day) and skips non-datetime rows.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_read_one_xlsx <- function(path, tz = "America/Bogota", verbose = FALSE) {
  # 1) read raw (no names); treat "----" / empty as NA
  raw <- suppressMessages(readxl::read_excel(path, col_names = TRUE,
                                             na = c("----", "", "NA")))
  if (nrow(raw) < 5L) {
    if (verbose) message("⚠️  Too few rows in ", basename(path), " — returning empty tibble.")
    return(tibble::tibble(datetime = as.POSIXct(character()),
                          station = character(), 
                          year = integer()))
  }
  
  k <- ncol(raw)
  
  # 2) extract station from row2,col2; fallback to filename STATION_YEAR.xlsx
  fname   <- basename(path)
  st_cell <- as.character(raw[[2, 2]])
  st_file <- stringr::str_match(fname, "^(.*?)_(\\d{4})\\.xlsx$")[, 2]
  station <- if (!is.na(st_cell) && nzchar(st_cell)) stringr::str_squish(st_cell) else st_file
  
  # 3) build raw column names before normalization
  col1 <- as.character(raw[[2, 1]]); if (is.na(col1) || !nzchar(col1)) col1 <- "DateTime"
  raw_vars <- as.character(raw[3, 2:k])
  if (length(raw_vars) && any(!nzchar(raw_vars))) {
    empty_idx <- which(!nzchar(raw_vars))
    raw_vars[empty_idx] <- paste0("var", empty_idx)
  }
  raw_names <- c(col1, raw_vars)
  
  # 4) slice off header rows; assign names
  dat <- raw[-c(1:4), , drop = FALSE]
  if (ncol(dat) != length(raw_names)) {
    # guard against malformed sheets
    raw_names <- head(raw_names, ncol(dat))
  }
  names(dat) <- raw_names
  
  # 5) robust datetime parsing: filter non-datetime rows; handle 24:00
  raw_dt <- as.character(dat[[col1]])
  raw_dt2 <- gsub("/", "-", raw_dt)
  pat <- "^\\d{2}-\\d{2}-\\d{4}\\s+\\d{2}:\\d{2}$"     # strict "DD-MM-YYYY HH:MM"
  is_dt <- !is.na(raw_dt2) & grepl(pat, raw_dt2)
  
  dropped <- sum(!is_dt, na.rm = TRUE)
  dat <- dat[is_dt, , drop = FALSE]
  raw_dt2 <- raw_dt2[is_dt]
  
  is24    <- grepl("\\b24:00\\b", raw_dt2)
  raw_dt3 <- sub("\\b24:00\\b", "00:00", raw_dt2)
  
  parsed  <- lubridate::dmy_hm(raw_dt3, tz = tz)
  parsed[is24 & !is.na(parsed)] <- parsed[is24 & !is.na(parsed)] + lubridate::days(1)
  dat[[col1]] <- parsed
  
  # 6) normalize variable names; coerce to numeric
  norm_vars <- if (length(raw_vars)) {
    vapply(raw_vars,
           bogota_normalize_varname,
           FUN.VALUE = character(1)) 
  } else { character(0) }
  names(dat) <- c("datetime", norm_vars)
  
  num_cols <- setdiff(names(dat), "datetime")
  if (length(num_cols)) {
    dat[num_cols] <- lapply(dat[num_cols], function(x) suppressWarnings(as.numeric(x)))
  }
  
  # 7) add station/year; drop rows with NA datetime (parsing failures)
  dat <- tibble::as_tibble(dat) |>
    dplyr::filter(!is.na(.data$datetime)) |>
    dplyr::mutate(station = station,
                  year    = lubridate::year(.data$datetime))
  
  # 8) provenance + optional message
  attr(dat, "source_file") <- fname
  if (isTRUE(verbose)) {
    msg <- sprintf("📄 %s → rows: %d (dropped %d non-datetime)", fname, nrow(dat), dropped)
    message(msg)
  }
  dat
}


# --------------------------------------------------------------------------------------------
# Function: bogota_merge_downloads
# @Arg       : downloads_folder — string; directory containing *.xlsx exports
# @Arg       : cleanup          — logical; TRUE deletes the .xlsx after merging (default TRUE)
# @Arg       : tz               — string; Olson timezone for datetime parsing (default
#                                 "America/Bogota")
# @Output    : tibble; all files row-bound, sorted and de-duplicated by (station, datetime).
# @Purpose   : Read every XLSX via bogota_read_one_xlsx(), stack, sort, de-dup, and return the
#              combined table. Side-effect (optional): deletes the source .xlsx files.
# @Written_on: 27/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_merge_stations_downloads <- function(downloads_folder,
                                            cleanup = TRUE,
                                            tz = "America/Bogota") {
  # 1) enumerate target files
  files <- list.files(downloads_folder, pattern = "\\.xlsx$", full.names = TRUE)
  if (length(files) == 0L) stop("No .xlsx files found in ", downloads_folder)
  
  # 2) read + stack (robust reader defined elsewhere)
  big_tbl <- purrr::map_dfr(files, function(p) bogota_read_one_xlsx(p, tz = tz))
  
  # 3) sort + de-dup (safety for overlapping or re-downloaded years)
  big_tbl <- big_tbl |>
    dplyr::arrange(.data$station, .data$datetime) |>
    dplyr::distinct(.data$station, .data$datetime, .keep_all = TRUE)
  
  # 4) optional cleanup of raw XLSX
  if (isTRUE(cleanup)) {
    ok <- try(file.remove(files), silent = TRUE)
    if (inherits(ok, "try-error")) {
      warning("Failed to remove some .xlsx files in ", downloads_folder)
    } else {
      message("🗑️  Removed original .xlsx files (",
              sum(ok, na.rm = TRUE), "/", length(files), ").")
    }
  }
  
  # 5) return combined table
  big_tbl
}


# --------------------------------------------------------------------------------------------
# Function: bogota_missing_matrix
# @Arg       : merged_tbl  — tibble; result from bogota_merge_downloads() or similar
# @Arg       : years       — integer vector; target coverage, e.g., 2000:2023
# @Arg       : station_set — character vector; canonical list of station names (optional)
# @Output    : tibble with columns (station, year) indicating missing combinations
# @Purpose   : compute gaps in coverage by comparing the merged table vs. the full grid
#              of (station, year); useful to drive a re-download step.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_missing_matrix <- function(merged_tbl,
                                  years = 2000:2023,
                                  station_set = NULL) {
  # 1) sanity checks
  req <- c("station", "year")
  if (!all(req %in% names(merged_tbl))) {
    stop("merged_tbl must contain columns: ", paste(req, collapse = ", "))
  }
  
  # 2) observed coverage
  have <- merged_tbl |>
    dplyr::distinct(.data$station, .data$year)
  
  # 3) station universe (provided or inferred)
  stations <- if (!is.null(station_set)) {
    sort(unique(as.character(station_set)))
  } else {
    sort(unique(merged_tbl$station))
  }
  
  # 4) full grid and anti-join to get missing
  full_grid <- tidyr::expand_grid(station = stations, year = as.integer(years))
  missing   <- dplyr::anti_join(full_grid, have, by = c("station", "year"))
  
  # 5) return
  return(missing)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_filter_harmonize_census
# @Arg       : census_zip   — path to CG2005_AMPLIADO.zip
# @Arg       : out_dir      — where to write selected dept folders with CSVs
# @Arg       : overwrite    — re-extract if output exists (default FALSE)
# @Arg       : quiet        — suppress messages (default FALSE)
# @Output    : list with $bogota and $cundinamarca
#              - dir_extracted (folder containing only CSV files)
#              - files_index   (tibble with file, size, type)
# @Purpose   : Fast extract master ZIP (system tools), fix mojibake on disk,
#              locate dep. 11 & 25 packages, unpack, then unpack the *CSV.zip*
#              inside each and leave only the CSV files directly in the folder.
# @Written_on: 22/10/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_filter_harmonize_census <- function(
    census_zip = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip"),
    out_dir    = here::here("data", "raw", "census", "Bogota", "CG2005"),
    overwrite  = FALSE,
    quiet      = FALSE
) {
  if (!file.exists(census_zip)) stop("Master ZIP not found: ", census_zip)
  for (pkg in c("tibble", "tools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required.")
    }
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- helpers ----------------------------------------------------------------
  # fix_visible_glyphs()
  # Purpose : Replace mojibaked glyphs produced by some unzip tools (macOS/Finder)
  #           with the intended Spanish accented characters. Works on *valid*
  #           UTF-8 strings; avoids byte-level surgery to prevent invalid UTF-8.
  # Mapping : †→á, °→í, ¢→ó, §→ñ, Ç→é
  fix_visible_glyphs <- function(x) {
    repl <- c("†" = "á", "°" = "í", "¢" = "ó", "§" = "ñ", "Ç" = "é")
    for (k in names(repl)) x <- gsub(k, repl[[k]], x, fixed = TRUE)
    x
  }
  
  # rename_tree_utf8()
  # Purpose : Walk a directory tree deepest-first and rename each basename
  #           applying fix_visible_glyphs(). This normalizes files/dirs *on disk*.
  rename_tree_utf8 <- function(root) {
    paths <- list.files(
      root, recursive = TRUE, include.dirs = TRUE, full.names = TRUE
    )
    paths <- paths[order(nchar(paths), decreasing = TRUE)]
    for (p in paths) {
      new_base <- fix_visible_glyphs(basename(p))
      if (!identical(new_base, basename(p))) {
        new_path <- file.path(dirname(p), new_base)
        dir.create(dirname(new_path), recursive = TRUE, showWarnings = FALSE)
        suppressWarnings(file.rename(p, new_path))
      }
    }
    invisible(root)
  }
  
  # make_index_tbl()
  # Purpose : Return a tibble index of files in a folder (recursive).
  make_index_tbl <- function(root) {
    files <- list.files(root, recursive = TRUE, full.names = TRUE)
    if (!length(files)) {
      return(tibble::tibble(file = character(), size = numeric(), type = character()))
    }
    tibble::tibble(
      file = files, size = file.info(files)$size, type = tools::file_ext(files)
    )
  }
  
  # sys_extract_all()
  # Purpose : Fast, robust extraction using system tools. On macOS uses `ditto`
  #           (same engine as Finder/Archive Utility). Else, uses `unzip -o`.
  sys_extract_all <- function(zipfile, exdir, overwrite = FALSE, quiet = FALSE) {
    if (dir.exists(exdir) && overwrite) unlink(exdir, recursive = TRUE, force = TRUE)
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    
    is_mac  <- identical(Sys.info()[["sysname"]], "Darwin")
    has_dit <- nzchar(Sys.which("ditto"))
    has_unz <- nzchar(Sys.which("unzip"))
    status  <- 1L
    
    if (is_mac && has_dit) {
      args <- c("-x", "-k",
                shQuote(normalizePath(zipfile)),
                shQuote(normalizePath(exdir)))
      status <- system2("ditto", args,
                        stdout = if (quiet) FALSE else "",
                        stderr = if (quiet) FALSE else "")
    } else if (has_unz) {
      args <- c("-o", if (quiet) "-qq" else NULL,
                shQuote(normalizePath(zipfile)),
                "-d", shQuote(normalizePath(exdir)))
      status <- system2("unzip", args,
                        stdout = if (quiet) FALSE else "",
                        stderr = if (quiet) FALSE else "")
    } else {
      utils::unzip(zipfile, exdir = exdir, overwrite = TRUE)
      status <- 0L
    }
    if (!identical(status, 0L)) stop("System extraction failed for: ", zipfile)
    invisible(exdir)
  }
  
  # ---- 1) Extract the WHOLE master ZIP (once) ---------------------------------
  master_dir <- file.path(tempdir(), "cg2005_master")
  if (!dir.exists(master_dir) || isTRUE(overwrite)) {
    if (!quiet) message("📦 Extracting master ZIP → ", master_dir)
    sys_extract_all(census_zip, master_dir, overwrite = TRUE, quiet = quiet)
    rename_tree_utf8(master_dir)  # normalize names from the extractor
  } else if (!quiet) {
    message("↪︎ Master already extracted: ", master_dir)
  }
  
  # ---- 2) Locate department ZIPs by numeric prefix (11 & 25) ------------------
  all_zips <- list.files(
    master_dir, pattern = "\\.[Zz][Ii][Pp]$", recursive = TRUE, full.names = TRUE
  )
  if (!length(all_zips)) stop("No .zip files found under: ", master_dir)
  
  bf <- basename(all_zips)
  bf_fix <- fix_visible_glyphs(bf)
  
  get_code <- function(x) {
    m <- regexec("^([0-9]{2})", x); regmatches(x, m)[[1]][2]
  }
  codes <- vapply(bf, get_code, character(1L))
  na_idx <- which(is.na(codes) | codes == "")
  if (length(na_idx)) codes[na_idx] <- vapply(bf_fix[na_idx], get_code, character(1L))
  
  pick_by_code <- function(code) {
    cand <- which(codes == sprintf("%02d", as.integer(code)))
    if (!length(cand)) {
      stop("Could not locate ZIP for code ", code,
           ". Sample: ", paste(utils::head(bf_fix, 10), collapse = " | "))
    }
    cand[which.min(nchar(bf[cand]))]
  }
  
  idx_b <- pick_by_code(11)
  idx_c <- pick_by_code(25)
  
  bogota_zip_src       <- all_zips[idx_b]
  cundinamarca_zip_src <- all_zips[idx_c]
  
  # Paths we’ll extract into
  bog_dir <- file.path(out_dir, "11.Bogotá")
  cun_dir <- file.path(out_dir, "25. Cundinamarca")
  
  # ---- 3) Extract each dept ZIP, then extract CSV.zip and keep only CSVs -----
  dept_extract_csv_only <- function(zip_src, label, tgt_dir) {
    # 3a) Extract department package (contains CSV.zip/DTA.zip/SAV.zip)
    if (!dir.exists(tgt_dir) || isTRUE(overwrite)) {
      if (dir.exists(tgt_dir)) unlink(tgt_dir, recursive = TRUE, force = TRUE)
      if (!quiet) message("📤 Extracting ", label, " → ", tgt_dir)
      sys_extract_all(zip_src, tgt_dir, overwrite = TRUE, quiet = quiet)
      rename_tree_utf8(tgt_dir)
    } else if (!quiet) {
      message("↪︎ Using existing folder: ", tgt_dir)
    }
    
    # 3b) Find the *CSV.zip* inside the dept folder
    csv_zip <- list.files(
      tgt_dir, pattern = "CSV\\.[Zz][Ii][Pp]$", full.names = TRUE, ignore.case = TRUE
    )
    if (!length(csv_zip)) {
      # Fallback: maybe CSVs are loose; collect them if present
      csvs_fallback <- list.files(tgt_dir, pattern = "\\.csv$", recursive = TRUE,
                                  full.names = TRUE, ignore.case = TRUE)
      if (!length(csvs_fallback)) {
        stop("No CSV.zip and no .csv files found inside: ", tgt_dir)
      }
      # Move fallback CSVs to tgt_dir root with normalized names
      for (f in csvs_fallback) {
        file.copy(f, file.path(tgt_dir, fix_visible_glyphs(basename(f))),
                  overwrite = TRUE)
      }
    } else {
      # 3c) Extract CSV.zip to a staging dir, then move only *.csv to tgt_dir root
      stage <- file.path(tgt_dir, "_csv_stage")
      sys_extract_all(csv_zip[1], stage, overwrite = TRUE, quiet = quiet)
      rename_tree_utf8(stage)
      csvs <- list.files(stage, pattern = "\\.csv$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
      if (!length(csvs)) stop("CSV.zip contained no .csv files: ", csv_zip[1])
      for (f in csvs) {
        file.copy(f, file.path(tgt_dir, fix_visible_glyphs(basename(f))),
                  overwrite = TRUE)
      }
      unlink(stage, recursive = TRUE, force = TRUE)
    }
    
    # 3d) Remove any ZIPs left in tgt_dir (CSV/DTA/SAV) and any subfolders
    zips_left <- list.files(tgt_dir, pattern = "\\.[Zz][Ii][Pp]$",
                            full.names = TRUE, recursive = TRUE)
    if (length(zips_left)) file.remove(zips_left)
    
    # Remove all subdirectories (leave only the CSV files at root)
    subdirs <- list.dirs(tgt_dir, recursive = TRUE, full.names = TRUE)
    subdirs <- subdirs[subdirs != tgt_dir]
    if (length(subdirs)) {
      subdirs <- subdirs[order(nchar(subdirs), decreasing = TRUE)]
      for (d in subdirs) unlink(d, recursive = TRUE, force = TRUE)
    }
    
    tibble::tibble(dir_extracted = tgt_dir)
  }
  
  idx_bog <- dept_extract_csv_only(bogota_zip_src, "Bogotá (11)", bog_dir)
  idx_cun <- dept_extract_csv_only(cundinamarca_zip_src, "Cundinamarca (25)", cun_dir)
  
  # 3e) Ensure the top-level copies of the department ZIPs do not remain in out_dir
  #     (You asked to remove the zips extracted from the census_zip.)
  #     We never copied them to out_dir in this version; nothing to remove here.
  
  # ---- 4) Build indexes (should list only the CSVs) ---------------------------
  bogota_files       <- make_index_tbl(bog_dir)
  cundinamarca_files <- make_index_tbl(cun_dir)
  
  if (!quiet) {
    message("🧾 Bogotá CSVs: ", sum(tolower(bogota_files$type) == "csv"),
            " | Cundinamarca CSVs: ",
            sum(tolower(cundinamarca_files$type) == "csv"))
  }
  
  # ---- 5) Return --------------------------------------------------------------
  list(
    bogota = list(
      dir_extracted = bog_dir,
      files_index   = bogota_files
    ),
    cundinamarca = list(
      dir_extracted = cun_dir,
      files_index   = cundinamarca_files
    )
  )
}


# ============================================================================================
# ============================================================================================
#  Bogota's wrapper functions
# ============================================================================================
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: bogota_download
# @Purpose   : Thin wrapper around your working bogota_download_station_data(...),
#              filling defaults from bogota_cfg and keeping a single call site.
# --------------------------------------------------------------------------------------------
bogota_download <- function(cfg           = bogota_cfg,
                            container     = TRUE,
                            stations_idx  = NULL,
                            max_attempts  = 3,
                            timeout_page  = 30,
                            timeout_btn   = 30,
                            timeout_dl    = 400) {
  dir.create(cfg$dl_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Point the downloader to the *shared* downloads folder used by your merge function
  # (If you prefer per-city isolation, set DOWNLOADS_DIR in the env before calling.)
  old_env <- Sys.getenv("DOWNLOADS_DIR", unset = NA)
  Sys.setenv(DOWNLOADS_DIR = here::here("data", "downloads"))
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("DOWNLOADS_DIR") else Sys.setenv(DOWNLOADS_DIR = old_env)
  }, add = TRUE)
  
  bogota_download_station_data(
    base_url      = cfg$base_url_rmcab,
    start_year    = min(cfg$years),
    end_year      = max(cfg$years),
    container     = container,
    stations_idx  = stations_idx,
    max_attempts  = max_attempts,
    timeout_page  = timeout_page,
    timeout_btn   = timeout_btn,
    timeout_dl    = timeout_dl
  )
}


# --------------------------------------------------------------------------------------------
# Function: bogota_process
# @Purpose   : Merge XLSX, tidy, de-dup, and persist for Bogotá, using shared utils.
# --------------------------------------------------------------------------------------------
bogota_process <- function(cfg = bogota_cfg) {
  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  bogota_merge_downloads(
    downloads_folder = here::here("data", "downloads"),
    out_dir          = cfg$out_dir,
    out_name         = NULL,
    write_rds        = TRUE,
    write_parquet    = TRUE,
    write_csv_gz     = FALSE,
    cleanup          = FALSE,
    tz               = cfg$tz
  )
}


# ============================================================================================
#  Bogota's register options
# ============================================================================================

# ------------------------------- Register this city in the registry --------------------------
register_city(
  id       = bogota_cfg$id,
  cfg      = bogota_cfg,
  download = bogota_download,
  process  = bogota_process
)