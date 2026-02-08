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
  id               = "bogota",
  tz               = "America/Bogota",
  url_station_shp  = "https://www.ambientebogota.gov.co/estaciones-rmcab",
  base_url_shp     = "https://www.dane.gov.co/files/geoportal-provisional",
  base_url_rmcab   = "http://rmcab.ambientebogota.gov.co/Report/stationreport",
  base_url_sisaire = "http://sisaire.ideam.gov.co/ideam-sisaire-web/consultas.xhtml",
  base_url_census  = "https://microdatos.dane.gov.co/index.php/catalog/421/get-microdata",
  base_new_census  = "https://microdatos.dane.gov.co/index.php/catalog/643/get-microdata",
  years            = 2000L:2023L,
  dl_dir           = here::here("data", "downloads", "bogota"),
  out_dir          = here::here("data", "raw"),
  which_states     = c("Cundinamarca", "Bogotá D.C.", "Huila", "Meta", "Tolima"),
  cities_in_metro  = c("Bogotá DC", "Bojacá", "Cajicá", "Chía", "Cota", "El Rosal",
                       "Facatativá", "Funza", "Fusagasugá", "Gachancipá", "La Calera", "Madrid",
                       "Mosquera", "Sibaté", "Soacha", "Sopó", "Subachoque", "Tabio", "Tenjo",
                       "Tocancipá", "Zipaquirá"),
  city_code_metro  = c("25099", "25126", "25214", "25269", "25286", "25295", "25377", "25430",
                       "25758", "25769", "25785", "25799", "25817", "25899", "11001", "25754",
                       "25175", "25260", "25473", "25740", "25290"),
  station_nme_map  = c("Centro de alto rendimiento" = "Centro de Alto Rendimiento",
                       "Las Ferias"                 = "Las Ferias",
                       "Carvajal-Sevillana"         = "Carvajal-Sevillana")
)

# ============================================================================================
#  Bogotá-specific functions - downloading and its helpers
# ============================================================================================
# --------------------------------------------------------------------------------------------
# Function: bogota_download_metro_area
# @Arg level            : "mpio", "depto", or "manzana". 
# @Arg mgn_year         : 2018 or 2005. 
# @Arg base_url         : base URL of DANE geoportal files.
# @Arg municipality_codes: character vector of 5-digit codes (e.g. "11001").
# @Arg download_dir     : where to save the ZIP.
# @Arg out_file         : where to write the cropped GeoPackage.
# @Arg overwrite_zip    : logical; re-download if ZIP exists.
# @Arg overwrite_gpkg   : logical; overwrite output GeoPackage if exists.
# @Arg quiet            : logical; suppress progress.
#
# @Output               : Writes a GeoPackage; returns (invisibly) the sf object.
# @Purpose              : Download admin boundaries and crop to the Bogotá metro area.
#                         Specially handles 2018 Manzana by downloading BOTH Urban and Rural ZIPs.
#
# @Written_on           : 20/08/2025 (Updated 08/02/2026)
# @Written_by           : Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_download_metro_area <- function(
    level                  = c("mpio", "depto", "manzana"),
    mgn_year               = c(2018, 2005),
    base_url               = bogota_cfg$base_url_shp,
    municipality_codes     = bogota_cfg$city_code_metro,
    download_dir           = here::here("data", "downloads", "Administrative", "Colombia"),
    out_file               = here::here("data", "raw", "admin", "Colombia", "bogota.gpkg"),
    overwrite_zip          = FALSE,
    overwrite_gpkg         = TRUE,
    quiet                  = FALSE
) {
  
  level    <- match.arg(tolower(level), c("mpio", "depto", "manzana"))
  mgn_year <- as.numeric(match.arg(as.character(mgn_year), c("2018", "2005")))
  
  # ----------------------------------------------------------------------------
  # 1) Define ZIP names based on year and level
  # ----------------------------------------------------------------------------
  if (mgn_year == 2018) {
    if (level == "manzana") {
      # 2018 requires TWO files for full coverage at the block/sector level
      zip_names <- c("SHP_MGN2018_INTGRD_MANZ.zip",   # Urban
                     "SHP_MGN2018_INTGRD_SECCR.zip")  # Rural Sections
    } else {
      zip_names <- switch(level,
                          "mpio"  = "SHP_MGN2018_INTGRD_MPIO.zip",
                          "depto" = "SHP_MGN2018_INTGRD_DEPTO.zip")
    }
  } else {
    # 2005 data usually comes in a single national ZIP for all levels
    zip_names <- "SHP_MGN2005_COLOMBIA.zip"
  }
  
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Temporary directory for extraction (unique per run)
  exdir <- file.path(tempdir(), paste0("col_shp_", level, "_", mgn_year))
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE)
  dir.create(exdir)
  
  # 2) Loop Download & Extract
  # ----------------------------------------------------------------------------
  for (zip_name in zip_names) {
    zip_url  <- file.path(base_url, zip_name)
    zip_path <- file.path(download_dir, zip_name)
    
    # Download
    if (file.exists(zip_path) && !overwrite_zip) {
      if (!quiet) message("↪︎ ZIP already present: ", zip_name)
    } else {
      if (!quiet) message("⬇️  Downloading ", zip_name, " ...")
      req <- httr::RETRY("GET", zip_url, httr::write_disk(zip_path, overwrite = TRUE), 
                         times = 5, httr::timeout(3600))
      if (httr::status_code(req) != 200L) stop("Download failed for: ", zip_name)
    }
    
    # Extract into the same temp folder (files usually have distinct names)
    if (!quiet) message("📦 Extracting ", zip_name, "...")
    archive::archive_extract(zip_path, dir = exdir)
  }
  
  # 3) Processing Logic
  # ----------------------------------------------------------------------------
  
  if (mgn_year == 2005) {
    # --- 2005 LOGIC (Single Zip) ---
    if (level == "manzana") {
      if (!quiet) message("🧩 [2005] Merging Urban Manzanas and Rural Sections...")
      
      path_mza   <- list.files(exdir, pattern = "MGN_Manzana\\.shp$",
                               recursive = TRUE, full.names = TRUE)
      path_rural <- list.files(exdir, pattern = "MGN_Seccion_rural\\.shp$",
                               recursive = TRUE, full.names = TRUE)
      
      u_processed <- sf::st_read(path_mza, quiet = TRUE) %>%
        dplyr::rename_with(toupper) %>%
        dplyr::mutate(
          MPIO_FULL = paste0(stringr::str_pad(SETR_CLSE_, 2, pad="0"), 
                             stringr::str_pad(SECR_SETR_, 3, pad="0")),
          TIPO      = "Urban Block",
          GEO_ID    = MANZ_CCNCT
        ) %>%
        dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
        dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      
      r_processed <- sf::st_read(path_rural, quiet = TRUE) %>%
        dplyr::rename_with(toupper) %>%
        dplyr::mutate(
          MPIO_FULL = paste0(stringr::str_pad(SETR_CLSE_, 2, pad="0"), 
                             stringr::str_pad(SETR_CLSE1, 3, pad="0")),
          TIPO      = "Rural Section",
          GEO_ID    = SECR_CCNCT
        ) %>%
        dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
        dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      
      g_sel <- dplyr::bind_rows(u_processed, r_processed)
      
    } else {
      # 2005 Mpio/Depto
      pattern <- if(level == "mpio") "MGN_Municipio\\.shp$" else "MGN_Departamento\\.shp$"
      target_shp <- list.files(exdir, pattern = pattern, recursive = TRUE, full.names = TRUE)
      
      g_sel <- sf::st_read(target_shp, quiet = TRUE) %>%
        dplyr::rename_with(toupper) %>%
        dplyr::mutate(
          MPIO_FULL = paste0(stringr::str_pad(DPTO_DPTO_, 2, pad="0"), 
                             stringr::str_pad(MPIO_CCDGO, 3, pad="0"))
        ) %>%
        dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
        dplyr::select(MPIO_FULL, dplyr::everything())
    }
    
  } else {
    # --- 2018 LOGIC (Dual Zips for Manzana) ---
    
    if (level == "manzana") {
      if (!quiet) message("🧩 [2018] Merging Urban Manzanas and Rural Sections...")
      
      # 1. Identify Urban File (MANZ)
      path_mza <- list.files(exdir, pattern = "(?i)MANZ.*\\.shp$",
                             recursive = TRUE, full.names = TRUE)
      # 2. Identify Rural File (SECCR / SECCION)
      # The 2018 rural zip usually contains a file with 'SECCION' or 'SECCR' in name
      path_rural <- list.files(exdir, pattern = "(?i)(SECCION|SECCR).*\\.shp$",
                               recursive = TRUE, full.names = TRUE)
      
      # Process Urban
      if (length(path_mza) > 0) {
        path_mza <- path_mza[which.max(file.info(path_mza)$size)] # Robustness
        
        u_processed <- sf::st_read(path_mza, quiet = TRUE) %>%
          dplyr::rename_with(toupper) %>%
          dplyr::mutate(
            MPIO_FULL = as.character(MPIO_CDPMP), # 2018 code is concatenated
            TIPO      = "Urban Block",
            GEO_ID    = as.character(COD_DANE_A)  # Unique ID
          ) %>%
          dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
          dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      } else {
        warning("2018 Urban Manzana shapefile not found.")
        u_processed <- NULL
      }
      
      # Process Rural
      if (length(path_rural) > 0) {
        path_rural <- path_rural[which.max(file.info(path_rural)$size)] # Robustness
        
        r_processed <- sf::st_read(path_rural, quiet = TRUE) %>%
          dplyr::rename_with(toupper) %>%
          dplyr::mutate(
            MPIO_FULL = as.character(MPIO_CDPMP),
            TIPO      = "Rural Section",
            GEO_ID    = as.character(SECR_CCNCT)
          ) %>%
          dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
          dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      } else {
        if(!quiet) warning("⚠️ 2018 Rural Sections shapefile not found in extracted folder.")
        r_processed <- NULL
      }
      
      g_sel <- dplyr::bind_rows(u_processed, r_processed)
      
    } else {
      # 2018 Mpio or Depto
      pattern <- if(level == "mpio") "(?i)MPIO.*\\.shp$" else "(?i)DEPTO.*\\.shp$"
      target_shp <- list.files(exdir, pattern = pattern, recursive = TRUE, full.names = TRUE)
      
      if(length(target_shp) == 0) stop("Shapefile not found for level: ", level)
      target_shp <- target_shp[which.max(file.info(target_shp)$size)]
      
      g_sel <- sf::st_read(target_shp, quiet = TRUE) %>%
        dplyr::rename_with(toupper) %>%
        dplyr::mutate(
          MPIO_FULL = if("MPIO_CDPMP" %in% names(.)) as.character(MPIO_CDPMP) 
          else paste0(DPTO_CCDGO, MPIO_CCDGO)
        ) %>%
        dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
        dplyr::select(MPIO_FULL, dplyr::everything())
    }
  }
  
  # 4) Export
  # ----------------------------------------------------------------------------
  if (is.null(g_sel) || nrow(g_sel) == 0) stop("No features found for provided codes.")
  
  if (!quiet) message("💾 Writing GeoPackage → ", basename(out_file))
  sf::st_write(g_sel, out_file, delete_dsn = overwrite_gpkg, quiet = TRUE)
  
  # Cleanup temp files
  unlink(exdir, recursive = TRUE)
  
  invisible(g_sel)
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
    write_parquet = FALSE,
    write_csv     = TRUE
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


# ----------------------------------------------------------------------------------------
# Function: sisaire_download_department_metadata
# @Arg      : base_url      — string; main SISAIRE URL
# @Arg      : target_depts  — character vector; list of departments (e.g. "Cundinamarca")
# @Arg      : container     — logical; TRUE if running inside Docker (default TRUE)
# @Arg      : subdir        — string; subfolder in downloads to store files
# @Arg      : timeout_page  — integer; max wait for page load (default 50)
# @Arg      : timeout_dl    — integer; max wait for download (default 120)
# @Output   : tibble; log of actions (department, status, file_path)
# @Purpose  : Navigates SISAIRE -> Calidad del aire -> Por Departamento.
#             Features robust "Google bounce" navigation to prevent cold-start errors,
#             case-insensitive matching for "BOGOTÁ", and per-department retries.
# @Written_on: 09/12/2025
# @Written_by: Marcos Paulo
# ----------------------------------------------------------------------------------------
sisaire_download_department_metadata <- function(
    base_url = bogota_cfg$base_url_sisaire,
    target_depts = bogota_cfg$which_states,
    container = TRUE,
    subdir = "bogota/stations_metadata",
    timeout_page = 15,
    timeout_dl = 12
) {
  
  # 0) Setup Directories
  downloads_folder <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  dir.create(downloads_folder, recursive = TRUE, showWarnings = FALSE)
  
  final_dir <- file.path(downloads_folder, subdir)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  message("✔️  Downloads dir: ", downloads_folder)
  message("📂 Target subdir : ", final_dir)
  
  # 1) Docker / Selenium Setup
  if (!container) {
    message("🚀 Starting local Selenium on 4445…")
    cid <- system(
      "docker run -d -p 4445:4444 --shm-size=2g selenium/standalone-firefox:4.34.0-20250717",
      intern = TRUE
    )
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern = TRUE), silent = TRUE),
            add = TRUE)
    selenium_host <- "localhost"; selenium_port <- 4445L
  } else {
    selenium_host <- "selenium";  selenium_port <- 4444L
  }
  
  download_dir_container <- if (container) "/home/seluser/Downloads" else downloads_folder
  caps <- list(
    browserName = "firefox",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList" = 2L,
        "browser.download.dir" = download_dir_container,
        "browser.download.useDownloadDir" = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" = paste(
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "application/octet-stream", sep = ","
        )
      )
    ),
    timeouts = list(implicit = 0L, pageLoad = 60000L, script = 60000L)
  )
  
  session <- selenium::SeleniumSession$new(
    browser = "firefox", host = selenium_host, port = selenium_port, capabilities = caps
  )
  on.exit(session$close(), add = TRUE)
  
  # 2) Robust Helper: Google Bounce -> Dashboard -> Table
  nav_to_dept_search <- function() {
    # Sanity check: Bounce to Google to clear state
    session$navigate("https://www.google.com")
    Sys.sleep(5) 
    
    session$navigate(base_url)
    # Heavy wait for SISAIRE hydration
    Sys.sleep(40) 
    wait_ready(session, timeout_page)
    
    # Click "Calidad del aire" (Parent Menu)
    menu_top <- wait_for(session, "xpath", 
                         "//span[contains(text(),'Calidad del aire')]", timeout_page)
    menu_top$click()
    
    # Click "Por Departamento" (Submenu)
    menu_sub <- wait_for(session, "css selector", 
                         "a[href='calidad_aire_departamento.xhtml']", timeout_page)
    menu_sub$click()
    wait_ready(session, timeout_page)
    Sys.sleep(5) 
  }
  
  log <- list()
  
  # 3) Main Loop over Departments
  for (dept in target_depts) {
    message(sprintf("\n📍 Processing: %s", dept))
    
    status   <- "failed"
    file_res <- NA_character_
    attempt  <- 0
    max_retries <- 3
    
    # RETRY LOOP for stability
    repeat {
      attempt <- attempt + 1
      
      try_result <- tryCatch({
        
        # A. Navigate 
        nav_to_dept_search()
        
        # B. Filter Table
        input_sel <- "input[id*='calTablaCantEst'][id*='filter']"
        inp <- wait_for(session, "css selector", input_sel, timeout_page)
        
        inp$click()
        inp$clear()
        inp$send_keys(toupper(dept), keys$enter)
        
        Sys.sleep(5)
        
        # C. Find the Link (Uppercase)
        target_text <- toupper(dept)
        xpath_query <- sprintf("//a[contains(text(), '%s')]", target_text)
        
        row_link <- wait_for(session, "xpath", xpath_query, timeout_page)
        row_link$click()
        
        # Wait for Detail View
        Sys.sleep(60)
        wait_ready(session, timeout_page)
        
        # --- CRITICAL FIX: CLEAN SLATE ---
        # Before we start downloading, ensure the download folder is empty of .xlsx files.
        # This guarantees that 'wait_for_new_download' will see the new file,
        # even if it has the same name as a previous failed attempt.
        trash_files <- list.files(downloads_folder, pattern = "\\.xlsx$", full.names = TRUE)
        if (length(trash_files) > 0) {
          unlink(trash_files)
        }
        # ---------------------------------
        
        # D. Download
        before <- list.files(downloads_folder, pattern = "\\.xlsx$", full.names = TRUE)
        btn_excel <- wait_for(session, "css selector", 
                              "img[src*='logo-excel.png']", timeout_page)
        
        # Click parent <a>
        btn_excel$find_element("xpath", ".. ")$click()
        
        src <- wait_for_new_download(
          dir = downloads_folder,
          before = before,
          pattern = "\\.xls$",
          timeout = timeout_dl
        )
        
        # E. Move/Rename
        sanitized_dept <- sanitize_name(dept)
        dest_name <- paste0(sanitized_dept, ".xls")
        dest_path <- file.path(final_dir, dest_name)
        
        if (file.exists(dest_path)) unlink(dest_path)
        file.copy(src, dest_path)
        unlink(src)
        
        message("   ✅ Downloaded: ", dest_name)
        status   <- "ok"
        file_res <- dest_path
        
        TRUE # Break loop
        
      }, error = function(e) {
        message(sprintf("   ⚠️ Attempt %d/%d failed: %s", attempt, max_retries, e$message))
        return(FALSE)
      })
      
      if (try_result) break 
      if (attempt >= max_retries) {
        message("   ❌ Gave up on: ", dept)
        break
      }
      Sys.sleep(10)
    }
    
    log[[length(log) + 1L]] <- tibble::tibble(
      department = dept,
      status = status,
      file = file_res
    )
  }
  
  dplyr::bind_rows(log)
}


# ----------------------------------------------------------------------------------------
# Function: sisaire_download_hourly_data
#
# @Arg      : base_url      — string; The base URL for the SISAIRE query page.
# @Arg      : target_depts  — character vector; List of departments (e.g., "Bogotá D.C.").
# @Arg      : target_params — character vector|NULL; Pollutants (e.g., "PM10", "O3").
#                             If NULL, defaults to a standard list of 11 parameters.
# @Arg      : years_range   — integer vector; Range of years to query (e.g., 2000:2024).
# @Arg      : container     — logical; TRUE if running inside a Docker Selenium container.
# @Arg      : subdir        — string; Subdirectory within downloads to save files.
# @Arg      : timeout       — integer; Timeout in seconds for page loads (default 180).
#
# @Output   : tibble; A log containing: dept, param, start, end, status, file.
#
# @Purpose  : Robustly scrape hourly air quality data from the SISAIRE/IDEAM legacy app.
#             Uses advanced Selenium strategies to bypass PrimeFaces/JSF limitations:
#             1. "Direct Injection": Manipulates hidden <select> via JS.
#             2. "Adaptive Retry": Increases wait times dynamically if the server lags.
#             3. "Session Recovery": Forces a browser refresh if a specific date chunk
#                fails, preventing "zombie" sessions.
# ----------------------------------------------------------------------------------------
sisaire_download_hourly_data <- function(
    base_url = bogota_cfg$base_url_sisaire,
    target_depts = bogota_cfg$which_states,
    target_params = NULL,
    years_range = bogota_cfg$years,
    container = TRUE,
    subdir = "stations_hourly",
    timeout = 180
) {
  
  # 0) Directories & Docker Setup (Standard) -------------------------------------------
  downloads_folder <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  dir.create(downloads_folder, recursive = TRUE, showWarnings = FALSE)
  final_dir <- file.path(downloads_folder, subdir)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("✔️  Downloads dir: ", downloads_folder)
  message("📂 Target subdir : ", final_dir)
  
  if (is.null(target_params)) {
    target_params <- c("PM10", "PM2.5", "O3", "NO2", "SO2", "CO", 
                       "TMPR_AIR_10CM", "VViento", "DViento", "P", "RGlobal")
  }
  
  if (!container) {
    message("🚀 Starting local Selenium on 4445…")
    cid <- system(paste("docker run -d -p 4445:4444 --shm-size=2g", 
                        "selenium/standalone-firefox:4.34.0-20250717"), intern = TRUE)
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern=TRUE), silent=TRUE), add=TRUE)
    selenium_host <- "localhost"; selenium_port <- 4445L
  } else {
    selenium_host <- "selenium";  selenium_port <- 4444L
  }
  
  download_dir_container <- if (container) "/home/seluser/Downloads" else downloads_folder
  
  caps <- list(
    browserName = "firefox",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList" = 2L,
        "browser.download.dir" = download_dir_container,
        "browser.download.useDownloadDir" = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" = paste(
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "application/octet-stream", "text/csv", "text/plain", "application/csv", sep=","
        )
      )
    ),
    timeouts = list(implicit = 0L, pageLoad = 60000L, script = 60000L)
  )
  
  session <- selenium::SeleniumSession$new(
    browser = "firefox", host = selenium_host, port = selenium_port, 
    capabilities = caps, timeout = timeout
  )
  on.exit(session$close(), add = TRUE)
  
  # --- HELPERS ------------------------------------------------------------------------
  wait_for_spinner <- function() {
    Sys.sleep(0.5)
    session$execute_script("
        var start = new Date().getTime();
        while (new Date().getTime() < start + 10000) {
          if ($('#loading').is(':visible') == false) break;
        }
    ")
    Sys.sleep(1)
  }
  
  safe_navigate <- function(url) {
    for(attempt in 1:10) { 
      try({
        session$navigate(url)
        Sys.sleep(5)
        curr_url <- session$current_url()
        if (grepl("about:neterror", curr_url) || grepl("dnsNotFound", curr_url)) {
          stop("DNS/Net Error detected")
        }
        return(TRUE) 
      }, silent = TRUE) -> nav_res
      if (!inherits(nav_res, "try-error")) return(TRUE)
      message(sprintf("      ⚠️ Server down (Attempt %d/10). Waiting 60s...", attempt))
      Sys.sleep(60) 
    }
    stop("❌ Critical: Server is completely unreachable after 10 attempts.")
  }
  
  wait_for_xpath <- function(xpath, s_time=30) {
    t0 <- Sys.time()
    while(as.numeric(difftime(Sys.time(), t0, units="secs")) < s_time) {
      el <- try(session$find_element("xpath", xpath), silent=TRUE)
      if (!inherits(el, "try-error")) return(el)
      Sys.sleep(0.5)
    }
    stop("Timeout waiting for element: ", xpath)
  }
  
  pf_select_option <- function(widget_id, label_text) {
    input_id <- paste0(widget_id, "_input")
    js_code <- sprintf("
      var sel = document.getElementById('%s');
      if (!sel) return false;
      var found = false;
      for (var i = 0; i < sel.options.length; i++) {
        var txt = sel.options[i].text.trim().toUpperCase();
        var target = '%s'.trim().toUpperCase();
        if (txt === target) {
          sel.selectedIndex = i;
          found = true;
          break;
        }
      }
      if (found) {
        if (window.jQuery) { $(sel).trigger('change'); } 
        else {
           if(sel.onchange) sel.onchange(); 
           else sel.dispatchEvent(new Event('change'));
        }
        return true;
      }
      return false;
    ", input_id, label_text)
    
    found <- session$execute_script(js_code)
    if (isFALSE(found)) {
      stop(sprintf("Could not find option '%s' in %s", label_text, widget_id))
    }
    wait_for_spinner()
  }
  
  setup_form <- function(dept, param) {
    w_table <- 10; w_confirm <- 30
    for (attempt in 1:3) {
      res_seq <- tryCatch({
        pf_select_option("filtroForm\\:departamentoSel", dept)
        dept_val <- session$execute_script(
          "return document.getElementById('filtroForm:departamentoSel_input').value;"
        )
        if (is.null(dept_val) || dept_val == "" || dept_val == "0") {
          Sys.sleep(3)
          pf_select_option("filtroForm\\:departamentoSel", dept)
        }
        trigger_sel <- '//*[@id="filtroForm:estacionesSel"]/ul'
        session$find_element("xpath", trigger_sel)$click()
        
        message(sprintf("      ⏳ Waiting %ds for station table (Attempt %d)...", 
                        w_table, attempt))
        Sys.sleep(w_table)
        
        panel_sel <- "#filtroForm\\:estacionesSel_panel"
        header_sel <- paste0(panel_sel, " .ui-selectcheckboxmenu-header .ui-chkbox-box")
        chk_all <- session$find_element("css selector", header_sel)
        chk_all$click()
        
        message(sprintf("      ⏳ Waiting %ds for 'Select All' confirmation...", w_confirm))
        Sys.sleep(w_confirm)
        wait_for_spinner()
        
        session$find_element("css selector", "body")$send_keys(keys$escape)
        Sys.sleep(2)
        
        final_dept_val <- session$execute_script(
          "return document.getElementById('filtroForm:departamentoSel_input').value;"
        )
        if (is.null(final_dept_val) || final_dept_val == "" || final_dept_val == "0") {
          stop("Critical: Dept reset to blank.")
        }
        TRUE 
      }, error = function(e) { return(FALSE) })
      
      if (isTRUE(res_seq)) {
        try({
          pf_select_option("filtroForm\\:contaminanteSel", param)
          trigger_par <- '//*[@id="filtroForm:contaminanteSel"]/div[3]'
          try(session$find_element("xpath", trigger_par)$click(), silent=TRUE)
          try(session$find_element("xpath", trigger_par)$click(), silent=TRUE)
          Sys.sleep(2)
          session$find_element("css selector", "body")$send_keys(keys$escape)
          return(TRUE)
        }) -> res_param
        if(isTRUE(res_param)) return(TRUE)
      }
      w_table <- w_table + 30; w_confirm <- w_confirm + 30
      session$find_element("css selector", "body")$send_keys(keys$escape)
    }
    return(FALSE) 
  }
  
  log <- list()
  
  # 4) Main Execution Loop
  # ------------------------------------------------------------------------------------
  for (dept in target_depts) {
    message(sprintf("\n📍 DEPT: %s", dept))
    safe_navigate(base_url)
    
    for (param in target_params) {
      message(sprintf("   🔹 Param: %s", param))
      
      if (!setup_form(dept, param)) {
        message("      ❌ Hard Setup Failure. Refreshing page...")
        safe_navigate(base_url)
        if(!setup_form(dept, param)) { message("      ❌ Skipping Param: ", param); next }
      }
      
      session$execute_script("document.getElementById('filtroForm:labelFIniLimite').click();")
      Sys.sleep(2) 
      input_start <- session$find_element("css selector", "#filtroForm\\:fechaIni_input")
      start_val   <- input_start$get_attribute("value")
      
      if (is.null(start_val) || start_val == "") { 
        message("      ⚠️ No data start date."); next }
      d_start_avail <- as.Date(start_val)
      d_end_limit   <- as.Date(paste0(max(years_range), "-12-31"))
      message(sprintf("      📅 Data available from: %s", d_start_avail))
      
      curr_start <- d_start_avail
      step_unit  <- "1 year" 
      
      while (curr_start < d_end_limit) {
        
        next_date_cand <- seq(curr_start, by = step_unit, length.out = 2)[2]
        curr_end <- min(d_end_limit, next_date_cand - 1)
        
        if (curr_start > Sys.Date()) break
        
        status <- "ok"
        f_path <- NA_character_
        chunk_attempt <- 0
        resize_triggered <- FALSE
        
        repeat {
          chunk_attempt <- chunk_attempt + 1
          if (chunk_attempt > 3) { status <- "failed"; break }
          
          if (chunk_attempt > 1) {
            message(sprintf("      ⚠️ Retry #%d. Refreshing session...", chunk_attempt))
            try(session$navigate("about:blank"), silent = TRUE)
            Sys.sleep(1)
            safe_navigate(base_url)
            if(!setup_form(dept, param)) { status <- "failed"; break }
          }
          
          try({
            s_start <- format(curr_start, "%Y-%m-%d")
            s_end   <- format(curr_end,   "%Y-%m-%d")
            
            # [FIX 1] Aggressive Date Injection (+ .blur())
            # We trigger 'change' AND 'blur' to ensure the server sees the update
            js_dates <- sprintf("
              var start = $('#filtroForm\\\\:fechaIni_input');
              var end   = $('#filtroForm\\\\:fechaFin_input');
              start.val('%s').trigger('change').trigger('blur');
              end.val('%s').trigger('change').trigger('blur');
            ", s_start, s_end)
            session$execute_script(js_dates)
            Sys.sleep(1)
            wait_for_spinner()
            
            # Force Hourly
            trigger_time <- '//*[@id="filtroForm:tipoSel"]/div[3]/span'
            tm_trigger <- wait_for_xpath(trigger_time, 30)
            pf_select_option("filtroForm\\:tipoSel", "Hora")
            tm_trigger$click()
            tm_trigger$click()
            
            session$find_element("css selector", "body")$send_keys(keys$escape)
            Sys.sleep(1)
            
            # [FIX 2] Clear OLD Growl messages (Stale State Fix)
            # This prevents us from reading an error from the previous loop iteration
            session$execute_script("
               $('.ui-growl-item-container').remove(); 
               $('.ui-growl').html('');
            ")
            
            message(sprintf("      🔎 Querying: %s to %s", s_start, s_end))
            session$execute_script(
              "document.getElementById('filtroForm:btnConsultar').click();")
            
            # [FIX 3] Wait for the NEW request to start. We wait 2s, then check for spinner. 
            # If spinner is there, we wait for it to finish.
            Sys.sleep(2)
            wait_for_spinner()
            
            result_found <- FALSE
            
            # Polling Loop
            for(p_wait in 1:300) {
              if (p_wait %in% c(60, 120, 200)) {
                message("      💤 Server sleepy. Clicking Consultar again...")
                session$execute_script(
                  "document.getElementById('filtroForm:btnConsultar').click();")
              }
              
              growl_txt <- session$execute_script("
                 var g = document.querySelector('div.ui-growl-item-container');
                 if (g && window.getComputedStyle(g).opacity > 0.9) return g.innerText;
                 return null;
              ")
              
              if (!is.null(growl_txt)) {
                # Case A: Too Many Rows (65k limit) -> TRIGGER RESIZE
                if (grepl("65526|mayor a|excede|No es posible descargar",
                          growl_txt, ignore.case=TRUE)) {
                  message("      📉 OVERFLOW ERROR: Chunk too big. Resizing to 3 months...")
                  resize_triggered <- TRUE
                  result_found <- TRUE
                  break 
                }
                # Case B: Standard No Data
                if (grepl("No se encontraron resultados", growl_txt, ignore.case=T)) {
                  message(sprintf("      ⚠️ No data: %s [%s]", param, s_start))
                  status <- "no_data"
                  result_found <- TRUE
                  break
                }
                # Case C: Generic Server Error
                if (grepl("error|servidor|fallo|contacte", growl_txt, ignore.case=T)) {
                  message("      ❌ Server Error Detected: ", growl_txt)
                  status <- "server_error"
                  result_found <- TRUE 
                  break
                }
              }
              
              # Check CSV
              js_click_csv <- "
                 var xpath = \"//a[.//span[contains(@class, 'fa-file-text-o')]]\";
                 var res = document.evaluate(xpath, document, null, 
                                             XPathResult.FIRST_ORDERED_NODE_TYPE, null);
                 var el = res.singleNodeValue;
                 if (el) { el.click(); return true; }
                 return false;
               "
              was_clicked <- session$execute_script(js_click_csv)
              if (isTRUE(was_clicked)) {
                Sys.sleep(5)
                files_now <- list.files(downloads_folder, full.names = TRUE)
                if (!any(difftime(Sys.time(),
                                  file.info(files_now)$mtime, units="secs") < 10)) {
                  session$execute_script(js_click_csv) # Double tap
                }
                result_found <- TRUE; status <- "downloading"; break
              }
              Sys.sleep(1)
            }
            
            # Handle Results
            if (resize_triggered) {
              step_unit <- "3 months"
              status <- "resize"
            } else if (!result_found) {
              message("      ⏰ Timeout waiting for results.")
              status <- "timeout"
            } else if (status == "downloading") {
              dl_file <- wait_for_new_download(
                downloads_folder, character(0), "\\.(csv|txt)$", 120)
              san_dept  <- sanitize_name(dept); san_param <- sanitize_name(param)
              out_name  <- sprintf("%s_%s_%s_%s.csv", san_dept, san_param, s_start, s_end)
              dest      <- file.path(final_dir, out_name)
              if (file.exists(dest)) unlink(dest)
              file.copy(dl_file, dest); unlink(dl_file)
              f_path <- dest
              message(sprintf("      ⬇️  Saved: %s", out_name))
            }
            
          }, silent = FALSE) -> chunk_res
          
          if (status == "resize") break 
          if (!inherits(chunk_res, "try-error") && status %in% c("ok","downloading")) break
          if (status == "no_data") break
          
          message("      ⚠️ Chunk failed (Status: ", status, "). Preparing retry...")
          Sys.sleep(5) 
        } 
        
        if (status == "resize") {
          message("      🔄 Restarting query with smaller chunk size (3 months)...")
          next 
        }
        
        log[[length(log)+1]] <- tibble::tibble(
          dept = dept, param = param, start = curr_start, end = curr_end, 
          status = status, file = f_path
        )
        
        if (status == "downloading") {
          message("      ♻️  Resetting session...")
          try(session$navigate("about:blank"), silent = TRUE)
          Sys.sleep(1)
          try({ safe_navigate(base_url) }, silent=TRUE)
          if(!setup_form(dept, param)) { message("      ❌ Critical Recovery Fail"); break }
        }
        
        curr_start <- curr_end + 1
      } 
    }
    message("   🧊 Dept finished. Cooling down...")
    Sys.sleep(30)
  }
  
  message("🧹 Cleaning up temp files...")
  garbage <- list.files(downloads_folder, pattern="sisaire", full.names=T, ignore.case=T)
  if(length(garbage) > 0) unlink(garbage, force=TRUE)
  
  dplyr::bind_rows(log)
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
# @Arg year            : 2005 or 2018.
# @Arg type            : [2005 Only] "BASICO" or "AMPLIADO".
# @Arg regions         : [2018 Only] Regions to fetch (e.g. Bogota).
# @Arg url             : Catalog URL. Defaults based on year if NULL.
# @Arg download_folder : Where to save ZIPs.
# @Arg overwrite       : Logical; re-download if file exists.
# @Arg retries         : Integer; max HTTP retries.
# @Arg quiet           : Logical; suppress progress.
#
# @Output              : Tibble with cols: year, target, file_path, bytes.
# @Purpose             : Download Census microdata.
#                        - 2005: Single national ZIP via helper.
#                        - 2018: Scrapes download links for specific regions.
#
# @Written_on          : 21/08/2025 (Updated 08/02/2026)
# --------------------------------------------------------------------------------------------
bogota_download_census_data <- function(
    year            = 2018,
    type            = "BASICO",
    regions         = c("Bogota", "Cundinamarca"),
    url             = NULL,
    download_folder = here::here("data", "downloads", "Census"),
    overwrite       = FALSE,
    retries         = 5,
    quiet           = FALSE
) {
  
  year <- as.numeric(year)
  dir.create(download_folder, recursive = TRUE, showWarnings = FALSE)
  
  # 1) Set default URLs
  if (is.null(url)) {
    if (year == 2005) {
      url <- paste0("https://microdatos.dane.gov.co/index.php/catalog/421/",
                    "get-microdata")
    } else {
      url <- paste0("https://microdatos.dane.gov.co/index.php/catalog/643/",
                    "get-microdata")
    }
  }
  
  targets <- list()
  
  # LOGIC FOR 2005 (Legacy Single File)
  # ---------------------------------------------------------------------------
  if (year == 2005) {
    # Assuming 'bogota_find_resource_census' exists in your environment
    res <- bogota_find_resource_census(url = url, type = type)
    
    targets[[1]] <- list(
      name     = toupper(type),
      filename = res$filename,
      href     = res$href
    )
    
  } else if (year == 2018) {
    # LOGIC FOR 2018 (Scrape Regional Links)
    # -------------------------------------------------------------------------
    if (!quiet) message("🔎 Scraping 2018 Catalog for download links...")
    
    ua <- httr::user_agent(
      sprintf("R (%s) / IDB-AirMonitoring", 
              paste(R.version$platform, R.version$version.string))
    )
    
    page_req <- httr::GET(url, ua)
    if (httr::status_code(page_req) != 200) {
      stop("Failed to access 2018 catalog page.")
    }
    
    # Parse content
    page_content <- httr::content(page_req, "text", encoding = "UTF-8")
    
    # Find links for each requested region
    for (reg in regions) {
      # Regex explanation:
      # 1. Look for title="..." containing the Region name
      # 2. Look ahead for onclick="..."
      # 3. Capture the 2nd argument of mostrarModal('file', 'URL')
      pat <- paste0(
        "title=\"[^\"]*", reg, "[^\"]*\".*?",
        "onclick=\"mostrarModal\\('[^']+'\\s*,\\s*'([^']+)"
      )
      
      match_info <- regexec(pat, page_content, ignore.case = TRUE)
      extracted  <- regmatches(page_content, match_info)[[1]]
      
      if (length(extracted) < 2) {
        warning("⚠️ Could not find download link for region: ", reg)
        next
      }
      
      clean_url <- trimws(extracted[2])
      
      targets[[length(targets) + 1]] <- list(
        name     = reg,
        filename = paste0(year, "_", reg, ".zip"),
        href     = clean_url
      )
    }
    
    if (length(targets) == 0) {
      stop("No valid download links found for requested regions.")
    }
  }
  
  # Execute downloads loop
  # ---------------------------------------------------------------------------
  results_list <- list()
  
  for (t in targets) {
    dest <- file.path(download_folder, t$filename)
    
    # A) Check Cache
    if (file.exists(dest) && !isTRUE(overwrite)) {
      if (!quiet) message("↪︎ [", t$name, "] Already present (skip).")
      sz <- suppressWarnings(file.size(dest))
      
      results_list[[length(results_list) + 1]] <- tibble::tibble(
        year      = year,
        target    = t$name,
        file_path = normalizePath(dest),
        bytes     = sz,
        status    = "cached"
      )
      next
    }
    
    # B) Download
    if (!quiet) message("⬇️  [", t$name, "] Downloading...")
    
    ua_str <- sprintf("R (%s) / IDB-Air", R.version$version.string)
    
    rq <- httr::RETRY(
      verb = "GET",
      url  = t$href,
      httr::user_agent(ua_str),
      httr::add_headers(Referer = url),
      httr::write_disk(path = dest, overwrite = TRUE),
      httr::progress(type = if (quiet) "none" else "down"),
      times = as.integer(retries),
      terminate_on = c(200L),
      quiet = quiet,
      httr::timeout(7200) # 2 hours
    )
    
    code <- httr::status_code(rq)
    
    # C) Handle Status
    st_msg <- "ok"
    
    if (code %in% c(401L, 403L)) {
      if (file.exists(dest)) unlink(dest)
      warning("Server returned ", code, " (Captcha required?) for ", t$name)
      st_msg <- "captcha_required"
      
    } else if (code != 200L) {
      if (file.exists(dest)) unlink(dest)
      warning("Download failed HTTP ", code, " for ", t$name)
      st_msg <- paste0("http_", code)
      
    } else {
      # Size check
      bytes <- suppressWarnings(file.size(dest))
      if (is.na(bytes) || bytes < 1e6) {
        warning("File too small: ", dest)
        st_msg <- "size_warning"
      } else {
        if (!quiet) message("✅ Complete: ", basename(dest))
      }
    }
    
    # D) Save Result
    results_list[[length(results_list) + 1]] <- tibble::tibble(
      year      = year,
      target    = t$name,
      file_path = if (st_msg == "ok") normalizePath(dest) else NA_character_,
      bytes     = if (file.exists(dest)) file.size(dest) else NA_real_,
      status    = st_msg
    )
  }
  
  dplyr::bind_rows(results_list)
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
# Function: bogota_filter_stations_in_metro
# @Arg       : stations_df_1    — The pre-loaded dataframe (Source I: Bogotá local stations)
# @Arg       : metadata_dir     — Path to folder with Excel files (Source II: Cundinamarca..)
# @Arg       : metro_area       — sf (MULTI)POLYGON of the metropolitan area
# @Arg       : radius_km        — numeric; max distance to keep (default 20)
# @Arg       : stations_epsg    — EPSG for lon/lat (default 4326 WGS84)
# @Arg       : out_file         — where to write the cropped GeoPackage
# @Arg       : overwrite_gpkg   — logical; overwrite output GeoPackage if exists
# @Arg       : dissolve         — logical; TRUE unions metro polygons (default TRUE)
# @Output    : sf POINT data.frame of unique stations inside/near metro_area
# @Purpose   : Merges pre-loaded station data with external Excel metadata files,
#              standardizes coordinates, and spatially filters them.
# --------------------------------------------------------------------------------------------
bogota_filter_stations_in_metro <- function(
    stations_df_1,
    metadata_dir,
    metro_area,
    radius_km      = 20,
    stations_epsg  = 4326,
    out_file       = here::here("data", "raw", "geospatial_data", "bogota", "stations.gpkg"),
    overwrite_gpkg = TRUE,
    dissolve       = TRUE
) {
  
  # 0) Dependency Checks
  if (!inherits(metro_area, "sf")) stop("'metro_area' must be an sf object.")
  if (!dir.exists(metadata_dir)) stop("Metadata directory not found: ", metadata_dir)
  
  # Ensure output directory exists
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  message("🔄 Starting Data Integration...")
  
  # PART I: Process Pre-loaded Dataframe (Bogotá D.C. Stations)
  # ----------------------------------------------------------------------------
  df1_clean <- stations_df_1 |>
    dplyr::select(
      station_name = station,
      code,
      altitude_m,
      height_m,
      locality,
      zone_type,
      station_type,
      address,
      lat, 
      lon
    ) |>
    dplyr::mutate(source = "RMCAB - Bogota")
  
  message("   ✅ Loaded ", nrow(df1_clean), " stations from primary dataframe.")
  
  # PART II: Process Excel Files (Surrounding Depts)
  # ----------------------------------------------------------------------------
  # Note: Keeping readWorksheetFromFile (XLConnect) per user requirement
  xl_files <- list.files(metadata_dir, pattern = "\\.xls$", full.names = TRUE)
  xl_files <- xl_files[!grepl("bogota_d_c", basename(xl_files), ignore.case = TRUE)]
  
  if (length(xl_files) == 0) {
    warning("No .xlsx/.xls files found (excluding bogota_d_c).")
    df2_clean <- data.frame()
  } else {
    message("   📂 Found ", length(xl_files), " external metadata files. Processing...")
    
    df2_clean <- purrr::map_dfr(xl_files, function(f) {
      # XLConnect dependency
      # Ensure 'XLConnect' is loaded: library(XLConnect)
      raw_xl <- XLConnect::readWorksheetFromFile(f, sheet = 1, startRow = 10)
      
      clean_xl <- raw_xl |>
        dplyr::select(
          station_name = `Estación`,
          address      = `Dirección`,
          first_date   = `Fecha.primer.registro`,
          last_date    = `Fecha.último.registro`,
          lat          = `Latitud`,
          lon          = `Longitud`
        ) |>
        dplyr::mutate(
          lat = as.numeric(lat),
          lon = as.numeric(lon),
          source = paste0("SISAIRE - ", file_path_sans_ext(basename(f)))
        ) |>
        dplyr::distinct(station_name, .keep_all = TRUE) |>
        tidyr::drop_na(lat, lon)
      
      return(clean_xl)
    })
    
    message("   ✅ Loaded ", nrow(df2_clean), " unique stations from Excel files.")
  }
  
  # PART III: Merge and Spatial Conversion
  # ----------------------------------------------------------------------------
  all_stations <- dplyr::bind_rows(df1_clean, df2_clean)
  if (nrow(all_stations) == 0) stop("No stations found in either source.")
  
  stations_sf <- sf::st_as_sf(
    all_stations,
    coords = c("lon", "lat"),
    crs = stations_epsg
  )
  
  # PART IV: Spatial Filter (Radius Logic)
  # ----------------------------------------------------------------------------
  message("🔄 Applying Spatial Filter (Radius: ", radius_km, "km)...")
  
  # 1. Prepare Metro Polygon
  metro_valid <- sf::st_make_valid(metro_area)
  if (dissolve) metro_valid <- sf::st_union(metro_valid)
  
  # 2. Build Safe AEQD Projection (THE FIX)
  # We convert to WGS84 first so st_centroid gives us degrees (e.g. -74.0, 4.6),
  # ensuring the PROJ string is valid.
  metro_wgs <- sf::st_transform(metro_valid, 4326)
  cen       <- sf::st_coordinates(sf::st_centroid(metro_wgs))
  
  # "The Ruler": Azimuthal Equidistant centered on Bogotá
  aeqd_proj <- sprintf(
    "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
    cen[2], cen[1]
  )
  
  # 3. Transform everything to Meters using that Ruler
  metro_m    <- sf::st_transform(metro_valid, aeqd_proj)
  stations_m <- sf::st_transform(stations_sf, aeqd_proj)
  
  # 4. Filter "From the Walls"
  # st_is_within_distance measures from the closest edge of the polygon.
  radius_m   <- radius_km * 1000
  within_idx <- sf::st_is_within_distance(stations_m, metro_m, dist = radius_m)
  keep_mask  <- lengths(within_idx) > 0
  
  stations_final <- stations_sf[keep_mask, ]
  
  message("    Filter Stats: Input=", nrow(stations_sf), 
          " -> Output=", nrow(stations_final), 
          " (Dropped ", nrow(stations_sf) - nrow(stations_final), ")")
  
  # PART V: Save Output
  # ----------------------------------------------------------------------------
  if (file.exists(out_file) && !overwrite_gpkg) {
    message("↪︎ Output exists and overwrite=FALSE. Skipping write.")
  } else {
    if (file.exists(out_file)) unlink(out_file)
    sf::st_write(stations_final, out_file, quiet = TRUE, append = FALSE)
    message("💾 Saved GeoPackage: ", out_file)
  }
  
  return(stations_final)
}


# ---------------------------------------------------------------------------------------------
# Function: bogota_process_stations_data_to_parquet
#
# @Arg      : rmcab_folder         — string; folder containing RMCAB .xlsx files.
# @Arg      : sisaire_folder       — string; folder containing SISAIRE .csv files.
# @Arg      : stations_sf          — sf object; Spatial registry of stations to keep. 
#                                    Must have column 'station_name'.
# @Arg      : out_dir              — string; base output directory.
# @Arg      : out_name             — string; name of the dataset (default "bogota_metro_air").
# @Arg      : years                — int vector; years to filter/keep (default 2008:2024).
# @Arg      : include_bogota_sisaire — logical; if TRUE, includes "bogota" files from SISAIRE.
#                                      Default FALSE (prevents duplication with RMCAB).
# @Arg      : tz                   — string; Olson timezone (default "America/Bogota").
# @Arg      : verbose              — logical; print progress messages?
#
# @Output   : Arrow Dataset connection.
#
# @Purpose  : Unified ingestion pipeline for Bogota Metro Area air quality.
#             1. Ingests RMCAB (Excel) data.
#             2. Ingests SISAIRE (CSV) data (Cundinamarca, etc.).
#             3. Normalizes all variable names (pm2.5 -> pm25) to a common schema.
#             4. Filters SISAIRE stations against the provided spatial index (stations_sf).
#             5. Pivots and writes to Partitioned Parquet via DuckDB.
#
# @Written_on: 08/09/2025
# @Written_by: Marcos Paulo
# ---------------------------------------------------------------------------------------------
bogota_process_stations_data_to_parquet <- function(
    rmcab_folder,
    sisaire_folder,
    stations_sf,
    out_dir,
    out_name               = "bogota_metro_air",
    years                  = bogota_cfg$years,
    include_bogota_sisaire = FALSE,
    tz                     = bogota_cfg$tz,
    verbose                = TRUE
) {
  # 1) Check Dependencies
  req_pkgs <- c("duckdb", "DBI", "arrow", "tidyr", "dplyr", "readr", "stringr", "sf")
  for(p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop(paste("Package", p, "required."))
  }
  
  # 2) Validate Station Index
  if (!inherits(stations_sf, "sf") || !"station_name" %in% names(stations_sf)) {
    stop("'stations_sf' must be an sf object with a 'station_name' column.")
  }
  # Create a clean look-up vector for filtering
  valid_stations <- unique(tolower(trimws(stations_sf$station_name)))
  
  # 3) Setup DuckDB
  if (verbose) message("⬜️ Starting Unified Engine...")
  
  dbdir <- tempfile("bogota_unified_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(dbdir, force = TRUE)
  }, add = TRUE)
  
  # Optimization
  DBI::dbExecute(con, "PRAGMA memory_limit='8GB';") 
  DBI::dbExecute(con, paste0("PRAGMA threads=", parallel::detectCores() - 1, ";"))
  
  # Create Staging Table
  # Note: Added 'source_type' to track RMCAB vs SISAIRE origin
  DBI::dbExecute(con, "CREATE TABLE staging (
       datetime TIMESTAMP, 
       station VARCHAR, 
       year INTEGER, 
       param VARCHAR, 
       value DOUBLE,
       source_type VARCHAR
    );"
  )
  
  total_rows <- 0
  
  # ----------------------------------------------------------------------------
  # PHASE A: Process RMCAB (Excel)
  # ----------------------------------------------------------------------------
  xlsx_files <- list.files(rmcab_folder, pattern = "\\.xlsx$", full.names = TRUE)
  
  if (length(xlsx_files) > 0) {
    if (verbose) message(sprintf("📂 Processing %d RMCAB Excel files...",
                                 length(xlsx_files)))
    
    # Internal map for RMCAB -> DB Schema
    # Fixes the pm2.5 -> pm25 issue specifically for RMCAB files
    rmcab_map <- c(
      "pm2.5"         = "pm25",
      "pm10"          = "pm10",
      "o3"            = "ozone", 
      "no"            = "no",
      "no2"           = "no2",
      "nox"           = "nox",
      "co"            = "co",
      "so2"           = "so2",
      "temperatura"   = "temp",
      "rh"            = "rh",
      "presion baro"  = "pressure",
      "radiation"     = "radiation",
      "precipitacion" = "precipitation",
      "vel viento"    = "vel_viento",
      "dir viento"    = "dir_viento"
    )
    
    for (f in xlsx_files) {
      # Use your robust reader
      df <- tryCatch(
        bogota_read_one_xlsx(f, tz = tz, verbose = FALSE),
        error = function(e) { warning("Skip Excel: ", basename(f)); return(NULL) }
      )
      if (is.null(df) || nrow(df) == 0) next
      
      # Filter Year
      df <- df[df$year %in% years, ]
      if (nrow(df) == 0) next
      
      # Normalize Column Names (The Fix for Question 2)
      # We rename columns in R before pivoting
      curr_cols <- names(df)
      for (raw_name in names(rmcab_map)) {
        if (raw_name %in% curr_cols) {
          names(df)[names(df) == raw_name] <- rmcab_map[[raw_name]]
        }
      }
      
      # Pivot
      df_long <- tidyr::pivot_longer(
        df,
        cols       = -c(datetime, station, year),
        names_to   = "param",
        values_to  = "value",
        values_drop_na = TRUE
      )
      
      if (nrow(df_long) > 0) {
        df_long$source_type <- "RMCAB"
        duckdb::dbAppendTable(con, "staging", df_long)
        total_rows <- total_rows + nrow(df_long)
      }
    }
  } else {
    if (verbose) message("⚠️ No RMCAB files found.")
  }
  
  # ----------------------------------------------------------------------------
  # PHASE B: Process SISAIRE (CSV)
  # ----------------------------------------------------------------------------
  csv_files <- list.files(sisaire_folder, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) > 0) {
    if (verbose) message(sprintf("📂 Processing %d SISAIRE CSV files...", length(csv_files)))
    
    # SISAIRE Map -> DB Schema
    sisaire_map <- c(
      "PM2.5"         = "pm25", 
      "PM10"          = "pm10", 
      "O3"            = "ozone", 
      "NO2"           = "no2", 
      "CO"            = "co",
      "SO2"           = "so2",
      "TMPR.AIR.10CM" = "temp",
      "P"             = "pressure",
      "RGlobal"       = "radiation",
      "VViento"       = "vel_viento",
      "DViento"       = "dir_viento"
    )
    
    for (f in csv_files) {
      fname <- basename(f)

      # 1. Check Bogota Filter
      if (!include_bogota_sisaire && grepl("^bogota", tolower(fname))) {
        # Skip if file starts with 'bogota' and we didn't ask for it
        next 
      }
      
      # 2. Identify Variable from Filename or Header
      # Strategy: Read header first to find the value column
      # SISAIRE header: Estacion, Fecha.inicial, Fecha.final, [VARIABLE]
      raw_header <- names(read.csv(f, nrows = 1))
      val_col    <- setdiff(raw_header, c("Estacion", "Fecha.inicial", "Fecha.final"))
      
      if (length(val_col) == 0) { warning("No data col in: ", fname); next }
      
      # Map the variable name
      db_var <- sisaire_map[[ val_col[1] ]]
      if (is.null(db_var)) {
        # If new variable appears (e.g. unknown), normalize it or skip
        # For now, we lowercase and use as is if not in map
        db_var <- tolower(val_col[1]) 
      }
      
      # 3. Read Data (using readr/data.table for speed, fallback base)
      dt <- read.csv(f, stringsAsFactors = FALSE)
      
      # 4. Standardize Columns
      # Rename 'Estacion' -> 'station', 'Fecha.inicial' -> 'datetime'
      dt <- dt |> 
        dplyr::rename(station = Estacion, raw_time = Fecha.inicial, value = !!val_col[1]) |>
        dplyr::select(station, raw_time, value)
      
      # 5. Filter Stations (Strict Matching)
      # We normalize both sides to lowercase/trimmed for matching
      dt <- dt |>
        dplyr::filter(tolower(trimws(station)) %in% valid_stations)
      
      if (nrow(dt) == 0) next 
      
      # 6. Parse DateTime (Robust)
      # SISAIRE format: "YYYY-MM-DD HH:MM" (e.g., 2016-12-22 23:00)
      # We assume they are local time Bogota
      dt$datetime <- lubridate::ymd_hm(dt$raw_time, tz = tz, quiet = TRUE)
      
      # Drop parsing failures
      dt <- dt[!is.na(dt$datetime), ]
      if (nrow(dt) == 0) next
      
      # 7. Add Metadata
      dt$year  <- lubridate::year(dt$datetime)
      dt$param <- db_var
      dt$source_type <- "SISAIRE"
      
      # Filter Years again
      dt <- dt[dt$year %in% years, ]
      
      # 8. Append to DuckDB
      # Select only matching columns
      final_chk <- dt |> dplyr::select(datetime, station, year, param, value, source_type)
      
      if (nrow(final_chk) > 0) {
        duckdb::dbAppendTable(con, "staging", final_chk)
        total_rows <- total_rows + nrow(final_chk)
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # PHASE C: Pivot & Write
  # ----------------------------------------------------------------------------
  
  if (total_rows == 0) stop("No data found in RMCAB or SISAIRE folders.")
  if (verbose) message("💾 Staging complete. Total rows: ", total_rows)
  
  # Output Path
  dataset_path <- file.path(out_dir, paste0(out_name, "_dataset"))
  if (dir.exists(dataset_path)) unlink(dataset_path, recursive = TRUE)
  
  # Pivot Query (Updated for Normalized Schema)
  # Note: Since we normalized 'pm2.5' -> 'pm25' BEFORE inserting,
  #       we just query for 'pm25' here.
  sql_pivot <- "
    COPY (
      SELECT 
        datetime,
        station,
        year,
        source_type, -- Kept for lineage (might cause split rows if stn has both sources)
        -- Pollutants
        AVG(CASE WHEN param = 'pm10'    THEN value END) AS pm10,
        AVG(CASE WHEN param = 'pm25'    THEN value END) AS pm25, -- Captures normalized input
        AVG(CASE WHEN param = 'ozone'   THEN value END) AS ozone,
        AVG(CASE WHEN param = 'no'      THEN value END) AS no,
        AVG(CASE WHEN param = 'no2'     THEN value END) AS no2,
        AVG(CASE WHEN param = 'nox'     THEN value END) AS nox,
        AVG(CASE WHEN param = 'co'      THEN value END) AS co,
        AVG(CASE WHEN param = 'so2'     THEN value END) AS so2,
        -- Meteorology
        AVG(CASE WHEN param = 'temp'        THEN value END) AS temperature,
        AVG(CASE WHEN param = 'rh'          THEN value END) AS rh,
        AVG(CASE WHEN param = 'pressure'    THEN value END) AS pressure,
        AVG(CASE WHEN param = 'radiation'   THEN value END) AS radiation,
        AVG(CASE WHEN param = 'precipitation' THEN value END) AS precipitation,
        AVG(CASE WHEN param = 'vel_viento'  THEN value END) AS wind_speed,
        AVG(CASE WHEN param = 'dir_viento'  THEN value END) AS wind_dir
      FROM staging
      -- Group by source_type too if you want to distinguish rows
      -- Or remove it from GROUP BY to merge sources (taking AVG of overlaps)
      GROUP BY datetime, station, year, source_type
      ORDER BY station, datetime
    ) TO '%s' (
      FORMAT PARQUET, 
      PARTITION_BY (year), 
      COMPRESSION 'SNAPPY', 
      OVERWRITE_OR_IGNORE TRUE
    );
  "
  
  query <- sprintf(sql_pivot, dataset_path)
  
  if (verbose) message("🧱 Pivoting and writing Partitioned Parquet...")
  DBI::dbExecute(con, query)
  
  if (verbose) message("✅ Done! Dataset at: ", dataset_path)
  
  return(arrow::open_dataset(dataset_path))
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
# Function: bogota_filter_census
# @Arg       : census_zip   — path to CG2005_AMPLIADO.zip
# @Arg       : out_dir      — where to write selected dept folders with CSVs
# @Arg       : overwrite    — re-extract if output exists (default FALSE)
# @Arg       : quiet        — suppress messages (default FALSE)
# @Output    : list with $bogota and $cundinamarca
#              - dir_extracted (folder containing only CSV files)
#              - files_index   (tibble with file, size, type)
# @Purpose   : Fast extract master ZIP, fix mojibake, unpack dept 11 & 25,
#              unpack *CSV.zip* inside, and FLATTEN structure (CSVs to root).
#              Fixed to handle re-runs gracefully without "copy to self" errors.
# @Written_on: 22/10/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_filter_census <- function(
    census_zip = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip"),
    out_dir    = here::here("data", "raw", "census", "Bogota", "CG2005"),
    overwrite  = FALSE,
    quiet      = FALSE
) {
  if (!file.exists(census_zip)) stop("Master ZIP not found: ", census_zip)
  for (pkg in c("tibble", "tools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.")
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- helpers ----------------------------------------------------------------
  
  fix_visible_glyphs <- function(x) {
    repl <- c("†" = "á", "°" = "í", "¢" = "ó", "§" = "ñ", "Ç" = "é")
    for (k in names(repl)) x <- gsub(k, repl[[k]], x, fixed = TRUE)
    x
  }
  
  rename_tree_utf8 <- function(root) {
    paths <- list.files(root, recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
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
  
  make_index_tbl <- function(root) {
    files <- list.files(root, recursive = TRUE, full.names = TRUE)
    if (!length(files)) {
      return(tibble::tibble(file = character(), size = numeric(), type = character()))
    }
    tibble::tibble(
      file = files, size = file.info(files)$size, type = tools::file_ext(files)
    )
  }
  
  sys_extract_all <- function(zipfile, exdir, overwrite = FALSE, quiet = FALSE) {
    if (dir.exists(exdir) && overwrite) unlink(exdir, recursive = TRUE, force = TRUE)
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    
    is_mac  <- identical(Sys.info()[["sysname"]], "Darwin")
    has_dit <- nzchar(Sys.which("ditto"))
    has_unz <- nzchar(Sys.which("unzip"))
    status  <- 1L
    
    if (is_mac && has_dit) {
      args <- c("-x", "-k", shQuote(normalizePath(zipfile)), shQuote(normalizePath(exdir)))
      status <- system2("ditto", args, stdout = if (quiet) FALSE else "",
                        stderr = if (quiet) FALSE else "")
    } else if (has_unz) {
      args   <- c("-o", if (quiet) "-qq" else NULL,
                  shQuote(normalizePath(zipfile)), "-d", shQuote(normalizePath(exdir)))
      status <- system2("unzip", args, stdout = if (quiet) FALSE else "",
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
    rename_tree_utf8(master_dir) 
  } else if (!quiet) {
    message("↪︎ Master already extracted: ", master_dir)
  }
  
  # ---- 2) Locate department ZIPs by numeric prefix (11 & 25) ------------------
  all_zips <- list.files(master_dir, pattern = "\\.[Zz][Ii][Pp]$", recursive = TRUE,
                         full.names = TRUE)
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
    if (!length(cand)) stop("Could not locate ZIP for code ", code)
    cand[which.min(nchar(bf[cand]))]
  }
  
  bogota_zip_src       <- all_zips[pick_by_code(11)]
  cundinamarca_zip_src <- all_zips[pick_by_code(25)]
  
  bog_dir <- file.path(out_dir, "11.Bogotá")
  cun_dir <- file.path(out_dir, "25. Cundinamarca")
  
  # ---- 3) Extract each dept ZIP, then extract CSV.zip and keep only CSVs -----
  dept_extract_csv_only <- function(zip_src, label, tgt_dir) {
    
    # 3a) Extract department package
    if (!dir.exists(tgt_dir) || isTRUE(overwrite)) {
      if (dir.exists(tgt_dir)) unlink(tgt_dir, recursive = TRUE, force = TRUE)
      if (!quiet) message("📤 Extracting ", label, " → ", tgt_dir)
      sys_extract_all(zip_src, tgt_dir, overwrite = TRUE, quiet = quiet)
      rename_tree_utf8(tgt_dir)
    } else if (!quiet) {
      message("↪︎ Using existing folder: ", tgt_dir)
    }
    
    # 3b) Look for *CSV.zip* inside
    csv_zip <- list.files(tgt_dir, pattern = "CSV\\.[Zz][Ii][Pp]$", full.names = TRUE,
                          ignore.case = TRUE)
    
    if (length(csv_zip) > 0) {
      # Case A: Found a CSV.zip. Extract to stage, move to root.
      stage <- file.path(tgt_dir, "_csv_stage")
      sys_extract_all(csv_zip[1], stage, overwrite = TRUE, quiet = quiet)
      rename_tree_utf8(stage)
      
      csvs <- list.files(stage, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE,
                         ignore.case = TRUE)
      if (!length(csvs)) stop("CSV.zip contained no .csv files: ", csv_zip[1])
      
      for (f in csvs) {
        dest <- file.path(tgt_dir, fix_visible_glyphs(basename(f)))
        # Only copy if we aren't copying a file onto itself (unlikely here, but safe)
        if (normalizePath(f, mustWork = FALSE) != normalizePath(dest, mustWork = FALSE)) {
          file.copy(f, dest, overwrite = TRUE)
        }
      }
      unlink(stage, recursive = TRUE, force = TRUE)
      
    } else {
      # Case B: Fallback (ZIP missing, maybe already extracted?). Look for ANY CSVs.
      csvs_fallback <- list.files(tgt_dir, pattern = "\\.csv$", recursive = TRUE,
                                  full.names = TRUE, ignore.case = TRUE)
      
      if (!length(csvs_fallback)) {
        stop("No CSV.zip and no .csv files found inside: ", tgt_dir)
      }
      
      # Move fallback CSVs to tgt_dir root (Flattening)
      for (f in csvs_fallback) {
        dest <- file.path(tgt_dir, fix_visible_glyphs(basename(f)))
        
        # --- FIX IS HERE: Check if source != destination ---
        # If the file is already at the root with the correct name, skip copy to avoid error.
        if (normalizePath(f, mustWork = FALSE) != normalizePath(dest, mustWork = FALSE)) {
          file.copy(f, dest, overwrite = TRUE)
        }
      }
    }
    
    # 3d) Cleanup: Remove leftover ZIPs
    zips_left <- list.files(tgt_dir, pattern = "\\.[Zz][Ii][Pp]$",
                            full.names = TRUE, recursive = TRUE)
    if (length(zips_left)) file.remove(zips_left)
    
    # Cleanup: Remove subdirectories (flattening complete)
    subdirs <- list.dirs(tgt_dir, recursive = TRUE, full.names = TRUE)
    subdirs <- subdirs[subdirs != tgt_dir] # Don't delete the root itself
    if (length(subdirs)) {
      # Sort by length desc to delete deepest first
      subdirs <- subdirs[order(nchar(subdirs), decreasing = TRUE)]
      for (d in subdirs) unlink(d, recursive = TRUE, force = TRUE)
    }
    
    tibble::tibble(dir_extracted = tgt_dir)
  }
  
  idx_bog <- dept_extract_csv_only(bogota_zip_src, "Bogotá (11)", bog_dir)
  idx_cun <- dept_extract_csv_only(cundinamarca_zip_src, "Cundinamarca (25)", cun_dir)
  
  # ---- 4) Build indexes -------------------------------------------------------
  bogota_files       <- make_index_tbl(bog_dir)
  cundinamarca_files <- make_index_tbl(cun_dir)
  
  if (!quiet) {
    message("🧾 Bogotá CSVs: ", sum(tolower(bogota_files$type) == "csv"),
            " | Cundinamarca CSVs: ", sum(tolower(cundinamarca_files$type) == "csv"))
  }
  
  # ---- 5) Return --------------------------------------------------------------
  list(
    bogota = list(dir_extracted = bog_dir, files_index = bogota_files),
    cundinamarca = list(dir_extracted = cun_dir, files_index = cundinamarca_files)
  )
}


# --------------------------------------------------------------------------------------------
# Function: bogota_harmonize_census_data
# @Arg extract_list : List output from bogota_filter_harmonize_census
# @Arg is_extended  : Logical; True (Default) if the census data is the extended version
# @Arg metro_codes  : Vector of municipality codes for Cundinamarca filtering
# @Arg out_dir      : Where to save the processed individual and collapsed data
# @Arg quiet        : Suppress progress messages
#
# @Output           : Saves CSV/RData files; returns list of processed dataframes
# @Purpose          : Replicates Stata logic: Harmonizes education, creates labor/demographic
#                     dummies, filters adults (25+), and collapses to geographic level.
#
# @Written_on       : 21/01/2026
# @Written_by       : Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_harmonize_census_data <- function(
    extract_list,
    is_extended  = TRUE,
    metro_codes  = bogota_cfg$city_code_metro,
    out_dir      = here::here("data", "working_data"),
    quiet        = FALSE
) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required.")
  if (!requireNamespace("vroom", quietly = TRUE)) stop("vroom required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("stringr required.")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- Helper: Education Mapping (Unified for both versions) ---
  harmonize_education <- function(df) {
    cols <- names(df)
    
    if ("P44B3_NIVEL_ANOS" %in% cols) {
      # Extended Census Mapping
      df <- df %>%
        dplyr::mutate(
          VAL = as.numeric(P44B3_NIVEL_ANOS),
          VAL = dplyr::na_if(VAL, 98),
          VAL = dplyr::na_if(VAL, 99),
          VAL = ifelse(VAL == 43, 0, VAL),
          escolaridad = dplyr::case_when(
            VAL %in% 0:3   ~ 0,
            VAL == 4       ~ 1,  
            VAL == 5       ~ 2,  
            VAL == 6       ~ 3,
            VAL == 7       ~ 4,  
            VAL == 8       ~ 5,  
            VAL == 9       ~ 6,
            VAL == 10      ~ 7,  
            VAL == 11      ~ 8,  
            VAL == 12      ~ 9,
            VAL == 13      ~ 10, 
            VAL == 14      ~ 12, # Bachillerato
            VAL %in% c(15, 17) ~ 10,
            VAL %in% c(16, 18) ~ 12,
            VAL %in% c(19, 21, 23, 26) ~ 13,
            VAL %in% c(20, 22, 24, 27) ~ 14,
            VAL %in% c(25, 28) ~ 15,
            VAL == 29      ~ 16,
            VAL %in% 30:33 ~ 17, # Profesional
            VAL == 34      ~ 18,
            VAL %in% 35:36 ~ 19,
            VAL == 37      ~ 20,
            VAL == 38      ~ 21,
            VAL == 39      ~ 22,
            VAL %in% 40:42 ~ 23,
            TRUE           ~ NA_real_
          )
        )
    } else if ("P44B1_TIP_ESTUD" %in% cols) {
      # Basic Census Mapping (Specific values provided)
      df <- df %>%
        dplyr::mutate(
          VAL = as.numeric(P44B1_TIP_ESTUD),
          escolaridad = dplyr::case_when(
            VAL == 13    ~ 0,  # None
            VAL == 1     ~ 0,  # Preschool
            VAL == 2     ~ 5,  # Primary
            VAL == 3     ~ 9,  # Secondary
            VAL %in% 4:5 ~ 11, # High School
            VAL == 6     ~ 13, # Normalista
            VAL == 7     ~ 14, # Technical
            VAL == 8     ~ 15, # Technological
            VAL == 9     ~ 17, # Professional
            VAL == 10    ~ 18, # Specialization
            VAL == 11    ~ 20, # Master's
            VAL == 12    ~ 23, # Doctorate
            TRUE         ~ NA_real_
          )
        )
    }
    return(df %>% dplyr::select(-dplyr::any_of("VAL")))
  }
  
  # --- Internal Processing Pipeline ---
  process_file <- function(path, type, extended) {
    if (!quiet) message("📖 Processing ", type, " | File: ", basename(path))
    
    # I. Read all columns as character to avoid guessing errors
    # Use col_types = vroom::cols(.default = "c")
    raw <- vroom::vroom(
      path, 
      col_types = vroom::cols(.default = "c"), 
      show_col_types = FALSE,
      delim = ",",           # Ensure this matches your CSV format
      trim_ws = TRUE,        # Automatically removes leading/trailing spaces
      na = c("", " ", "NA")  # Define common null values found in DANE files
    ) %>% 
      dplyr::rename_with(toupper)
    
    # II. Basic uses full 22-chr MGN code; Extended uses Municipal/Locality ID
    df <- raw %>%
      dplyr::mutate(
        LocComuna = if (type == "bogota") {
          stringr::str_pad(I0B1_LOCALIDAD, 2, "left", "0")
        } else NA,
        LocCodigo = paste0(
          stringr::str_pad(I0A_DPTO, 2, "left", "0"),
          stringr::str_pad(I0B_MPIO, 3, "left", "0")),
        GEO_ID = if (!extended) {
          # Full MGN concatenation for Basic Census
          stringr::str_c(
            stringr::str_pad(I0A_DPTO, 2, "left", "0"),
            stringr::str_pad(I0B_MPIO, 3, "left", "0"),
            stringr::str_pad(I0C_CLASE, 1, "left", "0"),
            stringr::str_pad(I0D_SECT_R, 3, "left", "0"),
            stringr::str_pad(I0D_SEC_R, 2, "left", "0"),
            stringr::str_pad(I0D_CPOB, 3, "left", "0"),
            stringr::str_pad(I0D_SECT_U, 4, "left", "0"),
            stringr::str_pad(I0D_SEC_U, 2, "left", "0"),
            stringr::str_pad(I0D_MZA, 2, "left", "0")
          )
        } else if (extended & type == "bogota") {
          paste0(LocCodigo, LocComuna)
        } else {
          LocCodigo # Fallback to Locality for Extended
        }
      ) %>%
      # 2. Filter Metro Area (based on Municipality part of ID)
      dplyr::filter(stringr::str_sub(GEO_ID, 1, 5) %in% metro_codes) %>%
      # 3. Core Indicators
      dplyr::mutate(
        raw_age = if ("PC09B_EDAD" %in% names(.)) {
          as.numeric(PC09B_EDAD)} else {as.numeric(PC09B_EDADQ)},
        # Convert to "roughly real age" if it's the group version
        edad = if (extended) {
          raw_age} else {(raw_age - 1) * 5 + 2.5},
        fe    = if ("FACT_EXP_CAL_P_N" %in% names(.)) 
          round(as.numeric(FACT_EXP_CAL_P_N)) else 1,
        women = as.numeric(P25B_SEXO == 2),
        adult = if (extended) {
          as.numeric(raw_age >= 25)} else {as.numeric(raw_age >= 6) # Group 6 is 25-29 years
            }
      ) %>%
      harmonize_education() %>%
      dplyr::mutate(
        no_education         = as.numeric(escolaridad == 0),
        high_school_complete = as.numeric(escolaridad %in% 11:12),
        college_complete     = as.numeric(escolaridad == 17),
        graduate_educ        = as.numeric(escolaridad >= 18),
        employed             = if ("P47B_OCUPACION" %in% names(.)) {
          ifelse(edad >= 25, 
                 as.numeric(P47B_OCUPACION %in% 1:2), 
                 NA_real_)
        } else NA_real_
      )
    
    return(df)
  }
  
  # --- Execution ---
  bog_path <- list.files(extract_list$bogota$dir_extracted, 
                         pattern = "PERH", full.names = TRUE)[1]
  cun_path <- list.files(extract_list$cundinamarca$dir_extracted, 
                         pattern = "PERH", full.names = TRUE)[1]
  
  all_census <- dplyr::bind_rows(
    process_file(bog_path, "bogota", is_extended),
    process_file(cun_path, "cundinamarca", is_extended)
  )
  
  # --- Aggregation ---
  group_var <- "GEO_ID"
  
  collapse_data <- all_census %>%
    dplyr::filter(adult == 1) %>%
    dplyr::group_by(!!dplyr::sym(group_var)) %>%
    dplyr::summarise(
      n = sum(fe, na.rm = TRUE),
      escolaridad_avg = weighted.mean(escolaridad, fe, na.rm = TRUE),
      dplyr::across(c(no_education, high_school_complete, college_complete, 
                      graduate_educ, employed),
                    ~ sum(.x * fe, na.rm = TRUE))
    ) %>%
    dplyr::mutate(dplyr::across(c(no_education, high_school_complete, 
                                  college_complete, graduate_educ, employed),
                                ~ .x / n, .names = "share_{.col}_pop"))
  
  # --- Export ---
  prefix <- if (is_extended) "extended" else "basic"
  vroom::vroom_write(all_census, file.path(out_dir, 
                                           paste0("census_metro_individual_", prefix, ".csv")), 
                     delim = ",")
  vroom::vroom_write(collapse_data, file.path(out_dir, 
                                              paste0("collapse_metro_area_", prefix, ".csv")), 
                     delim = ",")
  
  return(list(individual = all_census, collapsed = collapse_data))
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