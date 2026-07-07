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
  # Core identity
  id               = "bogota",
  tz               = "America/Bogota",
  # Download URLs
  url_station_shp  = "https://www.ambientebogota.gov.co/estaciones-rmcab",
  base_url_shp     = "https://www.dane.gov.co/files/geoportal-provisional",
  base_url_rmcab   = "http://rmcab.ambientebogota.gov.co/Report/stationreport",
  base_url_sisaire = "http://sisaire.ideam.gov.co/ideam-sisaire-web/consultas.xhtml",
  base_url_census  = paste0(
    "https://microdatos.dane.gov.co/index.php/catalog/421/get-microdata"),
  base_new_census  = paste0(
    "https://microdatos.dane.gov.co/index.php/catalog/643/get-microdata"),
  base_url_comunas = paste0(
    "https://datosabiertos.bogota.gov.co/dataset/localidad-bogota-d-c"),
  # Processing parameters
  years           = 2000L:2023L,
  dl_dir          = here::here("data", "downloads", "bogota"),
  out_dir         = here::here("data", "raw"),
  which_states    = c("Bogotá D.C.", "Cundinamarca", "Huila", "Meta", "Tolima"),
  cities_in_metro = c(
    "Bogotá DC", "Bojacá", "Cajicá", "Chía", "Cota", "El Rosal", "Facatativá", "Funza",
    "Fusagasugá", "Gachancipá", "La Calera", "Madrid", "Mosquera", "Sibaté", "Soacha",
    "Sopó", "Subachoque", "Tabio", "Tenjo", "Tocancipá", "Zipaquirá"),
  city_code_metro = c(
    "25099", "25126", "25214", "25269", "25286", "25295", "25377", "25430", "25758", "25769",
    "25785", "25799", "25817", "25899", "11001", "25754", "25175", "25260", "25473", "25740",
    "25290"),
  station_nme_map = c("Centro de alto rendimiento" = "Centro de Alto Rendimiento",
                      "Las Ferias"                 = "Las Ferias",
                      "Carvajal-Sevillana"         = "Carvajal-Sevillana")
  )

# ============================================================================================
#  Bogotá-specific functions - downloading and its helpers
# ============================================================================================
# --------------------------------------------------------------------------------------------
# Function: bogota_download_metro_area
# @Arg       : level              — character; "mpio", "depto", "manzana", or "mpio_localidad". 
# @Arg       : mgn_year           — numeric; 2018 or 2005. 
# @Arg       : base_url           — string; base URL of DANE geoportal files.
# @Arg       : municipality_codes — character vector of 5-digit codes (e.g. "11001").
# @Arg       : download_dir       — character; where to save the ZIP.
# @Arg       : out_file           — character; where to write the cropped GeoPackage.
# @Arg       : overwrite_zip      — logical; re-download if ZIP exists.
# @Arg       : overwrite_gpkg     — logical; overwrite output GeoPackage if exists.
# @Arg       : quiet              — logical; suppress progress.
# 
# @Output    : Writes a GeoPackage; returns sf object invisibly.
# @Purpose   : Download admin boundaries and crop to Bogota metro.
#              "mpio_localidad" replaces Bogota with a clipped 
#              version of the official Localities GPKG.
# @Written_on: 20/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_download_metro_area <- function(
    level              = c("mpio", "depto", "manzana", "mpio_localidad"),
    mgn_year           = c(2018, 2005),
    base_url           = bogota_cfg$base_url_shp,
    municipality_codes = bogota_cfg$city_code_metro,
    download_dir       = here::here("data", "downloads", "Administrative", "Colombia"),
    out_file           = here::here("data", "raw", "admin", "Colombia", "bogota.gpkg"),
    overwrite_zip      = FALSE,
    overwrite_gpkg     = TRUE,
    quiet              = FALSE
) {
  
  level <- match.arg(
    tolower(level), c("mpio", "depto", "manzana", "mpio_localidad")
  )
  mgn_year <- as.numeric(match.arg(as.character(mgn_year), c("2018", "2005")))
  
  # 1) Define ZIP names and URLs
  # ---------------------------------------------------------------------------
  if (level == "mpio_localidad") {
    zip_names <- if (mgn_year == 2018) "SHP_MGN2018_INTGRD_MPIO.zip" else 
      "SHP_MGN2005_COLOMBIA.zip"
  } else if (mgn_year == 2018) {
    if (level == "manzana") {
      zip_names <- c("SHP_MGN2018_INTGRD_MANZ.zip",   
                     "SHP_MGN2018_INTGRD_SECCR.zip")  
    } else {
      zip_names <- switch(level,
                          "mpio"  = "SHP_MGN2018_INTGRD_MPIO.zip",
                          "depto" = "SHP_MGN2018_INTGRD_DEPTO.zip")
    }
  } else {
    zip_names <- "SHP_MGN2005_COLOMBIA.zip"
  }
  
  loc_url <- paste0(
    "https://web.archive.org/web/20251018145049/https://",
    "datosabiertos.bogota.gov.co/dataset/856cb657-8ca3-4ee8-857f-",
    "37211173b1f8/resource/b6c3fbda-1281-4735-8063-260e75ad95f8/",
    "download/loca.gpkg"
  )
  loc_path <- file.path(download_dir, "bogota_loca.gpkg")
  
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  exdir <- file.path(tempdir(), paste0("col_shp_", level, "_", mgn_year))
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE)
  dir.create(exdir)
  
  # 2) Loop Download & Extract
  # ---------------------------------------------------------------------------
  for (zip_name in zip_names) {
    zip_url  <- file.path(base_url, zip_name)
    zip_path <- file.path(download_dir, zip_name)
    
    if (file.exists(zip_path) && !overwrite_zip) {
      if (!quiet) message("↪︎ ZIP already present: ", zip_name)
    } else {
      if (!quiet) message("⬇️  Downloading ", zip_name, " ...")
      req <- httr::RETRY("GET", zip_url, 
                         httr::write_disk(zip_path, overwrite = TRUE), 
                         times = 5, httr::timeout(3600))
      if (httr::status_code(req) != 200L) stop("Download failed.")
    }
    if (!quiet) message("📦 Extracting ", zip_name, "...")
    archive::archive_extract(zip_path, dir = exdir)
  }
  
  if (level == "mpio_localidad") {
    if (file.exists(loc_path) && !overwrite_zip) {
      if (!quiet) message("↪︎ GPKG already present: bogota_loca.gpkg")
    } else {
      if (!quiet) message("⬇️  Downloading bogota_loca.gpkg ...")
      req_loc <- httr::RETRY("GET", loc_url, 
                             httr::write_disk(loc_path, overwrite = TRUE), 
                             times = 5, httr::timeout(3600))
      if (httr::status_code(req_loc) != 200L) stop("GPKG Download failed.")
    }
  }
  
  # 3) Processing Logic
  # ---------------------------------------------------------------------------
  if (level == "mpio_localidad") {
    if (!quiet) message("🧩 Merging Mpios with Clipped GPKG Localities...")
    
    pat_mp <- if(mgn_year == 2005) "MGN_Municipio\\.shp$" else "(?i)MPIO.*\\.shp$"
    path_mpio <- list.files(
      exdir, pattern = pat_mp, recursive = TRUE, full.names = TRUE
    )
    if(length(path_mpio) == 0) stop("MPIO Shapefile not found.")
    path_mpio <- path_mpio[which.max(file.info(path_mpio)$size)]
    
    mpio_all <- sf::st_read(path_mpio, quiet = TRUE) %>%
      dplyr::rename_with(toupper)
    sf::st_geometry(mpio_all) <- "geometry"
    
    mpio_all <- mpio_all %>%
      dplyr::mutate(
        MPIO_FULL = if ("MPIO_CDPMP" %in% names(.)) {
          as.character(MPIO_CDPMP)
        } else {
          paste0(stringr::str_pad(DPTO_DPTO_, 2, pad="0"), 
                 stringr::str_pad(MPIO_CCDGO, 3, pad="0"))
        },
        GEO_ID = MPIO_FULL,
        TIPO = "Municipio"
      )
    
    mpio_sf <- mpio_all %>%
      dplyr::filter(MPIO_FULL %in% municipality_codes & MPIO_FULL != "11001") %>%
      dplyr::select(
        MPIO_FULL, GEO_ID, TIPO, 
        dplyr::any_of(c("MPIO_CNMBR", "DPTO_CCDGO", "MPIO_CCDGO")), 
        dplyr::everything()
      )
    
    bogota_base <- mpio_all %>%
      dplyr::filter(MPIO_FULL == "11001") %>%
      sf::st_union()
    
    locs_sf <- sf::st_read(loc_path, quiet = TRUE)
    sf::st_geometry(locs_sf) <- "geometry"
    locs_sf <- sf::st_transform(locs_sf, sf::st_crs(mpio_all))
    
    suppressWarnings({
      locs_clipped <- sf::st_intersection(locs_sf, bogota_base) %>%
        sf::st_cast("MULTIPOLYGON")
    })
    
    locs_clipped <- locs_clipped %>%
      dplyr::mutate(
        MPIO_FULL = "11001",
        MPIO_CNMBR = LocNombre,
        GEO_ID = paste0("11001", stringr::str_pad(LocCodigo, 2, pad="0")),
        TIPO = "Localidad"
      ) %>%
      dplyr::select(MPIO_FULL, GEO_ID, TIPO, MPIO_CNMBR, dplyr::everything())
    
    g_sel <- dplyr::bind_rows(mpio_sf, locs_clipped)
    
  } else if (mgn_year == 2005) {
    if (level == "manzana") {
      if (!quiet) message("🧩 [2005] Merging Urban, Rural, and C. Poblado...")
      
      path_mza <- list.files(
        exdir, pattern = "MGN_Manzana\\.shp$", 
        recursive = TRUE, full.names = TRUE
      )
      path_rur <- list.files(
        exdir, pattern = "MGN_Seccion_rural\\.shp$", 
        recursive = TRUE, full.names = TRUE
      )
      path_cpob <- list.files(
        exdir, pattern = "MGN_CentroPoblado\\.shp$", 
        recursive = TRUE, full.names = TRUE
      )
      
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
      
      r_processed <- sf::st_read(path_rur, quiet = TRUE) %>%
        dplyr::rename_with(toupper) %>%
        dplyr::mutate(
          MPIO_FULL = paste0(stringr::str_pad(SETR_CLSE_, 2, pad="0"), 
                             stringr::str_pad(SETR_CLSE1, 3, pad="0")),
          TIPO      = "Rural Section",
          GEO_ID    = SECR_CCNCT
        ) %>%
        dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
        dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      
      c_processed <- sf::st_read(path_cpob, quiet = TRUE) %>%
        dplyr::rename_with(toupper) %>%
        dplyr::mutate(
          MPIO_FULL = substr(CPOB_CCNCT, 1, 5),
          TIPO      = "Centro Poblado",
          GEO_ID    = CPOB_CCNCT
        ) %>%
        dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
        dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      
      g_sel <- dplyr::bind_rows(u_processed, r_processed, c_processed)
      
    } else {
      pat <- if (level == "mpio") "MGN_Municipio\\.shp$" else 
        "MGN_Departamento\\.shp$"
      target_shp <- list.files(
        exdir, pattern = pat, recursive = TRUE, full.names = TRUE
      )
      
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
    if (level == "manzana") {
      if (!quiet) message("🧩 [2018] Merging Urban and Rural Sections...")
      
      path_mza <- list.files(
        exdir, pattern = "(?i)MANZ.*\\.shp$", 
        recursive = TRUE, full.names = TRUE
      )
      path_rur <- list.files(
        exdir, pattern = "(?i)(SECCION|SECCR).*\\.shp$", 
        recursive = TRUE, full.names = TRUE
      )
      
      if (length(path_mza) > 0) {
        path_mza <- path_mza[which.max(file.info(path_mza)$size)] 
        u_processed <- sf::st_read(path_mza, quiet = TRUE) %>%
          dplyr::rename_with(toupper) %>%
          dplyr::mutate(
            MPIO_FULL = as.character(MPIO_CDPMP), 
            TIPO      = "Urban Block",
            GEO_ID    = as.character(COD_DANE_A) 
          ) %>%
          dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
          dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      } else {
        u_processed <- NULL
      }
      
      if (length(path_rur) > 0) {
        path_rur <- path_rur[which.max(file.info(path_rur)$size)] 
        r_processed <- sf::st_read(path_rur, quiet = TRUE) %>%
          dplyr::rename_with(toupper) %>%
          dplyr::mutate(
            MPIO_FULL = as.character(MPIO_CDPMP),
            TIPO      = "Rural Section",
            GEO_ID    = as.character(SECR_CCNCT)
          ) %>%
          dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
          dplyr::select(MPIO_FULL, TIPO, GEO_ID, dplyr::everything())
      } else {
        r_processed <- NULL
      }
      
      g_sel <- dplyr::bind_rows(u_processed, r_processed)
      
    } else {
      pat <- if(level == "mpio") "(?i)MPIO.*\\.shp$" else "(?i)DEPTO.*\\.shp$"
      target_shp <- list.files(
        exdir, pattern = pat, recursive = TRUE, full.names = TRUE
      )
      target_shp <- target_shp[which.max(file.info(target_shp)$size)]
      
      g_sel <- sf::st_read(target_shp, quiet = TRUE) %>%
        dplyr::rename_with(toupper) %>%
        dplyr::mutate(
          MPIO_FULL = if("MPIO_CDPMP" %in% names(.)) {
            as.character(MPIO_CDPMP) 
          } else {
            paste0(DPTO_CCDGO, MPIO_CCDGO)
          }
        ) %>%
        dplyr::filter(MPIO_FULL %in% municipality_codes) %>%
        dplyr::select(MPIO_FULL, dplyr::everything())
    }
  }
  
  # 4) Export
  # ---------------------------------------------------------------------------
  if (!quiet) message("💾 Writing GeoPackage → ", basename(out_file))
  sf::st_write(g_sel, out_file, delete_dsn = overwrite_gpkg, quiet = TRUE)
  
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
# 
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
# 
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
# 
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
        Sys.sleep(15)

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
        # Making a double click for Bogotá DC case - very slow / bugged
        if (dept == "Bogotá D.C.") row_link$click()
        Sys.sleep(3)
        row_link$click()
        
        # Wait for Detail View
        Sys.sleep(80)
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
# @Purpose  : Robustly scrape hourly air quality data from the SISAIRE/IDEAM legacy app.
#             Uses advanced Selenium strategies to bypass PrimeFaces/JSF limitations:
#             1. "Direct Injection": Manipulates hidden <select> via JS.
#             2. "Adaptive Retry": Increases wait times dynamically if the server lags.
#             3. "Session Recovery": Forces a browser refresh if a specific date chunk
#                fails, preventing "zombie" sessions.
# @Written_on: 09/10/2025
# @Written_by: Marcos Paulo
# ----------------------------------------------------------------------------------------
sisaire_download_hourly_data <- function(
    base_url      = bogota_cfg$base_url_sisaire,
    target_depts  = bogota_cfg$which_states,
    target_params = NULL,
    years_range   = bogota_cfg$years,
    container     = TRUE,
    subdir        = "stations_hourly",
    timeout       = 180
) {
  
  # 0) Directories & Docker Setup
  downloads_folder <- Sys.getenv("DOWNLOADS_DIR", 
                                 here::here("data", "downloads"))
  dir.create(downloads_folder, recursive = TRUE, showWarnings = FALSE)
  
  final_dir <- file.path(downloads_folder, subdir)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("✔️  Downloads dir: ", downloads_folder)
  message("📂 Target subdir : ", final_dir)
  
  if (is.null(target_params)) {
    target_params <- c("PM10", "PM2.5", "O3", "NO2", "SO2", "CO", 
                       "TMPR_AIR_10CM", "VViento", "DViento", "P", "RGlobal")
  }
  
  # 1) Docker / Selenium Connection
  if (!container) {
    message("🚀 Starting local Selenium on 4445...")
    img <- "selenium/standalone-firefox:4.34.0-20250717"
    cmd <- paste("docker run -d -p 4445:4444 --shm-size=2g", img)
    cid <- system(cmd, intern = TRUE)
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern = TRUE), 
                silent = TRUE), add = TRUE)
    selenium_host <- "localhost"; selenium_port <- 4445L
  } else {
    selenium_host <- "selenium";  selenium_port <- 4444L
  }
  
  dl_dir_cont <- if (container) "/home/seluser/Downloads" else downloads_folder
  
  mimes <- paste(
    "text/csv", "application/csv", "text/plain", 
    "application/vnd.ms-excel", sep = ","
  )
  
  caps <- list(
    browserName = "firefox",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList" = 2L,
        "browser.download.dir" = dl_dir_cont,
        "browser.download.useDownloadDir" = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" = mimes
      )
    ),
    timeouts = list(implicit = 0L, pageLoad = 60000L, script = 60000L)
  )
  
  session <- selenium::SeleniumSession$new(
    browser = "firefox", host = selenium_host, port = selenium_port, 
    capabilities = caps, timeout = timeout
  )
  on.exit(session$close(), add = TRUE)
  
  # --- HELPERS ---------------------------------------------------------------
  
  # Helper to close floating menus preventing clicks
  close_overlays <- function() {
    try(session$find_element("css selector", "body")$send_keys(keys$escape), 
        silent=TRUE)
    Sys.sleep(0.5)
    # Click neutral label to blur
    try(session$execute_script("document.querySelector('label').click();"), 
        silent=TRUE)
  }
  
  wait_spinner_hidden <- function(spin_id, timeout_sec = 60) {
    js_check <- sprintf("
      var el = document.getElementById('%s');
      if (!el) return true; 
      var disp = window.getComputedStyle(el).display;
      return disp === 'none';
    ", spin_id)
    
    t0 <- Sys.time()
    repeat {
      hidden <- try(session$execute_script(js_check), silent = TRUE)
      if (isTRUE(hidden)) return(TRUE)
      
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_sec) {
        return(FALSE)
      }
      Sys.sleep(0.5)
    }
  }
  
  wait_for_main_spinner <- function() {
    Sys.sleep(0.2)
    wait_spinner_hidden("loading", timeout_sec = 30)
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
        var tgt = '%s'.trim().toUpperCase();
        if (txt === tgt) {
          sel.selectedIndex = i;
          found = true; break;
        }
      }
      if (found) {
        if (window.jQuery) { $(sel).trigger('change'); } 
        else { sel.dispatchEvent(new Event('change')); }
        return true;
      }
      return false;
    ", input_id, label_text)
    
    found <- session$execute_script(js_code)
    if (isFALSE(found)) {
      stop(sprintf("Option '%s' not found in %s", label_text, widget_id))
    }
    wait_for_main_spinner()
  }
  
  setup_form <- function(dept, param) {
    for (attempt in 1:3) {
      tryCatch({
        close_overlays()
        
        # 1. Select Department
        pf_select_option("filtroForm\\:departamentoSel", dept)
        
        # 2. Open Station Dropdown
        trigger_sel <- '//*[@id="filtroForm:estacionesSel"]/ul'
        wait_for(session, "xpath", trigger_sel, timeout = 10)$click()
        
        # 3. Wait for "Select All"
        p_sel <- "#filtroForm\\:estacionesSel_panel"
        chk_sel <- paste0(p_sel, 
                          " .ui-selectcheckboxmenu-header .ui-chkbox-box")
        
        chk_el <- try(wait_for(session, "css selector", chk_sel, timeout = 30), 
                      silent = TRUE)
        if (inherits(chk_el, "try-error")) stop("Station list didn't load.")
        
        chk_el$click()
        wait_for_main_spinner()
        
        close_overlays() 
        message("    Waiting for the table of stations to dropdown.")
        Sys.sleep(2)
        wait_for_main_spinner()
        
        # 4. Select Parameter
        pf_select_option("filtroForm\\:contaminanteSel", param)
        wait_for_main_spinner()
        
        # Verify
        js_chk <-
          "return document.getElementById('filtroForm:contaminanteSel_input').value"
        curr_param <- session$execute_script(js_chk)
        
        if (!is.null(curr_param) && curr_param != "0" && curr_param != "") {
          return(TRUE)
        }
      }, error = function(e) { 
        message("   ⚠️ Setup attempt ", attempt, " failed: ", e$message)
      })
      
      close_overlays()
      wait_for_main_spinner()
      Sys.sleep(2)
    }
    return(FALSE) 
  }
  
  safe_navigate <- function(url) {
    try({ session$navigate(url); return(TRUE) }, silent = TRUE)
    Sys.sleep(2)
    return(TRUE)
  }
  
  log <- list()
  
  # --- MAIN LOOP -------------------------------------------------------------
  for (dept in target_depts) {
    message(sprintf("\n📍 DEPT: %s", dept))
    safe_navigate(base_url)
    
    for (param in target_params) {
      message(sprintf("   🔹 Param: %s", param))
      
      if (!setup_form(dept, param)) {
        message("      ❌ Setup failed. Refreshing...")
        safe_navigate(base_url)
        if (!setup_form(dept, param)) { 
          message("      ❌ Skipping."); next 
        }
      }
      
      close_overlays()
      wait_for_main_spinner()
      
      # Prepare Dates
      lbl_id <- "document.getElementById('filtroForm:labelFIniLimite').click();"
      session$execute_script(lbl_id)
      Sys.sleep(2)
      
      inp_sel <- "#filtroForm\\:fechaIni_input"
      input_start <- try(session$find_element("css selector", inp_sel), 
                         silent = TRUE)
      
      if (inherits(input_start, "try-error")) { message("      ⚠️ Broken."); next }
      
      start_val <- input_start$get_attribute("value")
      if (is.null(start_val) || start_val == "") { message("      ⚠️ No data."); next }
      
      d_start_avail <- as.Date(start_val)
      d_end_limit   <- as.Date(paste0(max(years_range), "-12-31"))
      message(sprintf("      📅 Avail: %s", d_start_avail))
      
      curr_start <- d_start_avail
      step_unit  <- "1 year" 
      
      while (curr_start < d_end_limit) {
        
        # Calculate End Date
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
            message("      ⚠️ Retry #", chunk_attempt, ". Refreshing...")
            session$navigate(base_url)
            if (!setup_form(dept, param)) { status <- "failed"; break }
            close_overlays()
          }
          
          try({
            s_start <- format(curr_start, "%Y-%m-%d")
            s_end   <- format(curr_end,   "%Y-%m-%d")
            
            # Inject Dates
            js_dates <- sprintf(paste0(
              "$('#filtroForm\\\\:fechaIni_input').val('%s')",
              ".trigger('change').trigger('blur');",
              "$('#filtroForm\\\\:fechaFin_input').val('%s')",
              ".trigger('change').trigger('blur');"
            ), s_start, s_end)
            session$execute_script(js_dates)
            wait_for_main_spinner()
            close_overlays() 
            
            # Select Hourly
            trigger_time <- '//*[@id="filtroForm:tipoSel"]/div[3]/span'
            tm_trigger <- wait_for_xpath(trigger_time, 30)
            pf_select_option("filtroForm\\:tipoSel", "Hora")
            tm_trigger$click() # Refresh UI
            tm_trigger$click()
            close_overlays()
            
            session$execute_script("$('.ui-growl-item-container').remove();")
            message(sprintf("      🔎 Query: %s to %s", s_start, s_end))
            
            # Click Consultar Initial
            btn_js <-
              "document.getElementById('filtroForm:btnConsultar').click()"
            session$execute_script(btn_js)
            
            result_found <- FALSE
            
            # --- POLLING LOOP (Replaces simple wait) ---
            for(p_wait in 1:300) {
              
              # Sleepy Server Logic: Re-click if taking too long
              if (p_wait %in% c(60, 120, 200)) {
                message("       💤 Server sleepy. Clicking Consultar again...")
                session$execute_script(btn_js)
              }
              
              # 1. Check Growl
              js_growl <- "
                 var g = document.querySelector('div.ui-growl-item-container');
                 return (g) ? g.innerText : null;"
              growl_txt <- session$execute_script(js_growl)
              
              if (!is.null(growl_txt)) {
                if (grepl("65526|mayor a|excede", growl_txt, ignore.case=T)) {
                  message("       📉 Chunk too big. Resizing...")
                  resize_triggered <- TRUE
                  result_found <- TRUE
                  Sys.sleep(20)
                  break
                }
                if (grepl("No se encontraron", growl_txt, ignore.case=T)) {
                  message("       ⚠️ No data."); status <- "no_data"
                  result_found <- TRUE; break
                }
                if (grepl("error|servidor|fallo", growl_txt, ignore.case=T)) {
                  status <- "server_error"; result_found <- TRUE; break
                }
              }
              
              # 2. Check CSV Button
              js_click_csv <- "
                 var xp = \"//a[.//span[contains(@class, 'fa-file-text-o')]]\";
                 var res = document.evaluate(xp, document, null, 
                           XPathResult.FIRST_ORDERED_NODE_TYPE, null);
                 var el = res.singleNodeValue;
                 if (el) { el.click(); return true; }
                 return false;
              "
              was_clicked <- session$execute_script(js_click_csv)
              
              if (isTRUE(was_clicked)) {
                # Double tap check
                Sys.sleep(5)
                fnow <- list.files(downloads_folder, full.names=TRUE)
                if (!any(difftime(Sys.time(), 
                                  file.info(fnow)$mtime, units="secs") < 10)) {
                  session$execute_script(js_click_csv)
                }
                result_found <- TRUE; status <- "downloading"; break
              }
              
              Sys.sleep(1)
            }
            
            # --- END POLLING ---
            
            if (status == "downloading") {
              dl_file <- try(wait_for_new_download(
                downloads_folder, character(0), "\\.(csv|txt)$", 10, 120
              ), silent = TRUE)
              
              if (!inherits(dl_file, "try-error")) {
                san_dept  <- sanitize_name(dept)
                san_param <- sanitize_name(param)
                out_name  <- sprintf("%s_%s_%s_%s.csv", 
                                     san_dept, san_param, s_start, s_end)
                dest      <- file.path(final_dir, out_name)
                if (file.exists(dest)) unlink(dest)
                file.copy(dl_file, dest); unlink(dl_file)
                f_path <- dest
                message("       ⬇️  Saved: ", out_name)
              } else {
                status <- "timeout_dl"
              }
            } else if (!result_found && !resize_triggered) {
              status <- "timeout_query"
            }
            
          }) -> try_res
          
          if (resize_triggered) break
          if (status %in% c("downloading", "no_data")) break
          if (status == "failed") break
          
          message("      ⚠️ Retry logic engaged (Status: ", status, ")")
          close_overlays()
          Sys.sleep(5)
        }
        
        if (resize_triggered) {
          step_unit <- "3 months"
          message("      🔄 Restarting with 3-month chunks...")
          next
        }
        
        log[[length(log) + 1]] <- tibble::tibble(
          dept = dept, param = param, start = curr_start, end = curr_end, 
          status = status, file = f_path
        )
        
        if (status == "downloading") {
          message("      ♻️  Resetting session...")
          session$navigate("about:blank"); Sys.sleep(0.5)
          safe_navigate(base_url)
          if (!setup_form(dept, param)) break
        }
        
        curr_start <- curr_end + 1
      }
    }
    message("   🧊 Dept finished. Cooling down...")
    Sys.sleep(5)
  }
  
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
# 
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
# 
# @Output    : list(label, filename, href) for the requested resource
# @Purpose   : Extract the 'mostrarModal("FILE.zip","https://.../download/ID")' link.
#              We resolve relative → absolute URLs and trim whitespace.
# @Written_on: 21/08/2025
# @Written_by: Marcos Paulo
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
# @Arg        : year            — numeric; 2005 or 2018.
# @Arg        : type            — character; [2005 Only] "BASICO" or "AMPLIADO".
# @Arg        : regions         — character; [2018 Only] Regions to fetch (e.g. Bogota).
# @Arg        : url             — character: character; Catalog URL. 
#                                 Defaults based on year if NULL.
# @Arg        : download_folder — character; Where to save ZIPs.
# @Arg        : overwrite       — logical; re-download if file exists.
# @Arg        : retries         — numeric; max HTTP retries.
# @Arg        : quiet           — logical; suppress progress.
#
# @Output     : Tibble with cols: year, target, file_path, bytes.
# @Purpose    : Download Census microdata.
#               - 2005: Single national ZIP via helper.
#               - 2018: Scrapes download links for specific regions.
# @Written_on: 21/08/2025 (Updated 08/02/2026)
# @Written_by: Marcos Paulo
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
# Function: .bogota_normalize_varname
# @Arg       : x           — character; original column name
# 
# @Output    : character; normalized (ASCII, snake_case) variable name
# @Purpose   : Standardize column names from Bogotá XLSX exports; handles accents,
#              spaces/punct, and common Spanish labels (PM2.5/PM10, Ozono, etc.).
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
.bogota_normalize_varname <- function(x) {
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
# .bogota_standardize_name
# @Arg       : x        — list; contains names of stations to be changed
# 
# @Output    : list with standardized values/names
# @Purpose   : make names in Uppercase, strip accents, remove stray quotes.
#              Mirrors the same helper in compute_distance_matrices().
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
.bogota_standardize_name <- function(x) {
  x <- toupper(trimws(x))
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  gsub('"', "", x)
}

# --------------------------------------------------------------------------------------------
# # Function: .bogota_apply_mappings
# @Arg       : x        — list; contains names of stations to be mapped
# 
# @Output    : list with either changed values in the mapped cases or the unchanged value
# @Purpose   : Resolves known name discrepancies across RMCAB CSV (geolocation),
#              RMCAB XLSX (pollution data), and SISAIRE sources. The canonical name is 
#              whatever the RMCAB XLSX pollution files use, since that is the primary 
#              data source.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
.bogota_apply_mappings <- function(x) {
  dplyr::case_when(
    # SISAIRE/CSV: "P_CAMI - FONTIBON"    → RMCAB xlsx: "FONTIBON"
    grepl("CAMI.*FONTIBON", x)  ~ "FONTIBON",
    
    # SISAIRE: "CARVAJAL - SEVILLANA"    → RMCAB xlsx: "CARVAJAL-SEVILLANA"
    x == "CARVAJAL - SEVILLANA" ~ "CARVAJAL-SEVILLANA",
    
    # Geolocation CSV / SISAIRE: "EL JAZMIN" → RMCAB xlsx: "JAZMIN"
    #   stations_sf  : "El Jazmín"  → standardize → "EL JAZMIN"
    #   SISAIRE csv  : "EL JAZMÍN"  → standardize → "EL JAZMIN"
    #   RMCAB xlsx   : "Jazmin"     → standardize → "JAZMIN" (no "EL")
    x == "EL JAZMIN"            ~ "JAZMIN",
    
    # Passthrough — name already canonical after standardize
    TRUE                        ~ x
  )
}

# --------------------------------------------------------------------------------------------
# Function: ,bogota_read_one_xlsx
# @Arg       : path        — string; full path to a single STATION_YEAR.xlsx
# @Arg       : tz          — string; Olson timezone for datetime parsing 
#                            (default "UTC")
# @Arg       : verbose     — logical; TRUE prints a brief parsing summary (default FALSE)
# 
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
.bogota_read_one_xlsx <- function(path, tz = "UTC", verbose = FALSE) {
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
           .bogota_normalize_varname,
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
# @Arg       : rmcab_df         — Pre-loaded dataframe (RMCAB - Source I)
# @Arg       : metadata_dir     — Folder with Excel files (SISAIRE)
# @Arg       : metro_area       — sf polygon of the metropolitan area
# @Arg       : radius_km        — numeric; max distance to keep (default 20)
# @Arg       : stations_epsg    — EPSG for lon/lat (default 4326)
# @Arg       : out_file         — output GeoPackage path
# @Arg       : overwrite_gpkg   — logical; overwrite if exists
# @Arg       : dissolve         — logical; TRUE unions metro polygons
# 
# @Output    : sf POINT data.frame. station_name is normalised to the
#              all-uppercase RMCAB canonical form for all stations.
# @Purpose   : Merges RMCAB (Master) with SISAIRE (Dates), handles name
#              mismatches via .bogota_apply_mappings(), and spatially
#              filters to the metro area + buffer.
# @Written_on: 25/10/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_filter_stations_in_metro <- function(
    rmcab_df,
    metadata_dir,
    metro_area,
    radius_km      = 20,
    stations_epsg  = 4326,
    out_file       = here::here("data", "raw", "geospatial_data",
                                "bogota", "stations.gpkg"),
    overwrite_gpkg = TRUE,
    dissolve       = TRUE
) {
  
  # 0. Dependency checks
  # ---------------------------------------------------------------------------
  if (!inherits(metro_area, "sf"))
    stop("'metro_area' must be an sf object.")
  if (!dir.exists(metadata_dir))
    stop("Metadata dir not found: ", metadata_dir)
  
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  # Stations confirmed to be in RMCAB but absent from the scraped
  # geolocation CSV (web-scrape gap). They arrive via SISAIRE metadata
  # but must be labelled as RMCAB in the output.
  rmcab_scrape_gaps <- c("MOVIL 7MA")
  
  message("Starting Data Integration (RMCAB + SISAIRE)...")
  
  # PART I: Process RMCAB (Primary Source)
  # ---------------------------------------------------------------------------
  # Both helpers are applied to join_id so that names like "El Jazmín"
  # resolve to their canonical RMCAB form ("JAZMIN") and merge correctly
  # with SISAIRE, instead of producing a duplicate row.
  df1_clean <- rmcab_df |>
    dplyr::select(
      station_name = station,
      code, altitude_m, height_m, locality,
      zone_type, station_type, address, lat, lon
    ) |>
    dplyr::mutate(
      source  = "RMCAB",
      join_id = .bogota_apply_mappings(
        .bogota_standardize_name(station_name)
      )
    )
  
  message("  Loaded ", nrow(df1_clean), " stations from RMCAB.")
  
  # PART II: Process SISAIRE (Secondary Source)
  # ---------------------------------------------------------------------------
  xl_files <- list.files(
    metadata_dir, pattern = "\\.xls$", full.names = TRUE
  )
  
  if (length(xl_files) == 0) {
    warning("No .xls files found in metadata_dir.")
    df2_clean <- data.frame()
  } else {
    message(
      "  Found ", length(xl_files), " SISAIRE files. Processing..."
    )
    
    df2_clean <- purrr::map_dfr(xl_files, function(f) {
      raw_xl <- XLConnect::readWorksheetFromFile(
        f, sheet = 1, startRow = 10
      )
      raw_xl |>
        dplyr::select(
          station_name = `Estación`,
          address_sis  = `Dirección`,
          first_date   = `Fecha.primer.registro`,
          last_date    = `Fecha.último.registro`,
          lat_sis      = `Latitud`,
          lon_sis      = `Longitud`
        ) |>
        dplyr::mutate(
          lat_sis    = as.numeric(lat_sis),
          lon_sis    = as.numeric(lon_sis),
          source_sis = paste0(
            "SISAIRE-",
            tools::file_path_sans_ext(basename(f))
          )
        ) |>
        dplyr::filter(
          !is.na(first_date) & first_date != "",
          !is.na(last_date)  & last_date  != ""
        ) |>
        tidyr::drop_na(lat_sis, lon_sis) |>
        dplyr::mutate(
          join_id = .bogota_apply_mappings(
            .bogota_standardize_name(station_name)
          )
        )
    })
    
    # Deduplicate: keep the most recently active entry per station.
    # Needed because the FONTIBON mapping can create two rows with the
    # same join_id ("P_CAMI - FONTIBON" and "FONTIBON" → "FONTIBON").
    df2_clean <- df2_clean |>
      dplyr::arrange(join_id, dplyr::desc(last_date)) |>
      dplyr::distinct(join_id, .keep_all = TRUE)
    
    message(
      "  Loaded ", nrow(df2_clean), " valid stations from SISAIRE."
    )
  }
  
  # PART III: Intelligent Merge
  # ---------------------------------------------------------------------------
  if (nrow(df2_clean) > 0) {
    
    # A) Enrich RMCAB stations with SISAIRE date coverage
    df1_enriched <- df1_clean |>
      dplyr::left_join(
        df2_clean |>
          dplyr::select(join_id, first_date, last_date, source_sis),
        by = "join_id"
      ) |>
      dplyr::mutate(
        source = dplyr::if_else(
          !is.na(source_sis),
          paste(source, source_sis, sep = " + "),
          source
        )
      ) |>
      dplyr::select(-source_sis)
    
    # B) Append stations that only exist in SISAIRE.
    #    Stations listed in rmcab_scrape_gaps are known RMCAB stations
    #    missing from the scraped geolocation CSV. They are relabelled
    #    as "RMCAB" so downstream functions treat them correctly.
    df_new <- df2_clean |>
      dplyr::filter(!join_id %in% df1_clean$join_id) |>
      dplyr::mutate(
        source = dplyr::if_else(
          join_id %in% rmcab_scrape_gaps,
          "RMCAB + SISAIRE-bogota_d_c",
          source_sis
        )
      ) |>
      dplyr::select(
        station_name,
        address    = address_sis,
        lat        = lat_sis,
        lon        = lon_sis,
        first_date,
        last_date,
        source,
        join_id
      )
    
    all_stations <- dplyr::bind_rows(df1_enriched, df_new)
    
  } else {
    all_stations <- df1_clean
  }
  
  # Normalise station_name to the all-uppercase RMCAB canonical form.
  # This ensures every row in the output sf uses the same name that
  # appears in the RMCAB xlsx pollution files (e.g. "JAZMIN" not
  # "El Jazmín", "MOVIL FONTIBON" not "Móvil Fontibón"), which is
  # required for the station-matching logic in
  # bogota_process_stations_data_to_parquet().
  all_stations <- all_stations |>
    dplyr::mutate(
      station_name = .bogota_apply_mappings(
        .bogota_standardize_name(station_name)
      )
    ) |>
    dplyr::select(-join_id)
  
  message("  Merged total: ", nrow(all_stations), " stations.")
  
  # PART IV: Spatial Conversion & AEQD Buffer Filter
  # ---------------------------------------------------------------------------
  stations_sf <- sf::st_as_sf(
    all_stations,
    coords = c("lon", "lat"),
    crs    = stations_epsg
  )
  
  message(
    "Applying spatial filter (radius: ", radius_km, " km)..."
  )
  
  metro_valid <- sf::st_make_valid(metro_area)
  if (dissolve) metro_valid <- sf::st_union(metro_valid)
  
  metro_wgs <- sf::st_transform(metro_valid, 4326)
  cen       <- sf::st_coordinates(sf::st_centroid(metro_wgs))
  
  aeqd_proj <- sprintf(
    "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
    cen[2], cen[1]
  )
  
  metro_m    <- sf::st_transform(metro_valid, aeqd_proj)
  stations_m <- sf::st_transform(stations_sf, aeqd_proj)
  
  within_idx <- sf::st_is_within_distance(
    stations_m, metro_m, dist = radius_km * 1000
  )
  keep_mask <- lengths(within_idx) > 0
  
  stations_final <- stations_sf[keep_mask, ]
  
  message(
    "  Filter: Input = ", nrow(stations_sf),
    " → Kept = ",        nrow(stations_final),
    " (Dropped ",        nrow(stations_sf) - nrow(stations_final), ")"
  )
  
  # PART V: Save GeoPackage
  # ---------------------------------------------------------------------------
  if (file.exists(out_file) && !overwrite_gpkg) {
    message("Output exists and overwrite_gpkg = FALSE. Skipping write.")
  } else {
    if (file.exists(out_file)) unlink(out_file)
    sf::st_write(stations_final, out_file, quiet = TRUE, append = FALSE)
    message("Saved GeoPackage: ", out_file)
  }
  
  return(stations_final)
}


# ----------------------------------------------------------------------------------------
# Function: bogota_process_stations_data_to_parquet
# @Arg       : rmcab_folder   — character; folder with RMCAB .xlsx files.
# @Arg       : sisaire_folder — string; folder with SISAIRE .csv files.
# @Arg       : stations_sf    — sf object; spatial registry of stations to keep.
#                               Must have a 'station_name' column.
# @Arg       : out_dir        — string; base output directory.
# @Arg       : out_name       — string; dataset name prefix.
# @Arg       : years          — integer vector; years to retain.
# @Arg       : tz             — string; Olson timezone of the raw source data.
#                               Pass "UTC" (the default) to store raw hours without
#                               any timezone shift — see @Details.
# @Arg       : verbose        — logical; print progress messages.
#
# @Output    : Arrow Dataset connection to the written Parquet folder.
# @Details   :
#   SOURCE PRIORITY:
#     RMCAB is primary. For any station x year x param it covers, only RMCAB
#     values are used (NA hours stay NA, not filled from SISAIRE). SISAIRE is
#     used only for station x year x param combinations RMCAB never tracked.
#     Coverage is checked at param level so a station can take RMCAB PM10 and
#     SISAIRE PM2.5 in the same year.
#   SISAIRE SUB-HOURLY DATA:
#     Some SISAIRE files report at 5-minute cadence (e.g. BOGOTA RURAL -
#     MOCHUELO, 2017); others hourly. We floor SISAIRE timestamps to the hour
#     so the Phase-D AVG() collapses them to one hourly mean. RMCAB is already
#     hourly and untouched.
#   DATETIME SAFETY:
#     Datetimes enter DuckDB as VARCHAR (via to_iso()) to avoid a DuckDB
#     R-driver bug that shifts hours from POSIXct tzone attributes. STRPTIME()
#     converts them back to plain TIMESTAMP, so no timezone conversion occurs.
# @Written_on: 29/10/2025
# @Written_by: Marcos Paulo
# ----------------------------------------------------------------------------------------
bogota_process_stations_data_to_parquet <- function(
    rmcab_folder,
    sisaire_folder,
    stations_sf,
    out_dir,
    out_name  = "bogota_metro_air",
    years     = bogota_cfg$years,
    tz        = "UTC",
    verbose   = TRUE
) {
  
  # 0. Local helper
  # ---------------------------------------------------------------------------
  
  # Serialize POSIXct to plain text before DuckDB (see DATETIME SAFETY).
  to_iso <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # 1. Dependencies + input validation
  # ---------------------------------------------------------------------------
  req_pkgs <- c(
    "duckdb", "DBI", "arrow", "tidyr",
    "dplyr", "readr", "stringi", "sf", "lubridate"
  )
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' required.")
  }
  
  if (!inherits(stations_sf, "sf") ||
      !"station_name" %in% names(stations_sf)) {
    stop(
      "'stations_sf' must be an sf object with a 'station_name' column."
    )
  }
  
  # Allow-list of canonical station names (only stations that passed the
  # spatial buffer filter). Helpers fold "El Jazmin"/"EL JAZMIN"/"Jazmin"
  # to one key "JAZMIN".
  valid_stations_norm <- .bogota_apply_mappings(
    .bogota_standardize_name(unique(stations_sf$station_name))
  )
  
  # 2. DuckDB disk-backed connection
  # ---------------------------------------------------------------------------
  # Disk-backed so the staging + merge + pivot can spill to disk for long
  # series (2000-2024) instead of exhausting RAM.
  if (verbose) message("Setting up DuckDB engine ...")
  
  dbdir <- tempfile("bogota_unified_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  
  # Guarantee cleanup even on crash.
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    try(unlink(dbdir, force = TRUE), silent = TRUE)
  }, add = TRUE)
  
  DBI::dbExecute(con, "PRAGMA memory_limit='8GB';")
  DBI::dbExecute(con, paste0(
    "PRAGMA threads=", max(1L, parallel::detectCores() - 1L), ";"
  ))
  
  # Long-format staging, datetime as VARCHAR (see DATETIME SAFETY). One row
  # per (station, datetime, param, value) for a clean FULL OUTER JOIN later.
  DBI::dbExecute(con,
                 "CREATE TABLE staging_rmcab (
       datetime VARCHAR,  -- 'YYYY-MM-DD HH:MM:SS', no timezone
       station  VARCHAR,  -- canonical uppercase name
       year     INTEGER,
       param    VARCHAR,  -- e.g. 'pm10', 'pm25', 'ozone'
       value    DOUBLE
     );"
  )
  DBI::dbExecute(con,
                 "CREATE TABLE staging_sisaire (
       datetime VARCHAR,
       station  VARCHAR,
       year     INTEGER,
       param    VARCHAR,
       value    DOUBLE
     );"
  )
  
  # 3. Phase A: Read RMCAB Excel files -> load into staging_rmcab
  # ---------------------------------------------------------------------------
  # RMCAB header names -> project parameter codes.
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
  
  xlsx_files  <- list.files(
    rmcab_folder, pattern = "\\.xlsx$", full.names = TRUE
  )
  count_rmcab <- 0L
  
  if (length(xlsx_files) > 0) {
    if (verbose) message(sprintf(
      "Processing %d RMCAB files ...", length(xlsx_files)
    ))
    
    for (f in xlsx_files) {
      
      # Parse one wide RMCAB sheet -> datetime, station, year, <pollutants>.
      df <- tryCatch(
        .bogota_read_one_xlsx(f, tz = tz, verbose = FALSE),
        error = function(e) {
          warning("Skip RMCAB file: ", basename(f))
          NULL
        }
      )
      if (is.null(df) || nrow(df) == 0) next
      
      df <- df[df$year %in% years, ]
      if (nrow(df) == 0) next
      
      # Raw pollutant names -> project codes.
      for (raw in names(rmcab_map)) {
        if (raw %in% names(df))
          names(df)[names(df) == raw] <- rmcab_map[[raw]]
      }
      
      # Wide -> long.
      df_long <- tidyr::pivot_longer(
        df,
        cols            = -c(datetime, station, year),
        names_to        = "param",
        values_to       = "value",
        values_drop_na  = TRUE
      )
      if (nrow(df_long) == 0) next
      
      # Normalize names and keep only allowlisted stations.
      df_long$station <- .bogota_apply_mappings(
        .bogota_standardize_name(df_long$station)
      )
      df_long <- df_long[df_long$station %in% valid_stations_norm, ]
      if (nrow(df_long) == 0) next
      
      df_long$datetime <- to_iso(df_long$datetime)
      
      duckdb::dbAppendTable(con, "staging_rmcab", df_long)
      count_rmcab <- count_rmcab + nrow(df_long)
    }
  }
  
  # 4. Phase B: Read SISAIRE CSV files -> load into staging_sisaire
  # ---------------------------------------------------------------------------
  # SISAIRE header names -> project parameter codes.
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
  
  csv_files     <- list.files(
    sisaire_folder, pattern = "\\.csv$", full.names = TRUE
  )
  count_sisaire <- 0L
  
  if (length(csv_files) > 0) {
    if (verbose) message(sprintf(
      "Processing %d SISAIRE files ...", length(csv_files)
    ))
    
    for (f in csv_files) {
      
      # Each file holds one parameter; read the header to find which.
      raw_header <- names(read.csv(f, nrows = 1))
      val_col    <- setdiff(
        raw_header, c("Estacion", "Fecha.inicial", "Fecha.final")
      )
      if (length(val_col) == 0) next
      
      # Header name -> project code; fall back to lowercased header.
      db_var <- sisaire_map[[ val_col[1] ]]
      if (is.null(db_var)) db_var <- tolower(val_col[1])
      
      dt <- read.csv(f, stringsAsFactors = FALSE)
      if (nrow(dt) == 0) next
      
      dt <- dt |>
        dplyr::rename(
          station  = Estacion,
          raw_time = Fecha.inicial,
          value    = !!val_col[1]
        ) |>
        dplyr::select(station, raw_time, value)
      
      # Normalize names and keep only allowlisted stations.
      dt$station <- .bogota_apply_mappings(
        .bogota_standardize_name(dt$station)
      )
      dt <- dt[dt$station %in% valid_stations_norm, ]
      if (nrow(dt) == 0) next
      
      dt$datetime <- lubridate::ymd_hm(
        dt$raw_time, tz = tz, quiet = TRUE
      )
      dt <- dt[!is.na(dt$datetime), ]
      
      # Floor to the hour so 5-minute files collapse to hourly via Phase-D
      # AVG() (no-op on already-hourly :00 timestamps). See SISAIRE SUB-HOURLY.
      dt$datetime <- lubridate::floor_date(dt$datetime, unit = "hour")
      
      dt$year  <- lubridate::year(dt$datetime)
      dt$param <- db_var
      dt <- dt[dt$year %in% years, ]
      
      final_chk <- dt |>
        dplyr::select(datetime, station, year, param, value)
      if (nrow(final_chk) == 0) next
      
      final_chk$datetime <- to_iso(final_chk$datetime)
      
      duckdb::dbAppendTable(con, "staging_sisaire", final_chk)
      count_sisaire <- count_sisaire + nrow(final_chk)
    }
  }
  
  if (count_rmcab == 0L && count_sisaire == 0L)
    stop("No valid data found in RMCAB or SISAIRE folders.")
  
  if (verbose) message(
    "Staged: RMCAB = ",   format(count_rmcab,   big.mark = ","),
    " rows | SISAIRE = ", format(count_sisaire, big.mark = ","), " rows"
  )
  
  # 5. Phase C: Discrepancy report
  # ---------------------------------------------------------------------------
  # QA-only: per station x param, overlap count and source disagreement.
  # overlap_count counts raw SISAIRE rows (sub-hourly not yet averaged).
  report_dir <- file.path(out_dir, "bogota_rmcab_sisaire_comparison")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  diff_df <- DBI::dbGetQuery(con, "
    SELECT
      r.station,
      r.param,
      COUNT(*)                    AS overlap_count,
      AVG(ABS(r.value - s.value)) AS mean_abs_diff,
      MAX(ABS(r.value - s.value)) AS max_abs_diff
    FROM staging_rmcab r
    INNER JOIN staging_sisaire s
      ON  r.station  = s.station
      AND r.datetime = s.datetime
      AND r.param    = s.param
    GROUP BY r.station, r.param
    ORDER BY mean_abs_diff DESC;
  ")
  
  diff_file <- file.path(report_dir, "discrepancy_summary.csv")
  readr::write_csv(diff_df, diff_file)
  
  if (verbose) message(
    "Discrepancy report: ",
    format(sum(diff_df$overlap_count), big.mark = ","),
    " overlapping rows -> ", diff_file
  )
  
  # 6. Phase D: Merge + pivot + write Parquet
  # ---------------------------------------------------------------------------
  
  # D1: station x year x param combinations RMCAB ever reported (see
  # SOURCE PRIORITY).
  DBI::dbExecute(con, "
    CREATE TABLE rmcab_coverage AS
    SELECT DISTINCT station, year, param
    FROM staging_rmcab;
  ")
  
  # D2: merge sources. FULL OUTER JOIN keeps every row; cov tells whether RMCAB covered it.
  # Value follow SOURCE PRIORITY. NULL RMCAB values under coverage are kept NULL 
  # Floored SISAIRE sub-hourly rows share a datetime here and are averaged in D3.
  DBI::dbExecute(con, "
    CREATE TABLE staging_merged AS
    SELECT
      STRPTIME(
        COALESCE(r.datetime, s.datetime),
        '%Y-%m-%d %H:%M:%S'
      )                                     AS datetime,
      COALESCE(r.station,  s.station)       AS station,
      COALESCE(r.year,     s.year)          AS year,
      COALESCE(r.param,    s.param)         AS param,

      CASE
        WHEN cov.station IS NOT NULL THEN r.value
        ELSE                              s.value
      END                                   AS value,

      CASE
        WHEN cov.station IS NOT NULL
         AND r.value IS NOT NULL THEN 'RMCAB'
        WHEN cov.station IS NOT NULL
         AND r.value IS     NULL THEN 'RMCAB (hour NA — not filled)'
        ELSE                          'SISAIRE'
      END                                   AS source_origin

    FROM staging_rmcab r
    FULL OUTER JOIN staging_sisaire s
      ON  r.station  = s.station
      AND r.datetime = s.datetime
      AND r.param    = s.param

    LEFT JOIN rmcab_coverage cov
      ON  COALESCE(r.station, s.station) = cov.station
      AND COALESCE(r.year,    s.year)    = cov.year
      AND COALESCE(r.param,   s.param)   = cov.param;
  ")
  
  # D3: long -> wide, one row per (station, datetime), one column per pollutant. 
  # AVG() returns NULL when all inputs are NULL and also yields the hourly mean of 
  # floored SISAIRE readings. Partition by year for downstream pruning.
  dataset_path <- file.path(out_dir, paste0(out_name, "_dataset"))
  if (dir.exists(dataset_path)) unlink(dataset_path, recursive = TRUE)
  
  sql_pivot <- sprintf("
    COPY (
      SELECT
        datetime,
        station,
        year,
        AVG(CASE WHEN param='pm10'
                 THEN value END) AS pm10,
        AVG(CASE WHEN param='pm25'
                 THEN value END) AS pm25,
        AVG(CASE WHEN param='ozone'
                 THEN value END) AS ozone,
        AVG(CASE WHEN param='no'
                 THEN value END) AS no,
        AVG(CASE WHEN param='no2'
                 THEN value END) AS no2,
        AVG(CASE WHEN param='nox'
                 THEN value END) AS nox,
        AVG(CASE WHEN param='co'
                 THEN value END) AS co,
        AVG(CASE WHEN param='so2'
                 THEN value END) AS so2,
        AVG(CASE WHEN param='temp'
                 THEN value END) AS temperature,
        AVG(CASE WHEN param='rh'
                 THEN value END) AS rh,
        AVG(CASE WHEN param='pressure'
                 THEN value END) AS pressure,
        AVG(CASE WHEN param='radiation'
                 THEN value END) AS radiation,
        AVG(CASE WHEN param='precipitation'
                 THEN value END) AS precipitation,
        AVG(CASE WHEN param='vel_viento'
                 THEN value END) AS wind_speed,
        AVG(CASE WHEN param='dir_viento'
                 THEN value END) AS wind_dir,
        FIRST(source_origin)     AS primary_source
      FROM staging_merged
      GROUP BY datetime, station, year
      ORDER BY station, datetime
    ) TO '%s' (
      FORMAT PARQUET,
      PARTITION_BY (year),
      COMPRESSION 'SNAPPY',
      OVERWRITE_OR_IGNORE TRUE
    );",
                       gsub("\\\\", "/", dataset_path)
  )
  
  if (verbose) message("Pivoting and writing partitioned Parquet ...")
  DBI::dbExecute(con, sql_pivot)
  
  if (verbose) message("Done. Dataset: ", dataset_path)
  
  # Arrow handle so the caller can query immediately.
  arrow::open_dataset(dataset_path)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_merge_downloads
# @Arg       : downloads_folder — string; directory containing *.xlsx exports
# @Arg       : cleanup          — logical; TRUE deletes the .xlsx after merging (default TRUE)
# @Arg       : tz               — string; Olson timezone for datetime parsing 
#                                 (default: "America/Bogota")
# 
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
  big_tbl <- purrr::map_dfr(files, function(p) .bogota_read_one_xlsx(p, tz = tz))
  
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
# 
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
# Function: bogota_filter_census_2005
# @Arg       : census_zip   — path to CG2005_AMPLIADO.zip
# @Arg       : out_dir      — where to write selected dept folders with CSVs
# @Arg       : overwrite    — re-extract if output exists (default FALSE)
# @Arg       : quiet        — suppress messages (default FALSE)
# 
# @Output    : list with $bogota and $cundinamarca
#              - dir_extracted (folder containing only CSV files)
#              - files_index   (tibble with file, size, type)
# @Purpose   : Fast extract master ZIP, fix mojibake, unpack dept 11 & 25,
#              unpack *CSV.zip* inside, and FLATTEN structure (CSVs to root).
#              Fixed to handle re-runs gracefully without "copy to self" errors.
# @Written_on: 22/10/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_filter_census_2005 <- function(
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
# Function: bogota_harmonize_census_2005_data
# @Arg       : extract_list — List output from bogota_filter_harmonize_census
# @Arg       : is_extended — Logical; True (Default) if the census data is the extended version
# @Arg       : metro_codes —  Vector of municipality codes for Cundinamarca filtering
# @Arg       : out_dir — Where to save the processed individual and collapsed data
# @Arg       : quiet — Suppress progress messages
#
# @Output    : Saves CSV/RData files; returns list of processed dataframes
# @Purpose   : Replicates Stata logic: Harmonizes education, creates labor/demographic
#              dummies, filters adults (25+), and collapses to geographic level.
# @Written_on: 21/01/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_harmonize_census_2005_data <- function(
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
  
  # 1) HELPER FUNCTIONS
  # ===========================================================================
  
  # --- Helper 1: Safely pad missing values -----------------------------------
  safe_pad <- function(x, width) {
    stringr::str_pad(
      ifelse(is.na(x), "0", as.character(x)), width, "left", "0"
    )
  }
  
  # --- Helper 2: Harmonize Education Levels ----------------------------------
  harmonize_education <- function(df) {
    cols <- names(df)
    
    if ("P44B3_NIVEL_ANOS" %in% cols) {
      # Mapping logic for the EXTENDED census dataset
      df <- df %>%
        dplyr::mutate(
          VAL = as.numeric(P44B3_NIVEL_ANOS),
          VAL = dplyr::na_if(VAL, 98),
          VAL = dplyr::na_if(VAL, 99),
          VAL = ifelse(VAL == 43, 0, VAL),
          
          # Vertical expansion for easy auditing
          escolaridad = dplyr::case_when(
            VAL %in% 0:3               ~ 0,
            VAL == 4                   ~ 1,
            VAL == 5                   ~ 2,
            VAL == 6                   ~ 3,
            VAL == 7                   ~ 4,
            VAL == 8                   ~ 5,
            VAL == 9                   ~ 6,
            VAL == 10                  ~ 7,
            VAL == 11                  ~ 8,
            VAL == 12                  ~ 9,
            VAL == 13                  ~ 10,
            VAL == 14                  ~ 12, # Bachillerato
            VAL %in% c(15, 17)         ~ 10,
            VAL %in% c(16, 18)         ~ 12,
            VAL %in% c(19, 21, 23, 26) ~ 13,
            VAL %in% c(20, 22, 24, 27) ~ 14,
            VAL %in% c(25, 28)         ~ 15,
            VAL == 29                  ~ 16,
            VAL %in% 30:33             ~ 17, # Profesional
            VAL == 34                  ~ 18,
            VAL %in% 35:36             ~ 19,
            VAL == 37                  ~ 20,
            VAL == 38                  ~ 21,
            VAL == 39                  ~ 22,
            VAL %in% 40:42             ~ 23,
            .default                   = NA_real_
          )
        )
      
    } else if ("P44B1_TIP_ESTUD" %in% cols) {
      # Mapping logic for the BASIC census dataset
      df <- df %>%
        dplyr::mutate(
          VAL = as.numeric(P44B1_TIP_ESTUD),
          VAL = dplyr::na_if(VAL, 99),
          
          # Vertical expansion for easy auditing
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
            VAL == 11    ~ 19, # Master's
            VAL == 12    ~ 23, # Doctorate
            .default     = NA_real_
          )
        )
    }
    
    # Clean up the temporary VAL column before returning
    return(df %>% dplyr::select(-dplyr::any_of("VAL")))
  }
  
  # 2) MAIN PROCESSING PIPELINE
  # ===========================================================================
  
  process_file <- function(path, type, extended) {
    if (!quiet) message("📖 Processing ", type, " | File: ", basename(path))
    
    # 1. Read Data
    raw <- vroom::vroom(
      path, 
      col_types = vroom::cols(.default = "c"), 
      show_col_types = FALSE,
      delim = ",", 
      trim_ws = TRUE, 
      na = c("", " ", "NA")
    ) %>% 
      dplyr::rename_with(toupper)
    
    # 2. Geometry & Filtering
    df <- raw %>%
      dplyr::mutate(
        LocComuna = if (type == "bogota") {stringr::str_pad(I0B1_LOCALIDAD, 2, "left", "0")
        } else {NA_character_},
        LocCodigo = paste0(
          safe_pad(I0A_DPTO, 2), 
          safe_pad(I0B_MPIO, 3)),
        
        # Build the dynamic GEO_ID based on the geographic class
        GEO_ID = if (!extended) {
          
          # A) Build the absolute maximum 22-digit string
          full_id <- stringr::str_c(
            safe_pad(I0A_DPTO,   2),   
            safe_pad(I0B_MPIO,   3),
            safe_pad(I0C_CLASE,  1),  
            safe_pad(I0D_SECT_R, 3),
            safe_pad(I0D_SEC_R,  2),  
            safe_pad(I0D_CPOB,   3),
            safe_pad(I0D_SECT_U, 4), 
            safe_pad(I0D_SEC_U,  2),
            safe_pad(I0D_MZA,    2)
          )
          
          # B) Truncate ID to precisely match shapefile resolution
          dplyr::case_when(
            I0C_CLASE == "1" ~ full_id,                           # Urban: 22 
            I0C_CLASE == "2" ~ stringr::str_sub(full_id, 1, 14),  # CPOB : 14 
            I0C_CLASE == "3" ~ stringr::str_sub(full_id, 1, 11),  # Rural: 11 
            TRUE ~ full_id
          )
        } else if (extended & type == "bogota") {
          paste0(LocCodigo, LocComuna)
        } else {
          LocCodigo # Fallback to Locality for Extended
        }
      ) %>%
      # Keep only rows belonging to the defined metropolitan area
      dplyr::filter(stringr::str_sub(GEO_ID, 1, 5) %in% metro_codes)
    
    # 3. Demographic Indicators Calculation
    df <- df %>%
      dplyr::mutate(
        raw_age = if ("PC09B_EDAD" %in% names(.)) {as.numeric(PC09B_EDAD)
          } else {as.numeric(PC09B_EDADQ)},
        edad    = if (extended) {raw_age} else {(raw_age - 1) * 5 + 2.5},
        fe      = if ("FACT_EXP_CAL_P_N" %in% names(.)) {round(as.numeric(FACT_EXP_CAL_P_N)) 
        } else {1},
        women   = as.numeric(P25B_SEXO == 2),
        adult   = if (extended) {as.numeric(raw_age >= 25)
          } else {as.numeric(raw_age >= 6)} # Group 6 is 25-29 years
      ) %>%
      harmonize_education() %>%
      dplyr::mutate(
        no_education = as.numeric(escolaridad == 0),
        high_school_incomplete = if (extended) {as.numeric(
          escolaridad >= 1 & escolaridad <= 11)
        } else {as.numeric(escolaridad %in% c(5, 9))},
        high_school_complete   = if (extended) {as.numeric(escolaridad == 12)
          } else {as.numeric(escolaridad == 11)},
        college_incomplete = if (extended) {as.numeric(escolaridad >= 13 & escolaridad <= 16)
          } else {NA_real_},
        college_complete = as.numeric(escolaridad == 17),
        graduate_educ    = as.numeric(escolaridad >= 18),
        employed         = if ("P47B_OCUPACION" %in% names(.)) {
          ifelse(edad >= 25, as.numeric(P47B_OCUPACION %in% 1:2), NA_real_)
        } else {
          NA_real_
        }
      )
    
    return(df)
  }
  
  # 3) EXECUTION AND AGGREGATION
  # ===========================================================================
  
  # Locate the microdata files
  bog_path <- list.files(
    extract_list$bogota$dir_extracted, 
    pattern = "PERH", 
    full.names = TRUE
  )[1]
  
  cun_path <- list.files(
    extract_list$cundinamarca$dir_extracted, 
    pattern = "PERH", 
    full.names = TRUE
  )[1]
  
  # Bind rows from both regions
  all_census <- dplyr::bind_rows(
    process_file(bog_path, "bogota", is_extended),
    process_file(cun_path, "cundinamarca", is_extended)
  )
  
  # Collapse the microdata down to the geographic unit level (GEO_ID)
  collapse_data <- all_census %>%
    dplyr::filter(adult == 1) %>%
    dplyr::group_by(GEO_ID) %>%
    dplyr::summarise(
      n = sum(fe, na.rm = TRUE),
      escolaridad_avg = weighted.mean(escolaridad, fe, na.rm = TRUE),
      
      # Calculate total counts using expansion factors
      dplyr::across(
        c(
          no_education, 
          high_school_incomplete, 
          high_school_complete, 
          college_incomplete, 
          college_complete, 
          graduate_educ, 
          employed
        ),
        ~ sum(.x * fe, na.rm = TRUE)
      ),
      .groups = "drop" 
    ) %>%
    dplyr::mutate(
      # Convert total counts into population shares
      dplyr::across(
        c(
          no_education, 
          high_school_incomplete, 
          high_school_complete, 
          college_incomplete, 
          college_complete, 
          graduate_educ, 
          employed
        ),
        ~ .x / n, 
        .names = "share_{.col}_pop"
      )
    )
  
  # EXPORT
  # ===========================================================================
  prefix <- if (is_extended) "extended" else "basic"
  
  vroom::vroom_write(
    all_census, 
    file.path(out_dir, paste0("census_metro_individual_", prefix, ".csv")), 
    delim = ","
  )
  
  vroom::vroom_write(
    collapse_data, 
    file.path(out_dir, paste0("collapse_metro_area_", prefix, ".csv")), 
    delim = ","
  )
  
  return(list(individual = all_census, collapsed = collapse_data))
}


# --------------------------------------------------------------------------------------------
# Function: bogota_filter_census_2018
# @Arg      : census_folder  — Path to folder containing "2018_Bogota.zip", etc.
# @Arg      : out_dir        — Where to write selected CSVs
# @Arg      : overwrite      — Re-extract if output exists (default FALSE)
# @Arg      : quiet          — Suppress messages (default FALSE)
# 
# @Output   : list with $bogota and $cundinamarca paths
# @Purpose  : Handles the specific nested structure of DANE 2018:
#             1. 2018_Bogota.zip (Outer)
#             2. Unpacks to folder
#             3. Finds *CSV.zip (Inner)
#             4. Extracts *5PER* (Persons) and *MGN* (Geo) CSVs.
# @Written_on: 01/02/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_filter_census_2018 <- function(
    census_folder = here::here(bogota_cfg$dl_dir, "census_2018"),
    out_dir       = here::here("data", "raw", "census", "Bogota", "CNPV2018"),
    overwrite     = FALSE,
    quiet         = FALSE
) {
  
  if (!dir.exists(census_folder)) stop("Census folder not found: ", census_folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- Helpers ---
  sys_extract <- function(zipfile, exdir) {
    # Simple unzip wrapper
    utils::unzip(zipfile, exdir = exdir, overwrite = TRUE)
  }
  
  process_dept_2018 <- function(zip_name, dept_name, target_dir) {
    
    full_zip_path <- file.path(census_folder, zip_name)
    if (!file.exists(full_zip_path)) {
      if (!quiet) message("⚠️ ZIP not found: ", zip_name, " (Skipping)")
      return(NULL)
    }
    
    if (dir.exists(target_dir) && !overwrite) {
      if (!quiet) message("↪︎ Using existing folder: ", target_dir)
      return(target_dir)
    }
    
    # 1. Extract Outer ZIP to Temp
    temp_outer <- file.path(tempdir(), paste0("outer_", dept_name))
    if (dir.exists(temp_outer)) unlink(temp_outer, recursive = TRUE)
    dir.create(temp_outer, showWarnings = FALSE)
    
    if (!quiet) message("📦 Extracting Outer ZIP: ", zip_name)
    sys_extract(full_zip_path, temp_outer)
    
    # 2. Find Inner CSV ZIP
    # Usually named like "11_Bogota_CSV.zip" inside a subfolder
    inner_zips <- list.files(temp_outer, pattern = "CSV\\.zip$", 
                             recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    
    if (length(inner_zips) == 0) stop("No *CSV.zip found inside ", zip_name)
    
    # 3. Extract Inner ZIP to Temp
    temp_inner <- file.path(tempdir(), paste0("inner_", dept_name))
    if (dir.exists(temp_inner)) unlink(temp_inner, recursive = TRUE)
    dir.create(temp_inner, showWarnings = FALSE)
    
    if (!quiet) message("   ↳ Extracting Inner ZIP: ", basename(inner_zips[1]))
    sys_extract(inner_zips[1], temp_inner)
    
    # 4. Locate Target CSVs (PER = Persons, MGN = Manzanas/Geo)
    # Pattern is loose to handle DANE inconsistency (case, extra chars)
    csv_files <- list.files(temp_inner, pattern = "\\.CSV$", 
                            recursive = TRUE, full.names = TRUE)
    
    target_csvs <- csv_files[grepl("5PER|MGN", basename(csv_files), ignore.case = TRUE)]
    
    if (length(target_csvs) == 0) stop("No PER or MGN csvs found in ", zip_name)
    
    # 5. Move to Final Directory
    if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE)
    dir.create(target_dir, recursive = TRUE)
    
    for (f in target_csvs) {
      file.copy(f, file.path(target_dir, basename(f)))
    }
    
    # Cleanup
    unlink(temp_outer, recursive = TRUE)
    unlink(temp_inner, recursive = TRUE)
    
    return(target_dir)
  }
  
  # --- Execution ---
  
  # 1. Bogota (11)
  # Look for zip starting with 2018...Bogota...zip
  bog_zip <- list.files(census_folder, pattern = "2018.*Bogot.*\\.zip$", ignore.case = TRUE)
  if(length(bog_zip) == 0) bog_zip <- "2018_Bogota.zip" else bog_zip <- bog_zip[1]
  
  path_bog <- process_dept_2018(bog_zip, "bogota", file.path(out_dir, "11.Bogota"))
  
  # 2. Cundinamarca (25)
  cun_zip <- list.files(census_folder, pattern = "2018.*Cundinamarca.*\\.zip$",
                        ignore.case = TRUE)
  if(length(cun_zip) == 0) cun_zip <- "2018_Cundinamarca.zip" else cun_zip <- cun_zip[1]
  
  path_cun <- process_dept_2018(cun_zip, "cundinamarca",
                                file.path(out_dir, "25.Cundinamarca"))
  
  return(list(bogota = path_bog, cundinamarca = path_cun))
}


# --------------------------------------------------------------------------------------------
# Function: bogota_harmonize_census_2018_data
#
# @Arg extract_paths : list; output from bogota_filter_census_2018.
# @Arg metro_codes   : character; municipality codes to filter.
# @Arg out_dir       : string; output folder for processed data.
# @Arg quiet         : logical; suppress progress messages. Default FALSE.
#
# @Output : list(individual, collapsed); processed census data.
#
# @Purpose:
#   Merges person and geographic files, filters the Bogota metro area,
#   harmonizes education and demographic variables, and collapses adults
#   aged 25+ to the block level.
#
# @Written_on : 01/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_harmonize_census_2018_data <- function(
    extract_paths,
    metro_codes = bogota_cfg$city_code_metro,
    out_dir     = here::here("data", "working_data"),
    quiet       = FALSE
) {
  
  # Check required packages
  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop("vroom required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr required.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr required.")
  }
  
  # Create output folder
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Map 2018 categorical schooling variable into approximate years
  map_education_2018 <- function(x) {
    dplyr::case_when(
      x == 10 ~ 0,
      x == 1  ~ 0,
      x == 2  ~ 5,
      x == 3  ~ 9,
      x == 4  ~ 11,
      x == 5  ~ 11,
      x == 6  ~ 13,
      x == 7  ~ 14,
      x == 8  ~ 17,
      x == 9  ~ 19,
      TRUE    ~ NA_real_
    )
  }
  
  # Process one department folder
  process_dept_files <- function(folder) {
    if (is.null(folder)) {
      return(NULL)
    }
    
    # Identify person and geographic files
    files <- list.files(folder, full.names = TRUE)
    f_per <- files[grepl("5PER", basename(files))][1]
    f_mgn <- files[grepl("MGN", basename(files))][1]
    
    if (is.na(f_per) || is.na(f_mgn)) {
      warning("Missing PER or MGN file in: ", folder)
      return(NULL)
    }
    
    if (!quiet) {
      message("Reading: ", basename(f_per), " + ", basename(f_mgn))
    }
    
    # Read person file
    df_per <- vroom::vroom(
      f_per,
      col_types = vroom::cols(.default = "c"),
      show_col_types = FALSE
    ) %>%
      dplyr::select(
        COD_ENCUESTAS,
        P_SEXO,
        P_EDADR,
        P_NIVEL_ANOSR,
        P_TRABAJO
      )
    
    # Read geographic file
    df_mgn <- vroom::vroom(
      f_mgn,
      col_types = vroom::cols(.default = "c"),
      show_col_types = FALSE
    ) %>%
      dplyr::select(COD_ENCUESTAS, COD_DANE_ANM)
    
    # Merge people with geography and filter municipalities
    joined <- df_per %>%
      dplyr::inner_join(df_mgn, by = "COD_ENCUESTAS") %>%
      dplyr::mutate(MUNI_CODE = stringr::str_sub(COD_DANE_ANM, 1, 5)) %>%
      dplyr::filter(MUNI_CODE %in% metro_codes)
    
    # Harmonize variables
    joined <- joined %>%
      dplyr::mutate(
        GEO_ID = COD_DANE_ANM,
        women = as.numeric(P_SEXO == "2"),
        
        raw_group = as.numeric(P_EDADR),
        edad      = (raw_group - 1) * 5 + 2.5,
        adult     = as.numeric(raw_group >= 6),
        
        escolaridad = map_education_2018(as.numeric(P_NIVEL_ANOSR)),
        
        no_education         = as.numeric(escolaridad == 0),
        high_school_complete = as.numeric(escolaridad %in% 11:12),
        college_complete     = as.numeric(escolaridad == 17),
        graduate_educ        = as.numeric(escolaridad >= 18),
        
        employed = ifelse(
          adult == 1,
          as.numeric(P_TRABAJO %in% c("1", "2")),
          NA_real_
        ),
        
        fe = 1
      )
    
    return(joined)
  }
  
  # Process Bogota and Cundinamarca
  if (!quiet) {
    message("Processing Bogota.")
  }
  df_bog <- process_dept_files(extract_paths$bogota)
  
  if (!quiet) {
    message("Processing Cundinamarca.")
  }
  df_cun <- process_dept_files(extract_paths$cundinamarca)
  
  # Bind all departments
  all_census <- dplyr::bind_rows(df_bog, df_cun)
  
  # Collapse adults to block level
  if (!quiet) {
    message("Collapsing to block level using adults 25+.")
  }
  
  collapse_data <- all_census %>%
    dplyr::filter(adult == 1) %>%
    dplyr::group_by(GEO_ID) %>%
    dplyr::summarise(
      weight = sum(fe, na.rm = TRUE),
      n      = weight,
      
      education_mean = sum(escolaridad * fe, na.rm = TRUE) / weight,
      escolaridad     = education_mean,
      escolaridad_avg = education_mean,
      
      count_no_ed   = sum(no_education * fe, na.rm = TRUE),
      count_hs_com  = sum(high_school_complete * fe, na.rm = TRUE),
      count_col_com = sum(college_complete * fe, na.rm = TRUE),
      count_grad    = sum(graduate_educ * fe, na.rm = TRUE),
      
      count_employed = sum(employed * fe, na.rm = TRUE),
      count_women    = sum(women * fe, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      share_no_ed_pop   = count_no_ed / weight,
      share_hs_com_pop  = count_hs_com / weight,
      share_col_com_pop = count_col_com / weight,
      share_grad_pop    = count_grad / weight,
      
      share_employed_pop = count_employed / weight,
      share_women_pop    = count_women / weight,
      
      share_graduate_educ_pop = share_grad_pop,
      share_employed          = share_employed_pop
    )
  
  # Save outputs
  if (!quiet) {
    message("Saving processed Bogota census files.")
  }
  
  vroom::vroom_write(
    all_census,
    file.path(out_dir, "census_2018_metro_individual.csv"),
    delim = ","
  )
  
  vroom::vroom_write(
    collapse_data,
    file.path(out_dir, "census_2018_metro_collapsed.csv"),
    delim = ","
  )
  
  return(list(individual = all_census, collapsed = collapse_data))
}


# ============================================================================================
# ============================================================================================
#  Bogota's wrapper functions
# ============================================================================================
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: bogota_download
# @Arg    : cfg          — bogota_cfg list. Default: bogota_cfg.
# @Arg    : steps        — character vector; which steps to run. Default: all.
#                          Options: "metro_area", "stations_geo", "station_data",
#                                   "metro_station_data", "census"
# @Arg    : years        — integer vector; override cfg$years if provided.
# @Arg    : timeout_page — numeric; Selenium page timeout in seconds.
# @Arg    : timeout_btn  — numeric; Selenium button timeout in seconds.
# @Arg    : timeout_dl   — numeric; Selenium download timeout in seconds.
# @Arg    : quiet        — logical; suppress step banners. Default FALSE.
#
# @Purpose   : Run every download step for Bogotá in the correct order.
#              Steps mirror scripts/download_data/download_bogota_data.R.
# @Output    : named list; one entry per step with the function return
#              value, or a condition object if the step failed.
# @Written_on: 01/02/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_download <- function(
    cfg          = bogota_cfg,
    steps        = c("metro_area", "stations_geo",
                     "station_data", "metro_station_data",
                     "census"),
    years        = NULL,
    timeout_page = 30,
    timeout_btn  = 30,
    timeout_dl   = 400,
    quiet        = FALSE
) {
  if (!is.null(years)) cfg$years <- years
  dir.create(cfg$dl_dir, recursive = TRUE, showWarnings = FALSE)
  
  results <- list()
  
  .step <- function(name, expr) {
    if (!name %in% steps) return(invisible(NULL))
    if (!quiet) message("\n--- [bogota] ", name, " ---")
    results[[name]] <<- tryCatch(
      expr,
      error = function(e) {
        warning("[bogota] step '", name, "' failed: ", e$message)
        e
      }
    )
  }
  
  # 1. Metropolitan area shapefiles (four levels × two census years)
  .step("metro_area", {
    configs <- list(
      list(level = "mpio_localidad", yr = 2005,
           out = "bogota_area_metro_2005.gpkg"),
      list(level = "mpio",           yr = 2005,
           out = "bogota_area_metro_municipalities_2005.gpkg"),
      list(level = "manzana",        yr = 2005,
           out = "bogota_area_metro_census_tracts_2005.gpkg"),
      list(level = "mpio_localidad", yr = 2018,
           out = "bogota_area_metro_2018.gpkg"),
      list(level = "manzana",        yr = 2018,
           out = "bogota_area_metro_census_tracts_2018.gpkg")
    )
    lapply(configs, function(x) {
      bogota_download_metro_area(
        level        = x$level,
        mgn_year     = x$yr,
        base_url     = cfg$base_url_shp,
        download_dir = file.path(cfg$dl_dir, "metro_area"),
        out_file     = here::here("data", "raw", "geospatial_data",
                                  "bogota", x$out)
      )
    })
  })
  
  # 2. Station geo-location (RMCAB scrape + SISAIRE metadata)
  .step("stations_geo", {
    bogota_scrape_rmcab_station_table(
      page_url      = cfg$url_station_shp,
      parse_coords  = TRUE,
      harmonize_map = cfg$station_nme_map,
      dedupe        = TRUE,
      verbose       = !quiet,
      out_dir       = file.path(cfg$dl_dir,
                                "ground_stations_geolocation"),
      out_name      = "bogota_stations_location",
      write_csv     = TRUE
    )
    sisaire_download_department_metadata(
      base_url     = cfg$base_url_sisaire,
      timeout_page = timeout_page,
      subdir       = file.path("bogota", "stations_metadata")
    )
  })
  
  # 3. RMCAB hourly station data (city proper)
  .step("station_data", {
    bogota_download_station_data(
      base_url     = cfg$base_url_rmcab,
      start_year   = min(cfg$years),
      end_year     = max(cfg$years),
      timeout_page = timeout_page,
      timeout_btn  = timeout_btn,
      timeout_dl   = timeout_dl,
      subdir       = file.path("bogota", "ground_stations")
    )
  })
  
  # 4. SISAIRE metro-area hourly data
  .step("metro_station_data", {
    log <- sisaire_download_hourly_data(
      base_url     = cfg$base_url_sisaire,
      target_depts = cfg$which_states,
      years_range  = cfg$years,
      subdir       = file.path("bogota",
                               "metro_ground_stations_hourly")
    )
    utils::write.csv(
      log,
      file      = file.path(cfg$dl_dir, "metro_stations_log.csv"),
      row.names = FALSE
    )
    log
  })
  
  # 5. Census data (2005 Basic, 2005 Extended, 2018)
  .step("census", {
    bogota_download_census_data(
      year            = 2005,
      type            = "BASICO",
      url             = cfg$base_url_census,
      download_folder = file.path(cfg$dl_dir, "census")
    )
    bogota_download_census_data(
      year            = 2005,
      type            = "AMPLIADO",
      url             = cfg$base_url_census,
      download_folder = file.path(cfg$dl_dir, "census")
    )
    bogota_download_census_data(
      year            = 2018,
      url             = cfg$base_new_census,
      download_folder = file.path(cfg$dl_dir, "census")
    )
  })
  
  if (!quiet) message("\n[bogota] download complete.")
  invisible(results)
}


# --------------------------------------------------------------------------------------------
# Function: bogota_process
# @Arg       : cfg      — bogota_cfg list. Default: bogota_cfg.
# @Arg       : steps    — character vector; which steps to run. Default: all.
#              Options: "stations_filter", "pollution_parquet", "census_2005", "census_2018"
# @Arg       : quiet    — logical; suppress step banners. Default FALSE.
#
# @Purpose   : Run every processing step for Bogotá in the correct order.
#              Steps mirror scripts/process_data/process_bogota_data.R.
# @Output    : named list; one entry per step.
# @Written_on: 01/02/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_process <- function(
    cfg   = bogota_cfg,
    steps = c("stations_filter", "pollution_parquet",
              "census_2005",     "census_2018"),
    quiet = FALSE
) {
  outdir_pollution  <- here::here(cfg$out_dir, "monitoring_stations")
  outdir_geospatial <- here::here(cfg$out_dir, "geospatial_data")
  outdir_stations   <- file.path(cfg$dl_dir,
                                 "ground_stations_geolocation")
  outdir_metadata   <- file.path(cfg$dl_dir, "stations_metadata")
  
  dir.create(outdir_pollution,  recursive = TRUE,
             showWarnings = FALSE)
  dir.create(outdir_geospatial, recursive = TRUE,
             showWarnings = FALSE)
  
  results <- list()
  
  .step <- function(name, expr) {
    if (!name %in% steps) return(invisible(NULL))
    if (!quiet) message("\n--- [bogota] ", name, " ---")
    results[[name]] <<- tryCatch(
      expr,
      error = function(e) {
        warning("[bogota] step '", name, "' failed: ", e$message)
        e
      }
    )
  }
  
  # 1. Filter stations spatially (2005 and 2018 metro boundaries)
  .step("stations_filter", {
    stations_bogota <- read.csv(
      file.path(outdir_stations, "bogota_stations_location.csv")
    )
    metro_2018 <- sf::st_read(
      here::here(outdir_geospatial, "bogota",
                 "bogota_area_metro_2018.gpkg"),
      quiet = TRUE
    )
    metro_2005 <- sf::st_read(
      here::here(outdir_geospatial, "bogota",
                 "bogota_area_metro_2005.gpkg"),
      quiet = TRUE
    )
    bogota_filter_stations_in_metro(
      rmcab_df     = stations_bogota,
      metadata_dir = outdir_metadata,
      radius_km    = 20,
      metro_area   = metro_2018,
      out_file     = here::here(outdir_geospatial, "bogota",
                                "bogota_2018_stations_buffer_metro.gpkg")
    )
    bogota_filter_stations_in_metro(
      rmcab_df     = stations_bogota,
      metadata_dir = outdir_metadata,
      radius_km    = 20,
      metro_area   = metro_2005,
      out_file     = here::here(outdir_geospatial, "bogota",
                                "bogota_2005_stations_buffer_metro.gpkg")
    )
  })
  
  # 2. Merge raw XLSX / CSV downloads → Parquet Arrow dataset
  .step("pollution_parquet", {
    # Re-read stations_kept from the 2018 buffer (the canonical one)
    stations_kept <- sf::st_read(
      here::here(outdir_geospatial, "bogota",
                 "bogota_2018_stations_buffer_metro.gpkg"),
      quiet = TRUE
    )
    bogota_process_stations_data_to_parquet(
      rmcab_folder   = file.path(cfg$dl_dir, "ground_stations"),
      sisaire_folder = file.path(cfg$dl_dir,
                                 "metro_ground_stations_hourly"),
      stations_sf    = stations_kept,
      tz             = "UTC",
      out_dir        = outdir_pollution,
      out_name       = "bogota_metro"
    )
  })
  
  # 3. Census 2005 (Basic + Extended)
  .step("census_2005", {
    raw_census <- here::here("data", "raw", "census", "bogota")
    
    ext <- bogota_filter_census_2005(
      census_zip = file.path(cfg$dl_dir, "census",
                             "CG2005_AMPLIADO.zip"),
      out_dir    = file.path(raw_census, "CG2005_EXTENDED"),
      overwrite  = TRUE,
      quiet      = quiet
    )
    bogota_harmonize_census_2005_data(
      extract_list = ext,
      metro_codes  = cfg$city_code_metro,
      out_dir      = here::here("data", "interim", "census",
                                "bogota_extended_2005")
    )
    
    bas <- bogota_filter_census_2005(
      census_zip = file.path(cfg$dl_dir, "census",
                             "CG2005_BASICO.zip"),
      out_dir    = file.path(raw_census, "CG2005_BASIC"),
      overwrite  = FALSE,
      quiet      = quiet
    )
    bogota_harmonize_census_2005_data(
      extract_list = bas,
      is_extended  = FALSE,
      metro_codes  = cfg$city_code_metro,
      out_dir      = here::here("data", "interim", "census",
                                "bogota_basic_2005")
    )
  })
  
  # 4. Census 2018
  .step("census_2018", {
    raw_census <- here::here("data", "raw", "census", "bogota")
    
    paths <- bogota_filter_census_2018(
      census_folder = file.path(cfg$dl_dir, "census"),
      out_dir       = file.path(raw_census, "CNPV_2018"),
      overwrite     = FALSE,
      quiet         = quiet
    )
    bogota_harmonize_census_2018_data(
      extract_paths = paths,
      metro_codes   = cfg$city_code_metro,
      out_dir       = here::here("data", "interim", "census",
                                 "bogota_2018")
    )
  })
  
  if (!quiet) message("\n[bogota] processing complete.")
  invisible(results)
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
