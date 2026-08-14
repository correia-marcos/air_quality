# ============================================================================================
# IDB: Air monitoring — Santiago module
# ============================================================================================
#' @Goal  : Santiago-specific parameters, download/process wrappers, and any site-specific code
#' @Date   : Out 2025
#' @Author : Marcos Paulo
# Obs: Expect the caller to have already sourced:
#   - src/config_utils_download_data.R  (selenium helpers, waits, clicking helpers, etc.)
#   - src/config_utils_process_data.R   (merge, tidy, QA, parquet writing, etc.)
#   - src/cities/registry.R
# 
# Others obs:
# Definition of the metropolitan area comes from the Supreme Decrees No. 337 and No. 326 in the 
# Official Gazette (2023):
# — DECRETO 337 (17 de noviembre de 2023) CONSTITUYE ÁREA METROPOLITANA DE SANTIAGO
# ============================================================================================

# Parameters (single source)
santiago_cfg <- list(
  id               = "santiago",
  tz               = "America/Santiago",
  base_url_shp     = "https://censo2024.ine.gob.cl/resultados/",
  base_url_dpa_17  = "https://services5.arcgis.com/hUyD8u3TeZLKPe4T/arcgis/rest/services",
  base_url_sinca   = "https://sinca.mma.gob.cl/index.php/redes",
  base_url_census  = "https://www.ine.gob.cl/docs/default-source",
  base_new_census  = "https://storage.googleapis.com/bktdescargascenso2024/",
  years            = 2000L:2023L,
  dl_dir           = here::here("data", "downloads", "santiago"),
  out_dir          = here::here("data", "raw"),
  which_states     = c("Libertador General Bernardo O'Higgimns", "Metropolitana de Santiago",
                       "Valparaíso"), 
  cities_in_metro  = c("Buin", "Calera de Tango", "Cerrillos", "Cerro Navia", "Colina",
                       "Conchalí", "El Bosque", "El Monte", "Estación Central", "Huechuraba", 
                       "Independencia", "Isla de Maipo", "La Cisterna", "La Florida",
                       "La Granja", "La Pintana", "La Reina", "Lampa", "Las Condes",
                       "Lo Barnechea", "Lo Espejo", "Lo Prado", "Macul", "Maipú", "María Pinto",
                       "Ñuñoa", "Padre Hurtado", "Paine", "Pedro Aguirre Cerda", "Peñalolén", 
                       "Pirque", "Providencia", "Pudahuel", "Puente Alto", "Quilicura",
                       "Quinta Normal", "Recoleta", "Renca", "San Bernardo", "San Joaquín", 
                       "San José de Maipo", "San Miguel", "San Ramón", "Santiago", "Talagante",
                       "Tiltil", "Vitacura", "Peñaflor"),

  # INE names -> the canonical schema of doc/data_dictionary.md. Two vintages with
  # different units of analysis, so each carries its own mapping. Applied by
  # apply_canonical_names(); see that function's @details.
  schema = list(
    # 2017: the analysis unit is the zona censal, and `comuna` is a COARSER unit that
    # must not be confused with it, so it is kept under a distinct name.
    zona_2017 = list(
      geo_level    = "zona_censal",
      census_micro = c(
        zona_id     = "geo_id",           # geocodigo: CUT(5)+distrito(2)+area(1)+zona(3)
        comuna      = "comuna_id",        # coarser than geo_id -- see @details
        fe          = "person_weight",    # injected as 1
        escolaridad = "raw_escolaridad"), # raw code; differs from educ_years
      census_geo   = c(
        zona_id = "geo_id",
        weight  = "pop_total",
        n       = "n_records"),           # the one file where n is a genuine count
      raw          = c("hogar_ref_id", "p07", "p08", "p09", "p14", "p15", "p16", "p17")),
    # 2024: the analysis unit is the comuna itself, spelled `comuna` in the individual
    # file and `CUT` in the collapsed one. Both become geo_id.
    comuna_2024 = list(
      geo_level    = "comuna",
      census_micro = c(
        comuna = "geo_id",
        fe     = "person_weight"),
      census_geo   = c(
        CUT    = "geo_id",
        weight = "pop_total"),
      raw          = c("parentesco", "sexo", "sit_fuerza_trabajo", "p28_autoid_pueblo")),
    stations = c(station = "station_id"))
)

# ============================================================================================
#  Santiago-specific functions - downloading and its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: santiago_download_metro_area_2024
#
#' @param type             string; "metro_santiago" or "gran_santiago".
#' @param level            string; "mpio" or "manzana".
#' @param base_url         string; INE Census 2024 results URL.
#' @param keep_municipality character vector; municipalities to keep.
#' @param download_dir     string; local path to save the raw ZIP file.
#' @param out_file         string; local path to save the processed GeoPackage.
#' @param dissolve_by      string or NULL; id column whose repeated values are
#                          merged into one polygon. Default NULL (no merging).
#' @param overwrite_zip    logical; re-download ZIP if it exists. Default FALSE.
#' @param overwrite_gpkg   logical; overwrite output GeoPackage. Default TRUE.
#' @param container        logical; TRUE if running with Docker Selenium.
#' @param quiet            logical; suppress messages. Default FALSE.
#
#' @return  sf object containing the filtered spatial data.
#' @details
#   Downloads the INE 2024 census cartography, filters the requested Santiago
#   spatial definition, linearizes curved geometries, repairs validity, and
#   saves the result as a GeoPackage. Linearization is required because the INE
#   layer can contain MULTISURFACE/CURVEPOLYGON geometries that may fail in
#   st_make_valid() and downstream distance calculations.
#
#   When `dissolve_by` is supplied, rows sharing that id are unioned into a
#   single polygon. INE splits some comunas into several "entidades" — Lampa
#   (CUT 13302) arrives as CHICAUMA - VALLE GRANDE plus ESTACIÓN COLINA — so the
#   layer carries more rows than the census, which is one row per comuna. Left
#   unmerged, the comuna gets two representative points and appears twice in the
#   geo-to-station distance matrix. Attributes that disagree across the merged
#   rows are set to NA, because no single value describes the merged unit.
#
#   The layer keeps INE's own CRS, EPSG:4674 (SIRGAS 2000), so data/raw/ stays
#   faithful to the source. The 2017 zonas arrive in EPSG:4326 instead; both are
#   ITRF-aligned and every consumer reprojects, so the split is harmless.
#
#' @Written_on : 25/10/2025
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
santiago_download_metro_area_2024 <- function(
    type              = c("metro_santiago", "gran_santiago"),
    level             = c("mpio", "manzana"),
    base_url          = santiago_cfg$base_url_shp,
    keep_municipality = santiago_cfg$cities_in_metro,
    download_dir      = here::here("data", "downloads", "Administrative", "Chile"),
    out_file          = here::here("data", "raw", "admin", "Chile",
                                   "santiago_metro.gpkg"),
    dissolve_by       = NULL,
    overwrite_zip     = FALSE,
    overwrite_gpkg    = TRUE,
    container         = TRUE,
    quiet             = FALSE
) {
  
  # 0. Match arguments and check packages
  # -----------------------------------------------------------------------
  type  <- match.arg(tolower(type), c("metro_santiago", "gran_santiago"))
  level <- match.arg(tolower(level), c("mpio", "manzana"))
  
  pkgs <- c("sf", "selenium")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required. Add to renv.")
    }
  }
  
  # 1. Define paths
  # -----------------------------------------------------------------------
  root_dl_dir <- here::here("data", "downloads")
  
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  zip_browser_name <- "Cartografia_censo2024_Pais.zip"
  zip_landing_path <- file.path(root_dl_dir, zip_browser_name)
  zip_target_path  <- file.path(download_dir, zip_browser_name)
  
  # 2. Helpers
  # -----------------------------------------------------------------------
  # Normalize Spanish municipality names for matching.
  .norm_name <- function(x) {
    x <- toupper(x)
    x <- chartr("áéíóúÁÉÍÓÚñÑ", "AEIOUAEIOUnN", x)
    trimws(x)
  }
  
  # Linearize curved geometries and repair validity.
  .regularize_polygon_geometry <- function(x) {
    
    # Remove Z/M dimensions if present.
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    
    # Detect curved or surface geometry types.
    geom_types <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
    
    has_curves <- any(
      grepl("CURVE|SURFACE|CIRCULAR|COMPOUND", geom_types,
            ignore.case = TRUE)
    )
    
    # GDAL linearization is safer than st_make_valid() for CURVEPOLYGON.
    if (has_curves) {
      tmp_in  <- tempfile("santiago_curved_", fileext = ".gpkg")
      tmp_out <- tempfile("santiago_linear_", fileext = ".gpkg")
      
      on.exit(unlink(c(tmp_in, tmp_out), recursive = TRUE, force = TRUE),
              add = TRUE)
      
      sf::st_write(
        x,
        tmp_in,
        layer = "geo",
        delete_dsn = TRUE,
        quiet = TRUE
      )
      sf::gdal_utils(
        util = "vectortranslate",
        source = tmp_in,
        destination = tmp_out,
        options = c(
          "-f", "GPKG",
          "-nlt", "CONVERT_TO_LINEAR",
          "-nln", "geo"
        )
      )
      x <- sf::st_read(tmp_out, layer = "geo", quiet = TRUE)
    }
    
    # Repair validity after linearization.
    x <- sf::st_make_valid(x)
    
    # Keep polygonal components if validation returns geometry collections.
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    
    # Use MULTIPOLYGON for stable downstream processing.
    x <- suppressWarnings(sf::st_cast(x, "MULTIPOLYGON", warn = FALSE))

    return(x)
  }
  # Merge rows that share an id into one polygon, so the geometry unit matches the census unit. 
  .dissolve_by_id <- function(x, id_col) {

    if (!id_col %in% names(x)) {
      stop("Column '", id_col, "' not found; cannot dissolve.")
    }

    ids     <- as.character(x[[id_col]])
    dup_ids <- unique(ids[duplicated(ids)])

    if (length(dup_ids) == 0L) {
      return(x)
    }

    if (!quiet) {
      message("[santiago_area] Dissolving ", length(dup_ids), " repeated ",
              id_col, " value(s): ", paste(dup_ids, collapse = ", "))
    }

    attr_names <- setdiff(names(x), attr(x, "sf_column"))

    # Rebuild one row per id, keeping the layer's original id order.
    parts <- lapply(unique(ids), function(id) {

      rows <- x[ids == id, ]

      if (nrow(rows) == 1L) {
        return(rows)
      }

      # Union the parts into a single geometry for this id.
      merged <- rows[1, ]
      sf::st_geometry(merged) <- sf::st_union(sf::st_geometry(rows))

      # Drop attributes that differ across the merged rows: issue otherwise
      for (nm in attr_names) {
        if (length(unique(rows[[nm]])) > 1L) merged[[nm]][1] <- NA
      }

      merged
    })

    out <- do.call(rbind, parts)

    suppressWarnings(sf::st_cast(out, "MULTIPOLYGON", warn = FALSE))
  }

  # 3. Download ZIP with Selenium, if needed
  # -----------------------------------------------------------------------
  if (!file.exists(zip_target_path) || isTRUE(overwrite_zip)) {
    
    if (!quiet) {
      message("[santiago_area] Starting Selenium download from INE.")
    }
    
    if (!container) {
      if (!quiet) {
        message("[santiago_area] Starting local Selenium container on 4445.")
      }
      
      cid <- system(
        paste(
          "docker run -d -p 4445:4444 --shm-size=2g",
          "selenium/standalone-firefox:4.34.0-20250717"
        ),
        intern = TRUE
      )
      
      on.exit(
        try(system(sprintf("docker rm -f %s", cid), intern = TRUE),
            silent = TRUE),
        add = TRUE
      )
      
      selenium_host <- "localhost"
      selenium_port <- 4445L
      
    } else {
      selenium_host <- "selenium"
      selenium_port <- 4444L
    }
    
    download_dir_container <- if (container) {
      "/home/seluser/Downloads"
    } else {
      root_dl_dir
    }
    
    caps <- list(
      browserName = "firefox",
      "moz:firefoxOptions" = list(
        prefs = list(
          "browser.download.folderList" = 2L,
          "browser.download.dir" = download_dir_container,
          "browser.download.useDownloadDir" = TRUE,
          "browser.helperApps.neverAsk.saveToDisk" =
            "application/zip,application/octet-stream"
        )
      )
    )
    
    session <- selenium::SeleniumSession$new(
      browser = "firefox",
      host = selenium_host,
      port = selenium_port,
      capabilities = caps,
      timeout = 120
    )
    
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    
    if (!quiet) {
      message("[santiago_area] Navigating to: ", base_url)
    }
    
    session$navigate(base_url)
    Sys.sleep(8)
    
    if (!quiet) {
      message("[santiago_area] Switching to application iframe.")
    }
    
    frames <- session$find_elements("css selector", ".iframe-container iframe")
    
    if (length(frames) == 0L) {
      frames <- session$find_elements("css selector", "iframe")
    }
    
    if (length(frames) > 0L) {
      session$switch_to_frame(frames[[1]])
      Sys.sleep(2)
    } else {
      stop("Could not find the application iframe.")
    }
    
    if (!quiet) {
      message("[santiago_area] Opening results tab.")
    }
    
    xpath_res <- paste0(
      "//button[contains(@class, 'tab') and contains(text(), 'Resultados')]"
    )
    
    clicked_res <- FALSE
    
    for (k in 1:5) {
      el <- try(session$find_element("xpath", xpath_res), silent = TRUE)
      
      if (!inherits(el, "try-error")) {
        el$click()
        clicked_res <- TRUE
        break
      }
      
      Sys.sleep(1)
    }
    
    if (!clicked_res) {
      stop("Could not find the 'Resultados' button.")
    }
    
    Sys.sleep(2)
    
    if (!quiet) {
      message("[santiago_area] Opening census cartography section.")
    }
    
    session$find_element(
      "xpath",
      "//button[contains(., 'Cartografía Censal')]"
    )$click()
    
    Sys.sleep(3)
    
    if (!quiet) {
      message("[santiago_area] Starting cartography download.")
    }
    
    xpath_dl <- paste0(
      "//li[.//strong[contains(text(), 'Cartografía País Censo 2024')]]",
      "//button[contains(@class, 'btn-descargar')]"
    )
    
    dl_btn <- try(session$find_element("xpath", xpath_dl), silent = TRUE)
    
    if (inherits(dl_btn, "try-error")) {
      xpath_dl_alt <- paste0(
        "//button[contains(@class, 'btn-descargar')]",
        "[.//ancestor::li[contains(., 'Cartografía País')]]"
      )
      
      dl_btn <- session$find_element("xpath", xpath_dl_alt)
    }
    
    dl_btn$click()
    
    if (!quiet) {
      message("[santiago_area] Waiting for ZIP download in: ", root_dl_dir)
    }
    
    download_success <- FALSE
    
    for (i in 1:900) {
      
      if (file.exists(zip_landing_path)) {
        parts <- list.files(root_dl_dir, pattern = "\\.part$",
                            full.names = TRUE)
        
        if (length(parts) == 0L) {
          file_size <- file.info(zip_landing_path)$size
          
          if (!is.na(file_size) && file_size > 100 * 1024^2) {
            
            if (!quiet) {
              message("[santiago_area] Moving ZIP to: ", zip_target_path)
            }
            
            if (file.exists(zip_target_path)) {
              unlink(zip_target_path)
            }
            
            copy_ok <- file.copy(
              from = zip_landing_path,
              to = zip_target_path,
              overwrite = TRUE
            )
            
            if (copy_ok) {
              unlink(zip_landing_path)
              
              if (!quiet) {
                message("[santiago_area] Download completed.")
              }
              
              download_success <- TRUE
              break
            } else {
              stop("Failed to copy ZIP file. Check permissions.")
            }
          }
        }
      }
      
      Sys.sleep(1)
      
      if (i %% 30 == 0L && !quiet) {
        message("[santiago_area] Download still in progress.")
      }
    }
    
    if (!download_success) {
      stop("Timeout: ZIP file did not appear in ", root_dl_dir)
    }
    
  } else {
    if (!quiet) {
      message("[santiago_area] ZIP already present: ", zip_target_path)
    }
  }
  
  # 4. Extract ZIP and locate GeoPackage
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[santiago_area] Extracting cartography ZIP.")
  }
  
  exdir <- file.path(tempdir(), "santiago_carto_2024")
  
  if (dir.exists(exdir)) {
    unlink(exdir, recursive = TRUE, force = TRUE)
  }
  
  dir.create(exdir)
  utils::unzip(zip_target_path, exdir = exdir)
  
  gpkg_found <- file.path(exdir, "Cartografia_censo2024_Pais.gpkg")
  
  if (!file.exists(gpkg_found)) {
    candidates <- list.files(
      exdir,
      pattern = "Cartografia_censo2024_Pais\\.gpkg$",
      full.names = TRUE,
      recursive = TRUE
    )
    
    if (length(candidates) > 0L) {
      gpkg_found <- candidates[1]
    } else {
      stop("Could not find 'Cartografia_censo2024_Pais.gpkg' in ZIP.")
    }
  }
  
  if (!quiet) {
    message("[santiago_area] Found GeoPackage: ", basename(gpkg_found))
  }
  
  # 5. Process requested spatial definition
  # -----------------------------------------------------------------------
  sf_out <- NULL
  
  if (type == "gran_santiago") {
    
    # Gran Santiago uses the urban-limit layer.
    layer_admin <- "Limite_Urbano_CPV24"
    
    if (!quiet) {
      message("[santiago_area] Reading layer: ", layer_admin)
    }
    
    sf_admin <- sf::st_read(gpkg_found, layer = layer_admin, quiet = TRUE)
    
    # INE has multiple LOCALIDAD-like columns; the second matched the earlier code.
    loc_cols <- grep("LOCALIDAD", names(sf_admin), value = TRUE,
                     ignore.case = TRUE)
    
    if (length(loc_cols) == 0L) {
      stop("No LOCALIDAD-like column found in layer ", layer_admin)
    }
    
    col_loc <- if (length(loc_cols) >= 2L) loc_cols[2] else loc_cols[1]
    
    sf_filtered <- sf_admin[sf_admin[[col_loc]] == "GRAN SANTIAGO", ]
    
    if (nrow(sf_filtered) == 0L) {
      stop("Could not find 'GRAN SANTIAGO' in ", layer_admin)
    }
    
    if (level == "mpio") {
      sf_out <- sf_filtered
      
    } else {
      target_ids <- unique(as.character(sf_filtered$ID_ENTIDAD))
      
      if (!quiet) {
        message("[santiago_area] Reading Manzanas_CPV24 by ID_ENTIDAD.")
      }
      
      query <- sprintf(
        "SELECT * FROM Manzanas_CPV24 WHERE ID_ENTIDAD IN ('%s')",
        paste(target_ids, collapse = "','")
      )
      
      sf_out <- sf::st_read(gpkg_found, query = query, quiet = TRUE)
    }
    
  } else {
    
    # Metro Santiago uses administrative districts.
    layer_admin <- "Distrital_CPV24"
    
    if (!quiet) {
      message("[santiago_area] Reading layer: ", layer_admin)
    }
    
    sf_admin <- sf::st_read(gpkg_found, layer = layer_admin, quiet = TRUE)
    
    col_comuna <- grep("COMUNA", names(sf_admin), value = TRUE,
                       ignore.case = TRUE)[1]
    
    if (is.na(col_comuna)) {
      stop("Column 'COMUNA' missing in layer ", layer_admin)
    }
    
    target_comunas_norm <- .norm_name(keep_municipality)
    
    sf_filtered <- sf_admin[
      .norm_name(sf_admin[[col_comuna]]) %in% target_comunas_norm,
    ]
    
    if (nrow(sf_filtered) == 0L) {
      stop("No communes matched for Metro Santiago.")
    }
    
    if (!quiet) {
      message(
        "[santiago_area] Matched ",
        length(unique(sf_filtered[[col_comuna]])),
        " commune(s)."
      )
    }
    
    if (level == "mpio") {
      sf_out <- sf_filtered
      
    } else {
      target_ids <- unique(as.character(sf_filtered$ID_DISTRITO))
      
      if (!quiet) {
        message("[santiago_area] Reading Manzanas_CPV24 by ID_DISTRITO.")
      }
      
      query <- sprintf(
        "SELECT * FROM Manzanas_CPV24 WHERE ID_DISTRITO IN ('%s')",
        paste(target_ids, collapse = "','")
      )
      
      sf_out <- sf::st_read(gpkg_found, query = query, quiet = TRUE)
    }
  }
  
  # 6. Regularize geometry before saving
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[santiago_area] Regularizing polygon geometries.")
  }
  
  sf_out <- .regularize_polygon_geometry(sf_out)

  # 6b. Merge rows sharing an id (runs after repair so the union gets valid input)
  # -----------------------------------------------------------------------
  if (!is.null(dissolve_by)) {
    n_before <- nrow(sf_out)
    sf_out   <- .dissolve_by_id(sf_out, dissolve_by)

    if (!quiet) {
      message("[santiago_area] Rows: ", n_before, " -> ", nrow(sf_out),
              " (one per ", dissolve_by, ").")
    }
  }

  # 7. Save output GeoPackage
  # -----------------------------------------------------------------------
  if (file.exists(out_file) && !overwrite_gpkg) {
    if (!quiet) {
      message("[santiago_area] Output exists and overwrite_gpkg = FALSE.")
    }
  } else {
    if (!quiet) {
      message("[santiago_area] Writing GeoPackage: ", out_file)
    }
    
    if (file.exists(out_file)) {
      unlink(out_file)
    }
    
    sf::st_write(sf_out, out_file, quiet = TRUE)
  }
  
  return(invisible(sf_out))
}


# ----------------------------------------------------------------------------------------
# Function: santiago_download_pollution
#
#' @param states                 character vector; List of states (Regiones) to scrape.
#                                Defaults to santiago_cfg$which_states.
#' @param base_url               string; Base URL for SINCA historical data.
#' @param parameters             character vector; Pollutants to download.
#                                (PM10, PM2.5, NO2, CO, O3, SO2)
#' @param years_range            numeric vector; Years to include in the date range.
#' @param subdir                 string; Sub-path relative to root for saving files.
#' @param container              logical; TRUE if running inside Docker Selenium.
#' @param quiet                  logical; If TRUE, suppresses progress messages.
#
#' @return     tibble; A log of all downloaded files, including status, station name,
#              parameter, and local file path.
#              Side effect: Saves CSV files to the specified 'subdir'.
#
#' @Purpose   : Scrapes SINCA data handling the Legacy Frameset Architecture.
#              1. Maps all station URLs.
#              2. Navigates to the station page.
#              3. Switches context to the 'left' frame for changing parameters.
#              4. Switches context to the 'left' frame for downloading txt file.
#
#' @Written_by: Marcos Paulo
#' @Written_on: 10/11/2025
# ----------------------------------------------------------------------------------------
santiago_download_pollution <- function(
    states       = santiago_cfg$which_states,
    base_url     = santiago_cfg$base_url_sinca,
    parameters   = c("PM10", "PM2.5", "NO2", "CO", "O3", "SO2"),
    years_range  = santiago_cfg$years,
    subdir       = file.path("santiago", "ground_stations"),
    container    = TRUE,
    quiet        = FALSE
) {
  
  # 1) Setup & Directories -----------------------------------------------------
  root <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  
  target_dir <- root
  if (!is.null(subdir)) {
    safe_p <- try(normalizePath(subdir, winslash="/", mustWork=FALSE), 
                  silent=TRUE)
    sub_norm <- if(inherits(safe_p, "try-error")) subdir else safe_p
    
    target_dir <- if (grepl("^(/|[A-Za-z]:[/\\\\])", subdir)) {
      sub_norm
    } else {
      file.path(root, subdir)
    }
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  }
  
  param_map <- list(
    "PM10"="Material particulado MP 10", "PM2.5"="Material particulado MP 2,5", 
    "NO2"="Dióxido de nitrógeno", "CO"="Monóxido de carbono", 
    "O3"="Ozono", "SO2"="Dióxido de azufre"
  )
  
  keys <- list(end = "\uE010", backspace = "\uE003", tab = "\uE004")
  
  # 2) Start Selenium ----------------------------------------------------------
  if (!quiet) message("🚀 Starting Selenium...")
  
  if (!container) {
    cid <- system(paste("docker run -d -p 4445:4444 --shm-size=2g", 
                        "selenium/standalone-firefox:4.34.0-20250717"), intern=TRUE)
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern=TRUE), 
                silent=TRUE), add=TRUE)
    host <- "localhost"; port <- 4445L
  } else {
    host <- "selenium";  port <- 4444L
  }
  
  dl_inner <- if (container) "/home/seluser/Downloads" else target_dir
  
  caps <- list(browserName = "firefox",
               "moz:firefoxOptions" = list(prefs = list(
                 "browser.download.folderList" = 2L,
                 "browser.download.dir" = dl_inner,
                 "browser.download.useDownloadDir" = TRUE,
                 "browser.helperApps.neverAsk.saveToDisk" = 
                   "application/vnd.ms-excel,text/csv,text/html,text/plain"
               )))
  
  session <- selenium::SeleniumSession$new(
    browser="firefox", host=host, port=port, capabilities=caps, timeout=120
  )
  on.exit(try(session$close(), silent=TRUE), add = TRUE)
  
  end_date_str <- sprintf("%s0101", 
                          substr(as.character(max(years_range) + 1), 3, 4))
  
  job_queue <- list()
  
  # 3) PHASE 1: Build Job Queue ------------------------------------------------
  for (state in states) {
    if (!quiet) message(sprintf("\n📍 MAPPING STATE: %s", state))
    
    session$navigate(base_url)
    Sys.sleep(3) 
    
    # Open Menu
    menu_click_success <- FALSE
    for(k in 1:3) {
      menu_link <- try(session$find_element("xpath", 
                                            "//a[contains(text(),'Información histórica')]"), 
                       silent=TRUE)
      if (!inherits(menu_link, "try-error")) {
        menu_link$click()
        menu_click_success <- TRUE
        break
      }
      Sys.sleep(1)
    }
    
    if(!menu_click_success) {
      message("   ⚠️ Menu not found. Skipping state.")
      next
    }
    Sys.sleep(2) 
    
    # Find State Link
    el_state <- NULL
    xpath_st <- sprintf("//li/a[contains(text(), \"%s\")]", state)
    
    for(k in 1:3) {
      el_state <- try(session$find_element("xpath", xpath_st), silent=TRUE)
      if (!inherits(el_state, "try-error")) break
      
      w1 <- strsplit(state, " ")[[1]][1]
      el_state <- try(session$find_element("xpath", 
                                           sprintf("//li/a[contains(text(), '%s')]", w1)), 
                      silent=TRUE)
      if (!inherits(el_state, "try-error")) break
      Sys.sleep(1)
    }
    
    if (inherits(el_state, "try-error") || is.null(el_state)) {
      message("   ⚠️ State link not found: ", state)
      next
    }
    el_state$click()
    
    Sys.sleep(5) 
    
    # Parse Headers
    headers <- session$find_elements("css selector", "#tablaRegional thead th")
    if (length(headers) == 0) {
      message("   ⚠️ No table found for state.")
      next
    }
    
    limit_h <- if (length(headers) > 1) floor(length(headers)/2) else 1
    headers <- headers[1:limit_h]
    
    c_map <- list()
    for (i in seq_along(headers)) {
      t_attr <- headers[[i]]$get_attribute("title")
      if (is.null(t_attr)) next
      for (p in names(param_map)) {
        if (grepl(param_map[[p]], t_attr, fixed=TRUE) || 
            (p == "O3" && grepl("Ozono", t_attr))) c_map[[p]] <- i
      }
    }
    
    # Collect URLs
    rows <- session$find_elements("css selector", "#tablaRegional tbody tr")
    if (!quiet) message(sprintf("   📊 Mapping %d stations...", length(rows)))
    
    for (r_idx in seq_along(rows)) {
      r_xp <- sprintf("//*[@id='tablaRegional']/tbody/tr[%d]", r_idx)
      st_name <- tryCatch({
        session$find_element("xpath", paste0(r_xp, "/th/a"))$get_text()
      }, error=function(e) "Unknown")
      
      for (param in parameters) {
        idx <- c_map[[param]]
        if (is.null(idx)) next
        
        lnk_xp <- sprintf("%s/td[%d]//a[contains(@class,'iframe')]", 
                          r_xp, idx-1)
        lnk <- try(session$find_element("xpath", lnk_xp), silent=TRUE)
        
        if (!inherits(lnk, "try-error")) {
          href <- lnk$get_attribute("href")
          if (!is.null(href) && href != "") {
            if (startsWith(href, "//")) href <- paste0("https:", href)
            job_queue[[length(job_queue)+1]] <- list(
              state=state, station=st_name, param=param, url=href
            )
          }
        }
      }
    }
  }
  
  # 4) PHASE 2: Execute Jobs with RETRY Logic ----------------------------------
  if (!quiet) message(sprintf("\n⚡ PROCESSING %d JOBS...", length(job_queue)))
  log <- list()
  
  for (i in seq_along(job_queue)) {
    job <- job_queue[[i]]
    if (!quiet) message(sprintf("[%d/%d] %s | %s", i, length(job_queue), 
                                job$station, job$param))
    
    # Retry Configuration
    max_retries <- 5
    attempt     <- 1
    success     <- FALSE
    
    while(attempt <= max_retries && !success) {
      
      # Adaptive wait time: Attempt 1 = 1x, Attempt 2 = 2x, etc.
      wait_factor <- attempt
      
      if(attempt > 1) message(sprintf("     🔄 Retry %d/%d...", 
                                      attempt, max_retries))
      
      tryCatch({
        # 4.1 Navigation (Reset Context)
        session$navigate(job$url)
        Sys.sleep(1 * wait_factor) # Variable wait
        
        # 4.2 Switch to 'Left' Frame
        switched <- FALSE
        for(k in 1:10) {
          fr_el <- try(session$find_element("css selector", 
                                            "frame[name='left']"), silent=TRUE)
          if (!inherits(fr_el, "try-error")) {
            session$switch_to_frame(fr_el)
            switched <- TRUE
            Sys.sleep(1 * wait_factor)
            break
          }
          Sys.sleep(0.5)
        }
        if (!switched) stop("Could not find/switch to frame 'left'")
        
        # 4.3 Select 'registro horario'
        res_el <- session$find_element("css selector", "select#ic")
        opts   <- res_el$find_elements("tag name", "option")
        found_opt <- FALSE
        Sys.sleep(1 * wait_factor)
        
        for (o in opts) {
          if (grepl("registro horario", o$get_text(), ignore.case=TRUE)) {
            o$click()
            found_opt <- TRUE; break
          }
        }
        Sys.sleep(15 * wait_factor) # Increase wait here for heavy pages
        
        # 4.4 Set Date
        inp_to <- session$find_element("xpath", "//*[@id='to']")
        inp_to$click()
        inp_to$send_keys(keys$end)
        for (j in 1:8) inp_to$send_keys(keys$backspace)
        inp_to$send_keys(end_date_str)
        inp_to$send_keys(keys$tab)
        
        # 4.5 Prepare Download
        if (!quiet) message("     ⬇️ Requesting file...")
        files_before <- list.files(root, full.names = TRUE)
        
        session$switch_to_parent_frame()
        right_el <- try(session$find_element("css selector", 
                                             "frame[name='right']"), silent=TRUE)
        Sys.sleep(1 * wait_factor)
        session$switch_to_frame(right_el)
        Sys.sleep(2 * wait_factor)
        
        d_btn <- try(session$find_element("xpath", 
                                          "//a[contains(text(), 'Texto')]"), 
                     silent=TRUE)
        if (inherits(d_btn, "try-error")) stop("Download button missing")
        
        Sys.sleep(1 * wait_factor)
        d_btn$click()
        Sys.sleep(8 * wait_factor)
        
        # 4.6 File Monitor
        got_file <- FALSE
        downloaded_file <- NULL
        
        # Extended wait time for retries
        max_wait_time <- 900 + (attempt * 60) 
        
        for (w in 1:max_wait_time) { 
          files_now <- list.files(root, full.names = TRUE)
          new_files <- setdiff(files_now, files_before)
          valid <- new_files[!grepl("\\.(part|crdownload|tmp)$", new_files)]
          
          if (length(valid) > 0) {
            cand <- valid[1]
            # Stability Check
            is_stable <- FALSE
            prev_size <- -1
            for(s in 1:(15 + attempt*2)) { # Longer check on retries
              curr_size <- file.info(cand)$size
              if (curr_size > 0 && curr_size == prev_size) {
                is_stable <- TRUE; break
              }
              prev_size <- curr_size
              Sys.sleep(2)
            }
            if (is_stable) {
              downloaded_file <- cand; got_file <- TRUE; break 
            }
          }
          Sys.sleep(1)
        }
        
        if (got_file && !is.null(downloaded_file)) {
          # Success! Move and finish
          s_st <- gsub("[^A-Za-z0-9]", "_", job$station)
          s_pm <- gsub("[^A-Za-z0-9]", "", job$param)
          loc  <- substr(gsub(" ","_",job$state), 1, 25)
          fn   <- sprintf("%s_%s_%s_%s.txt", loc, s_st, s_pm, end_date_str)
          dest <- file.path(target_dir, fn)
          
          if (!quiet) message("     📦 Moving to: ", fn)
          if (file.exists(dest)) unlink(dest)
          
          if (file.exists(downloaded_file)) {
            ok <- file.copy(downloaded_file, dest, overwrite = TRUE)
            if (ok) {
              unlink(downloaded_file)
              log[[length(log)+1]] <- list(state=job$state, station=job$station,
                                           param=job$param, file=dest, status="OK")
              message("     ✅ Done.")
              success <- TRUE # Breaks the while loop
            } else {
              stop("Copy failed")
            }
          } else {
            stop("Source file vanished")
          }
        } else {
          stop("Timeout or file instability")
        }
        
      }, error = function(e) {
        message(sprintf("     ❌ Attempt %d failed: %s", attempt, e$message))
      })
      
      if(!success) {
        attempt <- attempt + 1
        Sys.sleep(5) # Cooldown before retry
      }
    } # End While
    
    if(!success) {
      message(sprintf("     ⛔ Job failed after %d attempts.", max_retries))
      log[[length(log)+1]] <- list(state=job$state, station=job$station, 
                                   param=job$param, file=NA, status="FAILED")
    }
    
    Sys.sleep(2)
  }
  
  message("✅ All Jobs Processed.")
  return(dplyr::bind_rows(log))
}


# --------------------------------------------------------------------------------------------
# Function: santiago_download_station_info
#
#' @param states                 character vector; List of states to scrape.
#                                Defaults to santiago_cfg$which_states.
#' @param base_url               string; Base URL for SINCA historical data.
#' @param subdir                 string; Sub-path relative to root for saving.
#                                Defaults to "santiago/station_metadata".
#' @param container              logical; TRUE if running inside Docker Selenium.
#' @param quiet                  logical; If TRUE, suppresses progress messages.
#
#' @return     tibble; Returns the metadata dataframe invisibly.
#              Side Effect: Saves a CSV file to the specified 'subdir'.
#
#' @Purpose   : Scrapes the "Ficha" (General Information) for air quality stations.
#              1. Navigates the SINCA table to find the "Ficha" icon link.
#              2. Visits each station's metadata page.
#              3. Extracts key-value pairs from the "Información general" table.
#              4. Cleans, structures, and SAVES the data to CSV.
#
#' @Written_by: Marcos Paulo
#' @Written_on: 13/12/2025
# --------------------------------------------------------------------------------------------
santiago_download_station_info <- function(
    states       = santiago_cfg$which_states,
    base_url     = santiago_cfg$base_url_sinca,
    subdir       = file.path("santiago", "station_metadata"),
    container    = TRUE,
    quiet        = FALSE
) {
  
  # 1) Setup & Directories -----------------------------------------------------
  root <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  
  target_dir <- root
  if (!is.null(subdir)) {
    safe_p <- try(normalizePath(subdir, winslash="/", mustWork=FALSE), 
                  silent=TRUE)
    sub_norm <- if(inherits(safe_p, "try-error")) subdir else safe_p
    
    target_dir <- if (grepl("^(/|[A-Za-z]:[/\\\\])", subdir)) {
      sub_norm
    } else {
      file.path(root, subdir)
    }
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  }
  
  if (!quiet) message("🚀 Starting Selenium for Metadata Scraping...")
  
  if (!container) {
    cid <- system(paste("docker run -d -p 4445:4444 --shm-size=2g", 
                        "selenium/standalone-firefox:4.34.0-20250717"), intern=TRUE)
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern=TRUE), 
                silent=TRUE), add=TRUE)
    host <- "localhost"; port <- 4445L
  } else {
    host <- "selenium";  port <- 4444L
  }
  
  caps <- list(browserName = "firefox")
  session <- selenium::SeleniumSession$new(
    browser="firefox", host=host, port=port, capabilities=caps, timeout=120
  )
  on.exit(try(session$close(), silent=TRUE), add = TRUE)
  
  job_queue <- list()
  
  # 2) PHASE 1: Map Station URLs -----------------------------------------------
  for (state in states) {
    if (!quiet) message(sprintf("\n📍 MAPPING STATE: %s", state))
    
    session$navigate(base_url)
    Sys.sleep(3) 
    
    # Open Menu
    menu_click_success <- FALSE
    for(k in 1:3) {
      menu_link <- try(session$find_element("xpath", 
                                            "//a[contains(text(),'Información histórica')]"), 
                       silent=TRUE)
      if (!inherits(menu_link, "try-error")) {
        menu_link$click()
        menu_click_success <- TRUE
        break
      }
      Sys.sleep(1)
    }
    
    if(!menu_click_success) {
      message("   ⚠️ Menu not found. Skipping state.")
      next
    }
    Sys.sleep(2) 
    
    # Find State Link
    el_state <- NULL
    xpath_st <- sprintf("//li/a[contains(text(), \"%s\")]", state)
    for(k in 1:3) {
      el_state <- try(session$find_element("xpath", xpath_st), silent=TRUE)
      if (!inherits(el_state, "try-error")) break
      
      w1 <- strsplit(state, " ")[[1]][1]
      el_state <- try(session$find_element("xpath", 
                                           sprintf("//li/a[contains(text(), '%s')]", w1)), 
                      silent=TRUE)
      if (!inherits(el_state, "try-error")) break
      Sys.sleep(1)
    }
    
    if (inherits(el_state, "try-error") || is.null(el_state)) {
      message("   ⚠️ State link not found: ", state)
      next
    }
    el_state$click()
    Sys.sleep(5) 
    
    headers <- session$find_elements("css selector", "#tablaRegional thead th")
    if (length(headers) == 0) {
      message("   ⚠️ No table found for state.")
      next
    }
    
    # Iterate Rows
    rows <- session$find_elements("css selector", "#tablaRegional tbody tr")
    if (!quiet) message(sprintf("   📊 Mapping %d stations...", length(rows)))
    
    for (r_idx in seq_along(rows)) {
      r_xp <- sprintf("//*[@id='tablaRegional']/tbody/tr[%d]", r_idx)
      st_name <- tryCatch({
        session$find_element("xpath", paste0(r_xp, "/th/a"))$get_text()
      }, error=function(e) "Unknown")
      
      # Find Ficha Icon (Direct XPath to icon link)
      ficha_xp <- paste0(r_xp, "//a[.//span[contains(@class, 'icon-ficha')]]")
      lnk <- try(session$find_element("xpath", ficha_xp), silent=TRUE)
      
      if (!inherits(lnk, "try-error")) {
        href <- lnk$get_attribute("href")
        if (!is.null(href) && href != "") {
          if (startsWith(href, "/")) {
            href <- paste0("https://sinca.mma.gob.cl", href)
          }
          job_queue[[length(job_queue)+1]] <- list(
            state=state, station=st_name, url=href
          )
        }
      }
    }
  }
  
  # 3) PHASE 2: Scrape Station Metadata ----------------------------------------
  if (!quiet) message(sprintf("\n⚡ SCRAPING INFO FOR %d STATIONS...", 
                              length(job_queue)))
  
  data_list <- list()
  
  for (i in seq_along(job_queue)) {
    job <- job_queue[[i]]
    if (!quiet) message(sprintf("[%d/%d] %s", i, length(job_queue), 
                                job$station))
    
    session$navigate(job$url)
    Sys.sleep(2) 
    
    tbl <- try(session$find_element("css selector", "table#tablaGeneral"), 
               silent=TRUE)
    
    if (inherits(tbl, "try-error")) {
      message("      ⚠️ Table 'tablaGeneral' not found.")
      next
    }
    
    row_data <- list(
      station_name = job$station,
      state_origin = job$state,
      source_url   = job$url
    )
    
    # Parse Rows
    table_rows <- tbl$find_elements("css selector", "tbody tr")
    
    for (tr in table_rows) {
      key_el <- try(tr$find_element("css selector", "th"), silent=TRUE)
      val_el <- try(tr$find_element("css selector", "td"), silent=TRUE)
      
      if (!inherits(key_el, "try-error") && !inherits(val_el, "try-error")) {
        key_txt <- key_el$get_text()
        val_txt <- val_el$get_text()
        
        # Clean Key/Value
        clean_key <- trimws(gsub(":", "", key_txt))
        # Standardize key names (e.g. "Coordenadas UTM" -> "coordenadas_utm")
        clean_key <- tolower(gsub(" ", "_", clean_key))
        clean_key <- gsub("[^a-z0-9_]", "", clean_key)
        
        clean_val <- trimws(val_txt)
        
        if (nchar(clean_key) > 0) {
          row_data[[clean_key]] <- clean_val
        }
      }
    }
    data_list[[length(data_list)+1]] <- row_data
    Sys.sleep(0.5) 
  }
  
  # 4) Save Output -------------------------------------------------------------
  if (length(data_list) > 0) {
    message("\n💾 Saving Data...")
    
    final_df <- dplyr::bind_rows(data_list)
    
    # Construct filename with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    filename  <- sprintf("SINCA_metadata_stations_%s.csv", timestamp)
    full_path <- file.path(target_dir, filename)
    
    # Save (using base CSV to avoid extra dependencies, change to readr if preferred)
    write.csv(final_df, full_path, row.names = FALSE, fileEncoding = "UTF-8")
    
    if (!quiet) message(sprintf("✅ Saved %d stations to: %s", 
                                nrow(final_df), full_path))
    
    return(invisible(final_df))
  } else {
    warning("No data collected.")
    return(dplyr::tibble())
  }
}


# --------------------------------------------------------------------------------------------
# Function: santiago_download_census_data
#
#' @param type                   string; The dataset to download.
#                                Options: "people", "homes", "households",
#                                "geo_location".
#' @param year                   integer; Census year (2017 or 2024).
#                                Defaults to 2017.
#' @param url                    string; OPTIONAL. Direct URL to download.
#                                If provided, it overrides 'type'/'year'.
#                                Useful if INE changes their links.
#' @param download_folder        string; Root path to save the ZIP file.
#                                Defaults to "data/downloads/santiago/census".
#' @param overwrite              logical; Re-download if file exists?
#' @param quiet                  logical; Suppress progress bars?
#
#' @return     tibble; Log containing type, file_path, bytes, and status.
#
#' @Purpose   : Downloads Chilean Census microdata.
#              1. Checks for a direct URL override.
#              2. Looks up stable defaults for 2017 (INE Archive) and
#                 2024 (Google Storage Buckets).
#              3. Downloads the file using httr with User-Agent headers.
#              4. Validates the file size to avoid broken HTML downloads.
# IMPORTANT: For 2017 Tabular data (people/homes/households),
#              this function now blocks the download and redirects the user
#              to use the 'censo2017' R package due to unstable URLs.
#' @Written_by: Marcos Paulo
#' @Written_on: 16/01/2026
# --------------------------------------------------------------------------------------------
santiago_download_census_data <- function(
    type            = "people",
    year            = 2017,
    url             = NULL,
    download_folder = file.path("data", "downloads", "santiago", "census"),
    overwrite       = FALSE,
    quiet           = FALSE
) {
  
  # 1) Deprecation Check for 2017 Tabular Data --------------------------------
  # The INE URLs for 2017 microdata are unstable/broken. We now use the 
  # 'censo2017' package for this.
  if (is.null(url) && year == 2017 && 
      type %in% c("people", "homes", "households")) {
    
    stop(paste0(
      "\n⛔️ DEPRECATED: Direct download for 2017 '", type, "' is unavailable.\n",
      "   The official URLs are broken/changed.\n\n",
      "   👉 ACTION: Please use the 'censo2017' package logic instead.\n",
      "      Run: censo2017::censo_descargar_base() to build the local DB.\n"
    ))
  }
  
  # 2) Define Defaults Dictionary ---------------------------------------------
  defaults_map <- list(
    "2017" = list(
      # People/Homes/Households removed (handled by censo2017 package)
      "geo_location" = list(
        url  = paste0(
          "https://www.ine.gob.cl/docs/default-source/geodatos-abiertos/",
          "cartografia/censo-2017/siedu/shp/microdatos_manzana.zip"
        ),
        file = "chile_census_2017_geo_location.zip"
      )
    ),
    "2024" = list(
      "people" = list(
        url  = paste0(
          "https://storage.googleapis.com/bktdescargascenso2024/",
          "personas_censo2024.zip"
        ),
        file = "chile_census_2024_people.zip"
      ),
      "homes" = list(
        url  = paste0(
          "https://storage.googleapis.com/bktdescargascenso2024/",
          "viviendas_censo2024.zip"
        ),
        file = "chile_census_2024_homes.zip"
      ),
      "households" = list(
        url  = paste0(
          "https://storage.googleapis.com/bktdescargascenso2024/",
          "hogares_censo2024.zip"
        ),
        file = "chile_census_2024_households.zip"
      ),
      "geo_location" = list(
        url  = paste0(
          "https://storage.googleapis.com/bktdescargascenso2024/",
          "Datos_agregados/Base_manzana_entidad_CPV24.zip"
        ),
        file = "chile_census_2024_geo_location.zip"
      )
    )
  )
  
  # 3) Determine Target URL and Filename --------------------------------------
  target_url  <- url
  target_file <- NULL
  year_char   <- as.character(year)
  
  if (!is.null(url)) {
    # Try to guess a clean filename from the user's custom URL
    clean_name  <- basename(sub("\\?.*$", "", url))
    target_file <- paste0("custom_", year, "_", clean_name)
    
  } else {
    if (year_char %in% names(defaults_map)) {
      if (type %in% names(defaults_map[[year_char]])) {
        def <- defaults_map[[year_char]][[type]]
        target_url  <- def$url
        target_file <- def$file
      }
    }
  }
  
  # 4) Sanity Checks ----------------------------------------------------------
  if (is.null(target_url)) {
    stop(
      "\n❌ No URL found for Year: ", year, ", Type: '", type, "'.\n",
      "   Please provide a custom 'url' argument or check the type."
    )
  }
  
  # Ensure target directory exists
  final_folder <- file.path(download_folder, year_char)
  if (!dir.exists(final_folder)) {
    dir.create(final_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  dest_path <- file.path(final_folder, target_file)
  
  # 5) Check Existing Files ---------------------------------------------------
  if (file.exists(dest_path) && !overwrite) {
    if (!quiet) {
      message("↪︎  File exists: ", basename(dest_path), " (Skipping)")
    }
    return(dplyr::tibble(
      year   = year,
      type   = type,
      file   = normalizePath(dest_path),
      bytes  = file.size(dest_path),
      status = "cached"
    ))
  }
  
  # 6) Perform Download -------------------------------------------------------
  if (!quiet) message("⬇️  Downloading to: ", dest_path)
  if (!quiet) message("🔗 Source: ", substr(target_url, 1, 60), "...")
  
  ua <- httr::user_agent(paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/90.0.4430.93 Safari/537.36"
  ))
  
  tryCatch({
    # Use HEAD first
    check_head <- httr::HEAD(target_url, ua)
    if (httr::status_code(check_head) >= 400) {
      stop("URL not accessible. Status: ", httr::status_code(check_head))
    }
    
    # Perform GET request
    res <- httr::GET(
      target_url, 
      ua, 
      httr::write_disk(dest_path, overwrite = TRUE),
      if (!quiet) httr::progress()
    )
    
    # 7) Validation -----------------------------------------------------------
    ct <- httr::headers(res)$`content-type`
    is_html <- !is.null(ct) && grepl("text/html", ct)
    is_small <- file.size(dest_path) < 15000 
    
    if (is_html || is_small) {
      if (file.exists(dest_path)) unlink(dest_path)
      stop("❌ Download failed. The server returned an HTML error page.")
    }
    
    if (!quiet) {
      sz <- format(structure(file.size(dest_path), class = "object_size"), 
                   units = "auto")
      message("✅ Success! Size: ", sz)
    }
    
    return(dplyr::tibble(
      year   = year,
      type   = type,
      file   = normalizePath(dest_path),
      bytes  = file.size(dest_path),
      status = "ok"
    ))
    
  }, error = function(e) {
    message("❌ Download Error: ", e$message)
    if (file.exists(dest_path)) unlink(dest_path)
    return(dplyr::tibble(
      year   = year,
      type   = type,
      file   = NA_character_,
      bytes  = 0,
      status = "error"
    ))
  })
}


# ============================================================================================
#  Bogotá-specific functions - processing data ans its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: santiago_filter_stations_in_metro
#
#' @param stations_df   data.frame; raw SINCA station-location data.
#' @param metro_area    sf POLYGON/MULTIPOLYGON; metropolitan area boundary.
#' @param radius_km     numeric; max distance from metro area to keep. Default 20.
#' @param out_file      string; output GeoPackage path.
#' @param overwrite_gpkg logical; overwrite output GeoPackage if exists. Default TRUE.
#' @param dissolve      logical; union metro polygons before filtering. Default TRUE.
#' @param correct_sinca logical; apply documented SINCA metadata corrections.
#' @param quiet         logical; suppress messages. Default FALSE.
#
#' @return  sf POINT data.frame of unique stations inside/near metro_area.
#' @details
#   Parses SINCA text UTM coordinates, converts stations to EPSG:32719, applies
#   documented metadata corrections, validates plausible UTM coordinates, and
#   spatially filters stations within radius_km of the metropolitan boundary.
#   Manual corrections are keyed by SINCA station id and based on map locations
#   checked from the station pages.
#
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
santiago_filter_stations_in_metro <- function(
    stations_df,
    metro_area,
    radius_km      = 20,
    out_file       = here::here(
      "data", "raw", "geospatial_data", "santiago", "stations.gpkg"
    ),
    overwrite_gpkg = TRUE,
    dissolve       = TRUE,
    correct_sinca  = TRUE,
    quiet          = FALSE
) {
  
  # 0. Dependencies and input checks
  # -----------------------------------------------------------------------
  pkgs <- c("sf", "dplyr", "tidyr", "stringr", "tibble")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required. Add to renv.")
    }
  }
  
  # Validate metropolitan area input.
  if (!inherits(metro_area, "sf")) {
    stop("'metro_area' must be an sf object.")
  }
  
  # Validate expected coordinate column.
  if (!"coordenadas_utm" %in% names(stations_df)) {
    stop("Column 'coordenadas_utm' not found in stations_df.")
  }
  
  # Ensure output directory exists.
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet) {
    message("[santiago_stations] Starting station integration.")
  }
  
  # 1. Inner helpers
  # -----------------------------------------------------------------------
  # Extract the SINCA numeric station id from the source URL when available.
  .extract_sinca_id <- function(x) {
    id_chr <- stringr::str_extract(as.character(x), "(?<=/id/)\\d+")
    as.integer(id_chr)
  }
  
  # Convert WGS84 lon-lat coordinates to UTM 19S coordinates.
  .wgs_to_utm19s <- function(lon, lat) {
    pt_wgs <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    pt_utm <- sf::st_transform(pt_wgs, crs = 32719)
    
    as.numeric(sf::st_coordinates(pt_utm)[1, c("X", "Y")])
  }
  
  # Check whether UTM 19S coordinates are plausible for central Chile.
  .valid_utm19s <- function(x, y) {
    !is.na(x) & !is.na(y) &
      x >= 160000 & x <= 834000 &
      y >= 6000000 & y <= 6600000
  }
  
  # 2. Parse SINCA coordinate strings
  # -----------------------------------------------------------------------
  # Input format example: "346716 E 6233063 N".
  df_clean <- stations_df |>
    tidyr::drop_na(coordenadas_utm) |>
    dplyr::filter(coordenadas_utm != "") |>
    dplyr::mutate(
      sinca_id = if ("source_url" %in% names(stations_df)) {
        .extract_sinca_id(source_url)
      } else {
        NA_integer_
      },
      utm_x_str = stringr::str_extract(coordenadas_utm, "^\\d+"),
      utm_y_str = stringr::str_extract(coordenadas_utm, "\\d+(?=\\s*N$)")
    ) |>
    dplyr::mutate(
      utm_x = as.numeric(dplyr::if_else(
        is.na(utm_x_str),
        stringr::str_split_fixed(coordenadas_utm, "\\D+", 4)[, 1],
        utm_x_str
      )),
      utm_y = as.numeric(dplyr::if_else(
        is.na(utm_y_str),
        stringr::str_split_fixed(coordenadas_utm, "\\D+", 4)[, 2],
        utm_y_str
      ))
    ) |>
    dplyr::select(-utm_x_str, -utm_y_str)
  
  if (!quiet) {
    message(
      "[santiago_stations] Parsed coordinates for ",
      nrow(df_clean), " station(s)."
    )
  }
  
  # 3. Apply documented SINCA metadata corrections
  # -----------------------------------------------------------------------
  if (isTRUE(correct_sinca)) {
    
    # Coordinates are lon-lat from station map locations.
    sinca_corrections <- tibble::tibble(
      sinca_id = c(131L, 142L, 183L, 202L),
      station_name_correction = c(
        "Chagres Meteorologia",
        "La Palma",
        "Quintero",
        "Concon MMA"
      ),
      lon = c(
        -70.960136,
        -71.208665,
        -71.535269,
        -71.512130
      ),
      lat = c(
        -32.805891,
        -32.891753,
        -32.772478,
        -32.926136
      ),
      correction_note = c(
        "SINCA UTM field appears truncated.",
        "SINCA UTM easting appears malformed.",
        "SINCA UTM northing appears malformed.",
        "SINCA UTM field appears to encode lon-lat digits."
      )
    )
    
    # Convert each documented lon-lat correction to UTM 19S.
    correction_xy <- lapply(
      seq_len(nrow(sinca_corrections)),
      function(i) {
        .wgs_to_utm19s(
          lon = sinca_corrections$lon[i],
          lat = sinca_corrections$lat[i]
        )
      }
    )
    
    sinca_corrections$utm_x <- vapply(correction_xy, `[`, numeric(1), 1)
    sinca_corrections$utm_y <- vapply(correction_xy, `[`, numeric(1), 2)
    
    corrected_n <- 0L
    
    # Apply corrections only when the corresponding SINCA id is present.
    for (i in seq_len(nrow(sinca_corrections))) {
      id_i <- sinca_corrections$sinca_id[i]
      row_i <- !is.na(df_clean$sinca_id) & df_clean$sinca_id == id_i
      
      if (any(row_i)) {
        df_clean[row_i, "utm_x"] <- sinca_corrections$utm_x[i]
        df_clean[row_i, "utm_y"] <- sinca_corrections$utm_y[i]
        corrected_n <- corrected_n + sum(row_i)
      }
    }
    
    if (corrected_n > 0L && !quiet) {
      message(
        "[santiago_stations] Applied ", corrected_n,
        " documented SINCA coordinate correction(s)."
      )
    }
  }
  
  # 4. Validate UTM coordinates before sf conversion
  # -----------------------------------------------------------------------
  valid_utm <- .valid_utm19s(df_clean$utm_x, df_clean$utm_y)
  
  if (any(!valid_utm)) {
    bad_rows <- df_clean[!valid_utm, , drop = FALSE]
    
    if (!quiet) {
      warning(
        "[santiago_stations] Dropping ", nrow(bad_rows),
        " station(s) with implausible UTM coordinates after corrections."
      )
    }
  }
  
  df_clean <- df_clean[valid_utm, , drop = FALSE]
  
  if (nrow(df_clean) == 0L) {
    stop("No valid station coordinates remain after UTM validation.")
  }
  
  # Keep source_url as the last non-geometry column when present.
  if ("source_url" %in% names(df_clean)) {
    df_clean <- df_clean |>
      dplyr::relocate(source_url, .after = dplyr::last_col())
  }
  
  # 5. Convert stations to sf
  # -----------------------------------------------------------------------
  stations_sf <- sf::st_as_sf(
    df_clean,
    coords = c("utm_x", "utm_y"),
    crs = 32719,
    remove = FALSE
  )
  
  # 6. Prepare metropolitan area and local metric projection
  # -----------------------------------------------------------------------
  if (!quiet) {
    message(
      "[santiago_stations] Applying spatial filter with radius ",
      radius_km, " km."
    )
  }
  
  # Build the local metric grid from the bounding box, which reads no vertices.
  aeqd_proj <- aeqd_for(metro_area)

  # Repair and dissolve on that grid, not in lon/lat, that would run through s2. Its 
  # rebuild snaps vertices to a ~1.1cm cell (2017 zonas has 4,154 edges shorter than that)
  metro_m <- metro_area |>
    sf::st_transform(crs = aeqd_proj) |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_make_valid()

  # Optionally dissolve all metro polygons into one boundary.
  if (isTRUE(dissolve)) {
    metro_m <- sf::st_union(metro_m)
  }

  # Transform station points to the same local metric CRS.
  stations_m <- sf::st_transform(stations_sf, crs = aeqd_proj)
  
  # Guard against invalid transformed points.
  empty_geom <- sf::st_is_empty(stations_m)
  
  if (any(empty_geom)) {
    if (!quiet) {
      warning(
        "[santiago_stations] Dropping ", sum(empty_geom),
        " station(s) with empty geometry after transformation."
      )
    }
    
    stations_sf <- stations_sf[!empty_geom, ]
    stations_m <- stations_m[!empty_geom, ]
  }
  
  # 7. Spatial filter using distance to metro boundary
  # -----------------------------------------------------------------------
  radius_m <- radius_km * 1000
  
  within_idx <- sf::st_is_within_distance(
    stations_m,
    metro_m,
    dist = radius_m
  )
  
  keep_mask <- lengths(within_idx) > 0
  stations_final <- stations_sf[keep_mask, ]
  
  if (!quiet) {
    message(
      "[santiago_stations] Filter stats: input = ", nrow(stations_sf),
      "; output = ", nrow(stations_final),
      "; dropped = ", nrow(stations_sf) - nrow(stations_final), "."
    )
  }
  
  # 8. Save output
  # -----------------------------------------------------------------------
  if (file.exists(out_file) && !isTRUE(overwrite_gpkg)) {
    if (!quiet) {
      message("[santiago_stations] Output exists and overwrite = FALSE.")
    }
  } else {
    if (file.exists(out_file)) {
      unlink(out_file)
    }
    
    sf::st_write(stations_final, out_file, quiet = TRUE, append = FALSE)
    
    if (!quiet) {
      message("[santiago_stations] Saved GeoPackage: ", out_file)
    }
  }
  
  return(stations_final)
}


# --------------------------------------------------------------------------------------------
# Function: santiago_download_metro_area_2017
#
#' @param base_url              string; INE 2017 DPA ArcGIS services root.
#' @param conurbacion           string; conurbation name delimiting the metro area.
#' @param region_prefix         string; CUT prefix of the region holding it.
#' @param out_file              string; GeoPackage to write.
#' @param overwrite_gpkg        logical; overwrite output GeoPackage. Default TRUE.
#' @param quiet                 logical; suppress messages. Default FALSE.
#
#' @return     sf object of zona censal polygons, one row per `zona_id`.
#
#' @Purpose   : Downloads the 2017 metropolitan area of Santiago at census-zone level,
#              which is the geography the 2017 census microdata identifies. Two REST
#              calls: the conurbation polygon that delimits the area, and the census
#              zones of the region, keeping the zones that fall inside it.
#
#' @details    `zona_id` is CUT(5) + distrito(2) + area(1) + zona(3), the same
#              11-character code the census reports as `geocodigo`, so this layer and
#              santiago_process_census_2017() join exactly. The area digit is 1
#              because Zona_Censal holds urban zones only; rural residents live
#              outside the conurbation and are not part of the metropolitan area.
#              The 2017 delimitation covers 813.9 km2 against 821.6 km2 for the 2024
#              one, so the two vintages agree to under one per cent.
#
#              The layer is written in EPSG:4326, the CRS the ArcGIS service returns
#              (`outSR=4326`), so data/raw/ stays faithful to the source. Every
#              polygon is GEOS-valid, but 976 of the 1,655 zones carry edges shorter
#              than 1 cm (4,154 in total, the shortest 3.2 mm). That is below s2's
#              rebuild grid, so any st_make_valid()/st_union() run on this layer in
#              lon/lat collapses them into duplicate vertices and aborts. Consumers
#              must repair it on a projected CRS: see santiago_filter_stations_in_metro().
#
#' @Written_on: July 2026
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
santiago_download_metro_area_2017 <- function(
    base_url       = santiago_cfg$base_url_dpa_17,
    conurbacion    = "GRAN SANTIAGO",
    region_prefix  = "13",
    out_file       = here::here("data", "raw", "geospatial_data", "santiago",
                                "gran_santiago_zonas_2017.gpkg"),
    overwrite_gpkg = TRUE,
    quiet          = FALSE
) {

  if (file.exists(out_file) && !isTRUE(overwrite_gpkg)) {
    if (!quiet) message("[santiago_2017_area] Output exists and overwrite = FALSE.")
    return(sf::st_read(out_file, quiet = TRUE))
  }

  # Build an ArcGIS query URL. sf reads GeoJSON straight from the endpoint.
  .query <- function(service, where, fields, geom = "true") {
    paste0(base_url, "/", service, "/FeatureServer/0/query",
           "?where=", utils::URLencode(where, reserved = TRUE),
           "&outFields=", utils::URLencode(fields, reserved = TRUE),
           "&returnGeometry=", geom, "&outSR=4326&f=geojson")
  }

  # 1. Conurbation polygon that delimits the metropolitan area.
  if (!quiet) message("[santiago_2017_area] Downloading conurbation: ", conurbacion)

  metro <- sf::st_read(
    .query("Conurbaciones_2017", sprintf("CONURB='%s'", conurbacion), "*"),
    quiet = TRUE
  )

  if (nrow(metro) == 0L) {
    stop("Conurbation '", conurbacion, "' not found in Conurbaciones_2017.")
  }

  # 2. Census zones of the region. One call returns them all, but the service caps
  # responses at 2000 records, so compare against the count the server reports.
  where_zonas <- sprintf("CUT LIKE '%s%%'", region_prefix)

  n_expected <- as.integer(jsonlite::fromJSON(paste0(
    base_url, "/Zona_Censal/FeatureServer/0/query",
    "?where=", utils::URLencode(where_zonas, reserved = TRUE),
    "&returnCountOnly=true&f=json"
  ))$count)

  if (!quiet) message("[santiago_2017_area] Downloading ", n_expected, " census zones.")

  zonas <- sf::st_read(
    .query("Zona_Censal", where_zonas, "CUT,COD_DISTRI,COD_ZONA,d_COMUNA"),
    quiet = TRUE
  )

  if (nrow(zonas) != n_expected) {
    stop("Zona_Censal returned ", nrow(zonas), " of ", n_expected,
         " features; the query was truncated.")
  }

  # 3. Keep the zones inside the conurbation. GEOS predicates are planar.
  zonas_utm <- sf::st_transform(zonas, 32719)
  metro_utm <- sf::st_union(sf::st_transform(metro, 32719))

  # A representative point is always inside its own polygon, so slivers on the
  # boundary do not decide membership. Subset the lon/lat layer by the same index.
  inside <- sf::st_within(sf::st_point_on_surface(sf::st_geometry(zonas_utm)),
                          metro_utm, sparse = FALSE)[, 1]
  zonas  <- zonas[inside, ]

  # 4. Build the census join key: CUT(5) + distrito(2) + area(1) + zona(3).
  zonas$zona_id <- sprintf("%05d%02d1%03d", as.integer(zonas$CUT),
                           as.integer(zonas$COD_DISTRI),
                           as.integer(zonas$COD_ZONA))

  zonas <- zonas[, c("zona_id", "CUT", "d_COMUNA")]

  if (anyDuplicated(zonas$zona_id) > 0L) {
    stop("Duplicated zona_id in the downloaded layer.")
  }

  if (!quiet) {
    message("[santiago_2017_area] ", nrow(zonas), " zone(s) in ",
            data.table::uniqueN(zonas$CUT), " commune(s).")
  }

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(out_file)) {
    unlink(out_file)
  }

  sf::st_write(zonas, out_file, quiet = TRUE, append = FALSE)

  if (!quiet) message("[santiago_2017_area] Saved GeoPackage: ", out_file)

  return(zonas)
}


# --------------------------------------------------------------------------------------------
# Function: santiago_process_stations_data_to_parquet
#
#' @param data_folder          string; folder with the station .txt files.
#' @param stations_sf          sf object; spatial registry of stations to keep.
#' @param out_dir              string; base output directory.
#' @param out_name             string; dataset name (default "santiago_metro_air").
#' @param years                int vector; years to filter.
#' @param tz                   string; Olson tz. Default "UTC". Datetimes are stored
#                              as the source wall clock with no tz shift (see
#                              DATETIME CONVENTION).
#' @param verbose              logical; print progress messages?
#
#' @return     Arrow Dataset connection. One row per (station, datetime) per year,
#              wide (one column per pollutant); datetime is a naive hourly TIMESTAMP.
#
#' @Purpose   : Ingests raw .txt files from SINCA/Chile.
#              1. Parses filenames to identify station and pollutant.
#              2. Matches filenames to the spatial registry (fuzzy normalization).
#              3. Reads the custom text format (skipping header/footer).
#              4. Coalesces validated/preliminary/unvalidated value columns.
#              5. Pivots and saves to Partitioned Parquet via DuckDB.
#
# DATETIME CONVENTION (gold standard, shared with Bogota/CDMX/SP):
#   The SINCA timestamp is YYMMDD HHMM. We parse it with ymd_hm (which resolves
#   the 2-digit-year century correctly: 97->1997, 00->2000; data span 1997-2026),
#   serialize the result to a naive ISO string, stage it as VARCHAR, and STRPTIME
#   it back to a plain TIMESTAMP.
#' @Written_on: 18/02/2026
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
santiago_process_stations_data_to_parquet <- function(
    data_folder,
    stations_sf,
    out_dir,
    out_name    = "santiago_metro_air",
    years       = 2000:2024,
    tz          = "UTC",
    verbose     = TRUE
) {
  
  # --- HELPER: Normalize strings for matching (accents, punct, case) ---
  # Santiago's own key: SINCA prefixes filenames with the region, so that has to come off
  # before the station name is isolated. base_utils::normalize_key() does everything else.
  santiago_normalize_key <- function(x) {
    x <- toupper(x)
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    # Drop the region prefix so the station name can be isolated.
    x <- gsub("METROPOLITANA DE SANTIAGO", "", x)
    x <- gsub("METROPOLITANA_DE_SANTIAGO", "", x)
    x <- gsub("[^A-Z0-9]", "", x)
    return(x)
  }
  
  # --- HELPER: Map filename to standard pollutant code ---
  get_pollutant_code <- function(filename) {
    fn <- toupper(filename)
    if (grepl("MP10|PM10", fn)) return("pm10")
    if (grepl("MP2.5|PM2.5|MP25|PM25", fn)) return("pm25")
    if (grepl("NO2", fn)) return("no2")
    if (grepl("_NO_", fn)) return("no")   # keep NO distinct from NO2
    if (grepl("NOX", fn)) return("nox")
    if (grepl("O3|OZONO", fn)) return("ozone")
    if (grepl("_CO_", fn)) return("co")
    if (grepl("SO2", fn)) return("so2")
    if (grepl("TEMP", fn)) return("temperature")
    if (grepl("HR|HUMEDAD", fn)) return("rh")
    if (grepl("VV|VELOCIDAD", fn)) return("wind_speed")
    if (grepl("DV|DIRECCION", fn)) return("wind_dir")
    return(NA_character_)
  }
  
  # --- HELPER: Serialize POSIXct to naive ISO text (gold standard) ---
  
  # 1) Dependencies
  req_pkgs <- c("duckdb", "DBI", "arrow", "dplyr", "readr", "stringi", "lubridate")
  for(p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop(paste("Package", p, "required."))
  }
  
  # 2) Validate station index
  if (!inherits(stations_sf, "sf") || !"station_name" %in% names(stations_sf)) {
    stop("'stations_sf' must be an sf object with a 'station_name' column.")
  }
  
  # Lookup map: normalized key -> real station name.
  valid_stations <- unique(stations_sf$station_name)
  station_lookup <- setNames(valid_stations, santiago_normalize_key(valid_stations))
  
  # 3) Setup DuckDB
  if (verbose) message("Starting Unified Engine (DuckDB)...")
  
  dbdir <- tempfile("santiago_air_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(dbdir, force = TRUE)
  }, add = TRUE)
  
  DBI::dbExecute(con, "PRAGMA memory_limit='8GB';")
  
  # Staging table: datetime is VARCHAR here (see DATETIME CONVENTION);
  # converted to a naive TIMESTAMP at the pivot step.
  DBI::dbExecute(con, "CREATE TABLE staging_sinca (
       datetime VARCHAR,
       station VARCHAR,
       year INTEGER,
       param VARCHAR,
       value DOUBLE
    );")
  
  # 4) Process files
  txt_files <- list.files(data_folder, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(txt_files) == 0) stop("No .txt files found in data_folder.")
  if (verbose) message(sprintf("Found %d raw files. Beginning processing...",
                               length(txt_files)))
  
  count_ingest <- 0
  
  for (f in txt_files) {
    fname <- basename(f)
    
    # A. Station and pollutant from filename.
    f_key <- santiago_normalize_key(fname)
    param_code <- get_pollutant_code(fname)
    if (is.na(param_code)) next
    
    # Keep the file only if a registry station key is a substring of it.
    match_idx <- which(sapply(names(station_lookup), function(k) grepl(k, f_key)))
    if (length(match_idx) == 0) next
    
    # Longest match wins (avoids subset-name collisions).
    cand <- names(station_lookup)[match_idx]
    best_match <- cand[which.max(nchar(cand))]
    real_station_name <- station_lookup[[best_match]]
    
    # B. Read the file. Latin-1: headers contain accented text (ano, Poblacion).
    # Station/pollutant come from the filename
    lines <- readLines(f, warn = FALSE, encoding = "latin1")
    
    # Data block runs from #DATA/EOH (+1) to EOF (-1).
    start_idx <- grep("^#DATA", lines)
    if (length(start_idx) == 0) start_idx <- grep("^EOH", lines)
    if (length(start_idx) == 0) start_idx <- grep("^050114", lines)
    end_idx <- grep("^EOF", lines)
    if (length(start_idx) == 0) next
    
    first_data_line <- start_idx[1] + 1
    last_data_line  <- if (length(end_idx) > 0) end_idx[1] - 1 else length(lines)
    if (first_data_line > last_data_line) next
    
    raw_data <- lines[first_data_line:last_data_line]
    
    # Rows are: X1=date(YYMMDD), X2=hour(HHMM), X3..X5 = value variants.
dt <- tryCatch(
  readr::read_csv(I(raw_data), col_names = FALSE,
                  col_types = readr::cols(.default = readr::col_character()),
                  show_col_types = FALSE, progress = FALSE),
  error = function(e) NULL
)
    if (is.null(dt) || nrow(dt) == 0) next
    
    # C. Coalesce the value columns: validated (X3) -> prelim (X4) -> raw (X5).
    val_cols <- dt %>% dplyr::select(dplyr::starts_with("X"))
    if (ncol(val_cols) < 3) next   # need date, hour, >=1 value
    
    d_col <- val_cols[[1]]
    h_col <- val_cols[[2]]
    
    # First non-NA across the value columns, row by row.
    values_mat <- as.matrix(val_cols[, 3:ncol(val_cols)])
    final_vals <- apply(values_mat, 1, function(row) {
      x <- na.omit(row)
      if (length(x) > 0) x[1] else NA
    })
    
    clean_df <- data.frame(
      d_txt = as.character(d_col),
      h_txt = as.character(h_col),
      value = as.numeric(final_vals),
      stringsAsFactors = FALSE
    )
    clean_df <- clean_df[!is.na(clean_df$value), ]
    if (nrow(clean_df) == 0) next
    
    # Pad to fixed widths (YYMMDD, HHMM), then parse with ymd_hm. ymd_hm
    clean_df$d_txt <- stringr::str_pad(clean_df$d_txt, 6, pad = "0")
    clean_df$h_txt <- stringr::str_pad(clean_df$h_txt, 4, pad = "0")
    clean_df$datetime_str <- paste0(clean_df$d_txt, " ", clean_df$h_txt)
    
    clean_df$datetime <- lubridate::ymd_hm(clean_df$datetime_str,
                                           tz = tz, quiet = TRUE)
    
    # Drop parse failures, then keep requested years.
    clean_df <- clean_df %>% dplyr::filter(!is.na(datetime))
    clean_df$year <- lubridate::year(clean_df$datetime)
    clean_df <- clean_df %>% dplyr::filter(year %in% years)
    if (nrow(clean_df) == 0) next
    
    # Stage with datetime serialized to naive ISO text (gold standard).
    db_payload <- data.frame(
      datetime = to_iso(clean_df$datetime),
      station  = real_station_name,
      year     = clean_df$year,
      param    = param_code,
      value    = clean_df$value,
      stringsAsFactors = FALSE
    )
    
    duckdb::dbAppendTable(con, "staging_sinca", db_payload)
    count_ingest <- count_ingest + nrow(db_payload)
  }
  
  if (verbose) message("Total rows staged: ", format(count_ingest, big.mark=","))
  if (count_ingest == 0) warning("No data ingested. Check date formats or filenames.")
  
  # 5) Pivot and export. STRPTIME converts the staged ISO text back to a naive TIMESTAMP. 
  # Path is injected via gsub to avoid %-codes sprintf misread.
  dataset_path <- file.path(out_dir, paste0(out_name, "_dataset"))
  if (dir.exists(dataset_path)) unlink(dataset_path, recursive = TRUE)
  
  sql_pivot <- "
    COPY (
      SELECT
        STRPTIME(datetime, '%Y-%m-%d %H:%M:%S') AS datetime,
        station,
        year,
        AVG(CASE WHEN param = 'pm10'        THEN value END) AS pm10,
        AVG(CASE WHEN param = 'pm25'        THEN value END) AS pm25,
        AVG(CASE WHEN param = 'ozone'       THEN value END) AS ozone,
        AVG(CASE WHEN param = 'no'          THEN value END) AS no,
        AVG(CASE WHEN param = 'no2'         THEN value END) AS no2,
        AVG(CASE WHEN param = 'nox'         THEN value END) AS nox,
        AVG(CASE WHEN param = 'co'          THEN value END) AS co,
        AVG(CASE WHEN param = 'so2'         THEN value END) AS so2,
        AVG(CASE WHEN param = 'temperature' THEN value END) AS temperature,
        AVG(CASE WHEN param = 'rh'          THEN value END) AS rh,
        AVG(CASE WHEN param = 'wind_speed'  THEN value END) AS wind_speed,
        AVG(CASE WHEN param = 'wind_dir'    THEN value END) AS wind_dir
      FROM staging_sinca
      GROUP BY datetime, station, year
      ORDER BY station, datetime
    ) TO '{{DATASET_PATH}}' (
      FORMAT PARQUET,
      PARTITION_BY (year),
      COMPRESSION 'SNAPPY',
      OVERWRITE_OR_IGNORE TRUE
    );
  "
  
  query <- gsub("{{DATASET_PATH}}", dataset_path, sql_pivot, fixed = TRUE)
  
  if (verbose) message("Pivoting and writing Partitioned Parquet...")
  DBI::dbExecute(con, query)
  
  if (verbose) message("Done! Dataset at: ", dataset_path)
  
  return(arrow::open_dataset(dataset_path))
}


# --------------------------------------------------------------------------------------------
# Function: santiago_process_census_2017
#
#' @param sf_data           sf object; metro-area census zones, from
#                           santiago_download_metro_area_2017().
#' @param match_col         string; zone-id column in sf_data (default "zona_id").
#' @param out_dir           string; Directory for the two output Parquet files.
#' @param quiet             logical; Suppress progress messages?
#
#' @return     list(individual, collapsed); Returns tibbles of the data. Also writes
#              census_individual_2017.parquet and census_collapsed_2017.parquet.
#              Parquet keeps zona_id character; a CSV roundtrip would not.
#
#' @Purpose   : Harmonizes 2017 Census data using a spatial filter.
#              1. Takes the census zones of the metro area from the sf object.
#              2. Connects to local 'censo2017' DuckDB and resolves each person's
#                 place of RESIDENCE by joining personas -> hogares -> viviendas
#                 -> zonas, which yields the 11-character `geocodigo`.
#              3. Harmonizes 'escolaridad' and injects fe=1 for schema parity.
#              4. Collapses data to the zona censal level.
#
#' @details    `geocodigo` is CUT(5) + distrito(2) + area(1) + zona(3), e.g.
#              "13101211002", and equals `zona_id` in the metro-area layer, so the
#              sample is exactly the population of the mapped zones and every zone
#              in the output has a polygon. Filtering by commune instead would add
#              everyone living in a commune the urban area merely clips, including
#              the whole Andean territory of San Jose de Maipo.
#
#' @Written_by: Marcos Paulo
#' @Updated_on: July 2026
# --------------------------------------------------------------------------------------------
santiago_process_census_2017 <- function(
    sf_data,
    match_col = "zona_id",
    out_dir   = here::here("data", "processed", "santiago", "census"),
    quiet     = FALSE
) {

  if (!requireNamespace("censo2017", quietly = TRUE)) {
    stop("Package 'censo2017' required.")
  }

  # Validate spatial inputs
  if (!inherits(sf_data, "sf")) stop("'sf_data' must be an sf spatial object.")
  if (!match_col %in% names(sf_data)) stop("Column missing in sf_data.")

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # The metro area is delimited below at commune level.
  keep_zonas <- unique(as.character(sf_data[[match_col]]))

  if (!quiet) {
    message(sprintf("[santiago_2017] Filtering for %d census zones.", length(keep_zonas)))
    message("[santiago_2017] Connecting to local Census 2017 database...")
  }
  
  con <- censo2017::censo_conectar()
  
  # Validate that the database is actually populated
  if (!"personas" %in% DBI::dbListTables(con)) {
    censo2017::censo_desconectar()
    censo2017::censo_descargar()
  }
  
  con <- censo2017::censo_conectar()

  # Residence geography is not in `personas` (p10/p11/p12comuna are migration
  # questions); it lives in `zonas`, via the household and dwelling keys.
  personas_db <- dplyr::tbl(con, "personas") %>%
    dplyr::select(
      hogar_ref_id, escolaridad, p14, p15, p17, p08, p09, p07, p16
    )

  geo_db <- dplyr::tbl(con, "hogares") %>%
    dplyr::select(hogar_ref_id, vivienda_ref_id) %>%
    dplyr::inner_join(
      dplyr::tbl(con, "viviendas") %>%
        dplyr::select(vivienda_ref_id, zonaloc_ref_id),
      by = "vivienda_ref_id"
    ) %>%
    dplyr::inner_join(
      dplyr::tbl(con, "zonas") %>% dplyr::select(zonaloc_ref_id, geocodigo),
      by = "zonaloc_ref_id"
    ) %>%
    dplyr::select(hogar_ref_id, zona_id = geocodigo)

  if (!quiet) message("[santiago_2017] Applying harmonization rules...")

  # Keep the zones of the metro area; the first five characters are the commune.
  processed_db <- personas_db %>%
    dplyr::inner_join(geo_db, by = "hogar_ref_id") %>%
    dplyr::filter(zona_id %in% keep_zonas) %>%
    dplyr::mutate(comuna = as.integer(substr(zona_id, 1L, 5L))) %>%
    dplyr::mutate(
      
      # Education harmonization: Specific rules must precede general mappings
      educ_years = dplyr::case_when(
        escolaridad == 21 & p15 == 14 & p14 == 4 ~ 23,
        escolaridad == 21 & p15 == 14 & p14 == 3 ~ 22,
        escolaridad == 21 ~ 21,
        
        escolaridad == 20 & p15 == 13 ~ 19,
        escolaridad == 20 ~ 20,
        
        escolaridad >= 18 & escolaridad <= 19 ~ escolaridad,
        escolaridad >= 13 & escolaridad <= 17 ~ escolaridad,
        escolaridad >= 0  & escolaridad <= 12 ~ escolaridad,
        TRUE ~ NA_real_
      ),
      
      # Inject unit expansion factor for cross-city schema parity
      fe = 1,
      
      # Education dummies
      no_education           = if_else(educ_years == 0, 1, 0),
      high_school_incomplete = if_else(educ_years >= 1 & educ_years <= 11, 1, 0),
      high_school_complete   = if_else(educ_years == 12, 1, 0),
      college_incomplete     = if_else(educ_years >= 13 & educ_years <= 16, 1, 0),
      college_complete       = if_else(educ_years == 17, 1, 0),
      graduate_educ          = if_else(educ_years >= 18, 1, 0),
      
      # Labor, demographics, and indigenous status
      employed      = if_else(p17 == 1 | p17 == 3, 1, 0),
      women         = if_else(p08 == 2, 1, 0),
      hh_head       = if_else(p07 == 1, 1, 0),
      hh_head_women = if_else(hh_head == 1 & women == 1, 1, 0),
      
      indigena = dplyr::case_when(
        p16 == 1 ~ 1,
        p16 == 99 ~ NA_real_,
        TRUE ~ 0
      ),
      adult = if_else(p09 >= 25, 1, 0)
    )
  
  if (!quiet) message("[santiago_2017] Collecting individual data into memory...")
  
  individual_df <- processed_db %>% 
    dplyr::collect() %>% 
    dplyr::filter(!is.na(educ_years))
  
  if (!quiet) message("[santiago_2017] Collapsing to zona censal (Adults 25+)...")

  # Aggregate metrics to the zona censal level.
  collapsed_df <- individual_df %>%
    dplyr::filter(adult == 1) %>%
    dplyr::group_by(zona_id) %>%
    # Population-weighted, matching the other three cities. `fe` is 1 here (the 2017
    # census is a full enumeration), so these give the same numbers an unweighted mean
    # would -- but they stay correct if a sampled census is ever substituted.
    dplyr::summarise(
      n                   = dplyr::n(),
      weight              = sum(fe, na.rm = TRUE),
      # Adults who reported education; every education measure below is over this
      # population, not over all adults.
      pop_educ_known      = sum(fe * !is.na(educ_years), na.rm = TRUE),
      education_mean      = sum(educ_years * fe, na.rm = TRUE) / pop_educ_known,
      count_no_ed         = sum(no_education * fe, na.rm = TRUE),
      count_hs_inc        = sum(high_school_incomplete * fe, na.rm = TRUE),
      count_hs_com        = sum(high_school_complete * fe, na.rm = TRUE),
      count_col_inc       = sum(college_incomplete * fe, na.rm = TRUE),
      count_col_com       = sum(college_complete * fe, na.rm = TRUE),
      count_grad          = sum(graduate_educ * fe, na.rm = TRUE),
      count_employed      = sum(employed * fe, na.rm = TRUE),
      .groups             = "drop"
    ) %>%
    dplyr::mutate(
      # Education shares are over the reporting population, so the six sum to 1.
      share_no_ed_pop     = count_no_ed / pop_educ_known,
      share_hs_inc_pop    = count_hs_inc / pop_educ_known,
      share_hs_com_pop    = count_hs_com / pop_educ_known,
      share_col_inc_pop   = count_col_inc / pop_educ_known,
      share_col_com_pop   = count_col_com / pop_educ_known,
      share_grad_educ_pop = count_grad / pop_educ_known,
      share_employed_pop  = count_employed / weight
    )

  # Zones with adults must not exceed the mapped ones; any shortfall is zones the
  # census has no adults for, which is worth seeing rather than assuming.
  if (!quiet) {
    message("[santiago_2017] ", nrow(individual_df), " people | ",
            nrow(collapsed_df), " of ", length(keep_zonas), " zonas censales.")
  }

  # INE names -> canonical schema; the mapping lives in santiago_cfg$schema$zona_2017.
  sch <- santiago_cfg$schema$zona_2017
  individual_df <- apply_canonical_names(individual_df, sch$census_micro,
                                         sch$geo_level, sch$raw, quiet = quiet)
  collapsed_df  <- apply_canonical_names(collapsed_df, sch$census_geo,
                                         sch$geo_level, quiet = quiet)

  if (!quiet) message("[santiago_2017] Saving outputs to: ", out_dir)

  # Write final analytical files
  # Parquet: geo_id is an 11-digit code that must stay character. A CSV saves badly.
  arrow::write_parquet(individual_df,
                       file.path(out_dir, "census_individual_2017.parquet"))
  arrow::write_parquet(collapsed_df,
                       file.path(out_dir, "census_collapsed_2017.parquet"))
  
  censo2017::censo_desconectar()
  return(list(individual = individual_df, collapsed = collapsed_df))
}


# --------------------------------------------------------------------------------------------
# Function: santiago_process_census_2024
#
#' @param census_dir string; folder containing chile_census_2024_people.zip.
#' @param sf_data   sf object; spatial data used to filter communes.
#' @param match_col string; column in sf_data with commune codes.
#' @param out_dir   string; output folder for the two processed Parquet files.
#' @param overwrite logical; re-extract ZIP if file exists. Default FALSE.
#' @param quiet     logical; suppress messages. Default FALSE.
#
#' @return  list(individual, collapsed); processed census data. Also writes
#           census_santiago_individual_2024.parquet and
#           census_santiago_collapsed_2024.parquet. Parquet keeps CUT character;
#           a CSV roundtrip drops the leading zero on region-1 communes.
#
#' @Purpose:
#   Extracts the 2024 Census CSV, filters communes, harmonizes variables,
#   adds unit weights, and collapses adults aged 25+ to commune level.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : April 2026
# --------------------------------------------------------------------------------------------
santiago_process_census_2024 <- function(
    census_dir = here::here("data", "downloads", "santiago", "census", "2024"),
    sf_data,
    match_col  = "CUT",
    out_dir    = here::here("data", "processed", "santiago", "census_2024"),
    overwrite  = FALSE,
    quiet      = FALSE
) {
  
  # Check required packages and spatial input
  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop("vroom required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr required.")
  }
  if (!inherits(sf_data, "sf")) {
    stop("'sf_data' must be an sf object.")
  }
  
  # Create output folder
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Locate census ZIP
  zip_path <- file.path(census_dir, "chile_census_2024_people.zip")
  
  if (!file.exists(zip_path)) {
    stop("ZIP not found: ", zip_path)
  }
  
  # Identify CSV inside ZIP
  files_in_zip <- utils::unzip(zip_path, list = TRUE)
  target_csv <- grep(
    "\\.csv$",
    files_in_zip$Name,
    value = TRUE,
    ignore.case = TRUE
  )
  
  if (length(target_csv) == 0) {
    stop("No CSV found inside ZIP.")
  }
  
  target_csv <- target_csv[1]
  dest_csv <- file.path(out_dir, basename(target_csv))
  
  # Extract CSV if needed
  if (!file.exists(dest_csv) || overwrite) {
    if (!quiet) {
      message("[santiago_2024] Extracting ", target_csv, ".")
    }
    
    utils::unzip(zip_path, files = target_csv, exdir = out_dir,
                 junkpaths = TRUE)
  } else {
    if (!quiet) {
      message("[santiago_2024] Using existing extracted file.")
    }
  }
  
  # Define target commune codes
  filter_codes <- as.integer(unique(sf_data[[match_col]]))
  
  if (!quiet) {
    message("[santiago_2024] Target communes: ", length(filter_codes))
  }
  
  # Read and filter raw census
  raw_data <- vroom::vroom(
    dest_csv,
    delim = ";",
    col_select = c(
      comuna,
      parentesco,
      sexo,
      edad,
      escolaridad,
      sit_fuerza_trabajo,
      p28_autoid_pueblo
    ),
    show_col_types = FALSE,
    na = c("", "NA", "-99", "-66", "99", "999")
  ) %>%
    dplyr::filter(comuna %in% filter_codes)
  
  if (nrow(raw_data) == 0) {
    stop("No data found for the requested communes.")
  }
  
  # Harmonize variables
  if (!quiet) {
    message("[santiago_2024] Harmonizing variables.")
  }
  
  df_harm <- raw_data %>%
    dplyr::mutate(
      educ_years = as.numeric(escolaridad),
      fe = 1L,
      
      no_education = as.numeric(educ_years == 0),
      high_school_incomplete = as.numeric(
        educ_years >= 1 & educ_years <= 11
      ),
      high_school_complete = as.numeric(educ_years == 12),
      college_incomplete = as.numeric(
        educ_years >= 13 & educ_years <= 16
      ),
      college_complete = as.numeric(educ_years == 17),
      graduate_educ = as.numeric(educ_years >= 18),
      
      employed = as.numeric(sit_fuerza_trabajo == 1),
      women = as.numeric(sexo == 2),
      hh_head = as.numeric(parentesco == 1),
      hh_head_women = as.numeric(hh_head == 1 & women == 1),
      
      indigena = dplyr::case_when(
        p28_autoid_pueblo == 1 ~ 1,
        p28_autoid_pueblo == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      
      edad_num = as.numeric(edad),
      adult = as.numeric(edad_num >= 25)
    )
  
  # Collapse adults to commune level
  if (!quiet) {
    message("[santiago_2024] Collapsing to commune level.")
  }
  
  df_collapse <- df_harm %>%
    dplyr::filter(adult == 1) %>%
    dplyr::group_by(comuna) %>%
    dplyr::summarise(
      weight    = sum(fe, na.rm = TRUE),
      n_records = dplyr::n(),

      # Adults who reported education; every education measure below is over this
      # population, not over all adults.
      pop_educ_known = sum(fe * !is.na(educ_years), na.rm = TRUE),

      education_mean = sum(educ_years * fe, na.rm = TRUE) / pop_educ_known,

      count_no_ed   = sum(no_education * fe, na.rm = TRUE),
      count_hs_inc  = sum(high_school_incomplete * fe, na.rm = TRUE),
      count_hs_com  = sum(high_school_complete * fe, na.rm = TRUE),
      count_col_inc = sum(college_incomplete * fe, na.rm = TRUE),
      count_col_com = sum(college_complete * fe, na.rm = TRUE),
      count_grad    = sum(graduate_educ * fe, na.rm = TRUE),
      
      count_employed = sum(employed * fe, na.rm = TRUE),
      count_women    = sum(women * fe, na.rm = TRUE),
      count_indigena = sum(indigena * fe, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Education shares are over the reporting population, so the six sum to 1.
      share_no_ed_pop   = count_no_ed / pop_educ_known,
      share_hs_inc_pop  = count_hs_inc / pop_educ_known,
      share_hs_com_pop  = count_hs_com / pop_educ_known,
      share_col_inc_pop = count_col_inc / pop_educ_known,
      share_col_com_pop = count_col_com / pop_educ_known,
      share_grad_pop    = count_grad / pop_educ_known,

      share_employed_pop = count_employed / weight,
      share_women_pop    = count_women / weight,
      share_indigena_pop = count_indigena / weight
    ) %>%
    dplyr::rename(CUT = comuna)

  # INE names -> canonical schema; the mapping lives in santiago_cfg$schema$comuna_2024.
  # `escolaridad` is dropped: it is a verified duplicate of educ_years at this vintage.
  sch <- santiago_cfg$schema$comuna_2024
  df_harm <- df_harm[, setdiff(names(df_harm), "escolaridad"), drop = FALSE]
  df_harm     <- apply_canonical_names(df_harm, sch$census_micro,
                                       sch$geo_level, sch$raw, quiet = quiet)
  df_collapse <- apply_canonical_names(df_collapse, sch$census_geo,
                                       sch$geo_level, quiet = quiet)

  # Save outputs
  if (!quiet) {
    message("[santiago_2024] Saving outputs to: ", out_dir)
  }

  arrow::write_parquet(
    df_harm,
    file.path(out_dir, "census_santiago_individual_2024.parquet")
  )

  arrow::write_parquet(
    df_collapse,
    file.path(out_dir, "census_santiago_collapsed_2024.parquet")
  )
  
  return(list(individual = df_harm, collapsed = df_collapse))
}


# --------------------------------------------------------------------------------------------
# Register this city so city_cfg() can find it. Registered under the slug the scripts use,
# not cfg$id, which is a display name for some cities. No download/process wrappers exist
# for this city yet, so only the config is exposed.
# --------------------------------------------------------------------------------------------
register_city(
  id  = "santiago",
  cfg = santiago_cfg
)
