# ============================================================================================
# IDB: Air monitoring — Santiago module
# ============================================================================================
# @Goal   : Santiago-specific parameters, download/process wrappers, and any site-specific code
# @Date   : Out 2025
# @Author : Marcos Paulo
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
  base_url_sinca   = "https://sinca.mma.gob.cl/index.php/redes",
  base_url_census  = "https://microdatos.dane.gov.co/index.php/catalog/421/get-microdata",
  years            = 2000L:2023L,
  dl_dir           = here::here("data", "downloads", "santiago"),
  out_dir          = here::here("data", "raw"),
  which_states     = c("Metro"),
  cities_in_metro  = c("Buin", "Calera de Tango", "Cerrillos", "Cerro Navia", "Colina",
                       "Conchalí", "El Bosque", "El Monte", "Estación Central", "Huechuraba", 
                       "Independencia", "Isla de Maipo", "La Cisterna", "La Florida",
                       "La Granja", "La Pintana", "La Reina", "Lampa", "Las Condes",
                       "Lo Barnechea", "Lo Espejo", "Lo Prado", "Macul", "Maipú", "María Pinto",
                       "Ñuñoa", "Padre Hurtado", "Paine", "Pedro Aguirre Cerda", "Peñalolén", 
                       "Pirque", "Providencia", "Pudahuel", "Puente Alto", "Quilicura",
                       "Quinta Normal", "Recoleta", "Renca", "San Bernardo", "San Joaquín", 
                       "San José de Maipo", "San Miguel", "San Ramón", "Santiago", "Talagante",
                       "Tiltil", "Vitacura", "Peñaflor")
)

# ============================================================================================
#  Santiago-specific functions - downloading and its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: santiago_download_metro_area
#
# @Arg       : type              — string; "metro_santiago" (Decree 337) or "gran_santiago".
# @Arg       : base_url          — string; INE Census 2024 results URL.
# @Arg       : keep_municipality — character vector; List of Comunas for Metro Area.
#                                  (Default: santiago_cfg$cities_in_metro)
# @Arg       : download_dir      — string; Local path to save the raw ZIP file.
# @Arg       : out_file          — string; Local path to save the processed GeoPackage.
# @Arg       : overwrite_zip     — logical; If TRUE, re-downloads ZIP even if it exists.
# @Arg       : overwrite_gpkg    — logical; If TRUE, overwrites the output .gpkg file.
# @Arg       : container         — logical; TRUE if running inside Docker Selenium.
# @Arg       : quiet             — logical; If TRUE, suppresses progress messages.
#
# @Output    : An sf object (invisible) containing the filtered spatial data.
#              Side effect: Writes a .gpkg file to disk.
#
# @Purpose   : Scrapes the INE Censo 2024 website to download the national cartography
#              (via Selenium) and filters it to represent Santiago.
#              - "gran_santiago": Uses 'Limite_Urbano_CPV24' layer (Urban Footprint).
#              - "metro_santiago": Uses 'Distrital_CPV24' layer (Admin Boundaries).
#
# @Written_on: 25/10/2025
# --------------------------------------------------------------------------------------------
santiago_download_metro_area <- function(
    type              = c("metro_santiago", "gran_santiago"),
    base_url          = santiago_cfg$base_url_shp,
    keep_municipality = santiago$cities_in_metro,
    download_dir      = here::here("data", "downloads", "Administrative", "Chile"),
    out_file          = here::here("data", "raw", "admin", "Chile", "santiago_metro.gpkg"),
    overwrite_zip     = FALSE,
    overwrite_gpkg    = TRUE,
    container         = TRUE,
    quiet             = FALSE
) {
  
  type <- match.arg(tolower(type), c("metro_santiago", "gran_santiago"))
  
  # 1) Define Paths
  root_dl_dir <- here::here("data", "downloads")
  
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  zip_browser_name <- "Cartografia_censo2024_Pais.zip" 
  zip_landing_path <- file.path(root_dl_dir, zip_browser_name) # Root download
  zip_target_path  <- file.path(download_dir, zip_browser_name) # Target destination
  
  # 2) Selenium Download Logic
  # ------------------------------------------------------------------------------------
  if (!file.exists(zip_target_path) || isTRUE(overwrite_zip)) {
    if (!quiet) message("⬇️  Starting Selenium to scrape INE Census Data...")
    
    # -- Docker Setup --
    if (!container) {
      if (!quiet) message("   🚀 Starting local Selenium on 4445…")
      cid <- system(paste("docker run -d -p 4445:4444 --shm-size=2g", 
                          "selenium/standalone-firefox:4.34.0-20250717"), intern = TRUE)
      on.exit(try(system(sprintf("docker rm -f %s", cid), intern=TRUE), silent=TRUE), add=TRUE)
      selenium_host <- "localhost"; selenium_port <- 4445L
    } else {
      selenium_host <- "selenium";  selenium_port <- 4444L
    }
    
    download_dir_container <- if (container) "/home/seluser/Downloads" else root_dl_dir
    
    caps <- list(
      browserName = "firefox",
      "moz:firefoxOptions" = list(
        prefs = list(
          "browser.download.folderList" = 2L,
          "browser.download.dir" = download_dir_container,
          "browser.download.useDownloadDir" = TRUE,
          "browser.helperApps.neverAsk.saveToDisk" = "application/zip,application/octet-stream"
        )
      )
    )
    
    session <- selenium::SeleniumSession$new(
      browser = "firefox", host = selenium_host, port = selenium_port, 
      capabilities = caps, timeout = 120
    )
    on.exit(try(session$close(), silent=TRUE), add = TRUE)
    
    # -- Navigation --
    if (!quiet) message("   🔎 Navigating to: ", base_url)
    session$navigate(base_url)
    Sys.sleep(8) 
    
    if (!quiet) message("   🔀 Switching context to Iframe...")
    frames <- session$find_elements("css selector", ".iframe-container iframe")
    if (length(frames) == 0) frames <- session$find_elements("css selector", "iframe")
    if (length(frames) > 0) {
      session$switch_to_frame(frames[[1]])
      Sys.sleep(2) 
    } else {
      stop("❌ Could not find the application Iframe.")
    }
    
    if (!quiet) message("   🖱️  Clicking 'Resultados'...")
    xpath_res <- "//button[contains(@class, 'tab') and contains(text(), 'Resultados')]"
    clicked_res <- FALSE
    for(k in 1:5) {
      el <- try(session$find_element("xpath", xpath_res), silent=TRUE)
      if (!inherits(el, "try-error")) { el$click(); clicked_res <- TRUE; break }
      Sys.sleep(1)
    }
    if(!clicked_res) stop("Could not find 'Resultados' button.")
    Sys.sleep(2)
    
    if (!quiet) message("   🖱️  Clicking 'Cartografía Censal'...")
    session$find_element("xpath", "//button[contains(., 'Cartografía Censal')]")$click()
    Sys.sleep(3)
    
    if (!quiet) message("   🖱️  Clicking Download...")
    xpath_dl <- paste0(
      "//li[.//strong[contains(text(), 'Cartografía País Censo 2024')]]",
      "//button[contains(@class, 'btn-descargar')]"
    )
    dl_btn <- try(session$find_element("xpath", xpath_dl), silent=TRUE)
    if (inherits(dl_btn, "try-error")) {
      xpath_dl_alt <- paste0("//button[contains(@class, 'btn-descargar')]",
                             "[.//ancestor::li[contains(., 'Cartografía País')]]")
      dl_btn <- session$find_element("xpath", xpath_dl_alt)
    }
    dl_btn$click()
    
    # -- Wait & Move & Delete --
    if (!quiet) message("   ⏳ Waiting for file download in: ", root_dl_dir)
    
    download_success <- FALSE
    for (i in 1:900) { # 15 mins max
      if (file.exists(zip_landing_path)) {
        parts <- list.files(root_dl_dir, pattern = "\\.part$", full.names = TRUE)
        if (length(parts) == 0) {
          if (file.info(zip_landing_path)$size > 100 * 1024^2) {
            
            if (!quiet) message("   📦 Moving file to: ", zip_target_path)
            
            # 1. Ensure target is clear
            if (file.exists(zip_target_path)) unlink(zip_target_path)
            
            # 2. Copy
            copy_ok <- file.copy(from = zip_landing_path, to = zip_target_path,
                                 overwrite = TRUE)
            
            if (copy_ok) {
              # 3. DELETE ORIGINAL
              unlink(zip_landing_path) 
              if (!quiet) message("   ✅ Download & Move Complete.")
              download_success <- TRUE
              break
            } else {
              stop("Failed to copy file. Check permissions.")
            }
          }
        }
      }
      Sys.sleep(1)
      if (i %% 30 == 0 && !quiet) message("      ... still downloading ...")
    }
    
    if (!download_success) stop("Timeout: File never appeared in ", root_dl_dir)
    
  } else {
    if (!quiet) message("↪︎ ZIP already present: ", zip_target_path)
  }
  
  # 3) Extraction
  # ------------------------------------------------------------------------------------
  if (!quiet) message("📦 Extracting Data...")
  exdir <- file.path(tempdir(), "santiago_carto_2024")
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir)
  
  utils::unzip(zip_target_path, exdir = exdir)
  
  gpkg_found <- file.path(exdir, "Cartografia_censo2024_Pais.gpkg")
  if (!file.exists(gpkg_found)) {
    candidates <- list.files(exdir, pattern = "Cartografia_censo2024_Pais\\.gpkg$", 
                             full.names = TRUE, recursive = TRUE)
    if (length(candidates) > 0) gpkg_found <- candidates[1]
    else stop("Could not find 'Cartografia_censo2024_Pais.gpkg' inside the ZIP.")
  }
  
  if (!quiet) message("   📂 Found GPKG: ", basename(gpkg_found))
  
  # 4) Processing
  # ------------------------------------------------------------------------------------
  if (type == "gran_santiago") {
    target_layer <- "Limite_Urbano_CPV24"
    if (!quiet) message("🗺️  Reading Layer: ", target_layer)
    sf_layer <- sf::st_read(gpkg_found, layer = target_layer, quiet = TRUE)
    
    if (!"LOCALIDAD" %in% names(sf_layer)) stop("Column 'LOCALIDAD' missing.")
    sf_out <- sf_layer |> dplyr::filter(LOCALIDAD == "GRAN SANTIAGO")
    
  } else {
    target_layer <- "Distrital_CPV24"
    if (!quiet) message("🗺️  Reading Layer: ", target_layer)
    sf_layer <- sf::st_read(gpkg_found, layer = target_layer, quiet = TRUE)
    
    if (!"COMUNA" %in% names(sf_layer)) stop("Column 'COMUNA' missing.")
    
    norm_name <- function(x) toupper(x)
    target_comunas_norm <- norm_name(keep_municipality)
    
    sf_out <- sf_layer |> 
      dplyr::filter(norm_name(COMUNA) %in% target_comunas_norm)
    
    if (nrow(sf_out) == 0) stop("No Comunas matched.")
    if (!quiet) message("🔎 Matched ", length(unique(sf_out$COMUNA)), " Comunas.")
  }
  
  # 5) Save
  # ------------------------------------------------------------------------------------
  if (file.exists(out_file) && !overwrite_gpkg) {
    if (!quiet) message("↪︎ Output exists. Skipping write.")
  } else {
    if (!quiet) message("💾 Writing GeoPackage → ", out_file)
    if (file.exists(out_file)) unlink(out_file)
    sf::st_write(sf_out, out_file, quiet = TRUE)
  }
  
  invisible(sf_out)
}

