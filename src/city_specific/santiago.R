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
  which_states     = c("Libertador General Bernardo O'Higgins", "Metropolitana de Santiago",
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


# --------------------------------------------------------------------------------------------
# Function: santiago_download_pollution_ui
#
# @Arg       : states            — character vector; List of states to scrape.
#                                  Defaults to santiago_cfg$which_states.
# @Arg       : base_url          — string; Base URL (sinca main page).
# @Arg       : parameters        — character vector; Pollutants to download.
#                                  (PM10, PM2.5, NO2, CO, O3, SO2)
# @Arg       : years_range       — numeric vector; Used to calculate the 'Until' date.
# @Arg       : subdir            — string; Sub-path for saving files.
# @Arg       : container         — logical; Use Docker Selenium?
# @Arg       : quiet             — logical; Suppress messages.
#
# @Output    : tibble; Log of downloaded files.
# @Purpose   : Scrapes hourly pollution data from SINCA using the UI Wizard.
# --------------------------------------------------------------------------------------------
santiago_download_pollution <- function(
    states       = santiago_cfg$which_states,
    base_url     = santiago_cfg$base_url_sinca,
    parameters   = c("PM10", "PM2.5", "NO2", "CO", "O3", "SO2"),
    years_range  = santiago_cfg$years,
    subdir       = "pollution/santiago_hourly",
    container    = TRUE,
    quiet        = FALSE
) {
  
  # 1) Directory Setup (User provided snippet) -------------------------------------------
  downloads_root <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  dir.create(downloads_root, recursive = TRUE, showWarnings = FALSE)
  
  is_abs <- function(p) grepl("^(/|[A-Za-z]:[/\\\\])", p)
  normalize_safe <- function(p) {
    out <- try(normalizePath(p, winslash = "/", mustWork = FALSE), silent = TRUE)
    if (inherits(out, "try-error")) p else out
  }
  
  target_dir <- downloads_root # Default
  if (!is.null(subdir)) {
    subdir_norm <- normalize_safe(subdir)
    target_dir  <- if (is_abs(subdir)) subdir_norm else file.path(downloads_root, subdir)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    message("✔️  Downloads Root: ", downloads_root)
    message("📂 Target Subdir  : ", target_dir)
  }
  
  # Map English Params to SINCA Table Title Attributes
  # Note: "Material particulado MP 2,5" uses a comma in title
  param_map <- list(
    "PM10"  = "Material particulado MP 10",
    "PM2.5" = "Material particulado MP 2,5", 
    "NO2"   = "Dióxido de nitrógeno",
    "CO"    = "Monóxido de carbono",
    "O3"    = "Ozono", # Sometimes "Ozono.-"
    "SO2"   = "Dióxido de azufre"
  )
  
  # 2) Selenium Start --------------------------------------------------------------------
  if (!quiet) message("🚀 Starting Selenium...")
  
  if (!container) {
    cid <- system(paste("docker run -d -p 4445:4444 --shm-size=2g", 
                        "selenium/standalone-firefox:4.34.0-20250717"), intern = TRUE)
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern=TRUE), silent=TRUE), add=TRUE)
    selenium_host <- "localhost"; selenium_port <- 4445L
  } else {
    selenium_host <- "selenium";  selenium_port <- 4444L
  }
  
  # Map container download path
  download_dir_container <- if (container) "/home/seluser/Downloads" else downloads_root
  
  caps <- list(
    browserName = "firefox",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList" = 2L,
        "browser.download.dir" = download_dir_container,
        "browser.download.useDownloadDir" = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" = 
          "application/vnd.ms-excel,text/csv,text/html,text/plain"
      )
    )
  )
  
  session <- selenium::SeleniumSession$new(
    browser = "firefox", host = selenium_host, port = selenium_port, 
    capabilities = caps, timeout = 120
  )
  on.exit(try(session$close(), silent=TRUE), add = TRUE)
  
  log <- list()
  
  # Calculate "Hasta" date (End of range + 1 year, Jan 1st)
  # Example: Range ends 2023 -> 240101
  end_year <- max(years_range)
  date_hasta_str <- sprintf("%s0101", substr(as.character(end_year + 1), 3, 4))
  
  # 3) Loop States
  # --------------------------------------------------------------------------------------
  
  for (state_name in states) {
    if (!quiet) message(sprintf("\n📍 STATE: %s", state_name))
    
    # Navigate Base
    session$navigate(base_url)
    Sys.sleep(3)
    
    # Click "Información histórica" (Expand menu)
    try({
      menu_link <- session$find_element(
        "xpath", "//a[contains(text(), 'Información histórica')]")
      menu_link$click()
    }, silent=TRUE)
    Sys.sleep(1)
    
    # Click Specific State
    # Note: State names in config might differ slightly from text (accents, spaces).
    # We try strict match first, then partial.
    content <- "//li/a[contains(translate(text(), 'áéíóúÁÉÍÓÚ', 'aeiouAEIOU'), '%s')]"
    xpath_state <- sprintf(content, 
                           stringi::stri_trans_general(state_name, "Latin-ASCII"))
    
    # Fallback to simple text match if fancy translate fails
    el_state <- try(session$find_element("xpath", xpath_state), silent=TRUE)
    if (inherits(el_state, "try-error")) {
      xpath_simple <- sprintf("//li/a[contains(text(), '%s')]", state_name)
      el_state <- try(session$find_element("xpath", xpath_simple), silent=TRUE)
    }
    
    if (inherits(el_state, "try-error")) {
      message("   ⚠️  Could not find state link: ", state_name)
      next
    }
    
    el_state$click()
    
    if (!quiet) message("   ⏳ Waiting for table...")
    Sys.sleep(5) # Wait for page load
    
    # 4) Parse Table Headers (Map Columns)
    # ------------------------------------------------------------------------------------
    # Identify which columns correspond to which pollutants
    headers <- session$find_elements("css selector", "#tablaRegional thead th")
    col_map <- list()
    
    for (i in seq_along(headers)) {
      title_attr <- headers[[i]]$get_attribute("title")
      if (is.null(title_attr)) next
      
      # Match against our param_map
      for (p_code in names(param_map)) {
        # Flexible match (startsWith to handle "Ozono.-")
        if (grepl(param_map[[p_code]], title_attr, fixed = TRUE) || 
            (p_code == "O3" && grepl("Ozono", title_attr))) {
          col_map[[p_code]] <- i
        }
      }
    }
    
    # Count stations (rows in body)
    rows <- session$find_elements("css selector", "#tablaRegional tbody tr")
    if (!quiet) message(sprintf("   📊 Found %d stations.", length(rows)))
    
    # 5) Loop Stations
    # ------------------------------------------------------------------------------------
    # We iterate by Index to avoid stale element reference
    for (r_idx in seq_along(rows)) {
      
      # Re-find row (DOM refreshes)
      row <- session$find_element("xpath", sprintf("//*[@id='tablaRegional']/tbody/tr[%d]",
                                                   r_idx))
      
      # Extract Name
      # Usually inside <th><a>...</a>
      st_name <- tryCatch({
        row$find_element("css selector", "th a")$get_text()
      }, error = function(e) "Unknown")
      
      if (!quiet) message(sprintf("   🔹 Station: %s", st_name))
      
      # 6) Loop Parameters
      # ----------------------------------------------------------------------------------
      for (param in parameters) {
        col_idx <- col_map[[param]]
        if (is.null(col_idx)) next # Param not in this table
        
        # Find the cell (td)
        # Use XPath relative to the specific row index
        cell_xpath <- sprintf("//*[@id='tablaRegional']/tbody/tr[%d]/td[%d]",
                              r_idx, col_idx - 1) 
        # Note: 'td' index is often shifted because the first column is 'th' (Station Name)
        # Usually: th, td, td, td... so column index N in header corresponds to td[N-1]
        
        cell <- try(session$find_element("xpath", cell_xpath), silent = TRUE)
        if (inherits(cell, "try-error")) next
        
        # Check if "iframe" icon exists (class='iframe')
        # This indicates data is available
        icon_link <- try(cell$find_element("css selector", "a.iframe"), silent = TRUE)
        
        if (!inherits(icon_link, "try-error")) {
          # Data Exists!
          if (!quiet) message("      🖱️  Opening Wizard for: ", param)
          
          # Click to open Fancybox
          icon_link$click()
          
          # Wait for Modal (User req: 20s, but we can poll)
          Sys.sleep(5) 
          
          # 7) Modal Interaction (Wizard)
          # ------------------------------------------------------------------------------
          tryCatch({
            # A. Switch to Iframe inside Fancybox
            # Usually id="fancybox-frame"
            frames <- session$find_elements("id", "fancybox-frame")
            if (length(frames) > 0) {
              session$switch_to_frame(frames[[1]])
            } else {
              stop("Could not find fancybox iframe")
            }
            
            # B. Set Time Resolution (registro horario)
            # Find options containing "horario"
            # Value usually ends in ".horario.horario.ic"
            # We look for text "registro horario"
            res_select <- session$find_element("id", "ic")
            res_opts   <- res_select$find_elements("tag name", "option")
            
            found_res <- FALSE
            for (opt in res_opts) {
              if (grepl("registro horario", opt$get_text(), ignore.case = TRUE)) {
                opt$click()
                found_res <- TRUE
                break
              }
            }
            if (!found_res) warning("      ⚠️  'Registro horario' option not found.")
            
            # C. Set End Date (Hasta)
            # ID: "to" or name="to". User said format YYMMDD.
            input_to <- session$find_element("name", "to")
            input_to$clear()
            input_to$send_keys(date_hasta_str)
            
            # D. Click "Texto" (Download)
            # <a href="javascript:Open('txt');">Texto</a>
            if (!quiet) message("      ⬇️  Clicking 'Texto'...")
            
            # Execute JS directly is safer than finding the link by text
            session$execute_script("Open('txt');")
            
            # E. Wait for Download
            got_file <- FALSE
            for (w in 1:60) { # Wait up to 60s
              files <- list.files(downloads_root, full.names = TRUE)
              # Look for recent 'apub' or .txt/.csv files 
              # (SINCA often downloads as 'apub.htmlindico2.cgi') We verify size > 0
              cands <- files[difftime(Sys.time(), file.info(files)$mtime, units="secs") < 10]
              
              if (length(cands) > 0) {
                # It might download as a weird CGI name or "descarga.txt"
                # We assume the most recent file is ours
                raw_file <- cands[1]
                
                # Check if download finished (size stable)
                if (file.info(raw_file)$size > 0) {
                  # Rename and Move
                  san_st <- gsub("[^A-Za-z0-9]", "", st_name)
                  san_p  <- gsub("[^A-Za-z0-9]", "", param)
                  new_name <- file.path(target_dir, 
                                        sprintf("SINCA_%s_%s_%s_%s.csv", 
                                                state_name, san_st, san_p, date_hasta_str))
                  
                  if (file.exists(new_name)) unlink(new_name)
                  file.rename(raw_file, new_name)
                  
                  log[[length(log)+1]] <- list(state=state_name, station=st_name, 
                                               param=param, file=new_name, status="OK")
                  got_file <- TRUE
                  break
                }
              }
              Sys.sleep(1)
            }
            if(!got_file) message("      ❌ Download timeout.")
            
            # F. Cleanup: Switch back and Close
            session$switch_to_parent_frame() # Exit iframe
            
          }, error = function(e) {
            message("      ❌ Error inside wizard: ", e$message)
            try(session$switch_to_parent_frame(), silent=TRUE)
          })
          
          # Close Fancybox (Click X)
          close_btn <- try(session$find_element("id", "fancybox-close"), silent=TRUE)
          if (!inherits(close_btn, "try-error")) {
            close_btn$click()
          } else {
            # Panic Button: Refresh page if close fails
            session$refresh()
            Sys.sleep(3) 
          }
          Sys.sleep(2) # Cooldown between params
        }
      } # End Param Loop
    } # End Station Loop
  } # End State Loop
  
  message("✅ Process Complete.")
  return(dplyr::bind_rows(log))
}


