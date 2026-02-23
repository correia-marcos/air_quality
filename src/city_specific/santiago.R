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
                       "Tiltil", "Vitacura", "Peñaflor")
)

# ============================================================================================
#  Santiago-specific functions - downloading and its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: santiago_download_metro_area
#
# @Arg       : type              — string; "metro_santiago" (Decree 337) or "gran_santiago".
# @Arg       : level             — string; "mpio" (Administrative/Urban Zones) or 
#                                  "manzana" (Census Blocks).
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
#              - "gran_santiago": Uses 'Limite_Urbano_CPV24' to define the area.
#              - "metro_santiago": Uses 'Distrital_CPV24' to define the area.
#              - If level="manzana", it joins these definitions with 'Manzanas_CPV24'.
#
# @Written_on: 25/10/2025
# --------------------------------------------------------------------------------------------
santiago_download_metro_area <- function(
    type              = c("metro_santiago", "gran_santiago"),
    level             = c("mpio", "manzana"),
    base_url          = santiago_cfg$base_url_shp,
    keep_municipality = santiago$cities_in_metro,
    download_dir      = here::here("data", "downloads", "Administrative", "Chile"),
    out_file          = here::here("data", "raw", "admin", "Chile", "santiago_metro.gpkg"),
    overwrite_zip     = FALSE,
    overwrite_gpkg    = TRUE,
    container         = TRUE,
    quiet             = FALSE
) {
  
  type  <- match.arg(tolower(type), c("metro_santiago", "gran_santiago"))
  level <- match.arg(tolower(level), c("mpio", "manzana"))
  
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
        # Check for .part files (firefox temporary download file)
        parts <- list.files(root_dl_dir, pattern = "\\.part$", full.names = TRUE)
        if (length(parts) == 0) {
          # Check size stability (simple check > 100MB)
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
  if (!quiet) message("📦 Extracting Data (this may take a moment)...")
  exdir <- file.path(tempdir(), "santiago_carto_2024")
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir)
  
  utils::unzip(zip_target_path, exdir = exdir)
  
  # Locate GPKG
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
  sf_out <- NULL
  
  if (type == "gran_santiago") {
    # --- Case A: Gran Santiago (Urban Footprint) ---
    
    layer_admin <- "Limite_Urbano_CPV24"
    if (!quiet) message("🗺️  Reading Admin Layer: ", layer_admin)
    
    # 1. Load Admin Layer to find the Entity ID for Gran Santiago
    sf_admin <- sf::st_read(gpkg_found, layer = layer_admin, quiet = TRUE)
    
    # Filter for Gran Santiago
    # Note: INE attributes can vary (LOCALIDAD vs NOM_LOCALIDAD), check generically if needed
    col_loc <- grep("LOCALIDAD", names(sf_admin), value = TRUE, ignore.case = TRUE)[2]
    
    if (is.na(col_loc)) stop("Column 'LOCALIDAD' missing in layer ", layer_admin)
    
    sf_filtered <- sf_admin[sf_admin[[col_loc]] == "GRAN SANTIAGO", ]
    
    if (nrow(sf_filtered) == 0) stop("Could not find 'GRAN SANTIAGO' in ", layer_admin)
    
    # 2. Return based on Level
    if (level == "mpio") {
      # If level is mpio/admin, we return the Urban Limit polygon itself
      sf_out <- sf_filtered
      
    } else {
      # If level is manzana, we use ID_ENTIDAD to fetch blocks
      target_ids <- unique(as.character(sf_filtered$ID_ENTIDAD))
      
      if (!quiet) message(
        "🧱  Reading Block Layer: Manzanas_CPV24 (filtering by ID_ENTIDAD)...")
      
      # Use SQL query to speed up reading if supported, otherwise read & filter
      # Manzanas layer is heavy, reading filtered is better if possible.
      # st_read supports SQL for GPKG.
      
      query <- sprintf("SELECT * FROM Manzanas_CPV24 WHERE ID_ENTIDAD IN ('%s')", 
                       paste(target_ids, collapse = "','"))
      
      sf_out <- sf::st_read(gpkg_found, query = query, quiet = TRUE)
    }
    
  } else {
    # --- Case B: Metro Santiago (Administrative Districts) ---
    
    layer_admin <- "Distrital_CPV24"
    if (!quiet) message("🗺️  Reading Admin Layer: ", layer_admin)
    
    # 1. Load Admin Layer to find District IDs belonging to the Comunas
    sf_admin <- sf::st_read(gpkg_found, layer = layer_admin, quiet = TRUE)
    
    col_comuna <- grep("COMUNA", names(sf_admin), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(col_comuna)) stop("Column 'COMUNA' missing in layer ", layer_admin)
    
    norm_name <- function(x) toupper(chartr("áéíóú", "AEIOU", x))
    target_comunas_norm <- norm_name(keep_municipality)
    
    sf_filtered <- sf_admin[norm_name(sf_admin[[col_comuna]]) %in% target_comunas_norm, ]
    
    if (nrow(sf_filtered) == 0) stop("No Comunas matched for Metro Santiago.")
    if (!quiet) message("🔎 Matched ", length(unique(sf_filtered[[col_comuna]])), " Comunas.")
    
    # 2. Return based on Level
    if (level == "mpio") {
      # Return the District polygons
      sf_out <- sf_filtered
      
    } else {
      # If level is manzana, we use ID_DISTRITO to fetch blocks
      # ID_DISTRITO connects Distrital_CPV24 with Manzanas_CPV24
      target_ids <- unique(as.character(sf_filtered$ID_DISTRITO))
      
      if (!quiet) message(
        "🧱  Reading Block Layer: Manzanas_CPV24 (filtering by ID_DISTRITO)...")
      
      query <- sprintf("SELECT * FROM Manzanas_CPV24 WHERE ID_DISTRITO IN ('%s')", 
                       paste(target_ids, collapse = "','"))
      
      sf_out <- sf::st_read(gpkg_found, query = query, quiet = TRUE)
    }
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


# ----------------------------------------------------------------------------------------
# Function: santiago_download_pollution
#
# @Arg       : states          — character vector; List of states (Regiones) to scrape.
#                                Defaults to santiago_cfg$which_states.
# @Arg       : base_url        — string; Base URL for SINCA historical data.
# @Arg       : parameters      — character vector; Pollutants to download.
#                                (PM10, PM2.5, NO2, CO, O3, SO2)
# @Arg       : years_range     — numeric vector; Years to include in the date range.
# @Arg       : subdir          — string; Sub-path relative to root for saving files.
# @Arg       : container       — logical; TRUE if running inside Docker Selenium.
# @Arg       : quiet           — logical; If TRUE, suppresses progress messages.
#
# @Output    : tibble; A log of all downloaded files, including status, station name,
#              parameter, and local file path.
#              Side effect: Saves CSV files to the specified 'subdir'.
#
# @Purpose   : Scrapes SINCA data handling the Legacy Frameset Architecture.
#              1. Maps all station URLs.
#              2. Navigates to the station page.
#              3. Switches context to the 'left' frame for changing parameters.
#              4. Switches context to the 'left' frame for downloading txt file.
#
# @Written_by: Marcos Paulo
# @Written_on: 10/11/2025
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
# @Arg       : states          — character vector; List of states to scrape.
#                                Defaults to santiago_cfg$which_states.
# @Arg       : base_url        — string; Base URL for SINCA historical data.
# @Arg       : subdir          — string; Sub-path relative to root for saving.
#                                Defaults to "santiago/station_metadata".
# @Arg       : container       — logical; TRUE if running inside Docker Selenium.
# @Arg       : quiet           — logical; If TRUE, suppresses progress messages.
#
# @Output    : tibble; Returns the metadata dataframe invisibly.
#              Side Effect: Saves a CSV file to the specified 'subdir'.
#
# @Purpose   : Scrapes the "Ficha" (General Information) for air quality stations.
#              1. Navigates the SINCA table to find the "Ficha" icon link.
#              2. Visits each station's metadata page.
#              3. Extracts key-value pairs from the "Información general" table.
#              4. Cleans, structures, and SAVES the data to CSV.
#
# @Written_by: Marcos Paulo
# @Written_on: 13/12/2025
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


# -----------------------------------------------------------------------------
# Function: santiago_download_census_data
#
# @Arg       : type            — string; The dataset to download.
#                                Options: "people", "homes", "households",
#                                "geo_location".
# @Arg       : year            — integer; Census year (2017 or 2024).
#                                Defaults to 2017.
# @Arg       : url             — string; OPTIONAL. Direct URL to download.
#                                If provided, it overrides 'type'/'year'.
#                                Useful if INE changes their links.
# @Arg       : download_folder — string; Root path to save the ZIP file.
#                                Defaults to "data/downloads/santiago/census".
# @Arg       : overwrite       — logical; Re-download if file exists?
# @Arg       : quiet           — logical; Suppress progress bars?
#
# @Output    : tibble; Log containing type, file_path, bytes, and status.
#
# @Purpose   : Downloads Chilean Census microdata.
#              1. Checks for a direct URL override.
#              2. Looks up stable defaults for 2017 (INE Archive) and
#                 2024 (Google Storage Buckets).
#              3. Downloads the file using httr with User-Agent headers.
#              4. Validates the file size to avoid broken HTML downloads.
# IMPORTANT: For 2017 Tabular data (people/homes/households),
#              this function now blocks the download and redirects the user
#              to use the 'censo2017' R package due to unstable URLs.
# @Written_by: Marcos Paulo
# @Written_on: 16/01/2026
# -----------------------------------------------------------------------------
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
# @Arg       : stations_df    — The raw dataframe (station_location)
# @Arg       : metro_area     — sf (MULTI)POLYGON of the metropolitan area
# @Arg       : radius_km      — numeric; max distance to keep (default 20)
# @Arg       : out_file       — where to write the cropped GeoPackage
# @Arg       : overwrite_gpkg — logical; overwrite output GeoPackage if exists
# @Arg       : dissolve       — logical; TRUE unions metro polygons (default TRUE)
# 
# @Output    : sf POINT data.frame of unique stations inside/near metro_area
# @Purpose   : Parses text UTM coordinates, converts to sf, and spatially filters.
# --------------------------------------------------------------------------------------------
santiago_filter_stations_in_metro <- function(
    stations_df,
    metro_area,
    radius_km      = 20,
    out_file       = here::here("data", "raw", "geospatial_data", "santiago", "stations.gpkg"),
    overwrite_gpkg = TRUE,
    dissolve       = TRUE
) {
  
  # 0) Dependency & Input Checks
  if (!inherits(metro_area, "sf")) stop("'metro_area' must be an sf object.")
  
  # Ensure output directory exists
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  message("🔄 Starting Santiago Data Integration...")
  
  # PART I: Process & Parse Coordinates
  # ----------------------------------------------------------------------------
  # Input format example: "346716 E 6233063 N"
  # Logic: Extract the first number as X (Easting), second as Y (Northing).
  # EPSG for Chile (Zone 19S): 32719
  
  df_clean <- stations_df |>
    # 1. Remove rows with empty coords
    tidyr::drop_na(coordenadas_utm) |>
    dplyr::filter(coordenadas_utm != "") |>
    
    # 2. Extract numeric parts using regex
    # We capture all digits, assuming the first group is Easting, second is Northing
    dplyr::mutate(
      utm_x_str = stringr::str_extract(coordenadas_utm, "^\\d+"),
      utm_y_str = stringr::str_extract(coordenadas_utm, "\\d+(?=\\s*N$)"), # Digits before 'N'
      
      # Fallback regex if the specific structure varies slightly (just grabs 1st and 2nd number)
      utm_x = as.numeric(ifelse(is.na(utm_x_str), 
                                stringr::str_split_fixed(coordenadas_utm, "\\D+", 4)[,1], 
                                utm_x_str)),
      utm_y = as.numeric(ifelse(is.na(utm_y_str), 
                                stringr::str_split_fixed(coordenadas_utm, "\\D+", 4)[,2], 
                                utm_y_str))
    ) |>
    
    # 3. Clean and Reorder Columns (source_url last)
    dplyr::select(-utm_x_str, -utm_y_str) |>
    dplyr::relocate(source_url, .after = dplyr::last_col())
  
  message("   ✅ Parsed coordinates for ", nrow(df_clean), " stations.")
  
  # PART II: Spatial Conversion
  # ----------------------------------------------------------------------------
  # Chile is UTM Zone 19S => EPSG:32719
  stations_sf <- sf::st_as_sf(
    df_clean,
    coords = c("utm_x", "utm_y"),
    crs = 32719 
  )
  
  # PART III: Spatial Filter (Radius Logic)
  # ----------------------------------------------------------------------------
  message("🔄 Applying Spatial Filter (Radius: ", radius_km, "km)...")
  
  # 1. Prepare Metro Polygon
  metro_valid <- metro_area %>% 
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_make_valid()
  if (dissolve) metro_valid <- sf::st_union(metro_valid)
  
  # 2. Build Safe AEQD Projection (Same robustness logic as Bogota)
  # Center on the metro area to create a perfect metric ruler
  metro_wgs <- sf::st_transform(metro_valid, 4326)
  cen       <- sf::st_coordinates(sf::st_centroid(metro_wgs))
  
  aeqd_proj <- sprintf(
    "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
    cen[2], cen[1]
  )
  
  # 3. Transform everything to Meters using that Ruler
  metro_m    <- sf::st_transform(metro_valid, aeqd_proj)
  stations_m <- sf::st_transform(stations_sf, aeqd_proj)
  
  # 4. Filter
  radius_m   <- radius_km * 1000
  within_idx <- sf::st_is_within_distance(stations_m, metro_m, dist = radius_m)
  keep_mask  <- lengths(within_idx) > 0
  
  stations_final <- stations_sf[keep_mask, ]
  
  message("     Filter Stats: Input=", nrow(stations_sf), 
          " -> Output=", nrow(stations_final), 
          " (Dropped ", nrow(stations_sf) - nrow(stations_final), ")")
  
  # PART IV: Save Output
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


# --------------------------------------------------------------------------------------------
# Function: santiago_process_stations_data_to_parquet
#
# @Arg       : data_folder     — string; folder containing the station .txt files.
# @Arg       : stations_sf     — sf object; Spatial registry of stations to keep. 
# @Arg       : out_dir         — string; base output directory.
# @Arg       : out_name        — string; name of the dataset (default: "santiago_metro_air").
# @Arg       : years           — int vector; years to filter.
# @Arg       : tz              — string; Olson timezone (default "America/Santiago").
# @Arg       : verbose         — logical; print progress messages?
#
# @Output    : Arrow Dataset connection.
#
# @Purpose   : Ingests raw .txt files from SINCA/Chile.
#              1. Parses complex filenames to identify Station and Pollutant.
#              2. Matches filenames to the provided spatial registry (fuzzy normalization).
#              3. Reads custom text format (skipping headers/footers).
#              4. Coalesces valid/preliminary/unvalidated data columns.
#              5. Pivots and saves to Partitioned Parquet via DuckDB.
#
# @Written_on: 18/02/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
santiago_process_stations_data_to_parquet <- function(
    data_folder,
    stations_sf,
    out_dir,
    out_name    = "santiago_metro_air",
    years       = 2000:2024,
    tz          = "America/Santiago",
    verbose     = TRUE
) {
  
  # --- HELPER: Normalize Strings for Matching ---
  # Removes accents, spaces, punctuation, and uppercase for robust comparison
  normalize_key <- function(x) {
    x <- toupper(x)
    # Remove accents
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    # Remove region prefix common in filenames to isolate station name
    x <- gsub("METROPOLITANA DE SANTIAGO", "", x)
    x <- gsub("METROPOLITANA_DE_SANTIAGO", "", x)
    # Remove all non-alphanumeric
    x <- gsub("[^A-Z0-9]", "", x)
    return(x)
  }
  
  # --- HELPER: Pollutant Mapper ---
  # Maps filename strings to standard database column names
  get_pollutant_code <- function(filename) {
    fn <- toupper(filename)
    if (grepl("MP10|PM10", fn)) return("pm10")
    if (grepl("MP2.5|PM2.5|MP25|PM25", fn)) return("pm25")
    if (grepl("NO2", fn)) return("no2")
    if (grepl("_NO_", fn)) return("no") # Ensure NO doesn't match NO2
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
  
  # 1) Check Dependencies
  req_pkgs <- c("duckdb", "DBI", "arrow", "dplyr", "readr", "stringi", "lubridate")
  for(p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop(paste("Package", p, "required."))
  }
  
  # 2) Validate Station Index
  if (!inherits(stations_sf, "sf") || !"station_name" %in% names(stations_sf)) {
    stop("'stations_sf' must be an sf object with a 'station_name' column.")
  }
  
  # Create Lookup Map: Normalized Key -> Real Station Name
  valid_stations <- unique(stations_sf$station_name)
  station_lookup <- setNames(valid_stations, normalize_key(valid_stations))
  
  # 3) Setup DuckDB
  if (verbose) message("⬜️ Starting Unified Engine (DuckDB)...")
  
  dbdir <- tempfile("santiago_air_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(dbdir, force = TRUE)
  }, add = TRUE)
  
  DBI::dbExecute(con, "PRAGMA memory_limit='8GB';") 
  
  # Staging Table
  DBI::dbExecute(con, "CREATE TABLE staging_sinca (
       datetime TIMESTAMP, 
       station VARCHAR, 
       year INTEGER, 
       param VARCHAR, 
       value DOUBLE
    );")
  
  # 4) Process Files
  txt_files <- list.files(data_folder, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(txt_files) == 0) stop("No .txt files found in data_folder.")
  if (verbose) message(sprintf("📂 Found %d raw files. Beginning processing...",
                               length(txt_files)))
  
  count_ingest <- 0
  
  for (f in txt_files) {
    fname <- basename(f)
    
    # A. Determine Station and Pollutant from Filename
    # ------------------------------------------------
    f_key <- normalize_key(fname)
    param_code <- get_pollutant_code(fname)
    
    if (is.na(param_code)) {
      # if (verbose) warning("Skipping unknown pollutant file: ", fname)
      next 
    }
    
    # Match Station: Check which valid station key is a substring of the file key
    # e.g., Station Key "LASCONDESACREDITADA" is inside File Key "LASCONDESACREDITADANO2..."
    match_idx <- which(sapply(names(station_lookup), function(k) grepl(k, f_key)))
    
    if (length(match_idx) == 0) {
      # Station in file is not in our spatial filter list
      next 
    }
    
    # Pick the longest match (in case of subset names)
    best_match <- names(station_lookup)[match_idx
                                        [which.max(nchar(names(station_lookup)[match_idx]))]]
    real_station_name <- station_lookup[[best_match]]
    
    # B. Read the Messy Text File
    # ------------------------------------------------
    # We read lines to find #DATA (or EOH) and EOF
    lines <- readLines(f, warn = FALSE)
    
    # Find start: Look for line starting with #DATA or just DATA, or after EOH
    start_idx <- grep("^#DATA", lines)
    if (length(start_idx) == 0) start_idx <- grep("^EOH", lines)
    if (length(start_idx) == 0) start_idx <- grep("^050114", lines)
    
    # Find end: Look for EOF
    end_idx <- grep("^EOF", lines)
    
    if (length(start_idx) == 0) next
    
    # Extract the data chunk
    # We start reading 1 line after #DATA
    first_data_line <- start_idx[1] + 1
    last_data_line  <- if (length(end_idx) > 0) end_idx[1] - 1 else length(lines)
    
    if (first_data_line > last_data_line) next
    
    raw_data <- lines[first_data_line:last_data_line]
    
    # Parse CSV structure (Date, Hour, Val1, Val2, Val3)
    # Note: Use read_csv with col_names = FALSE for speed
    # We expect 5 columns typically.
    dt <- tryCatch({
      readr::read_csv(I(raw_data), col_names = FALSE, show_col_types = FALSE, progress = FALSE)
    }, error = function(e) return(NULL))
    
    if (is.null(dt) || nrow(dt) == 0) next
    
    # C. Clean and Transform
    # ------------------------------------------------
    # Columns: X1=Date, X2=Hour, X3=Val1, X4=Val2, X5=Val3 (Logic: Coalesce 3->4->5)
    
    # Select and Coalesce Value
    # We prioritize X3 (Validated), then X4 (Prelim), then X5 (Non-validated)
    # Note: read_csv might read empty fields as NA, which is what we want.
    
    val_cols <- dt %>% dplyr::select(dplyr::starts_with("X"))
    if (ncol(val_cols) < 3) next # Need at least Date, Hour, 1 Value
    
    # Extract date/hour
    d_col <- val_cols[[1]]
    h_col <- val_cols[[2]]
    
    # Coalesce values: find first non-NA in remaining columns
    values_mat <- as.matrix(val_cols[, 3:ncol(val_cols)])
    
    # Fast coalesce row-wise
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
    
    # Parse Date: YYMMDD as per instructions (IV)
    # Hour: HHMM
    
    # Pad strings just in case
    clean_df$d_txt <- stringr::str_pad(clean_df$d_txt, 6, pad = "0")
    clean_df$h_txt <- stringr::str_pad(clean_df$h_txt, 4, pad = "0")
    
    # Parse Datetime
    # Note: If MMDDYY is strict:
    clean_df$datetime_str <- paste0(clean_df$d_txt, " ", clean_df$h_txt)
    
    # Usage of 'mdy_hm' for YYMMDD HHMM
    clean_df$datetime <- lubridate::ymd_hm(clean_df$datetime_str, tz = tz, quiet = TRUE)
    
    # Fallback check: If huge amount of NAs, maybe it was YYMMDD?
    if (sum(is.na(clean_df$datetime)) > (0.5 * nrow(clean_df))) {
      clean_df$datetime <- lubridate::ymd_hm(clean_df$datetime_str, tz = tz, quiet = TRUE)
    }
    
    clean_df <- clean_df %>% dplyr::filter(!is.na(datetime))
    clean_df$year <- lubridate::year(clean_df$datetime)
    
    # Filter Years
    clean_df <- clean_df %>% dplyr::filter(year %in% years)
    
    if (nrow(clean_df) > 0) {
      # Prepare for DB
      db_payload <- data.frame(
        datetime = clean_df$datetime,
        station  = real_station_name,
        year     = clean_df$year,
        param    = param_code,
        value    = clean_df$value
      )
      
      duckdb::dbAppendTable(con, "staging_sinca", db_payload)
      count_ingest <- count_ingest + nrow(db_payload)
    }
  }
  
  if (verbose) message("💾 Total rows staged: ", format(count_ingest, big.mark=","))
  if (count_ingest == 0) warning("No data was ingested. Check date formats or filenames.")
  
  # 5) Pivot and Export
  # ------------------------------------------------
  dataset_path <- file.path(out_dir, paste0(out_name, "_dataset"))
  if (dir.exists(dataset_path)) unlink(dataset_path, recursive = TRUE)
  
  # SQL to Pivot pollutants to columns and average hourly duplicates
  sql_pivot <- "
    COPY (
      SELECT 
        datetime,
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
# Function: santiago_process_census_2017
#
# @Arg       : sf_data    — sf object; The spatial dataframe containing the
#                           communes of interest (e.g., metro_area).
# @Arg       : match_col  — string; The name of the column in sf_data that 
#                           contains the commune codes (e.g., "CUT").
# @Arg       : out_dir    — string; Directory to save processed CSVs.
# @Arg       : quiet      — logical; Suppress progress messages?
#
# @Output    : list(individual, collapsed); Returns tibbles of the data.
#
# @Purpose   : Harmonizes 2017 Census data using a spatial filter.
#              1. Extracts commune codes from the provided sf object.
#              2. Connects to local 'censo2017' DuckDB.
#              3. Filters the Census 'personas' table using those codes.
#              4. Harmonizes 'escolaridad' (fixing the Stata logic bug).
#              5. Collapses data to the Commune level.
#
# @Written_by: Marcos Paulo
# @Updated_on: 18/02/2026 (Added dynamic SF filtering)
# --------------------------------------------------------------------------------------------
santiago_process_census_2017 <- function(
    sf_data,
    match_col = "CUT",
    out_dir   = here::here("data", "processed", "santiago", "census"),
    quiet     = FALSE
) {
  
  # 1. Checks & Setup ---------------------------------------------------------
  if (!requireNamespace("censo2017", quietly = TRUE)) {
    stop("Package 'censo2017' required. Install with install.packages('censo2017')")
  }
  if (!inherits(sf_data, "sf")) stop("'sf_data' must be an sf spatial object.")
  if (!match_col %in% names(sf_data)) stop(paste("Column", match_col, "not found in sf_data."))
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # 2. Extract Filter Codes ---------------------------------------------------
  # We convert to integer because censo2017 uses integer keys for p10comuna
  filter_codes <- as.integer(unique(sf_data[[match_col]]))
  
  if (!quiet) message(sprintf("🎯 Filtering for %d communes found in '%s'...", 
                              length(filter_codes), match_col))
  
  # 3. Connect to Data --------------------------------------------------------
  if (!quiet) message("🔌 Connecting to local Census 2017 database...")
  con <- censo2017::censo_conectar()
  
  # Select columns from 'personas' table
  # Note: p10comuna is "Comuna de Residencia Habitual"
  personas_db <- dplyr::tbl(con, "personas") %>% 
    dplyr::select(
      comuna = p10comuna, 
      escolaridad, 
      p14, # Curso aprobado
      p15, # Nivel aprobado
      p17, # Employment status
      p08, # Sexo
      p09, # Edad
      p07, # Relacion parentesco
      p16  # Indigena
    )
  
  # 4. Filter and Harmonize (Lazy Execution) ----------------------------------
  if (!quiet) message("⚙️  Applying harmonization rules...")
  
  processed_db <- personas_db %>%
    # Dynamic Filter
    dplyr::filter(comuna %in% filter_codes) %>%
    
    dplyr::mutate(
      
      # --- Education Harmonization (FIXED LOGIC) ---
      # case_when evaluates sequentially. We put specific rules BEFORE general rules.
      educ_years = dplyr::case_when(
        
        # A. PhD Specifics (Logic: 21 is base PhD. P14 adds years?)
        # Stata logic: replace escolaridad = 23 if (old==21 & p15==14 & p14==4)
        escolaridad == 21 & p15 == 14 & p14 == 4 ~ 23,
        escolaridad == 21 & p15 == 14 & p14 == 3 ~ 22,
        escolaridad == 21 ~ 21,
        
        # B. Master's (The Fix)
        # We catch Master's (p15==13) BEFORE we catch the general 20 code.
        escolaridad == 20 & p15 == 13 ~ 19,
        escolaridad == 20 ~ 20,
        
        # C. General Mappings
        escolaridad >= 18 & escolaridad <= 19 ~ escolaridad,
        escolaridad >= 13 & escolaridad <= 17 ~ escolaridad,
        escolaridad >= 0  & escolaridad <= 12 ~ escolaridad,
        
        TRUE ~ NA_real_
      ),
      
      # --- Education Dummies ---
      no_education           = if_else(educ_years == 0, 1, 0),
      high_school_incomplete = if_else(educ_years >= 1 & educ_years <= 11, 1, 0),
      high_school_complete   = if_else(educ_years == 12, 1, 0),
      college_incomplete     = if_else(educ_years >= 13 & educ_years <= 16, 1, 0),
      college_complete       = if_else(educ_years == 17, 1, 0),
      graduate_educ          = if_else(educ_years >= 18, 1, 0),
      
      # --- Labor & Demographics ---
      # p17: 1=Paid work, 3=Employed but absent (vacation/sick)
      employed = if_else(p17 == 1 | p17 == 3, 1, 0),
      
      women = if_else(p08 == 2, 1, 0),
      
      hh_head = if_else(p07 == 1, 1, 0),
      hh_head_women = if_else(hh_head == 1 & women == 1, 1, 0),
      
      indigena = dplyr::case_when(
        p16 == 1 ~ 1,
        p16 == 99 ~ NA_real_, # Missing
        TRUE ~ 0
      ),
      
      adult = if_else(p09 >= 25, 1, 0)
    )
  
  # 5. Collect Individual Data ------------------------------------------------
  if (!quiet) message("⬇️  Collecting individual data into memory...")
  
  individual_df <- processed_db %>% 
    dplyr::collect() %>% 
    dplyr::filter(!is.na(educ_years))
  
  # 6. Collapse (Aggregation) -------------------------------------------------
  if (!quiet) message("📉 Collapsing to Comuna level (Adults 25+)...")
  
  collapsed_df <- individual_df %>%
    dplyr::filter(adult == 1) %>%
    dplyr::group_by(comuna) %>%
    dplyr::summarise(
      n = dplyr::n(),
      
      # Mean years of schooling
      avg_escolaridad = mean(educ_years, na.rm = TRUE),
      
      # Counts
      count_no_ed     = sum(no_education, na.rm = TRUE),
      count_hs_inc    = sum(high_school_incomplete, na.rm = TRUE),
      count_hs_com    = sum(high_school_complete, na.rm = TRUE),
      count_col_inc   = sum(college_incomplete, na.rm = TRUE),
      count_col_com   = sum(college_complete, na.rm = TRUE),
      count_grad      = sum(graduate_educ, na.rm = TRUE),
      count_employed  = sum(employed, na.rm = TRUE),
      
      # Shares (Proportions)
      share_no_ed_pop     = mean(no_education, na.rm = TRUE),
      share_hs_inc_pop    = mean(high_school_incomplete, na.rm = TRUE),
      share_hs_com_pop    = mean(high_school_complete, na.rm = TRUE),
      share_col_inc_pop   = mean(college_incomplete, na.rm = TRUE),
      share_col_com_pop   = mean(college_complete, na.rm = TRUE),
      share_grad_educ_pop = mean(graduate_educ, na.rm = TRUE),
      share_employed_pop  = mean(employed, na.rm = TRUE)
    )
  
  # 7. Save and Return --------------------------------------------------------
  if (!quiet) message("💾 Saving outputs to: ", out_dir)
  
  readr::write_csv(individual_df, file.path(out_dir, "census_santiago_individual_2017.csv"))
  readr::write_csv(collapsed_df, file.path(out_dir, "census_santiago_collapsed_2017.csv"))
  
  censo2017::censo_desconectar()
  
  return(list(individual = individual_df, collapsed = collapsed_df))
}


# ------------------------------------------------------------------------------
# Function: santiago_process_census_2024
#
# @Arg       : census_dir — string; folder containing chile_census_2024_people.zip
# @Arg       : sf_data    — sf object; Spatial dataframe (metro_area) to filter by.
# @Arg       : match_col  — string; Column in sf_data with Commune codes (e.g., "CUT").
# @Arg       : out_dir    — string; Output folder for processed CSVs.
# @Arg       : overwrite  — logical; Re-extract ZIP if exists? (default FALSE).
# @Arg       : quiet      — logical; Suppress messages? (default FALSE).
#
# @Output    : list(individual, collapsed); Tibbles with processed data.
#
# @Purpose   : 1. Extracts the 2024 Census CSV from the ZIP file.
#              2. Filters by communes using the 'comuna' variable.
#              3. Uses the 'escolaridad' variable (pre-calculated by INE).
#              4. Creates labor/demographic dummies (Adults 25+).
#              5. Collapses to the commune level (simple count, no factors).
#
# @Written_by: Marcos Paulo
# @Updated_on: 18/02/2026 (Adjusted to real 2024 variables)
# ------------------------------------------------------------------------------
santiago_process_census_2024 <- function(
    census_dir = here::here("data", "downloads", "santiago", "census", "2024"),
    sf_data,
    match_col  = "CUT",
    out_dir    = here::here("data", "processed", "santiago", "census_2024"),
    overwrite  = FALSE,
    quiet      = FALSE
) {
  
  # --- 1. Checks & Setup -----------------------------------------------------
  if (!requireNamespace("vroom", quietly = TRUE)) stop("vroom required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required.")
  if (!inherits(sf_data, "sf")) stop("'sf_data' must be an sf object.")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- 2. Extract ZIP --------------------------------------------------------
  zip_path <- file.path(census_dir, "chile_census_2024_people.zip")
  if (!file.exists(zip_path)) stop("ZIP not found: ", zip_path)
  
  # List files inside ZIP to find the CSV
  files_in_zip <- utils::unzip(zip_path, list = TRUE)
  target_csv   <- grep("\\.csv$", files_in_zip$Name, value = TRUE, ignore.case = TRUE)
  
  if (length(target_csv) == 0) stop("No CSV found inside ZIP.")
  target_csv <- target_csv[1] 
  
  dest_csv <- file.path(out_dir, basename(target_csv))
  
  if (!file.exists(dest_csv) || overwrite) {
    if (!quiet) message("📦 Extracting ", target_csv, "...")
    utils::unzip(zip_path, files = target_csv, exdir = out_dir, junkpaths = TRUE)
  } else {
    if (!quiet) message("↪︎ Using existing extracted file.")
  }
  
  # --- 3. Filter Codes -------------------------------------------------------
  # Extract codes from SF object
  filter_codes <- as.integer(unique(sf_data[[match_col]]))
  if (!quiet) message(sprintf("🎯 Target: %d communes.", length(filter_codes)))
  
  # --- 4. Read & Filter (Vroom) ----------------------------------------------
  if (!quiet) message("📖 Reading and filtering CSV...")
  
  # We use vroom, selecting only necessary columns based on your provided list
  raw_data <- vroom::vroom(
    dest_csv,
    delim = ";",
    col_select = c(
      comuna,
      parentesco,          # 1 = Head of Household
      sexo,                # 1 = Male, 2 = Female
      edad,                # Numeric
      escolaridad,         # Numeric 0-24 (Pre-calculated!)
      sit_fuerza_trabajo,  # 1 = Employed
      p28_autoid_pueblo    # 1 = Yes (Indigenous)
    ),
    show_col_types = FALSE,
    # Define what strings are considered NA
    na = c("", "NA", "-99", "-66", "99", "999") 
  ) %>%
    # Immediate geographic filtering
    dplyr::filter(comuna %in% filter_codes)
  
  if (nrow(raw_data) == 0) stop("No data found for the requested communes.")
  
  # --- 5. Harmonization Logic ------------------------------------------------
  if (!quiet) message("⚙️  Harmonizing variables...")
  
  df_harm <- raw_data %>%
    dplyr::mutate(
      
      # 5.1 Education
      # -------------
      # Variable 'escolaridad' is already clean (0 to 24 years).
      # We just ensure it is numeric and generate dummies.
      educ_years = as.numeric(escolaridad),
      
      no_education           = as.numeric(educ_years == 0),
      high_school_incomplete = as.numeric(educ_years >= 1 & educ_years <= 11),
      high_school_complete   = as.numeric(educ_years == 12),
      college_incomplete     = as.numeric(educ_years >= 13 & educ_years <= 16),
      college_complete       = as.numeric(educ_years == 17),
      graduate_educ          = as.numeric(educ_years >= 18),
      
      # 5.2 Employment (sit_fuerza_trabajo)
      # -----------------------------------
      # 1 = Occupied, 2 = Unemployed, 3 = Inactive
      employed = as.numeric(sit_fuerza_trabajo == 1),
      
      # 5.3 Demographics
      # ----------------
      # Sex: 1=Male, 2=Female
      women = as.numeric(sexo == 2),
      
      # Relationship: 1=Head of household
      hh_head = as.numeric(parentesco == 1),
      hh_head_women = as.numeric(hh_head == 1 & women == 1),
      
      # Indigenous: p28_autoid_pueblo (1=Yes, 2=No)
      # Explicit NA handling
      indigena = dplyr::case_when(
        p28_autoid_pueblo == 1 ~ 1,
        p28_autoid_pueblo == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Age
      edad_num = as.numeric(edad),
      adult = as.numeric(edad_num >= 25)
    )
  
  # --- 6. Collapse -----------------------------------------------------------
  if (!quiet) message("📉 Collapsing to Commune level (Adults 25+)...")
  
  df_collapse <- df_harm %>%
    dplyr::filter(adult == 1) %>%
    dplyr::group_by(comuna) %>%
    dplyr::summarise(
      # Simple count (1 row = 1 person, as confirmed no expansion factor)
      n = dplyr::n(),
      
      # Average years of schooling
      avg_escolaridad = mean(educ_years, na.rm = TRUE),
      
      # Absolute counts
      count_no_ed     = sum(no_education, na.rm = TRUE),
      count_hs_inc    = sum(high_school_incomplete, na.rm = TRUE),
      count_hs_com    = sum(high_school_complete, na.rm = TRUE),
      count_col_inc   = sum(college_incomplete, na.rm = TRUE),
      count_col_com   = sum(college_complete, na.rm = TRUE),
      count_grad      = sum(graduate_educ, na.rm = TRUE),
      count_employed  = sum(employed, na.rm = TRUE),
      
      # Proportions (Shares)
      share_no_ed_pop     = mean(no_education, na.rm = TRUE),
      share_hs_inc_pop    = mean(high_school_incomplete, na.rm = TRUE),
      share_hs_com_pop    = mean(high_school_complete, na.rm = TRUE),
      share_col_inc_pop   = mean(college_incomplete, na.rm = TRUE),
      share_col_com_pop   = mean(college_complete, na.rm = TRUE),
      share_grad_educ_pop = mean(graduate_educ, na.rm = TRUE),
      share_employed_pop  = mean(employed, na.rm = TRUE)
    ) %>%
    dplyr::rename(CUT = comuna) # Rename to match SF object for future joins
  
  # --- 7. Save ---------------------------------------------------------------
  if (!quiet) message("💾 Saving outputs to: ", out_dir)
  
  vroom::vroom_write(df_harm, file.path(out_dir, "census_santiago_individual_2024.csv"),
                     delim = ",")
  vroom::vroom_write(df_collapse, file.path(out_dir, "census_santiago_collapsed_2024.csv"),
                     delim = ",")
  
  return(list(individual = df_harm, collapsed = df_collapse))
}

