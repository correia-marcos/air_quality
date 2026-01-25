# ============================================================================================
# IDB: Air monitoring — São Paulo module
# ============================================================================================
# @Goal   : São Paulo-specific parameters, download/process wrappers, and any site-specific code
# @Date   : Dec 2025
# @Author : Marcos Paulo
# Obs: Expect the caller to have already sourced:
#   - src/config_utils_download_data.R  (selenium helpers, waits, clicking helpers, etc.)
#   - src/config_utils_process_data.R   (merge, tidy, QA, parquet writing, etc.)
#   - src/cities/registry.R
# 
# Others obs:
# Definition of the metropolitan area comes from the State-Complementary Law No. 1139 in 
# 16/06/2011
# — Lei Complementar nº 1.139, de 16 de junho de 2011: Reorganiza a Região Metropolitana da 
# Grande São Paulo, cria o respectivo Conselho de Desenvolvimento e dá providências correlatas.
# ============================================================================================

# Parameters (single source)
sao_paulo_cfg <- list(
  id               = "santiago",
  tz               = "America/Sao_Paulo",
  base_url_shp     = "https://www.ibge.gov.br/geociencias/downloads-geociencias.html",
  base_url_qualar  = "https://qualar.cetesb.sp.gov.br/",
  base_url_census  = "",
  years            = 2000L:2023L,
  dl_dir           = here::here("data", "downloads", "sao_paulo"),
  out_dir          = here::here("data", "raw"),
  which_states     = c("Libertador General Bernardo O'Higgimns", "Metropolitana de Santiago",
                       "Valparaíso"), 
  cities_in_metro  = c(3503901, 3505708, 3506607, 3509007, 3509205, 3510609, 3513009, 3513801,
                       3515004, 3515103, 3515707, 3516309, 3516408, 3518305, 3518800, 3522208,
                       3522505, 3523107, 3525003, 3526209, 3528502, 3529401, 3530607, 3534401,
                       3539103, 3539806, 3543303, 3544103, 3545001, 3546801, 3547304, 3547809,
                       3548708, 3548807, 3549953, 3550308, 3552502, 3552809, 3556453),
  advanced_url_qualar = 
    "https://qualar.cetesb.sp.gov.br/qualar/exportaDadosAvanc.do?method=pesquisarInit"
)

# ============================================================================================
#  Santiago-specific functions - downloading and its helpers
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: sao_paulo_download_metro_area
#
# @Arg       : type              — string; "municipalities", "immediately_rg", 
#                                  "intermediate_rg", "state".
# @Arg       : base_url          — string; IBGE Downloads page URL.
# @Arg       : keep_municipality — character vector; List of municipality codes (CD_MUN)
#                                  to filter. Defaults to sao_paulo_cfg$cities_in_metro.
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
# @Purpose   : — Scrapes the IBGE Geociências website by expanding the JS Tree layer-by-layer 
#                (Drill Down) to find the São Paulo shapefiles.
#              — Filter for the metro area of São Paulo
#              — Save the filtered shape as a .gpkg file
# @Written_by: Marcos Paulo
# @Updated_on: 14/11/2025
# --------------------------------------------------------------------------------------------
sao_paulo_download_metro_area <- function(
    type              = "municipalities",
    base_url          = sao_paulo_cfg$base_url_shp,
    keep_municipality = sao_paulo_cfg$cities_in_metro,
    download_dir      = here::here("data", "downloads", "Administrative", "Brazil"),
    out_file          = here::here("data", "raw", "admin", "Brazil", "sao_paulo_metro.gpkg"),
    overwrite_zip     = FALSE,
    overwrite_gpkg    = TRUE,
    container         = TRUE,
    quiet             = FALSE
) {
  
  # 1) Setup -------------------------------------------------------------------
  valid_types <- c("municipalities", "immediately_rg", "intermediate_rg", "state")
  if (!type %in% valid_types) stop("Invalid type.")
  
  filename_map <- list(
    "municipalities"  = "SP_Municipios_2024.zip",
    "immediately_rg"  = "SP_RG_Imediatas_2024.zip",
    "intermediate_rg" = "SP_RG_Intermediarias_2024.zip",
    "state"           = "SP_UF_2024.zip"
  )
  target_file_name <- filename_map[[type]]
  
  tree_path <- c(
    "organizacao_do_territorio",
    "malhas_territoriais",
    "malhas_municipais",
    "municipio_2024",
    "UFs",
    "SP"
  )
  
  root_dl_dir <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
  if (!dir.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
  
  zip_landing_path <- file.path(root_dl_dir, target_file_name) 
  zip_target_path  <- file.path(download_dir, target_file_name)
  
  # 2) Selenium Logic ----------------------------------------------------------
  if (!file.exists(zip_target_path) || isTRUE(overwrite_zip)) {
    if (!quiet) message("⬇️  Starting Selenium to scrape IBGE Data...")
    
    if (!container) {
      if (!quiet) message("   🚀 Starting local Selenium on 4445…")
      cid <- system(paste("docker run -d -p 4445:4444 --shm-size=2g", 
                          "selenium/standalone-firefox:4.34.0-20250717"), intern = TRUE)
      on.exit(try(system(sprintf("docker rm -f %s", cid), intern=TRUE), 
                  silent=TRUE), add=TRUE)
      selenium_host <- "localhost"; selenium_port <- 4445L
    } else {
      selenium_host <- "selenium";  selenium_port <- 4444L
    }
    
    dl_dir_cont <- if (container) "/home/seluser/Downloads" else root_dl_dir
    
    caps <- list(browserName = "firefox",
                 "moz:firefoxOptions" = list(prefs = list(
                   "browser.download.folderList" = 2L,
                   "browser.download.dir" = dl_dir_cont,
                   "browser.download.useDownloadDir" = TRUE,
                   "browser.helperApps.neverAsk.saveToDisk" = 
                     "application/zip,application/octet-stream"
                 )))
    
    session <- selenium::SeleniumSession$new(
      browser = "firefox", host = selenium_host, port = selenium_port, 
      capabilities = caps, timeout = 120
    )
    on.exit(try(session$close(), silent=TRUE), add = TRUE)
    
    # -- Navigation --
    if (!quiet) message("   🔎 Navigating to IBGE...")
    session$navigate(base_url)
    Sys.sleep(5) 
    
    # --- FIX: THE COOKIE DESTROYER ---
    # We remove the banner from the DOM entirely so it can't block clicks.
    if (!quiet) message("   🍪 Nuking Cookie Banner...")
    session$execute_script(
      "var element = document.getElementById('cookie-container');
       if(element) { element.parentNode.removeChild(element); }"
    )
    Sys.sleep(1) # Allow UI to settle
    
    # -- DRILL DOWN LOOP --
    if (!quiet) message("   🪜 Drilling down the folder tree...")
    
    for (step in tree_path) {
      if (!quiet) message(sprintf("      📂 Opening: %s", step))
      
      # Use ID-based XPath for precision
      xpath_step <- sprintf("//a[contains(@class, 'jstree-anchor') and contains(@id, '%s')]",
                            step)
      
      step_found <- FALSE
      for(w in 1:5) {
        el <- try(session$find_element("xpath", xpath_step), silent=TRUE)
        if (!inherits(el, "try-error")) {
          # Use JS click to bypass any remaining invisible overlays
          session$execute_script("arguments[0].click();", el)
          step_found <- TRUE
          break
        }
        Sys.sleep(1)
      }
      
      if (!step_found) {
        # Fallback: Find by text
        xpath_text <- sprintf("//a[contains(text(), '%s')]", step)
        el <- try(session$find_element("xpath", xpath_text), silent=TRUE)
        if (!inherits(el, "try-error")) {
          session$execute_script("arguments[0].click();", el)
        } else {
          stop("❌ Failed to find tree node: ", step)
        }
      }
      Sys.sleep(2) # Wait for children to load
    }
    
    # -- CLICK TARGET FILE --
    if (!quiet) message("   🖱️  Clicking file: ", target_file_name)
    
    xpath_file <- sprintf("//a[contains(text(), '%s')]", target_file_name)
    file_link <- try(session$find_element("xpath", xpath_file), silent=TRUE)
    
    if (inherits(file_link, "try-error")) {
      stop("❌ File link not found after drill-down.")
    }
    
    # Force click via JS (Robust against scroll issues)
    session$execute_script("arguments[0].click();", file_link)
    
    # -- Wait & Move --
    if (!quiet) message("   ⏳ Waiting for file download...")
    
    download_success <- FALSE
    for (i in 1:300) { 
      if (file.exists(zip_landing_path)) {
        parts <- list.files(root_dl_dir, pattern = "\\.part$", full.names = TRUE)
        if (length(parts) == 0) {
          if (file.info(zip_landing_path)$size > 1024) {
            
            if (!quiet) message("   📦 Moving file to: ", zip_target_path)
            if (file.exists(zip_target_path)) unlink(zip_target_path)
            
            copy_ok <- file.copy(zip_landing_path, zip_target_path, overwrite = TRUE)
            if (copy_ok) {
              unlink(zip_landing_path)
              if (!quiet) message("   ✅ Download & Move Complete.")
              download_success <- TRUE
              break
            } else {
              stop("Failed to move downloaded file.")
            }
          }
        }
      }
      Sys.sleep(1)
    }
    
    if (!download_success) stop("Timeout: File never finished downloading.")
    
  } else {
    if (!quiet) message("↪︎ ZIP already present: ", basename(zip_target_path))
  }
  
  # 3) Extraction --------------------------------------------------------------
  if (!quiet) message("📦 Extracting Data...")
  exdir <- file.path(tempdir(), "sp_carto_2024")
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir)
  utils::unzip(zip_target_path, exdir = exdir)
  
  shp_files <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  target_shp <- shp_files[which.max(file.info(shp_files)$size)]
  
  # 4) Processing --------------------------------------------------------------
  if (!quiet) message("🗺️  Reading and Filtering Spatial Data...")
  sf_layer <- sf::st_read(target_shp, quiet = TRUE)
  
  if (!"CD_MUN" %in% names(sf_layer)) {
    potential_cols <- grep("CD_MUN|COD_MUN|GEOCOD", names(sf_layer), value=TRUE)
    if(length(potential_cols) > 0) {
      names(sf_layer)[names(sf_layer) == potential_cols[1]] <- "CD_MUN"
    } else {
      stop("Column 'CD_MUN' missing.")
    }
  }
  
  sf_layer$CD_MUN <- as.character(sf_layer$CD_MUN)
  sf_out <- sf_layer |> dplyr::filter(CD_MUN %in% as.character(keep_municipality))
  
  if (nrow(sf_out) == 0) stop("No municipalities matched.")
  
  if (!quiet) message("🔎 Matched ", nrow(sf_out), " Municipalities.")
  
  # 5) Save --------------------------------------------------------------------
  if (file.exists(out_file) && !overwrite_gpkg) {
    if (!quiet) message("↪︎ Output GPKG exists. Skipping write.")
  } else {
    if (!quiet) message("💾 Writing GeoPackage → ", out_file)
    if (file.exists(out_file)) unlink(out_file)
    sf::st_write(sf_out, out_file, quiet = TRUE)
  }
  
  invisible(sf_out)
}


# ------------------------------------------------------------------------------
# Function: sao_paulo_download_pollution
#
# @Arg       : base_url   — string; Login URL for QUALAR.
# @Arg       : search_url — string; Advanced Search URL.
# @Arg       : login      — string; User login (default: env var QUALAR_USER).
# @Arg       : password   — string; User password (default: env var QUALAR_PASS).
# @Arg       : years      — numeric vector; Years to download (e.g., 2000:2023).
# @Arg       : subdir     — string; Sub-path relative to root for saving files.
# @Arg       : container  — logical; TRUE if running inside Docker Selenium.
# @Arg       : quiet      — logical; If TRUE, suppresses progress messages.
#
# @Output    : tibble; Log of downloaded files.
#
# @Purpose   : Scrapes the CETESB QUALAR system.
#              1. Logs in.
#              2. Navigates to Advanced Search.
#              3. Maps all available stations.
#              4. For each station, maps available pollutants.
#              5. Batches pollutants (groups of 3) to respect system limits.
#              6. Iterates over years, downloading CSVs.
#              + Handles "No Data" pop-ups and other issues like zero byte file
#
# @Written_by: Marcos Paulo
# @Updated_on: 12/11/2025
# ------------------------------------------------------------------------------
sao_paulo_download_pollution <- function(
    base_url   = sao_paulo_cfg$base_url_qualar,
    search_url = sao_paulo_cfg$advanced_url_qualar,
    login      = Sys.getenv("QUALAR_USER"),
    password   = Sys.getenv("QUALAR_PASS"),
    years      = 2000:2023,
    subdir     = file.path("sao_paulo", "qualar_hourly"),
    container  = TRUE,
    quiet      = FALSE
) {
  
  # 1) Setup & Validation ------------------------------------------------------
  if (login == "" || password == "") stop("QUALAR_USER/PASS not set.")
  
  root_dl_dir <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  
  target_dir <- root_dl_dir
  if (!is.null(subdir)) {
    target_dir <- file.path(root_dl_dir, subdir)
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  }
  
  # 2) Start Selenium ----------------------------------------------------------
  if (!quiet) message("🚀 Starting Selenium for QUALAR...")
  
  if (!container) {
    cid <- system(paste("docker run -d -p 4445:4444 --shm-size=2g", 
                        "selenium/standalone-firefox:4.34.0-20250717"), intern=TRUE)
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern=TRUE), 
                silent=TRUE), add=TRUE)
    host <- "localhost"; port <- 4445L
  } else {
    host <- "selenium";  port <- 4444L
  }
  
  dl_inner <- if (container) "/home/seluser/Downloads" else root_dl_dir
  
  caps <- list(browserName = "firefox",
               "moz:firefoxOptions" = list(prefs = list(
                 "browser.download.folderList" = 2L,
                 "browser.download.dir" = dl_inner,
                 "browser.download.useDownloadDir" = TRUE,
                 "browser.helperApps.neverAsk.saveToDisk" = 
                   "text/csv,application/csv,text/plain"
               )))
  
  session <- selenium::SeleniumSession$new(
    browser="firefox", host=host, port=port, capabilities=caps, timeout=120
  )
  on.exit(try(session$close(), silent=TRUE), add = TRUE)
  
  # 3) Login Process -----------------------------------------------------------
  if (!quiet) message("🔐 Logging in...")
  session$navigate(base_url)
  Sys.sleep(5)
  
  login_btn    = "//*[@id='cetesb_login']"
  pass_btn     = "//*[@id='segurancaForm']/table/tbody/tr[2]/td[2]/input"
  ok_btn       = "//*[@id='segurancaForm']/table/tbody/tr[2]/td[3]/input"
  
  tryCatch({
    session$find_element("xpath", login_btn)$send_keys(login)
    session$find_element("xpath", pass_btn)$send_keys(password)
    session$find_element("xpath", ok_btn)$click()
  }, error = function(e) stop("Login failed."))
  
  Sys.sleep(5) 
  
  # 4) Mapping Stations --------------------------------------------------------
  if (!quiet) message("🗺️  Mapping Stations...")
  session$navigate(search_url)
  Sys.sleep(5)
  
  # Use a more robust selector (Name) instead of the brittle full XPath
  # The XPath /html/body/table... breaks if the page layout changes slightly
  stations_xpath_robust = "//select[@name='estacaoVO.nestcaMonto']"
  
  sel_stn <- try(session$find_element("xpath", stations_xpath_robust), silent=TRUE)
  if (inherits(sel_stn, "try-error")) stop("Could not find station dropdown.")
  
  opts <- sel_stn$find_elements("tag name", "option")
  
  station_map <- list()
  for (o in opts) {
    val <- o$get_attribute("value")
    txt <- o$get_text()
    if (val != "-1" && val != "") {
      station_map[[length(station_map)+1]] <- list(id = val, name = txt)
    }
  }
  station_map <- station_map[74:75]
  
  if (!quiet) message(sprintf("   Found %d stations.", length(station_map)))
  
  # 5) Main Loop ---------------------------------------------------------------
  log <- list()
  
  for (stn in station_map) {
    if (!quiet) message(sprintf("\n📍 Processing Station: %s (ID: %s)", stn$name, stn$id))
    
    # 5.1 Navigate & Select Station
    session$navigate(search_url)
    Sys.sleep(3)
    
    sel_stn <- session$find_element("xpath", stations_xpath_robust)
    xpath_opt <- sprintf("//option[@value='%s']", stn$id)
    sel_stn$find_element("xpath", xpath_opt)$click()
    try(session$execute_script("arguments[0].onchange();", sel_stn), silent=TRUE)
    Sys.sleep(5) 
    
    # 5.2 Map Pollutants
    checks <- session$find_elements("xpath", "//input[@name='nparmtsSelecionados']")
    
    if (length(checks) == 0) {
      message("   ⚠️ No parameters found. Skipping.")
      next
    }
    
    pollutant_map <- list()
    for (chk in checks) {
      p_id <- chk$get_attribute("value")
      p_name_el <- chk$find_element("xpath", "../../td[2]") 
      p_name <- trimws(p_name_el$get_text())
      pollutant_map[[length(pollutant_map)+1]] <- list(id = p_id, name = p_name)
    }
    
    # 5.3 Batching
    n_p <- length(pollutant_map)
    batches <- split(1:n_p, ceiling(seq_along(1:n_p)/3))
    
    for (b_idx in seq_along(batches)) {
      batch_indices <- batches[[b_idx]]
      current_polls <- pollutant_map[batch_indices]
      p_names_str <- paste(sapply(current_polls, function(x) x$name), collapse=", ")
      
      if (!quiet) message(sprintf("   ⚗️ Batch %d/%d: [%s]",
                                  b_idx, length(batches), p_names_str))
      
      # 5.4 Loop Years
      for (yr in years) {
        start_date <- sprintf("01/01/%d", yr)
        end_date   <- sprintf("01/01/%d", yr + 1)
        
        if (!quiet) message(sprintf("      📅 Date: %s to %s", start_date, end_date))
        
        attempt_counter <- 0
        success_step <- FALSE
        
        # --- SELF-HEALING RETRY LOOP ---
        while(!success_step && attempt_counter < 3) {
          attempt_counter <- attempt_counter + 1
          
          # Try/Catch block to detect the CRASH (Element Not Found)
          op_res <- tryCatch({
            
            # A. Set Dates
            d_ini <- session$find_element("xpath", "//input[@name='dataInicialStr']")
            d_ini$clear(); d_ini$send_keys(start_date); d_ini$send_keys(start_date)
            
            d_end <- session$find_element("xpath", "//input[@name='dataFinalStr']")
            d_end$clear(); d_end$send_keys(end_date); d_end$send_keys(end_date)
            
            # B. Check Boxes
            all_checks <- session$find_elements("xpath",
                                                "//input[@name='nparmtsSelecionados']")
            for (c in all_checks) { if (c$is_selected()) c$click() }
            
            for (cp in current_polls) {
              xpath_chk <- sprintf("//input[@name='nparmtsSelecionados' and @value='%s']",
                                   cp$id)
              chk <- session$find_element("xpath", xpath_chk)
              if (!chk$is_selected()) chk$click()
            }
            
            # C. Export
            btn_exp <- session$find_element("xpath", "//input[@value='Exportar']")
            files_before <- list.files(root_dl_dir, full.names = TRUE)
            btn_exp$click()
            
            TRUE 
            
          }, error = function(e) {
            if (!quiet) message("      ⚠️ Page broken/timeout. Initiating Soft Reset...")
            return(FALSE)
          })
          
          # Handle Crash Recovery
          if (op_res == FALSE) {
            
            # Try to login again (FIXED: replaced stop() with message())
            tryCatch({
              session$navigate(search_url)
              # If we are already logged in, finding these might fail or succeed, 
              # but we just want to ensure we are good to go.
              # Often just navigating to search_url is enough if cookies are alive.
              if (length(session$find_elements("xpath", login_btn)) > 0) {
                session$find_element("xpath", login_btn)$send_keys(login)
                session$find_element("xpath", pass_btn)$send_keys(password)
                session$find_element("xpath", ok_btn)$click()
                Sys.sleep(5)
              }
            }, error = function(e) {
              message("      ℹ️ Login elements not found (probably already logged in).")
            })
            
            # SOFT RESET LOGIC
            session$navigate(search_url)
            Sys.sleep(5)
            
            sel_stn <- session$find_element("xpath", stations_xpath_robust)
            xpath_opt <- sprintf("//option[@value='%s']", stn$id)
            sel_stn$find_element("xpath", xpath_opt)$click()
            try(session$execute_script("arguments[0].onchange();", sel_stn), silent=TRUE)
            Sys.sleep(8) # INCREASED SLEEP TO PREVENT STALE ELEMENT
            next 
          }
          
          # D. Alert Check
          alert_present <- FALSE
          tryCatch({
            Sys.sleep(1.5) 
            al <- session$get_alert_text()
            if (!is.null(al) && al != "") {
              if (grepl("Atualizando base de dados", al, ignore.case = TRUE)) {
                if (!quiet) message("      🔄 Database update alert. Soft Resetting...")
                session$accept_alert()
                
                # SOFT RESET LOGIC
                session$navigate(search_url)
                Sys.sleep(5)
                sel_stn <- session$find_element("xpath", stations_xpath_robust)
                sel_stn$find_element("xpath", sprintf("//option[@value='%s']", stn$id))$click()
                try(session$execute_script("arguments[0].onchange();", sel_stn), silent=TRUE)
                Sys.sleep(8) # INCREASED SLEEP
                
                alert_present <- TRUE 
              } else {
                if (!quiet) message("      ⚠️ Alert: ", al)
                session$accept_alert()
                alert_present <- TRUE
              }
            }
          }, error = function(e) {})
          
          if (alert_present) next 
          
          success_step <- TRUE 
        }
        
        if (!success_step) next 
        
        # E. Monitor Download
        got_file <- FALSE
        for (w in 1:60) {
          files_now <- list.files(root_dl_dir, full.names = TRUE)
          new_files <- setdiff(files_now, files_before)
          valid <- new_files[!grepl("\\.(part|crdownload|tmp)$", new_files)]
          
          if (length(valid) > 0) {
            # Stability Check
            cand <- valid[1]
            size <- file.info(cand)$size
            if (!is.na(size) && size > 0) {
              Sys.sleep(2)
              if (file.info(cand)$size == size) {
                # Move Logic...
                raw <- cand
                safe_stn <- gsub("[^A-Za-z0-9]", "", stn$name)
                safe_pol <- gsub("[^A-Za-z0-9]", "", p_names_str)
                if (nchar(safe_pol) > 50) safe_pol <- substr(safe_pol, 1, 50)
                
                fn <- sprintf("QUALAR_%s_%s_%d.csv", safe_stn, safe_pol, yr)
                dest <- file.path(target_dir, fn)
                
                if (file.exists(dest)) unlink(dest)
                file.copy(raw, dest)
                unlink(raw)
                
                log[[length(log)+1]] <- list(station = stn$name,
                                             params  = p_names_str,
                                             year    = yr,
                                             file    = dest,
                                             status  = "OK")
                got_file <- TRUE
                break
              }
            }
          }
          Sys.sleep(0.5)
        }
        
        if (!got_file) {
          pg_src <- try(session$get_page_source(), silent=TRUE)
          if (!inherits(pg_src, "try-error") && grepl("Nenhum registro encontrado", pg_src)) {
            message("      ⚠️ No records found.")
            
            # RECOVERY LOGIC - UPDATED
            session$navigate(search_url) 
            Sys.sleep(5) # Increased wait
            
            sel_stn <- session$find_element("xpath", stations_xpath_robust)
            sel_stn$find_element("xpath", sprintf("//option[@value='%s']", stn$id))$click()
            session$execute_script("arguments[0].onchange();", sel_stn)
            
            # CRITICAL FIX: Longer wait here prevents Stale Element in next loop
            Sys.sleep(8) 
          } else {
            message("      ❌ Timeout or 0-byte file ignored.")
          }
        }
      } # End Year
    } # End Batch
  } # End Station
  
  message("✅ QUALAR Scraping Complete.")
  return(dplyr::bind_rows(log))
}

