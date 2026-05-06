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
    "https://qualar.cetesb.sp.gov.br/qualar/exportaDadosAvanc.do?method=pesquisarInit",
  metadata_url_qualar = 
    "https://qualar.cetesb.sp.gov.br/qualar/relConfiguracaoEstacao.do?method=pesquisarInit"
)

# ============================================================================================
#  São Paulo-specific helpers function (for downloading pollution data from stations)
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: download_qualar_year
#
# @Arg       : station_id      — string; ID value of the station (from dropdown).
# @Arg       : station_name    — string; Name of the station (for filename).
# @Arg       : year            — integer; The specific year to download.
# @Arg       : pollutant_ids   — character vector; IDs of pollutants to check.
# @Arg       : pollutant_names — character vector; Names of pollutants (for filename).
# @Arg       : base_url        — string; Login URL.
# @Arg       : search_url      — string; Advanced Search URL.
# @Arg       : login           — string; Username.
# @Arg       : password        — string; Password.
# @Arg       : download_dir    — string; Local path to save the CSV.
# @Arg       : container       — logical; TRUE if running inside Docker Selenium.
# @Arg       : quiet           — logical; Suppress messages?
#
# @Output    : logical; TRUE if successful (or skipped), FALSE if failed.
#
# @Purpose   : Starts a FRESH Selenium session to download ONE year of data
#              for ONE station. This prevents session rot and crashes.
#              1. Launches fresh browser.
#              2. Logs in & Navigates to search.
#              3. Selects Station, Parameters, and Dates.
#              4. Downloads and moves file to final destination.
#              5. Closes browser immediately.
#
# @Written_by: Marcos Paulo
# @Updated_on: 17/02/2026
# --------------------------------------------------------------------------------------------
download_qualar_year <- function(
    station_id,
    station_name,
    year,
    pollutant_ids,
    pollutant_names,
    base_url,
    search_url,
    login,
    password,
    download_dir,
    container = TRUE,
    quiet = FALSE
) {
  
  # 1) Setup
  root_dl_dir  <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  container_dl <- if (container) "/home/seluser/Downloads" else root_dl_dir
  
  # Define Output Filename Pattern
  # Clean names for filesystem safety
  safe_stn <- gsub("[^A-Za-z0-9]", "", station_name)
  
  safe_pol <- gsub("[^A-Za-z0-9]", "", paste(pollutant_names))
  safe_pol <- paste(sapply(safe_pol, function(x) x), collapse="__")
  
  # Truncate if too long to avoid FS errors
  if (nchar(safe_pol) > 40) safe_pol <- substr(safe_pol, 1, 40)
  
  final_filename <- sprintf("QUALAR_%s_%s_%d.csv", safe_stn, safe_pol, year)
  dest_path      <- file.path(download_dir, final_filename)
  
  # Skip if already exists (Idempotency)
  if (file.exists(dest_path) && file.size(dest_path) > 1000) {
    if (!quiet) message(sprintf("   ⏭️  Skipping %s (Already exists)", final_filename))
    return(TRUE)
  }
  
  if (!quiet) message(sprintf("   🚀 Launching Worker for: %s | %d", station_name, year))
  
  # 2) Launch Browser
  if (!container) {
    host <- "localhost"; port <- 4445L
  } else {
    host <- "selenium";  port <- 4444L
  }
  
  caps <- list(browserName = "firefox",
               "moz:firefoxOptions" = list(prefs = list(
                 "browser.download.folderList" = 2L,
                 "browser.download.dir" = container_dl,
                 "browser.download.useDownloadDir" = TRUE,
                 "browser.helperApps.neverAsk.saveToDisk" = 
                   "text/csv,application/csv,text/plain"
               )))
  
  session <- NULL
  success <- FALSE
  
  tryCatch({
    session <- selenium::SeleniumSession$new(
      browser = "firefox", host = host, port = port, capabilities = caps, timeout = 60
    )
    
    # 3) Login
    session$navigate(base_url)
    Sys.sleep(4)
    
    session$find_element("xpath", "//*[@id='cetesb_login']")$send_keys(login)
    session$find_element(
      "xpath",
      "//*[@id='segurancaForm']/table/tbody/tr[2]/td[2]/input")$send_keys(password)
    session$find_element("xpath",
                         "//*[@id='segurancaForm']/table/tbody/tr[2]/td[3]/input")$click()
    Sys.sleep(4)
    
    # 4) Navigate to Search
    session$navigate(search_url)
    Sys.sleep(3)
    
    # 5) Select Station
    # Use name-based selector for robustness
    sel_stn <- session$find_element("xpath", "//select[@name='estacaoVO.nestcaMonto']")
    
    # Click option by Value
    xpath_opt <- sprintf("//option[@value='%s']", station_id)
    opt <- sel_stn$find_element("xpath", xpath_opt)
    opt$click()
    
    # Force change event
    try(session$execute_script("arguments[0].onchange();", sel_stn), silent = TRUE)
    Sys.sleep(5) # Critical wait for parameters to load
    
    # 6) Select Parameters
    # Uncheck everything first
    all_checks <- session$find_elements("xpath", "//input[@name='nparmtsSelecionados']")
    for (chk in all_checks) { if (chk$is_selected()) chk$click() }
    
    # Check specific pollutants
    # Note: We wrap this in try() because the global list might have a pollutant 
    # that is not valid for *this specific* station.
    for (pid in pollutant_ids) {
      xpath_chk <- sprintf("//input[@name='nparmtsSelecionados' and @value='%s']", pid)
      chk <- try(session$find_element("xpath", xpath_chk), silent=TRUE)
      if (!inherits(chk, "try-error")) {
        if (!chk$is_selected()) chk$click()
      }
    }
    
    # 7) Set Dates
    start_date <- sprintf("01/01/%d", year)
    end_date   <- sprintf("01/01/%d", year + 1)
    
    d_ini <- session$find_element("xpath", "//input[@name='dataInicialStr']")
    d_ini$clear(); d_ini$send_keys(start_date); d_ini$send_keys(start_date)
    
    d_end <- session$find_element("xpath", "//input[@name='dataFinalStr']")
    d_end$clear(); d_end$send_keys(end_date); d_end$send_keys(end_date)
    
    # 8) Export
    files_before <- list.files(root_dl_dir, full.names = TRUE)
    
    btn_exp <- session$find_element("xpath", "//input[@value='Exportar']")
    btn_exp$click()
    
    # 9) Wait for Download
    got_file <- FALSE
    for (w in 1:40) { # Wait up to 40s
      files_now <- list.files(root_dl_dir, full.names = TRUE)
      new_files <- setdiff(files_now, files_before)
      valid <- new_files[!grepl("\\.(part|crdownload|tmp)$", new_files)]
      
      if (length(valid) > 0) {
        cand <- valid[1]
        # Ensure file is done writing
        if (file.info(cand)$size > 0) {
          Sys.sleep(2) 
          
          if (file.exists(dest_path)) unlink(dest_path)
          file.copy(cand, dest_path)
          unlink(cand)
          
          if (!quiet) message("      ✅ Downloaded.")
          got_file <- TRUE
          success  <- TRUE
          break
        }
      }
      
      # Check for "No Data" Alert
      al <- try(session$get_alert_text(), silent=TRUE)
      if (!inherits(al, "try-error") && !is.null(al)) {
        if (grepl("Nenhum registro", al) || grepl("Atualizando", al)) {
          if (!quiet) message("      ⚠️ No data/Maintenance (Alert)")
          session$accept_alert()
          success <- TRUE # Handled failure
          break
        }
      }
      Sys.sleep(1)
    }
    
    if (!got_file && !success) {
      # Check page text for "No data" if alert didn't trigger
      src <- try(session$get_page_source(), silent=TRUE)
      if (!inherits(src, "try-error") && grepl("Nenhum registro encontrado", src)) {
        if (!quiet) message("      ⚠️ No data found (Page Text)")
        success <- TRUE
      } else {
        warning("Timeout waiting for file.")
      }
    }
    
  }, error = function(e) {
    message(paste("      ❌ Worker Error:", e$message))
    success <- FALSE
  }, finally = {
    if (!is.null(session)) try(session$close(), silent=TRUE)
  })
  
  return(success)
}


# --------------------------------------------------------------------------------------------
# Function: map_qualar_metadata
#
# @Arg       : base_url   — string; Login URL.
# @Arg       : search_url — string; Search page URL.
# @Arg       : login      — string; Username.
# @Arg       : password   — string; Password.
# @Arg       : container  — logical; TRUE if Docker.
#
# @Output    : list(stations = list(...), pollutants = list(...))
#
# @Purpose   : Performs a quick login to scrape the available Stations and
#              Pollutant parameters (checkboxes). 
#              Required by the Manager to generate the task list.
#
# @Written_by: Marcos Paulo
# @Updated_on: 17/02/2026
# --------------------------------------------------------------------------------------------
map_qualar_metadata <- function(base_url, search_url, login, password, container) {
  
  # 1) Setup
  if (login == "" || password == "") stop("QUALAR_USER/PASS not set.")
  
  # 2) Start Selenium
  if (!container) {
    host <- "localhost"; port <- 4445L # Assuming local instance available
  } else {
    host <- "selenium";  port <- 4444L
  }
  
  caps <- list(browserName = "firefox")
  
  session <- selenium::SeleniumSession$new(
    browser="firefox", host=host, port=port, capabilities=caps, timeout=60
  )
  on.exit(try(session$close(), silent=TRUE), add = TRUE)
  
  # 3) Login Process
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
  
  # 4) Mapping Stations
  session$navigate(search_url)
  Sys.sleep(5)
  
  stations_xpath_robust = "//select[@name='estacaoVO.nestcaMonto']"
  
  sel_stn <- try(session$find_element("xpath", stations_xpath_robust), silent=TRUE)
  if (inherits(sel_stn, "try-error")) stop("Could not find station dropdown.")
  
  # A. Scrape Stations
  opts <- sel_stn$find_elements("tag name", "option")
  station_map <- list()
  for (o in opts) {
    val <- o$get_attribute("value")
    txt <- o$get_text()
    if (val != "-1" && val != "") {
      station_map[[length(station_map)+1]] <- list(id = val, name = txt)
    }
  }
  
  # B. Scrape Pollutants
  # (We must ensure at least one station is 'active' or checks are visible)
  # Usually checks are visible by default, or appear after select.
  # We select the first real station just to be safe.
  if (length(station_map) > 0) {
    first_id <- station_map[[1]]$id
    
    session$navigate(search_url)
    Sys.sleep(3)
    
    sel_stn <- session$find_element("xpath", stations_xpath_robust)
    xpath_opt <- sprintf("//option[@value='%s']", first_id)
    sel_stn$find_element("xpath", xpath_opt)$click()
    check <- try(session$execute_script("arguments[0].onchange();", sel_stn),
                 silent=TRUE)
    Sys.sleep(3)
  }
  
  checks <- session$find_elements("xpath", "//input[@name='nparmtsSelecionados']")
  pollutant_map <- list()
  
  if (length(checks) > 0) {
    for (chk in checks) {
      p_id <- chk$get_attribute("value")
      # Label is usually in the TD next to it or parent
      # XPath: ../../td[2] based on your previous valid code
      p_name_el <- try(chk$find_element("xpath", "../../td[2]"), silent=TRUE)
      p_name <- if (inherits(p_name_el, "try-error")) {
        "Unknown"} else trimws(p_name_el$get_text())
      
      pollutant_map[[length(pollutant_map)+1]] <- list(id = p_id, name = p_name)
    }
  }
  
  return(list(stations = station_map, pollutants = pollutant_map))
}

# ============================================================================================
#  São Paulo-specific functions - downloading main functions
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: sao_paulo_download_metro_area
#
# @Arg       : level             — string; "mpio" (Municipality) or 
#                                  "setor_censitario" (Census Tract).
# @Arg       : base_url          — string; IBGE Downloads page URL.
# @Arg       : keep_municipality — character vector; List of municipality codes (CD_GEOCODM)
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
# @Purpose   : - Scrapes the IBGE Geociências website for Census 2010 Shapefiles.
#              - "mpio": Downloads sp_municipio.zip (Municipal Boundaries 2010).
#              - "setor_censitario": Downloads sp_setores_censitarios.zip (Tracts 2010).
#              - Filters for the metro area of São Paulo.
#              - Fixes encoding (LATIN1) and saves as .gpkg.
#
# @Written_by: Marcos Paulo
# @Updated_on: 05/12/2025
# --------------------------------------------------------------------------------------------
sao_paulo_download_metro_area <- function(
    level             = c("mpio", "setor_censitario"),
    base_url          = sao_paulo_cfg$base_url_shp,
    keep_municipality = sao_paulo_cfg$cities_in_metro,
    download_dir      = here::here("data", "downloads", "Administrative", "Brazil"),
    out_file          = here::here("data", "raw", "admin", "Brazil", "sao_paulo_metro.gpkg"),
    overwrite_zip     = FALSE,
    overwrite_gpkg    = TRUE,
    container         = TRUE,
    quiet             = FALSE
) {
  
  level <- match.arg(level)
  
  # 1) Setup Paths & Filenames ---------------------------------------------------

  # Both levels use the same tree path to get to the 2010 Census folder
  tree_path <- c(
    "organizacao_do_territorio",
    "malhas_territoriais",
    "malhas_de_setores_censitarios__divisoes_intramunicipais",
    "censo_2010",
    "setores_censitarios_shp",
    "sp"
  )
  
  # Define target file based on level
  if (level == "mpio") {
    target_file_name <- "sp_municipios.zip"
  } else {
    target_file_name <- "sp_setores_censitarios.zip"
  }
  
  root_dl_dir <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  
  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
  if (!dir.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
  
  zip_landing_path <- file.path(root_dl_dir, target_file_name) 
  zip_target_path  <- file.path(download_dir, target_file_name)
  
  # 2) Selenium Download Logic ---------------------------------------------------
  if (!file.exists(zip_target_path) || isTRUE(overwrite_zip)) {
    if (!quiet) message("⬇️  Starting Selenium to scrape IBGE Data (Census 2010)...")
    
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
    
    # -- Cookie Banner Nuke --
    if (!quiet) message("   🍪 Nuking Cookie Banner...")
    session$execute_script(
      "var element = document.getElementById('cookie-container');
       if(element) { element.parentNode.removeChild(element); }"
    )
    Sys.sleep(1)
    
    # -- Drill Down --
    if (!quiet) message("   🪜 Drilling down the folder tree...")
    
    for (step in tree_path) {
      if (!quiet) message(sprintf("      📂 Opening: %s", step))
      
      # Try ID-based click first, then text-based
      xpath_step <- sprintf("//a[contains(@class, 'jstree-anchor') and contains(@id, '%s')]",
                            step)
      
      step_found <- FALSE
      for(w in 1:5) {
        el <- try(session$find_element("xpath", xpath_step), silent=TRUE)
        if (!inherits(el, "try-error")) {
          session$execute_script("arguments[0].click();", el)
          step_found <- TRUE
          break
        }
        Sys.sleep(1)
      }
      
      if (!step_found) {
        xpath_text <- sprintf("//a[contains(text(), '%s')]", step)
        el <- try(session$find_element("xpath", xpath_text), silent=TRUE)
        if (!inherits(el, "try-error")) {
          session$execute_script("arguments[0].click();", el)
        } else {
          stop("❌ Failed to find tree node: ", step)
        }
      }
      Sys.sleep(2)
    }
    
    # -- Click Target File --
    if (!quiet) message("   🖱️  Clicking file: ", target_file_name)
    
    xpath_file <- sprintf("//a[contains(text(), '%s')]", target_file_name)
    file_link <- try(session$find_element("xpath", xpath_file), silent=TRUE)
    
    if (inherits(file_link, "try-error")) {
      stop("❌ File link not found after drill-down.")
    }
    
    session$execute_script("arguments[0].click();", file_link)
    
    # -- Download Wait & Move --
    if (!quiet) message("   ⏳ Waiting for download...")
    
    download_success <- FALSE
    for (i in 1:300) { 
      if (file.exists(zip_landing_path)) {
        parts <- list.files(root_dl_dir, pattern = "\\.part$", full.names = TRUE)
        if (length(parts) == 0) {
          # Check size > 1KB
          if (file.info(zip_landing_path)$size > 1024) {
            
            if (!quiet) message("   📦 Moving to: ", zip_target_path)
            if (file.exists(zip_target_path)) unlink(zip_target_path)
            
            copy_ok <- file.copy(zip_landing_path, zip_target_path, overwrite = TRUE)
            if (copy_ok) {
              unlink(zip_landing_path)
              if (!quiet) message("   ✅ Download Complete.")
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
  
  # Create a specific temp dir for this extraction to avoid collisions
  exdir <- file.path(tempdir(), paste0("sp_carto_2010_", level))
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir)
  
  utils::unzip(zip_target_path, exdir = exdir)
  
  shp_files <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp_files) == 0) stop("No .shp file found in ZIP.")
  
  # Usually only one shapefile per 2010 zip, but pick largest just in case
  target_shp <- shp_files[which.max(file.info(shp_files)$size)]
  
  # 4) Processing --------------------------------------------------------------
  if (!quiet) message("🗺️  Reading and Filtering Spatial Data...")
  
  # IMPORTANT: Set ENCODING=LATIN1 for IBGE 2010 files to fix character issues
  sf_layer <- sf::st_read(target_shp, quiet = TRUE, options = "ENCODING=LATIN1")
  
  # Standardize Column Names
  # 2010 files usually have CD_GEOCODM for municipalities.
  # Sector files have CD_GEOCODI (tract ID) and usually CD_GEOCODM (muni ID).
  
  if (!"CD_GEOCODM" %in% names(sf_layer)) {
    # If explicit muni column is missing, try to infer or check alternatives
    potential_cols <- grep("CD_GEOCODM|COD_MUN|CD_MUN", names(sf_layer), value=TRUE)
    if(length(potential_cols) > 0) {
      names(sf_layer)[names(sf_layer) == potential_cols[1]] <- "CD_GEOCODM"
    } else {
      # If it's a sector file without muni column, we can slice CD_GEOCODI
      if ("CD_GEOCODI" %in% names(sf_layer)) {
        sf_layer$CD_GEOCODM <- substr(as.character(sf_layer$CD_GEOCODI), 1, 7)
      } else {
        stop("Critical columns (CD_GEOCODM or CD_GEOCODI) missing.")
      }
    }
  }
  
  # Filter
  sf_layer$CD_GEOCODM <- as.character(sf_layer$CD_GEOCODM)
  keep_municipality   <- as.character(keep_municipality)
  
  sf_out <- sf_layer |> dplyr::filter(CD_GEOCODM %in% keep_municipality)
  
  if (nrow(sf_out) == 0) stop("No data matched the provided municipality codes.")
  
  if (!quiet) message("🔎 Matched ", nrow(sf_out), " features (Level: ", level, ")")
  
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



# --------------------------------------------------------------------------------------------
# Function: sao_paulo_download_weighting_areas
#
# @Arg       : keep_municipality — vector; IBGE municipality codes to keep.
# @Arg       : year              — numeric; Census year (default 2010).
# @Arg       : out_file          — string; Output GeoPackage path.
# @Arg       : overwrite_gpkg    — logical; Overwrite if exists?
# @Arg       : quiet             — logical; Suppress console messages?
#
# @Output    : sf dataframe
#
# @Purpose   : Downloads Census Weighting Areas (Áreas de Ponderação) via geobr.
#              Filters for specific municipalities and saves to disk.
# --------------------------------------------------------------------------------------------
sao_paulo_download_weighting_areas <- function(
    keep_municipality,
    year           = 2010,
    out_file       = here::here("data", "raw", "admin", "Brazil", 
                                "sp_weighting_areas.gpkg"),
    overwrite_gpkg = TRUE,
    quiet          = FALSE
) {
  
  # 1) Check Dependencies
  req_pkgs <- c("geobr", "sf", "dplyr")
  for(p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(paste("Package", p, "required."))
    }
  }
  
  # 2) Check for Existing File
  if (file.exists(out_file) && !overwrite_gpkg) {
    if (!quiet) message("↪︎ Output GPKG exists. Loading from disk...")
    return(sf::st_read(out_file, quiet = TRUE))
  }
  
  # 3) Download Data
  if (!quiet) message("⬇️  Downloading weighting areas via geobr...")
  
  # Using code_weighting = "SP" prevents downloading the whole country
  data_sf <- tryCatch({
    geobr::read_weighting_area(
      code_weighting = "SP", 
      year = year, 
      showProgress = !quiet
    )
  }, error = function(e) {
    stop("❌ Failed to download data from geobr: ", e$message)
  })
  
  # 4) Process and Filter
  if (!quiet) message("🗺️  Filtering Spatial Data...")
  
  # Ensure character matching for IBGE codes to prevent silent failures
  keep_municipality <- as.character(keep_municipality)
  
  sf_out <- data_sf |> 
    dplyr::mutate(code_muni = as.character(code_muni)) |>
    dplyr::filter(code_muni %in% keep_municipality)
  
  if (nrow(sf_out) == 0) {
    stop("No data matched the provided municipality codes.")
  }
  
  if (!quiet) {
    message("🔎 Matched ", nrow(sf_out), " weighting areas.")
  }
  
  # 5) Save Output
  if (!quiet) message("💾 Writing GeoPackage → ", out_file)
  
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(out_file)) unlink(out_file)
  
  sf::st_write(sf_out, out_file, quiet = TRUE)
  
  return(sf_out)
}


# --------------------------------------------------------------------------------------------
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
# @Output    : NULL (Actions are side-effects: file downloads).
#
# @Purpose   : Scrapes the CETESB QUALAR system.
#              1. Maps stations and pollutants via `map_qualar_metadata`.
#              2. Generates a to-do list (Stations x Pollutants x Years).
#              3. Calls `download_qualar_year` (The Worker) for each task.
#              4. Retries failed tasks up to 3 times before moving on.
#
# @Written_by: Marcos Paulo
# @Updated_on: 17/02/2026
# --------------------------------------------------------------------------------------------
sao_paulo_download_pollution <- function(
    base_url   = sao_paulo_cfg$base_url_qualar,
    search_url = sao_paulo_cfg$advanced_url_qualar,
    login      = Sys.getenv("QUALAR_USER"),
    password   = Sys.getenv("QUALAR_PASS"),
    years      = sao_paulo_cfg$years,
    subdir     = file.path("sao_paulo", "qualar_hourly"),
    container  = TRUE,
    quiet      = FALSE
) {
  
  # 1) Setup
  root_dl_dir <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  target_dir <- if (!is.null(subdir)) file.path(root_dl_dir, subdir) else root_dl_dir
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  
  # 2) Step 1: Map Metadata (One-time short session)
  if (!quiet) message("🗺️  Step 1: Mapping Stations and Parameters...")
  
  metadata <- tryCatch({
    map_qualar_metadata(base_url, search_url, login, password, container)
  }, error = function(e) stop("Failed to map metadata: ", e$message))
  
  station_map   <- metadata$stations
  pollutants    <- metadata$pollutants
  
  if (!quiet) message(sprintf("   Found %d stations and %d parameters.", 
                              length(station_map), length(pollutants)))
  
  # 3) Step 2: Execute Tasks
  for (stn in station_map) {
    if (!quiet) message(sprintf("\n📍 Station: %s (ID: %s)", stn$name, stn$id))
    
    # Batching Pollutants (Groups of 3)
    n_p <- length(pollutants)
    if (n_p == 0) { message("   ⚠️ No pollutants found. Skipping."); next }
    
    batches <- split(1:n_p, ceiling(seq_along(1:n_p)/3))
    
    for (b_idx in seq_along(batches)) {
      batch_indices <- batches[[b_idx]]
      current_polls <- pollutants[batch_indices]
      
      p_ids   <- sapply(current_polls, function(x) x$id)
      p_names <- sapply(current_polls, function(x) x$name)
      
      for (yr in years) {
        
        # RETRY LOGIC for the Worker
        attempts <- 0
        done <- FALSE
        while(!done && attempts < 3) {
          attempts <- attempts + 1
          
          done <- download_qualar_year(
            station_id   = stn$id,
            station_name = stn$name,
            year         = yr,
            pollutant_ids = p_ids,
            pollutant_names = p_names,
            base_url     = base_url,
            search_url   = search_url,
            login        = login,
            password     = password,
            download_dir = target_dir,
            container    = container,
            quiet        = quiet
          )
          
          if (!done) {
            message("      ⚠️ Worker failed. Retrying in 10s...")
            Sys.sleep(10)
          }
        }
      }
    }
  }
  
  message("✅ All Downloads Complete.")
}


# --------------------------------------------------------------------------------------------
# Function: sao_paulo_download_metadata
#
# @Arg       : base_url   — string; Login URL for QUALAR.
# @Arg       : search_url — string; URL for "Station Configuration" page.
# @Arg       : login      — string; User login.
# @Arg       : password   — string; User password.
# @Arg       : out_file   — string; Path to save the resulting CSV.
# @Arg       : container  — logical; TRUE if running inside Docker Selenium.
# @Arg       : quiet      — logical; If TRUE, suppresses progress messages.
#
# @Output    : tibble; The scraped metadata (invisibly).
#
# @Purpose   : Scrapes station metadata from CETESB QUALAR.
#              1. Logs in.
#              2. Navigates to "Configuração das Estações".
#              3. Clicks "Gerar" to open the pop-up report.
#              4. Switches Selenium context to the new pop-up window.
#              5. Iterates through each UGRHI table to extract:
#                 - Station Name & Address
#                 - UTM Coordinates (North/East/Zone)
#                 - Monitoring Parameters (Active/Inactive)
#
# @Written_by: Marcos Paulo
# @Updated_on: 17/02/2026
# --------------------------------------------------------------------------------------------
sao_paulo_download_metadata <- function(
    base_url   = sao_paulo_cfg$base_url_qualar,
    search_url = sao_paulo_cfg$metadata_url_qualar,
    login      = Sys.getenv("QUALAR_USER"),
    password   = Sys.getenv("QUALAR_PASS"),
    out_file   = sao_paulo_cfg$dl_dir,
    container  = TRUE,
    quiet      = FALSE
) {
  
  # 1) Setup
  if (login == "" || password == "") stop("QUALAR_USER/PASS not set.")
  
  if (!dir.exists(dirname(out_file))) {
    dir.create(dirname(out_file), recursive = TRUE)
  }
  
  # 2) Start Selenium
  if (!quiet) message("🚀 Starting Selenium for Metadata Scraping...")
  
  if (!container) {
    host <- "localhost"; port <- 4445L
  } else {
    host <- "selenium";  port <- 4444L
  }
  
  caps <- list(browserName = "firefox")
  
  session <- selenium::SeleniumSession$new(
    browser = "firefox", host = host, port = port, 
    capabilities = caps, timeout = 60
  )
  on.exit(try(session$close(), silent = TRUE), add = TRUE)
  
  # 3) Login
  if (!quiet) message("🔐 Logging in...")
  session$navigate(base_url)
  Sys.sleep(4)
  
  tryCatch({
    session$find_element(
      "xpath", "//*[@id='cetesb_login']"
    )$send_keys(login)
    
    pass_xpath <- "//*[@id='segurancaForm']/table/tbody/tr[2]/td[2]/input"
    session$find_element("xpath", pass_xpath)$send_keys(password)
    
    ok_xpath   <- "//*[@id='segurancaForm']/table/tbody/tr[2]/td[3]/input"
    session$find_element("xpath", ok_xpath)$click()
  }, error = function(e) stop("Login failed."))
  
  Sys.sleep(4)
  
  # 4) Navigate to Configuration Page
  if (!quiet) message("🗺️  Navigating to Station Configuration...")
  session$navigate(search_url)
  Sys.sleep(3)
  
  # 5) Trigger Pop-up
  # Capture window handles before clicking (Snake Case for 'selenium')
  handles_before <- session$window_handles(timeout = 20)
  
  if (!quiet) message("🖱️  Clicking 'Gerar' to open report...")
  
  btn_gerar <- session$find_element("xpath", "//input[@name='btnGerar']")
  btn_gerar$click()
  
  # 6) Switch to Pop-up Window
  if (!quiet) message("⏳ Waiting for pop-up window...")
  
  new_handle <- NULL
  for (i in 1:20) { # Wait up to 20s
    handles_now <- session$window_handles()
    diff <- setdiff(handles_now, handles_before)
    if (length(diff) > 0) {
      new_handle <- diff[[1]]
      break
    }
    Sys.sleep(1)
  }
  
  if (is.null(new_handle)) stop("❌ Pop-up window did not appear.")
  
  # Switch focus to the new window
  session$switch_to_window(new_handle)
  Sys.sleep(5) # Wait for table render
  
  # 7) Get HTML & Parse
  if (!quiet) message("📥 Downloading Page Source...")
  page_source <- session$get_page_source()
  
  # We can close the session now as we have the HTML
  session$close() 
  on.exit() 
  
  # 8) Parsing Logic (Iterate over tables)
  if (!quiet) message("🏗️  Parsing HTML Tables...")
  
  html <- rvest::read_html(page_source)
  
  # Find all tables that look like data tables (contain 'Estações')
  xpath_tables <- "//table[.//th[contains(., 'Estações')]]"
  tables <- html |> rvest::html_nodes(xpath = xpath_tables)
  
  if (length(tables) == 0) stop("No data tables found in the pop-up.")
  
  all_data_list <- list()
  
  for (i in seq_along(tables)) {
    tbl <- tables[[i]]
    
    # A. Extract Headers for *this* table
    # The param row is immediately after the row containing "Estações"
    xpath_th <- paste0(".//tr[th[contains(., 'Estações')]]",
                       "/following-sibling::tr[1]")
    
    header_row <- tbl |> rvest::html_node(xpath = xpath_th)
    headers    <- header_row |> rvest::html_nodes("th") |> rvest::html_text()
    
    # Clean headers
    headers <- trimws(gsub("\u00A0", " ", headers))
    
    # Safety check: We need at least 3 headers for coords
    if (length(headers) < 3) next
    
    # The last 3 are always coords, the rest are pollutants
    param_names <- head(headers, -3)
    
    # B. Extract Data Rows for *this* table
    xpath_rows <- ".//tr[td[@id='borda_branco' or @id='borda_azul']]"
    rows <- tbl |> rvest::html_nodes(xpath = xpath_rows)
    
    for (r in rows) {
      cols <- r |> rvest::html_nodes("td") |> rvest::html_text()
      cols <- trimws(gsub("\u00A0", " ", cols))
      
      # Calc expected length: 
      # 1 (Name) + N (Params) + 1 (Addr) + 3 (Coords) + 1 (Link)
      expected_len <- 1 + length(param_names) + 1 + 3 + 1
      
      # We allow missing link column just in case
      if (length(cols) >= expected_len - 1) {
        
        station_name <- cols[1]
        
        # Params are cols 2 to N+1
        param_vals  <- cols[2:(1 + length(param_names))]
        # Convert "X" or "*" to TRUE
        param_bools <- grepl("X|\\*", param_vals, ignore.case = TRUE)
        names(param_bools) <- param_names
        
        # Address & Coords
        idx_addr <- 1 + length(param_names) + 1
        address  <- cols[idx_addr]
        utm_y    <- cols[idx_addr + 1] # "Latitude" header = UTM N
        utm_zone <- cols[idx_addr + 2] # "Fuso"
        utm_x    <- cols[idx_addr + 3] # "Longitude" header = UTM E
        
        record <- tibble::tibble(
          station_name = station_name,
          address      = address,
          utm_y        = utm_y,
          utm_zone     = utm_zone,
          utm_x        = utm_x
        )
        
        # Add dynamic params
        record <- dplyr::bind_cols(
          record, 
          tibble::as_tibble(as.list(param_bools))
        )
        
        all_data_list[[length(all_data_list) + 1]] <- record
      }
    }
  }
  
  # Combine all results
  final_df <- dplyr::bind_rows(all_data_list)
  
  # Convert NAs in boolean columns to FALSE 
  # (NA means the station is in a table that didn't even list that pollutant)
  meta_cols <- c("station_name", "address", "utm_y", "utm_zone", "utm_x")
  
  final_df <- final_df |>
    dplyr::mutate(dplyr::across(
      !dplyr::any_of(meta_cols), 
      ~tidyr::replace_na(., FALSE)
    ))
  
  if (!quiet) message(sprintf("✅ Parsed %d stations.", nrow(final_df)))
  
  # 9) Save
  if (!quiet) message("💾 Saving to: ", out_file)
  readr::write_csv(final_df, out_file)
  
  invisible(final_df)
}


# ============================================================================================
#  São Paulo-specific functions - processing main functions
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: sp_filter_stations_in_metro
# @Arg       : stations_sp    — Pre-loaded dataframe (São Paulo Stations)
# @Arg       : metro_area     — sf polygon of the metropolitan area
# @Arg       : radius_km      — numeric; max distance to keep (default 20)
# @Arg       : stations_esri  — ESRI for UTM Zone 23S (default 103213 for UTM 23S)
# @Arg       : out_file       — output GeoPackage path
# @Arg       : overwrite_gpkg — logical; overwrite if exists
# @Arg       : dissolve       — logical; TRUE unions metro polygons
# @Output    : sf POINT data.frame
# @Purpose   : Cleans shifted UTM columns, limits to necessary fields, and spatially filters.
# @Written_on: 19/02/2026
# @Written_by: Marcos
# --------------------------------------------------------------------------------------------
sp_filter_stations_in_metro <- function(
    stations_sp,
    metro_area,
    radius_km      = 20,
    stations_esri  = "ESRI:103213", # Defaulting to UTM Zone 23S
    out_file       = here::here("data", "raw", "geospatial_data", 
                                "sao_paulo", "stations.gpkg"),
    overwrite_gpkg = TRUE,
    dissolve       = TRUE
) {
  
  # 0) Dependency Checks
  if (!inherits(metro_area, "sf")) stop("'metro_area' must be an sf object.")
  
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  message("🔄 Starting São Paulo Data Cleaning...")
  
  # PART I: Clean Data & Fix Column Shifts
  # ---------------------------------------------------------------------------
  # X coordinates in SP are always 6 digits, Y coordinates are always 7 digits
# PART I: Clean Data & Fix Column Shifts
  # ---------------------------------------------------------------------------
  # X coordinates in SP are always 6 digits, Y coordinates are always 7 digits
  df_clean <- stations_sp |>
    distinct() |>
    dplyr::mutate(
      # Coerce to character for regex evaluation
      x_chr = as.character(utm_x),
      y_chr = as.character(utm_y),
      z_chr = as.character(utm_zone),
      
      # Extract True Y (7 digits) - suppressWarnings silences the coercion of "23k" to NA
      true_y = dplyr::coalesce(
        ifelse(grepl("^\\d{7}(\\.\\d+)?$", y_chr), suppressWarnings(as.numeric(y_chr)),
               NA_real_),
        ifelse(grepl("^\\d{7}(\\.\\d+)?$", z_chr), suppressWarnings(as.numeric(z_chr)),
               NA_real_),
        ifelse(grepl("^\\d{7}(\\.\\d+)?$", x_chr), suppressWarnings(as.numeric(x_chr)),
               NA_real_)
      ),
      
      # Extract True X (6 digits)
      true_x = dplyr::coalesce(
        ifelse(grepl("^\\d{6}(\\.\\d+)?$", x_chr), suppressWarnings(as.numeric(x_chr)),
               NA_real_),
        ifelse(grepl("^\\d{6}(\\.\\d+)?$", y_chr), suppressWarnings(as.numeric(y_chr)),
               NA_real_),
        ifelse(grepl("^\\d{6}(\\.\\d+)?$", z_chr), suppressWarnings(as.numeric(z_chr)),
               NA_real_)
      )
    ) |>
    # Drop rows that couldn't be salvaged and keep only the requested columns
    dplyr::filter(!is.na(true_x) & !is.na(true_y)) |>
    dplyr::select(station_name, address, true_x, true_y) |>
    # Clean up the heavily nested \n\t escape characters in the addresses
    dplyr::mutate(
      address = stringr::str_squish(address) 
    ) %>% 
    distinct()
  
  message("  ✅ Cleaned ", nrow(df_clean), " stations. Column shifts resolved.")
  
  # PART II: Spatial Conversion
  # ---------------------------------------------------------------------------
  stations_sf <- sf::st_as_sf(
    df_clean,
    coords = c("true_x", "true_y"),
    crs = stations_esri
  )
  
  message("🔄 Applying Spatial Filter (Radius: ", radius_km, "km)...")
  
  # PART III: Spatial Filter (AEQD Projection for accurate distance)
  # ---------------------------------------------------------------------------
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
  radius_m   <- radius_km * 1000
  
  within_idx <- sf::st_is_within_distance(stations_m, metro_m, dist = radius_m)
  keep_mask  <- lengths(within_idx) > 0
  
  stations_final <- stations_sf[keep_mask, ]
  stations_final <- sf::st_transform(stations_final, crs = "EPSG:4674")
  
  message("    Filter Stats: Input=", nrow(stations_sf), 
          " -> Output=", nrow(stations_final), 
          " (Dropped ", nrow(stations_sf) - nrow(stations_final), ")")
  
  # PART IV: Save
  # ---------------------------------------------------------------------------
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
# Function: sp_process_stations_data_to_parquet
#
# @Arg       : data_folder   — string; folder with .csv/.txt files.
# @Arg       : stations_sf   — sf object; Spatial registry to keep. 
# @Arg       : out_dir       — string; base output directory.
# @Arg       : out_name      — string; name of dataset (default "sp_metro_air").
# @Arg       : years         — int vector; years to filter.
# @Arg       : tz            — string; Olson timezone.
# @Arg       : verbose       — logical; print progress messages?
#
# @Output    : Arrow Dataset connection.
#
# @Purpose   : Ingests raw files from QUALAR/CETESB (São Paulo).
#              1. Surgical header extraction for Name and Code.
#              2. Enforces column sizes by anchoring to the parameter line.
#              3. Safely pivots data, handling Brazilian commas and types.
#              4. Handles flexible dates and the "24:00" hour format.
#              5. Pivots and saves to Partitioned Parquet via DuckDB.
#
# @Written_on: 19/12/2025
# --------------------------------------------------------------------------------------------
sp_process_stations_data_to_parquet <- function(
    data_folder,
    stations_sf,
    out_dir,
    out_name    = "sp_metro_air",
    years       = sao_paulo_cfg$years,
    tz          = "America/Sao_Paulo",
    verbose     = TRUE
) {
  
  # --- HELPER: Normalize Strings for Matching ---
  normalize_key <- function(x) {
    x <- toupper(x)
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    x <- gsub("[^A-Z0-9]", "", x)
    return(x)
  }
  
  # --- HELPER: Pollutant Mapper (São Paulo Specific) ---
  get_sp_param_code <- function(raw_str) {
    x <- toupper(trimws(raw_str))
    if (grepl("^MP10", x)) return("pm10")
    if (grepl("^MP2\\.5", x)) return("pm25")
    if (grepl("^NO\\b|^NO\\(", x)) return("no")
    if (grepl("^NO2", x)) return("no2")
    if (grepl("^NOX", x)) return("nox")
    if (grepl("^O3|^OZONO", x)) return("ozone")
    if (grepl("^PRESS", x)) return("pressure")
    if (grepl("^RADG", x)) return("solar_rad")
    if (grepl("^RADUV", x)) return("uv_rad")
    if (grepl("^SO2", x)) return("so2")
    if (grepl("^TEMP", x)) return("temperature")
    if (grepl("^UR", x)) return("rh")
    if (grepl("^VV", x)) return("wind_speed")
    if (grepl("^DVG", x)) return("wind_dir_global")
    if (grepl("^DV", x)) return("wind_dir")
    if (grepl("^CO\\b|^CO\\(", x)) return("co")
    return(NA_character_)
  }
  
  # 1) Check Dependencies
  req_pkgs <- c("duckdb", "DBI", "arrow", "dplyr", "readr", 
                "stringi", "lubridate", "tidyr")
  for(p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(paste("Package", p, "required."))
    }
  }
  
  # 2) Validate Station Index
  has_name <- "station_name" %in% names(stations_sf)
  if (!inherits(stations_sf, "sf") || !has_name) {
    stop("'stations_sf' must be an sf object with 'station_name'.")
  }
  
  valid_stations <- unique(stations_sf$station_name)
  station_lookup <- setNames(valid_stations, normalize_key(valid_stations))
  
  # 3) Setup DuckDB
  if (verbose) message("⬜️ Starting Unified Engine (DuckDB)...")
  
  dbdir <- tempfile("sp_air_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(dbdir, force = TRUE)
  }, add = TRUE)
  
  DBI::dbExecute(con, "PRAGMA memory_limit='8GB';") 
  
  # Staging Table 
  DBI::dbExecute(con, "CREATE TABLE staging_cetesb (
       datetime TIMESTAMP, 
       station VARCHAR, 
       station_code VARCHAR,
       year INTEGER, 
       param VARCHAR, 
       value DOUBLE
    );")
  
  # 4) Process Files
  raw_files <- list.files(
    data_folder, pattern = "\\.(txt|csv)$", full.names = TRUE
  )
  
  if (length(raw_files) == 0) stop("No data files found in data_folder.")
  if (verbose) {
    message(sprintf("📂 Found %d files. Processing...", length(raw_files)))
  }
  
  count_ingest <- 0
  
  for (f in raw_files) {
    
    # Read raw lines to extract metadata (Check more lines, force Latin-1)
    lines <- readLines(f, warn = FALSE, n = 50, encoding = "latin1")
    delim <- ifelse(any(grepl(";", lines[1:15])), ";", "\t")
    
    # Extract Code and Name (Using dots as wildcards)
    code_line <- grep("C.digo da esta..o", lines, ignore.case = TRUE, 
                      value = TRUE)
    name_line <- grep("Nome da esta..o", lines, ignore.case = TRUE, 
                      value = TRUE)
    
    if (length(code_line) == 0 || length(name_line) == 0) next
    
    # Surgical extraction to avoid greedy regex traps
    file_station_code <- sub(".*esta..o[^0-9a-zA-Z]*", "", code_line[1], 
                             ignore.case = TRUE)
    file_station_code <- trimws(gsub("[:;\t]", " ", file_station_code))
    
    file_station_name <- sub(".*esta..o[^0-9a-zA-Z]*", "", name_line[1], 
                             ignore.case = TRUE)
    file_station_name <- trimws(gsub("[:;\t]", " ", file_station_name))
    
    # Match Station to our spatial registry
    f_key <- normalize_key(file_station_name)
    match_idx <- which(sapply(names(station_lookup), function(k) {
      grepl(k, f_key) || grepl(f_key, k)
    }))
    
    if (length(match_idx) == 0) next # Not inside the buffer, skip
    
    best_match <- names(station_lookup)[
      match_idx[which.max(nchar(names(station_lookup)[match_idx]))]
    ]
    real_station_name <- station_lookup[[best_match]]
    
    # Locate data blocks (Handle spacing variations)
    data_start_idx <- grep("^Data\\s*(;|\\t)\\s*Hora", lines, 
                           ignore.case = TRUE)
    if (length(data_start_idx) == 0) next
    
    # Extract Parameter Names 
    param_line <- lines[data_start_idx[1] + 1]
    param_raw_names <- strsplit(param_line, delim)[[1]]
    
    # Create mapping: Column Index -> Standard Param Name (AS VECTOR)
    param_map <- character()
    for (i in seq_along(param_raw_names)) {
      if (param_raw_names[i] != "") {
        std_code <- get_sp_param_code(param_raw_names[i])
        if (!is.na(std_code)) param_map[as.character(i)] <- std_code
      }
    }
    
    if (length(param_map) == 0) next 
    
    # Read data starting FROM the param line to force correct column counts
    dt <- suppressWarnings(suppressMessages(
      readr::read_delim(f, delim = delim, skip = data_start_idx[1], 
                        col_names = FALSE, na = c("", "NA", "---", "NR"),
                        locale = readr::locale(encoding = "latin1"),
                        show_col_types = FALSE, progress = FALSE)
    ))
    
    if (nrow(dt) <= 1) next 
    
    dt <- dt[-1, ] # Remove the parameter string row we used for sizing
    names(dt) <- paste0("V", seq_len(ncol(dt)))
    
    # Extract only Date, Hour, and the mapped parameter columns
    keep_cols <- c("V1", "V2", paste0("V", names(param_map)))
    keep_cols <- intersect(keep_cols, names(dt))
    
    if(length(keep_cols) <= 2) next 
    
    dt <- dt |> dplyr::select(dplyr::all_of(keep_cols))
    
    # Pivot longer to Melt the data
    dt_long <- dt |>
      dplyr::mutate(dplyr::across(-c(V1, V2), as.character)) |>
      tidyr::pivot_longer(
        cols = -c(V1, V2),
        names_to = "col_idx",
        values_to = "value",
        values_drop_na = TRUE
      ) |>
      dplyr::mutate(
        col_idx = sub("V", "", col_idx),
        param = unname(param_map[col_idx]),
        value = suppressWarnings(as.numeric(gsub(",", ".", value)))
      ) |>
      dplyr::select(Data = V1, Hora = V2, param, value) |>
      dplyr::filter(!is.na(value))
    
    if (nrow(dt_long) == 0) next
    
    # Handle CETESB Dates & 24:00 Hour format
    dt_long <- dt_long |>
      dplyr::mutate(
        is_24 = (Hora == hms("24:00:00")),
        Hora_clean = hour(Hora),
        datetime_str = paste(Data, Hora_clean),
        # Flexible parser for multiple Brazilian date variants (FIXED)
        datetime = lubridate::parse_date_time(
          datetime_str, 
          orders = c("dmy_H"),
          tz = tz, quiet = TRUE
        ),
        datetime = ifelse(
          is_24, datetime + lubridate::days(1), datetime
        )
      ) |>
      dplyr::mutate(
        datetime = as.POSIXct(datetime, origin = "1970-01-01", tz = tz),
        year = lubridate::year(datetime)
      ) |>
      dplyr::filter(!is.na(datetime) & year %in% years)
    
    if (nrow(dt_long) > 0) {
      db_payload <- data.frame(
        datetime     = dt_long$datetime,
        station      = real_station_name,
        station_code = as.character(file_station_code),
        year         = dt_long$year,
        param        = dt_long$param,
        value        = as.numeric(dt_long$value)
      )
      
      duckdb::dbAppendTable(con, "staging_cetesb", db_payload)
      count_ingest <- count_ingest + nrow(db_payload)
      
      # Optional trace to show progress
      if (verbose && count_ingest == nrow(db_payload)) {
        message("  [Trace] First file successfully loaded: ", basename(f))
      }
    }
  }
  
  if (verbose) {
    message("💾 Total rows staged: ", format(count_ingest, big.mark=","))
  }
  if (count_ingest == 0) {
    stop("No data was ingested. Check date formats or filenames.")
  }
  
  # 5) Pivot and Export
  # ----------------------------------------------------------------------------
  dataset_path <- file.path(out_dir, paste0(out_name, "_dataset"))
  if (dir.exists(dataset_path)) unlink(dataset_path, recursive = TRUE)
  
  sql_pivot <- "
    COPY (
      SELECT 
        datetime,
        station,
        station_code,
        year,
        AVG(CASE WHEN param = 'pm10' THEN value END) AS pm10,
        AVG(CASE WHEN param = 'pm25' THEN value END) AS pm25,
        AVG(CASE WHEN param = 'ozone' THEN value END) AS ozone,
        AVG(CASE WHEN param = 'no' THEN value END) AS no,
        AVG(CASE WHEN param = 'no2' THEN value END) AS no2,
        AVG(CASE WHEN param = 'nox' THEN value END) AS nox,
        AVG(CASE WHEN param = 'co' THEN value END) AS co,
        AVG(CASE WHEN param = 'so2' THEN value END) AS so2,
        AVG(CASE WHEN param = 'temperature' THEN value END) AS temperature,
        AVG(CASE WHEN param = 'rh' THEN value END) AS rh,
        AVG(CASE WHEN param = 'wind_speed' THEN value END) AS wind_speed,
        AVG(CASE WHEN param = 'wind_dir' THEN value END) AS wind_dir,
        AVG(CASE WHEN param = 'wind_dir_global' THEN value END) 
          AS wind_dir_global,
        AVG(CASE WHEN param = 'pressure' THEN value END) AS pressure,
        AVG(CASE WHEN param = 'solar_rad' THEN value END) AS solar_rad,
        AVG(CASE WHEN param = 'uv_rad' THEN value END) AS uv_rad
      FROM staging_cetesb
      GROUP BY datetime, station, station_code, year
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
# Function: sp_process_census_2010
#
# @Arg       : sf_data    — sf object; Spatial weighting areas.
# @Arg       : match_col  — string; Column with weighting area codes.
# @Arg       : out_dir    — string; Output folder for processed CSVs.
# @Arg       : quiet      — logical; Suppress messages?
#
# @Output    : list(individual, collapsed); Tibbles with processed data.
#
# @Purpose   : 1. Connects to 2010 Census via censobr (Arrow backend).
#              2. Filters geographically using 'code_weighting' (V0011).
#              3. Harmonizes education variables exactly as per methodology.
#              4. Fixes gender and demographic mappings.
#              5. Collapses using Expansion Weights (V0010) for adults 25+.
#
# @Written_on: 19/02/2026
# --------------------------------------------------------------------------------------------
sp_process_census_2010 <- function(
    sf_data,
    match_col = "code_weighting",
    out_dir   = here::here("data", "processed", "sao_paulo", "census"),
    quiet     = FALSE
) {
  
  # 1. Checks & Setup ----------------------------------------------------------
  req_pkgs <- c("censobr", "dplyr", "arrow", "readr")
  for(p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop(paste("Need", p))
  }
  if (!inherits(sf_data, "sf")) stop("'sf_data' must be an sf object.")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 2. Extract Filter Codes ----------------------------------------------------
  filter_codes <- as.character(unique(sf_data[[match_col]]))
  if (!quiet) message(sprintf("🎯 Target: %d weighting areas.", 
                              length(filter_codes)))
  
  # 3. Connect to censobr ------------------------------------------------------
  if (!quiet) message("🔌 Connecting to 2010 Census data via censobr...")
  
  # Download/Load only the necessary columns into an Arrow dataset
  raw_db <- censobr::read_population(
    year = 2010,
    columns = c("V0011", "V0010", "V1004", "V0601", "V0606", 
                "V0633", "V0634", "V0648", "V6036", "V6400"),
    showProgress = !quiet
  )
  
  # 4. Filter and Harmonize (Lazy Execution) -----------------------------------
  if (!quiet) message("⚙️  Applying harmonization rules...")
  
  processed_db <- raw_db |>
    collect() |>
    dplyr::filter(V1004 == "20") |>
    dplyr::filter(V0011 %in% filter_codes) |>
    dplyr::mutate(
      
      # Expansion Weight & Age
      weight = as.numeric(V0010),
      age    = as.numeric(V6036),
      adult  = dplyr::if_else(age >= 25, 1, 0),
      
      # --- Demographics & Labor (Fixed logic) ---
      # V0601: 1=Male, 2=Female
      women = dplyr::if_else(V0601 == "2", 1, 0),
      
      # V0606: 1=White, 2=Black, 4=Pardo
      white       = dplyr::if_else(V0606 == "1", 1, 0),
      black_pardo = dplyr::if_else(V0606 %in% c("2", "4"), 1, 0),
      
      # Employment (V0648)
      employed     = dplyr::if_else(V0648 %in% as.character(1:6), 1, 0),
      formal_emp   = dplyr::if_else(V0648 %in% c("1", "2", "3", "6"), 1, 0),
      informal_emp = dplyr::if_else(V0648 %in% c("4", "5"), 1, 0),
      
      # --- Education Harmonization ---
      years_schooling = dplyr::case_when(
        V0633 == "01" ~ 0,
        V0633 == "02" & V0634 == "2" ~ 0,
        V0633 == "02" & V0634 == "1" ~ 2,
        V0633 == "03" & V0634 == "2" ~ 0,
        V0633 == "03" & V0634 == "1" ~ 5,
        V0633 == "04" & V0634 == "2" ~ 5,
        V0633 == "04" & V0634 == "1" ~ 9,
        V0633 == "05" & V0634 == "2" ~ 0,
        V0633 == "05" & V0634 == "1" ~ 4,
        V0633 == "05" & is.na(V0634) ~ 2,   # Imputed
        V0633 == "06" & V0634 == "2" ~ 4,
        V0633 == "06" & V0634 == "1" ~ 5,
        V0633 == "06" & is.na(V0634) ~ 4.5, # Imputed
        V0633 == "07" & V0634 == "2" ~ 5,
        V0633 == "07" & V0634 == "1" ~ 9,
        V0633 == "08" & V0634 == "2" ~ 0,
        V0633 == "08" & V0634 == "1" ~ 9,
        V0633 == "09" & V0634 == "2" ~ 9,
        V0633 == "09" & V0634 == "1" ~ 12,
        V0633 == "10" & V0634 == "2" ~ 9,
        V0633 == "10" & V0634 == "1" ~ 12,
        V0633 == "11" & V0634 == "2" ~ 12,
        V0633 == "11" & V0634 == "1" ~ 17,
        V0633 == "12" & V0634 == "2" ~ 17,
        V0633 == "12" & V0634 == "1" ~ 19,
        V0633 == "13" & V0634 == "2" ~ 17,
        V0633 == "13" & V0634 == "1" ~ 19,
        V0633 == "14" & V0634 == "2" ~ 19,
        V0633 == "14" & V0634 == "1" ~ 23,
        TRUE ~ NA_real_
      ),
      
      # Education Dummies
      no_education   = dplyr::if_else(years_schooling == 0, 1, 0),
      hs_incomplete  = dplyr::if_else(years_schooling >= 1 & 
                                        years_schooling <= 11, 1, 0),
      hs_complete    = dplyr::if_else(years_schooling == 12, 1, 0),
      col_incomplete = dplyr::if_else(years_schooling >= 13 & 
                                        years_schooling <= 16, 1, 0),
      col_complete   = dplyr::if_else(years_schooling == 17, 1, 0),
      graduate_educ  = dplyr::if_else(years_schooling >= 18, 1, 0)
    )
  
  # 5. Collect Individual Data -------------------------------------------------
  if (!quiet) message("⬇️  Pulling individual records into memory...")
  
  indiv_df <- processed_db |> 
    dplyr::collect() |> 
    dplyr::rename(code_weighting = V0011)
  
  # 6. Weighted Collapse -------------------------------------------------------
  if (!quiet) message("📉 Collapsing using Expansion Weights (Adults 25+)...")
  
  # Note: Safe weighted summaries using V0010 (weight)
  collapse_df <- indiv_df |>
    dplyr::filter(adult == 1) |>
    dplyr::group_by(code_weighting) |>
    dplyr::summarise(
      total_adult_pop = sum(weight, na.rm = TRUE),
      
      # Weighted means
      avg_escolaridad = sum(years_schooling * weight, na.rm = TRUE) / 
        total_adult_pop,
      
      # Weighted absolute counts
      count_no_ed   = sum(no_education * weight, na.rm = TRUE),
      count_hs_inc  = sum(hs_incomplete * weight, na.rm = TRUE),
      count_hs_com  = sum(hs_complete * weight, na.rm = TRUE),
      count_col_inc = sum(col_incomplete * weight, na.rm = TRUE),
      count_col_com = sum(col_complete * weight, na.rm = TRUE),
      count_grad    = sum(graduate_educ * weight, na.rm = TRUE),
      
      count_employed = sum(employed * weight, na.rm = TRUE),
      count_formal   = sum(formal_emp * weight, na.rm = TRUE),
      count_informal = sum(informal_emp * weight, na.rm = TRUE),
      
      # Proportions
      share_no_ed_pop   = count_no_ed / total_adult_pop,
      share_hs_inc_pop  = count_hs_inc / total_adult_pop,
      share_hs_com_pop  = count_hs_com / total_adult_pop,
      share_col_inc_pop = count_col_inc / total_adult_pop,
      share_col_com_pop = count_col_com / total_adult_pop,
      share_grad_pop    = count_grad / total_adult_pop,
      
      share_employed = count_employed / total_adult_pop,
      share_female   = sum(women * weight, na.rm = TRUE) / total_adult_pop,
      share_black    = sum(black_pardo * weight, na.rm = TRUE) / total_adult_pop
    ) |>
    dplyr::ungroup()
  
  # 7. Save --------------------------------------------------------------------
  if (!quiet) message("💾 Saving outputs...")
  
  readr::write_csv(indiv_df, 
                   file.path(out_dir, "census_sp_individual_2010.csv"))
  readr::write_csv(collapse_df, 
                   file.path(out_dir, "census_sp_collapsed_2010.csv"))
  
  if (!quiet) message("✅ Finished.")
  
  return(list(individual = indiv_df, collapsed = collapse_df))
}

