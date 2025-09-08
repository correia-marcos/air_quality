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
  id               = "Mexico City",
  tz               = "America/Mexico_City",
  base_url_shp     = "https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=794551132173",
  base_url_sinaica = "https://sinaica.inecc.gob.mx/scica/",
  years            = 2000L:2023L,
  dl_dir           = here::here("data", "downloads", "CDMX"),
  out_dir          = here::here("data", "raw"),
  cities_in_metro  = c(09002, 09003, 09004, 09005, 09006, 09007, 09008, 09009, 09010, 09011,
                       09012, 09013, 09014, 09015, 09016, 09017, 13013, 13069, 15002, 15011,
                       15013, 15020, 15022, 15023, 15024, 15025, 15028, 15029, 15030, 15031,
                       15033, 15035, 15037, 15038, 15039, 15044, 15046, 15053, 15057, 15058,
                       15059, 15060, 15069, 15070, 15075, 15081, 15083, 15084, 15089, 15091,
                       15092, 15093, 15095, 15099, 15100, 15103, 15104, 15108, 15109, 15120, 
                       15121, 15122, 15125),
  url_loc_stations_cdmx = 
    "https://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmI=%27&opcion=Zg=="
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
  keep_chr <- sprintf("%05d", keep_municipality)
  keep_chr <- gsub("[^0-9]", "", as.character(keep_chr))
  
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
# Function: cdmx_download_sinaica_data
# @Arg       : base_url      — URL of SINAICA form ("https://sinaica.inecc.gob.mx/scica/")
# @Arg       : years         — integer vector of years to loop (default cdmx_cfg$years)
# @Arg       : container     — logical; TRUE if using docker-compose Selenium
# @Arg       : max_attempts  — retries per (smca, red, param, year)
# @Arg       : timeout_page  — seconds to wait page ready
# @Arg       : timeout_ctrl  — seconds to wait controls visible
# @Arg       : timeout_modal — seconds to wait the first "Aceptar" modal sniff
# @Arg       : timeout_csv   — max seconds waiting results (modal→CSV) after Buscar
# @Arg       : timeout_dl    — seconds to wait each CSV download
# @Arg       : timeout_param_ready — seconds to wait parameter list to (re)populate
# @Arg       : timeout_year_ready  — seconds to wait year list to (re)populate
# @Arg       : timeout_est_ready   — seconds to wait station list to (re)populate
# @Arg       : wd_request_timeout_ms — HTTP timeout per WebDriver call (best effort)
# @Arg       : settle_first_year_sec      — small wait before first-year fetch
# @Arg       : settle_before_csv_click_sec— small wait after results, before CSV click
# @Arg       : subdir        — optional subfolder under DOWNLOADS_DIR for final files
# @Output    : writes CSVs; returns (invisibly) a tibble log:
#              smca, red, parametro, year, status, file
# @Purpose   : SINAICA "Descargas" downloader for CDMX metro area (SMCA→red→param→año).
# @Written_on: 06/09/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
cdmx_download_sinaica_data <- function(
    base_url      = cdmx_cfg$base_url_sinaica,
    years         = cdmx_cfg$years,
    container     = TRUE,
    max_attempts  = 5,
    timeout_page  = 50,
    timeout_ctrl  = 50,
    timeout_modal = 50,
    timeout_csv   = 5,
    timeout_dl    = 600,
    timeout_param_ready = 180,
    timeout_year_ready  = 180,
    timeout_est_ready   = 60,
    wd_request_timeout_ms        = 60000L,  # NEW: best effort (if supported)
    settle_first_year_sec        = 5,       # NEW: was 15; now shorter & tunable
    settle_before_csv_click_sec  = 2,       # NEW: was 20; now wait-for + 2s
    subdir        = "Ground_stations/Mexico/SINAICA"
) {
  # 0) Define the year cap (max requested year)
  end_year_cap <- max(years)
  
  # 1) Guarantee folders exist
  downloads_root <- Sys.getenv("DOWNLOADS_DIR",
                               here::here("data","downloads"))
  dir.create(downloads_root, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Downloads dir: ", downloads_root)
  
  # 2) Resolve target folder (no duplicate top folder)
  is_abs <- function(p) grepl("^(/|[A-Za-z]:[/\\\\])", p)
  normalize_safe <- function(p) {
    out <- try(normalizePath(p, winslash = "/", mustWork = FALSE),
               silent = TRUE)
    if (inherits(out, "try-error")) p else out
  }
  target_dir <- NULL
  if (!is.null(subdir)) {
    subdir_norm <- normalize_safe(subdir)
    if (is_abs(subdir)) target_dir <- subdir_norm
    else                target_dir <- file.path(downloads_root, subdir)
    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    }
    message("📂 Target subdir : ", target_dir)
  }
  
  # 3) Start / connect Selenium (CSV auto-download)
  if (!container) {
    message("🚀 Starting local Selenium on 4445…")
    cid <- system(
      "docker run -d -p 4445:4444 --shm-size=2g "
      %+% "selenium/standalone-firefox:4.34.0-20250717",
      intern = TRUE
    )
    on.exit(try(system(sprintf("docker rm -f %s", cid), intern = TRUE),
                silent = TRUE), add = TRUE)
    sel_host <- "localhost"; sel_port <- 4445L
  } else {
    sel_host <- "selenium";  sel_port <- 4444L
  }
  dl_dir_container <- if (container) "/home/seluser/Downloads" else downloads_root
  
  caps <- list(
    browserName = "firefox",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList"     = 2L,
        "browser.download.dir"            = dl_dir_container,
        "browser.download.useDownloadDir" = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" = paste(
          "text/csv",
          "application/csv",
          "application/vnd.ms-excel",
          "application/octet-stream",
          sep = ","
        )
      )
    ),
    timeouts = list(implicit = 0L, pageLoad = 120000L, script = 60000L)
  )
  
  # Best-effort: pass a driver HTTP timeout only if your build supports it
  ses_args <- list(
    browser      = "firefox",
    host         = sel_host,
    port         = sel_port,
    capabilities = caps
  )
  add_timeout_if_supported <- function(args) {
    fml <- try(names(formals(selenium::SeleniumSession$new)), silent = TRUE)
    if (!inherits(fml, "try-error")) {
      if ("request_timeout" %in% fml) {
        args$request_timeout <- as.integer(wd_request_timeout_ms)
      } else if ("http_timeout_ms" %in% fml) {
        args$http_timeout_ms <- as.integer(wd_request_timeout_ms)
      }
    }
    args
  }
  ses_args <- add_timeout_if_supported(ses_args)
  
  session <- do.call(selenium::SeleniumSession$new, ses_args)
  on.exit(session$close(), add = TRUE)
  
  # 4) JS helpers
  js_set_select <- function(id, value = NULL, text = NULL) {
    val <- if (is.null(value)) "null" else sprintf("'%s'", value)
    txt <- if (is.null(text))  "null" else jsonlite::toJSON(text)
    script <- paste(
      "var sel=document.getElementById('", id, "');",
      "if(!sel) return 'no-sel'; var i=-1;",
      "if(", val, "!==null){",
      " for(var k=0;k<sel.options.length;k++){",
      "  if(sel.options[k].value===", val, "){i=k;break;}",
      " }",
      "} else if(", txt, "!==null){",
      " var want=", txt, ";",
      " for(var k=0;k<sel.options.length;k++){",
      "  if(sel.options[k].text.trim()===want.trim()){i=k;break;}",
      " }",
      "}",
      "if(i<0) return 'not-found';",
      "sel.selectedIndex=i;",
      "sel.dispatchEvent(new Event('change',{bubbles:true}));",
      "sel.scrollIntoView({block:'center'});",
      "return 'ok';",
      sep = ""
    )
    session$execute_script(script)[[1]]
  }
  js_click_text <- function(xpath_expr) {
    sc <- sprintf(
      "var x=document.evaluate(%s,document,null,"
      %+% "XPathResult.FIRST_ORDERED_NODE_TYPE,null);",
      jsonlite::toJSON(xpath_expr)
    )
    sc <- paste0(
      sc,
      "var el=x.singleNodeValue; if(!el) return false;",
      "el.scrollIntoView({block:'center'}); el.click(); return true;"
    )
    isTRUE(session$execute_script(sc)[[1]])
  }
  
  # 5) Page bootstrap helpers
  open_descargas <- function() {
    session$navigate(base_url)
    wait_ready(session, timeout_page)
    ok_tab <- FALSE
    tab_try <- try(wait_for(session, "css selector", "#ui-id-3", timeout_ctrl),
                   silent = TRUE)
    if (!inherits(tab_try, "try-error")) { tab_try$click(); ok_tab <- TRUE }
    else {
      ok_tab <- js_click_text(
        "//a[@href='#descargas' and normalize-space(.)='Descargas']"
      )
    }
    if (!ok_tab) stop("Could not activate 'Descargas' tab.")
    wait_for(session, "css selector", "#SMCASelDesc", timeout_ctrl)
  }
  
  read_select_df_by_id <- function(id) {
    js <- paste0(
      "var s=document.getElementById('", id, "');",
      "if(!s){return '[]';} var out=[];",
      "for(var i=0;i<s.options.length;i++){",
      " var t=s.options[i].text; var v=s.options[i].value;",
      " if(v && t && t.indexOf('- - -')===-1){ out.push([t,v]); }",
      "}",
      "return JSON.stringify(out);"
    )
    raw <- session$execute_script(js)
    if (is.list(raw)) raw <- raw[[1]]
    if (is.null(raw) || identical(raw, "")) {
      return(data.frame(text=character(0), value=character(0),
                        stringsAsFactors = FALSE))
    }
    arr <- jsonlite::fromJSON(raw)
    if (length(arr) == 0) {
      return(data.frame(text=character(0), value=character(0),
                        stringsAsFactors = FALSE))
    }
    df <- as.data.frame(arr, stringsAsFactors = FALSE)
    names(df) <- c("text","value")
    df
  }
  
  wait_options <- function(id, min_n = 1, timeout = 20) {
    t0 <- Sys.time()
    repeat {
      js_n <- paste0(
        "var s=document.getElementById('", id, "');",
        "if(!s) return 0;",
        "var n=s.options.length;",
        "if(n>0 && s.options[0].value==='') n--;",
        "return n;"
      )
      n <- session$execute_script(js_n)
      if (is.list(n)) n <- n[[1]]
      if (is.null(n) || !is.finite(as.numeric(n))) n <- 0L
      n <- as.integer(n)
      if (n >= min_n) break
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) break
      Sys.sleep(1.0)
    }
    read_select_df_by_id(id)
  }
  
  # 6) Control finders and result waiters
  find_red_select <- function() wait_for(session, "css selector",
                                         "#redSelDesc", timeout_ctrl)
  find_est_select <- function() wait_for(session, "css selector",
                                         "#estSelDesc", timeout_ctrl)
  get_year_select <- function() {
    el <- try(session$find_element("css selector", "#yInitDesc"), silent = TRUE)
    if (!inherits(el, "try-error")) return(el)
    session$find_element("xpath",
                         "//*[normalize-space()='Elige un año:']/following::select"
    )
  }
  find_btn_buscar <- function() {
    el <- try(session$find_element("xpath", '//*[@id="buscIndDesc"]'),
              silent = TRUE)
    if (!inherits(el, "try-error")) return(el)
    session$find_element(
      "xpath",
      "//input[( @type='button' or @type='submit') and @value='Buscar']"
    )
  }
  find_btn_csv <- function() {
    els <- list(
      try(session$find_element("xpath", '//*[@id="descargarDesc_CSV"]'),
          silent = TRUE),
      try(session$find_element("css selector", "#btnCSV"), silent = TRUE),
      try(session$find_element("xpath",
                               "//a[contains(normalize-space(.),'CSV')]"),
          silent = TRUE),
      try(session$find_element("xpath",
                               "//button[contains(normalize-space(.),'CSV')]"),
          silent = TRUE)
    )
    for (e in els) if (!inherits(e, "try-error")) return(e)
    stop("CSV control not found after results.")
  }
  
  # Adaptive CSV wait (CDMX is slow; ensure >=45s there)
  wait_results_or_modal <- function(timeout = timeout_csv) {
    t0 <- Sys.time(); accepted <- FALSE
    repeat {
      b <- try(session$find_element("css selector", "#btnMDAceptar"),
               silent = TRUE)
      if (!inherits(b, "try-error")) {
        ok <- try(b$click(), silent = TRUE)
        if (inherits(ok, "try-error")) {
          session$execute_script(
            "var x=document.querySelector('#btnMDAceptar'); if(x){x.click();}"
          )
        }
        accepted <- TRUE
        Sys.sleep(0.8)
      }
      csv_btn <- try(find_btn_csv(), silent = TRUE)
      if (!inherits(csv_btn, "try-error")) return(csv_btn)
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
        stop("Timeout waiting for results (CSV) after ",
             if (accepted) "accepting modal" else "clicking Buscar", ".")
      }
      Sys.sleep(0.5)
    }
  }
  
  # 7) SMCA targets
  smca_targets <- list(
    list(value = "43", text = "EDOMEX : México"),
    list(value = "40", text = "HGO : Hidalgo"),
    list(value = "61", text = "CDMX : Ciudad de México")
  )
  
  # Mapping for filename prefix
  smca_slug_map <- c(
    "EDOMEX : México"         = "mexico_state",
    "HGO : Hidalgo"           = "hidalgo",
    "CDMX : Ciudad de México" = "cmdx"
  )
  slugify <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
    x <- gsub("^_+|_+$", "", x)
    x <- gsub("_+", "_", x)
    x
  }
  next_nonconflicting <- function(path) {
    if (!file.exists(path)) return(path)
    base <- tools::file_path_sans_ext(basename(path))
    ext  <- tools::file_ext(path)
    dirn <- dirname(path)
    k <- 2L
    repeat {
      cand <- file.path(dirn, sprintf("%s_%02d.%s", base, k, ext))
      if (!file.exists(cand)) return(cand)
      k <- k + 1L
    }
  }
  
  # 8) Logging
  log <- list()
  
  # --- Helpers that (re)hydrate the form --------------------------------------
  # Returns TRUE if stations exist and parámetro set; FALSE if no stations.
  rehydrate_form <- function(smca, red_val, par_val, long_pause = TRUE) {
    open_descargas()
    res <- js_set_select("SMCASelDesc", value = smca$value, text = smca$text)
    if (!identical(res, "ok")) stop("SMCA select failed after reload.")
    if (nzchar(red_val)) {
      res <- js_set_select("redSelDesc", value = red_val, text = NULL)
      if (!identical(res, "ok")) stop("Red select failed after reload.")
    }
    # Stations present?
    find_est_select()
    st_df <- wait_options("estSelDesc", min_n = 1, timeout = timeout_est_ready)
    if (nrow(st_df) == 0) {
      message("      🚫 No stations in this red — skipping.")
      return(FALSE)
    }
    # Add all stations
    try(session$find_element("css selector", "#delLstEstDat")$click(),
        silent = TRUE)
    wait_for(session, "css selector", "#addTodasEstDesc", timeout_ctrl)$click()
    
    # Parámetro
    res <- js_set_select("paramSelDesc", value = par_val, text = NULL)
    if (!identical(res, "ok")) {
      ok <- js_click_text(
        paste0(
          "//th[normalize-space()='Selecciona un parámetro:']",
          "/following::select[1]/option[@value=", jsonlite::toJSON(par_val), "]"
        )
      )
      if (!ok) stop("Failed to select parámetro after reload.")
    }
    # Long pause is only used on hard reloads; otherwise we keep it short.
    if (isTRUE(long_pause)) Sys.sleep(45) else Sys.sleep(1.0)
    TRUE
  }
  
  # Wait until years populate; if they stall, nudge the parámetro again
  wait_years_ready <- function(par_val,
                               timeout = timeout_year_ready,
                               poke_every = 8) {
    t0 <- Sys.time(); tries <- 0L
    repeat {
      years_df <- wait_options("yInitDesc", min_n = 1, timeout = 2)
      if (nrow(years_df)) return(years_df)
      tries <- tries + 1L
      if (tries %% poke_every == 0L) {
        js_set_select("paramSelDesc", value = par_val, text = NULL)
        Sys.sleep(0.8)
      }
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
        return(data.frame(text = character(0), value = character(0),
                          stringsAsFactors = FALSE))
      }
      Sys.sleep(0.8)
    }
  }
  # ---------------------------------------------------------------------------
  
  # 9) Main loops (reload page at the START of EACH parámetro)
  for (smca in smca_targets) {
    message("\n🏷️  SMCA: ", smca$text)
    
    # Discover redes once per SMCA
    open_descargas()
    res <- js_set_select("SMCASelDesc", value = smca$value, text = smca$text)
    if (!identical(res, "ok")) stop("Failed to select SMCA: ", smca$text)
    find_red_select()
    redes_df <- wait_options("redSelDesc", min_n = 1, timeout = 60)
    if (nrow(redes_df) == 0) {
      redes_df <- data.frame(text = "Única", value = "", stringsAsFactors = FALSE)
    }
    
    # CDMX tends to be slower → ensure CSV wait >= 45s here
    csv_wait_this_smca <- if (grepl("^CDMX", smca$text)) max(timeout_csv, 45) else timeout_csv
    
    for (ri in seq_len(nrow(redes_df))) {
      red_name <- redes_df$text[ri]
      red_val  <- redes_df$value[ri]
      message("   🌐 Red: ", red_name)
      
      # Fresh page; set SMCA/red; verify stations exist
      open_descargas()
      js_set_select("SMCASelDesc", value = smca$value, text = smca$text)
      if (nzchar(red_val)) js_set_select("redSelDesc", value = red_val, text = NULL)
      
      find_est_select()
      st_df <- wait_options("estSelDesc", min_n = 1, timeout = timeout_est_ready)
      if (nrow(st_df) == 0) {
        message("   🚫 No stations in this red — skipping whole red.")
        next
      }
      
      # Add all stations and read available parámetros
      try(session$find_element("css selector", "#delLstEstDat")$click(),
          silent = TRUE)
      wait_for(session, "css selector", "#addTodasEstDesc", timeout_ctrl)$click()
      params_df <- wait_options("paramSelDesc", min_n = 1,
                                timeout = timeout_param_ready)
      if (nrow(params_df) == 0) {
        message("   ⚠️ Parámetros empty for this red — skipping.")
        next
      }
      
      for (pi in seq_len(nrow(params_df))) {
        par_name <- params_df$text[pi]
        par_val  <- params_df$value[pi]
        par_name <- iconv(par_name, from = "", to = "UTF-8", sub = "byte")
        message("      🧪 Parámetro: ", par_name)
        
        ok_form <- rehydrate_form(smca, red_val, par_val, long_pause = FALSE)
        if (!isTRUE(ok_form)) {
          message("      ⏭️  Skip parámetro due to no stations.")
          next
        }
        
        # Detect available years (robust)
        years_df <- wait_years_ready(par_val, timeout = timeout_year_ready)
        if (nrow(years_df) == 0) {
          message("      ↪︎ Years empty; reloading form and retrying…")
          ok_form <- rehydrate_form(smca, red_val, par_val, long_pause = TRUE)
          if (!isTRUE(ok_form)) {
            message("      ⏭️  Skip parámetro (no stations after reload).")
            next
          }
          years_df <- wait_years_ready(par_val, timeout = timeout_year_ready)
        }
        avail_years <- suppressWarnings(as.integer(years_df$value))
        avail_years <- sort(unique(avail_years[is.finite(avail_years)]))
        if (!length(avail_years)) {
          message("      ⚠️ No available years listed — skipping parámetro.")
          next
        }
        
        min_req <- min(as.integer(years))
        max_req <- max(as.integer(years))
        end_cap <- min(end_year_cap, max_req, max(avail_years))
        years_iter <- avail_years[avail_years >= min_req & avail_years <= end_cap]
        if (!length(years_iter)) years_iter <- avail_years[avail_years <= end_cap]
        if (!length(years_iter)) {
          message("      ⚠️ No matching years after cap — skipping parámetro.")
          next
        }
        message(sprintf("         ↳ Years available here: %s",
                        paste(years_iter, collapse = ", ")))
        
        # ---- Year loop using dynamic list ----
        for (yr in years_iter) {
          attempt <- 0L
          repeat {
            attempt <- attempt + 1L
            message(sprintf("         📥 Año %d (attempt %d/%d)…",
                            yr, attempt, max_attempts))
            newest_path <- NA_character_
            
            res_try <- try({
              # Short settle on first year of this parámetro
              if (yr == min(years_iter) && settle_first_year_sec > 0) {
                Sys.sleep(settle_first_year_sec)
              }
              
              invisible(get_year_select())
              resy <- js_set_select("yInitDesc",
                                    value = as.character(yr),
                                    text  = as.character(yr))
              if (!identical(resy, "ok")) stop("Could not set year ", yr)
              
              before <- list.files(downloads_root, pattern = "\\.csv$",
                                   full.names = TRUE)
              find_btn_buscar()$click()
              
              # Wait for either modal or CSV readiness
              try({
                wait_for(session, "css selector", "#btnMDAceptar",
                         timeout_modal)$click()
              }, silent = TRUE)
              csv_btn <- wait_results_or_modal(timeout = csv_wait_this_smca)
              
              # Small settle then click CSV
              if (settle_before_csv_click_sec > 0)
                Sys.sleep(settle_before_csv_click_sec)
              ok_click <- try(csv_btn$click(), silent = TRUE)
              if (inherits(ok_click, "try-error")) {
                session$execute_script(
                  paste0("var e=arguments[0];",
                         "if(e){e.scrollIntoView({block:'center'});e.click();}"),
                  list(csv_btn$elementId)
                )
              }
              
              # Wait a new CSV to appear in the downloads folder
              src <- wait_for_new_download(
                dir       = downloads_root,
                before    = before,
                pattern   = "\\.csv$",
                quiet_sec = 2,
                timeout   = timeout_dl
              )
              
              # ----- Rename to conflict-free, informative name -----------------
              smca_slug <- smca_slug_map[[smca$text]] %||% slugify(smca$text)
              red_slug  <- slugify(red_name)
              par_slug  <- slugify(par_name)
              new_base  <- sprintf("%s__%s__%s__%d.csv",
                                   smca_slug, red_slug, par_slug, yr)
              dest0     <- if (!is.null(target_dir))
                file.path(target_dir, new_base) else
                  file.path(dirname(src), new_base)
              dest      <- next_nonconflicting(dest0)
              
              # Move/rename
              ok_mv <- try(file.rename(src, dest), silent = TRUE)
              if (inherits(ok_mv, "try-error") || !isTRUE(ok_mv)) {
                file.copy(src, dest, overwrite = TRUE); unlink(src)
              }
              newest_path <- dest
              message("            ✅ ", basename(newest_path))
            }, silent = TRUE)
            
            if (!inherits(res_try, "try-error")) {
              log[[length(log) + 1L]] <- tibble::tibble(
                smca       = smca$text,
                red        = red_name,
                parametro  = par_name,
                year       = yr,
                status     = "ok",
                file       = newest_path
              )
              break
            } else if (attempt < max_attempts) {
              back <- min(45, 6 ^ attempt)
              message(sprintf("            ⚠️  Failed; backoff %ds, retrying…",
                              back))
              Sys.sleep(back)
            } else {
              message("            ❌ Failed after max attempts.")
              log[[length(log) + 1L]] <- tibble::tibble(
                smca      = smca$text,
                red       = red_name,
                parametro = par_name,
                year      = yr,
                status    = "failed",
                file      = NA_character_
              )
              Sys.sleep(2)
              rehydrate_form(smca, red_val, par_val, long_pause = TRUE)
              break
            }
          } # repeat
        }   # years_iter
        # ----------------------------------------------------------------------
      }     # parámetros
    }       # redes
  }         # smca
  
  out <- dplyr::bind_rows(log)
  invisible(out)
}