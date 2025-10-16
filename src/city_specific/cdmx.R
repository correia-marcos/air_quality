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
  which_states     = c("Guerrero", "Hidalgo", "México", "Michoacán", "Morelos", "Querétaro",
                       "Puebla", "Tlaxcala"),
  base_url_shp     = "https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=794551132173",
  base_url_sinaica = "https://sinaica.inecc.gob.mx/scica/",
  base_url_census  = "https://www.inegi.org.mx/programas/ccpv/2020/#microdatos",
  url_loc_stations_others =
    "https://sinaica.inecc.gob.mx/index.php",
  url_loc_stations_cdmx   = 
    "https://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmI=%27&opcion=Zg=="
  )

# ============================================================================================
# CDMX-specific helpers function (most for downloading pollution data from stations)
# ============================================================================================
# ------------------------------------------------------------------------------------------
# Function : .http_retry_get_robust
# @Arg     : url                 — target URL
# @Arg     : ua                  — httr::user_agent(...) object
# @Arg     : timeout_sec         — per-attempt timeout
# @Arg     : verbose             — print progress (TRUE/FALSE)
# @Arg     : allow_insecure_fallback — one last attempt with verify OFF if SSL chain fails
# @Output  : httr response (status 200) or stop() with a helpful message
# @Purpose : Robust GET for Docker + OpenSSL 3:
#            - strict TLS over IPv4
#            - retry with OpenSSL security level 1 (verification ON)
#            - optional insecure try
#            - http:// fallback
# Notes    : The curl/httr option name is **ssl_cipher_list** (not 'ciphers').
#            The OpenSSL string is "DEFAULT@SECLEVEL=1".
# ------------------------------------------------------------------------------------------
.http_retry_get_robust <- function(url, ua, timeout_sec = 30,
                                   verbose = TRUE,
                                   allow_insecure_fallback = TRUE) {
  is_sslish <- function(msg) {
    grepl("SSL|certificate|verify|peer|issuer|CA|handshake|dh key too small",
          msg, ignore.case = TRUE)
  }

  # A) Strict TLS over IPv4 (some gov hosts have flaky IPv6)
  resp <- try(
    httr::RETRY(
      "GET", url, ua,
      httr::config(ipresolve = 1),
      httr::timeout(timeout_sec),
      times = 4, pause_min = 1, pause_cap = 4, terminate_on = c(200),
      quiet = !isTRUE(verbose)
    ),
    silent = TRUE
  )
  if (!inherits(resp, "try-error") && httr::status_code(resp) == 200L) return(resp)
  
  last_msg <- if (inherits(resp, "try-error")) {
    conditionMessage(attr(resp, "condition"))
  } else {
    paste("HTTP", httr::status_code(resp))
  }
  
  # B) OpenSSL 3 policy: lower security level for this request (keep verification ON)
  if (is_sslish(last_msg)) {
    if (isTRUE(verbose)) message("⚠️ SSL policy error (OpenSSL 3). Retrying @SECLEVEL=1…")
    resp2 <- try(
      httr::RETRY(
        "GET", url, ua,
        # IMPORTANT: use 'ssl_cipher_list' (curl option CURLOPT_SSL_CIPHER_LIST)
        httr::config(ipresolve = 1, ssl_cipher_list = "DEFAULT@SECLEVEL=1"),
        httr::timeout(timeout_sec),
        times = 3, pause_min = 1, pause_cap = 3, terminate_on = c(200),
        quiet = !isTRUE(verbose)
      ),
      silent = TRUE
    )
    if (!inherits(resp2, "try-error") && httr::status_code(resp2) == 200L) return(resp2)
    last_msg <- if (inherits(resp2, "try-error")) {
      conditionMessage(attr(resp2, "condition"))
    } else {
      paste("HTTP", httr::status_code(resp2))
    }
  }
  
  # C) Optional: single insecure try (verification OFF) as a last resort
  if (allow_insecure_fallback && is_sslish(last_msg)) {
    if (isTRUE(verbose)) message("⚠️ SSL chain error. Retrying once without verification…")
    resp3 <- try(
      httr::RETRY(
        "GET", url, ua,
        httr::config(ipresolve = 1, ssl_verifypeer = 0L, ssl_verifyhost = 0L),
        httr::timeout(timeout_sec),
        times = 1, terminate_on = c(200), quiet = !isTRUE(verbose)
      ),
      silent = TRUE
    )
    if (!inherits(resp3, "try-error") && httr::status_code(resp3) == 200L) return(resp3)
    last_msg <- if (inherits(resp3, "try-error")) {
      conditionMessage(attr(resp3, "condition"))
    } else {
      paste("HTTP", httr::status_code(resp3))
    }
  }
  
  # D) http:// fallback (warn loudly)
  if (grepl("^https://", url, ignore.case = TRUE)) {
    url_http <- sub("^https://", "http://", url, ignore.case = TRUE)
    resp4 <- try(
      httr::RETRY(
        "GET", url_http, ua,
        httr::config(ipresolve = 1),
        httr::timeout(timeout_sec),
        times = 2, pause_min = 1, pause_cap = 2, terminate_on = c(200),
        quiet = !isTRUE(verbose)
      ),
      silent = TRUE
    )
    if (!inherits(resp4, "try-error") && httr::status_code(resp4) == 200L) {
      warning("Fell back to plain HTTP (no TLS).")
      return(resp4)
    }
    last_msg <- if (inherits(resp4, "try-error")) {
      conditionMessage(attr(resp4, "condition"))
    } else {
      paste("HTTP", httr::status_code(resp4))
    }
  }
  
  stop("HTTP/TLS error while fetching: ", url, "\nLast error: ", last_msg)
}
# ------------------------------------------------------------------------------------------
# Function: selenium_start_session_flat_first
# @Arg       : container             — logical; TRUE when using docker-compose service
# @Arg       : wd_request_timeout_ms — integer; per-request HTTP timeout to the driver
# @Output    : selenium::SeleniumSession
# @Purpose   : Start Selenium with flat (W3C) capabilities only — avoids the
#              "Illegal key values seen in w3c capabilities" 500 error.
# ------------------------------------------------------------------------------------------
selenium_start_session_flat_first <- function(container = TRUE,
                                              wd_request_timeout_ms = 60000L) {
  sel_host <- if (container) "selenium" else "localhost"
  sel_port <- if (container) 4444L      else 4445L
  
  flat_caps <- list(
    browserName = "firefox",
    acceptInsecureCerts = TRUE,
    "moz:firefoxOptions" = list(args = list(), prefs = list()),
    timeouts = list(implicit = 0L, pageLoad = 120000L, script = 60000L)
  )
  
  ses_args <- list(
    browser      = "firefox",
    host         = sel_host,
    port         = sel_port,
    capabilities = flat_caps
  )
  fml <- try(names(formals(selenium::SeleniumSession$new)), silent = TRUE)
  if (!inherits(fml, "try-error")) {
    if ("request_timeout" %in% fml) {
      ses_args$request_timeout <- as.integer(wd_request_timeout_ms)
    } else if ("http_timeout_ms" %in% fml) {
      ses_args$http_timeout_ms <- as.integer(wd_request_timeout_ms)
    }
  }
  do.call(selenium::SeleniumSession$new, ses_args)
}
# --------------------------------------------------------------------------------------------
# Function: sinaica_scroll_into_view_sel
# @Arg       : session — SeleniumSession
# @Arg       : sel     — CSS selector to bring into view
# @Output    : invisible(TRUE)
# @Purpose   : Scroll the first match of `sel` into view (best-effort).
# --------------------------------------------------------------------------------------------
sinaica_scroll_into_view_sel <- function(session, sel) {
  js <- paste0(
    "var s=", jsonlite::toJSON(sel, auto_unbox = TRUE), ";",
    "var e=document.querySelector(s);",
    "if(e && e.scrollIntoView){",
    "  try{e.scrollIntoView({block:'start'});}catch(_){e.scrollIntoView();}",
    "}"
  )
  try(session$execute_script(js), silent = TRUE)
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function: sinaica_wait_state_table
# @Arg       : session  — SeleniumSession
# @Arg       : timeout  — seconds to wait (default 30)
# @Arg       : min_rows — minimal station rows required (default 1)
# @Output    : TRUE if table rows are present; FALSE otherwise
# @Purpose   : After picking a state, wait until the stations table exists. The table can be
#              lazy-rendered near the bottom, so we close overlays, scroll, and re-check.
# --------------------------------------------------------------------------------------------
sinaica_wait_state_table <- function(session, timeout = 30, min_rows = 1) {
  t0 <- Sys.time()
  repeat {
    # Close any lingering dropdown/modal focus
    try(session$execute_script("try{document.activeElement.blur();}catch(_){}"),
        silent = TRUE)
    
    # Count Estación cells (what we actually need to click)
    n <- try(
      session$execute_script(
        paste(
          "var r=document.querySelectorAll(",
          "'table.table.table-striped tbody tr td.tdEst[id^=\"est_\"]');",
          "return r ? r.length : 0;"
        )
      )[[1]],
      silent = TRUE
    )
    n_ok <- !inherits(n, "try-error") && is.finite(n) &&
      suppressWarnings(as.integer(n)) >= min_rows
    if (n_ok) {
      # Bring the first table into view for reliable clicks
      sinaica_scroll_into_view_sel(
        session, "table.table.table-striped tbody tr td.tdEst[id^='est_']"
      )
      return(TRUE)
    }
    
    # Nudge page to trigger lazy rendering
    try(session$execute_script(
      "try{window.scrollBy(0, Math.max(200, window.innerHeight/2));}catch(_){}"
    ), silent = TRUE)
    
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) break
    Sys.sleep(0.35)
  }
  FALSE
}
# ------------------------------------------------------------------------------------------
# Function: sinaica_clean_text
# @Arg       : x — character
# @Output    : trimmed UTF-8 string with collapsed whitespace
# @Purpose   : Normalize text captured from pages for stable comparisons.
# ------------------------------------------------------------------------------------------
sinaica_clean_text <- function(x) {
  x <- iconv(x %||% "", from = "", to = "UTF-8")
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
# ------------------------------------------------------------------------------------------
# Function: sinaica_parse_coords
# @Arg       : txt — like "20.06 N, 99.22 O"
# @Output    : c(lat=..., lon=...)
# @Purpose   : Parse N/S/E/O coordinates to signed decimal degrees.
# ------------------------------------------------------------------------------------------
sinaica_parse_coords <- function(txt) {
  txt <- tolower(sinaica_clean_text(txt))
  m <- regmatches(
    txt,
    regexec("(-?\\d+(?:[\\.,]\\d+)?)\\s*([ns])[^\\d-]*(-?\\d+(?:[\\.,]\\d+)?)\\s*([eow])",
            txt, perl = TRUE)
  )[[1]]
  if (length(m) < 5) return(c(lat = NA_real_, lon = NA_real_))
  lat  <- as.numeric(sub(",", ".", m[2], fixed = TRUE))
  lon  <- as.numeric(sub(",", ".", m[4], fixed = TRUE))
  if (!is.na(lat) && m[3] == "s") lat <- -abs(lat)
  if (!is.na(lon) && m[5] %in% c("o","w")) lon <- -abs(lon)
  c(lat = lat, lon = lon)
}
# ------------------------------------------------------------------------------------------
# Function: sinaica_parse_alt
# @Arg       : txt — like "2240 msnm"
# @Output    : numeric altitude (meters) or NA
# @Purpose   : Extract altitude number from free text.
# ------------------------------------------------------------------------------------------
sinaica_parse_alt <- function(txt) {
  x <- gsub("[^0-9\\.,-]", "", tolower(txt %||% ""))
  x <- sub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
# --------------------------------------------------------------------------------------------
# Function: sinaica_open_state_dropdown
# @Arg       : session  — SeleniumSession
# @Arg       : timeout  — integer; max seconds to wait for items (default 10)
# @Output    : TRUE if the menu is open and items are present; error on timeout
# @Purpose   : Open the “Seleccionar estado” dropdown and wait until <p.selSMCA> items exist.
# --------------------------------------------------------------------------------------------
sinaica_open_state_dropdown <- function(session, timeout = 10) {
  btn <- wait_for(
    session,
    "xpath",
    paste0(
      "//button[contains(@class,'dropdown-toggle') and ",
      "contains(normalize-space(.),'Seleccionar estado')]"
    ),
    timeout = timeout
  )
  try(btn$click(), silent = TRUE)
  
  t0 <- Sys.time()
  repeat {
    n <- try(
      session$execute_script(
        "return document.querySelectorAll('p.selSMCA').length;"
      )[[1]],
      silent = TRUE
    )
    if (!inherits(n, "try-error") && is.finite(n) && n >= 5) return(TRUE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop("Timed out opening the 'Seleccionar estado' dropdown.")
    Sys.sleep(0.25)
  }
}
# --------------------------------------------------------------------------------------------
# Function: sinaica_select_state
# @Arg       : session     — SeleniumSession
# @Arg       : state_name  — visible text ("Hidalgo", "México", …)
# @Output    : TRUE if clicked; FALSE otherwise
# @Purpose   : Click a state row inside the dropdown (accent/space-insensitive; robust).
# @Written_on: 14/09/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
sinaica_select_state <- function(session, state_name) {
  js <- paste0(
    "var want=", jsonlite::toJSON(state_name, auto_unbox = TRUE), ";",
    "if(Array.isArray(want)) want = want[0];",
    "if(want==null) return false;",
    # normalize input: trim, collapse spaces, remove diacritics, lowercase
    "function norm(s){",
    "s=(s||'').toString().normalize('NFD')",
    ".replace(/\\p{Diacritic}/gu,'');",
    "s=s.replace(/\\s+/g,' ').trim().toLowerCase();",
    "return s;",
    "}",
    "want = norm(want);",
    # best-effort: ensure menu is open
    "(function(){",
    "var b=document.querySelector(",
    "\"button.dropdown-toggle.btn-verde-index\"",
    ");",
    "if(b){ try{b.click();}catch(_){}}",
    "})();",
    # search across all duplicated containers
    "var opts=[].slice.call(document.querySelectorAll('p.selSMCA'));",
    "function findBy(pred){",
    "for(var i=0;i<opts.length;i++){",
    "var t=norm(opts[i].textContent);",
    "if(pred(t)){ return opts[i]; }",
    "}",
    "return null;",
    "}",
    "var el = findBy(function(t){ return t===want; });",
    "if(!el) el = findBy(function(t){ return t.startsWith(want); });",
    "if(!el) el = findBy(function(t){ return t.indexOf(want)!==-1; });",
    "if(!el) return false;",
    "try{el.scrollIntoView({block:'center'});}catch(_){ }",
    "try{el.click(); return true;}catch(_){ return false; }"
  )
  isTRUE(session$execute_script(js)[[1]])
}
# ------------------------------------------------------------------------------------------
# Function: sinaica_read_state_table
# @Arg       : session — SeleniumSession
# @Output    : data.frame(est_id, station, red)
# @Purpose   : Read the “Red / Estación” table into a compact data frame.
# ------------------------------------------------------------------------------------------
sinaica_read_state_table <- function(session) {
  js <- "
    var rows=[].slice.call(document.querySelectorAll(
      'table.table.table-striped tbody tr'
    ));
    var out=[];
    for(var i=0;i<rows.length;i++){
      var tdR=rows[i].querySelector('td.tdRed');
      var tdE=rows[i].querySelector('td.tdEst[id^=\"est_\"]');
      if(tdR && tdE){
        out.push([tdE.id.replace('est_',''), tdE.textContent.trim(),
                  tdR.textContent.trim()]);
      }
    }
    return JSON.stringify(out);
  "
  raw <- session$execute_script(js)[[1]]
  arr <- try(jsonlite::fromJSON(raw), silent = TRUE)
  if (inherits(arr, "try-error") || !length(arr)) {
    return(data.frame(est_id=character(0), station=character(0), red=character(0)))
  }
  df <- as.data.frame(arr, stringsAsFactors = FALSE)
  names(df) <- c("est_id","station","red")
  df
}
# ------------------------------------------------------------------------------------------
# Function: sinaica_read_station_detail
# @Arg       : session — SeleniumSession
# @Output    : list with elements kv (named list), url (string), h3 (string)
# @Purpose   : Pull key/value rows, current URL, and the <h3> title from a station page.
# @Notes     : 
# - Restrict to 'table.tbl-est' (ignores the "Red/Estación" list table).
# - Normalize keys: trim, collapse spaces, remove diacritics, lowercase,
#   strip trailing colons. Return JSON to avoid Selenium's object coercion.
# ------------------------------------------------------------------------------------------
sinaica_read_station_detail <- function(session) {
  js <- paste(
    "function norm(s){",
    "  s=(s||'').toString().normalize('NFD')",
    "     .replace(/\\p{Diacritic}/gu,'');",
    "  s=s.replace(/\\s+/g,' ').replace(/[:\\s]+$/,'').trim().toLowerCase();",
    "  return s;",
    "}",
    "var rows=[].slice.call(",
    "  document.querySelectorAll('table.tbl-est tbody tr')",
    ");",
    "var out=[];",
    "for(var i=0;i<rows.length;i++){",
    "  var th=rows[i].querySelector('th');",
    "  var td=rows[i].querySelector('td');",
    "  if(!th||!td) continue;",
    "  var k=norm(th.textContent||'');",
    "  var v=(td.textContent||'').replace(/\\s+/g,' ').trim();",
    "  if(k){ out.push([k, v]); }",
    "}",
    "return JSON.stringify(out);",
    sep = "\n"
  )
  raw_pairs <- session$execute_script(js)[[1]]
  pairs <- try(jsonlite::fromJSON(raw_pairs), silent = TRUE)
  
  # Build named list 'kv' even if duplicates or empty.
  kv <- list()
  if (!inherits(pairs, "try-error") && length(pairs)) {
    # 'pairs' is a 2-col matrix/data.frame: [,1]=key, [,2]=value
    ks <- as.character(pairs[, 1])
    vs <- as.character(pairs[, 2])
    for (i in seq_along(ks)) kv[[ks[i]]] <- vs[i]
  }
  
  url <- session$execute_script("return location.href")[[1]]
  h3  <- session$execute_script(
    "var h=document.querySelector('h3'); return h?(h.textContent||''):'';"
  )[[1]]
  
  list(kv = kv, url = url, h3 = h3)
}
# ------------------------------------------------------------------------------------------
# Function: sinaica_wait_station_detail
# @Arg       : session — SeleniumSession
# @Arg       : timeout — integer; max seconds to wait for items (default 25)
# @Output    : FALSE
# @Purpose   : Stronger wait for the station detail panel
# - Waits for the specific details table to be present and visible.
# - Also checks that at least one of 'coordenadas' or 'altitud' exists.
# ------------------------------------------------------------------------------------------
sinaica_wait_station_detail <- function(session, timeout = 25) {
  t0 <- Sys.time()
  repeat {
    ok_tbl <- try(
      session$execute_script(
        paste(
          "var t=document.querySelector('table.tbl-est');",
          "if(!t) return false;",
          "var s=window.getComputedStyle(t);",
          "return s && s.display!=='none' && s.visibility!=='hidden';"
        )
      )[[1]],
      silent = TRUE
    )
    if (!inherits(ok_tbl, "try-error") && isTRUE(ok_tbl)) {
      # Peek keys quickly to ensure meaningful rows are there
      js_has <- paste(
        "function has(k){",
        "  var rows=document.querySelectorAll('table.tbl-est tbody tr');",
        "  for(var i=0;i<rows.length;i++){",
        "    var th=rows[i].querySelector('th');",
        "    if(!th) continue;",
        "    var s=(th.textContent||'').toString().normalize('NFD')",
        "      .replace(/\\p{Diacritic}/gu,'').toLowerCase();",
        "    s=s.replace(/\\s+/g,' ').replace(/[:\\s]+$/,'').trim();",
        "    if(s===k) return true;",
        "  }",
        "  return false;",
        "}",
        "return has('coordenadas') || has('altitud');",
        sep = "\n"
      )
      has_any <- try(session$execute_script(js_has)[[1]], silent = TRUE)
      if (!inherits(has_any, "try-error") && isTRUE(has_any)) return(TRUE)
    }
    
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) break
    Sys.sleep(0.25)
  }
  FALSE
}
# ------------------------------------------------------------------------------------------
# Function: sinaica_extract_name_code_from_h3
# @Arg       : h3_txt — e.g., "Estación: Centro de Salud (ATI)"
# @Output    : list(name=<station name>, code=<3–6 letters> or NA)
# @Purpose   : Extract final station display name and CODE from the title.
# ------------------------------------------------------------------------------------------
sinaica_extract_name_code_from_h3 <- function(h3_txt) {
  x <- sinaica_clean_text(h3_txt)
  m <- regmatches(
    x, regexec("(?i)estaci[oó]n:\\s*(.+?)\\s*\\((\\w{2,6})\\)", x, perl = TRUE)
  )[[1]]
  if (length(m) >= 3) {
    return(list(name = sinaica_clean_text(m[2]), code = toupper(sinaica_clean_text(m[3]))))
  }
  list(name = NA_character_, code = NA_character_)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_slugify
# @Arg     : x — character; any label to convert to a safe file-friendly slug
# @Output  : slug (lowercase, ASCII, underscores, no dup underscores)
# @Purpose : Build safe filenames from SMCA/red/param names.
# --------------------------------------------------------------------------------------------
sinaica_slugify <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  gsub("_+", "_", x)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_next_nonconflicting
# @Arg     : path — full path to a file that may already exist
# @Output  : path itself if free, otherwise "name_02.ext", "name_03.ext", ...
# @Purpose : Prevent accidental overwrites on repeated downloads.
# --------------------------------------------------------------------------------------------
sinaica_next_nonconflicting <- function(path) {
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
# --------------------------------------------------------------------------------------------
# Function : sinaica_open_descargas
# @Arg     : session      — SeleniumSession
# @Arg     : base_url     — SINAICA base
# @Arg     : timeout_page — seconds to wait for on-load readiness
# @Arg     : timeout_ctrl — seconds to wait for the Descargas tab controls
# @Output  : TRUE (or error)
# @Purpose : Navigate to page and activate the "Descargas" tab.
# Note     : Relies on your existing wait_ready() and wait_for() utilities.
# --------------------------------------------------------------------------------------------
sinaica_open_descargas <- function(session, base_url, timeout_page, timeout_ctrl) {
  session$navigate(base_url)
  wait_ready(session, timeout_page)
  ok_tab <- FALSE
  tab_try <- try(wait_for(session, "css selector", "#ui-id-3", timeout_ctrl),
                 silent = TRUE)
  if (!inherits(tab_try, "try-error")) { tab_try$click(); ok_tab <- TRUE } else {
    ok_tab <- sinaica_js_click_text(
      session,
      "//a[@href='#descargas' and normalize-space(.)='Descargas']"
    )
  }
  if (!ok_tab) stop("Could not activate 'Descargas' tab.")
  wait_for(session, "css selector", "#SMCASelDesc", timeout_ctrl)
  TRUE
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_js_set_select
# @Arg     : session — SeleniumSession
# @Arg     : id      — select id
# @Arg     : value   — exact option value to set (preferred)
# @Arg     : text    — fallback exact visible text to match
# @Output  : "ok" | "not-found" | "no-sel"
# @Purpose : Robustly set a <select> value and fire change event.
# --------------------------------------------------------------------------------------------
sinaica_js_set_select <- function(session, id, value = NULL, text = NULL) {
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
    "sel.dispatchEvent(new Event('input',{bubbles:true}));",
    "sel.dispatchEvent(new Event('change',{bubbles:true}));",
    "sel.scrollIntoView({block:'center'});",
    "return 'ok';",
    sep = ""
  )
  session$execute_script(script)[[1]]
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_js_click_text
# @Arg     : session — SeleniumSession
# @Arg     : xpath   — xpath expression to locate element
# @Output  : TRUE if clicked; FALSE otherwise
# @Purpose : Click an element found by XPath (scrolls into view first).
# --------------------------------------------------------------------------------------------
sinaica_js_click_text <- function(session, xpath) {
  sc <- sprintf(
    "var x=document.evaluate(%s,document,null," %s%
      "XPathResult.FIRST_ORDERED_NODE_TYPE,null);",
    jsonlite::toJSON(xpath), ""
  )
  sc <- paste0(
    sc,
    "var el=x.singleNodeValue; if(!el) return false;",
    "el.scrollIntoView({block:'center'}); el.click(); return true;"
  )
  isTRUE(session$execute_script(sc)[[1]])
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_read_select_df_by_id
# @Arg     : session — SeleniumSession
# @Arg     : id      — select id
# @Output  : data.frame(text, value)
# @Purpose : Read <select> options (skip the dashed placeholder).
# --------------------------------------------------------------------------------------------
sinaica_read_select_df_by_id <- function(session, id) {
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
    return(data.frame(text = character(0), value = character(0)))
  }
  arr <- jsonlite::fromJSON(raw)
  if (!length(arr)) return(data.frame(text = character(0), value = character(0)))
  df <- as.data.frame(arr, stringsAsFactors = FALSE)
  names(df) <- c("text","value")
  df
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_wait_options
# @Arg     : session — SeleniumSession
# @Arg     : id      — select id
# @Arg     : min_n   — minimum options needed (excludes placeholder)
# @Arg     : timeout — seconds to wait
# @Output  : data.frame(text, value)
# @Purpose : Wait until a <select> has at least min_n options, then return them.
# --------------------------------------------------------------------------------------------
sinaica_wait_options <- function(session, id, min_n = 1, timeout = 20) {
  t0 <- Sys.time()
  repeat {
    js_n <- paste0(
      "var s=document.getElementById('", id, "');",
      "if(!s) return 0;",
      "var n=s.options.length;",
      "if(n>0 && s.options[0].value==='') n--;",
      "return n;"
    )
    n <- session$execute_script(js_n)[[1]]
    n <- suppressWarnings(as.integer(n %||% 0L))
    if (n >= min_n) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) break
    Sys.sleep(1.0)
  }
  sinaica_read_select_df_by_id(session, id)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_wait_years_ready
# @Arg     : session  — SeleniumSession
# @Arg     : par_val  — value selected in parámetro <select>
# @Arg     : timeout  — seconds to wait for the year list to appear
# @Arg     : poke_every — re-select parámetro every k tries
# @Output  : data.frame(text, value) of years (possibly empty)
# @Purpose : Wait until "Elige un año:" is populated; gently poke parámetro if stuck.
# --------------------------------------------------------------------------------------------
sinaica_wait_years_ready <- function(session, par_val,
                                     timeout = 180, poke_every = 8) {
  t0 <- Sys.time(); tries <- 0L
  repeat {
    years_df <- sinaica_wait_options(session, "yInitDesc", min_n = 1, timeout = 2)
    if (nrow(years_df)) return(years_df)
    tries <- tries + 1L
    if (tries %% poke_every == 0L) {
      sinaica_js_set_select(session, "paramSelDesc", value = par_val)
      Sys.sleep(0.8)
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
      return(data.frame(text = character(0), value = character(0)))
    }
    Sys.sleep(0.8)
  }
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_click_el
# @Arg     : session     — SeleniumSession
# @Arg     : el          — element to click
# @Output  : TRUE or error on timeout
# @Purpose : Tries a native click. It doesn't work, make a click in a real object
# --------------------------------------------------------------------------------------------
sinaica_click_el <- function(session, el) {
  # Tries native click, then JS click using the real element object.
  ok <- try(el$click(), silent = TRUE)
  if (!inherits(ok, "try-error")) return(invisible(TRUE))
  session$execute_script(
    "var e = arguments[0];
     if (e && e.scrollIntoView) e.scrollIntoView({block:'center'});
     if (e && e.click) e.click();",
    list(el)  # IMPORTANT: pass the element, NOT el$elementId
  )
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_get_select_value
# @Arg     : session — SeleniumSession
# @Arg     : id      — select id
# @Output  : list(value, text) for the current selection (or NULLs)
# @Purpose : Read back which option is selected now.
# --------------------------------------------------------------------------------------------
sinaica_get_select_value <- function(session, id) {
  sc <- paste0(
    "var s=document.getElementById('", id, "');",
    "if(!s) return JSON.stringify({value:null,text:null});",
    "var i=s.selectedIndex; if(i<0) i=0;",
    "var v=s.options[i] ? s.options[i].value : null;",
    "var t=s.options[i] ? s.options[i].text  : null;",
    "return JSON.stringify({value:v,text:t});"
  )
  out <- session$execute_script(sc)[[1]]
  jsonlite::fromJSON(out)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_force_select_value
# @Arg     : session — SeleniumSession
# @Arg     : id      — select id
# @Arg     : value   — target value (string)
# @Output  : "ok" | "not-found" | "no-sel"
# @Purpose : If target is already selected, flip to a different option first to
#            ensure the change event fires, then set back to target.
# --------------------------------------------------------------------------------------------
sinaica_force_select_value <- function(session, id, value) {
  js <- paste0(
    "var s = document.getElementById('", id, "');",
    "if (!s) return 'no-sel';",
    # // ensure enabled
    "try { s.disabled = false; } catch(e) {}",
    "function fire(el){",
    "  el.dispatchEvent(new Event('input',{bubbles:true}));",
    "  el.dispatchEvent(new Event('change',{bubbles:true}));",
    "}",
    # // pick an alternate option first to guarantee a real change event
    "var target = '", value, "';",
    "var cur = s.value || null;",
    "if (cur === target) {",
    "  for (var k=0;k<s.options.length;k++){",
    "    var v=s.options[k].value;",
    "    if (v && v !== target){ s.selectedIndex=k; fire(s); break; }",
    "  }",
    "}",
    # // now set the real target
    "var ok = false;",
    "for (var k=0;k<s.options.length;k++){",
    "  if (s.options[k].value === target){ s.selectedIndex=k; fire(s); ok=true; break; }",
    "}",
    "return ok ? 'ok' : 'not-found';"
  )
  session$execute_script(js)[[1]]
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_set_year_robust
# @Arg     : session  — SeleniumSession
# @Arg     : yr       — numeric/integer year to select
# @Arg     : par_val  — parámetro value (used when re-poking the list)
# @Arg     : timeout_year_ready — seconds to wait for the year list to load
# @Output  : TRUE (or error)
# @Purpose : Reliable "Elige un año:" selection with verification.
# --------------------------------------------------------------------------------------------
sinaica_set_year_robust <- function(session, yr, par_val, timeout_year_ready = 180) {
  years_df <- sinaica_wait_years_ready(
    session, par_val, timeout = timeout_year_ready
  )
  if (!nrow(years_df)) stop("Year list did not populate.")
  yr_chr <- as.character(yr)
  if (!any(years_df$value == yr_chr)) {
    stop("Target year ", yr_chr, " not listed for this parámetro/red/SMCA.")
  }
  # main attempt
  res <- sinaica_force_select_value(session, "yInitDesc", yr_chr)
  if (!identical(res, "ok")) {
    # as a fallback, click the <option> node and then fire change on the select
    ok <- sinaica_js_click_text(
      session, sprintf("//select[@id='yInitDesc']/option[@value='%s']", yr_chr)
    )
    if (isTRUE(ok)) {
      session$execute_script(
        "var s=document.getElementById('yInitDesc');
         if(s){
           s.dispatchEvent(new Event('input',{bubbles:true}));
           s.dispatchEvent(new Event('change',{bubbles:true}));
         }"
      )
    } else {
      stop("Could not set year (neither force nor option click worked): ", yr_chr)
    }
  }
  # verify it stuck
  sel <- sinaica_get_select_value(session, "yInitDesc")
  if (!identical(sel$value, yr_chr)) {
    stop("Year did not stick (wanted ", yr_chr, ", got ", sel$value %||% "NULL", ").")
  }
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_find_btns
# @Arg     : session — SeleniumSession
# @Output  : list(buscar = el, csv = el or NULL)
# @Purpose : Locate "Buscar" button and any visible CSV download button(s).
# --------------------------------------------------------------------------------------------
sinaica_find_btns <- function(session) {
  # Buscar: try id first, then generic <input value="Buscar">, both guarded.
  buscar <- NULL
  e1 <- try(session$find_element("css selector", "#buscIndDesc"), silent = TRUE)
  if (!inherits(e1, "try-error")) buscar <- e1 else {
    e2 <- try(
      session$find_element(
        "xpath",
        paste0("//input[( @type='button' or @type='submit') and ",
               "normalize-space(@value)='Buscar']")
      ),
      silent = TRUE
    )
    if (!inherits(e2, "try-error")) buscar <- e2
  }
  
  # CSV candidates (same as before)
  csv_candidates <- list(
    try(session$find_element("css selector", "#descargarDesc_CSV"), silent = TRUE),
    try(session$find_element("css selector", "#btnCSV"),            silent = TRUE),
    try(session$find_element("xpath",
                             "//a[contains(normalize-space(.),'CSV')]"),
        silent = TRUE),
    try(session$find_element("xpath",
                             "//button[contains(normalize-space(.),'CSV')]"),
        silent = TRUE)
  )
  csv <- NULL
  for (e in csv_candidates) if (!inherits(e, "try-error")) { csv <- e; break }
  
  # Best-effort href (optional)
  csv_href <- NULL
  if (!is.null(csv)) {
    csv_href <- try(
      session$execute_script(
        "return arguments[0] && arguments[0].getAttribute
         ? arguments[0].getAttribute('href') : null;", list(csv)
      )[[1]],
      silent = TRUE
    )
    if (inherits(csv_href, "try-error")) csv_href <- NULL
  }
  
  list(buscar = buscar, csv = csv, href = csv_href)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_accept_modal_if_any
# @Arg     : session — SeleniumSession
# @Output  : TRUE
# @Purpose : Click the "Aceptar" modal button if it is present right now.
# --------------------------------------------------------------------------------------------
sinaica_accept_modal_if_any <- function(session) {
  try({
    b <- session$find_element("css selector", "#btnMDAceptar")
    if (!inherits(b, "try-error")) {
      try(b$click(), silent = TRUE)
      if (inherits(try(b$click(), silent = TRUE), "try-error")) {
        session$execute_script(
          "var x=document.querySelector('#btnMDAceptar'); if(x){x.click();}"
        )
      }
      Sys.sleep(0.6)
    }
  }, silent = TRUE)
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_wait_results_or_modal
# @Arg     : session — SeleniumSession
# @Arg     : timeout — seconds to wait for CSV controls after Buscar
# @Output  : WebElement of the CSV button (or error on timeout)
# @Purpose : Wait for results; auto-accept modal if it appears.
# --------------------------------------------------------------------------------------------
sinaica_wait_results_or_modal <- function(session, timeout = 5) {
  t0 <- Sys.time()
  repeat {
    sinaica_accept_modal_if_any(session)
    btns <- sinaica_find_btns(session)
    if (!is.null(btns$csv)) return(btns$csv)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
      stop("Timeout waiting for results (CSV).")
    }
    Sys.sleep(0.5)
  }
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_wait_csv_or_nodata
# @Arg     : session  — SeleniumSession
# @Arg     : timeout  — seconds to wait overall (modal or CSV)
# @Output  : list(type = "csv"/"nodata", btn = <WebElement or NULL>)
# @Purpose : After "Buscar", wait until either the CSV button is visible or a definitive
#            "No hay datos" modal shows up. Uses only selector-based JS (no :visible, no
#            element handles passed into JS) to avoid stale-element and style() errors.
# --------------------------------------------------------------------------------------------
sinaica_wait_csv_or_nodata <- function(session, timeout = 30) {
  t0 <- Sys.time()
  
  # --- tiny utilities (selector-based; robust in headless/remote) -------------------------
  
  js_is_visible <- function(sel) {
    out <- try({
      session$execute_script(
        "var sel=arguments[0];
         var e=document.querySelector(sel);
         if(!e) return false;
         var s=window.getComputedStyle(e);
         if(!s) return false;
         var r=e.getBoundingClientRect();
         return s.display!=='none' && s.visibility!=='hidden' &&
                s.opacity!=='0' && r.width>0 && r.height>0;", list(sel)
      )[[1]]
    }, silent = TRUE)
    isTRUE(out)
  }
  
  js_click_if_visible <- function(sel) {
    ok <- try({
      session$execute_script(
        "var sel=arguments[0];
         var e=document.querySelector(sel);
         if(!e) return false;
         var s=window.getComputedStyle(e);
         if(!s) return false;
         var r=e.getBoundingClientRect();
         var vis = s.display!=='none' && s.visibility!=='hidden' &&
                   s.opacity!=='0' && r.width>0 && r.height>0;
         if(!vis) return false;
         try{ if(e.scrollIntoView) e.scrollIntoView({block:'center'}); }catch(_){}
         try{ e.click(); return true; }catch(_){}
         return false;", list(sel)
      )[[1]]
    }, silent = TRUE)
    isTRUE(ok)
  }
  
  js_modal_text_near <- function(trigger_sel) {
    out <- try({
      session$execute_script(
        "var sel=arguments[0];
         var b=document.querySelector(sel);
         if(!b) return '';
         var root=b.closest('.modal,.ui-dialog,.ui-widget,.modal-dialog')||b;
         var t=(root && root.textContent) ? root.textContent : '';
         return t;", list(trigger_sel)
      )[[1]]
    }, silent = TRUE)
    tolower(paste(out %||% "", collapse = " "))
  }
  
  # CSV candidates we accept as “results ready”
  csv_sels <- c("#descargarDesc_CSV", "#btnCSV",
                "a:contains('CSV')", "button:contains('CSV')")
  # Note: the :contains() is not real CSS. We’ll handle the text search via XPath below.
  
  # --- main wait loop ---------------------------------------------------------------------
  
  repeat {
    # 1) Long-wait modal → click “Aceptar” (may appear before any data/no-data state)
    if (js_is_visible("#btnMDAceptar")) {
      js_click_if_visible("#btnMDAceptar")
      Sys.sleep(0.25)  # small settle, then loop again
    }
    
    # 2) “No hay datos” modal → click and return fast
    if (js_is_visible("#envOkModal")) {
      txt <- js_modal_text_near("#envOkModal")
      if (grepl("\\bno\\s*hay\\s*datos\\b|\\bsin\\s*datos\\b", txt, perl = TRUE)) {
        js_click_if_visible("#envOkModal")
        Sys.sleep(0.2)
        return(list(type = "nodata", btn = NULL))
      } else {
        # some other alert → accept and continue
        js_click_if_visible("#envOkModal")
        Sys.sleep(0.2)
      }
    }
    
    # 3) CSV button present? Try strict IDs first (fast path)
    if (js_is_visible("#descargarDesc_CSV") || js_is_visible("#btnCSV")) {
      # Return a *real* WebElement for the caller
      btn <- try(session$find_element("css selector", "#descargarDesc_CSV"),
                 silent = TRUE)
      if (inherits(btn, "try-error"))
        btn <- try(session$find_element("css selector", "#btnCSV"),
                   silent = TRUE)
      if (!inherits(btn, "try-error")) return(list(type = "csv", btn = btn))
    }
    
    # 4) Fallback: look for any visible element containing “CSV” text
    #    (use XPath contains(text()) to avoid jQuery-only selectors)
    #    Try <a> and <button>.
    btn <- try(session$find_element(
      "xpath", "//a[contains(normalize-space(.),'CSV') or contains(@id,'CSV')]"
    ), silent = TRUE)
    if (inherits(btn, "try-error")) {
      btn <- try(session$find_element(
        "xpath", "//button[contains(normalize-space(.),'CSV') or contains(@id,'CSV')]"
      ), silent = TRUE)
    }
    if (!inherits(btn, "try-error")) {
      # We can’t check visibility in JS with this element (stale risk). Instead, click
      # via Selenium (will no-op if hidden), and if it’s truly there, next stage will
      # observe the download. If you prefer, you can attempt a quick JS visibility
      # check by re-querying with a selector rather than passing the element.
      return(list(type = "csv", btn = btn))
    }
    
    # 5) Timeout guard
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
      stop("Timeout waiting for CSV or a definitive 'No hay datos' alert.")
    }
    Sys.sleep(0.35)
  }
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_quick_csv_download
# @Arg     : session     — SeleniumSession
# @Arg     : downloads_root — path to watch for new .csv
# @Arg     : before      — vector of files that existed before
# @Arg     : settle_sec  — small pause before clicking CSV (default 2s)
# @Arg     : timeout_dl  — seconds to wait for the actual download
# @Output  : path to the downloaded file, or NULL if not available quickly
# @Purpose : If a retry left results visible, finish immediately without full
#            re-hydration. Non-blocking probe with a short wait budget.
# --------------------------------------------------------------------------------------------
sinaica_quick_csv_download <- function(session, downloads_root, before,
                                       settle_sec = 1.0,
                                       timeout_dl_quick = 25) {
  btns <- sinaica_find_btns(session)
  if (is.null(btns$csv)) return(NULL)
  
  if (settle_sec > 0) Sys.sleep(settle_sec)
  sinaica_download_via_btn(session, btns$csv)
  
  # SHORT wait only; do not burn the whole attempt here.
  src <- try(
    wait_for_new_download(
      dir       = downloads_root,
      before    = before,
      pattern   = "\\.csv$",
      quiet_sec = 2,
      timeout   = timeout_dl_quick
    ),
    silent = TRUE
  )
  if (inherits(src, "try-error")) return(NULL)
  src
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_rehydrate_form
# @Arg     : session        — SeleniumSession
# @Arg     : base_url       — SINAICA base
# @Arg     : smca           — list(value, text) for the SMCA select
# @Arg     : red_val        — selected red value (may be "")
# @Arg     : par_val        — parámetro value
# @Arg     : timeout_page   — seconds for page ready
# @Arg     : timeout_ctrl   — seconds for controls visible
# @Arg     : timeout_est    — seconds for stations list to populate
# @Arg     : long_pause     — if TRUE add a long settle pause at the end
# @Output  : TRUE if stations exist and parámetro set; FALSE if no stations
# @Purpose : Open Descargas, set SMCA/red, add all stations, set parámetro.
# --------------------------------------------------------------------------------------------
sinaica_rehydrate_form <- function(session, base_url,
                                   smca, red_val, par_val,
                                   timeout_page, timeout_ctrl, timeout_est,
                                   long_pause = TRUE) {
  sinaica_open_descargas(session, base_url, timeout_page, timeout_ctrl)
  res <- sinaica_js_set_select(session, "SMCASelDesc",
                               value = smca$value, text = smca$text)
  if (!identical(res, "ok")) stop("SMCA select failed after reload.")
  if (nzchar(red_val)) {
    res <- sinaica_js_set_select(session, "redSelDesc", value = red_val)
    if (!identical(res, "ok")) stop("Red select failed after reload.")
  }
  wait_for(session, "css selector", "#estSelDesc", timeout_ctrl)
  st_df <- sinaica_wait_options(session, "estSelDesc", min_n = 1,
                                timeout = timeout_est)
  if (!nrow(st_df)) {
    message("      🚫 No stations in this red — skipping.")
    return(FALSE)
  }
  try(session$find_element("css selector", "#delLstEstDat")$click(),
      silent = TRUE)
  wait_for(session, "css selector", "#addTodasEstDesc", timeout_ctrl)$click()
  res <- sinaica_js_set_select(session, "paramSelDesc", value = par_val)
  if (!identical(res, "ok")) {
    ok <- sinaica_js_click_text(
      session,
      paste0("//th[normalize-space()='Selecciona un parámetro:']",
             "/following::select[1]/option[@value=",
             jsonlite::toJSON(par_val), "]")
    )
    if (!ok) stop("Failed to select parámetro after reload.")
  }
  if (isTRUE(long_pause)) Sys.sleep(45) else Sys.sleep(1.0)
  TRUE
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_download_via_btn
# @Arg     : session         — SeleniumSession
# @Arg     : btn         — Button to click on the url
# @Output  : TRUE if succeeded - None otherwise
# @Purpose : Download data through the href if present; otherwise click the element.
# --------------------------------------------------------------------------------------------
sinaica_download_via_btn <- function(session, btn) {
  href <- NULL
  try({
    href <- session$execute_script(
      paste0("return arguments[0] && arguments[0].getAttribute",
             " ? arguments[0].getAttribute('href') : null;"),
      list(btn)
    )[[1]]
  }, silent = TRUE)
  
  if (is.character(href) && nzchar(href)) {
    # Direct navigation triggers the file immediately if server sends Content-Disposition.
    session$navigate(href)
  } else {
    # Fall back to a robust click on the real element object.
    sinaica_click_el(session, btn)
  }
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_download_with_reclick
# @Arg     : session        — SeleniumSession
# @Arg     : downloads_root — directory we are watching for the CSV
# @Arg     : before         — vector of files that existed before clicking
# @Arg     : timeout_total  — hard cap in seconds (e.g., 60–90)
# @Arg     : click_every    — seconds between retries
# @Output  : path to the downloaded file (string)
# @Purpose : Keep (re)clicking the CSV button until a new file appears, or time out.
# --------------------------------------------------------------------------------------------
sinaica_download_with_reclick <- function(session, downloads_root, before,
                                          timeout_total = 75, click_every = 4) {
  t_end <- Sys.time() + timeout_total
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    # (Re)locate the CSV button each attempt to avoid stale references
    btns <- sinaica_find_btns(session)
    if (is.null(btns$csv)) {
      # sometimes the modal is back—accept if present and try again
      sinaica_accept_modal_if_any(session)
      btns <- sinaica_find_btns(session)
    }
    if (is.null(btns$csv)) {
      # If we still can't see it, give the page a tiny chance and try again.
      if (Sys.time() > t_end) stop("CSV button not present before timeout.")
      Sys.sleep(1.0)
      next
    }
    
    msg <- sprintf("            …Clicking CSV (attempt %d)", attempt)
    message(msg)
    ok_click <- try(btns$csv$click(), silent = TRUE)
    if (inherits(ok_click, "try-error")) {
      # fall back to JS click on the real element
      sinaica_click_el(session, btns$csv)
    }
    
    # Short polling window after each click (don’t block for long)
    got <- try(
      wait_for_new_download(
        dir       = downloads_root,
        before    = before,
        pattern   = "\\.csv$",
        quiet_sec = 2,
        timeout   = min(6, as.numeric(difftime(t_end, Sys.time(), units = "secs")))
      ),
      silent = TRUE
    )
    if (!inherits(got, "try-error")) return(got)
    
    # Not yet — wait a bit then re-click
    if (Sys.time() > t_end) stop("No download detected before timeout.")
    Sys.sleep(click_every)
  }
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_cleanup_downloads
# @Arg     : dir       — directory to tidy (e.g., downloads_root)
# @Arg     : grace_sec — only delete files older than this many seconds
# @Arg     : patterns  — filename regex patterns to remove
# @Output  : integer number of files deleted
# @Purpose : Remove stale CSVs and temporary browser artifacts left by failed clicks.
# Note     : Non-recursive on purpose: we do NOT touch subdirectories (e.g., your subdir).
# --------------------------------------------------------------------------------------------
sinaica_cleanup_downloads <- function(
    dir,
    grace_sec = 120,
    patterns = c("\\.csv$", "\\.csv\\.part$", "\\.part$", "\\.tmp$",
                 "\\.crdownload$", "\\.download$")
) {
  now <- Sys.time()
  # list only top-level files inside downloads_root (no recursion!)
  paths <- list.files(dir, all.files = FALSE, full.names = TRUE, recursive = FALSE)
  if (!length(paths)) return(0L)
  
  # union of patterns, case-insensitive
  pat_union <- paste0("(", paste(patterns, collapse = ")|("), ")")
  match_idx <- grep(pat_union, basename(paths), ignore.case = TRUE)
  if (!length(match_idx)) return(0L)
  
  info <- file.info(paths[match_idx])
  old  <- which(!is.na(info$mtime) & difftime(now, info$mtime, "secs") > grace_sec)
  if (!length(old)) return(0L)
  
  unlink(paths[match_idx][old], force = TRUE)
  length(old)
}
# ============================================================================================
# RAW (“Datos crudos”) HELPERS
# ============================================================================================
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_open_datos_crudos
# @Arg     : session      — SeleniumSession
# @Arg     : base_url     — SINAICA base (e.g., "https://sinaica.inecc.gob.mx/scica/")
# @Arg     : timeout_page — seconds to wait for on-load readiness
# @Arg     : timeout_ctrl — seconds to wait for first control on raw page
# @Output  : invisible(TRUE) or error
# @Purpose : Navigate to base URL, open “Módulos ▸ Datos crudos”, and wait controls.
# --------------------------------------------------------------------------------------------
sinaica_raw_open_datos_crudos <- function(session, base_url,
                                          timeout_page = 10, timeout_ctrl = 10) {
  session$navigate(base_url)
  wait_ready(session, timeout_page)
  
  # Open "Módulos" dropdown then click "Datos crudos"
  ok_open <- try(
    session$find_element(
      "xpath",
      "//a[contains(@class,'dropdown-toggle') and contains(normalize-space(.),'Módulos')]"
    )$click(),
    silent = TRUE
  )
  
  ok_click <- try(
    session$find_element(
      "xpath",
      "//a[@href='/data.php?tipo=C' and normalize-space(.)='Datos crudos']"
    )$click(),
    silent = TRUE
  )
  if (inherits(ok_open, "try-error") || inherits(ok_click, "try-error")) {
    # Fallback: find by text only
    sinaica_js_click_text(
      session,
      "//a[contains(@href,'data.php?tipo=C') and contains(normalize-space(.),'Datos crudos')]"
    )
  }
  
  # Wait a key control on the raw page
  wait_for(session, "css selector", "button[data-id='estacion']", timeout_ctrl)
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_open_dropdown
# @Arg     : session  — SeleniumSession
# @Arg     : data_id  — one of "estacion" or "parametro"
# @Arg     : timeout  — seconds to wait for the opened menu
# @Output  : invisible(TRUE) or error
# @Purpose : Open a Bootstrap-select dropdown (by data-id) and ensure items are visible.
# --------------------------------------------------------------------------------------------
sinaica_raw_open_dropdown <- function(session, data_id = c("estacion","parametro"),
                                      timeout = 8) {
  data_id <- match.arg(data_id)
  btn <- wait_for(
    session, "css selector", sprintf("button[data-id='%s']", data_id), timeout
  )
  try(btn$click(), silent = TRUE)
  
  # Wait until the inner <ul> has at least 1 <li> (header or option)
  t0 <- Sys.time()
  repeat {
    n <- try(
      session$execute_script(
        paste0(
          "var b=document.querySelector(\"button[data-id='", data_id, "']\");",
          "if(!b) return 0;",
          "var root=b.closest('.bootstrap-select');",
          "if(!root) return 0;",
          "var ul=root.querySelector('ul.dropdown-menu.inner');",
          "if(!ul) return 0;",
          "return ul.children.length||0;"
        )
      )[[1]],
      silent = TRUE
    )
    if (!inherits(n, "try-error") && is.finite(n) && n > 0) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop("Dropdown '", data_id, "' did not open or has no items.")
    Sys.sleep(0.25)
  }
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_list_networks_and_stations
# @Arg     : session — SeleniumSession
# @Output  : data.frame(red_text, red_slug, station_text, station_slug)
# @Purpose : Read the “Estación” dropdown content as pairs (Red, Estación). Red slug is the
#            site header after “Red: ”, normalized via sinaica_slugify().
# --------------------------------------------------------------------------------------------
sinaica_raw_list_networks_and_stations <- function(session) {
  # Ensure dropdown is open
  try(sinaica_raw_open_dropdown(session, "estacion", timeout = 8), silent = TRUE)
  
  raw <- session$execute_script(
    "
    function normSpaces(s){ return (s||'').toString().replace(/\\s+/g,' ').trim(); }
    function stripRedPrefix(s){
      s = normSpaces(s);
      var i = s.indexOf(':');
      if(i>=0) s = s.slice(i+1);
      return normSpaces(s);
    }
    var btn = document.querySelector(\"button[data-id='estacion']\");
    if(!btn) return '[]';
    var root = btn.closest('.bootstrap-select');
    if(!root) return '[]';
    var ul = root.querySelector('ul.dropdown-menu.inner');
    if(!ul) return '[]';
    var lis = ul.querySelectorAll('li');
    var out = [];
    var curRed = null;
    for (var i=0;i<lis.length;i++){
      var li = lis[i];
      if(li.classList.contains('dropdown-header')){
        var ht = li.querySelector('.text');
        curRed = stripRedPrefix(ht ? ht.textContent : li.textContent);
      } else if(li.classList.contains('divider')){
        continue;
      } else {
        var a = li.querySelector('a .text') || li.querySelector('a');
        var st = normSpaces(a ? a.textContent : '');
        if(curRed && st){ out.push([curRed, st]); }
      }
    }
    return JSON.stringify(out);
    "
  )[[1]]
  
  arr <- try(jsonlite::fromJSON(raw), silent = TRUE)
  if (inherits(arr, "try-error") || !length(arr)) {
    return(data.frame(
      red_text     = character(0),
      red_slug     = character(0),
      station_text = character(0),
      station_slug = character(0)
    ))
  }
  df <- as.data.frame(arr, stringsAsFactors = FALSE)
  names(df) <- c("red_text","station_text")
  df$red_slug     <- vapply(df$red_text,     sinaica_slugify, character(1))
  df$station_slug <- vapply(df$station_text, sinaica_slugify, character(1))
  df[, c("red_text","red_slug","station_text","station_slug")]
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_select_station
# @Arg     : session      — SeleniumSession
# @Arg     : red_slug     — network slug (e.g., 'atitalaquia'); see list helper
# @Arg     : station_text — exact visible station name inside that red
# @Arg     : timeout      — seconds to wait after click
# @Output  : TRUE if clicked; FALSE otherwise
# @Purpose : Click a station option under the specified Red header. Matching for the Red
#            is by slug; station is by exact visible text.
# --------------------------------------------------------------------------------------------
sinaica_raw_select_station <- function(session, red_slug, station_text, timeout = 5) {
  sinaica_raw_open_dropdown(session, "estacion", timeout = 8)
  js <- paste0(
    "
    function slug(s){
      s=(s||'').toString().normalize('NFD').replace(/\\p{Diacritic}/gu,'');
      s=s.replace(/[^A-Za-z0-9]+/g,'_').replace(/^_+|_+$/g,'').toLowerCase();
      return s.replace(/_+/g,'_');
    }
    function norm(s){ return (s||'').toString().replace(/\\s+/g,' ').trim(); }

    var wantRed = ", jsonlite::toJSON(red_slug, auto_unbox = TRUE), ";
    var wantSta = ", jsonlite::toJSON(station_text, auto_unbox = TRUE), ";

    var btn = document.querySelector(\"button[data-id='estacion']\");
    if(!btn) return false;
    var root = btn.closest('.bootstrap-select');
    if(!root) return false;
    var ul = root.querySelector('ul.dropdown-menu.inner');
    if(!ul) return false;

    var lis = ul.querySelectorAll('li');
    var curRed = null;
    for (var i=0;i<lis.length;i++){
      var li = lis[i];
      if(li.classList.contains('dropdown-header')){
        var ht = li.querySelector('.text');
        var txt = ht ? ht.textContent : li.textContent;
        // strip 'Red:' prefix
        var p = txt.indexOf(':'); if(p>=0) txt = txt.slice(p+1);
        curRed = slug(norm(txt));
      } else if(li.classList.contains('divider')) {
        continue;
      } else {
        var a = li.querySelector('a .text') || li.querySelector('a');
        var st = norm(a ? a.textContent : '');
        if(curRed===wantRed && st===wantSta){
          try{ a.click(); return true; }catch(_){ return false; }
        }
      }
    }
    return false;
    "
  )
  ok <- isTRUE(session$execute_script(js)[[1]])
  if (ok) Sys.sleep(0.3)
  ok
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_list_parametros
# @Arg     : session — SeleniumSession
# @Output  : character vector of parameter names (visible text)
# @Purpose : Open and read the “Parámetro” dropdown options.
# --------------------------------------------------------------------------------------------
sinaica_raw_list_parametros <- function(session) {
  try(sinaica_raw_open_dropdown(session, "parametro", timeout = 8), silent = TRUE)
  raw <- session$execute_script(
    "
    function norm(s){ return (s||'').toString().replace(/\\s+/g,' ').trim(); }
    var btn = document.querySelector(\"button[data-id='parametro']\");
    if(!btn) return '[]';
    var root = btn.closest('.bootstrap-select');
    if(!root) return '[]';
    var ul = root.querySelector('ul.dropdown-menu.inner');
    if(!ul) return '[]';
    var as = ul.querySelectorAll('li a .text, li a');
    var out = [];
    for (var i=0;i<as.length;i++){
      var t = norm(as[i].textContent);
      if(t) out.push(t);
    }
    return JSON.stringify(out);
    "
  )[[1]]
  arr <- try(jsonlite::fromJSON(raw), silent = TRUE)
  arr  <- unique(arr)
  if (inherits(arr, "try-error")) character(0) else as.character(arr)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_select_parametro
# @Arg     : session  — SeleniumSession
# @Arg     : name     — visible text (e.g., "Ozono")
# @Output  : TRUE if clicked; FALSE otherwise
# @Purpose : Select a parameter by its visible name in the dropdown.
# --------------------------------------------------------------------------------------------
sinaica_raw_select_parametro <- function(session, name) {
  sinaica_raw_open_dropdown(session, "parametro", timeout = 8)
  js <- paste0(
    "
    function norm(s){ return (s||'').toString().replace(/\\s+/g,' ').trim(); }
    var want = ", jsonlite::toJSON(name, auto_unbox = TRUE), ";
    var btn = document.querySelector(\"button[data-id='parametro']\");
    if(!btn) return false;
    var root = btn.closest('.bootstrap-select'); if(!root) return false;
    var a = root.querySelector(\"ul.dropdown-menu.inner li a .text\");
    var as = root.querySelectorAll('ul.dropdown-menu.inner li a');
    for (var i=0;i<as.length;i++){
      var t = norm(as[i].textContent);
      if(t===want){ try{as[i].click();return true;}catch(_){return false;} }
    }
    return false;
    "
  )
  isTRUE(session$execute_script(js)[[1]])
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_set_datobase_hourly
# @Arg     : session — SeleniumSession
# @Output  : "ok" if set to hourly; "none" if only dashed placeholder; "absent" if no select
# @Purpose : If #slcDatoBase exists and offers options, pick “Concentraciones horarias”.
# --------------------------------------------------------------------------------------------
sinaica_raw_set_datobase_hourly <- function(session) {
  raw <- try(
    session$execute_script(
      "
      function norm(s){ return (s||'').toString().replace(/\\s+/g,' ').trim(); }
      var sel=document.getElementById('slcDatoBase');
      if(!sel) return 'absent';
      var n=sel.options.length;
      if(n===0) return 'none';
      if(n===1 && /-\\s-/.test(sel.options[0].textContent)) return 'none';

      var targetIndex = -1;
      for (var i=0;i<n;i++){
        var t = norm(sel.options[i].textContent);
        if (/^concentraciones\\s+horarias$/i.test(t)) { targetIndex = i; break; }
        if (sel.options[i].value==='1') { targetIndex = i; } // fallback
      }
      if (targetIndex<0) targetIndex = 0;
      sel.selectedIndex = targetIndex;
      sel.dispatchEvent(new Event('input',{bubbles:true}));
      sel.dispatchEvent(new Event('change',{bubbles:true}));
      return 'ok';
      "
    )[[1]],
    silent = TRUE
  )
  if (inherits(raw, "try-error")) "absent" else as.character(raw)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_set_fecha_inicio
# @Arg     : session   — SeleniumSession
# @Arg     : start_iso — "YYYY-MM-DD" (default "2023-01-01")
# @Output  : invisible(TRUE)
# @Purpose : Set #fechaIni directly (datepicker-friendly) and fire change/input events.
# --------------------------------------------------------------------------------------------
sinaica_raw_set_fecha_inicio <- function(session, start_iso = "2023-01-01") {
  session$execute_script(
    paste0(
      "var d=document.getElementById('fechaIni');",
      "if(d){ d.value=", jsonlite::toJSON(start_iso, auto_unbox = TRUE), ";",
      " d.dispatchEvent(new Event('input',{bubbles:true}));",
      " d.dispatchEvent(new Event('change',{bubbles:true})); }"
    )
  )
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_set_rango_best
# @Arg     : session — SeleniumSession
# @Output  : character selected label (e.g., "1 año") or NA if control absent
# @Purpose : Prefer “1 año” in #rango; otherwise pick the largest available span.
# --------------------------------------------------------------------------------------------
sinaica_raw_set_rango_best <- function(session) {
  raw <- try(
    session$execute_script(
      "
      function norm(s){ return (s||'').toString().replace(/\\s+/g,' ').trim(); }
      var sel=document.getElementById('rango');
      if(!sel) return '';
      var n=sel.options.length, bestI=-1, bestScore=-1, chosen='';
      for (var i=0;i<n;i++){
        var t = norm(sel.options[i].textContent);
        if (/^1\\s*a[nñ]o$/i.test(t)) { bestI=i; chosen=t; break; }
        var v = parseInt(sel.options[i].value||'0',10);
        if (isFinite(v) && v>bestScore){ bestScore=v; bestI=i; chosen=t; }
      }
      if (bestI<0) return '';
      sel.selectedIndex=bestI;
      sel.dispatchEvent(new Event('input',{bubbles:true}));
      sel.dispatchEvent(new Event('change',{bubbles:true}));
      return chosen;
      "
    )[[1]],
    silent = TRUE
  )
  if (inherits(raw, "try-error")) return(NA_character_)
  z <- as.character(raw %||% "")
  if (!nzchar(z)) NA_character_ else z
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_click_actualizar
# @Arg     : session — SeleniumSession
# @Output  : invisible(TRUE) or error
# @Purpose : Click the “Actualizar” button (#actualiza).
# --------------------------------------------------------------------------------------------
sinaica_raw_click_actualizar <- function(session) {
  btn <- wait_for(session, "css selector", "#actualiza", timeout = 10)
  try(btn$click(), silent = TRUE)
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_wait_descarga_ready
# @Arg     : session — SeleniumSession
# @Arg     : timeout — seconds to wait for the “Descarga Datos” button
# @Output  : TRUE if visible; FALSE otherwise
# @Purpose : Wait until #descDatos appears (or becomes visible) after Actualizar.
# --------------------------------------------------------------------------------------------
sinaica_raw_wait_descarga_ready <- function(session, timeout = 45) {
  t0 <- Sys.time()
  repeat {
    ok <- try(
      session$execute_script(
        "var e=document.getElementById('descDatos'); if(!e) return false;
         var s=window.getComputedStyle(e);
         var r=e.getBoundingClientRect();
         return s && s.display!=='none' && s.visibility!=='hidden' &&
                r.width>0 && r.height>0;"
      )[[1]],
      silent = TRUE
    )
    if (!inherits(ok, "try-error") && isTRUE(ok)) return(TRUE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) break
    Sys.sleep(0.4)
  }
  FALSE
}
# --------------------------------------------------------------------------------------------
# Function : sinaica_raw_click_descarga
# @Arg     : session — SeleniumSession
# @Output  : TRUE if click attempted; FALSE if #descDatos not found
# @Purpose : Click the “Descarga Datos” button (span#descDatos). The caller should then use
#            wait_for_new_download(...) as in your SINAICA CSV flow.
# --------------------------------------------------------------------------------------------
sinaica_raw_click_descarga <- function(session) {
  b <- try(session$find_element("css selector", "#descDatos"), silent = TRUE)
  if (inherits(b, "try-error")) return(FALSE)
  try(b$click(), silent = TRUE)
  TRUE
}
# --------------------------------------------------------------------------------------------
# Function: inegi_census_2020_ampliado_links
# @Output    : tibble(area, slug, filename, href) for the 3 requested areas
# @Purpose   : Hardcode the official CSV ZIP paths (relative → absolute).
# --------------------------------------------------------------------------------------------
inegi_census_2020_ampliado_links <- function() {
  base <- "https://www.inegi.org.mx"
  paths <- c(
    "Ciudad de México" = paste0("/contenidos/programas/ccpv/2020/microdatos/",
    "Censo2020_CA_cdmx_csv.zip"),
    "Hidalgo"          = paste0("/contenidos/programas/ccpv/2020/microdatos/",
    "Censo2020_CA_hgo_csv.zip"),
    "México"           = paste0("/contenidos/programas/ccpv/2020/microdatos/",
    "Censo2020_CA_mex_csv.zip")
  )
  slug <- c("Ciudad de México" = "cdmx", "Hidalgo" = "hgo", "México" = "mex")
  tibble::tibble(
    area     = names(paths),
    slug     = unname(slug[names(paths)]),
    filename = basename(paths),
    href     = paste0(base, unname(paths))
  )
}
# --------------------------------------------------------------------------------------------
# Function: .http_retry_get_zip
# @Arg       : url, dest, retries, quiet, referer
# @Output    : list(code=<http code>, bytes=<file size or NA>)
# @Purpose   : Robust GET with retries and disk write (httr::RETRY).
# --------------------------------------------------------------------------------------------
.http_retry_get_zip <- function(url, dest, retries = 5, quiet = FALSE,
                                referer = NULL) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ua <- httr::user_agent(
    sprintf("R/%s (%s) IDB-AirMonitoring",
            getRversion(), R.version$platform)
  )
  hdrs <- if (is.null(referer)) NULL else httr::add_headers(Referer = referer)
  rq <- httr::RETRY(
    verb        = "GET",
    url         = url,
    ua,
    hdrs,
    httr::write_disk(path = dest, overwrite = TRUE),
    httr::progress(type = if (quiet) "none" else "down"),
    times       = as.integer(retries),
    terminate_on = c(200L),
    quiet       = quiet,
    httr::timeout(60 * 60)  # up to 60 min
  )
  code  <- httr::status_code(rq)
  bytes <- suppressWarnings(file.size(dest))
  list(code = code, bytes = bytes)
}

# ============================================================================================
#  CDMX-specific functions - downloading main function and tiny helpers
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
# Function: cdmx_scrape_station_catalog
# @Arg       : page_url      — string; page that contains the "Catálogo estaciones" link
# @Arg       : harmonize_map — named chr vec (optional) to rename station display names
# @Arg       : verbose       — logical; print progress (default TRUE)
# @Arg       : out_dir       — string; directory to write outputs (created if missing)
# @Arg       : out_name      — string; base filename *without* extension
# @Arg       : write_rds     — logical; write .rds (default FALSE)
# @Arg       : write_parquet — logical; write .parquet via {arrow} (default TRUE)
# @Arg       : write_csv     — logical; write .csv (default FALSE)
# @Output    : tibble with columns:
#              entity, station, code, lon, lat, altitude_m, notes, id_station, source_url
# @Purpose   : Scrape the "Catálogo estaciones" for CDMX, read the CSV safely (Latin-1 → UTF-8),
#              and return a clean tibble with station metadata (code, name, lat/lon, etc.).
# @Written_on: 2025-09-04
# --------------------------------------------------------------------------------------------
cdmx_scrape_station_catalog <- function(
    page_url,
    harmonize_map = c(),
    verbose       = TRUE,
    out_dir,
    out_name,
    write_rds     = FALSE,
    write_parquet = TRUE,
    write_csv     = FALSE
) {
  # -- 0) Small helpers ---------------------------------------------------------
  
  `%||%`    <- function(a, b) if (!is.null(a)) a else b
  msg       <- function(...) if (isTRUE(verbose)) message(...)

  .clean_txt <- function(x) {
    x %>%
      stringr::str_replace_all("\u00A0", " ") %>%  # nbsp → space
      stringr::str_squish() %>%
      trimws()
  }
  
  # Build absolute URL and percent-encode spaces.
  .fix_url <- function(u, base) {
    u <- .clean_txt(u)
    if (!grepl("^https?://", u, ignore.case = TRUE)) {
      u <- xml2::url_absolute(u, base)
    }
    utils::URLencode(u, reserved = FALSE)
  }
  
  .ua <- httr::user_agent(
    sprintf("R/%s (%s) IDB-AirMonitoring",
            getRversion(), R.version$platform)
  )
  
  .to_num <- function(x) {
    readr::parse_number(
      x,
      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    )
  }
  
  # Convert specific character columns from Latin-1 → UTF-8 safely.
  .to_utf8_cols <- function(df, cols) {
    for (cc in cols) {
      if (cc %in% names(df)) {
        df[[cc]] <- iconv(df[[cc]], from = "latin1", to = "UTF-8")
        df[[cc]][is.na(df[[cc]])] <- ""
        df[[cc]] <- .clean_txt(df[[cc]])
      }
    }
    df
  }
  
  # Heuristic: is this error one of the SSL chain/cert issues curl emits?
  .is_ssl_chain_err <- function(e) {
    msg <- tolower(paste0(conditionMessage(e), collapse = " "))
    any(grepl("ssl.*(certificate|cert).*unable.*issuer|",
              "peer certificate|certificate verify failed|",
              "self[- ]signed|unknown ca|unable to get local issuer",
              msg, perl = TRUE))
  }
  
  # ---- 0.1) Early exit: if requested outputs already exist, load and return ----------------
  if (!missing(out_dir) && !missing(out_name) &&
      isTRUE(write_rds || write_parquet || write_csv)) {
    
    file_name <- basename(out_name)
    dir_path  <- file.path(out_dir, file_name)
    
    want <- c(
      if (isTRUE(write_parquet)) paste0(dir_path, ".parquet") else NULL,
      if (isTRUE(write_rds))     paste0(dir_path, ".rds")     else NULL,
      if (isTRUE(write_csv))     paste0(dir_path, ".csv")     else NULL
    )
    have_all <- length(want) > 0 && all(file.exists(want))
    
    if (have_all) {
      msg("↪︎ Outputs already exist — loading and returning without scraping.")
      # Prefer Parquet → RDS → CSV
      fpq <- paste0(dir_path, ".parquet")
      frd <- paste0(dir_path, ".rds")
      fcs <- paste0(dir_path, ".csv")
      
      if (file.exists(fpq) && requireNamespace("arrow", quietly = TRUE)) {
        df <- arrow::read_parquet(fpq)
        return(tibble::as_tibble(df))
      } else if (file.exists(frd)) {
        df <- readRDS(frd)
        return(tibble::as_tibble(df))
      } else if (file.exists(fcs)) {
        df <- readr::read_csv(fcs, show_col_types = FALSE, progress = FALSE)
        return(tibble::as_tibble(df))
      }
      # If we somehow couldn't load, fall through to scraping.
      msg("⚠️  Existing files found but could not be read; proceeding to scrape.")
    }
  }
  
  # -- 1) Fetch page and locate the CSV link -----------------------------------
  
  if (isTRUE(verbose)) message("Fetching page: ", page_url)
  
  # Robust fetch (httr only), then parse raw → HTML. This is from the helper functions
  # Works in Docker & local.
  resp_page <- suppressWarnings(.http_retry_get_robust(
    page_url, .ua, timeout_sec = 45, verbose = verbose, allow_insecure_fallback = TRUE
  ))
  doc <- xml2::read_html(httr::content(resp_page, as = "raw"))
  
  # Get the necessary elements
  a_tags <- rvest::html_elements(doc, "a")
  a_txt  <- tolower(rvest::html_text2(a_tags))
  a_href <- rvest::html_attr(a_tags, "href")
  
  hit <- which(
    stringr::str_detect(a_txt, "cat[aá]logo\\s+estaciones") &
      stringr::str_detect(tolower(a_href %||% ""), "\\.csv\\b")
  )
  if (!length(hit)) {
    stop("Could not find the 'Catálogo estaciones' CSV link on the page.")
  }
  
  csv_url <- .fix_url(a_href[hit[1]], base = page_url)
  if (isTRUE(verbose)) message("CSV link: ", csv_url)
  
  # -- 2) Download CSV robustly -------------------------------------------------
  
  # Same robust pattern for the CSV (scoped insecure fallback if needed)
  resp_csv <- suppressWarnings(.http_retry_get_robust(
    csv_url, .ua, timeout_sec = 60, verbose = verbose, allow_insecure_fallback = TRUE
  ))
  tmp_csv <- tempfile(fileext = ".csv")
  writeBin(httr::content(resp_csv, as = "raw"), tmp_csv)
  
  # -- 3) Find the header line number (tolerant to encoding) -------------------
  # Read raw lines without assuming UTF-8; use CP1252/Latin1 which matches site.
  # We only need the header index; warnings about bytes are acceptable here.
  
  lines_guess <- try(
    readLines(tmp_csv, encoding = "CP1252", warn = FALSE),
    silent = TRUE
  )
  if (inherits(lines_guess, "try-error") || !length(lines_guess)) {
    lines_guess <- try(
      readLines(tmp_csv, encoding = "latin1", warn = FALSE),
      silent = TRUE
    )
  }
  if (inherits(lines_guess, "try-error") || !length(lines_guess)) {
    stop("Failed to read the CSV to locate the header line.")
  }
  
  # Locate "cve_estac,nom_estac," ignoring case and leading spaces.
  hdr_idx <- suppressWarnings(
    grep("^\\s*cve_estac\\s*,\\s*nom_estac\\s*,",
         lines_guess, ignore.case = TRUE)
  )
  if (!length(hdr_idx)) {
    stop("Could not find the CSV header line.")
  }
  hdr_idx <- hdr_idx[1L]
  
  # -- 4) Parse the CSV directly from file (no string building) ----------------
  # Critical change: readr parses the file with the correct encoding and we
  # skip any preface lines. This avoids building a giant UTF-8 string.
  
  raw_df <- readr::read_csv(
    file = tmp_csv,
    skip = hdr_idx - 1L,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "Latin1",
                           decimal_mark = ".", grouping_mark = ",")
  )
  
  # -- 5) Normalize and ensure expected fields ---------------------------------
  names(raw_df) <- tolower(names(raw_df))
  need <- c("cve_estac","nom_estac","longitud","latitud",
            "alt","obs_estac","id_station")
  for (k in need) if (!k %in% names(raw_df)) raw_df[[k]] <- NA
  
  # Normalize character fields to UTF-8 for consistent outputs if needed
  chr_coding <- unique(Encoding(raw_df$nom_estac))
  if ("latin1" %in% chr_coding){
    raw_df <- .to_utf8_cols(raw_df, c("cve_estac","nom_estac","obs_estac",
                                      "id_station"))
  }
  
  df <- raw_df[, need]
  
  # -- 6) Clean values and standardize units -----------------------------------
  
  df <- df %>%
    dplyr::transmute(
      entity     = "CDMX",
      code       = .clean_txt(cve_estac),
      station    = .clean_txt(nom_estac),
      lon        = longitud,
      lat        = latitud,
      altitude_m = alt,
      notes      = .clean_txt(obs_estac),
      id_station = .clean_txt(id_station),
      source_url = csv_url
    ) %>%
    dplyr::mutate(
      # Mexico: lon negative (W), lat positive (N)
      lon = dplyr::case_when(!is.na(lon) & lon > 0 ~ -abs(lon), TRUE ~ lon),
      lat = dplyr::case_when(!is.na(lat) & lat < 0 ~  abs(lat), TRUE ~ lat)
    ) %>%
    dplyr::distinct(code, station, lat, lon, .keep_all = TRUE) %>%
    dplyr::arrange(station)
  
  # -- 7) Optional harmonization of names --------------------------------------
  
  if (length(harmonize_map)) {
    df$station <- dplyr::recode(df$station, !!!harmonize_map,
                                .default = df$station)
  }
  
  # -- 8) Summary ---------------------------------------------------------------
  
  if (isTRUE(verbose)) {
    message("===================================================================",
            "\n", "\n",
            "Rows: ", nrow(df),
            " | Unique stations: ", dplyr::n_distinct(df$station),
            " | With coords: ", sum(!is.na(df$lat) & !is.na(df$lon)))
  }
  
  # -- 9) Optional outputs ------------------------------------------------------
  if (!missing(out_dir) && !missing(out_name)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    if (isTRUE(write_rds)) {
      rds <- file.path(out_dir, paste0(out_name, ".rds"))
      saveRDS(df, rds, compress = "xz")
      if (verbose) message("💾 Wrote RDS → ", rds)
    }
    
    if (isTRUE(write_parquet)) {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required for Parquet output.")
      }
      pq <- file.path(out_dir, paste0(out_name, ".parquet"))
      arrow::write_parquet(df, pq, compression = "zstd")
      if (verbose) message("🧱 Wrote Parquet → ", pq)
    }
    
    if (isTRUE(write_csv)) {
      csv <- file.path(out_dir, paste0(out_name, ".csv"))
      readr::write_csv(df, csv)
      if (verbose) message("📝 Wrote CSV → ", csv)
    }
  } else if (isTRUE(verbose)) {
    message("No out_dir/out_name provided → returning tibble only.")
  }
  
  return(df)
}


# --------------------------------------------------------------------------------------------
# Function: cdmx_scrape_states_merge
# @Arg       : station_in_cdmx — tibble; pre-scraped CDMX stations
# @Arg       : base_url        — string; landing with the state dropdown
# @Arg       : states          — character; state names to scrape
# @Arg       : container       — TRUE if using docker-compose Selenium service
# @Arg       : session         — optional SeleniumSession (reuse if provided)
# @Arg       : timeout_page    — seconds to wait for page load
# @Arg       : timeout_ctrl    — seconds to wait for controls/table
# @Arg       : wd_request_timeout_ms — per-request driver timeout
# @Arg       : out_dir         — string; directory to write outputs (created if missing)
# @Arg       : out_name        — string; base filename *without* extension
# @Arg       : write_rds       — logical; write .rds (default FALSE)
# @Arg       : write_parquet   — logical; write .parquet via {arrow} (default TRUE)
# @Arg       : write_csv       — logical; write .csv (default FALSE)
# @Arg       : verbose         — logical; print progress (default TRUE)
# @Output    : tibble merged (CDMX + selected states)
# @Purpose   : Open dropdown → click state → wait table (scroll-aware) → iterate stations.
# @Written_on: 20/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
cdmx_scrape_states_merge <- function(
    station_in_cdmx,
    base_url  = cdmx_cfg$url_station_info,
    states    = c("Hidalgo", "México"),
    container = TRUE,
    session   = NULL,
    timeout_page = 25,
    timeout_ctrl = 25,
    wd_request_timeout_ms = 60000L,
    out_dir,
    out_name,
    write_rds       = FALSE,
    write_parquet   = TRUE,
    write_csv       = FALSE,
    verbose         = TRUE
) {
  `%||%`    <- function(a, b) if (!is.null(a)) a else b
  isTRUEtry <- function(x) isTRUE(try(x, silent = TRUE))
  msg       <- function(...) if (isTRUE(verbose)) message(...)
  
  # ---- 0) Early exit: if requested outputs already exist, load and return ----------------
  if (!missing(out_dir) && !missing(out_name) &&
      isTRUE(write_rds || write_parquet || write_csv)) {
    
    file_name <- basename(out_name)
    dir_path  <- file.path(out_dir, file_name)
    
    want <- c(
      if (isTRUE(write_parquet)) paste0(dir_path, ".parquet") else NULL,
      if (isTRUE(write_rds))     paste0(dir_path, ".rds")     else NULL,
      if (isTRUE(write_csv))     paste0(dir_path, ".csv")     else NULL
    )
    have_all <- length(want) > 0 && all(file.exists(want))
    
    if (have_all) {
      msg("↪︎ Outputs already exist — loading and returning without scraping.")
      # Prefer Parquet → RDS → CSV
      fpq <- paste0(dir_path, ".parquet")
      frd <- paste0(dir_path, ".rds")
      fcs <- paste0(dir_path, ".csv")
      
      if (file.exists(fpq) && requireNamespace("arrow", quietly = TRUE)) {
        df <- arrow::read_parquet(fpq)
        return(tibble::as_tibble(df))
      } else if (file.exists(frd)) {
        df <- readRDS(frd)
        return(tibble::as_tibble(df))
      } else if (file.exists(fcs)) {
        df <- readr::read_csv(fcs, show_col_types = FALSE, progress = FALSE)
        return(tibble::as_tibble(df))
      }
      # If we somehow couldn't load, fall through to scraping.
      msg("⚠️  Existing files found but could not be read; proceeding to scrape.")
    }
  }
  
  # ---- 1) Start / reuse Selenium ---------------------------------------------------------
  close_on_exit <- FALSE
  if (is.null(session)) {
    close_on_exit <- TRUE
    if (!container) {
      msg("🚀 Starting local Selenium on 4445…")
      cid <- system(
        paste0(
          "docker run -d -p 4445:4444 --shm-size=2g ",
          "selenium/standalone-firefox:4.34.0-20250717"
        ),
        intern = TRUE
      )
      on.exit(try(system(sprintf("docker rm -f %s", cid), intern = TRUE),
                  silent = TRUE), add = TRUE)
      sel_host <- "localhost"; sel_port <- 4445L
    } else {
      sel_host <- "selenium";  sel_port <- 4444L
    }
    
    dl_dir_container <- if (container) "/home/seluser/Downloads" else
      Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
    
    caps <- list(
      browserName = "firefox",
      "moz:firefoxOptions" = list(
        prefs = list(
          "browser.download.folderList"     = 2L,
          "browser.download.dir"            = dl_dir_container,
          "browser.download.useDownloadDir" = TRUE,
          "browser.helperApps.neverAsk.saveToDisk" = paste(
            "text/csv", "application/csv", "application/vnd.ms-excel",
            "application/octet-stream", sep = ","
          ),
          "browser.download.manager.showWhenStarting" = FALSE,
          "browser.download.alwaysOpenPanel"         = FALSE,
          "pdfjs.disabled"                           = TRUE
        )
      ),
      timeouts = list(implicit = 0L, pageLoad = 120000L, script = 60000L)
    )
    
    ses_args <- list(
      browser      = "firefox",
      host         = sel_host,
      port         = sel_port,
      capabilities = caps
    )
    fml <- try(names(formals(selenium::SeleniumSession$new)), silent = TRUE)
    if (!inherits(fml, "try-error")) {
      if ("request_timeout" %in% fml) {
        ses_args$request_timeout <- as.integer(wd_request_timeout_ms)
      } else if ("http_timeout_ms" %in% fml) {
        ses_args$http_timeout_ms <- as.integer(wd_request_timeout_ms)
      }
    }
    session <- do.call(selenium::SeleniumSession$new, ses_args)
  }
  if (isTRUE(close_on_exit)) on.exit(session$close(), add = TRUE)
  
  # ---- 2) Navigate to landing ------------------------------------------------------------
  session$navigate(base_url)
  wait_ready(session, timeout_page)
  
  out_rows <- list()
  
  # ---- 3) Loop for states ---------------------------------------------------------------
  for (state_name in states) {
    msg("🗺️  State: ", state_name)
    
    # Fresh page each time (avoids residual overlays).
    session$navigate(base_url)
    wait_ready(session, timeout_page)
    
    ok_dd <- isTRUEtry(
      sinaica_open_state_dropdown(session, timeout = timeout_ctrl)
    )
    if (!ok_dd) {
      isTRUEtry(
        session$find_element(
          "css selector", "button.dropdown-toggle.btn-verde-index"
        )$click()
      )
      Sys.sleep(0.3)
      ok_dd <- isTRUEtry(
        sinaica_open_state_dropdown(session, timeout = timeout_ctrl)
      )
    }
    if (!ok_dd) stop("Could not open the 'Seleccionar estado' dropdown.")
    
    ok_sel <- isTRUEtry(sinaica_select_state(session, state_name))
    if (!ok_sel) {
      isTRUEtry(
        session$find_element(
          "css selector", "button.dropdown-toggle.btn-verde-index"
        )$click()
      )
      Sys.sleep(0.3)
      ok_sel <- isTRUEtry(sinaica_select_state(session, state_name))
    }
    if (!ok_sel) stop("Could not select state: ", state_name)
    
    # ---- 4) Wait stations table (scroll-aware) ------------------------------------------
    if (!sinaica_wait_state_table(session, timeout = timeout_ctrl + 20,
                                  min_rows = 1)) {
      stop("State table did not appear for: ", state_name)
    }
    
    # ---- 5) Read table and iterate Estación cells ---------------------------------------
    st_tbl <- sinaica_read_state_table(session)
    if (!nrow(st_tbl)) {
      msg("   ⚠️ No station table found for ", state_name, " — skipping.")
      next
    }
    msg(sprintf("   ↳ %d stations listed", nrow(st_tbl)))
    
    # ---- 6) For each station: open detail → wait detail → parse → append row ------------
    for (i in seq_len(nrow(st_tbl))) {
      est_id   <- st_tbl$est_id[i]
      st_name0 <- sinaica_clean_text(st_tbl$station[i])
      red_name <- sinaica_clean_text(st_tbl$red[i])
      
      msg(sprintf("      🏷️  %s — %s (id=%s)", red_name, st_name0, est_id))
      
      css_cell <- sprintf("td.tdEst#est_%s", est_id)
      sinaica_scroll_into_view_sel(session, css_cell)
      el <- try(session$find_element("css selector", css_cell), silent = TRUE)
      if (inherits(el, "try-error")) {
        msg("         ⚠️  Station cell not found; skipping.")
        next
      }
      sinaica_click_el(session, el)
      
      if (!sinaica_wait_station_detail(session, timeout = timeout_ctrl)) {
        msg("         ⚠️  Detail table not visible; skipping.")
        isTRUEtry(session$execute_script("history.back();"))
        isTRUEtry(wait_for(
          session, "css selector", "td.tdEst[id^='est_']",
          timeout = timeout_ctrl
        ))
        next
      }
      
      det <- sinaica_read_station_detail(session)
      kv  <- det$kv %||% list()
      url <- det$url %||% session$execute_script("return location.href")[[1]]
      h3  <- det$h3 %||% ""
      
      nm <- sinaica_extract_name_code_from_h3(h3)
      st_name <- ifelse(is.na(nm$name) || !nzchar(nm$name), st_name0, nm$name)
      st_code <- ifelse(is.na(nm$code) || !nzchar(nm$code),
                        NA_character_, nm$code)
      
      coords <- sinaica_parse_coords(kv[["coordenadas"]] %||% "")
      alt_m  <- sinaica_parse_alt(kv[["altitud"]] %||% "")
      
      sys_txt  <- sinaica_clean_text(kv[["sistema de monitoreo"]] %||% "")
      ent_abbr <- sub(".*\\(([^\\)]+)\\).*", "\\1", sys_txt)
      ent_abbr <- if (identical(ent_abbr, sys_txt) || !nzchar(ent_abbr))
        toupper(substr(state_name, 1, 3)) else toupper(ent_abbr)
      
      out_rows[[length(out_rows) + 1L]] <- tibble::tibble(
        entity     = ent_abbr,
        code       = st_code,
        station    = st_name,
        lon        = coords[["lon"]],
        lat        = coords[["lat"]],
        altitude_m = alt_m,
        notes      = NA_character_,
        id_station = as.character(est_id),
        source_url = url
      )
      
      isTRUEtry(session$execute_script("history.back();"))
      isTRUEtry(wait_for(
        session, "css selector", "td.tdEst[id^='est_']",
        timeout = timeout_ctrl
      ))
      Sys.sleep(0.2)
    }
  }
  
  # ---- 7) Build merged tibble ------------------------------------------------------------
  new_df <- if (length(out_rows)) dplyr::bind_rows(out_rows) else
    tibble::tibble(
      entity=character(), code=character(), station=character(),
      lon=double(), lat=double(), altitude_m=double(), notes=character(),
      id_station=character(), source_url=character()
    )
  
  df <- dplyr::bind_rows(station_in_cdmx, new_df) %>%
    dplyr::distinct(entity, station, .keep_all = TRUE)
  
  # ---- 8) Summary -----------------------------------------------------------------------
  if (isTRUE(verbose)) {
    msg("===================================================================")
    msg(paste0(
      "Rows: ", nrow(df),
      " | Unique stations: ", dplyr::n_distinct(df$station),
      " | With coords: ", sum(!is.na(df$lat) & !is.na(df$lon))
    ))
  }
  
  # ---- 9) Optional outputs ---------------------------------------------------------------
  if (!missing(out_dir) && !missing(out_name)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    if (isTRUE(write_rds)) {
      rds <- file.path(out_dir, paste0(out_name, ".rds"))
      saveRDS(df, rds, compress = "xz")
      msg("💾 Wrote RDS → ", rds)
    }
    
    if (isTRUE(write_parquet)) {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required for Parquet output.")
      }
      pq <- file.path(out_dir, paste0(out_name, ".parquet"))
      arrow::write_parquet(df, pq, compression = "zstd")
      msg("🧱 Wrote Parquet → ", pq)
    }
    
    if (isTRUE(write_csv)) {
      csv <- file.path(out_dir, paste0(out_name, ".csv"))
      readr::write_csv(df, csv)
      msg("📝 Wrote CSV → ", csv)
    }
  } else if (isTRUE(verbose)) {
    msg("No out_dir/out_name provided → returning tibble only.")
  }
  
  # ---- 10) Return -----------------------------------------------------------------------
  return(df)
}


# --------------------------------------------------------------------------------------------
# Function : cdmx_download_sinaica_data
# @Arg     : base_url      — SINAICA form URL ("https://sinaica.inecc.gob.mx/scica/")
# @Arg     : years         — integer vector (e.g., 2000:2023)
# @Arg     : container     — TRUE if using docker-compose Selenium service
# @Arg     : session       — optional SeleniumSession (reuse if provided)
# @Arg     : max_attempts  — retries per (smca, red, parámetro, year)
# @Arg     : timeout_page  — seconds for page load readiness
# @Arg     : timeout_ctrl  — seconds for controls to appear
# @Arg     : timeout_modal — seconds to see the first "Aceptar" modal
# @Arg     : timeout_csv   — seconds to wait results → CSV after Buscar
# @Arg     : timeout_dl    — seconds to wait for each CSV file to download
# @Arg     : timeout_param_ready — seconds to wait parámetro list
# @Arg     : timeout_year_ready  — seconds to wait year list
# @Arg     : timeout_est_ready   — seconds to wait station list
# @Arg     : wd_request_timeout_ms — per-request driver timeout (if supported)
# @Arg     : settle_first_year_sec — pause before first year of a parámetro
# @Arg     : settle_before_csv_click_sec — pause after results before CSV click
# @Arg     : subdir        — folder under DOWNLOADS_DIR to move files to
# @Output  : invisible tibble log: smca, red, parametro, year, status, file
# @Purpose : Drive SINAICA "Descargas" reliably. Same as before, but with adaptive waits
#            for the slow CDMX group and a safer Buscar lookup.
# @Written_on: 05/09/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
cdmx_download_sinaica_data <- function(
    base_url      = cdmx_cfg$base_url_sinaica,
    years         = cdmx_cfg$years,
    container     = TRUE,
    session       = NULL,
    max_attempts  = 5,
    timeout_page  = 5,
    timeout_ctrl  = 5,
    timeout_modal = 5,
    timeout_csv   = 5,
    timeout_dl    = 60,
    timeout_param_ready = 10,
    timeout_year_ready  = 10,
    timeout_est_ready   = 10,
    wd_request_timeout_ms       = 60000L,
    settle_first_year_sec       = 5,
    settle_before_csv_click_sec = 2,
    subdir        = "Ground_stations/Mexico/SINAICA"
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # -- local: safer Buscar/CSV discovery (avoids throwing when missing) ---------------------
  robust_find_btns <- function(session) {
    buscar <- NULL
    e1 <- try(session$find_element("css selector", "#buscIndDesc"), silent = TRUE)
    if (!inherits(e1, "try-error")) buscar <- e1 else {
      e2 <- try(
        session$find_element(
          "xpath",
          paste0("//input[( @type='button' or @type='submit') and ",
                 "normalize-space(@value)='Buscar']")
        ),
        silent = TRUE
      )
      if (!inherits(e2, "try-error")) buscar <- e2
    }
    csv_candidates <- list(
      try(session$find_element("css selector", "#descargarDesc_CSV"), silent = TRUE),
      try(session$find_element("css selector", "#btnCSV"),            silent = TRUE),
      try(session$find_element("xpath",
                               "//a[contains(normalize-space(.),'CSV')]"),
          silent = TRUE),
      try(session$find_element("xpath",
                               "//button[contains(normalize-space(.),'CSV')]"),
          silent = TRUE)
    )
    csv <- NULL
    for (e in csv_candidates) if (!inherits(e, "try-error")) { csv <- e; break }
    csv_href <- NULL
    if (!is.null(csv)) {
      csv_href <- try(
        session$execute_script(
          "return arguments[0] && arguments[0].getAttribute
           ? arguments[0].getAttribute('href') : null;",
          list(csv)
        )[[1]],
        silent = TRUE
      )
      if (inherits(csv_href, "try-error")) csv_href <- NULL
    }
    list(buscar = buscar, csv = csv, href = csv_href)
  }
  
  # -- local: sweep the top-level downloads folder for stale CSVs ---------------------------
  cleanup_top_downloads <- function(dir, grace_sec = 120) {
    old <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    if (!length(old)) return(invisible(TRUE))
    now <- Sys.time()
    for (p in old) {
      age <- try(now - file.info(p)$mtime, silent = TRUE)
      if (!inherits(age, "try-error") &&
          is.finite(as.numeric(age)) &&
          as.numeric(age, units = "secs") > grace_sec) {
        try(unlink(p), silent = TRUE)
      }
    }
    invisible(TRUE)
  }
  
  # 0) Cap the last requested year (do not exceed provided vector)
  end_year_cap <- max(years)
  
  # 1) Ensure folders exist
  downloads_root <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  dir.create(downloads_root, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Downloads dir: ", downloads_root)
  
  # 2) Resolve subdir destination
  is_abs <- function(p) grepl("^(/|[A-Za-z]:[/\\\\])", p)
  normalize_safe <- function(p) {
    out <- try(normalizePath(p, winslash = "/", mustWork = FALSE), silent = TRUE)
    if (inherits(out, "try-error")) p else out
  }
  target_dir <- NULL
  if (!is.null(subdir)) {
    subdir_norm <- normalize_safe(subdir)
    target_dir  <- if (is_abs(subdir)) subdir_norm else file.path(downloads_root, subdir)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    message("📂 Target subdir : ", target_dir)
  }
  
  # -- 3.1) EARLY EXIT: skip if target_dir already looks complete -------------------------
  if (!is.null(target_dir)) {
    # Count all CSVs in the target dir (non-recursive; matches how we save files)
    csvs   <- list.files(target_dir, pattern = "\\.csv$", full.names = TRUE)
    n_csv  <- length(csvs)
    
    # Heuristics: look for per-SMCA prefixes we generate in filenames
    # (robust to extra text around them; case-insensitive)
    has_cdmx     <- any(grepl("cdmx",         basename(csvs), ignore.case = TRUE))
    has_hgo      <- any(grepl("hidalgo",      basename(csvs), ignore.case = TRUE))
    has_mxst     <- any(grepl("mexico_state", basename(csvs), ignore.case = TRUE))
    has_morelos  <- any(grepl("morelos", basename(csvs), ignore.case = TRUE))
    has_puebla   <- any(grepl("puebla", basename(csvs), ignore.case = TRUE))
    has_tlaxcala <- any(grepl("tlaxcala", basename(csvs), ignore.case = TRUE))
    has_michocan <- any(grepl("michoacán", basename(csvs), ignore.case = TRUE))
    has_guerrero <- any(grepl("guerrero", basename(csvs), ignore.case = TRUE))
    
    # Condition A: absolute count threshold (>= 620 CSVs)
    # Condition B: at least one file for each state group present
    already_complete <- (n_csv >= 1000) || (has_cdmx && has_hgo && has_mxst && n_csv > 0)
    
    if (already_complete) {
      msg_dir <- target_dir
      msg_cnt <- sprintf("✅ There are %d files downloaded%s",
                         n_csv, if (n_csv >= 1000) " (≥ 1000)" else "")
      message(sprintf("\n↪︎ Data already present on \"%s\" - Skipping the download", msg_dir))
      message(msg_cnt)
      
      # Return a tiny log so callers can tell we skipped on purpose
      return(invisible(
        tibble::tibble(
          smca      = NA_character_,
          red       = NA_character_,
          parametro = NA_character_,
          year      = NA_integer_,
          status    = "skipped_preexisting",
          file      = NA_character_
        )
      ))
    }
  }
  
  
  # 3) Start or reuse Selenium (configure Firefox for silent CSV downloads)
  close_on_exit <- FALSE
  if (is.null(session)) {
    close_on_exit <- TRUE
    if (!container) {
      message("🚀 Starting local Selenium on 4445…")
      cid <- system(
        paste0(
          "docker run -d -p 4445:4444 --shm-size=2g ",
          "selenium/standalone-firefox:4.34.0-20250717"
        ),
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
            "text/csv", "application/csv", "application/vnd.ms-excel",
            "application/octet-stream", sep = ","
          ),
          "browser.download.manager.showWhenStarting" = FALSE,
          "browser.download.alwaysOpenPanel"         = FALSE,
          "pdfjs.disabled"                           = TRUE
        )
      ),
      timeouts = list(implicit = 0L, pageLoad = 120000L, script = 60000L)
    )
    ses_args <- list(
      browser      = "firefox",
      host         = sel_host,
      port         = sel_port,
      capabilities = caps
    )
    fml <- try(names(formals(selenium::SeleniumSession$new)), silent = TRUE)
    if (!inherits(fml, "try-error")) {
      if ("request_timeout" %in% fml) {
        ses_args$request_timeout <- as.integer(wd_request_timeout_ms)
      } else if ("http_timeout_ms" %in% fml) {
        ses_args$http_timeout_ms <- as.integer(wd_request_timeout_ms)
      }
    }
    session <- do.call(selenium::SeleniumSession$new, ses_args)
  }
  if (isTRUE(close_on_exit)) on.exit(session$close(), add = TRUE)
  
  # 4) Static config
  smca_targets <- list(
    list(value = "43", text = "EDOMEX : México"),
    list(value = "40", text = "HGO : Hidalgo"),
    list(value = "61", text = "CDMX : Ciudad de México"),
    list(value = "45", text = "MOR : Morelos"),
    list(value = "62", text = "GUE : Guerrero"),
    list(value = "44", text = "MICH : Michoacán"),
    list(value = "56", text = "TLAX : Tlaxcala"),
    list(value = "49", text = "PUE : Puebla"),
    list(value = "50", text = "QRO : Querétaro")
  )

  smca_slug_map <- c(
    "EDOMEX : México"         = "mexico_state",
    "HGO : Hidalgo"           = "hidalgo",
    "CDMX : Ciudad de México" = "cmdx",
    "MOR : Morelos"           = "morelos",
    "GUE : Guerrero"          = "guerrero",
    "MICH : Michoacán"        = "michoacán",
    "TLAX : Tlaxcala"         = "tlaxcala",
    "PUE : Puebla"            = "puebla",
    "QRO : Querétaro"         = "querétaro"
  )
  
  # 5) Run and log
  log <- list()
  
  for (smca in smca_targets) {
    message("\n🏷️  SMCA: ", smca$text)
    
    # -- Adaptive budgets for the slow CDMX group -----------------------------------------
    is_cdmx <- identical(smca$value, "61") || grepl("^\\s*CDMX", smca$text)
    mult    <- if (is_cdmx) 3L else 1L
    
    csv_wait_this        <- if (is_cdmx) max(timeout_csv, 45) else timeout_csv
    timeout_ctrl_sm      <- max(timeout_ctrl * mult,      if (is_cdmx) 15 else 5)
    timeout_param_sm     <- max(timeout_param_ready * mult, if (is_cdmx) 60 else 10)
    timeout_year_sm      <- max(timeout_year_ready  * mult, if (is_cdmx) 60 else 10)
    timeout_est_sm       <- max(timeout_est_ready   * mult, if (is_cdmx) 30 else 10)
    settle_after_add     <- if (is_cdmx) 30 else 1
    settle_after_param   <- if (is_cdmx) 60 else 1
    long_pause_cdmx      <- is_cdmx
    
    # Read redes once per SMCA
    sinaica_open_descargas(session, base_url, timeout_page, timeout_ctrl_sm)
    res <- sinaica_js_set_select(session, "SMCASelDesc",
                                 value = smca$value, text = smca$text)
    if (!identical(res, "ok")) stop("Failed to select SMCA: ", smca$text)
    wait_for(session, "css selector", "#redSelDesc", timeout_ctrl_sm)
    redes_df <- sinaica_wait_options(session, "redSelDesc", min_n = 1, timeout = 60)
    if (!nrow(redes_df)) {
      redes_df <- data.frame(text = "Única", value = "", stringsAsFactors = FALSE)
    }
    
    for (ri in seq_len(nrow(redes_df))) {
      red_name <- redes_df$text[ri]
      red_val  <- redes_df$value[ri]
      message("   🌐 Red: ", red_name)
      
      # Fresh page; ensure stations exist before reading parámetros
      Sys.sleep(if (is_cdmx) 6 else 4)
      sinaica_open_descargas(session, base_url, timeout_page, timeout_ctrl_sm)
      sinaica_js_set_select(session, "SMCASelDesc", value = smca$value)
      if (nzchar(red_val)) sinaica_js_set_select(session, "redSelDesc", value = red_val)
      
      wait_for(session, "css selector", "#estSelDesc", timeout_ctrl_sm)
      st_df <- sinaica_wait_options(session, "estSelDesc",
                                    min_n = 1, timeout = timeout_est_sm)
      if (!nrow(st_df)) {
        message("   🚫 No stations in this red — skipping whole red.")
        next
      }
      
      # Add all stations, then get parámetros (CDMX settles longer)
      try(session$find_element("css selector", "#delLstEstDat")$click(), silent = TRUE)
      wait_for(session, "css selector", "#addTodasEstDesc", timeout_ctrl_sm)$click()
      if (settle_after_add > 0) Sys.sleep(settle_after_add)
      
      params_df <- sinaica_wait_options(session, "paramSelDesc",
                                        min_n = 1, timeout = timeout_param_sm)
      if (!nrow(params_df)) {
        message("   ⚠️ Parámetros empty for this red — skipping.")
        next
      }
      
      for (pi in seq_len(nrow(params_df))) {
        par_name <- iconv(params_df$text[pi], from = "", to = "UTF-8", sub = "byte")
        par_val  <- params_df$value[pi]
        message("      🧪 Parámetro: ", par_name)
        
        ok_form <- sinaica_rehydrate_form(
          session, base_url, smca, red_val, par_val,
          timeout_page, timeout_ctrl_sm, timeout_est_sm,
          long_pause = long_pause_cdmx
        )
        if (!isTRUE(ok_form)) {
          message("      ⏭️  Skip parámetro due to no stations.")
          next
        }
        if (settle_after_param > 0) Sys.sleep(settle_after_param)
        
        # Years available for this parámetro/red/SMCA
        years_df <- sinaica_wait_years_ready(
          session, par_val, timeout = timeout_year_sm
        )
        if (!nrow(years_df)) {
          message("      ↪︎ Years empty; hard reload and retry…")
          ok_form <- sinaica_rehydrate_form(
            session, base_url, smca, red_val, par_val,
            timeout_page, timeout_ctrl_sm, timeout_est_sm,
            long_pause = TRUE
          )
          if (!isTRUE(ok_form)) next
          if (settle_after_param > 0) Sys.sleep(settle_after_param)
          years_df <- sinaica_wait_years_ready(session, par_val,
                                               timeout = timeout_year_sm)
        }
        avail_years <- sort(unique(suppressWarnings(as.integer(years_df$value))))
        avail_years <- avail_years[is.finite(avail_years)]
        if (!length(avail_years)) {
          message("      ⚠️ No available years listed — skipping parámetro.")
          next
        }
        
        # Respect requested range and the availability
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
        
        # Year loop
        for (yr in years_iter) {
          attempt <- 0L
          repeat {
            attempt <- attempt + 1L
            message(sprintf("         📥 Año %d (attempt %d/%d)…",
                            yr, attempt, max_attempts))
            
            newest_path <- NA_character_
            before <- list.files(downloads_root, pattern = "\\.csv$", full.names = TRUE)
            
            # Fast path: if results remain visible, skip re-hydration entirely.
            goto_download <- FALSE
            if (attempt > 1L) {
              fast <- sinaica_quick_csv_download(
                session, downloads_root, before,
                settle_sec = settle_before_csv_click_sec,
                timeout_dl_quick = min(25, timeout_dl)
              )
              if (!is.null(fast)) { src <- fast; goto_download <- TRUE }
            }
            
            res_try <- try({
              if (!isTRUE(goto_download)) {
                # Clean slate to avoid sticky UI state (CDMX: allow long settle)
                sinaica_rehydrate_form(
                  session, base_url, smca, red_val, par_val,
                  timeout_page, timeout_ctrl_sm, timeout_est_sm,
                  long_pause = long_pause_cdmx
                )
                
                # Select the target year robustly
                if (yr == min(years_iter) && settle_first_year_sec > 0) {
                  Sys.sleep(settle_first_year_sec)  # small settle for first year
                }
                sinaica_set_year_robust(
                  session, yr, par_val, timeout_year_ready = timeout_year_sm
                )
                
                # Buscar → (modal) → decide CSV vs. nodata
                btns <- robust_find_btns(session)
                if (is.null(btns$buscar)) {
                  try(
                    wait_for(session, "css selector", "#buscIndDesc",
                             timeout = max(timeout_ctrl_sm, 45)),
                    silent = TRUE
                  )
                  btns <- robust_find_btns(session)
                }
                if (is.null(btns$buscar)) stop("Buscar control not present.")
                btns$buscar$click()
                
                res <- sinaica_wait_csv_or_nodata(session, timeout = csv_wait_this)
                if (identical(res$type, "nodata")) {
                  message("            ↪︎ No hay datos — skipping this year quickly.")
                  log[[length(log) + 1L]] <- tibble::tibble(
                    smca      = smca$text,
                    red       = red_name,
                    parametro = par_name,
                    year      = yr,
                    status    = "nodata",
                    file      = NA_character_
                  )
                  break
                }
                if (identical(res$type, "none")) {
                  stop("Transient: CSV/modal not ready yet.")
                }
                
                # Otherwise we have a CSV button → click → re-click loop until file
                if (settle_before_csv_click_sec > 0)
                  Sys.sleep(settle_before_csv_click_sec)
                sinaica_click_el(session, res$btn)
                
                src <- sinaica_download_with_reclick(
                  session,
                  downloads_root = downloads_root,
                  before         = before,
                  timeout_total  = min(timeout_dl, if (is_cdmx) 90 else 75),
                  click_every    = if (is_cdmx) 5 else 4
                )
              }
              
              # Rename with conflict-proof informative name
              smca_slug <- smca_slug_map[[smca$text]] %||% sinaica_slugify(smca$text)
              red_slug  <- sinaica_slugify(red_name)
              par_slug  <- sinaica_slugify(par_name)
              new_base  <- sprintf("%s__%s__%s__%d.csv",
                                   smca_slug, red_slug, par_slug, yr)
              dest0 <- if (!is.null(target_dir)) {
                file.path(target_dir, new_base)
              } else {
                file.path(dirname(src), new_base)
              }
              dest <- sinaica_next_nonconflicting(dest0)
              ok_mv <- try(file.rename(src, dest), silent = TRUE)
              if (inherits(ok_mv, "try-error") || !isTRUE(ok_mv)) {
                file.copy(src, dest, overwrite = TRUE); unlink(src)
              }
              newest_path <- dest
              message("            ✅ ", basename(newest_path))
            }, silent = TRUE)
            
            # Always keep the top-level downloads tidy (ignore errors).
            try(cleanup_top_downloads(downloads_root, grace_sec = 60), silent = TRUE)
            
            if (!inherits(res_try, "try-error")) {
              log[[length(log) + 1L]] <- tibble::tibble(
                smca      = smca$text,
                red       = red_name,
                parametro = par_name,
                year      = yr,
                status    = "ok",
                file      = newest_path
              )
              break
            } else if (attempt < max_attempts) {
              back <- min(45, 6 ^ attempt)
              message(sprintf("            ⚠️  Failed; backoff %ds, retrying…", back))
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
              # Leave page fresh for next parámetro
              sinaica_rehydrate_form(
                session, base_url, smca, red_val, par_val,
                timeout_page, timeout_ctrl_sm, timeout_est_sm, long_pause = TRUE
              )
              break
            }
          } # repeat
        }   # years
      }     # parámetros
    }       # redes
  }         # smca
  
  # Final pass: sweep anything older than 5 minutes in the top-level downloads dir
  try(cleanup_top_downloads(downloads_root, grace_sec = 300), silent = TRUE)
  
  # -- Final targeted sweep --------------------------------------------------------------
  # If we renamed/moved files to a subdir, prune the original SINAICA CSVs that the
  # browser left in the top-level downloads folder. Be *very* specific: only delete
  # CSVs whose basename contains BOTH "Datos" and "SINAICA".
  if (!is.null(subdir)) {
    try({
      leftovers <- list.files(
        downloads_root, pattern = "\\.csv$", full.names = TRUE
      )
      if (length(leftovers)) {
        base <- basename(leftovers)
        sel  <- grepl("Datos",  base, ignore.case = TRUE) &
          grepl("SINAICA", base, ignore.case = TRUE)
        to_rm <- leftovers[sel]
        if (length(to_rm)) unlink(to_rm, recursive = FALSE, force = TRUE)
      }
    }, silent = TRUE)
  }
  
  invisible(dplyr::bind_rows(log))
}


# ============================================================================================
# Function: cdmx_download_remaining_raw_sinaica
# @Arg  : base_url         — SINAICA base URL (e.g., cdmx_cfg$base_url_sinaica)
# @Arg  : subdir_existing  — dir with existing validated CSVs (to check missing 2023)
# @Arg  : out_subdir_raw   — dir to save new raw downloads (separate from validated)
# @Arg  : container        — TRUE if using docker-compose Selenium service
# @Arg  : session          — optional SeleniumSession (reuse if provided)
# @Arg  : year_check       — integer year to pull as raw (default 2023)
# @Arg  : max_attempts     — retries per (station, parameter)
# @Arg  : timeout_page     — seconds for base page ready
# @Arg  : timeout_ctrl     — seconds for controls to appear
# @Arg  : timeout_descarga — seconds to wait “Descarga Datos” after “Actualizar”
# @Arg  : timeout_dl       — seconds to wait for each file to land in downloads
# @Arg  : wd_request_timeout_ms — per-request driver timeout (if supported)
# @Arg  : settle_after_station_sec — pause after picking a station
# @Arg  : settle_after_param_sec   — pause after picking a parameter
# @Output: invisible tibble log: smca, red, station, parametro, year, status, file
# @Purpose: (1) Detect which SMCA/state slugs lack `year_check` in validated folder.
#           (2) On “Módulos ▸ Datos crudos”, loop networks/stations in those states and
#               download hourly raw files for every available parameter in 2023.
# @Notes : Requires the raw helpers you extracted:
#          - sinaica_raw_open_datos_crudos(), sinaica_raw_list_networks_and_stations(),
#            sinaica_raw_select_station(), sinaica_raw_list_parametros(),
#            sinaica_raw_select_parametro(), sinaica_raw_set_datobase_hourly(),
#            sinaica_raw_set_fecha_inicio(), sinaica_raw_set_rango_best(),
#            sinaica_raw_click_actualizar(), sinaica_raw_wait_descarga_ready(),
#            sinaica_raw_click_descarga()
#          Also uses: wait_ready(), wait_for(), wait_for_new_download(),
#                      sinaica_slugify(), sinaica_next_nonconflicting()
# @Written_on: 10/16/2025
# @Written_by: Marcos Paulo (with ChatGPT)
# ============================================================================================
cdmx_download_remaining_raw_sinaica <- function(
    base_url        = cdmx_cfg$base_url_sinaica,
    subdir_existing = here::here(cdmx_cfg$dl_dir, "Ground_stations"),
    out_subdir_raw  = here::here(cdmx_cfg$dl_dir, "Ground_stations_raw_missing_data"),
    container       = TRUE,
    session         = NULL,
    year_check      = 2023L,
    max_attempts    = 3L,
    timeout_page    = 10,
    timeout_ctrl    = 10,
    timeout_descarga = 60,
    timeout_dl      = 90,
    wd_request_timeout_ms = 60000L,
    settle_after_station_sec = 0.5,
    settle_after_param_sec   = 0.5
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # -- Standard SMCA → slug mapping (keep in sync with validated pipeline) ------------------
  smca_slug_map <- c(
    "EDOMEX : México"         = "mexico_state",
    "HGO : Hidalgo"           = "hidalgo",
    "CDMX : Ciudad de México" = "cmdx",
    "MOR : Morelos"           = "morelos",
    "GUE : Guerrero"          = "guerrero",
    "MICH : Michoacán"        = "michoacán",
    "TLAX : Tlaxcala"         = "tlaxcala",
    "PUE : Puebla"            = "puebla",
    "QRO : Querétaro"         = "querétaro"
  )
  target_smca_slugs <- unique(unname(smca_slug_map))
  
  # -- 1) Inspect validated folder to see which SMCA slugs miss `year_check` ----------------
  if (!dir.exists(subdir_existing)) {
    stop("Validated folder not found: ", subdir_existing)
  }
  files <- list.files(subdir_existing, pattern = "\\.csv$", full.names = FALSE)
  base  <- sub("\\.csv$", "", basename(files))
  toks  <- strsplit(base, "__", fixed = TRUE)
  
  smca_slug <- vapply(toks, function(x) x[1] %||% NA_character_, character(1))
  yr        <- suppressWarnings(
    as.integer(vapply(toks, function(x) tail(x, 1) %||% NA_character_,
                      character(1)))
  )
  
  seen <- data.frame(slug = smca_slug, year = yr, stringsAsFactors = FALSE)
  seen <- seen[!is.na(seen$slug) & !is.na(seen$year), , drop = FALSE]
  
  have_yr <- unique(seen$slug[seen$year == as.integer(year_check)])
  needed  <- setdiff(target_smca_slugs, have_yr)
  
  if (!length(needed)) {
    message("✅ All SMCA slugs already have validated data for ", year_check, ".")
    return(invisible(
      tibble::tibble(
        smca      = character(0),
        red       = character(0),
        station   = character(0),
        parametro = character(0),
        year      = integer(0),
        status    = character(0),
        file      = character(0)
      )
    ))
  }
  
  message("Missing ", year_check, " for slugs: ", paste(needed, collapse = ", "))
  
  # -- 2) Prepare output dirs and Selenium session ------------------------------------------
  downloads_root <- Sys.getenv("DOWNLOADS_DIR", here::here("data", "downloads"))
  dir.create(downloads_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_subdir_raw, recursive = TRUE, showWarnings = FALSE)
  message("📥  Top-level downloads: ", downloads_root)
  message("📂  Raw output dir     : ", out_subdir_raw)
  
  close_on_exit <- FALSE
  if (is.null(session)) {
    close_on_exit <- TRUE
    if (!container) {
      message("🚀 Starting local Selenium on 4445…")
      cid <- system(
        paste0(
          "docker run -d -p 4445:4444 --shm-size=2g ",
          "selenium/standalone-firefox:4.34.0-20250717"
        ),
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
            "text/csv", "application/csv", "application/vnd.ms-excel",
            "application/octet-stream", sep = ","
          ),
          "browser.download.manager.showWhenStarting" = FALSE,
          "browser.download.alwaysOpenPanel"         = FALSE,
          "pdfjs.disabled"                           = TRUE
        )
      ),
      timeouts = list(implicit = 0L, pageLoad = 120000L, script = 60000L)
    )
    ses_args <- list(
      browser      = "firefox",
      host         = sel_host,
      port         = sel_port,
      capabilities = caps
    )
    fml <- try(names(formals(selenium::SeleniumSession$new)), silent = TRUE)
    if (!inherits(fml, "try-error")) {
      if ("request_timeout" %in% fml) {
        ses_args$request_timeout <- as.integer(wd_request_timeout_ms)
      } else if ("http_timeout_ms" %in% fml) {
        ses_args$http_timeout_ms <- as.integer(wd_request_timeout_ms)
      }
    }
    session <- do.call(selenium::SeleniumSession$new, ses_args)
  }
  if (isTRUE(close_on_exit)) on.exit(session$close(), add = TRUE)
  
  # -- 3) Open raw module and read the (red, station, state) index --------------------------
  sinaica_raw_open_datos_crudos(session, base_url, timeout_page, timeout_ctrl)
  
  # Read pairs now; then enrich with state by one DOM pass (data-tokens heuristic).
  idx <- sinaica_raw_list_networks_and_stations(session)
  
  # Inject state via a single JS sweep of the open "Estación" menu (robust for multiword).
  state_json <- session$execute_script(
    "
    function norm(s){return (s||'').toString().replace(/\\s+/g,' ').trim();}
    function slug(s){
      s=(s||'').toString().normalize('NFD').replace(/\\p{Diacritic}/gu,'');
      s=s.replace(/[^A-Za-z0-9]+/g,'_').replace(/^_+|_+$/g,'').toLowerCase();
      return s.replace(/_+/g,'_');
    }
    function firstCapsIx(tokens){
      for(var i=0;i<tokens.length;i++){
        if(/^[A-ZÁÉÍÓÚÜÑ]{2,4}$/.test(tokens[i])) return i;
      }
      return -1;
    }
    var btn=document.querySelector(\"button[data-id='estacion']\");
    if(!btn) return '[]';
    var root=btn.closest('.bootstrap-select'); if(!root) return '[]';
    var ul=root.querySelector('ul.dropdown-menu.inner'); if(!ul) return '[]';
    var lis=ul.querySelectorAll('li');
    var out=[], curRed=null;
    for(var i=0;i<lis.length;i++){
      var li=lis[i];
      if(li.classList.contains('dropdown-header')){
        var ht=li.querySelector('.text');
        var txt=ht?ht.textContent:li.textContent;
        var p=txt.indexOf(':'); if(p>=0) txt=txt.slice(p+1);
        curRed=norm(txt);
      } else if(li.classList.contains('divider')){
        continue;
      } else {
        var a=li.querySelector('a');
        if(!a) continue;
        var stSpan=a.querySelector('.text');
        var stTxt=norm(stSpan?stSpan.textContent:a.textContent);
        var dt=norm(a.getAttribute('data-tokens')||'');
        var tokens=dt.split(/\\s+/);
        var j=firstCapsIx(tokens); // tokens before j form the state name
        var state= (j>0? tokens.slice(0,j).join(' ') : '');
        out.push([curRed, stTxt, state, slug(state)]);
      }
    }
    return JSON.stringify(out);
    "
  )[[1]]
  
  enrich <- try(jsonlite::fromJSON(state_json), silent = TRUE)
  if (!inherits(enrich, "try-error") && length(enrich)) {
    e <- as.data.frame(enrich, stringsAsFactors = FALSE)
    names(e) <- c("red_text","station_text","state_text","state_slug")
    idx <- merge(
      idx, e,
      by = c("red_text","station_text"),
      all.x = TRUE, sort = FALSE
    )
  } else {
    # Fallback: unknown states → NA; we will keep all and filter later softly.
    idx$state_text <- NA_character_
    idx$state_slug <- NA_character_
  }
  
  # Keep only target SMCA/state slugs
  need_set <- unique(needed)
  idx_need <- idx[!is.na(idx$state_slug) & idx$state_slug %in% need_set, , drop = FALSE]
  
  if (!nrow(idx_need)) {
    message("⚠️ Could not match any station rows to missing states: ",
            paste(need_set, collapse = ", "))
    return(invisible(
      tibble::tibble(
        smca      = character(0),
        red       = character(0),
        station   = character(0),
        parametro = character(0),
        year      = integer(0),
        status    = character(0),
        file      = character(0)
      )
    ))
  }
  
  # -- 4) Main loop: for each (network, station) in missing states, pull all parameters -----
  log <- list()
  
  # Small util to rename/move downloaded file to 5-part schema
  save_download <- function(src, smca_slug, red_slug, par_slug, station_slug) {
    base_out <- sprintf("%s__%s__%s__%s__%d.xls",
                        smca_slug, red_slug, par_slug, station_slug, year_check)
    dest0 <- file.path(out_subdir_raw, base_out)
    dest  <- sinaica_next_nonconflicting(dest0)
    ok_mv <- try(file.rename(src, dest), silent = TRUE)
    if (inherits(ok_mv, "try-error") || !isTRUE(ok_mv)) {
      file.copy(src, dest, overwrite = TRUE); unlink(src)
    }
    dest
  }
  
  # Map state_slug back to smca_slug (they are identical for all but EDOMEX/CDMX labels)
  # Here state_slug should be like 'hidalgo','puebla', etc.; for safety, normalize.
  norm_state_slug <- function(s) gsub("_+", "_", tolower(iconv(s, "", "ASCII//TRANSLIT")))
  
  # Loop networks → stations
  for (i in seq_len(nrow(idx_need))) {
    red_text     <- idx_need$red_text[i]
    red_slug     <- idx_need$red_slug[i]
    station_text <- idx_need$station_text[i]
    station_slug <- idx_need$station_slug[i]
    state_slug   <- norm_state_slug(idx_need$state_slug[i])
    
    smca_slug_final <- state_slug  # e.g., 'hidalgo', 'puebla', etc.
    
    msg_head <- sprintf("\n🌐 Red: %s  |  📍 Estación: %s  |  🏷️ Estado: %s",
                        red_text, station_text, state_slug)
    message(msg_head)
    
    # Select station
    ok_sel <- FALSE
    for (att in seq_len(max_attempts)) {
      ok_sel <- sinaica_raw_select_station(session, red_slug, station_text)
      if (ok_sel) break
      Sys.sleep(0.5)
    }
    if (!ok_sel) {
      message("   ❌ Could not select station, skipping.")
      next
    }
    if (settle_after_station_sec > 0) Sys.sleep(settle_after_station_sec)
    
    # List parameters for this station now
    params <- sinaica_raw_list_parametros(session)
    if (!length(params)) {
      message("   ⚠️ No parameters listed for this station, skipping.")
      next
    }
    
    for (pname in params) {
      par_slug <- sinaica_slugify(pname)
      message("   🧪 Param: ", pname)
      
      # Attempt loop (parameter-level)
      attempt <- 0L
      repeat {
        attempt <- attempt + 1L
        newest_path <- NA_character_
        
        res_try <- try({
          # Pick parameter
          if (!sinaica_raw_select_parametro(session, pname)) {
            stop("Could not select parameter.")
          }
          if (settle_after_param_sec > 0) Sys.sleep(settle_after_param_sec)
          
          # Dato base → hourly or skip this param if none available
          db <- sinaica_raw_set_datobase_hourly(session)
          if (identical(db, "none")) {
            message("      ↪︎ No 'Dato base' options; skipping parameter.")
            log[[length(log) + 1L]] <- tibble::tibble(
              smca      = smca_slug_final,
              red       = red_text,
              station   = station_text,
              parametro = pname,
              year      = year_check,
              status    = "nodatabase",
              file      = NA_character_
            )
            break
          }
          
          # Set date & longest range (prefer 1 año)
          sinaica_raw_set_fecha_inicio(session, sprintf("%d-01-01", year_check))
          sinaica_raw_set_rango_best(session)
          
          # Update → wait for "Descarga Datos"
          sinaica_raw_click_actualizar(session)
          Sys.sleep(2)
          ok_ready <- sinaica_raw_wait_descarga_ready(session, timeout_descarga)
          if (!ok_ready) stop("'Descarga Datos' did not appear in time.")
          
          # Download
          before <- list.files(downloads_root, pattern = "\\.xls$", full.names = TRUE)
          if (!sinaica_raw_click_descarga(session)) {
            stop("Could not click 'Descarga Datos'.")
          }
          Sys.sleep(2)
          src <- wait_for_new_download(
            dir       = downloads_root,
            before    = before,
            pattern   = "\\.xls$",
            quiet_sec = 2,
            timeout   = timeout_dl
          )
          
          # Rename/move
          newest_path <- save_download(
            src, smca_slug_final, red_slug, par_slug, station_slug
          )
          message("      ✅ ", basename(newest_path))
        }, silent = TRUE)
        
        if (!inherits(res_try, "try-error")) {
          log[[length(log) + 1L]] <- tibble::tibble(
            smca      = smca_slug_final,
            red       = red_text,
            station   = station_text,
            parametro = pname,
            year      = year_check,
            status    = "ok",
            file      = newest_path
          )
          break
        } else if (attempt < max_attempts) {
          back <- min(45, 3 ^ attempt)
          message(sprintf("      ⚠️ Failed attempt %d — backoff %ds, retrying…",
                          attempt, back))
          Sys.sleep(back)
        } else {
          message("      ❌ Failed parameter after max attempts.")
          log[[length(log) + 1L]] <- tibble::tibble(
            smca      = smca_slug_final,
            red       = red_text,
            station   = station_text,
            parametro = pname,
            year      = year_check,
            status    = "failed",
            file      = NA_character_
          )
          break
        }
      } # repeat attempts
    }   # parameters
  }     # stations
  
  invisible(dplyr::bind_rows(log))
}


# --------------------------------------------------------------------------------------------
# Function: cdmx_download_inegi_census_2020_ampliado
# @Arg       : areas          — character; which areas to fetch
# @Arg       : base_url       — catalog page (for Referer header/log only)
# @Arg       : out_dir        — directory to save ZIPs
# @Arg       : overwrite      — re-download if file exists (default FALSE)
# @Arg       : retries        — max HTTP retries (default 5)
# @Arg       : quiet          — suppress progress messages (default FALSE)
# @Output    : tibble(area, file_path, bytes, status)
# @Purpose   : Download the CSV ZIPs for CA (cuestionario ampliado) directly.
# @Written_on: 08/07/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
cdmx_download_census_data <- function(
    areas     = c("Ciudad de México", "Hidalgo", "México"),
    base_url  = cdmx_cfg$base_url_census,
    out_dir   = cdmx_cfg$dl_out,
    overwrite = FALSE,
    retries   = 5,
    quiet     = FALSE
) {
  links <- inegi_census_2020_ampliado_links()
  # Validate requested areas
  bad <- setdiff(areas, links$area)
  if (length(bad)) {
    stop("Unknown area(s): ", paste(bad, collapse = ", "),
         ". Allowed: ", paste(links$area, collapse = ", "))
  }
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Map areas → rows we need
  need <- links[match(areas, links$area), , drop = FALSE]
  
  res <- lapply(seq_len(nrow(need)), function(i) {
    area <- need$area[i]
    url  <- need$href[i]
    dst  <- file.path(out_dir, need$filename[i])
    
    if (file.exists(dst) && !overwrite) {
      if (!quiet) message("↪︎ ", area, ": already present (skip).")
      bytes <- suppressWarnings(file.size(dst))
      return(tibble::tibble(
        area      = area,
        file_path = normalizePath(dst),
        bytes     = bytes,
        status    = "cached"
      ))
    }
    
    if (!quiet) message("⬇️  ", area, ": downloading CSV ZIP…")
    got <- .http_retry_get_zip(url, dst, retries = retries,
                               quiet = quiet, referer = base_url)
    
    if (got$code != 200L || is.na(got$bytes) || got$bytes < 1e6) {
      # Treat <1MB as failure for these archives
      if (file.exists(dst)) unlink(dst)
      status <- if (got$code != 200L) paste0("http_", got$code) else "too_small"
      if (!quiet) message("⚠️  ", area, ": failed (", status, ").")
      return(tibble::tibble(
        area      = area,
        file_path = NA_character_,
        bytes     = NA_real_,
        status    = status
      ))
    }
    
    if (!quiet) {
      pretty <- format(structure(got$bytes, class = "object_size"))
      message("✅ ", area, ": ", basename(dst), " (", pretty, ")")
    }
    
    tibble::tibble(
      area      = area,
      file_path = normalizePath(dst),
      bytes     = got$bytes,
      status    = "ok"
    )
  })
  
  dplyr::bind_rows(res)
}

# ============================================================================================
#  CDMX-specific functions for processing data - helpers
# ============================================================================================

# ============================================================================================
# Helper: MEMORY engine (RAM-path; now writes per-year partitioned Parquet)
# --------------------------------------------------------------------------------------------
# @Arg  : csvs                 — character vector of CSV paths (already discovered)
# @Arg  : years                — integer vector of years to keep
# @Arg  : tz                   — Olson tz for final relabel (clock preserved)
# @Arg  : out_dir, out_name    — output location; Parquet goes to:
#                                 file.path(out_dir, paste0(out_name, "_dataset"))
#                                 with partition "year=YYYY/"
# @Arg  : write_parquet        — logical; write partitioned dataset if TRUE
# @Arg  : write_rds, write_csv — optional single-file artifacts (full table)
# @Arg  : cleanup              — logical; remove source CSVs after success
# @Arg  : verbose              — logical; print progress
# @Arg  : stations_keep_codes  — NULL or vector of station codes to keep
# @Return: If write_parquet=TRUE → Arrow Dataset handle to the dataset folder.
#          Else → tibble (in-memory).
# @Steps : (0) helpers, (1) read/longify (early station filter),
#          (2) filter years + map parámetros, (3) aggregate + pivot wide,
#          (4) canonical station_code, (5) hourly skeleton,
#          (6) join skeleton, (7) tz relabel,
#          (8) write artifacts (partitioned Parquet, optional RDS/CSV),
#          (9) optional cleanup, (10) return result.
# ============================================================================================
.cdmx_merge_memory_engine <- function(
    csvs, years, tz, out_dir, out_name,
    write_parquet, write_rds, write_csv,
    cleanup, verbose, stations_keep_codes = NULL
) {
  if (isTRUE(verbose)) message("Engine: memory (RAM intensive).")
  
  # ---- (0) tiny helpers ------------------------------------------------------
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  .tnb <- function(x) trimws(gsub("\u00A0", " ", x, fixed = TRUE))
  .guess_from_file <- function(base) {
    b <- tolower(base)
    if (grepl("pm10", b)) return("PM10")
    if (grepl("pm2_5|pm2\\.5", b)) return("PM2.5")
    if (grepl("dioxido_de_nitrogeno|\\bno2\\b", b)) return("NO2")
    if (grepl("dioxido_de_azufre|\\bso2\\b", b)) return("SO2")
    if (grepl("ozono|\\bo3\\b", b)) return("O3")
    if (grepl("monoxido.*carbono|\\bco\\b", b)) return("CO")
    NA_character_
  }
  .map_param <- function(p) {
    k <- toupper(.tnb(p))
    dplyr::case_when(
      k %in% c("PM10","PARTICULAS MENORES A 10 MICRAS") ~ "pm10",
      k %in% c("PM2.5","PM2,5","PARTICULAS MENORES A 2.5") ~ "pm2.5",
      k %in% c("OZONO","O3") ~ "ozone",
      k %in% c("DIOXIDO DE NITROGENO","NO2") ~ "no2",
      k %in% c("OXIDOS DE NITROGENO","NOX") ~ "nox",
      k %in% c("MONOXIDO DE NITROGENO","NO") ~ "no",
      k %in% c("MONOXIDO DE CARBONO","CO") ~ "co",
      k %in% c("DIOXIDO DE AZUFRE","SO2") ~ "so2",
      k %in% c("DIOXIDO DE CARBONO","CO2") ~ "co2",
      k %in% c("TEMPERATURA","TEMP") ~ "temperatura",
      k %in% c("HUMEDAD RELATIVA","RH") ~ "rh",
      k %in% c("VELOCIDAD DEL VIENTO","VEL VIENTO") ~ "vel viento",
      k %in% c("DIRECCION DEL VIENTO","DIR VIENTO") ~ "dir viento",
      k %in% c("PRESION BAROMETRICA","PRESION BARO") ~ "presion baro",
      k %in% c("PRECIPITACION","LLUVIA") ~ "precipitacion",
      k %in% c("RADIACION","RADIACION SOLAR") ~ "radiation",
      TRUE ~ NA_character_
    )
  }
  .mean_na <- function(x) {
    if (all(is.na(x))) return(NA_real_)
    suppressWarnings(mean(x, na.rm = TRUE))
  }
  
  # ---- (1) read/longify with early station filter ---------------------------
  warn_log <- list()
  read_one <- function(path) {
    if (isTRUE(verbose)) message("… ", basename(path))
    
    wtxt <- character()
    df <- withCallingHandlers(
      readr::read_csv(
        file   = path,
        locale = readr::locale(encoding = "Latin1",
                               decimal_mark = ".", grouping_mark = ","),
        show_col_types = FALSE, progress = FALSE
      ),
      warning = function(w) {
        wtxt <<- c(wtxt, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (length(wtxt)) {
      warn_log[[length(warn_log) + 1L]] <<- list(
        file = basename(path), warnings = unique(wtxt)
      )
    }
    if (!ncol(df)) return(NULL)
    names(df) <- .tnb(names(df))
    
    # "- - - -" placeholders → NA (strings only)
    df <- dplyr::mutate(
      df,
      dplyr::across(where(is.character), ~ dplyr::na_if(.x, "- - - -"))
    )
    
    pcol <- intersect(c("Parámetro","Parametro","parametro"), names(df))
    if (!length(pcol)) {
      df$parametro <- .guess_from_file(basename(path))
      pcol <- "parametro"
    }
    if (!all(c("Fecha","Hora") %in% names(df))) return(NULL)
    
    st_cols <- setdiff(
      names(df)[grepl("\\s:\\s", names(df), perl = TRUE)],
      c("Parámetro","Parametro","parametro","Fecha","Hora","Unidad")
    )
    if (!length(st_cols)) return(NULL)
    
    keep <- intersect(c(pcol[1], "Fecha","Hora","Unidad", st_cols), names(df))
    df   <- df[, keep, drop = FALSE]
    
    long <- tidyr::pivot_longer(
      df,
      cols = dplyr::all_of(st_cols),
      names_to = "station_label",
      values_to = "value"
    )
    
    sp <- t(vapply(
      strsplit(long$station_label, "\\s:\\s", perl = TRUE),
      function(x) c(x[1] %||% NA_character_, x[2] %||% NA_character_),
      character(2)
    ))
    station_code <- .tnb(sp[, 1])
    station_name <- .tnb(sp[, 2])
    
    # Early filter by station codes (if provided)
    if (!is.null(stations_keep_codes)) {
      keep_set <- unique(stats::na.omit(stations_keep_codes))
      ok <- !is.na(station_code) & station_code %in% keep_set
      if (!any(ok)) return(NULL)
      long <- long[ok, , drop = FALSE]
      station_code <- station_code[ok]
      station_name <- station_name[ok]
    }
    
    long$value <- suppressWarnings(
      readr::parse_number(
        as.character(long$value),
        locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
      )
    )
    
    hr <- suppressWarnings(as.integer(long$Hora))
    hr[is.na(hr)] <- 0L
    dt_chr <- sprintf(
      "%s %02d:00:00", as.character(as.Date(long$Fecha)),
      pmax(0L, pmin(23L, hr))
    )
    dt_utc <- as.POSIXct(dt_chr, tz = "UTC")
    
    tibble::tibble(
      datetime     = dt_utc,
      station      = station_name,
      station_code = station_code,
      year         = as.integer(format(dt_utc, "%Y", tz = "UTC")),
      parametro    = as.character(long[[pcol[1]]]),
      value        = long$value
    )
  }
  
  pieces <- lapply(csvs, read_one)
  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) stop("No usable rows after parsing.")
  long_all <- dplyr::bind_rows(pieces)
  
  # ---- (2) keep years + map parámetros --------------------------------------
  long_all <- long_all[long_all$year %in% years, , drop = FALSE]
  long_all$.__var <- .map_param(long_all$parametro)
  long_all <- long_all[!is.na(long_all$.__var), , drop = FALSE]
  
  # ---- (3) aggregate duplicates; pivot to wide ------------------------------
  aggr <- long_all |>
    dplyr::group_by(datetime, station, station_code, year, .__var) |>
    dplyr::summarise(value = .mean_na(value), .groups = "drop")
  
  wide <- tidyr::pivot_wider(
    aggr,
    id_cols     = c(datetime, station, station_code, year),
    names_from  = .__var,
    values_from = value,
    values_fn   = .mean_na
  )
  
  # Remove NaN (keep POSIXct intact)
  wide <- dplyr::mutate(
    wide,
    dplyr::across(
      tidyselect::where(~ is.double(.x) && !inherits(.x, "POSIXt")),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    )
  )
  
  # ---- (4) canonical station_code per station -------------------------------
  st_ref <- wide |>
    dplyr::filter(!is.na(station_code), nzchar(station_code)) |>
    dplyr::count(station, station_code, sort = TRUE) |>
    dplyr::group_by(station) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(station, station_code)
  
  # ---- (5) hourly skeleton across requested span ----------------------------
  t0  <- as.POSIXct(sprintf("%04d-01-01 00:00:00", min(years)), tz = "UTC")
  t1  <- as.POSIXct(sprintf("%04d-12-31 23:00:00", max(years)), tz = "UTC")
  hrs <- seq(t0, t1, by = "1 hour")
  
  stations <- sort(unique(wide$station))
  if (!length(stations)) stop("No stations found after reshaping.")
  
  skel <- tidyr::expand_grid(station = stations, datetime = hrs)
  skel$year <- as.integer(format(skel$datetime, "%Y", tz = "UTC"))
  
  # ---- (6) join skeleton ← st_ref ← wide ------------------------------------
  wide_full <- skel |>
    dplyr::left_join(st_ref, by = "station") |>
    dplyr::left_join(
      wide, by = c("station","station_code","datetime","year")
    )
  
  # ---- (7) tz relabel (preserve clock times) --------------------------------
  if (!identical(tz, "UTC")) {
    if (!requireNamespace("lubridate", quietly = TRUE)) {
      stop("Package 'lubridate' is required for tz relabeling.")
    }
    wide_full$datetime <- lubridate::force_tz(wide_full$datetime, tz)
  }
  
  # ---- (8) write artifacts ---------------------------------------------------
  # 8a) partitioned Parquet dataset when requested
  if (isTRUE(write_parquet) && !missing(out_dir) && !missing(out_name)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for Parquet output.")
    }
    base <- file.path(out_dir, paste0(out_name, "_dataset"))
    dir.create(base, recursive = TRUE, showWarnings = FALSE)
    
    # Write per-year dataset (overwrite if exists)
    arrow::write_dataset(
      dataset                 = wide_full,
      path                    = base,
      format                  = "parquet",
      partitioning            = "year",
      existing_data_behavior  = "overwrite",
      compression             = "zstd"
    )
    if (isTRUE(verbose)) {
      n_pq <- length(list.files(base, pattern = "\\.parquet$", recursive = TRUE))
      message("🧱 Wrote partitioned dataset → ", base,
              " (files: ", n_pq, ")")
    }
    ds <- arrow::open_dataset(base)
    
    # Optional extras (single-file RDS/CSV of the full table)
    if (isTRUE(write_rds)) {
      rds <- file.path(out_dir, paste0(out_name, ".rds"))
      saveRDS(wide_full, rds, compress = "xz")
      if (isTRUE(verbose)) message("💾 Wrote RDS → ", rds)
    }
    if (isTRUE(write_csv)) {
      csv <- file.path(out_dir, paste0(out_name, ".csv"))
      readr::write_csv(wide_full, csv)
      if (isTRUE(verbose)) message("📝 Wrote CSV → ", csv)
    }
    
    # Optional cleanup of raw CSVs
    if (isTRUE(cleanup)) {
      if (isTRUE(verbose)) message("🧹 Removing source CSVs…")
      invisible(lapply(csvs, function(f) try(unlink(f), TRUE)))
    }
    return(ds)
  }
  
  # 8b) if not writing Parquet, still allow RDS/CSV
  if (isTRUE(write_rds) && !missing(out_dir) && !missing(out_name)) {
    rds <- file.path(out_dir, paste0(out_name, ".rds"))
    saveRDS(wide_full, rds, compress = "xz")
    if (isTRUE(verbose)) message("💾 Wrote RDS → ", rds)
  }
  if (isTRUE(write_csv) && !missing(out_dir) && !missing(out_name)) {
    csv <- file.path(out_dir, paste0(out_name, ".csv"))
    readr::write_csv(wide_full, csv)
    if (isTRUE(verbose)) message("📝 Wrote CSV → ", csv)
  }
  
  # ---- (9) optional cleanup of CSVs -----------------------------------------
  if (isTRUE(cleanup)) {
    if (isTRUE(verbose)) message("🧹 Removing source CSVs…")
    invisible(lapply(csvs, function(f) try(unlink(f), TRUE)))
  }
  
  # ---- (10) return in-memory tibble -----------------------------------------
  wide_full
}


# ============================================================================================
# Helper: DUCKDB engine (disk-backed; UNPIVOT; partitioned Parquet dataset)
# --------------------------------------------------------------------------------------------
# Notes:
#  • Writes to "<out_name>_dataset/year=YYYY/*.parquet" (SNAPPY by default).
#  • Applies stations_keep_codes inside the UNPIVOT INSERT (early prune).
#  • Copies one year at a time to reduce peak memory and avoid long COPY stalls.
# ============================================================================================
.cdmx_merge_duckdb_engine <- function(
    csvs, years, tz, out_dir, out_name, cleanup, run_parallel,
    verbose, stations_keep_codes = NULL
) {
  if (isTRUE(verbose))
    message("Engine: duckdb (disk-backed, readr pre-clean).")
  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    stop("Packages 'duckdb' and 'DBI' are required for duckdb engine.")
  }
  if (utils::packageVersion("duckdb") < "0.9.2") {
    stop("DuckDB >= 0.9.2 is required for UNPIVOT.")
  }
  if (missing(out_dir) || missing(out_name)) {
    stop("duckdb engine requires out_dir and out_name.")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to open the dataset.")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for pre-cleaning CSVs.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for pre-cleaning CSVs.")
  }
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  .tnb <- function(x) trimws(gsub("\u00A0", " ", x, fixed = TRUE))
  .dq  <- function(id) sprintf('"%s"', gsub('"', '""', id))
  .ds  <- function(s)  sprintf("'%s'", gsub("'", "''", s))
  .guess_from_file <- function(base) {
    b <- tolower(base)
    if (grepl("pm10", b)) return("PM10")
    if (grepl("pm2_5|pm2\\.5", b)) return("PM2.5")
    if (grepl("dioxido_de_nitrogeno|\\bno2\\b", b)) return("NO2")
    if (grepl("dioxido_de_azufre|\\bso2\\b", b)) return("SO2")
    if (grepl("ozono|\\bo3\\b", b)) return("O3")
    if (grepl("monoxido.*carbono|\\bco\\b", b)) return("CO")
    NA_character_
  }
  
  # --- (0) pre-clean one CSV → temp UTF-8 file + header info
  .preclean_to_utf8 <- function(path) {
    wtxt <- character()
    df <- withCallingHandlers(
      readr::read_csv(
        file   = path,
        locale = readr::locale(encoding = "Latin1",
                               decimal_mark = ".", grouping_mark = ","),
        show_col_types = FALSE, progress = FALSE
      ),
      warning = function(w) {
        wtxt <<- c(wtxt, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (!ncol(df)) return(NULL)
    
    # Normalize header & cells; convert dash/space/NBSP soup to NA
    names(df) <- .tnb(names(df))
    nb <- "\u00A0"
    is_chr <- vapply(df, is.character, logical(1))
    if (any(is_chr)) {
      df[is_chr] <- lapply(df[is_chr], function(x) {
        x <- gsub(nb, " ", x, fixed = TRUE)
        x <- trimws(x)
        x[x == ""] <- NA_character_
        # Any combo of hyphens/spaces/NBSP only → NA
        x[grepl("^[-\\s\u00A0]+$", x)] <- NA_character_
        x
      })
    }
    
    # Write a temporary UTF-8 CSV (readr writes NA as empty)
    tmp <- tempfile("cdmx_utf8_", fileext = ".csv")
    readr::write_csv(df, tmp, na = "")
    list(
      path = tmp,
      hdr_raw = names(df),   # post-clean raw names
      warn = unique(wtxt)
    )
  }
  
  # ---- step 1: connect + PRAGMAs + temp dir ---------------------------------
  dbdir <- tempfile("cdmx_duck_")
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir, read_only = FALSE))
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    try(unlink(dbdir, recursive = TRUE, force = TRUE), silent = TRUE)
  }, add = TRUE)
  
  thr <- if (run_parallel) max(1L, (parallel::detectCores() - 2)) else 1L
  DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", thr))
  DBI::dbExecute(con, "PRAGMA memory_limit='6GB';")
  
  tdir <- file.path(tempdir(), sprintf("_duck_tmp_%s", Sys.getpid()))
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(con, sprintf("PRAGMA temp_directory=%s;", .ds(tdir)))
  on.exit(try(unlink(tdir, recursive = TRUE, force = TRUE), silent = TRUE),
          add = TRUE)
  
  # ---- step 1b: detect TZ support (AT TIME ZONE) -----------------------------
  tz_sql <- .ds(tz)
  tz_supported <- tryCatch({
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT (TIMESTAMP '2000-01-01 00:00:00' AT TIME ZONE ", tz_sql,
        ") IS NOT NULL AS ok;"
      )
    )$ok[[1]] %||% FALSE
  }, error = function(e) FALSE)
  
  if (isTRUE(verbose)) {
    msg <- if (tz_supported)
      sprintf("TZ conversion enabled via AT TIME ZONE (%s).", tz) else
        "TZ conversion not available; storing naive local timestamps."
    message(msg)
  }
  
  # ---- step 2: staging table -------------------------------------------------
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE staging_long(",
      "datetime TIMESTAMP, station VARCHAR, station_code VARCHAR, ",
      "year INTEGER, parametro VARCHAR, value DOUBLE);"
    )
  )
  
  # ---- step 3: insert per-CSV via UNPIVOT (pre-cleaned UTF-8) ---------------
  ylist <- paste(sort(unique(years)), collapse = ",")
  
  keep_sql <- ""
  if (!is.null(stations_keep_codes)) {
    ks <- unique(stats::na.omit(stations_keep_codes))
    ks <- ks[nzchar(ks)]
    if (length(ks)) {
      inlist <- paste(vapply(ks, .ds, character(1)), collapse = ",")
      keep_sql <- paste0(
        " AND NULLIF(REGEXP_EXTRACT(st, '^(.*?)\\s*:\\s*', 1), '') IN (",
        inlist, ")"
      )
    }
  }
  
  insert_one <- function(path) {
    # 0) pre-clean to UTF-8 temp file
    pc <- .preclean_to_utf8(path)
    if (is.null(pc)) return(0L)
    tmp <- pc$path
    cr  <- pc$hdr_raw
    ct  <- .tnb(cr)
    
    # 1) find core columns
    f_i <- which(ct == "Fecha")[1]
    h_i <- which(ct == "Hora")[1]
    if (is.na(f_i) || is.na(h_i)) return(0L)
    fecha_q <- .dq(cr[f_i])
    hora_q  <- .dq(cr[h_i])
    
    # parámetro source (existing column or filename guess) → param_src
    p_i <- which(ct %in% c("Parámetro","Parametro","parametro"))[1]
    if (!is.na(p_i)) {
      param_sel <- paste0(.dq(cr[p_i]), " AS param_src")
    } else {
      g <- .guess_from_file(basename(path))
      param_sel <- if (is.na(g)) "NULL AS param_src"
      else paste0(.ds(g), " AS param_src")
    }
    
    # station columns: anything "CODE : Name", excluding metadata
    meta <- ct %in% c("Fecha","Hora","Unidad","Parámetro",
                      "Parametro","parametro")
    st_cols <- cr[grepl("^[^:]+\\s*:\\s*.+$", ct, perl = TRUE) & !meta]
    if (!length(st_cols)) return(0L)
    unp_in <- paste(vapply(st_cols, .dq, character(1)), collapse = ",")
    
    # Optional prebuilt station filter
    keep_sql <- ""
    if (!is.null(stations_keep_codes)) {
      ks <- unique(stats::na.omit(stations_keep_codes))
      ks <- ks[nzchar(ks)]
      if (length(ks)) {
        inlist <- paste(vapply(ks, .ds, character(1)), collapse = ",")
        keep_sql <- paste0(
          " AND NULLIF(REGEXP_EXTRACT(st, '^(.*?)\\s*:\\s*', 1), '') ",
          "IN (", inlist, ")"
        )
      }
    }
    
    # 2) common FROM/WHERE block (so we can COUNT then INSERT)
    from_where <- paste0(
      "FROM (",
      "  SELECT ",
      "    *, ",
      "    ", param_sel, ", ",
      "    COALESCE(",
      "      STRPTIME(", fecha_q, ",'%Y-%m-%d'), ",
      "      STRPTIME(SPLIT_PART(", fecha_q, ",' ',1),'%Y-%m-%d'), ",
      "      STRPTIME(", fecha_q, ",'%Y/%m/%d'), ",
      "      STRPTIME(SPLIT_PART(", fecha_q, ",' ',1),'%Y/%m/%d'), ",
      "      STRPTIME(", fecha_q, ",'%d/%m/%Y'), ",
      "      STRPTIME(SPLIT_PART(", fecha_q, ",' ',1),'%d/%m/%Y'), ",
      "      STRPTIME(", fecha_q, ",'%d-%m-%Y'), ",
      "      STRPTIME(SPLIT_PART(", fecha_q, ",' ',1),'%d-%m-%Y'), ",
      "      STRPTIME(", fecha_q, ",'%m/%d/%Y'), ",
      "      STRPTIME(SPLIT_PART(", fecha_q, ",' ',1),'%m/%d/%Y'), ",
      "      STRPTIME(", fecha_q, ",'%m-%d-%Y'), ",
      "      STRPTIME(SPLIT_PART(", fecha_q, ",' ',1),'%m-%d-%Y'), ",
      "      STRPTIME(", fecha_q, ",'%d.%m.%Y'), ",
      "      STRPTIME(", fecha_q, ",'%Y.%m.%d') ",
      "    ) AS d, ",
      "    GREATEST(0, LEAST(23, ",
      "      TRY_CAST(SUBSTR(TRIM(", hora_q, "),1,2) AS INTEGER))) AS h ",
      "  FROM read_csv_auto(",
      .ds(tmp),
      "    , HEADER=TRUE, ALL_VARCHAR=TRUE, SAMPLE_SIZE=200000, ",
      "      ENCODING='utf-8', IGNORE_ERRORS=TRUE",
      "  )",
      ") t ",
      "UNPIVOT INCLUDE NULLS (val FOR st IN (", unp_in, ")) u ",
      "WHERE d IS NOT NULL ",
      "  AND EXTRACT(YEAR FROM (CAST(d AS TIMESTAMP) + ",
      "      (h * INTERVAL 1 HOUR))) IN (", ylist, ")",
      keep_sql
      # keep val even if NULL so skeleton can later fill the panel
      # if you want to skip empty cells: add " AND val IS NOT NULL"
    )
    
    # 3) dry-run count (how many rows would be inserted)
    count_sql <- paste0("SELECT COUNT(*) AS n ", from_where, ";")
    n_cand <- DBI::dbGetQuery(con, count_sql)[["n"]]
    if (isTRUE(verbose)) {
      message("   → candidate rows in ", basename(path), ": ",
              format(n_cand, big.mark = ","))
    }
    if (!is.finite(n_cand) || n_cand <= 0) return(0L)
    
    # 4) real insert; compute reliable delta by counting table n before/after
    n_before <- DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM staging_long;"
    )[["n"]]
    
    # Build datetime expr: UTC (tz-supported) or naive (fallback)
    dt_expr <- if (tz_supported) {
      paste0(
        "CAST((CAST(d AS TIMESTAMP) + (h * INTERVAL 1 HOUR)) ",
        "     AT TIME ZONE ", tz_sql, " AS TIMESTAMP)"
      )
    } else {
      "CAST(d AS TIMESTAMP) + (h * INTERVAL 1 HOUR)"
    }
    
    insert_sql <- paste0(
      "INSERT INTO staging_long ",
      "SELECT ",
      "  ", dt_expr, " AS datetime, ",
      "  COALESCE(",
      "    NULLIF(REGEXP_EXTRACT(st, '^.*?\\s*:\\s*(.*)$', 1), ''), st",
      "  ) AS station, ",
      "  NULLIF(REGEXP_EXTRACT(st, '^(.*?)\\s*:\\s*', 1), '') ",
      "    AS station_code, ",
      "  EXTRACT(YEAR FROM (CAST(d AS TIMESTAMP) + ",
      "        (h * INTERVAL 1 HOUR))) AS year, ",
      "  param_src AS parametro, ",
      "  TRY_CAST(REPLACE(TRIM(val), ',', '') AS DOUBLE) AS value ",
      from_where,
      ";"
    )
    DBI::dbExecute(con, insert_sql)
    
    n_after <- DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM staging_long;"
    )[["n"]]
    as.integer(n_after - n_before)
  }
  
  total_ins <- 0L
  for (pth in csvs) {
    ins <- tryCatch(insert_one(pth), error = function(e) {
      if (isTRUE(verbose)) message("   ! insert error: ", conditionMessage(e))
      0L
    })
    total_ins <- total_ins + as.integer(ins %||% 0L)
    if (isTRUE(verbose)) {
      message("… ", basename(pth), " | inserted=",
              as.integer(ins %||% 0L))
    }
  }
  
  n_rows <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) n FROM staging_long;"
  )[["n"]]
  if (!n_rows) stop("No rows loaded into staging_long.")
  
  # ---- step 4: map parámetros; aggregate; station ref ------------------------
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE long_mapped AS ",
      "SELECT datetime, station, station_code, year, ",
      "CASE ",
      "WHEN UPPER(TRIM(parametro)) IN ",
      "('PM10','PARTICULAS MENORES A 10 MICRAS') THEN 'pm10' ",
      "WHEN UPPER(TRIM(parametro)) IN ",
      "('PM2.5','PM2,5','PARTICULAS MENORES A 2.5') THEN 'pm2.5' ",
      "WHEN UPPER(TRIM(parametro)) IN ('OZONO','O3') THEN 'ozone' ",
      "WHEN UPPER(TRIM(parametro)) IN ('DIOXIDO DE NITROGENO','NO2') ",
      "THEN 'no2' ",
      "WHEN UPPER(TRIM(parametro)) IN ('OXIDOS DE NITROGENO','NOX') ",
      "THEN 'nox' ",
      "WHEN UPPER(TRIM(parametro)) IN ('MONOXIDO DE NITROGENO','NO') ",
      "THEN 'no' ",
      "WHEN UPPER(TRIM(parametro)) IN ('MONOXIDO DE CARBONO','CO') ",
      "THEN 'co' ",
      "WHEN UPPER(TRIM(parametro)) IN ('DIOXIDO DE AZUFRE','SO2') ",
      "THEN 'so2' ",
      "WHEN UPPER(TRIM(parametro)) IN ('DIOXIDO DE CARBONO','CO2') ",
      "THEN 'co2' ",
      "WHEN UPPER(TRIM(parametro)) IN ('TEMPERATURA','TEMP') ",
      "THEN 'temperatura' ",
      "WHEN UPPER(TRIM(parametro)) IN ('HUMEDAD RELATIVA','RH') ",
      "THEN 'rh' ",
      "WHEN UPPER(TRIM(parametro)) IN ",
      "('VELOCIDAD DEL VIENTO','VEL VIENTO') THEN 'vel viento' ",
      "WHEN UPPER(TRIM(parametro)) IN ",
      "('DIRECCION DEL VIENTO','DIR VIENTO') THEN 'dir viento' ",
      "WHEN UPPER(TRIM(parametro)) IN ",
      "('PRESION BAROMETRICA','PRESION BARO') THEN 'presion baro' ",
      "WHEN UPPER(TRIM(parametro)) IN ('PRECIPITACION','LLUVIA') ",
      "THEN 'precipitacion' ",
      "WHEN UPPER(TRIM(parametro)) IN ('RADIACION','RADIACION SOLAR') ",
      "THEN 'radiation' ",
      "ELSE NULL END AS var, value ",
      "FROM staging_long WHERE parametro IS NOT NULL;"
    )
  )
  
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE aggr AS ",
      "SELECT datetime, station, station_code, year, var, ",
      "AVG(value) AS value ",
      "FROM long_mapped WHERE var IS NOT NULL ",
      "GROUP BY 1,2,3,4,5;"
    )
  )
  
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE st_ref AS ",
      "SELECT station, station_code FROM (",
      "SELECT station, station_code, COUNT(*) AS n, ",
      "ROW_NUMBER() OVER(",
      "  PARTITION BY station ORDER BY COUNT(*) DESC",
      ") AS rn ",
      "FROM aggr ",
      "WHERE station_code IS NOT NULL AND station_code <> '' ",
      "GROUP BY 1,2) t WHERE rn = 1;"
    )
  )
  
  # ---- step 5: stream pivot → per-year Parquet (SNAPPY) ----------------------
  parquet_codec <- "SNAPPY"
  base <- file.path(out_dir, paste0(out_name, "_dataset"))
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  
  yrs <- sort(unique(years))
  for (yy in yrs) {
    if (isTRUE(verbose)) message(sprintf("→ Writing year %d …", yy))
    DBI::dbExecute(
      con,
      sprintf("CREATE TEMP TABLE aggr_y AS
               SELECT * FROM aggr WHERE year = %d;", yy)
    )
    outdir_y <- file.path(base, paste0(sprintf("year=%d", yy), ".parquet"))
    if (dir.exists(outdir_y)) unlink(outdir_y, recursive = TRUE, force = TRUE)
    
    # Skeleton: LOCAL → UTC if tz_supported; else naive local hours
    hours_sql <- if (tz_supported) {
      paste0(
        "SELECT CAST((gs.ts AT TIME ZONE ", .ds(tz), ") AS TIMESTAMP) ",
        "AS datetime FROM generate_series(",
        "TIMESTAMP '", yy, "-01-01 00:00:00', ",
        "TIMESTAMP '", yy, "-12-31 23:00:00', ",
        "INTERVAL 1 HOUR) AS gs(ts)"
      )
    } else {
      paste0(
        "SELECT gs.ts AS datetime FROM generate_series(",
        "TIMESTAMP '", yy, "-01-01 00:00:00', ",
        "TIMESTAMP '", yy, "-12-31 23:00:00', ",
        "INTERVAL 1 HOUR) AS gs(ts)"
      )
    }
    
    sql_copy_y <- paste0(
      "COPY (",
      "WITH stations AS (SELECT DISTINCT station FROM aggr_y), ",
      "hours AS (", hours_sql, "), ",
      "sk AS (SELECT s.station, h.datetime ",
      "       FROM stations s CROSS JOIN hours h) ",
      "SELECT ",
      "  sk.datetime, sk.station, sr.station_code, ", yy, " AS year, ",
      "  AVG(CASE WHEN a.var='pm2.5'        THEN a.value END) AS \"pm2.5\", ",
      "  AVG(CASE WHEN a.var='pm10'         THEN a.value END) AS pm10, ",
      "  AVG(CASE WHEN a.var='no'           THEN a.value END) AS no, ",
      "  AVG(CASE WHEN a.var='no2'          THEN a.value END) AS no2, ",
      "  AVG(CASE WHEN a.var='co'           THEN a.value END) AS co, ",
      "  AVG(CASE WHEN a.var='co2'          THEN a.value END) AS co2, ",
      "  AVG(CASE WHEN a.var='nox'          THEN a.value END) AS nox, ",
      "  AVG(CASE WHEN a.var='so2'          THEN a.value END) AS so2, ",
      "  AVG(CASE WHEN a.var='ozone'        THEN a.value END) AS ozone, ",
      "  AVG(CASE WHEN a.var='presion baro' THEN a.value END) ",
      "    AS \"presion baro\", ",
      "  AVG(CASE WHEN a.var='rh'           THEN a.value END) AS rh, ",
      "  AVG(CASE WHEN a.var='vel viento'   THEN a.value END) ",
      "    AS \"vel viento\", ",
      "  AVG(CASE WHEN a.var='dir viento'   THEN a.value END) ",
      "    AS \"dir viento\", ",
      "  AVG(CASE WHEN a.var='precipitacion' THEN a.value END) ",
      "    AS precipitacion, ",
      "  AVG(CASE WHEN a.var='radiation'    THEN a.value END) ",
      "    AS radiation, ",
      "  AVG(CASE WHEN a.var='temperatura'  THEN a.value END) ",
      "    AS temperatura ",
      "FROM sk ",
      "LEFT JOIN st_ref sr ON sk.station = sr.station ",
      "LEFT JOIN aggr_y a ON sk.station = a.station ",
      " AND COALESCE(sr.station_code, a.station_code) = a.station_code ",
      " AND sk.datetime = a.datetime ",
      "GROUP BY 1,2,3 ",
      "ORDER BY sk.station, sk.datetime ",
      ") TO ",
      .ds(base),
      " (FORMAT PARQUET, COMPRESSION ", parquet_codec,
      ", PARTITION_BY (year), OVERWRITE_OR_IGNORE TRUE);"
    )
    
    DBI::dbExecute(con, sql_copy_y)
    DBI::dbExecute(con, "DROP TABLE aggr_y;")
    if (isTRUE(verbose)) message(sprintf("✓ Year %d done.", yy))
  }
  
  n_pq <- length(list.files(base, pattern = "\\.parquet$", recursive = TRUE))
  if (isTRUE(verbose)) message(sprintf("Parquet files written: %d", n_pq))
  if (n_pq == 0) stop("No Parquet files were written.")
  
  ds <- arrow::open_dataset(base)
  if (isTRUE(cleanup)) {
    invisible(lapply(csvs, function(f) try(unlink(f), TRUE)))
  }
  ds
}


# ============================================================================================
#  CDMX-specific functions for processing data - main function and tiny helpers
# ============================================================================================

# ============================================================================================
# Function: cdmx_merge_pollution_csvs
# @Arg  : downloads_folder  — folder with .csv files (recursive = TRUE)
# @Arg  : tz                — Olson tz for final relabel (default "America/Mexico_City")
# @Arg  : years             — integer vector of years to keep (UTC/local)
# @Arg  : cleanup           — remove source CSVs after success (default FALSE)
# @Arg  : out_dir, out_name — output location (dataset goes to "<name>_dataset/")
# @Arg  : write_parquet     — TRUE → write Parquet artifacts
#                             • memory: per-year partitioned dataset via {arrow}
#                             • duckdb: per-year partitioned dataset via COPY
# @Arg  : write_rds, write_csv — optional single-file extras (memory engine)
# @Arg  : verbose           — print progress (default TRUE)
# @Arg  : engine            — "auto" | "duckdb" | "memory"
#                             Auto: if RAM > 64 GB → "memory", else "duckdb"
# @Arg  : stations_keep_codes — NULL or character vector of station codes to keep
# @Return: Arrow Dataset handle (if Parquet written) else tibble.
# @Steps : (1) discover CSVs, (2) RAM detect + engine pick,
#          (3A) memory engine, (3B) duckdb engine.
# @Written_on: 20/08/2025
# @Written_by: Marcos Paulo
# ============================================================================================
cdmx_merge_pollution_csvs <- function(
    downloads_folder,
    tz               = "America/Mexico_City",
    years            = 2000:2023,
    cleanup          = FALSE,
    out_dir,
    out_name,
    write_parquet    = TRUE,
    write_rds        = FALSE,
    write_csv        = FALSE,
    verbose          = TRUE,
    engine           = c("auto","duckdb","memory"),
    stations_keep_codes = NULL
) {
  engine <- match.arg(engine)
  
  # ---- (1) discover CSV files -----------------------------------------------
  csvs <- list.files(
    downloads_folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE
  )
  if (!length(csvs)) stop("No CSV files found under: ", downloads_folder)
  yrs <- sort(unique(years))
  
  # ---- (2) RAM detection + engine rule --------------------------------------
  if (!requireNamespace("memuse", quietly = TRUE)) {
    stop("Package 'memuse' is required for RAM detection.")
  }
  ram_gb <- memuse::Sys.meminfo()[1]$totalram@size
  run_parallel <- (ram_gb > 31)
  
  if (engine == "auto") {
    engine <- if (ram_gb > 64) "memory" else "duckdb"
  }
  if (isTRUE(verbose)) {
    message(sprintf("Engine=%s | RAM=%.1f GB | years=%d | parallel=%s",
                    engine, ram_gb, length(yrs),
                    ifelse(run_parallel, "yes", "no")))
  }
  
  # ---- (3A) memory engine ----------------------------------------------------
  if (engine == "memory") {
    return(
      .cdmx_merge_memory_engine(
        csvs                = csvs,
        years               = yrs,
        tz                  = tz,
        out_dir             = out_dir,
        out_name            = out_name,
        write_parquet       = write_parquet,
        write_rds           = write_rds,
        write_csv           = write_csv,
        cleanup             = cleanup,
        verbose             = verbose,
        stations_keep_codes = stations_keep_codes
      )
    )
  }
  
  # ---- (3B) duckdb engine ----------------------------------------------------
  .cdmx_merge_duckdb_engine(
    csvs                = csvs,
    years               = yrs,
    tz                  = tz,
    out_dir             = out_dir,
    out_name            = out_name,
    cleanup             = cleanup,
    run_parallel        = run_parallel,
    verbose             = verbose,
    stations_keep_codes = stations_keep_codes
  )
}


# --------------------------------------------------------------------------------------------
# Function: cdmx_filter_stations_in_metro
# @Arg       : station_location — data.frame/tibble with lon/lat columns
# @Arg       : metro_area       — sf (MULTI)POLYGON of the metropolitan area
# @Arg       : radius_km        — numeric; max distance to keep (default 20)
# @Arg       : lon_col          — name of longitude column (default "lon")
# @Arg       : lat_col          — name of latitude column  (default "lat")
# @Arg       : stations_epsg    — EPSG for lon/lat (default 4326 WGS84)
# @Arg       : out_file          — where to write the cropped GeoPackage
# @Arg       : overwrite_gpkg    — logical; overwrite output GeoPackage if exists
# @Arg       : dissolve         — logical; TRUE unions metro polygons (default TRUE)
# @Arg       : verbose          — logical; TRUE prints summary (default TRUE)
# @Output    : sf POINT data.frame of stations filtered to inside/near metro_area
# @Purpose   : Keep stations that lie inside or within 'radius_km' km from the
#              metropolitan area polygon. Builds geometry from lon/lat, aligns
#              CRS for accurate distance, and filters using geodesic-safe
#              distances (projects to meters when needed).
# @Written_on: 15/09/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
cdmx_filter_stations_in_metro <- function(
    station_location,
    metro_area,
    radius_km      = 20,
    lon_col        = "lon",
    lat_col        = "lat",
    stations_epsg  = 4326,
    out_file       = here::here("data", "interim", "geospatial_data_filtered", "CDMX.gpkg"),
    overwrite_gpkg = TRUE,
    dissolve       = TRUE,
    verbose        = TRUE
) {
  # 0) deps and basic checks ---------------------------------------------------
  if (!inherits(metro_area, "sf"))
    stop("'metro_area' must be an sf object.")
  if (!all(c(lon_col, lat_col) %in% names(station_location)))
    stop("Columns ", lon_col, " and ", lat_col, " not found in stations.")

  # Guarantee output directory exists
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  # 1) drop NA coords; convert stations → sf POINT (lon/lat) ------------------
  st_df <- station_location[
    stats::complete.cases(station_location[, c(lon_col, lat_col)]), , drop = FALSE
  ]
  if (!nrow(st_df)) stop("No stations with non-missing lon/lat.")
  st_sf <- sf::st_as_sf(
    st_df,
    coords = c(lon_col, lat_col),
    crs    = stations_epsg,
    remove = FALSE
  )
  
  # 2) prepare metro polygon: valid geometry; optionally dissolve -------------
  metro_sf <- sf::st_make_valid(metro_area)
  if (dissolve) metro_sf <- sf::st_union(metro_sf)
  
  # 3) pick a working CRS in meters for accurate distance ---------------------
  #    If metro_area already uses a projected CRS (units in m), reuse it.
  #    Else build a local Azimuthal Equidistant centered on metro centroid.
  need_proj <- isTRUE(sf::st_is_longlat(sf::st_crs(metro_sf)))
  if (need_proj) {
    # 3a) compute centroid lon/lat for a local AEQD projection
    metro_wgs <- sf::st_transform(metro_sf, 4326)
    cen       <- sf::st_coordinates(sf::st_centroid(metro_wgs))
    aeqd_str  <- sprintf(
      "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
      cen[2], cen[1]
    )
    target_crs <- sf::st_crs(aeqd_str)
  } else {
    target_crs <- sf::st_crs(metro_sf)
  }
  
  # 4) transform both layers to target_crs (meters) ---------------------------
  metro_m <- if (need_proj) sf::st_transform(metro_sf, target_crs) else metro_sf
  st_m    <- sf::st_transform(st_sf, target_crs)
  
  # 5) compute keep mask: inside OR within radius_km (meters) -----------------
  radius_m <- radius_km * 1000
  
  # Fast: within-distance against dissolved polygon
  within_dist <- sf::st_is_within_distance(st_m, metro_m, dist = radius_m)
  keep <- lengths(within_dist) > 0
  
  # 6) subset; report; return sf ----------------------------------------------
  out <- st_m[keep, , drop = FALSE]
  if (isTRUE(verbose)) {
    message("Stations input: ", nrow(st_sf),
            " | kept: ", nrow(out),
            " | radius: ", radius_km, " km")
  }
  
  # 7) return in original lon/lat CRS (often handy) -----------------
  out <- sf::st_transform(out, stations_epsg)
  
  # 8) Write GeoPackage
  if (file.exists(out_file) && !overwrite_gpkg) {
    if (!verbose) message("↪︎ Output exists and overwrite_gpkg=FALSE: ", out_file)
  } else {
    if (!verbose) message("💾 Writing GeoPackage → ", out_file)
    if (file.exists(out_file) && overwrite_gpkg) unlink(out_file, force = TRUE)
    suppressMessages(sf::st_write(out, out_file, verbose = TRUE))
  }
  
  # 9) Friendly summary
  if (!verbose) {
    message("✅ Done. Selected ", row(out), " stations that at most", radius_km, 
            " km from the metro area")
  }

  return(out)
}
