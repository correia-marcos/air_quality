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
# ============================================================================================

# Parameters (single source)
bogota_cfg <- list(
  id              = "bogota",
  tz              = "America/Bogota",
  base_url_rmcab  = "http://rmcab.ambientebogota.gov.co/Report/stationreport",
  base_url_census = "https://microdatos.dane.gov.co/index.php/catalog/421",
  years           = 2000L:2023L,
  dl_dir          = here::here("data", "downloads", "Bogota"),
  out_dir         = here::here("data", "raw",       "Bogota")
)

# ============================================================================================
#  Bogotá-specific functions - downloading and its helpers
# ============================================================================================

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
# @Arg       : url    — string; DANE catalog page
# @Arg       : type   — string; one of c("BASICO","AMPLIADO")
# @Output    : list(label, filename, href) for the requested resource
# @Purpose   : Extract the 'mostrarModal("FILE.zip","https://.../download/ID")' link.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_find_resource_census <- function(url, type = c("BASICO","AMPLIADO")) {
  type <- match.arg(toupper(type), c("BASICO","AMPLIADO"))
  
  pg   <- rvest::read_html(url)
  # Pull every onclick="mostrarModal('CG2005_*.zip','https://.../download/NNN')"
  onclicks <- pg |>
    rvest::html_elements(
      xpath = "//input[@type='image' and contains(@onclick,'mostrarModal')]"
      ) |>
    rvest::html_attr("onclick")
  
  if (length(onclicks) == 0L) {
    stop("No download inputs found; page markup may have changed or requires login.")
  }
  
  # Regex: grab filename (group 1) and download URL (group 2)
  m <- stringr::str_match(
    onclicks,
    "mostrarModal\\('([^']+\\.zip)'\\s*,\\s*'([^']+?/download/\\d+)"
  )
  
  # Keep rows with a match
  m <- m[stats::complete.cases(m), , drop = FALSE]
  if (nrow(m) == 0L) stop("Could not extract any 'download/<id>' links.")
  
  df <- tibble::tibble(
    filename = m[, 2],
    href     = m[, 3],
    label    = tools::file_path_sans_ext(m[, 2])
  )
  
  wanted <- paste0("CG2005_", type)
  cand   <- df[stringr::str_detect(df$label, stringr::fixed(wanted)), ]
  
  if (!nrow(cand)) {
    stop("Could not find resource labeled ", wanted,
         ". Available: ", paste(df$label, collapse = ", "))
  }
  
  # If multiple, keep the first visible occurrence.
  cand[1, c("label","filename","href")] |> as.list()
}


# --------------------------------------------------------------------------------------------
# Function: bogota_download_census_data
# @Arg       : type            — "BASICO" or "AMPLIADO"
# @Arg       : url             — catalog page
# @Arg       : download_folder — where to save ZIP (e.g., here('data','downloads','Census'))
# @Arg       : overwrite       — logical; re-download if file exists (default FALSE)
# @Arg       : retries         — integer; max HTTP retries (default 5)
# @Arg       : quiet           — logical; suppress progress (default FALSE)
# @Output    : tibble with columns: type, file_path, bytes, status
# @Purpose   : Resolve the link and download CG2005 microdata ZIP robustly.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_download_census_data <- function(
    type,
    url = "https://microdatos.dane.gov.co/index.php/catalog/421",
    download_folder = here::here('data','downloads','Census'),
    overwrite = FALSE,
    retries = 5,
    quiet = FALSE) {
  dir.create(download_folder, recursive = TRUE, showWarnings = FALSE)
  
  # I) Resolve which resource to fetch
  res <- bogota_find_resource_census(url = url, type = type)
  dest <- file.path(download_folder, res$filename)
  
  if (file.exists(dest) && !isTRUE(overwrite)) {
    message("↪︎ Already present: ", basename(dest), " (skip).")
    sz <- tryCatch(file.size(dest), error = function(e) NA_real_)
    return(tibble::tibble(
      type      = toupper(type),
      file_path = normalizePath(dest),
      bytes     = sz,
      status    = "cached"
    ))
  }
  
  # II) Robust download
  ua <- httr::user_agent(
    sprintf("R (%s) / IDB-AirMonitoring", paste(R.version$platform, R.version$version.string))
  )
  # Allow very large files; set long timeout
  rq <- httr::RETRY(
    verb = "GET",
    url  = res$href,
    ua,
    httr::write_disk(path = dest, overwrite = TRUE),
    httr::progress(type = if (quiet) "none" else "down"),
    times = as.integer(retries),
    terminate_on = c(200L),
    quiet = quiet,
    timeout(60 * 60)  # up to 1h wall time (adjust as needed)
  )
  
  code <- httr::status_code(rq)
  if (code != 200L) {
    warning("Download failed with HTTP ", code, " for ", res$href)
    return(tibble::tibble(
      type      = toupper(type),
      file_path = NA_character_,
      bytes     = NA_real_,
      status    = paste0("http_", code)
    ))
  }
  
  # III) Basic validation
  bytes <- tryCatch(file.size(dest), error = function(e) NA_real_)
  if (is.na(bytes) || bytes < 1e6) {
    warning("Downloaded file seems too small: ", dest, " (", bytes, " bytes)")
  }
  
  message("✅ Downloaded ", basename(dest),
          " (", format(structure(bytes, class="object_size")), ")")
  
  tibble::tibble(
    type      = toupper(type),
    file_path = normalizePath(dest),
    bytes     = bytes,
    status    = "ok"
  )
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
# Function: bogota_merge_downloads
# @Arg       : downloads_folder — string; directory containing *.xlsx exports
# @Arg       : out_dir          — string; directory to write outputs (created if missing)
# @Arg       : out_name         — string|NULL; base filename *without* extension.
#                                 If NULL, will use "bogota_stations_<minyear>_<maxyear>"
# @Arg       : write_rds        — logical; write .rds (default TRUE)
# @Arg       : write_parquet    — logical; write .parquet via {arrow} (default TRUE)
# @Arg       : write_csv_gz     — logical; write .csv.gz (default FALSE)
# @Arg       : cleanup          — logical; TRUE deletes the .xlsx after merging (default TRUE)
# @Arg       : tz               — string; Olson timezone for datetime parsing (default
#                                 "America/Bogota")
# @Output    : (invisible) tibble; all files row-bound, sorted and de-duplicated by
#              (station, datetime). Side-effects: writes selected artifacts to `out_dir`.
# @Purpose   : Read every XLSX via bogota_read_one_xlsx(), stack, sort, de-dup, and persist
#              to disk as RDS / Parquet / CSV.GZ with a consistent base filename.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_merge_downloads <- function(downloads_folder,
                                   out_dir,
                                   out_name = NULL,
                                   write_rds = TRUE,
                                   write_parquet = TRUE,
                                   write_csv_gz = FALSE,
                                   cleanup = TRUE,
                                   tz = "America/Bogota") {
  # 1) enumerate target files
  files <- list.files(downloads_folder, pattern = "\\.xlsx$", full.names = TRUE)
  if (length(files) == 0L) stop("No .xlsx files found in ", downloads_folder)
  
  # 2) read + stack
  big_tbl <- purrr::map_dfr(files, function(p) bogota_read_one_xlsx(p, tz = tz))
  
  # 3) sort + de-dup (safety for overlapping or re-downloaded years)
  big_tbl <- big_tbl |>
    dplyr::arrange(.data$station, .data$datetime) |>
    dplyr::distinct(.data$station, .data$datetime, .keep_all = TRUE)
  
  # 4) infer default output name if not provided
  yr_min <- suppressWarnings(min(big_tbl$year, na.rm = TRUE))
  yr_max <- suppressWarnings(max(big_tbl$year, na.rm = TRUE))
  if (is.null(out_name) || !nzchar(out_name)) {
    if (is.finite(yr_min) && is.finite(yr_max)) {
      out_name <- sprintf("bogota_stations_%d_%d", yr_min, yr_max)
    } else {
      out_name <- "bogota_stations"
    }
  }
  
  # 5) ensure output dir exists
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 6) materialize artifacts as requested
  if (isTRUE(write_rds)) {
    rds_path <- file.path(out_dir, paste0(out_name, ".rds"))
    saveRDS(big_tbl, rds_path, compress = "xz")
    message("💾 Wrote RDS → ", rds_path)
  }
  
  if (isTRUE(write_parquet)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for Parquet output. Install it
           (e.g., renv::install('arrow')).")
    }
    pq_path <- file.path(out_dir, paste0(out_name, ".parquet"))
    arrow::write_parquet(big_tbl, pq_path, compression = "zstd")
    message("🧱 Wrote Parquet → ", pq_path)
  }
  
  if (isTRUE(write_csv_gz)) {
    csv_path <- file.path(out_dir, paste0(out_name, ".csv.gz"))
    con <- gzfile(csv_path, open = "wt")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    readr::write_csv(big_tbl, con)
    message("📝 Wrote CSV.GZ → ", csv_path)
  }
  
  # 7) optional cleanup of raw XLSX
  if (isTRUE(cleanup)) {
    file.remove(files)
    message("🗑️  Removed original .xlsx files.")
  }
  
  # 8) return invisibly
  return(invisible(big_tbl))
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
  id       = "bogota",
  cfg      = bogota_cfg,
  download = bogota_download,
  process  = bogota_process
)