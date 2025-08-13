# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
# 
# @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "download_data" scripts.
# 
# @Date: Apr 2025
# @Author: Marcos Paulo
# ============================================================================================

# List of required packages
packages <- c(
  "arrow",
  "curl",
  "dplyr",
  "fs",
  "glue",
  "here",
  "httr",
  "jsonlite",
  "lubridate",
  "purrr",
  "readr",
  "rvest",
  "selenium",
  "stringr",
  "tidyr"
  )

# Define the default source library for packages installation - may have problems otherwise
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install (if needed) and load packages
toy_protect <- requireNamespace("renv", quietly = TRUE)
if (!toy_protect) stop("Please ensure renv is installed before running this script.")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    renv::install(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Clear objects on environment
rm(packages, pkg, toy_protect)

# Try installing tidyverse in different cran
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  renv::install("tidyverse")
  library(readxl)
  }

# ############################################################################################
# Functions
# ############################################################################################

# --------------------------------------------------------------------------------------------
# Function: generate_merra2_urls
# @Arg         : start_date is a Date or character (YYYY-MM-DD) referring to the beginning date
# @Arg         : end_date is a Date or character (YYYY-MM-DD) referring to the end date
# @Arg         : dataset_version is a character (e.g. "M2T1NXAER.5.12.4") referring to the 
#                MERRA2 version version and type of archive
# @Arg         : tile_id is a character (e.g. "400") referring to the processing stream/
#                version of the reanalysis output
# @Arg         : var_name is a character (e.g. "tavg1_2d_aer_Nx") referring to the variable 
#                collection
# @Output      : A character vector containing the full URLs
# @Purpose     : Build daily MERRA-2 .nc4 URLs for a given dataset, stream, and variable
# @Written_on  : 01/04/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
generate_merra2_urls <- function(start_date,
                                 end_date,
                                 dataset_version,
                                 tile_id,
                                 var_name) {
  # Parse and validate dates
  d0 <- lubridate::ymd(start_date)
  d1 <- lubridate::ymd(end_date)
  if (is.na(d0) || is.na(d1) || d1 < d0) {
    stop("Dates must be valid and end_date >= start_date")
  }
  
  # Generate a sequence of daily dates
  dates <- seq(d0, d1, by = "day")
  
  # Create the base URL for dataset
  base_url <- glue::glue(
    "https://data.gesdisc.earthdata.nasa.gov/data/MERRA2/{dataset_version}"
  )
  
  # Build URLs for each date ---------------------------------------------------------------
  urls <- purrr::map_chr(dates, function(d) {
    yyyy <- lubridate::year(d)                        # Year component
    mm   <- sprintf("%02d", lubridate::month(d))      # Month component
    ymd  <- format(d, "%Y%m%d")                       # Date string YYYYMMDD
    
    # Produce the filename term: MERRA2_<tile_id>.<var_name>.<YYYYMMDD>.nc4
    fname <- glue::glue(
      "MERRA2_{tile_id}.{var_name}.{ymd}.nc4"
    )
    
    # Join everything to create the full URL: base_url/YYYY/MM/fname
    glue::glue("{base_url}/{yyyy}/{mm}/{fname}")
  })
  
  return(urls)
}


# --------------------------------------------------------------------------------------------
# Function: download_merra2_files
# @Arg       : urls      is a character vector of download URLs
# @Arg       : dest_dir  is a directory path where files will be saved
# @Arg       : user      is a character for the Earthdata username (character) 
#                        [optional if using ~/.netrc]
# @Arg       : pass      is a character for the Earthdata password (character) 
#                        [optional if using ~/.netrc]
# @Arg       : overwrite is a logical term, whether to overwrite existing files
# @Output    : logical vector indicating success (TRUE) or failure (FALSE)
# @Purpose   : This function make the download of files using curl::curl_download() + .netrc 
#              (or basic auth) to fetch each URL - this is required for Earthdata - NASA files
# @Written_on: 05/04/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
download_merra2_files <- function(urls,
                                  dest_dir,
                                  user      = Sys.getenv("EARTHDATA_USER"),
                                  pass      = Sys.getenv("EARTHDATA_PASS"),
                                  overwrite = FALSE) {
  # Prepare directory
  fs::dir_create(dest_dir)
  
  # Set up a curl handle for authentication
  #    Preferred: credentials in ~/.netrc
  #    Fallback: explicit user:pass with basic auth
  h <- curl::new_handle()
  if (file.exists(path.expand("~/.netrc"))) {
    curl::handle_setopt(h, netrc = TRUE)
  } else if (nzchar(user) && nzchar(pass)) {
    curl::handle_setopt(h,
                        userpwd   = paste0(user, ":", pass),
                        httpauth  = 1)      # CURLAUTH_BASIC
  } else {
    stop("No Earthdata credentials found.  Please set EARTHDATA_USER/PASS or create ~/.netrc.")
  }
  
  # Download loop
  results <- purrr::map_lgl(urls, function(u) {
    fname <- fs::path_file(u)
    out   <- fs::path(dest_dir, fname)
    
    # skip if exists
    if (fs::file_exists(out) && !overwrite) {
      message("[skipped] ", fname)
      return(TRUE)
    }
    
    # Perform download
    success <- tryCatch({
      curl::curl_download(
        url      = u,
        destfile = out,
        handle   = h,
        quiet    = FALSE,
        mode     = "wb")

      # Tiny files (<1MB) are (almost certainly) an error page - it isn't the needed file
      if (fs::file_info(out)$size < 1e6) {
        warning("File too small (", fs::file_info(out)$size,
                " bytes), likely an error page: ", fname)
        return(FALSE)
        }
      # Message for success
      message("[ok]      ", fname)
      TRUE
    }, error = function(e) {
        warning("Error downloading ", fname, ": ", e$message)
        FALSE
        }
    )
    # Return of the inner function (results with function inside map_lgl)
    return(success)
  })

  # Return of the outer function (download_files)
  return(results)
}


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
# Helpers functions: waiting, setting inputs, downloads, sanitation
# --------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------
# Function: wait_ready
# @Arg       : session      — selenium::SeleniumSession; active browser session
# @Arg       : timeout      — integer; max seconds to wait (default 30)
# @Output    : invisible(TRUE) on success; error on timeout
# @Purpose   : Wait until document.readyState is "complete" (page fully loaded).
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
wait_ready <- function(session, timeout = 30) {
  t0 <- Sys.time()
  repeat {
    state <- try(session$execute_script("return document.readyState")[[1]], silent = TRUE)
    if (!inherits(state, "try-error") && identical(state, "complete")) return(invisible(TRUE))
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop("Timed out waiting for page readyState=complete")
    Sys.sleep(0.2)
  }
}
# --------------------------------------------------------------------------------------------
# Function: wait_for
# @Arg       : session         — selenium::SeleniumSession; active browser session
# @Arg       : by              — string; locator strategy ("css selector", "xpath", etc.)
# @Arg       : query           — string; selector/xpath
# @Arg       : timeout         — integer; max seconds to wait (default 30)
# @Arg       : require_visible — logical; TRUE requires element to be displayed (default TRUE)
# @Output    : selenium element handle (on success); error on timeout
# @Purpose   : Explicitly wait for an element to exist (and optionally be visible).
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
wait_for <- function(session, by, query, timeout = 30, require_visible = TRUE) {
  t0 <- Sys.time()
  repeat {
    el <- try(session$find_element(by, query), silent = TRUE)
    if (!inherits(el, "try-error")) {
      if (!require_visible) return(el)
      vis <- try(el$is_displayed()[[1]], silent = TRUE)
      if (!inherits(vis, "try-error") && isTRUE(vis)) return(el)
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop(sprintf("Timed out waiting for %s = `%s`", by, query))
    Sys.sleep(0.25)
  }
}
# --------------------------------------------------------------------------------------------
# Function: css_from_id
# @Arg       : input_id  — string; DOM id (e.g., "startDate") or CSS "#startDate"
# @Output    : string; CSS selector that begins with "#"
# @Purpose   : Normalize an input id into a CSS selector.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
css_from_id <- function(input_id) {
  if (startsWith(input_id, "#")) input_id else paste0("#", input_id)
}
# --------------------------------------------------------------------------------------------
# Function: click_clear_and_type
# @Arg       : session   — selenium::SeleniumSession; active browser session
# @Arg       : input_id  — string; DOM id (or "#id") of the input
# @Arg       : text      — string; value to type into the input
# @Arg       : blur      — logical; send Tab after typing (default TRUE)
# @Arg       : pause     — numeric; seconds to pause after typing (default 0.2)
# @Output    : invisible(TRUE)
# @Purpose   : Clicks an input, selects all (Ctrl+Shift+Home), clears, types text, tabs out.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
click_clear_and_type <- function(session, input_id, text, blur = TRUE, pause = 0.2) {
  css <- css_from_id(input_id)
  el  <- session$find_element("css selector", css)
  el$click()$send_keys(
    key_chord(keys$control, keys$shift, keys$home),
    key_chord(keys$backspace)
  )
  el$send_keys(text)
  if (isTRUE(blur)) el$send_keys(keys$tab)
  if (pause > 0) Sys.sleep(pause)
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function: click_date_input
# @Arg       : session   — selenium::SeleniumSession; active browser session
# @Arg       : input_id  — string; DOM id (or "#id") of a date input
# @Arg       : day       — integer; 1–31
# @Arg       : month     — integer; 1–12
# @Arg       : year      — integer; 4-digit year
# @Arg       : pause     — numeric; seconds to pause after typing (default 0.2)
# @Output    : invisible(TRUE)
# @Purpose   : Types a date as "DD-MM-YYYY" using click-clear-type sequence.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
click_date_input <- function(session, input_id, day, month, year, pause = 0.2) {
  val <- sprintf("%02d-%02d-%04d", day, month, year)
  click_clear_and_type(session, input_id, val, blur = TRUE, pause = pause)
}
# --------------------------------------------------------------------------------------------
# Function: click_time_input
# @Arg       : session   — selenium::SeleniumSession; active browser session
# @Arg       : input_id  — string; DOM id (or "#id") of a time input
# @Arg       : hh        — integer; hour 00–23
# @Arg       : mm        — integer; minute 00–59
# @Arg       : pause     — numeric; seconds to pause after typing (default 0.2)
# @Output    : invisible(TRUE)
# @Purpose   : Types a time as "HH:MM" using click-clear-type sequence.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
click_time_input <- function(session, input_id, hh, mm, pause = 0.2) {
  val <- sprintf("%02d:%02d", hh, mm)
  click_clear_and_type(session, input_id, val, blur = TRUE, pause = pause)
}
# --------------------------------------------------------------------------------------------
# Function: click_end_date
# @Arg       : session   — selenium::SeleniumSession; active browser session
# @Arg       : year      — integer; year to use (defaults day=31, month=12)
# @Arg       : day       — integer; default 31
# @Arg       : month     — integer; default 12
# @Arg       : pause     — numeric; seconds to pause after typing (default 0.2)
# @Output    : invisible(TRUE)
# @Purpose   : Convenience wrapper: force-type the end date (e.g., "31-12-year") into #endDate.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
click_end_date <- function(session, year, day = 31, month = 12, pause = 0.2) {
  click_date_input(session, "endDate", day, month, year, pause = pause)
}
# --------------------------------------------------------------------------------------------
# Function: click_set_period
# @Arg       : session   — selenium::SeleniumSession; active browser session
# @Arg       : sd, sm, sy — integers; start day, month, year
# @Arg       : sh, smin  — integers; start hour, minute
# @Arg       : ed, em, ey — integers; end day, month, year
# @Arg       : eh, emin  — integers; end hour, minute
# @Arg       : pause     — numeric; seconds to pause between inputs (default 0.2)
# @Output    : invisible(TRUE)
# @Purpose   : Set a full period reliably:
#              startDate → startTime → endDate → endTime → re-apply endTime to lock value.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
click_set_period <- function(session,
                             sd, sm, sy, sh, smin,
                             ed, em, ey, eh, emin,
                             pause = 0.2) {
  click_date_input(session, "startDate", sd, sm, sy, pause = pause)
  click_time_input(session, "startTime", sh, smin, pause = pause)
  click_date_input(session, "endDate",   ed, em, ey, pause = pause)
  click_time_input(session, "endTime",   eh, emin,  pause = pause)
  # Re-apply endTime after endDate (site sometimes resets the time on blur)
  click_time_input(session, "endTime",   eh, emin,  pause = pause)
  invisible(TRUE)
}
# --------------------------------------------------------------------------------------------
# Function: robust_click_excel
# @Arg       : session    — selenium::SeleniumSession; active browser session
# @Arg       : timeout_btn— integer; seconds to wait for the Excel control (default 30)
# @Output    : invisible(TRUE); raises if not clickable
# @Purpose   : Reliably click the Excel export control. Tries the inner <a> first; falls back
#              to JS scroll + click on the container. Retries a few times for overlays.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
robust_click_excel <- function(session, timeout_btn = 30) {
  # wait for the container; then prefer any <a> inside it
  cont <- wait_for(session, "css selector", "div.LinksReport.Excel", timeout_btn)
  tries <- 0L
  repeat {
    tries <- tries + 1L
    # re-query fresh each try (avoids stale element)
    cont <- wait_for(session, "css selector", "div.LinksReport.Excel", timeout_btn)
    links <- try(cont$find_elements("css selector", "a"), silent = TRUE)
    ok <- FALSE
    # 1) try normal click on <a>
    if (!inherits(links, "try-error") && length(links)) {
      el <- links[[1]]
      ok <- !inherits(try(el$click(), silent = TRUE), "try-error")
    }
    # 2) fall back to JS click on the container
    if (!ok) {
      js <- paste0(
        "var el=document.querySelector('div.LinksReport.Excel');",
        "if(!el) return false;",
        "el.scrollIntoView({block:'center'});",
        "el.click(); return true;"
      )
      ok <- isTRUE(try(session$execute_script(js)[[1]], silent = TRUE))
    }
    if (ok) return(invisible(TRUE))
    if (tries >= 5L) stop("Excel control not clickable after retries.")
    Sys.sleep(0.6)
  }
}
# --------------------------------------------------------------------------------------------
# Function: wait_for_download
# @Arg       : dir        — string; directory to watch
# @Arg       : pattern    — string; regex for final filename (default "\\.xlsx$")
# @Arg       : quiet_sec  — integer; seconds with no size change to deem "finished" (default 2)
# @Arg       : timeout    — integer; max seconds to wait (default 180)
# @Output    : string; full path of the newest completed file
# @Purpose   : Wait until a new download appears in `dir` and finishes (no .part/.tmp).
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
wait_for_download <- function(dir, pattern = "\\.xlsx$", quiet_sec = 2, timeout = 180) {
  t0 <- Sys.time()
  last_seen <- character(0)
  last_size <- 0L
  repeat {
    files <- list.files(dir, pattern = pattern, full.names = TRUE)
    # ignore partials
    files <- files[!grepl("\\.(part|tmp)$", files, ignore.case = TRUE)]
    if (length(files)) {
      newest <- files[which.max(file.info(files)$mtime)]
      size   <- file.info(newest)$size
      if (identical(newest, last_seen) && size == last_size) {
        # size hasn't changed since last tick → quiet period?
        if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > quiet_sec) return(newest)
      } else {
        t0 <- Sys.time(); last_seen <- newest; last_size <- size
      }
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop("Timed out waiting for download to complete.")
    Sys.sleep(0.5)
  }
}
# --------------------------------------------------------------------------------------------
# Function: sanitize_name
# @Arg       : x        — character; arbitrary station name
# @Output    : character; ASCII, lowercase, filesystem-safe token (words joined by "_")
# @Purpose   : Normalize station names for deterministic filenames like STATION_YEAR.xlsx.
# @Written_on: 05/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
sanitize_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}
# --------------------------------------------------------------------------------------------
# End of Helpers functions: waiting, setting inputs, downloads, sanitation
# --------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------
# Function: bogota_download_station_data
# @Arg         : base_url     — string; URL of the station‐report form page
# @Arg         : start_year   — integer; first year to download (e.g. 2000)
# @Arg         : end_year     — integer; last year to download (e.g. 2023)
# @Arg         : container    — logical; TRUE if running inside Docker/Selenium compose
# @Arg         : stations_idx — integer vector|NULL; which <li> indices to download (NULL=all)
# @Arg         : max_attempts — integer; retries per (station, year) (default 3)
# @Arg         : timeout_page — integer; seconds to wait page ready (default 30)
# @Arg         : timeout_btn  — integer; seconds to wait buttons visible (default 30)
# @Arg         : timeout_dl   — integer; seconds to wait per download (default 240)
# @Output      : writes XLSX files using the site’s random filenames; returns (invisibly)
#                a log tibble with columns: station, year, part, status, file
# @Purpose     : Same as before, but WITHOUT deterministic renaming and WITHOUT skip checks.
#                For each year it also downloads the missing last day:
#                31-12-yr 00:00 → 01-01-(yr+1) 00:00.
# @Written_on  : 05/08/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
bogota_download_station_data <- function(base_url,
                                         start_year,
                                         end_year,
                                         container     = TRUE,
                                         stations_idx  = NULL,
                                         max_attempts  = 3,
                                         timeout_page  = 30,
                                         timeout_btn   = 30,
                                         timeout_dl    = 240) {

  # 0) target folder
  downloads_folder <- Sys.getenv("DOWNLOADS_DIR",
                                 here::here("data","downloads"))
  dir.create(downloads_folder, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Downloads dir: ", downloads_folder)

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
    ul2 <- wait_for(session, "css selector", "#StationsMonitorsList > ul",
                    timeout_btn)
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
        ok <- TRUE
        try({
          # reload base page each attempt (avoids stale state)
          session$navigate(base_url)
          wait_ready(session, timeout_page)
          
          # Report period → Personalizado
          wait_for(
            session, "css selector", "#select-reportperiod > li:nth-child(6)", timeout_btn
          )$click()
          
          # Personalized period tab is already selected at this point
          click_date_input(session, "startDate", 1, 1,  yr)
          click_time_input(session, "startTime", 0, 0)
          click_end_date(session, year = yr, day = 31, month = 12)
          click_time_input(session, "endTime",   23, 0)
          # Safety: re-apply end time to prevent the "snap back to 01:00" bug
          click_time_input(session, "endTime",   23, 0)
          
          # Find station id
          lis  <- wait_for(
            session, "css selector", "#StationsMonitorsList > ul", timeout_btn
          )$find_elements("css selector", "li.k-item")
          item <- lis[[i]]
          chk  <- item$find_element(
            "xpath", ".//input[contains(@class,'k-checkbox')]"
          )
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
          
          # Mostrar → Excel
          wait_for(session, "xpath", '//*[@id="buttonsWrapper"]/input[2]', timeout_btn)$click()
          robust_click_excel(session, timeout_btn)
          
          # Wait for the site’s random-named XLSX to finish
          newest_path2 <- wait_for_download(
            downloads_folder,
            pattern = "\\.xlsx$",
            quiet_sec = 2,
            timeout = timeout_dl)
        message("      ✅ ", basename(newest_path2))
        }, silent = TRUE) -> res2     # End of TRY
        
        if (!inherits(res2, "try-error")) {
          log[[length(log) + 1L]] <- tibble::tibble(
            station = station_name, year = yr, part = "dec31",
            status  = "ok",         file = newest_path2
          )
          break
        } else if (attempt < max_attempts) {
          back <- min(30, 2 ^ attempt)
          message(sprintf("      ⚠️  Failed; backoff %ds, retrying…", back))
          Sys.sleep(back)
        } else {
          message("      ❌ Failed after max attempts (dec31).")
          log[[length(log) + 1L]] <- tibble::tibble(
            station = station_name, year = yr, part = "dec31",
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
        ok <- TRUE
        newest_path2 <- NA_character_
        try({
          session$navigate(base_url)
          wait_ready(session, timeout_page)
          
          wait_for(session, "css selector",
                   "#select-reportperiod > li:nth-child(6)", timeout_btn)$click()
          
          # Find station id again
          lis  <- wait_for(
            session, "css selector", "#StationsMonitorsList > ul", timeout_btn
          )$find_elements("css selector", "li.k-item")
          item <- lis[[i]]
          chk  <- item$find_element(
            "xpath", ".//input[contains(@class,'k-checkbox')]"
          )
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
          
          # Mostrar → Excel
          wait_for(session, "xpath", '//*[@id="buttonsWrapper"]/input[2]', timeout_btn)$click()
          robust_click_excel(session, timeout_btn)

          newest_path2 <- wait_for_download(
            downloads_folder, pattern = "\\.xlsx$", quiet_sec = 2,
            timeout = timeout_dl
          )
          message("      ✅ ", basename(newest_path2))
        }, silent = TRUE) -> res2
        
        if (!inherits(res2, "try-error")) {
          log[[length(log)+1]] <- tibble::tibble(
            station = station_name, year = yr, part = "dec31",
            status = "ok", file = newest_path2
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
            status = "failed", file = NA_character_
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

# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")