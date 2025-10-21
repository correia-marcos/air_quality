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
pkgs <- c(
  "arrow",
  "curl",
  "dplyr",
  "fs",
  "glue",
  "here",
  "httr",
  "janitor",
  "jsonlite",
  "lubridate",
  "purrr",
  "readr",
  "readxl",
  "rvest",
  "selenium",
  "sf",
  "stringr",
  "tibble",
  "tools",
  "tidyr",
  "xml2",
  "zip"
  )

# Strict check: fail fast if something isn't in the project library
ensure_installed <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop(
      "Missing packages: ", paste(miss, collapse = ", "),
      ". Run renv::restore() (or install locally with renv::install() then renv::snapshot())."
    )
  }
}

ensure_installed(pkgs)

# Attach (quiet)
invisible(lapply(pkgs, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# no repo tweaking, no renv::install() here
rm(pkgs, ensure_installed)

# ============================================================================================
# Helpers functions: waiting, setting inputs, downloads, sanitation
# ============================================================================================

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
  .Deprecated("wait_for_new_download",
              msg = "Use wait_for_new_download(dir, before = list.files(dir, pattern, TRUE)).")
  before <- list.files(dir, pattern = pattern, full.names = TRUE)
  wait_for_new_download(dir, before = before,
                        pattern = pattern, quiet_sec = quiet_sec, timeout = timeout)
  
}
# --------------------------------------------------------------------------------------------
# Function: wait_for_new_download
# @Arg       : dir        — string; directory to watch
# @Arg       : before     — character; vector of file paths seen before clicking
# @Arg       : pattern    — string; regex for final filename (default "\\.xlsx$")
# @Arg       : quiet_sec  — integer; secs with no size change to deem "finished" (default 2)
# @Arg       : timeout    — integer; max secs to wait (default 180)
# @Output    : string; full path of the new completed file (not present in `before`)
# @Purpose   : Wait until a new file appears in `dir` and finishes writing. Guarantees the
#              returned file was not present in `before` (avoids reusing a prior download).
# @Written_on: 13/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
wait_for_new_download <- function(dir,
                                  before,
                                  pattern   = "\\.xlsx$",
                                  quiet_sec = 2,
                                  timeout   = 180) {
  t_start <- Sys.time()
  candidate <- NULL
  last_size <- -1L
  t_quiet  <- Sys.time()
  
  repeat {
    now <- list.files(dir, pattern = pattern, full.names = TRUE)
    new <- setdiff(now, before)
    
    if (length(new)) {
      # watch the latest-new file
      fi <- file.info(new)
      cand <- new[which.max(fi$mtime)]
      size <- file.info(cand)$size
      
      if (!identical(cand, candidate)) {
        candidate <- cand
        last_size <- size
        t_quiet   <- Sys.time()
      } else {
        if (size == last_size &&
            as.numeric(difftime(Sys.time(), t_quiet, units = "secs")) >= quiet_sec) {
          return(candidate)
        }
        if (size != last_size) {
          last_size <- size
          t_quiet   <- Sys.time()
        }
      }
    }
    
    if (as.numeric(difftime(Sys.time(), t_start, units = "secs")) > timeout) {
      stop("Timed out waiting for new download in: ", dir)
    }
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


# ============================================================================================
# Main Functions
# ============================================================================================

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
# End of Helpers functions: waiting, setting inputs, downloads, sanitation
# --------------------------------------------------------------------------------------------

# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")