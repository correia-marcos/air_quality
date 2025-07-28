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
  "curl",
  "dplyr",
  "fs",
  "glue",
  "here",
  "httr",
  "jsonlite",
  "lubridate",
  "purrr",
  "rvest",
  "readr",
  "remotes",
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

# Get special version of Selenium
remotes::install_github("ropensci/RSelenium")
library(RSelenium)
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
# Function: start_selenium
# @Arg      : jar_path — path to your selenium-server JAR (e.g. from Sys.getenv("SELENIUM_JAR"))
# @Arg      : port     — integer; port for the server to listen on (default 4444)
# @Arg      : browser  — character; "chrome" or "firefox" (default "chrome")
# @Output    : named list with
#                • process: processx handle for chromedriver or selenium-server
#                • client : open RSelenium remoteDriver
# @Purpose   : Start Selenium 4 JAR, then open an RSelenium session, trying both the W3C root
#              (“/”) and legacy “/wd/hub” paths.
# @Written_on: 20/06/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
start_selenium <- function(
    jar_path = Sys.getenv("SELENIUM_JAR"),
    port     = 4444L,
    browser  = "chrome"
) {
  # 1) decide ChromeDriver vs full Selenium JAR
  use_chrome <- (
    identical(tolower(browser), "chrome") &&
      nzchar(Sys.which("chromedriver"))
  )
  if (use_chrome) {
    message("✅ Launching ChromeDriver: ", Sys.which("chromedriver"))
    px          <- processx::process$new(
      Sys.which("chromedriver"),
      args   = paste0("--port=", port),
      stdout = "|", stderr = "|"
    )
    server_name  <- "ChromeDriver"
    paths_to_try <- c("", "wd/hub")
  } else {
    # fallback to Selenium .jar
    if (!nzchar(jar_path) || !file.exists(jar_path))
      stop("🛑 SELENIUM_JAR not found or invalid.")
    if (Sys.which("java") == "")
      stop("🛑 `java` not on PATH.")
    message("✅ Launching Selenium Server: ", jar_path)
    px           <- processx::process$new(
      "java",
      c("-jar", jar_path, "-port", port),
      stdout = "|", stderr = "|"
    )
    server_name  <- "Selenium Server"
    paths_to_try <- c("wd/hub", "")
  }
  
  # 2) Give it a moment to spin up and then print the server created
  Sys.sleep(5)
  if (!px$is_alive()) {
    out <- px$read_all_output(); err <- px$read_all_error()
    stop(server_name, " failed to start:\n", out, "\n", err)
  }
  message("✅ ", server_name, " running on port ", port, " (PID=", px$get_pid(), ")")  
  
  # 3) try each path until we get a real sessionId back
  client     <- NULL
  last_error <- NULL
  for (p in paths_to_try) {
    rd <- RSelenium::remoteDriver(
      remoteServerAddr = "localhost",
      port              = port,
      browserName       = browser,
      path              = p
    )
    attempt <- try({
      rd$open(silent = TRUE)
      sid <- rd$getSession()$sessionId
      if (is.null(sid) || nchar(sid)==0) {
        stop("no sessionId")
      }
      message("✅ WebDriver session @ path='", p, "'; sessionId=", sid)
      client <- rd
      break
    }, silent = TRUE)
    last_error <- attempt
  }
  
  if (is.null(client)) {
    px$kill()
    stop("❌ Could not open a WebDriver session. Last error:\n",
         last_error)
  }
  # 4) (Optional) set an implicit wait so findElement() polls up to 5s
  client$setImplicitWaitTimeout(5000)
  
  # Return of the function
  return(list(
    process = px,
    client  = remDr
  ))
}


# --------------------------------------------------------------------------------------------
# Function: start_selenium_docker
# @Arg      : host    — string; hostname of your Selenium container
#                        (default Sys.getenv("REMOTE_DRIVER_HOST","selenium"))
# @Arg      : port    — integer; port on which the Selenium container listens
#                        (default Sys.getenv("REMOTE_DRIVER_PORT",4444))
# @Arg      : browser — character; browser name, “chrome” or “firefox” (default "firefox")
# @Output   : RSelenium remoteDriver client, already opened & with a 5 s implicit wait
# @Purpose  : Connect to a standalone-container Selenium service rather than
#             launching a local driver binary. Tries both “/” and “/wd/hub” paths.
# @Written  : 18/07/2025
# @Author   : Marcos Paulo
# --------------------------------------------------------------------------------------------
start_selenium_docker <- function(
    host    = Sys.getenv("REMOTE_DRIVER_HOST", "selenium"),
    port    = as.integer(Sys.getenv("REMOTE_DRIVER_PORT", 4444)),
    browser = "firefox"
) {
  # Build a W3C capabilities payload
  caps <- list(
    alwaysMatch = list(
      browserName  = browser,
      platformName = "linux"
    )
  )
  
  rd <- remoteDriver(
    remoteServerAddr  = host,
    port              = port,
    browserName       = browser,
    path              = "",
    extraCapabilities = caps
  )
  
  # Try opening a session
  rd$open()
  sid <- rd$getSession()[["sessionId"]]
  if (is.null(sid) || nchar(sid) == 0) {
    stop("❌ Selenium did not return a session ID.")
  }
  message("✅ Connected to Selenium! Session ID: ", sid)
  
  # Set a 5-second implicit wait for element lookups
  rd$setImplicitWaitTimeout(5000L)
  rd
  
}


# --------------------------------------------------------------------------------------------
# Function: stop_selenium
# @Arg   : ses — the list returned by start_selenium()
# @Purpose: close the browser session and kill the local Selenium process (if any)
# --------------------------------------------------------------------------------------------
stop_selenium <- function(ses) {
  ses$process$kill()
  if (!is.null(ses$proc) && ses$proc$is_alive()) {
    ses$proc$kill()
  }
}


# --------------------------------------------------------------------------------------------
# Function: download_bogota_station_info
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
download_bogota_station_info <- function(base_url) {
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
# Function: download_bogota_station_slice
# @Arg       : remDr          — RSelenium remoteDriver
# @Arg       : base_url       — string; form page URL
# @Arg       : station_uid    — string; one stationId from download_bogota_station_uids()
# @Arg       : from_date      — Date; slice start, e.g. as.Date("2010-01-01")
# @Arg       : to_date        — Date; slice end,   e.g. as.Date("2010-12-31")
# @Arg       : report_type    — string; Spanish label, e.g. "Promedio"
# @Arg       : tb_from, tb_to — string; dropdown labels, e.g. "1 Hora"
# @Output    : tibble of that station’s grid for the slice
# @Purpose   : drive the UI in “Personalizado” mode for one station + one year‐slice,
#              click “Mostrar”, scrape the resulting Kendo grid, return as tibble
# @Written_on: 10/07/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
download_bogota_station_slice <- function(
    remDr,
    base_url,
    station_uid,
    from_date,
    to_date,
    report_type = "Promedio",
    tb_from     = "1 Hora",
    tb_to       = "1 Hora"
) {
  remDr$navigate(base_url)
  Sys.sleep(2)  # let page load
  
  # 1a) Ensure “Mostrar estaciones activas” is checked
  active_cb <- remDr$findElement("css", "#ActiveStations")
  
  # 1) select station checkbox under #stationFilter
  remDr$findElement("css", "#stationFilter .k-icon.k-i-expand")$clickElement()
  Sys.sleep(0.5)
  chk_xpath <- sprintf(
    "//div[@id='stationFilter']//li[@data-uid='%s']//input[@type='checkbox']",
    station_uid
  )
  remDr$findElement("xpath", chk_xpath)$clickElement()
  
  # 2) switch "Periódico" → "Personalizado"
  remDr$findElement("xpath",
                    "//div[@id='wrapper2']//button[contains(., 'Personalizado')]"
  )$clickElement()
  
  # 3) set start/end date & time
  remDr$findElement("css", "#wrapper2 input[name='startDate']")$clearElement()
  remDr$findElement("css", "#wrapper2 input[name='startDate']")$
    sendKeysToElement(list(format(from_date, "%d-%m-%Y")))
  remDr$findElement("css", "#wrapper2 input[name='startTime']")$clearElement()
  remDr$findElement("css", "#wrapper2 input[name='startTime']")$
    sendKeysToElement(list("00:00"))
  
  remDr$findElement("css", "#wrapper2 input[name='endDate']")$clearElement()
  remDr$findElement("css", "#wrapper2 input[name='endDate']")$
    sendKeysToElement(list(format(to_date, "%d-%m-%Y")))
  remDr$findElement("css", "#wrapper2 input[name='endTime']")$clearElement()
  remDr$findElement("css", "#wrapper2 input[name='endTime']")$
    sendKeysToElement(list("23:00"))
  
  # 4) select "Promedio" & time-bases
  remDr$findElement("css", "#wrapper2 select[name='avtype']")$
    sendKeysToElement(list(report_type))
  remDr$findElement("css", "#wrapper2 select[name='from-base-time']")$
    sendKeysToElement(list(tb_from))
  remDr$findElement("css", "#wrapper2 select[name='to-base-time']")$
    sendKeysToElement(list(tb_to))
  
  # 5) click Mostrar & wait for grid
  remDr$findElement("xpath",
                    "//div[@id='wrapper2']//button[contains(., 'Mostrar')]"
  )$clickElement()
  Sys.sleep(5)
  
  # 6) scrape the grid HTML & parse locked + main columns
  raw <- remDr$getPageSource()[[1]] %>% xml2::read_html()
  locked <- raw %>% 
    rvest::html_node("#grid .k-grid-content-locked table") %>% 
    rvest::html_table(fill = TRUE)
  main   <- raw %>% 
    rvest::html_node("#grid .k-grid-content table") %>% 
    rvest::html_table(fill = TRUE)
  
  # 7) clean & bind
  colnames(locked) <- locked[1, ]
  locked <- locked[-1, , drop = FALSE]
  colnames(main) <- main[1, ]
  main   <- main[-1, , drop = FALSE]
  df <- dplyr::bind_cols(locked, main)
  df$stationId <- station_uid
  
  return(df)
}


# --------------------------------------------------------------------------------------------
# Function: download_bogota_stations_data
# @Arg       : remDr      — RSelenium remoteDriver from start_selenium()
# @Arg       : base_url   — string; station‐report form page URL
# @Arg       : start_date — Date; first day to fetch, e.g. as.Date("2010-01-01")
# @Arg       : end_date   — Date; last  day to fetch, e.g. as.Date("2010-12-31")
# @Output    : tibble with all station readings over your period
# @Purpose   : I) get station list II) slice into calendar years
#              III) for each station & each slice, call download_bogota_station_slice
#              IV) bind & return
# @Written_on: 10/07/2025  by Marcos Paulo
# --------------------------------------------------------------------------------------------
download_bogota_stations_data <- function(remDr, base_url, start_date, end_date) {
  # I) station metadata
  meta    <- download_bogota_station_info(base_url)
  uids    <- meta$stationId
  
  # II) calendar‐year slices
  yrs     <- seq(lubridate::year(start_date), lubridate::year(end_date))
  slices  <- purrr::map(yrs, ~ list(
    from = max(as.Date(sprintf("%d-01-01", .x)), start_date),
    to   = min(as.Date(sprintf("%d-12-31", .x)), end_date)
  ))
  
  # III) loop & collect
  out <- purrr::map_dfr(uids, function(st) {
    purrr::map_dfr(slices, function(sl) {
      message("Station ", st, ": ", sl$from, " → ", sl$to)
      download_bogota_station_slice(
        remDr       = remDr,
        base_url    = base_url,
        station_uid = st,
        from_date   = sl$from,
        to_date     = sl$to
      )
    })
  })
  
  message("Completed download: ", nrow(out), " rows.")
  return(out)
}


# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")