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
# Function: get_bogota_station_info
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
get_bogota_station_info <- function(base_url) {
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
# Function: download_bogota_station_data
# @Arg         : base_url    — string; URL of the station‐report form page
# @Arg         : start_year  — integer; first year to download (e.g. 2000)
# @Arg         : end_year    — integer; last year to download (e.g. 2022)
# @Arg         : container   — logical; TRUE if running inside your Docker/Selenium setup
# @Output      : writes XLSX files into "data/raw/pollution_ground_stations/Bogota"
# @Purpose     : for each station and each year, set the date range in the form,
#                trigger “Mostrar” & the Excel export, then rename the downloaded file.
# @Written_on  : 05/08/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
download_bogota_station_data <- function(base_url,
                                         start_year,
                                         end_year,
                                         container = TRUE) {
  # 0) ensure our target folder exists
  downloads_dir <- here("data", "raw", "pollution_ground_stations", "Bogota")
  dir.create(downloads_dir, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Downloads will go to: ", downloads_dir)
  
  # 1) if not in Docker, spin up a local Selenium container
  if (!container) {
    message("🚀 Starting local Selenium on 4445 …")
    system("docker run -d -p 4445:4444 selenium/standalone-firefox:4.34.0-20250717",
           intern = TRUE)
    Sys.sleep(6)
    selenium_host <- "localhost"
    selenium_port <- 4445L
  } else {
    selenium_host <- "selenium"
    selenium_port <- 4444L
  }
  
  # 2) pick the correct download path inside the browser
  download_dir_container <- if (container) {
    # inside the selenium container
    "/home/seluser/Downloads"
    } else {
      downloads_dir
    }
  
  caps <- list(
    browserName = "firefox",
    platformName = "LINUX",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList"     = 2L,
        "browser.download.dir"            = download_dir_container,
        "browser.download.useDownloadDir" = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" =
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    )
  )
  
  # 3) launch the session
  session <- selenium::SeleniumSession$new(
    browser      = "firefox",
    host         = selenium_host,
    port         = selenium_port,
    capabilities = caps
  )
  on.exit(session$close(), add = TRUE)
  
  # 4) load the page once to get station count
  session$navigate(base_url)
  ul <- session$find_element("css selector", "#StationsMonitorsList > ul")
  count  <- length(ul$find_elements("css selector","li.k-item"))
  message("🔎 Found ", count, " stations in the list.")
  
  # 5) loop by index (so we can re-find the element fresh after each navigation)
  for (i in seq_len(count)) {
    # re-find the station <li> each time
    session$navigate(base_url)
    Sys.sleep(1)
    lis <- session$find_element("css selector", "#StationsMonitorsList > ul") %>%
      {.$find_elements("css selector","li.k-item")}
    item <- lis[[i]]
    
    station_name <- item$get_text()[[1]]
    safe_name    <- gsub("[^A-Za-z0-9]", "_", station_name)
    message("\n🏷  Station [", i, "/", count, "]: ", station_name)
    
    # prepare the JS toggle once for this station
    chk    <- item$find_element("xpath", ".//input[contains(@class,'k-checkbox')]")
    cb_id  <- chk$get_attribute("id")[[1]]
    js_toggle <- sprintf("
      var cb = document.getElementById('%s');
      cb.scrollIntoView({block:'center'});
      cb.checked = !cb.checked;
      cb.dispatchEvent(new Event('change',{bubbles:true}));
    ", cb_id)
    
    for (yr in seq(start_year, end_year)) {
      message("   📥 Year ", yr, " …")
      
      # 5.1) select “Personalizado”
      # session$execute_script(js_toggle)      # turn it ON
      Sys.sleep(0.5)
      custom <- session$find_element("css selector",
                                     "#select-reportperiod > li:nth-child(6)")
      custom$click()
      
      # 5.2) fill dates
      start_str <- sprintf("01-01-%04d", yr)
      end_str   <- sprintf("31-12-%04d", yr)
      for (field in c("startDate","endDate")) {
        el <- session$find_element("css selector", paste0("#", field))
        el$click()
        el$send_keys(key_chord(keys$control, keys$shift, keys$home),
                     key_chord(keys$backspace))
        el$send_keys(
          if (field=="startDate") start_str else end_str
        )
      }
      # times
      st <- session$find_element("css selector", "#startTime")
      et <- session$find_element("css selector", "#endTime")
      st$click() 
      st$send_keys(key_chord(keys$control, keys$shift, keys$home),
                key_chord(keys$backspace))
      st$send_keys("00:00")
      
      et$click()
      et$send_keys(key_chord(keys$control, keys$shift, keys$home),
                   key_chord(keys$backspace))
      et$send_keys("23:00")
      
      # 5.3) click “Mostrar” (with an extra toggle to avoid the page bug)
      session$execute_script(js_toggle)
      show_btn <- session$find_element("xpath",'//*[@id="buttonsWrapper"]/input[2]')
      show_btn$click()
      Sys.sleep(3)
      
      # 5.4) export → Excel
      excel_btn <- session$find_element("css selector","div.LinksReport.Excel")
      excel_btn$click()
      Sys.sleep(5)
      
      # 5.5) rename the newest .xlsx in our host folder
      files <- list.files(downloads_dir, pattern="\\.xlsx$", full.names=TRUE)
      if (length(files)) {
        newest <- files[which.max(file.info(files)$ctime)]
        target <- file.path(
          downloads_dir,
          sprintf("%s_%04d.xlsx", safe_name, yr)
        )
        file.rename(newest, target)
        message("      ✔️ Saved → ", basename(target))
      } else {
        warning("      ⚠️ No .xlsx found for ", station_name, " ", yr)
      }
    }
    
    # 5.6) finally uncheck so next station starts fresh
    session$execute_script(js_toggle)
    Sys.sleep(0.5)
  }
  
  message("\n✅ All done—your files live in:\n  ", downloads_dir)
}


# --------------------------------------------------------------------------------------------
# Function: download_bogota_station_data
# @Arg       : base_url    — string; URL of the station-report form page
# @Arg       : start_year  — integer; first year to download (e.g. 2000)
# @Arg       : end_year    — integer; last year to download (e.g. 2022)
# @Arg       : container   — logical; TRUE if running inside Docker compose (host = "selenium", port = 4444),
#                            FALSE to launch a local Selenium container on port 4445
# @Output    : saves one Excel file per station-year into
#              ./data/raw/pollution_ground_stations/Bogota/
# @Purpose   : iterate over each station and each calendar year, set the custom date range,
#              click “Mostrar” and then click the Excel export button, renaming each download
#              to `<station>_<year>.xlsx`
# @Written_on: 28/07/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
download_bogota_station_data <- function(base_url,
                                         start_year,
                                         end_year,
                                         container = TRUE) {
  # 0) prepare download directory
  downloads_dir <- here::here("data", "raw", "pollution_ground_stations", "Bogota")
  dir.create(downloads_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1) decide Selenium host/port, possibly start local container
  if (!container) {
    message("Launching a local Selenium container on port 4445…")
    system("docker run -d -p 4445:4444 selenium/standalone-firefox:4.34.0-20250717",
           intern = TRUE)
    Sys.sleep(5)            # give it a moment to spin up
    selenium_host <- "localhost"
    selenium_port <- 4445L
  } else {
    selenium_host <- "selenium"
    selenium_port <- 4444L
  }
  
  # 2) configure Firefox to download silently into our target folder
  download_dir_container <- if (container) {
    "/air_monitoring/data/raw/pollution_ground_stations/Bogota"
  } else {
    downloads_dir
  }
  caps <- list(
    browserName = "firefox",
    platformName = "LINUX",
    "moz:firefoxOptions" = list(
      prefs = list(
        "browser.download.folderList"       = 2L,
        "browser.download.dir"              = download_dir_container,
        "browser.download.useDownloadDir"   = TRUE,
        "browser.helperApps.neverAsk.saveToDisk" =
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    )
  )
  
  # 3) start a Selenium session
  session <- selenium::SeleniumSession$new(
    browser      = "firefox",
    host         = selenium_host,
    port         = selenium_port,
    capabilities = caps
  )
  on.exit(session$close(), add = TRUE)
  
  # 4) navigate to the form page
  session$navigate(base_url)
  
  # 5) collect all station <li> elements once
  station_list  <- session$find_element("css selector", "#StationsMonitorsList > ul")
  station_items <- station_list$find_elements("css selector", "li.k-item")
  
  # 6) for each station & each year…
  for (item in station_items) {
    station_name <- item$get_text()[[1]]
    safe_name    <- gsub("[^A-Za-z0-9]", "_", station_name)
    
    for (yr in seq(start_year, end_year)) {
      message("📥 Downloading ", station_name, " station for year ", yr, " …")
      
      # -- toggle the station’s checkbox via JS injection
      chk <- item$find_element(
        "xpath",
        ".//input[contains(@class,'k-checkbox')]"
      )
      cb_id <- chk$get_attribute("id")[[1]]
      js_toggle <- sprintf("
        var cb = document.getElementById('%s');
        cb.scrollIntoView({block:'center'});
        cb.checked = !cb.checked;
        cb.dispatchEvent(new Event('change',{bubbles:true}));
      ", cb_id)
      # session$execute_script(js_toggle) can't apply now because of bug
      
      # -- select “Personalizado” period
      custom_btn <- session$find_element(
        "css selector",
        "#select-reportperiod > li:nth-child(6)"
      )
      custom_btn$click()
      
      # -- compute date strings
      start_str <- sprintf("01-01-%04d", yr)
      end_str   <- sprintf("31-12-%04d", yr)
      
      # -- fill “De la fecha”
      sd_in <- session$find_element("css selector", "#startDate")
      sd_in$click()
      sd_in$send_keys(key_chord(keys$control, keys$shift, keys$home),
                      key_chord(keys$backspace))
      sd_in$send_keys(start_str)
      
      # -- fill “A la fecha”
      ed_in <- session$find_element("css selector", "#endDate")
      ed_in$click()
      ed_in$send_keys(key_chord(keys$control, keys$shift, keys$home),
                      key_chord(keys$backspace))
      ed_in$send_keys(end_str)
      
      # -- fill times 00:00 → 23:00
      st_in <- session$find_element("css selector", "#startTime")
      st_in$click()
      st_in$send_keys(key_chord(keys$control, keys$shift, keys$home),
                      key_chord(keys$backspace))
      st_in$send_keys("00:00")
      et_in <- session$find_element("css selector", "#endTime")
      et_in$click()
      et_in$send_keys(key_chord(keys$control, keys$shift, keys$home),
                      key_chord(keys$backspace))
      et_in$send_keys("23:00")
      
      # -- due to page quirks, toggle JS only now before clicking “Mostrar”
      session$execute_script(js_toggle)
      show_btn <- session$find_element(
        "xpath",
        '//*[@id="buttonsWrapper"]/input[2]'
      )
      show_btn$click()
      Sys.sleep(4)
      
      # -- click the Excel export icon
      excel_btn <- session$find_element("css selector", "div.LinksReport.Excel")
      excel_btn$click()
      message("   → Excel export triggered for ", station_name, " ", yr)
      Sys.sleep(5)  # wait for download
      
      # -- rename the newest file
      downloaded_files <- list.files(
        downloads_dir,
        pattern = "\\.xlsx$",
        full.names = TRUE
      )
      if (length(downloaded_files)) {
        newest <- downloaded_files[which.max(file.info(downloaded_files)$ctime)]
        new_name <- file.path(
          downloads_dir,
          sprintf("%s_%04d.xlsx", safe_name, yr)
        )
        file.rename(newest, new_name)
      } else {
        warning("No .xlsx found for ", station_name, " ", yr)
      }
      
      # -- reset by re-navigating
      session$navigate(base_url)
      Sys.sleep(1)
      station_list  <- session$find_element("css selector", "#StationsMonitorsList > ul")
      station_items <- station_list$find_elements("css selector", "li.k-item")
    }
    
    # finally, un-check the station so subsequent stations start fresh
    session$execute_script(js_toggle)
  }
  
  message("✅ All downloads complete and saved to:\n  ", downloads_dir)
}

# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")