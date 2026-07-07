# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
# 
# @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "validation_old_version" scripts.
# 
# @Date: Aug 2025
# @Author: Marcos Paulo
# ============================================================================================

# List of required packages
pkgs <- c(
  "arrow",
  "dplyr",
  "haven",
  "here",
  "lubridate",
  "quarto",
  "readr",
  "tidyr",
  "sf"
)

# Strict check: fail fast if something isn't in the project library
ensure_installed <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    message(
      "Missing packages: ", paste(miss, collapse = ", "),
      ". Run renv::restore() (or install locally with renv::install() then renv::snapshot())."
    )
    renv::install(miss)}
}

ensure_installed(pkgs)

# Attach (quiet)
invisible(lapply(pkgs, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# no repo tweaking, no renv::install() here
rm(pkgs, ensure_installed)

# ============================================================================================
# Validation helpers and functions
# ============================================================================================
# Null-coalesce helper used inside compare_ground_stations
`%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------------------------------------------------------------
# Function: build_compare_cfg
# @Goal   : Generate the validation config sublist dynamically based on city_id.
# @Details: Keeps production configs (bogota.R) clean by isolating test parameters.
# -----------------------------------------------------------------------------------
build_compare_cfg <- function(city_id) {
  
  # 1. Base parameters shared across ALL cities
  base_compare <- list(
    pipeline_tz      = "UTC",
    out_root         = here::here("results", "validation_old_version"),
    city_dir         = file.path("results", "validation_old_version", city_id),
    compare_years    = 2023L,
    focus_pollutants = c("pm10", "pm25"),
    value_cols       = c("pm10", "pm25", "ozone", "co", "no2"),
    gs_tol           = c(pm10 = 0, pm25 = 0, ozone = 0, co = 0, no2 = 0),
    census_tol       = 0.001,
    idw_tol_km       = 0.5,
    outlier_params   = list(
      use_legacy_input    = TRUE,  
      pct_flag            = 0.99,
      n_sd                = 2,
      on_missing_temporal = "finish", 
      on_missing_neighbor = "finish"
    )
  )
  
  # 2. City-specific overrides and paths
  city_specific <- switch(
    city_id,
    "bogota" = list(
      legacy_single_csv       = here::here("data", "_legacy", "merged_pollution",
                                           "bogota", "Air_Pollution_Bogota_2002_2023.csv"),
      legacy_dir              = here::here("data", "_legacy", "raw_pollution", "bogota"),
      legacy_pattern          = "^Air_Pollution_Bogota_\\d{4}_\\d{4}\\.csv$",
      drop_stations           = character(0),
      residual_map            = c("CENTRODEALTORENDIMIENTO" = "CAR",
                                  "ELJAZMIN"                = "JAZMIN"),
      new_metro_gpkg          = here::here("data", "raw", "geospatial_data", "bogota", 
                                           "bogota_area_metro_2018.gpkg"),
      new_stations_gpkg       = here::here("data", "raw", "geospatial_data", "bogota",
                                           "bogota_2018_stations_buffer_metro.gpkg"),
      legacy_shp_dir          = here::here("data", "_legacy", "cities_shapefiles", 
                                           "Bogota_metro"),
      metro_buffer_km         = 20,
      new_census_collapsed    = here::here("data", "interim", "census",
                                           "bogota_extended_2005",
                                           "collapse_metro_area_extended.csv"),
      legacy_census_collapsed = here::here("data", "_legacy", "census",
                                           "collapse_bogota_metro.csv"),
      census_join_key         = "GEO_ID",
      new_station_dist        = here::here("data", "processed", "distances_matrices",
                                           "bogota_2018_station_distances.parquet"),
      legacy_station_dist     = here::here("data", "_legacy", "distances", "bogota",
                                           "stations_distance_bogota_v2.csv"),
      new_geo_dist            = here::here("data", "processed", "distances_matrices",
                                           "bogota_2018_geo_station_distances.parquet"),
      legacy_geo_dist         = here::here("data", "_legacy", "distances", "bogota",
                                           "dt_distances.rds"),
      new_clean_dir           = here::here("data", "processed", "outlier_detection",
                                           "bogota_metro_clean"),
      legacy_clean_path       = here::here("data", "_legacy", "outlier", "bogota",
                                           "pollution_data_balanced_2023.rds"),
      qmd_path                = here::here(base_compare$out_root, "bogota",
                                           "bogota_report.qmd"),
      html_dest               = file.path(base_compare$out_root, city_id, 
                                          "bogota_validation_report.html")
      ),
    "cdmx" = list(
      # Add CDMX-specific paths here when ready
      ),
    "santiago" = list(# Add Santiago-specific paths here when ready
      ),
    # Default if city is not matched
    stop("Validation config not defined for city: ", city_id)
    )
  
  # 3. Merge base and specific lists recursively
  utils::modifyList(base_compare, city_specific)
}

# ---------------------------------------------------------------------------
# prepare_new_bogota_like_legacy — harmonise new Arrow panel
# ---------------------------------------------------------------------------
prepare_new_bogota_like_legacy <- function(
    new_df,
    rename_map    = c(),
    drop_stations = character(),
    year_keep     = 2002:2023,
    hour_shift    = 0L,
    tz            = "America/Bogota"
) {
  df <- new_df |>
    dplyr::collect() |>
    dplyr::mutate(
      datetime = as.POSIXct(datetime, tz = tz)
    ) |>
    dplyr::select(station, datetime, pm10, pm25,
                  ozone, co, no2) |>
    dplyr::arrange(datetime)
  
  df <- build_time_parts(df, tz = tz,
                         hour_shift = hour_shift) |>
    dplyr::filter(.data$year %in% year_keep) |>
    harmonize_station_names(
      rename_map    = rename_map,
      drop_stations = drop_stations
    ) |>
    dplyr::select(
      station, datetime, pm10, pm25, ozone, co, no2,
      year, month, day, hour
    ) |>
    dplyr::arrange(station, datetime)
  
  df
}


# ---------------------------------------------------------------------------
# prepare_legacy_bogota — harmonise legacy multi-CSV panel
# ---------------------------------------------------------------------------
prepare_legacy_bogota <- function(
    legacy_df,
    rename_map    = c(),
    drop_stations = character(),
    tz            = "UTC",
    panelize      = TRUE,
    panel_years   = 2002:2023,
    verbose       = TRUE
) {
  df <- legacy_df |>
    dplyr::mutate(
      ozone = ozono,
      hour  = suppressWarnings(as.integer(hour)),
      year  = suppressWarnings(as.integer(year)),
      month = suppressWarnings(as.integer(month)),
      day   = suppressWarnings(as.integer(day))
    )
  
  date0 <- lubridate::make_datetime(
    year = df$year, month = df$month,
    day  = df$day,  hour  = df$hour, tz = tz
  )
  is24 <- !is.na(df$hour) & df$hour == 24L
  n24  <- sum(is24, na.rm = TRUE)
  if (any(is24)) df$hour[is24] <- 0L
  df$datetime <- as.POSIXct(date0)
  
  df <- df |>
    dplyr::select(station, datetime, pm10, pm25, ozone, co, no2) |>
    harmonize_station_names(
      rename_map    = rename_map,
      drop_stations = drop_stations
    ) |>
    dplyr::distinct(station, datetime, .keep_all = TRUE) |>
    dplyr::arrange(station, datetime)
  
  if (isTRUE(panelize)) {
    y0       <- min(panel_years, na.rm = TRUE)
    y1       <- max(panel_years, na.rm = TRUE)
    start_dt <- as.POSIXct(
      sprintf("%d-01-01 00:00:00", y0), tz = tz
    )
    end_dt   <- as.POSIXct(
      sprintf("%d-12-31 23:00:00", y1), tz = tz
    )
    hours_seq <- seq(start_dt, end_dt, by = "1 hour")
    stations  <- sort(unique(df$station))
    
    grid <- tidyr::crossing(
      station = stations, datetime = hours_seq
    ) |>
      dplyr::mutate(
        year  = lubridate::year(datetime),
        month = lubridate::month(datetime),
        day   = lubridate::day(datetime),
        hour  = lubridate::hour(datetime)
      )
    
    df <- dplyr::left_join(
      grid,
      dplyr::select(df, station, datetime,
                    pm10, pm25, ozone, co, no2),
      by = c("station", "datetime")
    ) |>
      dplyr::arrange(station, datetime)
    
    if (verbose) {
      message(sprintf(
        paste0(
          "\u2759 Panelised: %s stations \u00d7",
          " %s hours \u2192 %s rows."
        ),
        format(length(stations),  big.mark = ","),
        format(length(hours_seq), big.mark = ","),
        format(nrow(df),          big.mark = ",")
      ))
      if (n24 > 0)
        message(sprintf(
          "\u23f1\ufe0f  Rolled %d obs from 24:00 \u2192 00:00.", n24
        ))
    }
  } else {
    df <- df |>
      dplyr::mutate(
        year  = lubridate::year(datetime),
        month = lubridate::month(datetime),
        day   = lubridate::day(datetime),
        hour  = lubridate::hour(datetime)
      )
  }
  
  df |>
    dplyr::mutate(
      dplyr::across(c(pm10, pm25, ozone, co, no2),
                    ~ suppressWarnings(as.numeric(.x)))
    ) |>
    dplyr::select(
      station, datetime, pm10, pm25, ozone, co, no2,
      year, month, day, hour
    )
}


# ---------------------------------------------------------------------------
# read_legacy_period_csvs — read + row-bind period CSVs
# ---------------------------------------------------------------------------
read_legacy_period_csvs <- function(
    dir,
    pattern = "^Air_Pollution_Bogota_\\d{4}_\\d{4}\\.csv$",
    tz      = "America/Bogota"
) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No legacy CSVs found in: ", dir)
  purrr::map_dfr(files, readr::read_csv, show_col_types = FALSE)
}


# ---------------------------------------------------------------------------
# build_time_parts — add year/month/day/hour columns from POSIXct
# ---------------------------------------------------------------------------
build_time_parts <- function(df, tz = "America/Bogota",
                             hour_shift = 0L) {
  stopifnot("datetime" %in% names(df))
  dt <- lubridate::force_tz(df$datetime, tzone = tz)
  if (hour_shift != 0L) dt <- dt + lubridate::hours(hour_shift)
  df$year  <- as.integer(lubridate::year(dt))
  df$month <- as.integer(lubridate::month(dt))
  df$day   <- as.integer(lubridate::day(dt))
  df$hour  <- as.integer(lubridate::hour(dt))
  df
}


# ---------------------------------------------------------------------------
# harmonize_station_names — recode + drop stations in a data frame
# ---------------------------------------------------------------------------
harmonize_station_names <- function(
    df,
    rename_map    = c(),
    drop_stations = character()
) {
  stopifnot("station" %in% names(df))
  if (length(rename_map))
    df$station <- dplyr::recode(
      df$station, !!!rename_map, .default = df$station
    )
  if (length(drop_stations))
    df <- dplyr::filter(df, !.data$station %in% drop_stations)
  df
}


# --------------------------------------------------------------------------------------------
# Function: harmonize_station_names
# @Arg       : df            — tibble/data.frame with a 'station' column (chr)
# @Arg       : rename_map    — named chr vector c("Old Name"="NewName", ...)
# @Arg       : drop_stations — chr vector of stations to drop after renaming
# @Output    : tibble with station names harmonized and dropped as requested.
# @Purpose   : Standardize station labels so legacy vs new data align.
# @Written_on: 27/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
harmonize_station_names <- function(df, rename_map = c(), drop_stations = character()) {
  stopifnot("station" %in% names(df))
  if (length(rename_map)) {
    df$station <- dplyr::recode(df$station, !!!rename_map, .default = df$station)
  }
  if (length(drop_stations)) {
    df <- dplyr::filter(df, !.data$station %in% drop_stations)
  }
  df
}


# ---------------------------------------------------------------------------
# .std_name
# @Arg  x : character vector of station names (any case/encoding)
# @Out    : character; uppercase, accents stripped, non-alphanumeric removed.
#           "LasFerias" and "LAS FERIAS" both → "LASFERIAS".
# @Purpose: Normalise station names so that differences in casing, spaces,
#           hyphens and accents do not produce spurious mismatches.
#           Apply BEFORE any residual_map lookup.
# @Written_on: 20/03/2026
# @Written_by: Marcos Paulo
# ---------------------------------------------------------------------------
.std_name <- function(x) {
  if (!requireNamespace("stringi", quietly = TRUE))
    stop("Package 'stringi' required for name standardisation.")
  x <- toupper(trimws(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  gsub("[^A-Z0-9]", "", x)
}


# ---------------------------------------------------------------------------
# prepare_legacy_single_csv
#
# @Arg legacy_csv    : string; path to the single merged CSV from the
#                      old Stata pipeline.
#                      Required columns (minimum):
#                        datehour  — chr, Stata "01jan2002 01:00:00"
#                        hour      — numeric 1-24  (24 rolled next day)
#                        station   — chr, any case/accents
#                        year, month, day — integer date parts
#                        pm10, pm25, ozono, co, no2 — numeric pollutants
# @Arg residual_map  : named chr vec; overrides applied AFTER .std_name().
#                      Keys are already-normalised names. Default c().
# @Arg drop_stations : chr vec (raw); excluded after normalisation.
# @Arg tz            : Olson timezone. Default "UTC".
# @Arg compare_years : integer vector; years to retain. NULL = all.
# @Arg panelize      : logical; expand to full station × hour grid.
# @Arg verbose       : logical; print progress counts. Default TRUE.
#
# @Output: tibble with columns:
#            station, datetime, pm10, pm25, ozone, co, no2,
#            year, month, day, hour
# @Written_on: 20/03/2026
# @Written_by: Marcos Paulo
# ---------------------------------------------------------------------------
prepare_legacy_single_csv <- function(
    legacy_csv,
    residual_map  = c(),
    drop_stations = character(0),
    tz            = "UTC",
    compare_years = NULL,
    panelize      = TRUE,
    verbose       = TRUE
) {
  for (pkg in c("vroom", "stringi", "lubridate", "tidyr", "dplyr"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' required but not installed.")
  
  if (!file.exists(legacy_csv))
    stop("legacy_csv not found:\n  ", legacy_csv)
  
  # 1. Read all columns as character to avoid type-guessing issues
  raw <- vroom::vroom(
    legacy_csv,
    col_types      = vroom::cols(.default = vroom::col_character()),
    show_col_types = FALSE
  )
  
  # 2. Parse Stata datehour "01jan2002 01:00:00"
  dt_parsed <- as.POSIXct(
    strptime(tolower(trimws(raw$datehour)),
             format = "%d%b%Y %H:%M:%S",
             tz     = tz)
  )
  
  df        <- raw
  df$datetime <- dt_parsed
  df$hour     <- suppressWarnings(as.integer(df$hour))
  df$year     <- suppressWarnings(as.integer(df$year))
  df$month    <- suppressWarnings(as.integer(df$month))
  df$day      <- suppressWarnings(as.integer(df$day))
  
  # 3. Roll hour == 24 → 00:00 next day
  is24 <- !is.na(df$hour) & df$hour == 24L
  n24  <- sum(is24)
  if (n24 > 0L) {
    df$datetime[is24] <- df$datetime[is24] + 86400L
    df$hour[is24]     <- 0L
    df$year[is24]  <- as.integer(lubridate::year( df$datetime[is24]))
    df$month[is24] <- as.integer(lubridate::month(df$datetime[is24]))
    df$day[is24]   <- as.integer(lubridate::day(  df$datetime[is24]))
    if (verbose)
      message(sprintf(
        "Rolled %d obs from hour 24 \u2192 00:00 next day.", n24
      ))
  }
  
  # 4. Rename ozono → ozone; coerce pollutants to numeric
  if ("ozono" %in% names(df) && !"ozone" %in% names(df))
    names(df)[names(df) == "ozono"] <- "ozone"
  
  for (col in c("pm10", "pm25", "ozone", "co", "no2"))
    if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  
  # 5. Standardise station names then apply residual map
  df$station <- .std_name(df$station)
  if (length(residual_map) > 0L)
    df$station <- dplyr::recode(df$station, !!!residual_map)
  
  # 6. Drop stations; de-duplicate on station × datetime
  if (length(drop_stations) > 0L)
    df <- df[!df$station %in% .std_name(drop_stations), ,
             drop = FALSE]
  df <- dplyr::distinct(df, station, datetime, .keep_all = TRUE)
  
  # 7. Year filter
  if (!is.null(compare_years))
    df <- df[df$year %in% compare_years, , drop = FALSE]
  
  # 8. Select core columns
  keep_cols <- intersect(
    c("station", "datetime",
      "pm10", "pm25", "ozone", "co", "no2",
      "year", "month", "day", "hour"),
    names(df)
  )
  df <- df[, keep_cols, drop = FALSE]
  
  # 9. Optionally expand to balanced station × hour panel
  if (isTRUE(panelize) && nrow(df) > 0L && !is.infinite(min(df$year, na.rm = TRUE))) {
    stations  <- sort(unique(df$station))
    y0 <- min(df$year, na.rm = TRUE)
    y1 <- max(df$year, na.rm = TRUE)
    
    # ADDED format = "%Y-%m-%d %H:%M:%S" TO PREVENT as.POSIXlt.character ERROR
    start_dt  <- as.POSIXct(
      sprintf("%d-01-01 00:00:00", y0), tz = tz, format = "%Y-%m-%d %H:%M:%S"
    )
    end_dt    <- as.POSIXct(
      sprintf("%d-12-31 23:00:00", y1), tz = tz, format = "%Y-%m-%d %H:%M:%S"
    )
    hours_seq <- seq(start_dt, end_dt, by = "1 hour")
    
    grid <- tidyr::crossing(
      station  = stations,
      datetime = hours_seq
    ) |>
      dplyr::mutate(
        year  = lubridate::year(datetime),
        month = lubridate::month(datetime),
        day   = lubridate::day(datetime),
        hour  = lubridate::hour(datetime)
      )
    
    poll_cols <- intersect(
      c("pm10", "pm25", "ozone", "co", "no2"), names(df)
    )
    df <- dplyr::left_join(
      grid,
      df[, c("station", "datetime", poll_cols)],
      by = c("station", "datetime")
    ) |>
      dplyr::arrange(station, datetime)
    
    if (verbose) {
      added <- nrow(df) - length(stations) * length(hours_seq)
      message(sprintf(
        paste0(
          "Panelised: %s stations \u00d7 %s hours",
          " \u2192 %s rows."
        ),
        format(length(stations),  big.mark = ","),
        format(length(hours_seq), big.mark = ","),
        format(nrow(df),          big.mark = ",")
      ))
    }
  }
  
  dplyr::arrange(df, station, datetime)
}


# --------------------------------------------------------------------------------------------
# Function: prepare_legacy_cdmx
# @Arg       : legacy_df  — tibble read from legacy Stata/CSV for Mexico City
# @Arg       : tz         — Olson timezone for datetime parsing (default "UTC")
# @Output    : tibble with columns (order fixed):
#              datehour, year, month, day, hour, station_code, pm25, pm10, no2, o3, co
# @Purpose   : Normalize the *legacy* panel to the comparison schema:
#              - keep a single datetime (datehour), drop datehour2/day_week
#              - rename station → station_code
#              - enforce types and column order
# @Notes     : Assumes legacy_df already has datehour (POSIXct), year/month/day/hour.
# --------------------------------------------------------------------------------------------
prepare_legacy_cdmx <- function(legacy_df, tz = "UTC") {
  df <- legacy_df
  
  # 1) Prefer `datehour` as the single datetime and drop extras
  if (!"datehour" %in% names(df) && "datehour2" %in% names(df)) {
    # if only datehour2 exists, coerce to POSIXct at hour if available
    base_dt <- as.POSIXct(df$datehour2, tz = tz)
    if ("hour" %in% names(df)) {
      df$datehour <- base_dt + as.difftime(df$hour, units = "hours")
    } else {
      df$datehour <- base_dt
    }
  }
  df$datehour <- as.POSIXct(df$datehour, tz = tz)
  
  # 2) Drop unused columns if present
  drop_cols <- intersect(c("datehour2", "day_week", "date"), names(df))
  if (length(drop_cols)) df <- dplyr::select(df, -dplyr::all_of(drop_cols))
  
  # 3) Rename station → station_code
  if ("station" %in% names(df)) {
    df <- dplyr::rename(df, station_code = station)
  }
  
  # 4) Make sure pollutants/numerics are numeric
  num_cols <- intersect(c("pm10", "pm25", "no2", "o3", "co"), names(df))
  if (length(num_cols)) {
    df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
  }
  
  # 5) Ensure time parts exist (derive if missing)
  if (!all(c("year","month","day","hour") %in% names(df))) {
    df <- df |>
      dplyr::mutate(
        year  = if (!"year"  %in% names(df))  lubridate::year(.data$datehour)  else .data$year,
        month = if (!"month" %in% names(df))  lubridate::month(.data$datehour) else .data$month,
        day   = if (!"day"   %in% names(df))  lubridate::day(.data$datehour)   else .data$day,
        hour  = if (!"hour"  %in% names(df))  lubridate::hour(.data$datehour)  else .data$hour
      )
  }
  
  # 6) Reorder columns exactly as requested
  keep_order <- c("datehour","year","month","day","hour","station_code",
                  "pm25","pm10","no2","o3","co")
  # add any missing value cols as NA to keep the order stable
  for (nm in setdiff(keep_order, names(df))) df[[nm]] <- NA_real_
  df <- dplyr::select(df, dplyr::all_of(keep_order))
  
  dplyr::arrange(df, .data$station_code, .data$datehour)
}


# --------------------------------------------------------------------------------------------
# Function : prepare_new_panel_like_legacy
# @Arg  : new_data         — tibble/data.frame OR Arrow Dataset/dplyr tbl with columns:
#                            datetime, station_code (or station), pm10, `pm2.5`,
#                            no2, co, and ozone (or o3/ozono).
# @Arg  : stations_keep_df — OPTIONAL data.frame/sf with a station code column
#                            (see station_code_col). Geometry is ignored.
# @Arg  : station_code_col — column name in stations_keep_df (default "code")
# @Arg  : year_keep        — integer vector of UTC years to keep (default 2010:2023)
# @Arg  : tz               — Olson tz string. Used to RELABEL timestamps after collect
#                            (no clock shift; like lubridate::force_tz).
# @Arg  : return           — "tibble" (collect to R) or "arrow" (keep lazy). Default "tibble".
#
# @Output : tibble (if return="tibble") or lazy dplyr query (if return="arrow")
#           with columns:
#           datehour, year, month, day, hour, station_code, pm25, pm10, no2, o3, co
#
# @Purpose : Make a new panel comparable to the legacy schema in an Arrow-friendly way:
#            • no base R string ops inside the lazy pipeline,
#            • Arrow-translatable datetime filters,
#            • robust ozone column detection,
#            • optional station allow-list,
#            • stable column order and numeric types.
# --------------------------------------------------------------------------------------------
prepare_new_panel_like_legacy <- function(
    new_data,
    stations_keep_df = NULL,
    station_code_col = "code",
    year_keep        = 2010:2023,
    tz               = "UTC",
    return           = c("tibble", "arrow")
) {
  # ---- 0) Validate & set up ---------------------------------------------------
  return <- match.arg(return)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Need 'dplyr'.")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Need 'rlang'.")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Need 'lubridate'.")
  
  cols <- names(new_data)
  if (is.null(cols)) {
    stop("`new_data` must be a data.frame/tibble or an Arrow Dataset/table.")
  }
  
  # (0.1) Ensure we have a station identifier column (station_code or station)
  if (!("station_code" %in% cols || "station" %in% cols)) {
    stop("`new_data` must include 'station_code' or 'station'.")
  }
  
  # (0.2) Detect ozone column (allow common variants)
  o3_col <- intersect(c("ozone", "o3", "ozono"), cols)
  if (!length(o3_col)) stop("`new_data` must include 'ozone' (or 'o3'/'ozono').")
  o3_col <- o3_col[1]
  
  # (0.3) Required columns
  if (!"pm2.5" %in% cols) stop("`new_data` must include column `pm2.5`.")
  must_have <- c("pm10", "no2", "co", "datetime")
  miss <- setdiff(must_have, cols)
  if (length(miss)) stop("`new_data` is missing: ", paste(miss, collapse = ", "))
  
  df <- new_data
  
  # ---- 1) Arrow-safe year filtering ------------------------------------------
  # Build UTC bounds in R (so no as.POSIXct/format inside Arrow filter).
  ymin <- min(year_keep, na.rm = TRUE)
  ymax <- max(year_keep, na.rm = TRUE)
  start_utc <- as.POSIXct(sprintf("%04d-01-01 00:00:00", ymin), tz = "UTC")
  end_utc   <- as.POSIXct(sprintf("%04d-01-01 00:00:00", ymax + 1L), tz = "UTC")
  
  # ---- 2) Create a station_code (coalesce) without string transforms ----------
  # Arrow can translate dplyr::coalesce; avoid trimws/toupper here.
  df <- dplyr::mutate(
    df,
    station_code = dplyr::coalesce(.data$station_code, .data$station)
  )
  
  # ---- 3) Optional: filter by a station allow-list ----------------------------
  # IMPORTANT: do NOT apply toupper/trimws to the Arrow column.
  # Instead, normalize the *vector of values* in R, and compare raw equality.
  if (!is.null(stations_keep_df)) {
    if (!station_code_col %in% names(stations_keep_df)) {
      stop("`stations_keep_df` must have column '", station_code_col, "'.")
    }
    keep_codes <- as.character(stations_keep_df[[station_code_col]])
    # Normalize the *values* (in R); Arrow will do a fast `%in%` compare.
    keep_codes <- unique(keep_codes[!is.na(keep_codes)])
    keep_codes <- trimws(keep_codes)
    # Widen matching a bit without touching the Arrow column:
    # include raw, UPPER, and lower variants in the value set.
    keep_all <- unique(c(keep_codes,
                         toupper(keep_codes),
                         tolower(keep_codes)))
    if (length(keep_all)) {
      df <- dplyr::filter(df, .data$station_code %in% keep_all)
    } else {
      df <- dplyr::filter(df, FALSE)
    }
  }
  
  # ---- 4) Apply Arrow-translatable datetime window ----------------------------
  df <- dplyr::filter(df, .data$datetime >= start_utc, .data$datetime < end_utc)
  
  # ---- 5) Select/rename to the harmonized schema ------------------------------
  # NOTE: Refer to `pm2.5` with backticks; this is Arrow-friendly.
  df <- dplyr::transmute(
    df,
    datehour     = .data$datetime,   # relabel tz later (no clock shift)
    station      = .data$station,
    station_code = .data$station_code,
    pm25         = .data$`pm2.5`,
    pm10         = .data$pm10,
    no2          = .data$no2,
    o3           = .data[[o3_col]],
    co           = .data$co
  )
  
  # ---- 6) Derive time parts ---------------------------------------------------
  if (return == "arrow") {
    # Stay lazy: use strftime (Arrow translates these)
    df <- dplyr::mutate(
      df,
      year  = as.integer(strftime(.data$datehour, "%Y")),
      month = as.integer(strftime(.data$datehour, "%m")),
      day   = as.integer(strftime(.data$datehour, "%d")),
      hour  = as.integer(strftime(.data$datehour, "%H"))
    )
    df <- dplyr::select(
      df,
      datehour, year, month, day, hour,
      station_code, pm25, pm10, no2, o3, co
    )
    df <- dplyr::arrange(df, .data$station_code, .data$datehour)
    return(df)
  }
  
  # If you want a tibble: collect to R, then finish normalization.
  df <- dplyr::collect(df)

  # ---- 7) Relabel timezone without shifting the wall clock -------------------
  # This matches your legacy behavior (display/interpretation tz).
  df$datehour <- lubridate::force_tz(df$datehour, tzone = tz)
  
  # ---- 8) Derive time parts in R (lubridate) ---------------------------------
  df <- dplyr::mutate(
    df,
    year  = lubridate::year(.data$datehour),
    month = lubridate::month(.data$datehour),
    day   = lubridate::day(.data$datehour),
    hour  = lubridate::hour(.data$datehour)
  )
  
  # ---- 9) Final hygiene: numeric coercion + stable column order --------------
  num_cols <- c("pm25", "pm10", "no2", "o3", "co")
  df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
  
  # Normalize station_code *now* (safe in R): trim + upper for stable joins.
  df$station_code <- toupper(trimws(df$station_code))
  
  keep_order <- c("datehour", "year", "month", "day", "hour", "station", "station_code",
                  "pm25", "pm10", "no2", "o3", "co")
  df <- dplyr::select(df, dplyr::all_of(keep_order)) |>
    dplyr::arrange(.data$station_code, .data$datehour)
  
  df
}


# --------------------------------------------------------------------------------------------
# Function: compare_panels
# @Arg       : old_df      — legacy-prepared tibble
# @Arg       : new_df      — new-prepared tibble
# @Arg       : keys        — key cols (default station,y/m/d/h; consider
#                            station_code,y/m/d/h for robustness)
# @Arg       : values      — value columns to compare
# @Arg       : tol         — named numeric tolerances per column (defaults 0)
# @Arg       : restrict_to_old_codes — if TRUE, keep in new_df only rows whose
#               station_code exists in old_df (no-op if column missing)
# @Arg       : prefer_station — named chr vec: station_code -> preferred name
#               (applies to new_df). Example: c(ATI = "Atizapán")
# @Arg       : new_exclude  — rows to drop from new_df before join:
#               * character: station_code values to remove
#               * data.frame/tibble: subset of cols to anti_join away
#               * function(df): returns filtered new_df
# @Arg       : new_shift_hours — integer hours to shift new_df time by.
#               Positive = move forward; negative = move backward.
#               Works if new_df has 'datetime' OR y/m/d/h columns.
# @Output    : list(only_old, only_new, diffs_long, diff_summary)
# @Purpose   : Pinpoint row- and cell-level differences with tolerances.
# --------------------------------------------------------------------------------------------
compare_panels <- function(
    old_df,
    new_df,
    keys   = c("station", "year", "month", "day", "hour"),
    values = c("pm10", "pm25", "ozone", "co", "no2"),
    tol    = c(),
    restrict_to_old_codes = TRUE,
    prefer_station = NULL,
    new_exclude = NULL,
    new_shift_hours = 0L
) {
  # 0) helper: fetch tolerance (0 if not provided)
  tol_get <- function(v) {
    if (!is.null(tol) && !is.null(tol[[v]]) && is.finite(tol[[v]]))
      as.numeric(tol[[v]]) else 0
  }
  
  # 0a) sanity for shift param
  if (length(new_shift_hours) != 1L || is.na(new_shift_hours) ||
      !is.finite(new_shift_hours)) {
    stop("new_shift_hours must be a single finite number.")
  }
  new_shift_hours <- as.integer(new_shift_hours)
  
  # 0b) optional: restrict new_df to codes present in old_df
  if (isTRUE(restrict_to_old_codes) &&
      "station_code" %in% names(old_df) &&
      "station_code" %in% names(new_df)) {
    keep_codes <- unique(old_df$station_code)
    keep_codes <- keep_codes[!is.na(keep_codes) & nzchar(keep_codes)]
    new_df <- new_df[new_df$station_code %in% keep_codes |
                       is.na(new_df$station_code), , drop = FALSE]
  }
  
  # 0c) explicit exclusions for new_df
  if (!is.null(new_exclude)) {
    if (is.character(new_exclude)) {
      if ("station_code" %in% names(new_df)) {
        new_df <- new_df[!(new_df$station_code %in% new_exclude), ,
                         drop = FALSE]
      }
    } else if (is.data.frame(new_exclude)) {
      by_cols <- intersect(names(new_df), names(new_exclude))
      if (length(by_cols)) {
        new_df <- dplyr::anti_join(new_df, new_exclude, by = by_cols)
      }
    } else if (is.function(new_exclude)) {
      new_df <- new_exclude(new_df)
    }
  }
  
  # 0d) disambiguate duplicated codes by preferred station
  if (!is.null(prefer_station) &&
      "station_code" %in% names(new_df) &&
      "station" %in% names(new_df)) {
    stopifnot(is.character(prefer_station))
    for (sc in names(prefer_station)) {
      nm <- unname(prefer_station[[sc]])
      new_df <- new_df[!(new_df$station_code == sc &
                           !is.na(new_df$station) &
                           new_df$station != nm), , drop = FALSE]
    }
  }
  
  # 0e) apply time shift on new_df, if requested
  if (new_shift_hours != 0L) {
    h <- as.difftime(as.numeric(new_shift_hours), units = "hours")
    
    has_dt    <- "datetime" %in% names(new_df)
    has_parts <- all(c("year","month","day","hour") %in% names(new_df))
    
    if (!has_dt && !has_parts) {
      stop("To shift time, new_df must have 'datetime' or y/m/d/h columns.")
    }
    
    if (!has_dt && has_parts) {
      # Build a temporary UTC datetime from y/m/d/h, then shift
      tmp_dt <- ISOdatetime(new_df$year, new_df$month, new_df$day,
                            new_df$hour, 0, 0, tz = "UTC")
      tmp_dt <- as.POSIXct(tmp_dt, tz = "UTC")
      tmp_dt <- tmp_dt + h
      # Overwrite parts with shifted components
      lt <- as.POSIXlt(tmp_dt, tz = "UTC")
      new_df$year  <- as.integer(lt$year + 1900L)
      new_df$month <- as.integer(lt$mon + 1L)
      new_df$day   <- as.integer(lt$mday)
      new_df$hour  <- as.integer(lt$hour)
    } else {
      # Shift existing datetime, keep its tz attribute as-is
      dt <- new_df$datetime
      if (!inherits(dt, "POSIXt")) dt <- as.POSIXct(dt, tz = "UTC")
      dt <- dt + h
      new_df$datetime <- dt
      # If date-parts exist, recompute them from shifted datetime
      if ("year" %in% names(new_df)) {
        lt <- as.POSIXlt(dt)
        if ("year"  %in% names(new_df))
          new_df$year  <- as.integer(lt$year + 1900L)
        if ("month" %in% names(new_df))
          new_df$month <- as.integer(lt$mon + 1L)
        if ("day"   %in% names(new_df))
          new_df$day   <- as.integer(lt$mday)
        if ("hour"  %in% names(new_df))
          new_df$hour  <- as.integer(lt$hour)
      }
    }
  }
  
  # 1) sanity checks
  miss_old <- setdiff(c(keys, values), names(old_df))
  miss_new <- setdiff(c(keys, values), names(new_df))
  if (length(miss_old) || length(miss_new)) {
    stop(
      "Missing columns. ",
      if (length(miss_old))
        paste0("old_df lacks: ", paste(miss_old, collapse = ", ")),
      if (length(miss_old) && length(miss_new)) " ; ",
      if (length(miss_new))
        paste0("new_df lacks: ", paste(miss_new, collapse = ", "))
    )
  }
  
  # 2) join on keys, keep both sides of value columns
  joined <- dplyr::full_join(
    dplyr::select(old_df, dplyr::all_of(c(keys, values))),
    dplyr::select(new_df, dplyr::all_of(c(keys, values))),
    by = keys,
    suffix = c("_old", "_new")
  )
  
  # 3) rows present only in old or only in new
  has_any_old <- joined |>
    dplyr::select(dplyr::ends_with("_old")) |>
    dplyr::mutate(
      any_old = rowSums(!is.na(dplyr::across(dplyr::everything()))) > 0
    ) |>
    (\(x) x$any_old)()
  
  has_any_new <- joined |>
    dplyr::select(dplyr::ends_with("_new")) |>
    dplyr::mutate(
      any_new = rowSums(!is.na(dplyr::across(dplyr::everything()))) > 0
    ) |>
    (\(x) x$any_new)()
  
  only_old <- joined[ has_any_old & !has_any_new,
                      c(keys, grep("_old$", names(joined), value = TRUE)),
                      drop = FALSE]
  
  only_new <- joined[!has_any_old &  has_any_new,
                     c(keys, grep("_new$", names(joined), value = TRUE)),
                     drop = FALSE]
  
  # 4) long, cell-level diffs (carry value_old/value_new explicitly)
  diffs_long <- purrr::map_dfr(values, function(v) {
    vo <- paste0(v, "_old"); vn <- paste0(v, "_new")
    if (!(vo %in% names(joined) && vn %in% names(joined))) return(NULL)
    dplyr::select(joined, dplyr::all_of(keys), !!vo, !!vn) |>
      dplyr::filter(!is.na(.data[[vo]]) | !is.na(.data[[vn]])) |>
      dplyr::mutate(
        variable   = v,
        value_old  = .data[[vo]],
        value_new  = .data[[vn]],
        diff       = value_new - value_old,
        absv       = abs(diff),
        within_tol = absv <= tol_get(v)
      ) |>
      dplyr::select(variable, dplyr::all_of(keys),
                    value_old, value_new, diff, absv, within_tol)
  })
  
  # 5) summaries by variable
  diff_summary <- diffs_long |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      n              = dplyr::n(),
      n_diff         = sum(!within_tol | is.na(within_tol)),
      n_both_na      = sum(is.na(value_old) & is.na(value_new)),
      share_diff     = n_diff / n,
      mean_abs_diff  = mean(absv, na.rm = TRUE),
      p95_abs_diff   = stats::quantile(absv, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    only_old     = only_old,
    only_new     = only_new,
    diffs_long   = diffs_long,
    diff_summary = diff_summary
  )
}


# --------------------------------------------------------------------------------------------
# compare_ground_stations
# @Arg      : cfg              — city cfg list. Must contain a $compare sublist with:
#                                legacy_single_csv, legacy_dir, legacy_pattern,
#                                compare_years, value_cols, residual_map,
#                                pipeline_tz, focus_pollutants (optional).
# @Arg      : out_root         — root output folder; {out_root}/{cfg$id}/ is created.
# @Arg      : focus_pollutants — character; restrict comparison to these pollutants.
#                                Must be a subset of cfg$compare$value_cols. Default NULL falls 
#                                back to cfg$compare$focus_pollutants, then 
#                                cfg$compare$value_cols (all pollutants)
# @Arg      : pipeline_tz      — string; timezone used when the Arrow dataset was BUILT.
#                                Set to "UTC" when bogota_process_stations_data_to_parquet was 
#                                called with tz = "UTC" (the Bogotá default, used to avoid a 
#                                DuckDB R-driver timezone-shift bug). Intentionally separate 
#                                from cfg$tz, which holds the city's true local timezone and 
#                                must remain intact for other pipeline steps. NULL falls back 
#                                to cfg$compare$pipeline_tz, then cfg$tz.
# @Arg      : tol        —  named numeric; per-pollutant tolerance. Default 0.
# @Arg      : quiet      — logical; suppress messages. Default FALSE.
#
# @Output   : named list (invisible) with:
#   $diff_summary, $diffs_long, $only_legacy, $only_new,
#   $station_audit, $out_dir
#   Five Parquet files written to {out_root}/{cfg$id}/ground_station_comparison/.
#
# @Purpose  : Create report of new vs legacy data handling.
# @Details  :  
#   SISAIRE CONTAMINATION
#   The new Arrow dataset contains both RMCAB core stations and SISAIRE
#   metro-area stations. After .std_name() normalisation some SISAIRE
#   station names collide with RMCAB names (e.g. municipality "Bolivia" =
#   "BOLIVIA" = RMCAB station "Bolivia"). A full_join on colliding names
#   creates a cartesian product, inflating n and producing wrong values.
#   Fix: new_prep is restricted to the legacy station universe before
#   compare_panels() is called. This mirrors the original script's
#   filter(station %in% rename_map).
#
#   PIPELINE TIMEZONE
#   bogota_process_stations_data_to_parquet passes tz = "UTC" to DuckDB to
#   avoid a known driver bug where POSIXct tzone attributes trigger an
#   implicit clock shift during type coercion. The Parquet therefore stores
#   local Bogotá clock time with the UTC label. When reading those timestamps
#   back, pipeline_tz = "UTC" recovers the correct year/month/day/hour
#   integers. cfg$tz = "America/Bogota" is left unchanged.
#
#   HOUR CONVENTION
#   RMCAB exports hours in the 1–24 range (1 = first hour of the day,
#   24 = midnight = next day 00:00). Both pipelines receive the same raw
#   hour range. The midnight hour (24) is rolled to day+1 hour=0 during
#   legacy preparation. Stations that lack prior-year data cannot produce
#   the Jan 1 00:00 reading (it comes from Dec 31 24:00), which is why
#   that hour appears only in the new pipeline for recently-commissioned
#   stations.
#
#   STATA BUG (combined CSV — not used here)
#   The coauthor's Stata combine script (0_manage_pollution_data_bogota.do)
#   contains a bug: after replacing hour==24 with hour==0, it applies
#   `replace day=day+1 if hour==0` to ALL hour-0 rows. This shifts the
#   correctly-rolled Jan 31 24:00→Feb 1 00:00 a second time, landing it
#   at March 1 00:00. Feb 1 00:00 is therefore absent from the combined
#   CSV, and March 1 00:00 carries an incorrect measurement value. This
#   bug only affects Air_Pollution_Bogota_2002_2023.csv. The individual
#   period CSVs used here are unaffected.
#
# @Written_on: 20/03/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_ground_stations <- function(
    cfg,
    out_root,
    focus_pollutants = NULL,
    pipeline_tz      = NULL,
    tol              = c(),
    quiet            = FALSE
) {
  
  # 1) Validate that the cfg contains a $compare sublist and all required fields.
  cmp <- cfg$compare
  if (is.null(cmp))
    stop("[", cfg$id, "] cfg$compare is NULL. Add a compare sublist.")
  
  required <- c(
    "legacy_dir", "legacy_pattern", "legacy_single_csv",
    "compare_years", "value_cols", "residual_map"
  )
  missing_f <- setdiff(required, names(cmp))
  if (length(missing_f))
    stop(
      "[", cfg$id, "] cfg$compare missing: ",
      paste(missing_f, collapse = ", ")
    )
  
  # 2) Resolve which pollutants to compare.
  #    Priority: function arg > cfg$compare$focus_pollutants > all value_cols.
  #    This lets the caller narrow the comparison without editing the cfg.
  active_pols <- focus_pollutants %||%
    cmp$focus_pollutants %||%
    cmp$value_cols
  
  # Guard: caller cannot request a pollutant that is absent from value_cols
  unknown_pols <- setdiff(active_pols, cmp$value_cols)
  if (length(unknown_pols))
    stop(
      "[", cfg$id, "] focus_pollutants not in value_cols: ",
      paste(unknown_pols, collapse = ", ")
    )
  
  # 3) Resolve the timezone for reading the Arrow dataset.
  # pipeline_tz is intentionally separate from cfg$tz (the city's true local timezone) 
  # because the processing pipeline may store timestamps with a different label to avoid 
  # DuckDB driver bugs.
  # Priority: function arg > cfg$compare$pipeline_tz > cfg$compare$tz > cfg$tz.
  cmp_tz <- pipeline_tz %||%
    cmp$pipeline_tz %||%
    cmp$tz %||%
    cfg$tz
  
  # 4) Locate the new Arrow dataset. The naming convention is fixed:
  #    data/raw/monitoring_stations/{city_id}_metro_dataset.
  new_arrow_dir <- here::here(
    "data", "raw", "monitoring_stations",
    paste0(cfg$id, "_metro_dataset")
  )
  if (!dir.exists(new_arrow_dir))
    stop("[", cfg$id, "] Arrow dir not found:\n  ", new_arrow_dir)
  
  # Create the per-city output folder under out_root.
  out_dir <- file.path(out_root, "ground_station_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet) {
    message("[", cfg$id, "] Comparing ground stations ...")
    message(
      "  Pollutants : ", paste(active_pols, collapse = ", "),
      " | Years : ", paste(cmp$compare_years, collapse = ", "),
      " | Pipeline tz : ", cmp_tz
    )
  }

  # 5) Load and prepare the legacy dataset.
  # Two sub-paths:
  # A) Single merged CSV (produced by the coauthor's Stata pipeline).
  #    Contains a known bug: Jan 31 24:00 is wrongly mapped to March 1
  #    00:00 in the combined file (see @Details above). Avoid this path
  #    for Bogotá unless the bug has been corrected.
  # B) Individual period CSVs (preferred). Exported before the Stata
  #    combine+correction step, so free of the day+1 bug.
  # -----------------------------------------------------------------------
  use_single <- !is.null(cmp$legacy_single_csv) &&
    file.exists(cmp$legacy_single_csv)
  
  if (use_single) {
    # Sub-path A: one CSV covering all years
    if (!quiet)
      message("[", cfg$id, "] Reading single merged CSV ...")
    legacy_prep <- prepare_legacy_single_csv(
      legacy_csv    = cmp$legacy_single_csv,
      residual_map  = cmp$residual_map,
      drop_stations = cmp$drop_stations %||% character(0),
      tz            = cmp_tz,
      compare_years = cmp$compare_years,
      panelize      = TRUE,   # expand to full station × hour grid
      verbose       = !quiet
    )
  } else {
    # Sub-path B: multiple period CSVs (e.g. 2002-2007, 2008-2013, ...)
    if (!quiet)
      message("[", cfg$id, "] Reading period CSVs ...")
    if (!dir.exists(cmp$legacy_dir))
      stop("[", cfg$id, "] legacy_dir not found: ", cmp$legacy_dir)
    
    # Row-bind all matching CSVs from legacy_dir
    legacy_raw <- read_legacy_period_csvs(
      dir     = cmp$legacy_dir,
      pattern = cmp$legacy_pattern,
      tz      = cmp_tz
    )
    
    # Harmonise column types, roll hour==24 to next-day 00:00, and optionally expand to
    # a balanced panel across compare_years.
    legacy_prep <- prepare_legacy_bogota(
      legacy_df     = legacy_raw,
      rename_map    = cmp$residual_map,
      drop_stations = cmp$drop_stations %||% character(0),
      tz            = cmp_tz,
      panelize      = TRUE,
      panel_years   = cmp$compare_years,
      verbose       = !quiet
    )
    
    # Standardize names AFTER internal renaming so the residual_map is applied to the
    # already-renamed values, not raw CSV names.
    legacy_prep$station <- .std_name(legacy_prep$station)
    if (length(cmp$residual_map) > 0L)
      legacy_prep$station <- dplyr::recode(
        legacy_prep$station, !!!cmp$residual_map
      )
  }

  # 6) Load and prepare the new Arrow dataset.
  # We intentionally pass rename_map = c() and drop_stations = character(0) here — name 
  # standardization and exclusion are applied manually below so both datasets go through the
  # same normalisation path regardless of which legacy branch was taken above.
  # -----------------------------------------------------------------------
  new_ds   <- arrow::open_dataset(new_arrow_dir)
  new_prep <- prepare_new_bogota_like_legacy(
    new_df        = new_ds,
    rename_map    = c(),          # applied manually below
    drop_stations = character(0), # applied manually below
    year_keep     = cmp$compare_years,
    hour_shift    = 0L,
    tz            = cmp_tz
  )
  
  # Apply the same normalisation pipeline as legacy
  new_prep$station <- .std_name(new_prep$station)
  if (length(cmp$residual_map) > 0L)
    new_prep$station <- dplyr::recode(
      new_prep$station, !!!cmp$residual_map
    )
  
  # Apply explicit exclusions (if any) after normalizing names
  drop_std <- .std_name(cmp$drop_stations %||% character(0))
  if (length(drop_std) > 0L)
    new_prep <- new_prep[!new_prep$station %in% drop_std, , drop = FALSE]
  
  # 7) CRITICAL: restrict new_prep to the legacy station universe.
  # We build a station_audit table first so the removed stations are documented in the output
  # (shown in the Quarto report).
  # -----------------------------------------------------------------------
  legacy_stations <- unique(legacy_prep$station)
  new_stations    <- unique(new_prep$station)
  
  # Stations in new that are absent from legacy — these are SISAIRE-only
  sisaire_only  <- setdiff(new_stations, legacy_stations)
  
  # Legacy stations not found in new — signals a name-matching failure
  unmatched_leg <- setdiff(legacy_stations, new_stations)
  
  station_audit <- data.frame(
    station      = sort(union(legacy_stations, new_stations)),
    in_legacy    = sort(union(legacy_stations, new_stations)) %in%
      legacy_stations,
    in_new       = sort(union(legacy_stations, new_stations)) %in%
      new_stations,
    sisaire_only = sort(union(legacy_stations, new_stations)) %in%
      sisaire_only
  )
  
  # Keep only RMCAB stations (those present in legacy) in the new dataset
  new_prep <- new_prep[
    new_prep$station %in% legacy_stations, ,
    drop = FALSE
  ]
  
  if (!quiet) {
    message(sprintf(
      "  Stations — legacy: %d | new (all): %d | SISAIRE dropped: %d",
      length(legacy_stations),
      length(new_stations),
      length(sisaire_only)
    ))
    if (length(unmatched_leg) > 0)
      message(
        "  WARNING — legacy stations absent from new: ",
        paste(unmatched_leg, collapse = ", ")
      )
  }
  
  # 8) Build per-pollutant tolerances. Caller overrides take precedence.
  base_tol <- stats::setNames(rep(0, length(active_pols)), active_pols)
  for (v in names(tol))
    if (v %in% active_pols) base_tol[v] <- tol[v]
  
  # 9) Run the comparison.
  #    compare_panels() does a full_join on keys, then computes cell-level
  #    differences for each value column. Tolerance is applied per pollutant.
  # -----------------------------------------------------------------------
  res <- compare_panels(
    old_df = legacy_prep,
    new_df = new_prep,
    keys   = c("station", "year", "month", "day", "hour"),
    values = active_pols,
    tol    = base_tol
  )
  
  # Derive the rows-only-in-new summary (station × key columns only)
  missing_new <- res$only_new |>
    dplyr::distinct(station, year, month, day, hour) |>
    dplyr::arrange(station, year, month, day, hour)
  
  # 10) Persist results as Parquet.
  #     All artefacts go into one subfolder so they are easy to find and
  #     read together in the Quarto report. No CSV or RDS files are written.
  # -----------------------------------------------------------------------
  cmp_dir <- file.path(out_dir, "ground_station_comparison")
  dir.create(cmp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Helper: write one Parquet with zstd compression (good ratio + fast reads)
  .wpq <- function(df, name)
    arrow::write_parquet(
      dplyr::as_tibble(df),
      file.path(cmp_dir, paste0(name, ".parquet")),
      compression = "zstd"
    )
  
  .wpq(res$diff_summary, "diff_summary")
  .wpq(res$diffs_long,   "diffs_long")
  .wpq(res$only_old,     "only_legacy")
  .wpq(res$only_new,     "only_new")
  .wpq(station_audit,    "station_audit")
  
  if (!quiet)
    message("[", cfg$id, "] Saved to: ", cmp_dir)
  
  # 11) Return a list so the caller can inspect results without
  #     re-reading the Parquet files.
  invisible(list(
    diff_summary  = res$diff_summary,
    diffs_long    = res$diffs_long,
    only_legacy   = dplyr::as_tibble(res$only_old),
    only_new      = dplyr::as_tibble(res$only_new),
    station_audit = station_audit,
    out_dir       = cmp_dir
  ))
}


# --------------------------------------------------------------------------------------------
# compare_metro_area
# @Arg      : cfg              — city cfg list (must contain $id, $tz).
# @Arg      : out_root         — root output folder; {out_root}/{cfg$id}/ is created.
# @Arg      : new_metro_gpkg   — character; path to the new pipeline metro area GeoPackage.
# @Arg      : new_stations_gpkg — character; path to the new pipeline stations GeoPackage.
# @Arg      : legacy_shp_dir   — character; path to the directory containing the legacy
#                                 metro area shapefile (.shp + sidecar files).
# @Arg      : station_audit    — data.frame; output from compare_ground_stations()$station_audit.
#                                 Used to identify which stations belong to the legacy RMCAB
#                                 universe. If NULL, legacy stations are inferred from the
#                                 new stations sf by filtering for source containing "RMCAB".
# @Arg      : buffer_km        — numeric; radius (km) for outside-metro station buffers
#                                 in the new pipeline map. Default 20.
# @Arg      : quiet            — logical; suppress messages. Default FALSE.
#
# @Output   : named list (invisible) with:
#   $summary        — tibble comparing key metrics (area, n_municipalities, n_stations)
#   $new_metro_sf   — sf object of the new pipeline metro area (WGS84)
#   $legacy_metro_sf — sf object of the legacy metro area (WGS84)
#   $new_stations_sf — sf object of ALL new pipeline stations (WGS84)
#   $legacy_stations_sf — sf object of legacy-universe stations (WGS84)
#   $out_dir        — path to the output directory
#   Four Parquet/GeoPackage files written to
#   {out_root}/{cfg$id}/metro_area_comparison/.
#
# @Purpose  : Compare the geographic definitions of the metropolitan area and station
#             coverage between the Dropbox legacy pipeline and the new automated pipeline.
# @Details  :
#   The new pipeline uses the official SDP (2022) metropolitan area definition
#   (Bogotá D.C. + 20 municipalities), while the legacy pipeline uses a
#   Wikipedia-derived definition (23 municipalities). This function loads both
#   boundary polygons, computes summary statistics (total area, number of
#   municipalities, number of stations inside/outside the metro), and writes
#   comparison artefacts for the Quarto report.
#
#   STATION MATCHING
#   Legacy stations are identified by cross-referencing the station_audit table
#   from compare_ground_stations(). If station_audit is not available, the
#   function falls back to filtering the new stations sf by the "source" column
#   for entries containing "RMCAB" (all RMCAB stations are present in the legacy).
#
#   COORDINATE REFERENCE SYSTEM
#   Both metro area polygons are transformed to WGS84 (EPSG:4326) for Leaflet
#   rendering and to a local UTM zone for accurate area calculations.
#
# @Written_on: 10/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_metro_area <- function(
    cfg,
    out_root,
    station_audit = NULL,
    quiet         = FALSE
) {
  
  # 0) Dependencies
  req_pkgs <- c("sf", "dplyr", "tibble", "arrow")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  # 1) Validate cfg and extract paths
  cmp <- cfg$compare
  if (is.null(cmp)) stop("[", cfg$id, "] cfg$compare is NULL.")
  
  req_fields <- c("new_metro_gpkg", "new_stations_gpkg", "legacy_shp_dir")
  missing_f <- setdiff(req_fields, names(cmp))
  if (length(missing_f) > 0) {
    stop("[", cfg$id, "] cfg$compare missing: ", paste(missing_f, collapse = ", "))
  }
  
  if (!file.exists(cmp$new_metro_gpkg))
    stop("[", cfg$id, "] New metro GPKG not found: ", cmp$new_metro_gpkg)
  if (!file.exists(cmp$new_stations_gpkg))
    stop("[", cfg$id, "] New stations GPKG not found: ", cmp$new_stations_gpkg)
  if (!dir.exists(cmp$legacy_shp_dir))
    stop("[", cfg$id, "] Legacy shapefile dir not found: ", cmp$legacy_shp_dir)
  
  out_dir <- file.path(out_root, cfg$id, "metro_area_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet) message("[", cfg$id, "] Comparing metro area definitions ...")
  
  # 2) Load the new pipeline metro area and stations
  # sf::st_read loads spatial files. The |> pipe passes the data to st_transform
  # to standardise the map coordinates to EPSG 4326 (standard GPS Lat/Lon).
  new_metro_sf <- sf::st_read(cmp$new_metro_gpkg, quiet = TRUE) |>
    sf::st_transform(4326)
  
  new_stations_sf <- sf::st_read(cmp$new_stations_gpkg, quiet = TRUE) |>
    sf::st_transform(4326)
  
  # 3) Load the legacy metro area shapefile
  # Find the main .shp file inside the directory, ignoring sidecar files (.shx, etc)
  shp_file <- list.files(
    cmp$legacy_shp_dir, pattern = "\\.shp$", 
    full.names = TRUE, ignore.case = TRUE
  )
  if (length(shp_file) == 0)
    stop("[", cfg$id, "] No .shp file found in: ", cmp$legacy_shp_dir)
  
  legacy_metro_sf <- sf::st_read(shp_file[1], quiet = TRUE) |>
    sf::st_transform(4326)
  
  if (!quiet) {
    message("  New metro: ", nrow(new_metro_sf), " features | ",
            "Legacy metro: ", nrow(legacy_metro_sf), " features")
  }
  
  # 4) Identify legacy vs new stations
  # Create a standardised name column to allow safe text matching
  if ("station_name" %in% names(new_stations_sf)) {
    new_stations_sf$station_std <- .std_name(new_stations_sf$station_name)
  }
  
  if (!is.null(station_audit)) {
    # Extract names confirmed to be in the legacy dataset
    legacy_names <- station_audit$station[station_audit$in_legacy]
    
    # Filter the dataset. .data$ is used to safely reference column names.
    # grepl() searches for the text "RMCAB" inside the source column.
    legacy_stations_sf <- new_stations_sf |>
      dplyr::filter(
        .data$station_std %in% legacy_names |
          grepl("RMCAB", .data$source, ignore.case = TRUE)
      )
    
    # Deduplicate: if regex grabbed too many, strict filter by exact name match
    if (nrow(legacy_stations_sf) > length(legacy_names)) {
      legacy_stations_sf <- new_stations_sf |>
        dplyr::filter(.data$station_std %in% legacy_names)
    }
  } else {
    # Fallback if no audit table: just look for RMCAB in the source metadata
    if ("source" %in% names(new_stations_sf)) {
      legacy_stations_sf <- new_stations_sf |>
        dplyr::filter(grepl("RMCAB", .data$source, ignore.case = TRUE))
    } else {
      warning("[", cfg$id, "] No audit or source column. Using all as legacy.")
      legacy_stations_sf <- new_stations_sf
    }
  }
  
  if (!quiet) {
    message("  Stations — new pipeline: ", nrow(new_stations_sf),
            " | legacy universe: ", nrow(legacy_stations_sf))
  }
  
  # 5) Compute area statistics in metric CRS
  # To calculate area accurately, we must project coordinates from degrees to meters.
  # This formula finds the correct local UTM zone based on the map's center.
  bb <- sf::st_bbox(new_metro_sf)
  lon_center <- (bb["xmin"] + bb["xmax"]) / 2
  lat_center <- (bb["ymin"] + bb["ymax"]) / 2
  utm_zone <- floor((lon_center + 180) / 6) + 1
  epsg_utm <- if (lat_center >= 0) 32600 + utm_zone else 32700 + utm_zone
  
  # Apply the metric projection to polygons
  new_metro_m    <- sf::st_transform(new_metro_sf, epsg_utm)
  legacy_metro_m <- sf::st_transform(legacy_metro_sf, epsg_utm)
  
  # st_area calculates square meters. Divide by 1e6 to get square kilometers.
  new_area_km2    <- as.numeric(sum(sf::st_area(new_metro_m))) / 1e6
  legacy_area_km2 <- as.numeric(sum(sf::st_area(legacy_metro_m))) / 1e6
  
  # Apply the metric projection to station points
  new_stations_m    <- sf::st_transform(new_stations_sf, epsg_utm)
  legacy_stations_m <- sf::st_transform(legacy_stations_sf, epsg_utm)
  
  # st_within to return a matrix showing which points fall inside the polygon.
  n_new_inside_new <- sum(sf::st_within(
    new_stations_m, sf::st_union(new_metro_m), sparse = FALSE
  )[, 1])

  # st_union to merge the metro shapes into one solid boundary before checking.
  n_legacy_inside_legacy <- sum(sf::st_within(
    legacy_stations_m, sf::st_union(legacy_metro_m), sparse = FALSE
  )[, 1])
  
  # Stations outside the metro are simply total stations minus inside stations
  n_new_outside_new <- nrow(new_stations_sf) - n_new_inside_new
  n_legacy_outside_legacy <- nrow(legacy_stations_sf) - n_legacy_inside_legacy
  
  # Build a tidy data frame to hold the final metrics
  summary_tbl <- tibble::tibble(
    Metric = c(
      "Total area (km\u00b2)",
      "Number of municipalities/features",
      "Total stations",
      "Stations inside metro",
      "Stations outside metro (with buffer)"
    ),
    `Dropbox legacy` = c(
      round(legacy_area_km2, 1),
      nrow(legacy_metro_sf),
      nrow(legacy_stations_sf),
      n_legacy_inside_legacy,
      n_legacy_outside_legacy
    ),
    `New pipeline` = c(
      round(new_area_km2, 1),
      nrow(new_metro_sf),
      nrow(new_stations_sf),
      n_new_inside_new,
      n_new_outside_new
    )
  )
  
  # 6) Persist artefacts
  # Write the summary dataframe to Parquet for fast reading in Quarto
  arrow::write_parquet(
    summary_tbl,
    file.path(out_dir, "metro_summary.parquet"),
    compression = "zstd"
  )
  
  # Write spatial objects to GeoPackage format so Quarto can map them
  sf::st_write(new_metro_sf, file.path(out_dir, "new_metro.gpkg"), 
               delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(legacy_metro_sf, file.path(out_dir, "legacy_metro.gpkg"), 
               delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(new_stations_sf, file.path(out_dir, "new_stations.gpkg"), 
               delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(legacy_stations_sf, file.path(out_dir, "legacy_stations.gpkg"), 
               delete_dsn = TRUE, quiet = TRUE)
  
  if (!quiet) message("[", cfg$id, "] Metro comparison saved to: ", out_dir)
  
  invisible(list(
    summary            = summary_tbl,
    new_metro_sf       = new_metro_sf,
    legacy_metro_sf    = legacy_metro_sf,
    new_stations_sf    = new_stations_sf,
    legacy_stations_sf = legacy_stations_sf,
    out_dir            = out_dir
  ))
}


# --------------------------------------------------------------------------------------------
# compare_census
# @Arg cfg          : city cfg list. Must contain a $compare sublist with:
#                     new_census_collapsed, legacy_census_collapsed, 
#                     census_join_key, and optionally census_tol and individual paths.
# @Arg out_root     : root output folder; {out_root}/{cfg$id}/ is created.
# @Arg compare_vars : character vector; variable names to compare. Default covers 
#                     core education/labor shares.
# @Arg quiet        : logical; suppress messages. Default FALSE.
#
# @Output   : named list (invisible) with:
#   $collapsed_summary — tibble; per-variable match statistics
#   $collapsed_diffs   — tibble; rows where values differ beyond tolerance
#   $geo_coverage      — tibble; geographic units present in each pipeline
#   $individual_summary — tibble or NULL; row counts and key stats from micro data
#   $out_dir           — path to the output directory
#   Parquet files written to {out_root}/{cfg$id}/census_comparison/.
#
# @Purpose  : Compare census processing between the new pipeline and the Dropbox legacy.
# @Details  :
#   SCOPE
#   This function compares the Extended 2005 Census processing, which is the
#   census version used in the Dropbox legacy pipeline. The new pipeline also
#   supports the Basic 2005 and 2018 Census, but those have no legacy
#   counterpart to compare against.
#
#   GEOGRAPHIC UNITS
#   Both pipelines collapse the Extended 2005 Census to LocCodigo level:
#   - Bogotá D.C.: LocCodigo = localidad code (2 digits, "01"–"20")
#   - Cundinamarca: LocCodigo = department + municipality code (5 digits, e.g. "25740")
#   The new pipeline's GEO_ID for the extended version is constructed as
#   paste0(dept_code, muni_code, localidad_code), which maps to LocCodigo
#   for Cundinamarca (first 5 chars) and to the localidad for Bogotá.
#
#   METRO AREA DIFFERENCES
#   The legacy pipeline uses a different set of Cundinamarca municipalities
#   (22 municipalities from the Wikipedia definition) than the new pipeline
#   (20 municipalities from the SDP 2022 definition). Geographic units that
#   appear in only one pipeline are flagged but do not affect the comparison
#   of overlapping units.
#
#   VARIABLE DEFINITIONS
#   Both pipelines apply the same education harmonisation (P44B3_NIVEL_ANOS
#   mapping) and labour variable construction (P47B_OCUPACION). The new
#   pipeline's R code was written to replicate the Stata logic line-by-line.
#   Any differences should therefore be small rounding artefacts from the
#   expansion factor (fe = round(FACT_EXP_CAL_P_N)).
#
# @Written_on: 10/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_census <- function(
    cfg,
    out_root,
    compare_vars = c(
      "n", "escolaridad",
      "share_no_education_pop",
      "share_high_school_incomplete_pop",
      "share_high_school_complete_pop",
      "share_college_incomplete_pop",
      "share_college_complete_pop",
      "share_graduate_educ_pop",
      "share_employed_pop"
    ),
    quiet = FALSE
) {
  
  # 0) Dependencies
  req_pkgs <- c("dplyr", "tidyr", "tibble", "readr", "arrow")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  # 1) Validate cfg and extract parameters
  cmp <- cfg$compare
  if (is.null(cmp)) stop("[", cfg$id, "] cfg$compare is NULL.")
  
  req_fields <- c("new_census_collapsed", "legacy_census_collapsed", 
                  "census_join_key")
  missing_f <- setdiff(req_fields, names(cmp))
  if (length(missing_f) > 0) {
    stop("[", cfg$id, "] cfg$compare missing: ", paste(missing_f, collapse = ", "))
  }
  
  new_col_path <- cmp$new_census_collapsed
  leg_col_path <- cmp$legacy_census_collapsed
  new_ind_path <- cmp$new_census_individual
  leg_ind_path <- cmp$legacy_census_individual
  
  join_key <- cmp$census_join_key
  tol      <- if (!is.null(cmp$census_tol)) cmp$census_tol else 0.001
  
  if (!file.exists(new_col_path))
    stop("[", cfg$id, "] New collapsed census not found: ", new_col_path)
  if (!file.exists(leg_col_path))
    stop("[", cfg$id, "] Legacy collapsed census not found: ", leg_col_path)
  
  out_dir <- file.path(out_root, cfg$id, "census_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet) message("[", cfg$id, "] Comparing census data (Extended 2005) ...")
  
  # 2) Load collapsed datasets
  new_col <- readr::read_csv(new_col_path, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))
  
  legacy_col <- readr::read_csv(leg_col_path, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))
  
  # Legacy sets usually have LocCodigo. Standardise if it exists.
  if ("LocCodigo" %in% names(legacy_col)) {
    legacy_col <- legacy_col |> dplyr::mutate(GEO_ID = LocCodigo)
  }
  
  if (!join_key %in% names(new_col) && "GEO_ID" %in% names(new_col)) {
    if (join_key == "LocCodigo") new_col <- new_col |> dplyr::rename(LocCodigo = GEO_ID)
  }
  
  if (!join_key %in% names(new_col))
    stop("[", cfg$id, "] join_key '", join_key, "' missing in new data.")
  if (!join_key %in% names(legacy_col))
    stop("[", cfg$id, "] join_key '", join_key, "' missing in legacy data.")
  
  new_col[[join_key]]    <- as.character(new_col[[join_key]])
  legacy_col[[join_key]] <- as.character(legacy_col[[join_key]])
  
  if (!quiet) {
    message("  Geo units — new: ", dplyr::n_distinct(new_col[[join_key]]),
            " | legacy: ", dplyr::n_distinct(legacy_col[[join_key]]))
  }
  
  # 3) Geographic coverage audit
  new_geos    <- sort(unique(new_col[[join_key]]))
  legacy_geos <- sort(unique(legacy_col[[join_key]]))
  all_geos    <- sort(union(new_geos, legacy_geos))
  
  geo_coverage <- tibble::tibble(
    geo_unit  = all_geos,
    in_new    = all_geos %in% new_geos,
    in_legacy = all_geos %in% legacy_geos,
    overlap   = all_geos %in% intersect(new_geos, legacy_geos)
  )
  
  if (!quiet) {
    message(sprintf(
      "  Overlap: %d | New only: %d | Legacy only: %d",
      sum(geo_coverage$overlap), 
      sum(geo_coverage$in_new & !geo_coverage$in_legacy), 
      sum(geo_coverage$in_legacy & !geo_coverage$in_new)
    ))
  }
  
  # 4) Compare collapsed values on overlapping geographic units
  overlap_geos <- geo_coverage$geo_unit[geo_coverage$overlap]
  
  # Use cmp$census_vars if provided, otherwise default args
  vars_to_check <- if (!is.null(cmp$census_vars)) cmp$census_vars else compare_vars
  avail_vars    <- intersect(intersect(vars_to_check, names(new_col)), names(legacy_col))
  missing_vars  <- setdiff(vars_to_check, avail_vars)
  
  if (length(missing_vars) > 0 && !quiet) {
    message("  Variables not in both datasets (skipped): ", 
            paste(missing_vars, collapse = ", "))
  }
  
  new_overlap <- new_col |>
    dplyr::filter(.data[[join_key]] %in% overlap_geos) |>
    dplyr::select(dplyr::all_of(c(join_key, avail_vars)))
  
  legacy_overlap <- legacy_col |>
    dplyr::filter(.data[[join_key]] %in% overlap_geos) |>
    dplyr::select(dplyr::all_of(c(join_key, avail_vars)))
  
  joined <- dplyr::full_join(
    new_overlap, legacy_overlap, by = join_key, suffix = c("_new", "_legacy")
  )
  
  diffs_list <- lapply(avail_vars, function(v) {
    v_new <- paste0(v, "_new")
    v_leg <- paste0(v, "_legacy")
    if (!all(c(v_new, v_leg) %in% names(joined))) return(NULL)
    
    tibble::tibble(
      geo_unit     = joined[[join_key]],
      variable     = v,
      value_new    = as.numeric(joined[[v_new]]),
      value_legacy = as.numeric(joined[[v_leg]])
    ) |>
      dplyr::mutate(
        diff       = .data$value_new - .data$value_legacy,
        abs_diff   = abs(.data$diff),
        within_tol = .data$abs_diff <= tol | 
          (is.na(.data$value_new) & is.na(.data$value_legacy))
      )
  })
  diffs_long <- dplyr::bind_rows(diffs_list)
  
  collapsed_summary <- diffs_long |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      n_geo_units   = dplyr::n(),
      n_match       = sum(.data$within_tol, na.rm = TRUE),
      n_diff        = sum(!.data$within_tol, na.rm = TRUE),
      n_both_na     = sum(is.na(.data$value_new) & is.na(.data$value_legacy)),
      share_match   = .data$n_match / .data$n_geo_units,
      mean_abs_diff = mean(.data$abs_diff, na.rm = TRUE),
      max_abs_diff  = max(.data$abs_diff, na.rm = TRUE),
      .groups = "drop"
    )
  
  collapsed_diffs <- diffs_long |> dplyr::filter(!.data$within_tol)
  
  if (!quiet) {
    message(sprintf("  Variables compared: %d | Overall match rate: %.2f%%",
                    length(avail_vars), 100 * mean(collapsed_summary$share_match)))
  }
  
  # 5) Individual-level comparison
  individual_summary <- NULL
  
  if (!is.null(leg_ind_path) && file.exists(leg_ind_path) && file.exists(new_ind_path)) {
    if (!quiet) message("  Comparing individual-level census ...")
    
    new_ind <- readr::read_csv(new_ind_path, show_col_types = FALSE)
    leg_ind <- readr::read_csv(leg_ind_path, show_col_types = FALSE)
    
    names(new_ind) <- tolower(names(new_ind))
    names(leg_ind) <- tolower(names(leg_ind))
    
    ind_stats <- function(df, label) {
      edad_col  <- intersect(c("edad", "pc09b_edad", "raw_age"), names(df))
      edad      <- if (length(edad_col)) as.numeric(df[[edad_col[1]]]) else NA_real_
      esc_col   <- intersect(c("escolaridad"), names(df))
      esc       <- if (length(esc_col)) as.numeric(df[[esc_col[1]]]) else NA_real_
      fe_col    <- intersect(c("fe"), names(df))
      fe        <- if (length(fe_col)) as.numeric(df[[fe_col[1]]]) else rep(1, nrow(df))
      adult_col <- intersect(c("adult"), names(df))
      adult     <- if (length(adult_col)) as.numeric(df[[adult_col[1]]]) else NA_real_
      
      tibble::tibble(
        pipeline         = label,
        total_rows       = nrow(df),
        n_adults         = sum(adult == 1, na.rm = TRUE),
        weighted_n       = sum(fe, na.rm = TRUE),
        mean_escolaridad = stats::weighted.mean(esc, fe, na.rm = TRUE),
        share_women      = if ("women" %in% names(df)) 
          mean(as.numeric(df$women), na.rm = TRUE) else NA_real_,
        n_geo_units      = dplyr::n_distinct(
          df[[intersect(c("geo_id", "loccodigo", tolower(join_key)), names(df))[1]]]
        )
      )
    }
    
    individual_summary <- dplyr::bind_rows(
      ind_stats(new_ind, "New pipeline"),
      ind_stats(leg_ind, "Dropbox legacy")
    )
    
    if (!quiet) {
      message(sprintf("  Individual rows — new: %s | legacy: %s",
                      format(nrow(new_ind), big.mark = ","),
                      format(nrow(leg_ind), big.mark = ",")))
    }
  } else if (!quiet) {
    message("  Individual-level comparison skipped (file not found).")
  }
  
  # 6) Persist artefacts as Parquet
  .wpq <- function(df, name) arrow::write_parquet(
    dplyr::as_tibble(df), file.path(out_dir, paste0(name, ".parquet")), 
    compression = "zstd"
  )
  
  .wpq(collapsed_summary, "collapsed_summary")
  .wpq(collapsed_diffs,   "collapsed_diffs")
  .wpq(geo_coverage,      "geo_coverage")
  if (!is.null(individual_summary)) .wpq(individual_summary, "individual_summary")
  
  if (!quiet) message("[", cfg$id, "] Census comparison saved to: ", out_dir)
  
  invisible(list(
    collapsed_summary  = collapsed_summary,
    collapsed_diffs    = collapsed_diffs,
    geo_coverage       = geo_coverage,
    individual_summary = individual_summary,
    out_dir            = out_dir
  ))
}


# ----------------------------------------------------------------------------------------
# Function: aggregate_idw_exposure_legacy
#
# @Arg arrow_dir      : string; cleaned partitioned Arrow/Parquet hourly data.
# @Arg geo_sta_pq     : string; geo-station distance Parquet (geo_id, station_id,
#                       distance_km) from compute_distance_matrices().
# @Arg census_col     : data.frame; individual-level census (one row per person).
# @Arg geo_id_col     : string; geo ID column in census_col.
# @Arg pop_col        : string; expansion-weight column in census_col.
# @Arg group_var      : string; schooling variable to quintile (e.g. "escolaridad").
# @Arg adult_col      : string; adult filter column. Default "adult".
# @Arg target_year    : integer; the single year to process (legacy ran 2023).
# @Arg buffer_km      : numeric; max geo-to-station distance. Default 3.
# @Arg out_dir        : string; output directory.
# @Arg out_name       : string; output file prefix.
# @Arg quiet          : logical; suppress messages. Default FALSE.
#
# @Output : list(exposure_path, individual_path); writes two parquet files.
#
# @Details:
#   STEP-0 LEGACY REPLICATION ONLY — reproduces the *old* IDW scheme so the
#   Quarto report can compare it to aggregate_idw_exposure() on identical data.
#   It is intentionally NOT missingness-aware: inverse-distance weights are
#   normalized over ALL in-buffer stations in a geo-hour (including those
#   missing the pollutant that hour), then missing products are dropped by a
#   na.rm sum. The kept denominator therefore includes absent stations, which
#   deflates the estimate relative to the corrected method. Quintiles follow
#   the legacy weighted rule: sort adults by schooling, cut on the cumulative
#   expansion-weight share cumsum(fe)/sum(fe) at 0.2 steps. This is the legacy
#   Bogota/CDMX construction; Santiago (fe == 1) is the special case that
#   reduces to equal individual counts. Single year, PM10/PM2.5, power-1
#   weights — matching the 2023 legacy script.
#   Do not use for paper results; use aggregate_idw_exposure() instead.
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# ----------------------------------------------------------------------------------------
aggregate_idw_exposure_legacy <- function(
    arrow_dir,
    geo_sta_pq,
    census_col,
    geo_id_col   = "GEO_ID",
    pop_col      = "fe",
    group_var    = "escolaridad",
    adult_col    = "adult",
    target_year  = 2023L,
    buffer_km    = 3,
    out_dir,
    out_name,
    quiet        = FALSE
) {
  
  pkgs <- c("arrow", "data.table", "stringi")
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("Missing: ", p)
  
  # Normalize station IDs the same way as the distance/IDW steps.
  .norm_sta <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  pollutants <- c("pm10", "pm25")
  # Only it1/it2 reach the yearly table in the legacy script.
  who_it <- list(pm10 = c(it1 = 150, it2 = 100),
                 pm25 = c(it1 = 75,  it2 = 50))
  
  # 1. Distances: keep in-buffer pairs. Legacy used `distance_km <= 3` with no
  # positive-distance filter, so a d == 0 pair would give inv_d = Inf. Match
  # legacy literally but warn if any d == 0 exists.
  dist_dt <- data.table::as.data.table(arrow::read_parquet(geo_sta_pq))
  dist_dt[, geo_id := as.character(geo_id)]
  dist_dt[, station_id := .norm_sta(station_id)]
  dist_dt <- dist_dt[!is.na(distance_km) & distance_km <= buffer_km]
  n_zero <- dist_dt[distance_km == 0, .N]
  if (n_zero > 0L && !quiet) {
    message("[", out_name, "] WARNING: ", n_zero,
            " pair(s) with distance_km == 0 (inv_d = Inf), kept to match legacy.")
  }
  if (nrow(dist_dt) == 0L) stop("No geo-station pairs within ", buffer_km, " km.")
  dist_dt[, inv_d := 1 / distance_km]              # power 1, as legacy
  dist_dt <- dist_dt[, .(geo_id, station_id, inv_d)]
  
  # 2. Hourly pollution for the single target year.
  ds <- arrow::open_dataset(arrow_dir)
  poll <- ds |>
    dplyr::filter(year == target_year) |>
    dplyr::select(station, datetime, dplyr::all_of(pollutants)) |>
    dplyr::collect() |>
    data.table::as.data.table()
  if (nrow(poll) == 0L) stop("No pollution rows for year ", target_year, ".")
  poll[, station_id := .norm_sta(station)]
  
  # 3. Per-pollutant legacy IDW. The denominator is normalized over ALL
  # in-buffer stations present in the geo-hour BEFORE dropping missing values,
  # reproducing the deflating-denominator behavior.
  yearly <- vector("list", length(pollutants))
  names(yearly) <- pollutants
  
  for (pol in pollutants) {
    # Long station-hour readings (value may be NA).
    ph <- poll[, .(station_id, datetime, val = get(pol))]
    
    # Cartesian merge of in-buffer stations to each geo via station_id.
    # Every in-buffer station-hour row is kept, missing val included.
    gh <- merge(ph, dist_dt, by = "station_id", allow.cartesian = TRUE)
    
    # Weight normalized over ALL in-buffer stations in the geo-hour
    # (na.rm only guards Inf/NA in inv_d, not missing values).
    gh[, weight := inv_d / sum(inv_d, na.rm = TRUE),
       by = .(geo_id, datetime)]
    
    # Weighted value; missing val -> NA product, dropped by the na.rm sum.
    gh[, wval := val * weight]
    agg <- gh[, .(idw = sum(wval, na.rm = TRUE)),
              by = .(geo_id, datetime)]
    
    # WHO indicators on the (deflated) hourly estimate.
    thr <- who_it[[pol]]
    agg[, d_it1 := as.integer(idw >= thr[["it1"]])]
    agg[, d_it2 := as.integer(idw >= thr[["it2"]])]
    
    # Annual: mean over all geo-hours (legacy used mean(agg, na.rm=TRUE) with
    # .N total hours; every geo-hour here has a value, possibly 0).
    yr <- agg[, .(
      avg          = mean(idw, na.rm = TRUE),
      hrs_d_it1    = sum(d_it1, na.rm = TRUE),
      hrs_d_it2    = sum(d_it2, na.rm = TRUE),
      total_hrs    = .N
    ), by = geo_id]
    
    data.table::setnames(yr, c("avg", "hrs_d_it1", "hrs_d_it2", "total_hrs"),
                         paste0(c("avg_", "hrs_d_it1_", "hrs_d_it2_",
                                  "total_hrs_"), pol))
    yearly[[pol]] <- yr
  }
  
  # Merge pollutants into one geo-level table.
  exposure <- Reduce(function(a, b) merge(a, b, by = "geo_id", all = TRUE),
                     yearly)
  exposure[, year := target_year]
  
  # 4. Legacy quintiles: adults only, expansion-weighted cumulative cut. Sort by
  # schooling, take cumsum(fe)/sum(fe), cut on seq(0,1,0.2). This is the legacy
  # Bogota/CDMX method; Santiago is the fe == 1 special case (then it reduces to
  # equal individual counts, matching legacy's frank-random partition).
  ce <- data.table::copy(data.table::as.data.table(census_col))
  data.table::setnames(ce, geo_id_col, "geo_id")
  ce[, geo_id := as.character(geo_id)]
  ce <- ce[get(adult_col) == 1]
  if (nrow(ce) == 0L) stop("No adult rows after filtering.")
  
  # Drop NA schooling (no quintile, no contribution to any quintile mean).
  ce <- ce[!is.na(get(group_var))]
  data.table::setorderv(ce, group_var)
  ce[, .cum_w := cumsum(get(pop_col)) / sum(get(pop_col))]
  # include.lowest puts cum_w == 0.2 in bin 1, matching the legacy cut.
  ce[, edu_quintile := as.integer(cut(.cum_w, breaks = seq(0, 1, 0.2),
                                      include.lowest = TRUE, labels = 1:5))]
  ce[, .cum_w := NULL]
  
  # 5. Write outputs (exposure + individual quintiles), mirroring the
  # individual-mode outputs of aggregate_idw_exposure().
  exp_path   <- file.path(out_dir, paste0(out_name, "_idw_exposure.parquet"))
  indiv_path <- file.path(out_dir, paste0(out_name, "_indiv_groups.parquet"))
  arrow::write_parquet(exposure, exp_path)
  arrow::write_parquet(ce, indiv_path)
  
  if (!quiet) message("[", out_name, "] Legacy IDW written: ", exp_path)
  
  invisible(list(exposure_path = exp_path, individual_path = indiv_path))
}


# --------------------------------------------------------------------------------------------
# compare_idw
# @Arg      : cfg              — city cfg list. Must contain a $compare sublist with:
#                                new_station_dist, legacy_station_dist, and optionally
#                                new_geo_dist, legacy_geo_dist, and idw_tol_km.
# @Arg      : out_root         — root output folder; {out_root}/{cfg$id}/ is created.
# @Arg      : station_audit    — data.frame; output from compare_ground_stations().
#                                Used to restrict the new pipeline's distance matrix to 
#                                the legacy station universe for fair comparison.
# @Arg      : quiet            — logical; suppress messages. Default FALSE.
#
# @Output   : named list (invisible) with:
#   $station_dist_summary — tibble; per-station-pair comparison statistics
#   $station_dist_diffs   — tibble; station pairs where distances differ > tol_km
#   $geo_dist_summary     — tibble or NULL; per-geo-unit comparison statistics
#   $geo_dist_diffs       — tibble or NULL; geo-unit-station pairs differing > tol_km
#   $method_note          — character; explains the methodological differences
#   $out_dir              — path to the output directory
#   Parquet files written to {out_root}/{cfg$id}/distance_comparison/.
#
# @Purpose  : Compare distance matrices between the new pipeline and Dropbox legacy.
# @Details  :
#   METHODOLOGICAL DIFFERENCES
#   The new pipeline uses an Azimuthal Equidistant (AEQD) projection centred on
#   the station cloud centroid for Euclidean distance computation. The legacy
#   pipeline uses the Haversine great-circle formula via geosphere::distm().
#   For metro-area scales (~50-200 km), these methods agree within ~0.1-0.5 km.
#   Differences beyond this threshold indicate either different station
#   coordinates or a processing error.
#
#   STATION MATCHING
#   The new pipeline normalises station names (uppercase, no accents). The legacy
#   uses manually recoded names (e.g. "Usaquén" → "Usaquen"). Matching is done
#   after normalising both sides to uppercase ASCII.
#
#   GEO-UNIT DISTANCES
#   The new pipeline computes centroid-to-station distances using the AEQD
#   projection on census tract (manzana/sector) centroids. The legacy computes
#   centroid-to-station distances using sf::st_distance() on EPSG:4674
#   (SIRGAS 2000), which uses the s2 spherical engine. Both should agree closely.
#
# @Written_on: 10/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_idw <- function(
    cfg,
    out_root,
    station_audit = NULL,
    quiet         = FALSE
) {
  
  # 0) Dependencies
  req_pkgs <- c("dplyr", "tidyr", "tibble", "arrow", "data.table")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  # 1) Validate cfg and extract paths
  cmp <- cfg$compare
  if (is.null(cmp)) stop("[", cfg$id, "] cfg$compare is NULL.")
  
  req_fields <- c("new_station_dist", "legacy_station_dist")
  missing_f <- setdiff(req_fields, names(cmp))
  if (length(missing_f) > 0) {
    stop("[", cfg$id, "] cfg$compare missing: ", paste(missing_f, collapse = ", "))
  }
  
  new_station_dist    <- cmp$new_station_dist
  legacy_station_dist <- cmp$legacy_station_dist
  new_geo_dist        <- cmp$new_geo_dist
  legacy_geo_dist     <- cmp$legacy_geo_dist
  tol_km              <- if (!is.null(cmp$idw_tol_km)) cmp$idw_tol_km else 0.5
  
  if (!file.exists(new_station_dist))
    stop("[", cfg$id, "] New station dist file not found: ", new_station_dist)
  if (!file.exists(legacy_station_dist))
    stop("[", cfg$id, "] Legacy station dist file not found: ", legacy_station_dist)
  
  out_dir <- file.path(out_root, cfg$id, "distance_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet) message("[", cfg$id, "] Comparing distance matrices ...")
  
  # 2) Normalisation helper
  .norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    if (requireNamespace("stringi", quietly = TRUE))
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    gsub('["\']', "", x)
  }
  
  # 3) Load new pipeline station distances
  # The new pipeline stores distances in "long" format (station_from, station_to, dist)
  new_sta <- arrow::read_parquet(new_station_dist) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      station_from = .norm(station_from),
      station_to   = .norm(station_to)
    )
  
  # 4) Load legacy station distances
  ext <- tools::file_ext(legacy_station_dist)
  leg_wide <- if (ext == "rds") {
    readRDS(legacy_station_dist)
  } else data.table::fread(legacy_station_dist)
  
  # The legacy pipeline stores distances as a wide matrix. We need to convert it 
  # to long format so we can join it easily with the new pipeline data.
  id_col <- if ("station_code" %in% names(leg_wide)) "station_code" else names(leg_wide)[1]
  sta_cols <- setdiff(names(leg_wide), id_col)
  
  # pivot_longer collapses the matrix columns down into rows
  leg_long <- leg_wide |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(sta_cols),
      names_to = "station_to",
      values_to = "distance_m_legacy"
    ) |>
    dplyr::rename(station_from = !!id_col) |>
    dplyr::mutate(
      station_from       = .norm(station_from),
      station_to         = .norm(station_to),
      distance_km_legacy = as.numeric(distance_m_legacy) / 1000
    ) |>
    dplyr::select(station_from, station_to, distance_km_legacy)
  
  # 5) Restrict new data to legacy station universe
  if (!is.null(station_audit)) {
    legacy_names <- .norm(station_audit$station[station_audit$in_legacy])
  } else {
    legacy_names <- unique(c(leg_long$station_from, leg_long$station_to))
  }
  
  new_sta_filtered <- new_sta |>
    dplyr::filter(station_from %in% legacy_names, station_to %in% legacy_names) |>
    dplyr::rename(distance_km_new = distance_km)
  
  if (!quiet) {
    message(sprintf("  Station pairs — new: %s | legacy: %s",
                    format(nrow(new_sta_filtered), big.mark = ","),
                    format(nrow(leg_long), big.mark = ",")))
  }
  
  # 6) Join and compare
  # full_join matches rows based on the origin and destination stations.
  joined <- dplyr::full_join(
    new_sta_filtered, leg_long, by = c("station_from", "station_to")
  ) |>
    dplyr::mutate(
      diff_km    = distance_km_new - distance_km_legacy,
      abs_diff   = abs(diff_km),
      within_tol = abs_diff <= tol_km | 
        (is.na(distance_km_new) & is.na(distance_km_legacy))
    )
  
  # Summary: filter out self-distances (station A to station A)
  sta_summary <- joined |>
    dplyr::filter(!is.na(distance_km_new) & !is.na(distance_km_legacy)) |>
    dplyr::filter(station_from != station_to) |>
    dplyr::summarise(
      n_pairs       = dplyr::n(),
      n_match       = sum(within_tol, na.rm = TRUE),
      n_diff        = sum(!within_tol, na.rm = TRUE),
      mean_abs_diff = mean(abs_diff, na.rm = TRUE),
      max_abs_diff  = max(abs_diff, na.rm = TRUE),
      share_match   = n_match / n_pairs
    )
  
  sta_diffs <- joined |>
    dplyr::filter(!within_tol) |>
    dplyr::arrange(dplyr::desc(abs_diff))
  
  if (!quiet) {
    message(sprintf("  Station distances: %d pairs match (%.1f%%), %d differ > %.1f km",
                    sta_summary$n_match, 100 * sta_summary$share_match,
                    sta_summary$n_diff, tol_km))
    message(sprintf("  Mean abs diff: %.3f km | Max: %.3f km",
                    sta_summary$mean_abs_diff, sta_summary$max_abs_diff))
  }
  
  # 7) Geo-unit distances (optional)
  geo_summary <- NULL
  geo_diffs   <- NULL
  
  if (!is.null(new_geo_dist) && !is.null(legacy_geo_dist) &&
      file.exists(new_geo_dist) && file.exists(legacy_geo_dist)) {
    if (!quiet) message("  Comparing geo-unit distances ...")
    
    new_geo <- arrow::read_parquet(new_geo_dist) |>
      dplyr::as_tibble() |>
      dplyr::mutate(station_id = .norm(station_id)) |>
      dplyr::rename(distance_km_new = distance_km)
    
    leg_geo <- if (tools::file_ext(legacy_geo_dist) == "rds") {
      data.table::as.data.table(readRDS(legacy_geo_dist))
    } else data.table::fread(legacy_geo_dist)
    
    geo_id_col <- intersect(c("locality", "geo_id", "LocCodigo"), names(leg_geo))[1]
    sta_id_col <- intersect(c("station_code", "station_id"), names(leg_geo))[1]
    dist_col   <- intersect(c("distance", "distance_km"), names(leg_geo))[1]
    
    leg_geo_clean <- leg_geo |>
      dplyr::as_tibble() |>
      dplyr::transmute(
        geo_id             = as.character(.data[[geo_id_col]]),
        station_id         = .norm(.data[[sta_id_col]]),
        distance_km_legacy = as.numeric(.data[[dist_col]])
      )
    
    new_geo_filtered <- new_geo |> dplyr::filter(station_id %in% legacy_names)
    
    geo_joined <- dplyr::inner_join(
      new_geo_filtered, leg_geo_clean, by = c("geo_id", "station_id")
    ) |>
      dplyr::mutate(
        diff_km    = distance_km_new - distance_km_legacy,
        abs_diff   = abs(diff_km),
        within_tol = abs_diff <= tol_km
      )
    
    geo_summary <- geo_joined |>
      dplyr::summarise(
        n_pairs       = dplyr::n(),
        n_match       = sum(within_tol, na.rm = TRUE),
        mean_abs_diff = mean(abs_diff, na.rm = TRUE),
        max_abs_diff  = max(abs_diff, na.rm = TRUE),
        share_match   = n_match / n_pairs
      )
    
    geo_diffs <- geo_joined |>
      dplyr::filter(!within_tol) |>
      dplyr::arrange(dplyr::desc(abs_diff))
    
    if (!quiet) {
      message(sprintf("  Geo distances: %d pairs, %.1f%% match within %.1f km",
                      geo_summary$n_pairs, 100 * geo_summary$share_match, tol_km))
    }
  }
  
  # 8) Method note
  method_note <- paste0(
    "The new pipeline uses AEQD (Azimuthal Equidistant) projection for ",
    "Euclidean distance. The legacy uses Haversine great-circle distance ",
    "(geosphere::distm). At metro scales these agree within ~0.1-0.5 km."
  )
  
  # 9) Persist
  .wpq <- function(df, name) arrow::write_parquet(
    dplyr::as_tibble(df), file.path(out_dir, paste0(name, ".parquet")), 
    compression = "zstd"
  )
  
  .wpq(sta_summary, "station_dist_summary")
  if (nrow(sta_diffs) > 0) .wpq(sta_diffs, "station_dist_diffs")
  if (!is.null(geo_summary)) .wpq(geo_summary, "geo_dist_summary")
  if (!is.null(geo_diffs) && nrow(geo_diffs) > 0) .wpq(geo_diffs, "geo_dist_diffs")
  
  if (!quiet) message("[", cfg$id, "] Distance comparison saved to: ", out_dir)
  
  invisible(list(
    station_dist_summary = sta_summary,
    station_dist_diffs   = sta_diffs,
    geo_dist_summary     = geo_summary,
    geo_dist_diffs       = geo_diffs,
    method_note          = method_note,
    out_dir              = out_dir
  ))
}


# ----------------------------------------------------------------------------------
# compare_outlier_procedure
# @Arg cfg           : city cfg list (must contain $id and $compare sublist).
# @Arg out_root      : root output folder; {out_root}/{cfg$id}/ is created.
# @Arg station_audit : data.frame; from compare_ground_stations().
# @Arg quiet         : logical; suppress messages. Default FALSE.
#
# @Output : named list (invisible) with step_summary, comparison, out_dir.
#           Parquet files written to {out_root}/{cfg$id}/outlier_comparison/.
# ----------------------------------------------------------------------------------
compare_outlier_procedure <- function(
    cfg,
    out_root,
    station_audit = NULL,
    quiet         = FALSE
) {
  
  req_pkgs <- c("dplyr", "tibble", "arrow", "data.table")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  cmp <- cfg$compare
  if (is.null(cmp)) stop("[", cfg$id, "] cfg$compare is NULL.")
  
  new_clean_dir     <- cmp$new_clean_dir
  new_raw_dir       <- here::here("data", "raw", "monitoring_stations", 
                                  paste0(cfg$id, "_metro_dataset"))
  legacy_clean_path <- cmp$legacy_clean_path
  legacy_raw_path   <- cmp$legacy_single_csv
  station_dist_path <- cmp$new_station_dist
  
  pollutants    <- cmp$focus_pollutants %||% c("pm10", "pm25")
  compare_years <- cmp$compare_years
  
  out_dir <- file.path(out_root, "outlier_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  .norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    if (requireNamespace("stringi", quietly = TRUE))
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    gsub('["\']', "", x)
  }
  
  # 1) Check Configuration Toggle for Legacy Test
  out_p <- cmp$outlier_params
  if (isTRUE(out_p$use_legacy_input) && file.exists(legacy_raw_path)) {
    if (!quiet) message("  [Toggle] Running NEW algorithm on LEGACY raw data...")
    if (is.null(station_dist_path)) stop("station_dist_path required for test.")
    
    # Manually and safely load legacy data to bypass external pipeline bugs
    leg_raw <- data.table::fread(legacy_raw_path)
    names(leg_raw) <- tolower(names(leg_raw))
    
    sta_col <- intersect(c("station_code", "station"), names(leg_raw))[1]
    if (!is.na(sta_col)) leg_raw[, station := .norm(get(sta_col))]
    
    if (length(cmp$residual_map) > 0L) {
      leg_raw[, station := dplyr::recode(station, !!!cmp$residual_map)]
    }
    
    # ROBUST DATE MATH: Extracts date, mathematically adds hours (fixes hour==24)
    if (!"datetime" %in% names(leg_raw)) {
      if ("datehour" %in% names(leg_raw)) {
        leg_raw[, raw_date_str := sub(" .*$", "", trimws(datehour))]
        parsed_date <- as.Date(leg_raw$raw_date_str, format = "%d%b%Y")
      } else if ("date" %in% names(leg_raw)) {
        parsed_date <- as.Date(leg_raw$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
      } else {
        parsed_date <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
      }
      
      # Convert Date to POSIXct and add seconds. 
      leg_raw[, datetime := as.POSIXct(format(parsed_date, "%Y-%m-%d"), 
                                       format = "%Y-%m-%d", 
                                       tz = "UTC") + (as.integer(hour) * 3600)]
      attr(leg_raw$datetime, "tzone") <- cmp$pipeline_tz %||% cfg$tz
    }
    
    if (!"year" %in% names(leg_raw)) {
      leg_raw[, year := as.integer(format(datetime, "%Y"))]
    }
    
    mock_raw <- file.path(tempdir(), "legacy_arrow_raw")
    mock_out <- file.path(tempdir(), "legacy_arrow_test")
    if (dir.exists(mock_raw)) unlink(mock_raw, recursive = TRUE)
    
    arrow::write_dataset(leg_raw, mock_raw, format="parquet", partitioning="year")
    
    detect_pollution_outliers(
      arrow_dir           = mock_raw,
      station_dist_path   = station_dist_path,
      out_dir             = mock_out,
      out_name            = "legacy_mock",
      pollutants          = pollutants,
      pct_flag            = out_p$pct_flag %||% 0.99,
      n_sd                = out_p$n_sd %||% 2,
      on_missing_temporal = out_p$on_missing_temporal %||% "finish",
      on_missing_neighbor = out_p$on_missing_neighbor %||% "finish",
      quiet               = quiet
    )
    
    new_raw_dir   <- mock_raw
    new_clean_dir <- file.path(mock_out, "legacy_mock_clean")
  }
  
  if (!dir.exists(new_clean_dir)) stop("New clean dir not found: ", new_clean_dir)
  if (!dir.exists(new_raw_dir)) stop("New raw dir not found: ", new_raw_dir)
  
  if (!quiet) message("[", cfg$id, "] Comparing outlier detection procedures ...")
  
  legacy_names <- if (!is.null(station_audit)) {
    .norm(station_audit$station[station_audit$in_legacy])
  } else NULL
  
  # 2) Load target datasets
  new_raw <- arrow::open_dataset(new_raw_dir) |>
    dplyr::filter(year %in% compare_years) |>
    dplyr::collect() |> data.table::as.data.table()
  new_raw[, station := .norm(station)]
  
  new_clean <- arrow::open_dataset(new_clean_dir) |>
    dplyr::filter(year %in% compare_years) |>
    dplyr::collect() |> data.table::as.data.table()
  new_clean[, station := .norm(station)]
  
  if (!is.null(legacy_names)) {
    new_raw   <- new_raw[station %in% legacy_names]
    new_clean <- new_clean[station %in% legacy_names]
  }
  
  # 3) Build step-level summary pulling the new Diagnostic Reason Codes
  step_list <- list()
  for (pol in pollutants) {
    flag_col   <- paste0(pol, "_outlier")
    reason_col <- paste0(pol, "_outlier_reason")
    if (!pol %in% names(new_raw) || !flag_col %in% names(new_clean)) next
    
    raw_vals     <- new_raw[[pol]]
    outlier_flag <- new_clean[[flag_col]]
    
    n_total     <- sum(!is.na(raw_vals))
    n_outlier   <- sum(outlier_flag == 1L, na.rm = TRUE)
    pct_removed <- round(100 * n_outlier / max(n_total, 1), 3)
    
    # Extract failure point breakdowns based on new detect_pollution_outliers logic
    if (reason_col %in% names(new_clean)) {
      reason_vals <- new_clean[[reason_col]]
      n_r1 <- sum(reason_vals == 1L, na.rm = TRUE)
      n_r2 <- sum(reason_vals == 2L, na.rm = TRUE)
      n_r3 <- sum(reason_vals == 3L, na.rm = TRUE)
    } else {
      n_r1 <- NA_integer_; n_r2 <- NA_integer_; n_r3 <- NA_integer_
    }
    
    step_list[[pol]] <- tibble::tibble(
      pollutant        = toupper(pol),
      total_obs        = n_total,
      outliers_removed = n_outlier,
      pct_removed      = pct_removed,
      reason_no_temp   = n_r1,
      reason_no_spat   = n_r2,
      reason_fail_both = n_r3
    )
  }
  step_summary <- dplyr::bind_rows(step_list)
  
  # 4) Load legacy cleaned data and compare
  comparison <- NULL
  if (file.exists(legacy_clean_path)) {
    ext <- tools::file_ext(legacy_clean_path)
    leg_clean <- if (ext == "rds") {
      data.table::as.data.table(readRDS(legacy_clean_path))
    } else data.table::fread(legacy_clean_path)
    
    names(leg_clean) <- tolower(names(leg_clean))
    sta_col <- intersect(c("station_code", "station"), names(leg_clean))[1]
    if (!is.na(sta_col)) leg_clean[, station := .norm(get(sta_col))]
    
    leg_raw_exists <- file.exists(legacy_raw_path)
    if (leg_raw_exists) {
      ext_raw <- tools::file_ext(legacy_raw_path)
      leg_raw_chk <- if (ext_raw == "rds") {
        data.table::as.data.table(readRDS(legacy_raw_path))
      } else data.table::fread(legacy_raw_path)
      
      names(leg_raw_chk) <- tolower(names(leg_raw_chk))
      sta_col_raw <- intersect(c("station_code", "station"), names(leg_raw_chk))[1]
      if (!is.na(sta_col_raw)) leg_raw_chk[, station := .norm(get(sta_col_raw))]
      if ("year" %in% names(leg_raw_chk)) leg_raw_chk <- leg_raw_chk[year %in% compare_years]
    }
    
    comp_list <- list()
    for (pol in pollutants) {
      flag_col <- paste0(pol, "_outlier")
      if (flag_col %in% names(new_clean)) {
        new_out <- new_clean[, .(
          outliers_new = sum(get(flag_col) == 1L, na.rm = TRUE),
          total_new    = sum(!is.na(new_raw[[pol]][
            match(paste(station, datetime), paste(new_raw$station, new_raw$datetime))
          ]))
        ), by = station]
      } else next
      
      leg_out <- tibble::tibble(station = character(), outliers_legacy = integer())
      if (pol %in% names(leg_clean) && leg_raw_exists && pol %in% names(leg_raw_chk)) {
        if ("will_be_na" %in% names(leg_clean)) {
          will_na_col <- if (pol == "pm25") "will_be_na_pm25" else "will_be_na"
          if (will_na_col %in% names(leg_clean)) {
            leg_out <- leg_clean[, .(
              outliers_legacy = sum(get(will_na_col) == 1L, na.rm = TRUE)
            ), by = station]
          }
        }
      }
      
      comp_list[[pol]] <- dplyr::full_join(
        dplyr::as_tibble(new_out), dplyr::as_tibble(leg_out), by = "station"
      ) |> dplyr::mutate(pollutant = toupper(pol))
    }
    comparison <- dplyr::bind_rows(comp_list)
  }
  
  # 5) Persist
  .wpq <- function(df, name) arrow::write_parquet(
    dplyr::as_tibble(df), file.path(out_dir, paste0(name, ".parquet")), compression = "zstd"
  )
  
  .wpq(step_summary, "step_summary")
  if (!is.null(comparison) && nrow(comparison) > 0) .wpq(comparison, "outlier_comparison")
  
  if (!quiet) message("[", cfg$id, "] Outlier comparison saved to: ", out_dir)
  
  invisible(list(
    step_summary = step_summary, comparison = comparison, out_dir = out_dir
  ))
}


# --------------------------------------------------------------------------------------------
# Function   : build_bogota_progression_specs
# @Arg cfg   : city cfg with $compare sublist (paths).
# @Output    : named list of 4 step specs. Each has:
#                id, label, mode ("precomputed" | "compute"), enabled (logical),
#                reason (NULL or skip explanation), and either `parquet_path`
#                (precomputed exposure parquet) or a full argument bundle for
#                `aggregate_idw_exposure()`.
# @Purpose   : Declaratively describe the 4-step methodological progression for
#              Bogotá so `compare_results_progression()` stays generic. Steps:
#                1 — Everything legacy (legacy census, legacy metro, legacy stations).
#                2 — New 2005 metro gpkg (40 GEO_IDs) + legacy census + legacy stations.
#                3 — Step 2 + new 2018 metro (manzana-level) + new 2018 census.
#                4 — Step 3 + new station set.
#              A step with any missing input is marked enabled=FALSE with a reason.
# @Written_on: 17/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
build_bogota_progression_specs <- function(cfg, buffer_km = 5L) {
  cmp <- cfg$compare %||% list()
  
  idw_root  <- here::here("data", "processed", "idw_estimates")
  dist_root <- here::here("data", "processed", "distances_matrices")
  
  # Step 4 is the canonical new-pipeline output; reuse a precomputed parquet
  # produced by aggregate_exposure_all_cities.R so we don't rerun DuckDB.
  step4_tag     <- paste0(buffer_km, "km")
  step4_parquet <- file.path(idw_root, "bogota_2018",
                             sprintf("bogota_2018_%s_idw_exposure.parquet", step4_tag))
  step4_indiv   <- file.path(idw_root, "bogota_2018",
                             sprintf("bogota_2018_%s_individual_quintiles.parquet", step4_tag))
  # For Step 4 day-level metrics we re-query the same inputs
  # aggregate_exposure_all_cities.R used: the cleaned Arrow panel and the
  # new-pipeline 2018 geo-station distance parquet.
  step4_arrow_clean <- here::here("data", "processed", "outlier_detection",
                                  "bogota_metro_clean")
  step4_arrow_raw   <- here::here("data", "raw", "monitoring_stations",
                                  "bogota_metro_dataset")
  step4_arrow_dir   <- if (dir.exists(step4_arrow_clean)) step4_arrow_clean
  else if (dir.exists(step4_arrow_raw)) step4_arrow_raw
  else NULL
  step4_geo_dist    <- file.path(dist_root, "bogota_2018_geo_station_distances.parquet")
  if (!file.exists(step4_geo_dist)) step4_geo_dist <- NULL
  
  # Steps 1-3 reuse the legacy Arrow panel (legacy raw CSV passed through the
  # new outlier-detection-on-legacy-input toggle produces a mock clean dir on
  # disk). If compare_outlier_procedure hasn't run, we fall back to the new
  # clean panel for steps 2-3 and skip step 1.
  legacy_mock_clean <- file.path(tempdir(), "legacy_arrow_test", "legacy_mock_clean")
  new_clean_dir     <- cmp$new_clean_dir
  
  # Legacy census (geo mode) — CSV produced by legacy Stata pipeline, one row per locality.
  legacy_census_csv <- cmp$legacy_census_collapsed
  has_legacy_census <- !is.null(legacy_census_csv) && file.exists(legacy_census_csv)
  
  # Legacy station × legacy metro distance (RDS long): dt_distances.rds
  legacy_geo_dist_rds <- cmp$legacy_geo_dist
  has_legacy_geo_dist <- !is.null(legacy_geo_dist_rds) && file.exists(legacy_geo_dist_rds)
  
  # New 2005 metro geometry (40 GEO_IDs). Parquet: bogota_2005_geo_station_distances.parquet.
  new_2005_geo_dist <- file.path(dist_root, "bogota_2005_geo_station_distances.parquet")
  has_new_2005_dist <- file.exists(new_2005_geo_dist)
  
  # Legacy stations × new 2018 manzana (44,979) — produced by
  # generate_distances_matrices.R (see the bogota_2018_legacy_stations block).
  legacy_sta_2018_dist <- file.path(dist_root,
                                    "bogota_2018_legacy_stations_geo_station_distances.parquet")
  has_legacy_sta_2018_dist <- file.exists(legacy_sta_2018_dist)
  
  # New 2018 census individual microdata (adult flag + years of schooling).
  new_census_indiv_csv <- here::here("data", "interim", "census", "bogota_2018",
                                     "census_2018_metro_individual.csv")
  has_new_census_indiv <- file.exists(new_census_indiv_csv)
  
  # ----------------------------------------------------------------------
  # Legacy geo-station distance RDS → parquet (tempdir cache). The RDS
  # from dt_distances.rds stores (station_code, locality, distance) plus
  # d_any_Xkm flags; aggregate_idw_exposure() only needs geo_id/station_id/
  # distance_km, so we transform and persist a minimal parquet.
  # ----------------------------------------------------------------------
  .legacy_dist_to_parquet <- function() {
    out_pq <- file.path(tempdir(), "legacy_geo_station_distances.parquet")
    if (file.exists(out_pq)) return(out_pq)
    if (!has_legacy_geo_dist) return(NULL)
    dd <- tryCatch(readRDS(legacy_geo_dist_rds), error = function(e) NULL)
    if (is.null(dd)) return(NULL)
    dd <- data.table::as.data.table(dd)
    geo_col  <- intersect(c("geo_id", "GEO_ID", "locality", "LocCodigo"), names(dd))[1]
    sta_col  <- intersect(c("station_id", "station_code", "station"), names(dd))[1]
    dist_col <- intersect(c("distance_km", "distance"), names(dd))[1]
    if (is.na(geo_col) || is.na(sta_col) || is.na(dist_col)) return(NULL)
    dd <- dd[, .(geo_id      = as.character(get(geo_col)),
                 station_id  = toupper(trimws(as.character(get(sta_col)))),
                 distance_km = as.numeric(get(dist_col)))]
    # Heuristic: legacy distances were in metres in some extracts.
    if (max(dd$distance_km, na.rm = TRUE) > 1000)
      dd[, distance_km := distance_km / 1000]
    arrow::write_parquet(dd, out_pq, compression = "zstd")
    out_pq
  }
  
  legacy_geo_dist_pq <- if (has_legacy_geo_dist) .legacy_dist_to_parquet() else NULL
  
  # ----------------------------------------------------------------------
  # Legacy census recoders.
  #   * Step 1 uses the legacy distance matrix whose geo_id is LocCodigo
  #     (01..20 for Bogotá localidades, 25xxx for surrounding municipalities).
  #   * Step 2 uses the new 2005 gpkg distance matrix whose geo_id is
  #     "11001<LocCodigo>" for Bogotá localidades, and 5-digit DANE code
  #     for the surrounding municipalities. So for Step 2 the census needs
  #     an augmented GEO_ID column.
  # ----------------------------------------------------------------------
  legacy_census_raw <- if (has_legacy_census)
    readr::read_csv(legacy_census_csv, show_col_types = FALSE,
                    col_types = readr::cols(LocCodigo = readr::col_character())) else NULL
  
  legacy_census_for_2005 <- if (!is.null(legacy_census_raw)) {
    tmp <- data.table::as.data.table(data.table::copy(legacy_census_raw))
    tmp[, GEO_ID := ifelse(nchar(LocCodigo) <= 2L,
                           paste0("11001", LocCodigo),
                           LocCodigo)]
    dplyr::as_tibble(tmp)
  } else NULL
  
  list(
    # Legacy collapsed census uses LocCodigo (01–20 + 25xxx) as geo id and
    # `escolaridad` as pre-averaged years of schooling; schema:
    # data/_legacy/census/collapse_bogota_metro.csv.
    "1_legacy" = list(
      id    = "1_legacy",
      label = "Step 1 — Legacy (census, metro, stations)",
      mode  = "compute",
      enabled = has_legacy_census && !is.null(legacy_geo_dist_pq) &&
        dir.exists(legacy_mock_clean),
      reason  = if (!has_legacy_census)                    "missing legacy census CSV"
      else if (is.null(legacy_geo_dist_pq))      "missing legacy geo-station distances"
      else if (!dir.exists(legacy_mock_clean))   "run compare_outlier_procedure() first (creates legacy mock Arrow)"
      else NULL,
      args    = list(
        arrow_dir      = legacy_mock_clean,
        geo_sta_pq     = legacy_geo_dist_pq %||% "",
        census_col     = legacy_census_raw,
        geo_id_col     = "LocCodigo",
        pop_col        = "n",
        edu_col        = "escolaridad",
        quintile_level = "geo",
        buffer_km      = buffer_km
      )
    ),
    "2_new_metro_2005" = list(
      id    = "2_new_metro_2005",
      label = "Step 2 — New 2005 metro gpkg (40 GEO_IDs) + legacy census + legacy stations",
      mode  = "compute",
      enabled = has_legacy_census && has_new_2005_dist && dir.exists(legacy_mock_clean),
      reason  = if (!has_legacy_census)              "missing legacy census CSV"
      else if (!has_new_2005_dist)         "missing bogota_2005_geo_station_distances.parquet (run generate_distances_matrices.R)"
      else if (!dir.exists(legacy_mock_clean)) "run compare_outlier_procedure() first"
      else NULL,
      args    = list(
        arrow_dir      = legacy_mock_clean,
        geo_sta_pq     = new_2005_geo_dist,
        census_col     = legacy_census_for_2005,  # LocCodigo → 11001XX
        geo_id_col     = "GEO_ID",
        pop_col        = "n",
        edu_col        = "escolaridad",
        quintile_level = "geo",
        buffer_km      = buffer_km
      )
    ),
    "3_new_metro_census_2018" = list(
      id    = "3_new_metro_census_2018",
      label = "Step 3 — 2018 manzana metro + 2018 census + legacy stations",
      mode  = "compute",
      enabled = has_new_census_indiv && has_legacy_sta_2018_dist &&
        dir.exists(legacy_mock_clean),
      reason  = if (!has_new_census_indiv)             "missing 2018 manzana-level census CSV"
      else if (!has_legacy_sta_2018_dist)    "missing bogota_2018_legacy_stations_geo_station_distances.parquet (run generate_distances_matrices.R)"
      else if (!dir.exists(legacy_mock_clean)) "run compare_outlier_procedure() first"
      else NULL,
      args    = list(
        arrow_dir      = legacy_mock_clean,
        geo_sta_pq     = legacy_sta_2018_dist,
        census_col     = if (has_new_census_indiv)
          vroom::vroom(new_census_indiv_csv, col_types = "cnnnnccc") else NULL,
        geo_id_col     = "GEO_ID",
        pop_col        = "fe",
        edu_col        = "escolaridad",
        quintile_level = "individual",
        buffer_km      = buffer_km
      )
    ),
    "4_new_stations" = list(
      id    = "4_new_stations",
      label = "Step 4 — Everything new (2018 census + 2018 metro + new stations)",
      mode  = "precomputed",
      enabled = file.exists(step4_parquet),
      reason  = if (!file.exists(step4_parquet))
        sprintf("missing %s (run aggregate_exposure_all_cities.R for buffer=%dkm)",
                basename(step4_parquet), buffer_km) else NULL,
      parquet_path            = step4_parquet,
      individual_parquet_path = if (file.exists(step4_indiv)) step4_indiv else NULL,
      quintile_level          = "individual",
      # For day-level metrics: rerun the IDW query against the cleaned Arrow
      # dataset + new 2018 geo-station distance matrix (same inputs as the
      # canonical aggregate_exposure_all_cities.R call).
      arrow_dir_for_days      = step4_arrow_dir,
      geo_sta_pq_for_days     = step4_geo_dist
    )
  )
}


# --------------------------------------------------------------------------------------------
# Function   : compare_results_progression
# @Arg cfg        : city cfg with $id and $compare sublist.
# @Arg out_root   : results root; {out_root}/results_progression/ is created.
# @Arg step_specs : list of step specs (see build_bogota_progression_specs).
#                   If NULL, auto-built for Bogotá via build_bogota_progression_specs(cfg).
# @Arg year       : integer; focus year for the summary (default 2023).
# @Arg pollutants : character vector; pollutants to summarise. Default c("pm25","pm10").
# @Arg buffer_km  : integer; buffer (km) applied to geo-station pairs inside
#                   aggregate_idw_exposure(). Must match the precomputed Step 4 parquet.
# @Arg quiet      : logical; suppress messages. Default FALSE.
#
# @Output : invisible list with
#            $progression_long — (step_id, step_label, pollutant, edu_quintile,
#                                 n_geo, weighted_mean_conc, share_hrs_above_it1,
#                                 share_hrs_above_it4, days_with_hr_above_it1,
#                                 days_with_hr_above_it2, days_with_2hr_above_it1)
#            $progression_gap  — one row per (step, pollutant) with q1, q5, gap, ratio
#            $step_status      — (step_id, label, status, reason) audit log
#            $out_dir          — path to results_progression directory
#           Writes to results_progression/:
#             progression_long.parquet, progression_gap.parquet, step_status.parquet,
#             <step_id>_exposure.parquet (per-step exposure dump)
#
# @Purpose : Run `aggregate_idw_exposure()` across a 4-step methodological progression
#            (legacy → new metro → new census → new stations) and measure how the
#            headline results — population-weighted mean PM concentration, share of
#            hours above WHO interim targets, and "days with ≥1 / ≥2 hours above IT"
#            — shift at each step, stratified by education quintile (Q1..Q5).
#
# @Details :
#   For each enabled step:
#     - mode = "precomputed": read parquet at parquet_path; if individual mode
#       and individual_parquet_path is supplied, derive a modal geo-quintile.
#     - mode = "compute": call aggregate_idw_exposure() with args; the per-step
#       output is cached under <out_dir>/<step_id>/.
#
#   Per-day metrics: the aggregate_idw_exposure output only carries hour-level
#   IT-threshold counts. To recover day-level counts (e.g. "days with ≥1 hour
#   above IT1"), this function re-queries the per-step clean Arrow dataset with
#   the same IDW formula — only if `step_days` is computable (step provides a
#   valid geo_sta_pq + arrow_dir).
#
# @Written_on: 17/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_results_progression <- function(
    cfg,
    out_root,
    step_specs = NULL,
    year       = 2023L,
    pollutants = c("pm25", "pm10"),
    buffer_km  = 5L,
    quiet      = FALSE
) {
  req_pkgs <- c("dplyr", "tibble", "arrow", "data.table", "tidyr")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  pollutants <- tolower(pollutants)
  stopifnot(all(pollutants %in% c("pm25", "pm10")))
  
  out_dir <- file.path(out_root, "results_progression")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(step_specs)) {
    if (cfg$id != "bogota")
      stop("Auto-built step specs only available for Bogotá; pass step_specs explicitly.")
    step_specs <- build_bogota_progression_specs(cfg, buffer_km = buffer_km)
  }
  
  status_rows       <- list()
  per_step_quintile <- list()
  .yr               <- as.integer(year)
  
  # WHO interim-target thresholds (µg/m³). Mirrors aggregate_idw_exposure()'s defaults.
  who_it <- list(
    pm10 = c(it1 = 150, it2 = 100, it3 = 75,  it4 = 50),
    pm25 = c(it1 = 75,  it2 = 50,  it3 = 37.5, it4 = 25)
  )
  
  # Helper: compute day-level IT-exceedance counts for one (step, pollutant) by
  # re-running the IDW query hour-by-hour, then collapsing to daily max. Returns
  # a data.table keyed by geo_id with days_with_hr_above_{it1,it2} columns.
  .compute_day_metrics <- function(arrow_dir, geo_sta_pq, pollutant, year,
                                   buffer_km) {
    if (is.null(arrow_dir) || is.null(geo_sta_pq) ||
        !nzchar(arrow_dir)  || !nzchar(geo_sta_pq)) return(NULL)
    if (!requireNamespace("duckdb", quietly = TRUE) ||
        !dir.exists(arrow_dir) || !file.exists(geo_sta_pq)) return(NULL)
    thr1 <- who_it[[pollutant]][["it1"]]
    thr2 <- who_it[[pollutant]][["it2"]]
    con <- tryCatch(
      DBI::dbConnect(duckdb::duckdb(dbdir = tempfile("prog_days_", fileext = ".db"))),
      error = function(e) NULL)
    if (is.null(con)) return(NULL)
    on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
    poll_glob <- paste0(gsub("\\\\", "/", arrow_dir), "/**/*.parquet")
    q <- sprintf("
      WITH dist AS (
        SELECT geo_id, station_id, 1.0 / distance_km AS inv_d
        FROM read_parquet('%s')
        WHERE distance_km > 0 AND distance_km <= %d
      ),
      poll AS (
        SELECT station, datetime, %s AS val,
               CAST(strftime(datetime, '%%Y-%%m-%%d') AS VARCHAR) AS dt_date
        FROM read_parquet('%s', hive_partitioning = true)
        WHERE year = %d AND %s IS NOT NULL
      ),
      hr_geo AS (
        SELECT d.geo_id, p.dt_date, p.datetime,
               SUM(p.val * d.inv_d) / SUM(d.inv_d) AS idw
        FROM poll p JOIN dist d ON p.station = d.station_id
        GROUP BY d.geo_id, p.dt_date, p.datetime
      ),
      day_geo AS (
        SELECT geo_id, dt_date,
               SUM(CASE WHEN idw >= %s THEN 1 ELSE 0 END) AS n_hr_it1,
               SUM(CASE WHEN idw >= %s THEN 1 ELSE 0 END) AS n_hr_it2
        FROM hr_geo
        GROUP BY geo_id, dt_date
      )
      SELECT geo_id,
             SUM(CASE WHEN n_hr_it1 >= 1 THEN 1 ELSE 0 END)
               AS days_with_hr_above_it1,
             SUM(CASE WHEN n_hr_it1 >= 2 THEN 1 ELSE 0 END)
               AS days_with_2hr_above_it1,
             SUM(CASE WHEN n_hr_it2 >= 1 THEN 1 ELSE 0 END)
               AS days_with_hr_above_it2
      FROM day_geo
      GROUP BY geo_id;",
                 gsub("'", "''", geo_sta_pq),
                 as.integer(buffer_km),
                 pollutant,
                 gsub("'", "''", poll_glob),
                 as.integer(year),
                 pollutant,
                 format(thr1, nsmall = 1),
                 format(thr2, nsmall = 1))
    res <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) NULL)
    if (is.null(res) || !nrow(res)) return(NULL)
    data.table::as.data.table(res)[, geo_id := as.character(geo_id)]
  }
  
  for (sp in step_specs) {
    if (!isTRUE(sp$enabled)) {
      status_rows[[sp$id]] <- tibble::tibble(
        step_id = sp$id, label = sp$label, status = "skipped",
        reason  = sp$reason %||% "disabled"
      )
      if (!quiet) message(sprintf("[progression] Skip %s — %s", sp$id, sp$reason %||% "disabled"))
      next
    }
    
    if (!quiet) message(sprintf("[progression] %s ...", sp$label))
    
    exp_dt <- NULL; indiv_dt <- NULL
    arrow_dir_for_days <- NULL; geo_sta_pq_for_days <- NULL
    step_buffer <- buffer_km
    
    if (identical(sp$mode, "precomputed")) {
      exp_dt <- data.table::as.data.table(arrow::read_parquet(sp$parquet_path))
      if (!is.null(sp$individual_parquet_path))
        indiv_dt <- data.table::as.data.table(arrow::read_parquet(sp$individual_parquet_path))
      # For Step 4 we need arrow_dir + geo_sta_pq to compute day metrics; these
      # are carried on the spec for reuse.
      arrow_dir_for_days  <- sp$arrow_dir_for_days  %||% NULL
      geo_sta_pq_for_days <- sp$geo_sta_pq_for_days %||% NULL
    } else {
      step_out_dir <- file.path(out_dir, sp$id)
      args <- sp$args
      args$out_dir   <- step_out_dir
      args$out_name  <- sp$id
      args$quiet     <- quiet
      args$overwrite <- FALSE
      step_buffer <- args$buffer_km %||% buffer_km
      args$buffer_km <- step_buffer
      res <- tryCatch(do.call(aggregate_idw_exposure, args),
                      error = function(e) { warning(e$message); NULL })
      if (is.null(res)) {
        status_rows[[sp$id]] <- tibble::tibble(
          step_id = sp$id, label = sp$label, status = "error",
          reason = "aggregate_idw_exposure failed — see warning"
        )
        next
      }
      exp_dt   <- data.table::as.data.table(res$exposure_yearly)
      indiv_dt <- if (!is.null(res$individual_quintiles))
        data.table::as.data.table(res$individual_quintiles) else NULL
      arrow_dir_for_days  <- args$arrow_dir
      geo_sta_pq_for_days <- args$geo_sta_pq
    }
    
    # Persist the per-step exposure for downstream reports
    arrow::write_parquet(
      dplyr::as_tibble(exp_dt),
      file.path(out_dir, paste0(sp$id, "_exposure.parquet")),
      compression = "zstd"
    )
    
    if ("year" %in% names(exp_dt)) exp_dt <- exp_dt[year == .yr]
    if (nrow(exp_dt) == 0L) {
      status_rows[[sp$id]] <- tibble::tibble(
        step_id = sp$id, label = sp$label, status = "empty",
        reason = sprintf("no rows for year=%d", .yr)
      )
      next
    }
    
    exp_dt[, geo_id := as.character(geo_id)]
    
    if (!"edu_quintile" %in% names(exp_dt)) {
      # Individual mode: derive a geo-level quintile from individual_quintiles.
      if (is.null(indiv_dt) || !"edu_quintile" %in% names(indiv_dt)) {
        status_rows[[sp$id]] <- tibble::tibble(
          step_id = sp$id, label = sp$label, status = "error",
          reason = "edu_quintile missing in both exposure and individual outputs"
        )
        next
      }
      indiv_dt[, geo_id := as.character(geo_id)]
      weight_col <- intersect(c("fe", "FACTOR", "weight", "n"), names(indiv_dt))[1]
      if (is.na(weight_col)) {
        indiv_dt[, fe := 1L]
        weight_col <- "fe"
      }
      # Modal quintile per geo_id weighted by the expansion factor.
      q_geo <- indiv_dt[!is.na(edu_quintile),
                        .(w = sum(get(weight_col), na.rm = TRUE)),
                        by = .(geo_id, edu_quintile)]
      data.table::setorder(q_geo, geo_id, -w)
      q_geo <- q_geo[, .SD[1], by = geo_id][, .(geo_id, edu_quintile)]
      pop_geo <- indiv_dt[, .(pop_w = sum(get(weight_col), na.rm = TRUE)), by = geo_id]
      exp_dt <- merge(exp_dt, q_geo,   by = "geo_id", all.x = TRUE)
      exp_dt <- merge(exp_dt, pop_geo, by = "geo_id", all.x = TRUE)
    } else {
      pop_candidates <- intersect(c("n", "pop", "fe", "FACTOR", "weight"), names(exp_dt))
      if (length(pop_candidates) && !"pop_w" %in% names(exp_dt))
        exp_dt[, pop_w := as.numeric(get(pop_candidates[1]))]
      if (!"pop_w" %in% names(exp_dt)) exp_dt[, pop_w := 1]
    }
    
    # --------------------------------------------------------------------
    # Per-pollutant quintile summary. Fall through pollutants that are not
    # present in the exposure output (e.g. some legacy years lack PM25).
    # --------------------------------------------------------------------
    poll_tabs <- list()
    for (pol in pollutants) {
      conc_col  <- paste0("avg_",       pol)
      total_col <- paste0("total_hrs_", pol)
      it1_col   <- paste0("hrs_d_",     pol, "_it1")
      it4_col   <- paste0("hrs_d_",     pol, "_it4")
      if (!conc_col %in% names(exp_dt)) next
      
      # Day-level metrics (optional; skipped if Arrow/distance unavailable).
      days_dt <- .compute_day_metrics(
        arrow_dir  = arrow_dir_for_days,
        geo_sta_pq = geo_sta_pq_for_days,
        pollutant  = pol,
        year       = .yr,
        buffer_km  = step_buffer
      )
      
      one <- exp_dt[!is.na(get(conc_col)),
                    .(geo_id, edu_quintile, pop_w,
                      conc_col  = get(conc_col),
                      hrs_it1   = if (it1_col %in% names(exp_dt))
                        get(it1_col) else NA_real_,
                      hrs_it4   = if (it4_col %in% names(exp_dt))
                        get(it4_col) else NA_real_,
                      total_hrs = if (total_col %in% names(exp_dt))
                        get(total_col) else NA_real_)]
      if (!is.null(days_dt))
        one <- merge(one, days_dt, by = "geo_id", all.x = TRUE)
      
      qsum <- one[!is.na(edu_quintile),
                  .(n_geo              = .N,
                    weighted_mean_conc = stats::weighted.mean(conc_col, pop_w, na.rm = TRUE),
                    share_hrs_above_it1 = sum(hrs_it1, na.rm = TRUE) /
                      max(sum(total_hrs, na.rm = TRUE), 1),
                    share_hrs_above_it4 = sum(hrs_it4, na.rm = TRUE) /
                      max(sum(total_hrs, na.rm = TRUE), 1),
                    days_with_hr_above_it1 = if ("days_with_hr_above_it1" %in% names(one))
                      stats::weighted.mean(days_with_hr_above_it1, pop_w,
                                           na.rm = TRUE) else NA_real_,
                    days_with_2hr_above_it1 = if ("days_with_2hr_above_it1" %in% names(one))
                      stats::weighted.mean(days_with_2hr_above_it1, pop_w,
                                           na.rm = TRUE) else NA_real_,
                    days_with_hr_above_it2 = if ("days_with_hr_above_it2" %in% names(one))
                      stats::weighted.mean(days_with_hr_above_it2, pop_w,
                                           na.rm = TRUE) else NA_real_),
                  by = edu_quintile][order(edu_quintile)]
      qsum[, `:=`(step_id = sp$id, step_label = sp$label, pollutant = pol)]
      poll_tabs[[pol]] <- qsum
    }
    if (!length(poll_tabs)) {
      status_rows[[sp$id]] <- tibble::tibble(
        step_id = sp$id, label = sp$label, status = "error",
        reason  = "no avg_pm* columns in exposure output"
      )
      next
    }
    per_step_quintile[[sp$id]] <- data.table::rbindlist(poll_tabs, fill = TRUE)
    
    status_rows[[sp$id]] <- tibble::tibble(
      step_id = sp$id, label = sp$label, status = "ok", reason = NA_character_
    )
  }
  
  progression_long <- data.table::rbindlist(per_step_quintile, fill = TRUE)
  progression_long <- dplyr::as_tibble(progression_long) |>
    dplyr::select(step_id, step_label, pollutant, edu_quintile,
                  n_geo, weighted_mean_conc,
                  share_hrs_above_it1, share_hrs_above_it4,
                  dplyr::any_of(c("days_with_hr_above_it1",
                                  "days_with_2hr_above_it1",
                                  "days_with_hr_above_it2")))
  
  # Headline gap Q5-Q1 and ratio Q5/Q1 per (step, pollutant).
  progression_gap <- progression_long |>
    dplyr::filter(edu_quintile %in% c(1L, 5L)) |>
    dplyr::select(step_id, step_label, pollutant, edu_quintile, weighted_mean_conc) |>
    tidyr::pivot_wider(names_from = edu_quintile,
                       names_prefix = "q",
                       values_from = weighted_mean_conc) |>
    dplyr::mutate(
      gap_q5_minus_q1  = q5 - q1,
      ratio_q5_over_q1 = dplyr::if_else(!is.na(q1) & q1 > 0, q5 / q1, NA_real_)
    )
  
  step_status <- dplyr::bind_rows(status_rows)
  
  arrow::write_parquet(progression_long,
                       file.path(out_dir, "progression_long.parquet"),
                       compression = "zstd")
  arrow::write_parquet(progression_gap,
                       file.path(out_dir, "progression_gap.parquet"),
                       compression = "zstd")
  arrow::write_parquet(step_status,
                       file.path(out_dir, "step_status.parquet"),
                       compression = "zstd")
  
  if (!quiet) {
    message("[", cfg$id, "] Progression comparison saved to: ", out_dir)
    n_ok <- sum(step_status$status == "ok")
    message(sprintf("  %d of %d steps completed (%s)",
                    n_ok, nrow(step_status),
                    paste(step_status$step_id[step_status$status == "ok"], collapse = ", ")))
  }
  
  invisible(list(
    progression_long = progression_long,
    progression_gap  = progression_gap,
    step_status      = step_status,
    out_dir          = out_dir
  ))
}


# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")