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
  if (isTRUE(panelize) && nrow(df) > 0L) {
    stations  <- sort(unique(df$station))
    y0 <- min(df$year, na.rm = TRUE)
    y1 <- max(df$year, na.rm = TRUE)
    start_dt  <- as.POSIXct(
      sprintf("%d-01-01 00:00:00", y0), tz = tz
    )
    end_dt    <- as.POSIXct(
      sprintf("%d-12-31 23:00:00", y1), tz = tz
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
  out_dir <- file.path(out_root, cfg$id)
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
    new_metro_gpkg    = NULL,
    new_stations_gpkg = NULL,
    legacy_shp_dir    = NULL,
    station_audit     = NULL,
    buffer_km         = 20,
    quiet             = FALSE
) {
  
  # 0) Dependencies
  # ---------------------------------------------------------------------------
  req_pkgs <- c("sf", "dplyr", "tibble", "arrow")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' required.")
  }
  
  # 1) Resolve file paths from cfg defaults if not supplied
  # ---------------------------------------------------------------------------
  dir_geo <- here::here("data", "raw", "geospatial_data")
  
  if (is.null(new_metro_gpkg))
    new_metro_gpkg <- here::here(
      dir_geo, cfg$id,
      paste0(cfg$id, "_area_metro_2018.gpkg")
    )
  if (is.null(new_stations_gpkg))
    new_stations_gpkg <- here::here(
      dir_geo, cfg$id,
      paste0(cfg$id, "_2018_stations_buffer_metro.gpkg")
    )
  if (is.null(legacy_shp_dir))
    legacy_shp_dir <- here::here(
      "data", "_legacy", "cities_shapefiles",
      tools::toTitleCase(cfg$id)
    )
  
  # Validate inputs
  if (!file.exists(new_metro_gpkg))
    stop("[", cfg$id, "] New metro GeoPackage not found:\n  ", new_metro_gpkg)
  if (!file.exists(new_stations_gpkg))
    stop("[", cfg$id, "] New stations GeoPackage not found:\n  ", new_stations_gpkg)
  if (!dir.exists(legacy_shp_dir))
    stop("[", cfg$id, "] Legacy shapefile dir not found:\n  ", legacy_shp_dir)
  
  out_dir <- file.path(out_root, cfg$id, "metro_area_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet)
    message("[", cfg$id, "] Comparing metro area definitions ...")
  
  # 2) Load the new pipeline metro area and stations
  # ---------------------------------------------------------------------------
  new_metro_sf <- sf::st_read(new_metro_gpkg, quiet = TRUE) |>
    sf::st_transform(4326)
  
  new_stations_sf <- sf::st_read(new_stations_gpkg, quiet = TRUE) |>
    sf::st_transform(4326)
  
  # 3) Load the legacy metro area shapefile
  # ---------------------------------------------------------------------------
  shp_file <- list.files(
    legacy_shp_dir, pattern = "\\.shp$",
    full.names = TRUE, ignore.case = TRUE
  )
  if (length(shp_file) == 0)
    stop("[", cfg$id, "] No .shp file found in: ", legacy_shp_dir)
  
  legacy_metro_sf <- sf::st_read(shp_file[1], quiet = TRUE) |>
    sf::st_transform(4326)
  
  if (!quiet)
    message(
      "  New metro: ", nrow(new_metro_sf), " features | ",
      "Legacy metro: ", nrow(legacy_metro_sf), " features"
    )
  
  # 4) Identify legacy vs new stations
  # ---------------------------------------------------------------------------
  # Standardise station names in the new sf for matching
  if ("station_name" %in% names(new_stations_sf)) {
    new_stations_sf$station_std <- .std_name(new_stations_sf$station_name)
  }
  
  # Determine legacy station subset
  if (!is.null(station_audit)) {
    # Use the audit table from compare_ground_stations()
    legacy_names <- station_audit$station[station_audit$in_legacy]
    legacy_stations_sf <- new_stations_sf |>
      dplyr::filter(
        .data$station_std %in% legacy_names |
          grepl("RMCAB", .data$source, ignore.case = TRUE)
      )
    # Deduplicate: keep only those that match legacy names if possible
    if (nrow(legacy_stations_sf) > length(legacy_names)) {
      legacy_stations_sf <- new_stations_sf |>
        dplyr::filter(.data$station_std %in% legacy_names)
    }
  } else {
    # Fallback: filter by source column containing "RMCAB"
    if ("source" %in% names(new_stations_sf)) {
      legacy_stations_sf <- new_stations_sf |>
        dplyr::filter(grepl("RMCAB", .data$source, ignore.case = TRUE))
    } else {
      warning(
        "[", cfg$id, "] No station_audit and no 'source' column. ",
        "Using all stations as legacy."
      )
      legacy_stations_sf <- new_stations_sf
    }
  }
  
  if (!quiet)
    message(
      "  Stations — new pipeline: ", nrow(new_stations_sf),
      " | legacy universe: ", nrow(legacy_stations_sf)
    )
  
  # 5) Compute area statistics in metric CRS
  # ---------------------------------------------------------------------------
  # Determine a local UTM zone from the metro bbox centroid
  bb <- sf::st_bbox(new_metro_sf)
  lon_center <- (bb["xmin"] + bb["xmax"]) / 2
  lat_center <- (bb["ymin"] + bb["ymax"]) / 2
  utm_zone <- floor((lon_center + 180) / 6) + 1
  epsg_utm <- if (lat_center >= 0) 32600 + utm_zone else 32700 + utm_zone
  
  new_metro_m    <- sf::st_transform(new_metro_sf, epsg_utm)
  legacy_metro_m <- sf::st_transform(legacy_metro_sf, epsg_utm)
  
  new_area_km2    <- as.numeric(sum(sf::st_area(new_metro_m))) / 1e6
  legacy_area_km2 <- as.numeric(sum(sf::st_area(legacy_metro_m))) / 1e6
  
  # Count municipalities (features in the metro sf)
  new_n_muni    <- nrow(new_metro_sf)
  legacy_n_muni <- nrow(legacy_metro_sf)
  
  # Count stations inside each metro polygon
  new_stations_m    <- sf::st_transform(new_stations_sf, epsg_utm)
  legacy_stations_m <- sf::st_transform(legacy_stations_sf, epsg_utm)
  
  n_new_inside_new <- sum(sf::st_within(
    new_stations_m, sf::st_union(new_metro_m), sparse = FALSE
  )[, 1])
  
  n_legacy_inside_legacy <- sum(sf::st_within(
    legacy_stations_m, sf::st_union(legacy_metro_m), sparse = FALSE
  )[, 1])
  
  n_new_outside_new <- nrow(new_stations_sf) - n_new_inside_new
  n_legacy_outside_legacy <- nrow(legacy_stations_sf) - n_legacy_inside_legacy
  
  # Build summary table
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
      legacy_n_muni,
      nrow(legacy_stations_sf),
      n_legacy_inside_legacy,
      n_legacy_outside_legacy
    ),
    `New pipeline` = c(
      round(new_area_km2, 1),
      new_n_muni,
      nrow(new_stations_sf),
      n_new_inside_new,
      n_new_outside_new
    )
  )
  
  # 6) Persist artefacts
  # ---------------------------------------------------------------------------
  # Summary as Parquet
  arrow::write_parquet(
    summary_tbl,
    file.path(out_dir, "metro_summary.parquet"),
    compression = "zstd"
  )
  
  # Save sf objects as GeoPackage for the Quarto report maps
  sf::st_write(
    new_metro_sf,
    file.path(out_dir, "new_metro.gpkg"),
    delete_dsn = TRUE, quiet = TRUE
  )
  sf::st_write(
    legacy_metro_sf,
    file.path(out_dir, "legacy_metro.gpkg"),
    delete_dsn = TRUE, quiet = TRUE
  )
  sf::st_write(
    new_stations_sf,
    file.path(out_dir, "new_stations.gpkg"),
    delete_dsn = TRUE, quiet = TRUE
  )
  sf::st_write(
    legacy_stations_sf,
    file.path(out_dir, "legacy_stations.gpkg"),
    delete_dsn = TRUE, quiet = TRUE
  )
  
  if (!quiet)
    message("[", cfg$id, "] Metro comparison saved to: ", out_dir)
  
  # 7) Return
  # ---------------------------------------------------------------------------
  invisible(list(
    summary           = summary_tbl,
    new_metro_sf      = new_metro_sf,
    legacy_metro_sf   = legacy_metro_sf,
    new_stations_sf   = new_stations_sf,
    legacy_stations_sf = legacy_stations_sf,
    out_dir           = out_dir
  ))
}


# --------------------------------------------------------------------------------------------
# compare_census
# @Arg      : cfg              — city cfg list (must contain $id).
# @Arg      : out_root         — root output folder; {out_root}/{cfg$id}/ is created.
# @Arg      : new_collapsed    — character; path to the new pipeline's collapsed census CSV.
#                                 Default: data/interim/census/bogota_extended_2005/
#                                          collapse_metro_area_extended.csv
# @Arg      : new_individual   — character; path to the new pipeline's individual census CSV.
#                                 Default: data/interim/census/bogota_extended_2005/
#                                          census_metro_individual_extended.csv
# @Arg      : legacy_collapsed — character; path to the Dropbox legacy collapsed CSV.
#                                 Default: data/_legacy/census/collapse_bogota_metro.csv
# @Arg      : legacy_individual — character; path to the Dropbox legacy individual census CSV.
#                                  Default: data/_legacy/census/census_bogota_metro.csv
#                                  Set to NULL to skip individual-level comparison.
# @Arg      : join_key         — character; column name used to join collapsed datasets.
#                                 Default "LocCodigo" (the geographic unit identifier shared
#                                 by both pipelines for the Extended 2005 census).
# @Arg      : compare_vars     — character vector; variable names to compare in collapsed
#                                 data. Default covers the core education/labor shares.
# @Arg      : tol              — numeric; absolute tolerance for value comparisons.
#                                 Default 0.001 (0.1 percentage point for shares).
# @Arg      : quiet            — logical; suppress messages. Default FALSE.
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
    new_collapsed    = NULL,
    new_individual   = NULL,
    legacy_collapsed = NULL,
    legacy_individual = NULL,
    join_key         = "LocCodigo",
    compare_vars     = c(
      "n", "escolaridad",
      "share_no_education_pop",
      "share_high_school_incomplete_pop",
      "share_high_school_complete_pop",
      "share_college_incomplete_pop",
      "share_college_complete_pop",
      "share_graduate_educ_pop",
      "share_employed_pop"
    ),
    tol   = 0.001,
    quiet = FALSE
) {
  
  # 0) Dependencies
  # ---------------------------------------------------------------------------
  req_pkgs <- c("dplyr", "tidyr", "tibble", "readr", "arrow")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' required.")
  }
  
  # 1) Resolve default file paths
  # ---------------------------------------------------------------------------
  if (is.null(new_collapsed))
    new_collapsed <- here::here(
      "data", "interim", "census",
      paste0(cfg$id, "_extended_2005"),
      "collapse_metro_area_extended.csv"
    )
  if (is.null(new_individual))
    new_individual <- here::here(
      "data", "interim", "census",
      paste0(cfg$id, "_extended_2005"),
      "census_metro_individual_extended.csv"
    )
  if (is.null(legacy_collapsed))
    legacy_collapsed <- here::here(
      "data", "_legacy", "census",
      paste0("collapse_", cfg$id, "_metro.csv")
    )
  if (is.null(legacy_individual))
    legacy_individual <- here::here(
      "data", "_legacy", "census",
      paste0("census_", cfg$id, "_metro.csv")
    )
  
  # Validate that at least the collapsed files exist
  
  if (!file.exists(new_collapsed))
    stop("[", cfg$id, "] New collapsed census not found:\n  ", new_collapsed)
  if (!file.exists(legacy_collapsed))
    stop("[", cfg$id, "] Legacy collapsed census not found:\n  ", legacy_collapsed)
  
  out_dir <- file.path(out_root, cfg$id, "census_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet)
    message("[", cfg$id, "] Comparing census data (Extended 2005) ...")
  
  # 2) Load collapsed datasets
  # ---------------------------------------------------------------------------
  new_col <- readr::read_csv(new_collapsed, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))
  
  legacy_col <- readr::read_csv(legacy_collapsed, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws)) |> 
    dplyr::mutate(GEO_ID = LocCodigo)
  
  # Ensure the join key exists in both
  if (!join_key %in% names(new_col))
    stop("[", cfg$id, "] join_key '", join_key, "' not in new collapsed data. ",
         "Available: ", paste(names(new_col), collapse = ", "))
  if (!join_key %in% names(legacy_col))
    stop("[", cfg$id, "] join_key '", join_key, "' not in legacy collapsed data. ",
         "Available: ", paste(names(legacy_col), collapse = ", "))
  
  # Harmonise the join key: the new pipeline may store GEO_ID instead of
  # LocCodigo. For the extended census, GEO_ID == LocCodigo + optional
  # localidad suffix. If GEO_ID is used, we extract the first 5 chars for
  # Cundinamarca and use as-is for Bogotá localidades (2-digit codes).
  if (join_key == "LocCodigo" && "GEO_ID" %in% names(new_col) &&
      !join_key %in% names(new_col)) {
    new_col <- new_col |>
      dplyr::rename(LocCodigo = GEO_ID)
  }
  
  # Standardise key to character
  new_col[[join_key]]    <- as.character(new_col[[join_key]])
  legacy_col[[join_key]] <- as.character(legacy_col[[join_key]])
  
  if (!quiet)
    message(
      "  Geo units — new: ", dplyr::n_distinct(new_col[[join_key]]),
      " | legacy: ", dplyr::n_distinct(legacy_col[[join_key]])
    )
  
  # 3) Geographic coverage audit
  # ---------------------------------------------------------------------------
  new_geos    <- sort(unique(new_col[[join_key]]))
  legacy_geos <- sort(unique(legacy_col[[join_key]]))
  all_geos    <- sort(union(new_geos, legacy_geos))
  
  geo_coverage <- tibble::tibble(
    geo_unit   = all_geos,
    in_new     = all_geos %in% new_geos,
    in_legacy  = all_geos %in% legacy_geos,
    overlap    = all_geos %in% intersect(new_geos, legacy_geos)
  )
  
  n_overlap   <- sum(geo_coverage$overlap)
  n_new_only  <- sum(geo_coverage$in_new & !geo_coverage$in_legacy)
  n_leg_only  <- sum(geo_coverage$in_legacy & !geo_coverage$in_new)
  
  if (!quiet)
    message(sprintf(
      "  Overlap: %d | New only: %d | Legacy only: %d",
      n_overlap, n_new_only, n_leg_only
    ))
  
  # 4) Compare collapsed values on overlapping geographic units
  # ---------------------------------------------------------------------------
  overlap_geos <- geo_coverage$geo_unit[geo_coverage$overlap]
  
  # Restrict compare_vars to columns present in both datasets
  avail_vars <- intersect(
    intersect(compare_vars, names(new_col)),
    names(legacy_col)
  )
  missing_vars <- setdiff(compare_vars, avail_vars)
  if (length(missing_vars) > 0 && !quiet)
    message(
      "  Variables not in both datasets (skipped): ",
      paste(missing_vars, collapse = ", ")
    )
  
  new_overlap <- new_col |>
    dplyr::filter(.data[[join_key]] %in% overlap_geos) |>
    dplyr::select(dplyr::all_of(c(join_key, avail_vars)))
  
  legacy_overlap <- legacy_col |>
    dplyr::filter(.data[[join_key]] %in% overlap_geos) |>
    dplyr::select(dplyr::all_of(c(join_key, avail_vars)))
  
  # Full join and compute differences
  joined <- dplyr::full_join(
    new_overlap,
    legacy_overlap,
    by = join_key,
    suffix = c("_new", "_legacy")
  )
  
  # Build long-format differences
  diffs_list <- lapply(avail_vars, function(v) {
    v_new <- paste0(v, "_new")
    v_leg <- paste0(v, "_legacy")
    if (!all(c(v_new, v_leg) %in% names(joined))) return(NULL)
    
    tibble::tibble(
      geo_unit  = joined[[join_key]],
      variable  = v,
      value_new = as.numeric(joined[[v_new]]),
      value_legacy = as.numeric(joined[[v_leg]])
    ) |>
      dplyr::mutate(
        diff     = .data$value_new - .data$value_legacy,
        abs_diff = abs(.data$diff),
        within_tol = 
          .data$abs_diff <= tol | (is.na(.data$value_new) & is.na(.data$value_legacy))
      )
  })
  diffs_long <- dplyr::bind_rows(diffs_list)
  
  # Per-variable summary
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
  
  # Extract rows that differ
  collapsed_diffs <- diffs_long |>
    dplyr::filter(!.data$within_tol)
  
  if (!quiet) {
    message(sprintf(
      "  Variables compared: %d | Overall match rate: %.2f%%",
      length(avail_vars),
      100 * mean(collapsed_summary$share_match)
    ))
  }
  
  # 5) Individual-level comparison (optional — row counts and key stats)
  # ---------------------------------------------------------------------------
  individual_summary <- NULL
  
  if (!is.null(legacy_individual) && file.exists(legacy_individual) &&
      file.exists(new_individual)) {
    if (!quiet) message("  Comparing individual-level census ...")
    
    # Read only enough to compute summary statistics (not the full micro data)
    new_ind <- readr::read_csv(new_individual, show_col_types = FALSE)
    leg_ind <- readr::read_csv(legacy_individual, show_col_types = FALSE)
    
    # Harmonise column names: legacy may have lowercase or different names
    names(new_ind) <- tolower(names(new_ind))
    names(leg_ind) <- tolower(names(leg_ind))
    
    # Key comparisons: total rows, adults, mean schooling
    ind_stats <- function(df, label) {
      edad_col <- intersect(c("edad", "pc09b_edad", "raw_age"), names(df))
      edad <- if (length(edad_col)) as.numeric(df[[edad_col[1]]]) else NA_real_
      
      esc_col <- intersect(c("escolaridad"), names(df))
      esc <- if (length(esc_col)) as.numeric(df[[esc_col[1]]]) else NA_real_
      
      fe_col <- intersect(c("fe"), names(df))
      fe <- if (length(fe_col)) as.numeric(df[[fe_col[1]]]) else rep(1, nrow(df))
      
      adult_col <- intersect(c("adult"), names(df))
      adult <- if (length(adult_col)) as.numeric(df[[adult_col[1]]]) else NA_real_
      
      tibble::tibble(
        pipeline           = label,
        total_rows         = nrow(df),
        n_adults           = sum(adult == 1, na.rm = TRUE),
        weighted_n         = sum(fe, na.rm = TRUE),
        mean_escolaridad   = stats::weighted.mean(esc, fe, na.rm = TRUE),
        share_women        = if ("women" %in% names(df))
          mean(as.numeric(df$women), na.rm = TRUE) else NA_real_,
        n_geo_units        = dplyr::n_distinct(
          df[[intersect(c("geo_id", "loccodigo", join_key), names(df))[1]]]
        )
      )
    }
    
    individual_summary <- dplyr::bind_rows(
      ind_stats(new_ind, "New pipeline"),
      ind_stats(leg_ind, "Dropbox legacy")
    )
    
    if (!quiet) {
      message(sprintf(
        "  Individual rows — new: %s | legacy: %s",
        format(nrow(new_ind), big.mark = ","),
        format(nrow(leg_ind), big.mark = ",")
      ))
    }
  } else if (!quiet) {
    message("  Individual-level comparison skipped (file not found).")
  }
  
  # 6) Persist artefacts as Parquet
  # ---------------------------------------------------------------------------
  .wpq <- function(df, name)
    arrow::write_parquet(
      dplyr::as_tibble(df),
      file.path(out_dir, paste0(name, ".parquet")),
      compression = "zstd"
    )
  
  .wpq(collapsed_summary, "collapsed_summary")
  .wpq(collapsed_diffs,   "collapsed_diffs")
  .wpq(geo_coverage,      "geo_coverage")
  if (!is.null(individual_summary))
    .wpq(individual_summary, "individual_summary")
  
  if (!quiet)
    message("[", cfg$id, "] Census comparison saved to: ", out_dir)
  
  # 7) Return
  # ---------------------------------------------------------------------------
  invisible(list(
    collapsed_summary  = collapsed_summary,
    collapsed_diffs    = collapsed_diffs,
    geo_coverage       = geo_coverage,
    individual_summary = individual_summary,
    out_dir            = out_dir
  ))
}


# --------------------------------------------------------------------------------------------
# compare_idw
# @Arg      : cfg              — city cfg list (must contain $id).
# @Arg      : out_root         — root output folder; {out_root}/{cfg$id}/ is created.
# @Arg      : new_station_dist — character; path to the new pipeline's station-to-station
#                                 distance Parquet (from compute_distance_matrices()).
# @Arg      : new_geo_dist     — character; path to the new pipeline's geo-unit-to-station
#                                 distance Parquet (from compute_distance_matrices()).
# @Arg      : legacy_station_dist — character; path to the Dropbox legacy station distance
#                                    file (CSV or RDS). Typically a wide-format matrix with
#                                    station_code as rows and station names as columns.
# @Arg      : legacy_geo_dist  — character; path to the Dropbox legacy geo-unit-to-station
#                                 distance file (RDS). Typically a long-format data.table with
#                                 station_code, locality, and distance columns.
# @Arg      : station_audit    — data.frame; output from compare_ground_stations()$station_audit.
#                                 Used to restrict the new pipeline's distance matrix to the
#                                 legacy station universe for fair comparison.
# @Arg      : tol_km           — numeric; tolerance in km for declaring two distances as
#                                 matching. Default 0.5 (500 m).
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
    new_station_dist   = NULL,
    new_geo_dist       = NULL,
    legacy_station_dist = NULL,
    legacy_geo_dist    = NULL,
    station_audit      = NULL,
    tol_km             = 0.5,
    quiet              = FALSE
) {
  
  # 0) Dependencies
  req_pkgs <- c("dplyr", "tidyr", "tibble", "arrow", "data.table")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' required.")
  }
  
  # 1) Resolve default file paths
  outdir_distances <- here::here("data", "processed", "distances_matrices")
  
  if (is.null(new_station_dist))
    new_station_dist <- here::here(
      outdir_distances,
      paste0(cfg$id, "_2018_station_distances.parquet")
    )
  if (is.null(legacy_station_dist))
    legacy_station_dist <- here::here(
      "data", "_legacy", "distances",
      paste0("stations_distance_", cfg$id, "_v2.csv")
    )
  
  if (!file.exists(new_station_dist))
    stop("[", cfg$id, "] New station distance file not found:\n  ", new_station_dist)
  if (!file.exists(legacy_station_dist))
    stop("[", cfg$id, "] Legacy station distance file not found:\n  ", legacy_station_dist)
  
  out_dir <- file.path(out_root, cfg$id, "distance_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet)
    message("[", cfg$id, "] Comparing distance matrices ...")
  
  # 2) Normalisation helper (same as compute_distance_matrices)
  .norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    if (requireNamespace("stringi", quietly = TRUE))
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    gsub('["\']', "", x)
  }
  
  # 3) Load new pipeline station distances (long format)
  new_sta <- arrow::read_parquet(new_station_dist) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      station_from = .norm(station_from),
      station_to   = .norm(station_to)
    )
  
  # 4) Load legacy station distances (wide format → long)
  ext <- tools::file_ext(legacy_station_dist)
  if (ext == "rds") {
    leg_wide <- readRDS(legacy_station_dist)
  } else {
    leg_wide <- data.table::fread(legacy_station_dist)
  }
  
  # Identify the station_code column (first column or named "station_code")
  id_col <- if ("station_code" %in% names(leg_wide)) "station_code" else names(leg_wide)[1]
  sta_cols <- setdiff(names(leg_wide), id_col)
  
  leg_long <- leg_wide |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(sta_cols),
      names_to = "station_to",
      values_to = "distance_m_legacy"
    ) |>
    dplyr::rename(station_from = !!id_col) |>
    dplyr::mutate(
      station_from = .norm(station_from),
      station_to   = .norm(station_to),
      distance_km_legacy = as.numeric(distance_m_legacy) / 1000
    ) |>
    dplyr::select(station_from, station_to, distance_km_legacy)
  
  # 5) Restrict new to legacy station universe
  if (!is.null(station_audit)) {
    legacy_names <- .norm(station_audit$station[station_audit$in_legacy])
  } else {
    legacy_names <- unique(c(leg_long$station_from, leg_long$station_to))
  }
  
  new_sta_filtered <- new_sta |>
    dplyr::filter(
      station_from %in% legacy_names,
      station_to %in% legacy_names
    ) |>
    dplyr::rename(distance_km_new = distance_km)
  
  if (!quiet)
    message(sprintf(
      "  Station pairs — new: %s | legacy: %s",
      format(nrow(new_sta_filtered), big.mark = ","),
      format(nrow(leg_long), big.mark = ",")
    ))
  
  # 6) Join and compare
  joined <- dplyr::full_join(
    new_sta_filtered,
    leg_long,
    by = c("station_from", "station_to")
  ) |>
    dplyr::mutate(
      diff_km    = distance_km_new - distance_km_legacy,
      abs_diff   = abs(diff_km),
      within_tol = abs_diff <= tol_km | (is.na(distance_km_new) & is.na(distance_km_legacy))
    )
  
  # Summary
  sta_summary <- joined |>
    dplyr::filter(!is.na(distance_km_new) & !is.na(distance_km_legacy)) |>
    dplyr::filter(station_from != station_to) |>
    dplyr::summarise(
      n_pairs        = dplyr::n(),
      n_match        = sum(within_tol, na.rm = TRUE),
      n_diff         = sum(!within_tol, na.rm = TRUE),
      mean_abs_diff  = mean(abs_diff, na.rm = TRUE),
      max_abs_diff   = max(abs_diff, na.rm = TRUE),
      share_match    = n_match / n_pairs
    )
  
  sta_diffs <- joined |>
    dplyr::filter(!within_tol) |>
    dplyr::arrange(dplyr::desc(abs_diff))
  
  if (!quiet) {
    message(sprintf(
      "  Station distances: %d pairs match (%.1f%%), %d differ > %.1f km",
      sta_summary$n_match, 100 * sta_summary$share_match,
      sta_summary$n_diff, tol_km
    ))
    message(sprintf(
      "  Mean abs diff: %.3f km | Max: %.3f km",
      sta_summary$mean_abs_diff, sta_summary$max_abs_diff
    ))
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
    
    if (tools::file_ext(legacy_geo_dist) == "rds") {
      leg_geo <- data.table::as.data.table(readRDS(legacy_geo_dist))
    } else {
      leg_geo <- data.table::fread(legacy_geo_dist)
    }
    
    # Legacy typically has station_code, locality, distance (in km)
    geo_id_col <- intersect(c("locality", "geo_id", "LocCodigo"), names(leg_geo))[1]
    sta_id_col <- intersect(c("station_code", "station_id"), names(leg_geo))[1]
    dist_col   <- intersect(c("distance", "distance_km"), names(leg_geo))[1]
    
    leg_geo_clean <- leg_geo |>
      dplyr::as_tibble() |>
      dplyr::transmute(
        geo_id = as.character(.data[[geo_id_col]]),
        station_id = .norm(.data[[sta_id_col]]),
        distance_km_legacy = as.numeric(.data[[dist_col]])
      )
    
    # Filter new to legacy stations
    new_geo_filtered <- new_geo |>
      dplyr::filter(station_id %in% legacy_names)
    
    geo_joined <- dplyr::inner_join(
      new_geo_filtered, leg_geo_clean,
      by = c("geo_id", "station_id")
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
    
    if (!quiet)
      message(sprintf(
        "  Geo distances: %d pairs, %.1f%% match within %.1f km",
        geo_summary$n_pairs, 100 * geo_summary$share_match, tol_km
      ))
  }
  
  # 8) Method note
  method_note <- paste0(
    "The new pipeline uses AEQD (Azimuthal Equidistant) projection for ",
    "Euclidean distance. The legacy uses Haversine great-circle distance ",
    "(geosphere::distm). At metro scales these agree within ~0.1-0.5 km."
  )
  
  # 9) Persist
  .wpq <- function(df, name)
    arrow::write_parquet(
      dplyr::as_tibble(df),
      file.path(out_dir, paste0(name, ".parquet")),
      compression = "zstd"
    )
  
  .wpq(sta_summary, "station_dist_summary")
  if (nrow(sta_diffs) > 0) .wpq(sta_diffs, "station_dist_diffs")
  if (!is.null(geo_summary)) .wpq(geo_summary, "geo_dist_summary")
  if (!is.null(geo_diffs) && nrow(geo_diffs) > 0) .wpq(geo_diffs, "geo_dist_diffs")
  
  if (!quiet)
    message("[", cfg$id, "] Distance comparison saved to: ", out_dir)
  
  invisible(list(
    station_dist_summary = sta_summary,
    station_dist_diffs   = sta_diffs,
    geo_dist_summary     = geo_summary,
    geo_dist_diffs       = geo_diffs,
    method_note          = method_note,
    out_dir              = out_dir
  ))
}


# --------------------------------------------------------------------------------------------
# compare_outlier_procedure
# @Arg      : cfg              — city cfg list (must contain $id).
# @Arg      : out_root         — root output folder; {out_root}/{cfg$id}/ is created.
# @Arg      : new_clean_dir    — character; path to the new pipeline's cleaned Arrow dataset
#                                 (output of detect_pollution_outliers()).
# @Arg      : new_raw_dir      — character; path to the new pipeline's raw Arrow dataset
#                                 (before outlier detection). Used to compute pre/post counts.
# @Arg      : legacy_clean_path — character; path to the Dropbox legacy's cleaned data
#                                  (RDS or CSV with outlier-corrected PM values).
# @Arg      : legacy_raw_path  — character; path to the Dropbox legacy's raw balanced panel
#                                 (RDS or CSV before outlier correction).
# @Arg      : pollutants       — character; pollutants to compare. Default c("pm10", "pm25").
# @Arg      : compare_years    — integer vector; years to compare. Default 2023.
# @Arg      : station_audit    — data.frame; from compare_ground_stations()$station_audit.
# @Arg      : quiet            — logical; suppress messages. Default FALSE.
#
# @Output   : named list (invisible) with:
#   $step_summary   — tibble; per-pollutant breakdown of the outlier detection steps:
#                     total obs, flagged (step 1), no benchmark (step 2), no neighbour
#                     value (step 3), final outliers removed.
#   $comparison      — tibble; per-station-pollutant: outliers in new vs legacy.
#   $agreement       — tibble; overlap statistics (both flagged, only new, only legacy).
#   $out_dir         — path to the output directory
#   Parquet files written to {out_root}/{cfg$id}/outlier_comparison/.
#
# @Purpose  : Compare outlier detection between the new pipeline and Dropbox legacy.
# @Details  :
#   PROCEDURE SUMMARY (both pipelines follow the same algorithm)
#   Step 1 — Flag: identify observations above the 99th percentile within
#            each station × year-month.
#   Step 2 — Benchmark: for flagged obs, compute a benchmark from lag/lead
#            values. Classify as reasonable (within mean ± 2 SD of first
#            differences) or unreasonable. Obs with no benchmark (no lag
#            and no lead) are classified as Type 3 (no benchmark).
#   Step 3 — Neighbour check: unreasonable obs are reclassified as
#            reasonable if the difference vs the nearest neighbour falls
#            within mean ± 2 SD of the neighbour-difference distribution.
#   Final  — Flagged obs that remain unreasonable or have no benchmark
#            are set to NA (outlier = 1).
#
#   METHODOLOGICAL DIFFERENCES
#   The new pipeline uses AEQD projection distances for nearest-neighbour
#   identification, while the legacy uses Haversine. The nearest neighbour
#   is also pollutant-specific in the new pipeline (only stations with data
#   for the current pollutant are considered), which may occasionally select
#   a different neighbour than the legacy.
#
# @Written_on: 10/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_outlier_procedure <- function(
    cfg,
    out_root,
    new_clean_dir     = NULL,
    new_raw_dir       = NULL,
    legacy_clean_path = NULL,
    legacy_raw_path   = NULL,
    pollutants        = c("pm10", "pm25"),
    compare_years     = 2023L,
    station_audit     = NULL,
    quiet             = FALSE
) {
  
  # 0) Dependencies
  req_pkgs <- c("dplyr", "tibble", "arrow", "data.table")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' required.")
  }
  
  # 1) Resolve default paths
  if (is.null(new_clean_dir))
    new_clean_dir <- here::here(
      "data", "processed", "outlier_detection",
      paste0(cfg$id, "_metro_clean")
    )
  if (is.null(new_raw_dir))
    new_raw_dir <- here::here(
      "data", "raw", "monitoring_stations",
      paste0(cfg$id, "_metro_dataset")
    )
  if (is.null(legacy_clean_path))
    legacy_clean_path <- here::here(
      "data", "_legacy", "outlier",
      paste0("pollution_data_balanced_", max(compare_years), ".rds")
    )
  if (is.null(legacy_raw_path))
    legacy_raw_path <- here::here(
      "data", "_legacy", "pollution",
      paste0("Air_Pollution_Bogota_2002_2023.csv")
    )
  
  if (!dir.exists(new_clean_dir))
    stop("[", cfg$id, "] New clean dir not found:\n  ", new_clean_dir)
  if (!dir.exists(new_raw_dir))
    stop("[", cfg$id, "] New raw dir not found:\n  ", new_raw_dir)
  
  out_dir <- file.path(out_root, cfg$id, "outlier_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet)
    message("[", cfg$id, "] Comparing outlier detection procedures ...")
  
  # 2) Normalisation helper
  .norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    if (requireNamespace("stringi", quietly = TRUE))
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    gsub('["\']', "", x)
  }
  
  # Legacy station universe
  if (!is.null(station_audit)) {
    legacy_names <- .norm(station_audit$station[station_audit$in_legacy])
  } else {
    legacy_names <- NULL
  }
  
  # 3) Load new pipeline: raw + cleaned
  new_raw <- arrow::open_dataset(new_raw_dir) |>
    dplyr::filter(year %in% compare_years) |>
    dplyr::collect() |>
    data.table::as.data.table()
  new_raw[, station := .norm(station)]
  
  new_clean <- arrow::open_dataset(new_clean_dir) |>
    dplyr::filter(year %in% compare_years) |>
    dplyr::collect() |>
    data.table::as.data.table()
  new_clean[, station := .norm(station)]
  
  # Restrict to legacy stations if available
  if (!is.null(legacy_names)) {
    new_raw   <- new_raw[station %in% legacy_names]
    new_clean <- new_clean[station %in% legacy_names]
  }
  
  if (!quiet)
    message(sprintf(
      "  New pipeline rows — raw: %s | clean: %s",
      format(nrow(new_raw), big.mark = ","),
      format(nrow(new_clean), big.mark = ",")
    ))
  
  # 4) Build step-level summary from the new pipeline
  step_list <- list()
  for (pol in pollutants) {
    flag_col <- paste0(pol, "_outlier")
    if (!pol %in% names(new_raw) || !flag_col %in% names(new_clean)) next
    
    raw_vals  <- new_raw[[pol]]
    clean_vals <- new_clean[[pol]]
    outlier_flag <- new_clean[[flag_col]]
    
    n_total     <- sum(!is.na(raw_vals))
    n_outlier   <- sum(outlier_flag == 1L, na.rm = TRUE)
    n_clean_na  <- sum(is.na(clean_vals) & !is.na(raw_vals))
    pct_removed <- round(100 * n_outlier / max(n_total, 1), 3)
    
    step_list[[pol]] <- tibble::tibble(
      pollutant       = toupper(pol),
      total_obs       = n_total,
      outliers_removed = n_outlier,
      pct_removed     = pct_removed
    )
  }
  step_summary <- dplyr::bind_rows(step_list)
  
  # 5) Load legacy cleaned data (if available) and compare
  comparison  <- NULL
  agreement   <- NULL
  
  if (file.exists(legacy_clean_path)) {
    if (!quiet) message("  Loading legacy cleaned data ...")
    
    ext <- tools::file_ext(legacy_clean_path)
    if (ext == "rds") {
      leg_clean <- data.table::as.data.table(readRDS(legacy_clean_path))
    } else {
      leg_clean <- data.table::fread(legacy_clean_path)
    }
    names(leg_clean) <- tolower(names(leg_clean))
    
    # Normalise station column
    sta_col <- intersect(c("station_code", "station"), names(leg_clean))[1]
    if (!is.na(sta_col)) {
      leg_clean[, station := .norm(get(sta_col))]
    }
    
    # Load legacy raw if available
    leg_raw_exists <- file.exists(legacy_raw_path)
    if (leg_raw_exists) {
      ext_raw <- tools::file_ext(legacy_raw_path)
      if (ext_raw == "rds") {
        leg_raw <- data.table::as.data.table(readRDS(legacy_raw_path))
      } else {
        leg_raw <- data.table::fread(legacy_raw_path)
      }
      names(leg_raw) <- tolower(names(leg_raw))
      sta_col_raw <- intersect(c("station_code", "station"), names(leg_raw))[1]
      if (!is.na(sta_col_raw))
        leg_raw[, station := .norm(get(sta_col_raw))]
      
      # Filter to compare years
      if ("year" %in% names(leg_raw))
        leg_raw <- leg_raw[year %in% compare_years]
    }
    
    # Per-station-pollutant outlier counts
    comp_list <- list()
    for (pol in pollutants) {
      flag_col <- paste0(pol, "_outlier")
      
      # New pipeline outliers per station
      if (flag_col %in% names(new_clean)) {
        new_out <- new_clean[, .(
          outliers_new = sum(get(flag_col) == 1L, na.rm = TRUE),
          total_new    = sum(!is.na(new_raw[[pol]][
            match(paste(station, datetime), paste(new_raw$station, new_raw$datetime))
          ]))
        ), by = station]
      } else next
      
      # Legacy: compare raw vs clean to infer outliers
      if (pol %in% names(leg_clean) && leg_raw_exists && pol %in% names(leg_raw)) {
        # Legacy outlier = was non-NA in raw but is NA in clean
        if ("will_be_na" %in% names(leg_clean)) {
          will_na_col <- if (pol == "pm25") "will_be_na_pm25" else "will_be_na"
          if (will_na_col %in% names(leg_clean)) {
            leg_out <- leg_clean[, .(
              outliers_legacy = sum(get(will_na_col) == 1L, na.rm = TRUE)
            ), by = station]
          } else {
            leg_out <- tibble::tibble(station = character(), outliers_legacy = integer())
          }
        } else {
          leg_out <- tibble::tibble(station = character(), outliers_legacy = integer())
        }
      } else {
        leg_out <- tibble::tibble(station = character(), outliers_legacy = integer())
      }
      
      merged <- dplyr::full_join(
        dplyr::as_tibble(new_out),
        dplyr::as_tibble(leg_out),
        by = "station"
      ) |>
        dplyr::mutate(pollutant = toupper(pol))
      
      comp_list[[pol]] <- merged
    }
    comparison <- dplyr::bind_rows(comp_list)
    
    if (!quiet && !is.null(comparison))
      message(sprintf(
        "  Comparison: %d station-pollutant combinations",
        nrow(comparison)
      ))
  } else if (!quiet) {
    message("  Legacy cleaned data not found — skipping comparison.")
  }
  
  # 6) Persist
  .wpq <- function(df, name)
    arrow::write_parquet(
      dplyr::as_tibble(df),
      file.path(out_dir, paste0(name, ".parquet")),
      compression = "zstd"
    )
  
  .wpq(step_summary, "step_summary")
  if (!is.null(comparison) && nrow(comparison) > 0)
    .wpq(comparison, "outlier_comparison")
  
  if (!quiet)
    message("[", cfg$id, "] Outlier comparison saved to: ", out_dir)
  
  invisible(list(
    step_summary = step_summary,
    comparison   = comparison,
    agreement    = agreement,
    out_dir      = out_dir
  ))
}


# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")