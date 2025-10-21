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
  "readr",
  "tidyr"
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
# Validation helpers and functions
# ============================================================================================

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


# --------------------------------------------------------------------------------------------
# Function: build_time_parts
# @Arg       : df         — tibble with POSIXct 'datetime'
# @Arg       : tz         — Olson timezone for consistency (default "America/Bogota")
# @Arg       : hour_shift — integer; add hours to align definitions (e.g., +1)
# @Output    : df with integer columns: year, month, day, hour (0–23)
# @Purpose   : Derive time parts in a consistent, explicit way.
# --------------------------------------------------------------------------------------------
build_time_parts <- function(df, tz = "America/Bogota", hour_shift = 0L) {
  stopifnot("datetime" %in% names(df))
  dt <- lubridate::with_tz(df$datetime, tzone = tz)
  if (hour_shift != 0L) dt <- dt + lubridate::hours(hour_shift)
  df$year  <- as.integer(lubridate::year(dt))
  df$month <- as.integer(lubridate::month(dt))
  df$day   <- as.integer(lubridate::day(dt))
  df$hour  <- as.integer(lubridate::hour(dt))
  df
}


# --------------------------------------------------------------------------------------------
# Function: read_legacy_period_csvs
# @Arg       : dir      — folder containing the legacy CSVs
# @Arg       : pattern  — regex pattern for files (default matches your 4 Bogota files)
# @Arg       : tz       — Olson timezone (not used here; legacy files are already split cols)
# @Output    : bound tibble with standard column set
# @Purpose   : Read + row-bind period CSVs created by the old Stata pipeline.
# --------------------------------------------------------------------------------------------
read_legacy_period_csvs <- function(
    dir,
    pattern = "^Air_Pollution_Bogota_\\d{4}_\\d{4}\\.csv$",
    tz = "America/Bogota"
) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No legacy CSVs found in: ", dir)
  purrr::map_dfr(files, readr::read_csv, show_col_types = FALSE)
}


# --------------------------------------------------------------------------------------------
# Function: prepare_legacy_bogota
# @Arg       : legacy_df       — tibble read from legacy CSVs
# @Arg       : rename_map      — named chr vector for station harmonization
#                                e.g. c("Centro de Alto Rendimiento"="CAR" ...)
# @Arg       : drop_stations   — chr vector to exclude (default: character(0) — keep all)
# @Arg       : tz              — Olson timezone (default "UTC")
# @Arg       : panelize        — logical; if TRUE, create a balanced hourly panel 
#                                (default TRUE)
# @Arg       : panel_years     — integer vector of years to span if panelize=TRUE 
#                                (default 2002:2023)
# @Arg       : verbose         — logical; print brief summary and counts (default TRUE)
# @Output    : tibble with columns:
#                station, datetime, pm10, pm25, ozone, co, no2, year, month, day, hour
# @Purpose   : Transform legacy columns/labels to the new schema; robust to hour==24.
#              Optionally expand to a full station × hourly grid with NAs for missing obs.
# @Written_on: 27/08/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
prepare_legacy_bogota <- function(legacy_df,
                                  rename_map = c(),
                                  drop_stations = character(),
                                  tz = "UTC",
                                  panelize = TRUE,
                                  panel_years = 2002:2023,
                                  verbose = TRUE) {
  # 1) Normalize legacy columns/types (handles upper/lower/Spanish variants)
  df <- legacy_df |>
    dplyr::mutate(
      ozone = ozono,
      pm25  = pm25,
      pm10  = pm10,
      co    = co,
      no2   = no2,
      hour  = suppressWarnings(as.integer(hour)),
      year  = suppressWarnings(as.integer(year)),
      month = suppressWarnings(as.integer(month)),
      day   = suppressWarnings(as.integer(day))
    )
  
  # 2) Fix 24:00 → 00:00 next day (safe across month/year boundary)
  date0 <- lubridate::make_datetime(year  = df$year,
                                    month = df$month,
                                    day   = df$day,
                                    hour  = df$hour,
                                    tz    = tz)
  is24  <- !is.na(df$hour) & df$hour == 24L
  n24   <- sum(is24, na.rm = TRUE)
  if (any(is24)) {
    date0[is24] <- date0[is24]
    df$hour[is24] <- 0L
  }
  df$datetime <- as.POSIXct(date0)
  
  # 3) Keep core columns and harmonize station names (no drops by default)
  df <- df |>
    dplyr::select(station, datetime, pm10, pm25, ozone, co, no2) |>
    harmonize_station_names(rename_map = rename_map,
                            drop_stations = drop_stations) |>
    dplyr::distinct(station, datetime, .keep_all = TRUE) |>
    dplyr::arrange(station, datetime)
  
  # 4) Optional: expand to a balanced hourly panel over requested years
  if (isTRUE(panelize)) {
    # 4a) Hourly sequence across the full span (local tz)
    y0 <- min(panel_years, na.rm = TRUE)
    y1 <- max(panel_years, na.rm = TRUE)
    start_dt <- as.POSIXct(sprintf("%d-01-01 00:00:00", y0), tz = tz)
    end_dt   <- as.POSIXct(sprintf("%d-12-31 23:00:00", y1), tz = tz)
    hours_seq <- seq(from = start_dt, to = end_dt, by = "1 hour")
    
    # 4b) Station universe after harmonization (keep all)
    stations <- sort(unique(df$station))
    
    # 4c) Cross station × hour and left-join pollutants
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
    
    df_narrow <- df |>
      dplyr::select(station, datetime, pm10, pm25, ozone, co, no2)
    
    panel <- grid |>
      dplyr::left_join(df_narrow, by = c("station", "datetime")) |>
      dplyr::arrange(station, datetime)
    
    if (isTRUE(verbose)) {
      added <- nrow(panel) - nrow(df)
      msg <- sprintf(
        "🧩 Panelized to %s stations × %s hours → %s rows (added %s synthetic rows).",
        format(length(stations), big.mark=","), 
        format(length(hours_seq), big.mark=","),
        format(nrow(panel), big.mark=","), 
        format(added, big.mark=","))
      message(msg)
      if (n24 > 0) {
        message(sprintf("⏱️  Rolled %d observations from 24:00 → 00:00 next day.", n24))
      }
    }
    df <- panel
  } else {
    # Add year/month/day/hour derived from datetime for consistency (no panel expansion)
    df <- df |>
      dplyr::mutate(
        year  = lubridate::year(datetime),
        month = lubridate::month(datetime),
        day   = lubridate::day(datetime),
        hour  = lubridate::hour(datetime)
      )
    if (isTRUE(verbose) && n24 > 0) {
      message(sprintf("⏱️  Rolled %d observations from 24:00 → 00:00 next day.", n24))
    }
  }
  
  # 5) Column order / types (pollutants numeric)
  df <- df |>
    dplyr::mutate(
      pm10  = suppressWarnings(as.numeric(pm10)),
      pm25  = suppressWarnings(as.numeric(pm25)),
      ozone = suppressWarnings(as.numeric(ozone)),
      co    = suppressWarnings(as.numeric(co)),
      no2   = suppressWarnings(as.numeric(no2))
    ) |>
    dplyr::select(station, datetime, pm10, pm25, ozone, co, no2, year, month, day, hour)
  
  df
}


# --------------------------------------------------------------------------------------------
# Function: prepare_new_bogota_like_legacy
# @Arg       : new_df        — tibble (your parquet read), must include datetime + vars
# @Arg       : rename_map    — named chr vector for station harmonization
# @Arg       : drop_stations — chr vector to exclude
# @Arg       : year_keep     — integer vector of years to keep (e.g., 2002:2023)
# @Arg       : hour_shift    — integer; shift hours to align legacy convention (e.g., +1)
# @Arg       : tz            — Olson timezone (default "America/Bogota")
# @Output    : tibble with same columns as prepare_legacy_bogota()
# @Purpose   : Make the new panel directly comparable to legacy.
# --------------------------------------------------------------------------------------------
prepare_new_bogota_like_legacy <- function(
    new_df,
    rename_map = c(),
    drop_stations = character(),
    year_keep = 2002:2023,
    hour_shift = 0L,
    tz = "America/Bogota"
) {
  df <- new_df |>
    dplyr::transmute(
      station,
      datetime = as.POSIXct(.data$datetime, tz = tz),
      pm10     = pm10, 
      pm25     = `pm2.5`,
      ozone    = ozone,
      co       = co,
      no2      = no2
    )

  df <- build_time_parts(df, tz = tz, hour_shift = hour_shift) |>
    dplyr::filter(.data$year %in% year_keep) |>
    harmonize_station_names(rename_map = rename_map, drop_stations = drop_stations) |>
    dplyr::select(station, datetime, pm10, pm25, ozone, co, no2, year, month, day, hour) |>
    dplyr::arrange(.data$station, .data$datetime)
  
  df
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

# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")