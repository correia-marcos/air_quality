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
packages <- c(
  "arrow",
  "dplyr",
  "haven",
  "here",
  "lubridate",
  "readr",
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
# Function: prepare_new_cdmx_like_legacy
# @Arg  : new_df            — tibble read from your new Parquet dataset
# @Arg  : stations_keep_df  — data frame (or sf) listing stations to keep.
#                             Default NULL (keep all). Must contain a column with
#                             station codes (default 'code'); geometry is ignored.
# @Arg  : station_code_col  — column name in stations_keep_df with station codes
#                             (default "code")
# @Arg  : year_keep         — integer vector of years to keep (default 2010:2023)
# @Arg  : tz                — Olson timezone (default "UTC")
# @Output : tibble with columns (order fixed):
#           datehour, year, month, day, hour, station_code, pm25, pm10, no2, o3, co
# @Purpose : Make the *new* panel directly comparable to the legacy one and (optionally)
#            subset to a set of station codes from an external table.
# --------------------------------------------------------------------------------------------
prepare_new_cdmx_like_legacy <- function(
    new_df,
    stations_keep_df = NULL,
    station_code_col = "code",
    year_keep = 2010:2023,
    tz = "UTC"
) {
  # -- 1) Ensure we have a station_code column to work with --------------------
  if (!"station_code" %in% names(new_df)) {
    if ("station" %in% names(new_df)) {
      new_df$station_code <- new_df$station
    } else {
      stop("new_df must have 'station_code' or 'station' column.")
    }
  }
  
  # -- 2) Optional: filter stations using stations_keep_df ---------------------
  if (!is.null(stations_keep_df)) {
    if (!station_code_col %in% names(stations_keep_df)) {
      stop("stations_keep_df must contain column '", station_code_col, "'.")
    }
    keep_codes <- unique(toupper(trimws(stations_keep_df[[station_code_col]])))
    new_df$station_code <- toupper(trimws(new_df$station_code))
    new_df <- dplyr::filter(new_df, .data$station_code %in% keep_codes)
  }
  
  # -- 3) Rename/transform to legacy names, keep only needed vars --------------
  if (!"datetime" %in% names(new_df)) {
    stop("new_df must have a 'datetime' column (timestamp).")
  }
  # Some files use "ozone", others "ozono"—prefer "ozone", fall back to "ozono"
  o3_col <- if ("ozone" %in% names(new_df)) "ozone" else
    if ("ozono" %in% names(new_df)) "ozono" else NA_character_
  if (is.na(o3_col)) stop("new_df must have 'ozone' (or 'ozono').")
  if (!"pm2.5" %in% names(new_df)) stop("new_df must have column `pm2.5`.")
  must_have <- c("pm10","no2","co")
  miss <- setdiff(must_have, names(new_df))
  if (length(miss)) stop("new_df is missing: ", paste(miss, collapse = ", "))
  
  df <- new_df |>
    dplyr::transmute(
      datehour     = as.POSIXct(.data$datetime, tz = tz),
      station_code = .data$station_code,
      pm25         = .data$`pm2.5`,
      pm10         = .data$pm10,
      no2          = .data$no2,
      o3           = .data[[o3_col]],
      co           = .data$co
    )
  
  # -- 4) Time parts + year filter --------------------------------------------
  df <- df |>
    dplyr::mutate(
      year  = lubridate::year(.data$datehour),
      month = lubridate::month(.data$datehour),
      day   = lubridate::day(.data$datehour),
      hour  = lubridate::hour(.data$datehour)
    ) |>
    dplyr::filter(.data$year %in% year_keep)
  
  # -- 5) Enforce numeric types + final column order ---------------------------
  num_cols <- c("pm25","pm10","no2","o3","co")
  df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
  
  keep_order <- c("datehour","year","month","day","hour",
                  "station_code","pm25","pm10","no2","o3","co")
  
  df |>
    dplyr::select(dplyr::all_of(keep_order)) |>
    dplyr::arrange(.data$station_code, .data$datehour)
}


# --------------------------------------------------------------------------------------------
# Function: compare_panels
# @Arg       : old_df      — legacy-prepared tibble
# @Arg       : new_df      — new-prepared tibble
# @Arg       : keys        — character vector of key columns (default station,y/m/d/h)
# @Arg       : values      — character vector of value columns to compare
# @Arg       : tol         — named numeric tolerances per column (defaults 0 when missing)
# @Output    : list(only_old, only_new, diffs_long, diff_summary)
# @Purpose   : Pinpoint row-level and cell-level differences with tolerances.
# --------------------------------------------------------------------------------------------
compare_panels <- function(
    old_df,
    new_df,
    keys   = c("station", "year", "month", "day", "hour"),
    values = c("pm10", "pm25", "ozone", "co", "no2"),
    tol    = c()
) {
  # 0) helper: fetch tolerance (0 if not provided)
  tol_get <- function(v) {
    if (!is.null(tol) && !is.null(tol[[v]]) && is.finite(tol[[v]])) as.numeric(tol[[v]]) else 0
  }
  
  # 1) sanity checks
  miss_old <- setdiff(c(keys, values), names(old_df))
  miss_new <- setdiff(c(keys, values), names(new_df))
  if (length(miss_old) || length(miss_new)) {
    stop(
      "Missing columns. ",
      if (length(miss_old)) paste0("old_df lacks: ", paste(miss_old, collapse = ", ")),
      if (length(miss_old) && length(miss_new)) " ; ",
      if (length(miss_new)) paste0("new_df lacks: ", paste(miss_new, collapse = ", "))
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
    dplyr::mutate(any_old = rowSums(!is.na(dplyr::across(dplyr::everything()))) > 0) |>
    (\(x) x$any_old)()
  
  has_any_new <- joined |>
    dplyr::select(dplyr::ends_with("_new")) |>
    dplyr::mutate(any_new = rowSums(!is.na(dplyr::across(dplyr::everything()))) > 0) |>
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
      dplyr::select(variable, dplyr::all_of(keys), value_old, value_new, diff, absv, within_tol)
  })
  
  # 5) summaries by variable (now easy & robust)
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