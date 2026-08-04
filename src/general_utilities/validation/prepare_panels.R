# ============================================================================================
# IDB: Air monitoring — legacy/new panel preparation
# ============================================================================================
# @Goal: Functions for legacy/new panel preparation.
#
# @Description: Reshapes both the legacy artefacts and the new pipeline output into one comparable shape,
#   so a difference in a comparison is a real difference and not a formatting one.
#   Sourced by config_utils_validation_old_version.R; never sourced directly.
#
# @Summary:
#   1. build_compare_cfg
#   2. prepare_new_bogota_like_legacy
#   3. prepare_legacy_bogota
#   4. read_legacy_period_csvs
#   5. build_time_parts
#   6. harmonize_station_names
#   7. .std_name
#   8. prepare_legacy_single_csv
#   9. prepare_legacy_cdmx
#   10. prepare_new_panel_like_legacy
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# ============================================================================================
# Validation helpers and functions
# ============================================================================================
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
    # Two tolerances, because the two matrices differ for different reasons.
    # Stations: legacy geosphere::distHaversine uses the *equatorial* radius
    # (6378137 m), which near the equator overstates north-south pairs by ~0.5%
    # (~150 m over 30 km). That is expected metric noise, so allow 0.25 km.
    # Geo units: the legacy-vs-new gap is dominated by the centroid ->
    # point_on_surface shift (routinely 100-500 m), which is exactly what the
    # Step 0-4 ladder must detect — so keep this well under that scale.
    station_tol_km   = 0.25,
    geo_tol_km       = 0.05,
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
                                           "collapse_metro_area_extended.parquet"),
      legacy_census_collapsed = here::here("data", "_legacy", "census",
                                           "collapse_bogota_metro.csv"),
      census_join_key         = "GEO_ID",
      # generate_distances_matrices.R writes <city>/<out_name>_*.parquet, so the
      # new-pipeline matrices live one directory down with the "matrix" prefix.
      new_station_dist        = here::here("data", "processed", "distances_matrices",
                                           "bogota_2018",
                                           "matrix_station_distances.parquet"),
      legacy_station_dist     = here::here("data", "_legacy", "distances", "bogota",
                                           "stations_distance_bogota_v2.csv"),
      new_geo_dist            = here::here("data", "processed", "distances_matrices",
                                           "bogota_2018",
                                           "matrix_geo_station_distances.parquet"),
      legacy_geo_dist         = here::here("data", "_legacy", "distances", "bogota",
                                           "dt_distances.rds"),
      new_clean_dir           = here::here("data", "processed",
                                           "monitoring_stations_outliers",
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
