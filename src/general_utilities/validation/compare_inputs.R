# ============================================================================================
# IDB: Air monitoring — input comparisons
# ============================================================================================
#' @Goal: Functions for input comparisons.
#
#' @Description: Compares the inputs of record between the legacy and new pipelines: hourly panels, ground
#   stations, metro-area definitions and census microdata.
#   Sourced by config_utils_validation_old_version.R; never sourced directly.
#
#' @Summary:
#   1. compare_panels
#   2. compare_ground_stations
#   3. compare_metro_area
#   4. compare_census
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: compare_panels
#' @param old_df             legacy-prepared tibble
#' @param new_df             new-prepared tibble
#' @param keys               key cols (default station,y/m/d/h; consider
#                            station_code,y/m/d/h for robustness)
#' @param values             value columns to compare
#' @param tol                named numeric tolerances per column (defaults 0)
#' @param restrict_to_old_codes        if TRUE, keep in new_df only rows whose
#               station_code exists in old_df (no-op if column missing)
#' @param prefer_station        named chr vec: station_code -> preferred name
#               (applies to new_df). Example: c(ATI = "Atizapán")
#' @param new_exclude         rows to drop from new_df before join:
#               * character: station_code values to remove
#               * data.frame/tibble: subset of cols to anti_join away
#               * function(df): returns filtered new_df
#' @param new_shift_hours        integer hours to shift new_df time by.
#               Positive = move forward; negative = move backward.
#               Works if new_df has 'datetime' OR y/m/d/h columns.
#' @return     list(only_old, only_new, diffs_long, diff_summary)
#' @Purpose   : Pinpoint row- and cell-level differences with tolerances.
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
#' @param cfg                    city cfg list. Must contain a $compare sublist with:
#                                legacy_single_csv, legacy_dir, legacy_pattern,
#                                compare_years, value_cols, residual_map,
#                                pipeline_tz, focus_pollutants (optional).
#' @param out_root               root output folder; {out_root}/{cfg$id}/ is created.
#' @param focus_pollutants       character; restrict comparison to these pollutants.
#                                Must be a subset of cfg$compare$value_cols. Default NULL falls 
#                                back to cfg$compare$focus_pollutants, then 
#                                cfg$compare$value_cols (all pollutants)
#' @param pipeline_tz            string; timezone used when the Arrow dataset was BUILT.
#                                Set to "UTC" when bogota_process_stations_data_to_parquet was 
#                                called with tz = "UTC" (the Bogotá default, used to avoid a 
#                                DuckDB R-driver timezone-shift bug). Intentionally separate 
#                                from cfg$tz, which holds the city's true local timezone and 
#                                must remain intact for other pipeline steps. NULL falls back 
#                                to cfg$compare$pipeline_tz, then cfg$tz.
#' @param tol               named numeric; per-pollutant tolerance. Default 0.
#' @param quiet            logical; suppress messages. Default FALSE.
#
#' @return    named list (invisible) with:
#   $diff_summary, $diffs_long, $only_legacy, $only_new,
#   $station_audit, $out_dir
#   Five Parquet files written to {out_root}/{cfg$id}/ground_station_comparison/.
#
#' @Purpose  : Create report of new vs legacy data handling.
#' @details
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
#' @Written_on: 20/03/2026
#' @Written_by: Marcos Paulo
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
    arrow::write_parquet(
      dplyr::as_tibble(df),
      file.path(cmp_dir, paste0(name, ".parquet")),
      compression = "zstd"
    )
  
  write_pq(res$diff_summary, cmp_dir, "diff_summary")
  write_pq(res$diffs_long, cmp_dir,   "diffs_long")
  write_pq(res$only_old, cmp_dir,     "only_legacy")
  write_pq(res$only_new, cmp_dir,     "only_new")
  write_pq(station_audit, cmp_dir,    "station_audit")
  
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
#' @param cfg                    city cfg list (must contain $id, $tz).
#' @param out_root               root output folder; {out_root}/{cfg$id}/ is created.
#' @param new_metro_gpkg         character; path to the new pipeline metro area GeoPackage.
#' @param new_stations_gpkg       character; path to the new pipeline stations GeoPackage.
#' @param legacy_shp_dir         character; path to the directory containing the legacy
#                                 metro area shapefile (.shp + sidecar files).
#' @param station_audit          data.frame; output from compare_ground_stations()$station_audit.
#                                 Used to identify which stations belong to the legacy RMCAB
#                                 universe. If NULL, legacy stations are inferred from the
#                                 new stations sf by filtering for source containing "RMCAB".
#' @param buffer_km              numeric; radius (km) for outside-metro station buffers
#                                 in the new pipeline map. Default 20.
#' @param quiet                  logical; suppress messages. Default FALSE.
#
#' @return    named list (invisible) with:
#   $summary        — tibble comparing key metrics (area, n_municipalities, n_stations)
#   $new_metro_sf   — sf object of the new pipeline metro area (WGS84)
#   $legacy_metro_sf — sf object of the legacy metro area (WGS84)
#   $new_stations_sf — sf object of ALL new pipeline stations (WGS84)
#   $legacy_stations_sf — sf object of legacy-universe stations (WGS84)
#   $out_dir        — path to the output directory
#   Four Parquet/GeoPackage files written to
#   {out_root}/{cfg$id}/metro_area_comparison/.
#
#' @Purpose  : Compare the geographic definitions of the metropolitan area and station
#             coverage between the Dropbox legacy pipeline and the new automated pipeline.
#' @details
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
#' @Written_on: 10/04/2026
#' @Written_by: Marcos Paulo
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
  # utm_epsg() picks the local UTM zone from the map's center.
  epsg_utm <- utm_epsg(new_metro_sf)
  
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
#' @param cfg         city cfg list. Must contain a $compare sublist with:
#                     new_census_collapsed, legacy_census_collapsed, 
#                     census_join_key, and optionally census_tol and individual paths.
#' @param out_root    root output folder; {out_root}/{cfg$id}/ is created.
#' @param compare_vars character vector; variable names to compare. Default covers
#                     core education/labor shares.
#' @param quiet       logical; suppress messages. Default FALSE.
#
#' @return    named list (invisible) with:
#   $collapsed_summary — tibble; per-variable match statistics
#   $collapsed_diffs   — tibble; rows where values differ beyond tolerance
#   $geo_coverage      — tibble; geographic units present in each pipeline
#   $individual_summary — tibble or NULL; row counts and key stats from micro data
#   $out_dir           — path to the output directory
#   Parquet files written to {out_root}/{cfg$id}/census_comparison/.
#
#' @Purpose  : Compare census processing between the new pipeline and the Dropbox legacy.
#' @details
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
#' @Written_on: 10/04/2026
#' @Written_by: Marcos Paulo
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
  
  # 2) Load collapsed datasets. The new side is Parquet; the legacy side stays CSV,
  # because data/_legacy/ is an input of record and is never rewritten.
  new_col <- arrow::read_parquet(new_col_path) |>
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
    
    new_ind <- arrow::read_parquet(new_ind_path)
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
  write_pq(collapsed_summary, out_dir, "collapsed_summary")
  write_pq(collapsed_diffs, out_dir,   "collapsed_diffs")
  write_pq(geo_coverage, out_dir,      "geo_coverage")
  if (!is.null(individual_summary)) {
    write_pq(individual_summary, out_dir, "individual_summary")
  }
  
  if (!quiet) message("[", cfg$id, "] Census comparison saved to: ", out_dir)
  
  invisible(list(
    collapsed_summary  = collapsed_summary,
    collapsed_diffs    = collapsed_diffs,
    geo_coverage       = geo_coverage,
    individual_summary = individual_summary,
    out_dir            = out_dir
  ))
}
