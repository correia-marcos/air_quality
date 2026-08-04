# ============================================================================================
# IDB: Air monitoring — hourly outlier detection
# ============================================================================================
# @Goal: Functions for hourly outlier detection.
#
# @Description: Flags anomalous hourly readings by comparing each station against its own history and
#   against its nearest neighbours, out-of-core via DuckDB.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
# @Summary:
#   1. detect_pollution_outliers
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: detect_pollution_outliers
#
# @Arg arrow_dir           : string; path to Arrow dataset of hourly data.
# @Arg station_dist_path   : string; station_distances.parquet path.
# @Arg out_dir             : string; output directory.
# @Arg out_name            : string; prefix, e.g. "bogota_2018".
# @Arg pollutants          : character; default c("pm10", "pm25").
# @Arg pct_flag            : numeric [0,1]; upper-tail quantile. Default 0.99.
# @Arg n_sd                : numeric; tolerance half-width in SD units. Default 2.
# @Arg on_missing_temporal : string; "finish" or "continue". Default "continue".
# @Arg on_missing_neighbor : string; "finish" or "second". Default "second".
# @Arg neighbor_eligibility: string; "with_data" or "all". Default "with_data".
# @Arg overwrite           : logical; skip if output exists. Default TRUE.
# @Arg quiet               : logical; suppress messages. Default FALSE.
#
# @Details:
#   `neighbor_eligibility` decides which stations may serve as the neighbor:
#     "with_data" = the paper's rule: only stations with at least one non-missing
#                   reading for this pollutant in this year are candidates.
#     "all"       = the legacy rule: the static distance matrix alone decides, so
#                   a station that never reported can still be picked as nearest.
#                   Its readings are then all NA, the spatial check is infeasible,
#                   and every flagged hour at that station is dropped unchecked.
#   Use "all" together with on_missing_temporal = "finish" and
#   on_missing_neighbor = "finish" to reproduce the legacy procedure exactly.
#
#   Creates `{pollutant}_outlier_reason` columns:
#     0 = Valid or not flagged
#     1 = Flagged, no temporal benchmark, no feasible spatial rescue
#     2 = Flagged, failed temporal, no feasible spatial rescue
#     3 = Flagged, failed temporal, failed spatial
#     4 = Flagged, no temporal benchmark, failed spatial
#
#   Also creates diagnostic count columns:
#     `{pollutant}_n_missing_temporal_sd`
#     `{pollutant}_n_zero_temporal_sd`
#     `{pollutant}_n_missing_spatial_sd`
#     `{pollutant}_n_zero_spatial_sd`
#
# @Written_on : 02/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
detect_pollution_outliers <- function(
    arrow_dir,
    station_dist_path,
    out_dir,
    out_name,
    pollutants          = c("pm10", "pm25"),
    pct_flag            = 0.99,
    n_sd                = 2,
    on_missing_temporal = "continue",
    on_missing_neighbor = "second",
    neighbor_eligibility = "with_data",
    overwrite           = TRUE,
    quiet               = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  # stringi is required to harmonize station identifiers across files.
  pkgs <- c("arrow", "data.table", "dplyr", "stringi")
  
  for (p in pkgs) {
  }
  
  # Validate behavioral options early to avoid silent mistakes.
  on_missing_temporal <- match.arg(
    on_missing_temporal,
    c("finish", "continue")
  )
  
  on_missing_neighbor <- match.arg(
    on_missing_neighbor,
    c("finish", "second")
  )

  neighbor_eligibility <- match.arg(
    neighbor_eligibility,
    c("with_data", "all")
  )
  
  # Normalize station IDs in the same way as the distance-matrix code.
  # This avoids failed joins due to accents, quotes, case, or whitespace.
  
  # 1. Output path + early exit
  # -----------------------------------------------------------------------
  out_path <- file.path(out_dir, paste0(out_name, "_clean"))
  
  # Skip computation only when explicitly requested.
  if (!overwrite && dir.exists(out_path)) {
    if (!quiet) {
      message("Output exists; overwrite=FALSE — skipping.")
    }
    
    return(invisible(out_path))
  }
  
  # Create output root if needed.
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # Replace previous output when overwrite = TRUE.
  if (dir.exists(out_path)) {
    unlink(out_path, recursive = TRUE)
  }
  
  dir.create(out_path)
  
  # 2. Load and validate station-distance table
  # -----------------------------------------------------------------------
  # This table is used only to define nearest monitoring stations.
  dist_dt <- data.table::as.data.table(
    arrow::read_parquet(station_dist_path)
  )
  
  # Require the schema produced by compute_distance_matrices().
  req_dist_cols <- c("station_from", "station_to", "distance_km")
  miss_dist_cols <- setdiff(req_dist_cols, names(dist_dt))
  
  if (length(miss_dist_cols) > 0L) {
    stop("Distance table is missing: ", paste(miss_dist_cols, collapse = ", "))
  }
  
  # Harmonize station names in the distance table.
  dist_dt[, station_from := normalize_station(station_from)]
  dist_dt[, station_to   := normalize_station(station_to)]
  
  if (!quiet) {
    message("Distance table loaded.")
  }
  
  # 3. Open Arrow dataset and collect available years
  # -----------------------------------------------------------------------
  arrow_ds <- arrow::open_dataset(arrow_dir)
  
  years <- arrow_ds |>
    dplyr::select(year) |>
    dplyr::distinct()   |>
    dplyr::collect()    |>
    dplyr::pull(year)   |>
    sort()
  
  # 4. Inner helper: flag one pollutant at a time
  # -----------------------------------------------------------------------
  .flag_pollutant <- function(dt, pol, dist_dt, pct_flag, n_sd,
                              miss_temp, miss_neigh, neigh_elig) {
    
    # Skip pollutant if it is absent in this city/year dataset.
    if (!pol %in% names(dt)) {
      return(invisible(NULL))
    }
    
    # Main output columns for this pollutant.
    flag_col   <- paste0(pol, "_outlier")
    reason_col <- paste0(pol, "_outlier_reason")
    
    # Diagnostic columns repeated within station-month cells.
    miss_tsd_col <- paste0(pol, "_n_missing_temporal_sd")
    zero_tsd_col <- paste0(pol, "_n_zero_temporal_sd")
    miss_ssd_col <- paste0(pol, "_n_missing_spatial_sd")
    zero_ssd_col <- paste0(pol, "_n_zero_spatial_sd")
    
    # Initialize classification outputs.
    dt[, (flag_col)   := 0L]
    dt[, (reason_col) := 0L]
    
    # Initialize diagnostics to zero.
    dt[, (miss_tsd_col) := 0L]
    dt[, (zero_tsd_col) := 0L]
    dt[, (miss_ssd_col) := 0L]
    dt[, (zero_ssd_col) := 0L]
    
    # Nothing to classify if the pollutant is entirely missing.
    if (all(is.na(dt[[pol]]))) {
      return(invisible(NULL))
    }
    
    # -- (1) Nearest neighbors -------------------------------------------
    # "with_data": a neighbor must have at least one non-missing observation for this 
    # pollutant in the current year-level data
    if (neigh_elig == "with_data") {
      has_data <- dt[!is.na(get(pol)), unique(station)]

      near_dt <- dist_dt[
        distance_km > 0 &
          station_from %in% has_data &
          station_to %in% has_data
      ]
    } else {
      near_dt <- dist_dt[distance_km > 0]
    }
    
    # Rank possible neighbors by distance within origin station.
    data.table::setorder(near_dt, station_from, distance_km)
    near_dt[, rank := seq_len(.N), by = station_from]
    
    # Attach closest eligible neighbor.
    near_1 <- near_dt[
      rank == 1L,
      .(station = station_from, near1 = station_to)
    ]
    
    dt[near_1, .t_near1 := i.near1, on = "station"]
    
    # Attach second closest eligible neighbor only when requested.
    if (miss_neigh == "second") {
      near_2 <- near_dt[
        rank == 2L,
        .(station = station_from, near2 = station_to)
      ]
      
      dt[near_2, .t_near2 := i.near2, on = "station"]
    }
    
    # -- (2) Lag, lead, and temporal differences -------------------------
    # The panel is balanced before this helper runs, so adjacent rows
    # represent adjacent hours within each station.
    dt[, `:=`(
      .t_lag  = data.table::shift(get(pol), 1L, type = "lag"),
      .t_lead = data.table::shift(get(pol), 1L, type = "lead")
    ), by = station]
    
    # First difference used to define normal temporal volatility.
    dt[, .t_diff := get(pol) - .t_lag]
    
    # Lookup table for simultaneous neighbor values.
    tmp_lkp <- dt[, .(station, datetime, tmp_p = get(pol))]
    
    # Pull simultaneous value at closest neighbor.
    dt[
      tmp_lkp,
      .t_vn1 := i.tmp_p,
      on = .(.t_near1 = station, datetime)
    ]
    
    dt[, .t_diff_nb1 := get(pol) - .t_vn1]
    
    # Pull simultaneous value at second closest neighbor when requested.
    if (miss_neigh == "second") {
      dt[
        tmp_lkp,
        .t_vn2 := i.tmp_p,
        on = .(.t_near2 = station, datetime)
      ]
      
      dt[, .t_diff_nb2 := get(pol) - .t_vn2]
    }
    
    rm(tmp_lkp)
    
    # -- (3) Temporal benchmark construction -----------------------------
    # Type 1: both adjacent readings; Type 2: one adjacent reading;
    # Type 3: no adjacent reading.
    dt[, .t_bench := data.table::fcase(
      !is.na(.t_lag) & !is.na(.t_lead), (.t_lag + .t_lead) / 2,
      !is.na(.t_lag),  .t_lag,
      !is.na(.t_lead), .t_lead,
      default = NA_real_
    )]
    
    dt[, .t_btype := data.table::fcase(
      !is.na(.t_lag) & !is.na(.t_lead), 1L,
      !is.na(.t_lag) | !is.na(.t_lead), 2L,
      default = 3L
    )]
    
    # Difference between the observed value and the temporal benchmark.
    dt[, .t_diff_b := get(pol) - .t_bench]
    
    # -- (4) Flag station-month right-tail observations ------------------
    # Only values above this threshold can become outliers tz = "UTC" is required: 
    # the Parquet stores timestamps with no time zone, so format() could shift timezone.
    dt[, .t_ym := format(datetime, "%Y-%m", tz = "UTC")]
    
    dt[, .t_p99 := as.numeric(
      stats::quantile(.SD[[1]], probs = pct_flag, na.rm = TRUE)
    ), by = .(station, .t_ym), .SDcols = pol]
    
    dt[, .t_flag := data.table::fifelse(
      !is.na(get(pol)) & !is.na(.t_p99) & get(pol) > .t_p99,
      1L,
      0L
    )]
    
    # -- (5) Station-month temporal and spatial statistics ---------------
    # Temporal stats are based on own-station first differences.
    # Spatial stats are based on station-neighbor simultaneous differences.
    if (miss_neigh == "second") {
      dt[, `:=`(
        .t_md   = mean(.t_diff,     na.rm = TRUE),
        .t_sd   = sd(.t_diff,       na.rm = TRUE),
        .t_mnb1 = mean(.t_diff_nb1, na.rm = TRUE),
        .t_snb1 = sd(.t_diff_nb1,   na.rm = TRUE),
        .t_mnb2 = mean(.t_diff_nb2, na.rm = TRUE),
        .t_snb2 = sd(.t_diff_nb2,   na.rm = TRUE)
      ), by = .(station, .t_ym)]
    } else {
      dt[, `:=`(
        .t_md   = mean(.t_diff,     na.rm = TRUE),
        .t_sd   = sd(.t_diff,       na.rm = TRUE),
        .t_mnb1 = mean(.t_diff_nb1, na.rm = TRUE),
        .t_snb1 = sd(.t_diff_nb1,   na.rm = TRUE)
      ), by = .(station, .t_ym)]
    }
    
    # -- (6) Diagnostic counts ------------------------------------------
    # sd() is NA when there are fewer than two non-missing differences.
    # sd() is zero when the station-month differences are constant.
    dt[, .t_missing_tsd := is.na(.t_sd)]
    dt[, .t_zero_tsd    := !is.na(.t_sd) & .t_sd == 0]
    
    dt[, .t_missing_ssd1 := is.na(.t_snb1)]
    dt[, .t_zero_ssd1    := !is.na(.t_snb1) & .t_snb1 == 0]
    
    # For the second-neighbor case, diagnose whether all spatial
    # alternatives are missing and whether any feasible one has zero SD.
    if (miss_neigh == "second") {
      dt[, .t_missing_ssd2 := is.na(.t_snb2)]
      dt[, .t_zero_ssd2    := !is.na(.t_snb2) & .t_snb2 == 0]
      
      dt[, .t_missing_ssd := .t_missing_ssd1 & .t_missing_ssd2]
      dt[, .t_zero_ssd    := .t_zero_ssd1 | .t_zero_ssd2]
    } else {
      dt[, .t_missing_ssd := .t_missing_ssd1]
      dt[, .t_zero_ssd    := .t_zero_ssd1]
    }
    
    # Store flagged-observation diagnostics as station-month counts.
    dt[, (miss_tsd_col) := sum(.t_flag == 1L & .t_missing_tsd),
       by = .(station, .t_ym)]
    
    dt[, (zero_tsd_col) := sum(.t_flag == 1L & .t_zero_tsd),
       by = .(station, .t_ym)]
    
    dt[, (miss_ssd_col) := sum(.t_flag == 1L & .t_missing_ssd),
       by = .(station, .t_ym)]
    
    dt[, (zero_ssd_col) := sum(.t_flag == 1L & .t_zero_ssd),
       by = .(station, .t_ym)]
    
    # -- (7) Temporal classification ------------------------------------
    # 1 = reasonable, 2 = unreasonable, 3 = no temporal benchmark.
    dt[, .t_cat := data.table::fcase(
      .t_btype == 3L, 3L,
      !is.na(.t_diff_b) &
        !is.na(.t_md) & !is.na(.t_sd) &
        .t_diff_b > (.t_md - n_sd * .t_sd) &
        .t_diff_b < (.t_md + n_sd * .t_sd), 1L,
      default = 2L
    )]
    
    # If temporal missingness is allowed to continue, Type 3 values
    # can still be rescued by the spatial check.
    cats_check <- if (miss_temp == "continue") c(2L, 3L) else 2L
    
    # -- (8) Spatial rescue using closest neighbor -----------------------
    # A spatial check is feasible only when both the current difference
    # and the station-month spatial benchmark exist.
    dt[, .t_spat1_feasible := !is.na(.t_diff_nb1) &
         !is.na(.t_mnb1) & !is.na(.t_snb1)]
    
    dt[, .t_spat1_pass := .t_spat1_feasible &
         .t_diff_nb1 > (.t_mnb1 - n_sd * .t_snb1) &
         .t_diff_nb1 < (.t_mnb1 + n_sd * .t_snb1)]
    
    # If closest-neighbor spatial check passes, keep the observation.
    dt[
      .t_cat %in% cats_check & .t_spat1_pass == TRUE,
      .t_cat := 1L
    ]
    
    # -- (9) Spatial rescue using second closest neighbor ----------------
    if (miss_neigh == "second") {
      
      # The second neighbor is used only when the first check is infeasible.
      dt[, .t_spat2_feasible := !is.na(.t_diff_nb2) &
           !is.na(.t_mnb2) & !is.na(.t_snb2)]
      
      dt[, .t_spat2_pass := .t_spat2_feasible &
           .t_diff_nb2 > (.t_mnb2 - n_sd * .t_snb2) &
           .t_diff_nb2 < (.t_mnb2 + n_sd * .t_snb2)]
      
      # Rescue observations when the first neighbor cannot be used
      # and the second neighbor validates the reading.
      dt[
        .t_cat %in% cats_check &
          .t_spat1_feasible == FALSE &
          .t_spat2_pass == TRUE,
        .t_cat := 1L
      ]
      
      # No spatial check exists if neither neighbor is feasible.
      dt[, .t_no_spat := .t_spat1_feasible == FALSE &
           .t_spat2_feasible == FALSE]
      
      # Spatial failure is assigned to the feasible check that was used.
      dt[, .t_failed_spat := .t_spat1_feasible == TRUE &
           .t_spat1_pass == FALSE]
      
      dt[
        .t_spat1_feasible == FALSE & .t_spat2_feasible == TRUE,
        .t_failed_spat := .t_spat2_pass == FALSE
      ]
    } else {
      
      # Without second-neighbor fallback, only the closest neighbor matters.
      dt[, .t_no_spat := .t_spat1_feasible == FALSE]
      
      dt[, .t_failed_spat := .t_spat1_feasible == TRUE &
           .t_spat1_pass == FALSE]
    }
    
    # -- (10) Assign diagnostic reason codes -----------------------------
    # Reason 1: flagged Type 3, no feasible spatial rescue.
    dt[
      .t_flag == 1L & .t_cat == 3L & .t_no_spat == TRUE,
      (reason_col) := 1L
    ]
    
    # Reason 2: flagged temporal failure, no feasible spatial rescue.
    dt[
      .t_flag == 1L & .t_cat == 2L & .t_no_spat == TRUE,
      (reason_col) := 2L
    ]
    
    # Reason 3: flagged temporal failure and spatial check failed.
    dt[
      .t_flag == 1L & .t_cat == 2L & .t_failed_spat == TRUE,
      (reason_col) := 3L
    ]
    
    # Reason 4: flagged Type 3 and spatial check failed.
    dt[
      .t_flag == 1L & .t_cat == 3L & .t_failed_spat == TRUE,
      (reason_col) := 4L
    ]
    
    # Legacy-style behavior: missing temporal benchmark is final.
    # This reproduces the older, more punitive rule when requested.
    if (miss_temp == "finish") {
      dt[.t_flag == 1L & .t_cat == 3L, (reason_col) := 1L]
    }
    
    # -- (11) Final masking ----------------------------------------------
    # Only observations with positive reason codes are removed.
    dt[get(reason_col) > 0L, (flag_col) := 1L]
    dt[get(flag_col) == 1L, (pol) := NA_real_]
    
    # -- (12) Cleanup temporary columns ----------------------------------
    # Keep final flags, reason codes, diagnostics, and cleaned pollutant.
    drop_cols <- c(
      ".t_lag", ".t_lead", ".t_diff", ".t_diff_nb1", ".t_bench",
      ".t_btype", ".t_diff_b", ".t_ym", ".t_p99", ".t_flag",
      ".t_md", ".t_sd", ".t_mnb1", ".t_snb1", ".t_cat",
      ".t_near1", ".t_vn1", ".t_missing_tsd", ".t_zero_tsd",
      ".t_missing_ssd1", ".t_zero_ssd1", ".t_missing_ssd",
      ".t_zero_ssd", ".t_spat1_feasible", ".t_spat1_pass",
      ".t_no_spat", ".t_failed_spat"
    )
    
    if (miss_neigh == "second") {
      drop_cols <- c(
        drop_cols, ".t_diff_nb2", ".t_mnb2", ".t_snb2",
        ".t_near2", ".t_vn2", ".t_missing_ssd2", ".t_zero_ssd2",
        ".t_spat2_feasible", ".t_spat2_pass"
      )
    }
    
    drop_cols <- intersect(drop_cols, names(dt))
    dt[, (drop_cols) := NULL]
    
    invisible(NULL)
  }
  
  # 5. Year loop
  # -----------------------------------------------------------------------
  for (yr in years) {
    if (!quiet) {
      message("  [", yr, "] Collecting ...")
    }
    
    # Collect one year at a time to limit memory use.
    dt_yr <- arrow_ds |>
      dplyr::filter(year == yr) |>
      dplyr::collect()          |>
      data.table::as.data.table()
    
    if (nrow(dt_yr) == 0L) {
      next
    }
    
    # Normalize station identifiers before balancing and joining.
    dt_yr[, station := normalize_station(station)]
    
    all_sta  <- unique(dt_yr$station)
    yr_start <- min(dt_yr$datetime)
    yr_end   <- max(dt_yr$datetime)
    
    # Add one boundary hour before and after the year.
    # This avoids losing lag/lead information at year boundaries.
    prev_cutoff <- yr_start - 3600
    next_cutoff <- yr_end   + 3600

    # Read the year in UTC for the same reason as .t_ym above: the hive partitions were 
    # written from UTC, so a session-zone read here would look in the wrong partition
    prev_yr     <- as.integer(format(prev_cutoff, "%Y", tz = "UTC"))
    next_yr     <- as.integer(format(next_cutoff, "%Y", tz = "UTC"))
    
    bnd_prev <- arrow_ds |>
      dplyr::filter(year == prev_yr, datetime == prev_cutoff) |>
      dplyr::collect() |>
      data.table::as.data.table()
    
    if (nrow(bnd_prev) > 0L) {
      bnd_prev[, station := normalize_station(station)]
      bnd_prev <- bnd_prev[station %in% all_sta]
    }
    
    bnd_next <- arrow_ds |>
      dplyr::filter(year == next_yr, datetime == next_cutoff) |>
      dplyr::collect() |>
      data.table::as.data.table()
    
    if (nrow(bnd_next) > 0L) {
      bnd_next[, station := normalize_station(station)]
      bnd_next <- bnd_next[station %in% all_sta]
    }
    
    # Create a balanced station-hour grid for the year.
    all_hours <- seq(yr_start, yr_end, by = "hour")
    grid      <- data.table::CJ(station = all_sta, datetime = all_hours)
    
    # Keep all variables except keys and year; year is reassigned below.
    non_key <- setdiff(names(dt_yr), c("station", "datetime", "year"))
    
    dt_bal <- data.table::merge.data.table(
      grid,
      dt_yr[, c("station", "datetime", non_key), with = FALSE],
      by = c("station", "datetime"),
      all.x = TRUE
    )
    
    dt_bal[, year := yr]
    
    # Attach boundary rows for lag/lead computation.
    dt_bal <- data.table::rbindlist(
      list(dt_bal, bnd_prev, bnd_next),
      fill = TRUE,
      use.names = TRUE
    )
    
    # Ensure shift() uses the correct station-hour order.
    data.table::setorder(dt_bal, station, datetime)
    
    # Mark the target-year rows so the boundary rows can be dropped after
    # the flagging step, once they have served as lag/lead donors.
    in_yr <- dt_bal$datetime >= yr_start & dt_bal$datetime <= yr_end
    
    # Apply the outlier procedure pollutant by pollutant.
    for (pol in pollutants) {
      .flag_pollutant(
        dt         = dt_bal,
        pol        = pol,
        dist_dt    = dist_dt,
        pct_flag   = pct_flag,
        n_sd       = n_sd,
        miss_temp  = on_missing_temporal,
        miss_neigh = on_missing_neighbor,
        neigh_elig = neighbor_eligibility
      )
    }
    
    # Drop boundary rows before saving.
    dt_out <- dt_bal[in_yr]
    
    # Write partitioned output in the same year=YYYY structure.
    yr_dir <- file.path(out_path, paste0("year=", yr))
    dir.create(yr_dir, showWarnings = FALSE)
    
    arrow::write_parquet(
      dt_out,
      file.path(yr_dir, "data.parquet"),
      compression = "snappy"
    )
    
    # Explicit cleanup after each year helps with large city panels.
    rm(dt_yr, dt_bal, dt_out, grid, bnd_prev, bnd_next, in_yr)
    gc(verbose = FALSE)
  }
  
  invisible(out_path)
}
