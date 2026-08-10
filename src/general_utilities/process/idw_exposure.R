# ============================================================================================
# IDB: Air monitoring — inverse-distance-weighted exposure
# ============================================================================================
# @Goal: Functions for inverse-distance-weighted exposure.
#
# @Description: Interpolates hourly station readings to each geographic unit inside a buffer, 
#   then aggregates to annual exposure and WHO interim-target exceedance hours.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
# @Summary:
#   1. assign_socio_group
#   2. aggregate_idw_exposure
#   3. run_idw_city
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: assign_socio_group
#
# @Arg dt      : data.table; modified in place. Must contain a `geo_id` column.
# @Arg var     : string; continuous variable that defines the ranking.
# @Arg wcol    : string; population or expansion-weight column.
# @Arg n       : integer; number of equal-population groups.
# @Arg out_col : string; name of the group column to create.
#
# @Output : the same data.table, invisibly, with `out_col` added.
#
# @Details:
#   Assigns 1..n by cumulative weight share of `var`. Group 1 holds the lowest
#   values and the last group takes the residual share, which reproduces the old
#   hardcoded quintile cut when n = 5. Used at two levels: on individuals inside
#   aggregate_idw_exposure(), and on geographic units inside run_idw_city(). It
#   lives here rather than inside either one so that the two levels cannot drift
#   apart — they define the same socioeconomic groups and must cut them the same
#   way.
#
# @Written_on : July 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
assign_socio_group <- function(dt, var, wcol, n, out_col) {

  # Ascending sort so that group 1 corresponds to the lowest values.
  # The sort must be fully specified. Years of schooling is coarse, so a
  # group edge falls inside a large tie group; who lands in group k versus
  # k+1 would otherwise depend on how the census file happened to be
  # ordered. Value, then geographic unit, then original row order.
  dt[, .row_id := .I]
  data.table::setorderv(dt, c(var, "geo_id", ".row_id"))

  # Cumulative and total weight over rows with valid value and weight.
  dt[
    !is.na(get(var)) & !is.na(get(wcol)),
    `:=`(.cum_w = cumsum(get(wcol)), .tot_w = sum(get(wcol)))
  ]

  # Interior cut points k/n for k = 1..(n-1); left.open matches "<= edge".
  edges <- seq_len(n - 1L) / n

  dt[
    !is.na(.cum_w),
    (out_col) := pmin(
      findInterval(.cum_w / .tot_w, edges, left.open = TRUE) + 1L,
      n
    )
  ]

  dt[, c(".cum_w", ".tot_w", ".row_id") := NULL]
  invisible(dt)
}


# --------------------------------------------------------------------------------------------
# Function: aggregate_idw_exposure
#
# @Arg arrow_dir           : string; path to partitioned Arrow/Parquet hourly data.
# @Arg geo_sta_pq          : string; path to geo-station distance Parquet file.
# @Arg census_col          : data.frame; census data used for group assignment.
# @Arg geo_id_col          : string; geographic ID column in census_col.
# @Arg pop_col             : string; population or expansion-weight column.
# @Arg group_var           : string; continuous variable used to define groups
#                            (e.g. "escolaridad_avg" or "income").
# @Arg n_groups            : integer; number of equal-population groups (5 or 10).
# @Arg group_name          : string; output group column name
#                            (e.g. "edu_quintile" or "income_decile").
# @Arg quintile_level      : string; "geo" or "individual". Default "geo".
# @Arg indiv_adult_col     : string; adult filter column. Default "adult".
# @Arg buffer_km           : numeric; maximum geo-to-station distance. Default 3.
# @Arg distance_power      : numeric; IDW distance exponent. Default 1.
# @Arg target_years        : numeric vector or NULL; years to process.
# @Arg pollutants          : character vector; pollutant columns to aggregate.
# @Arg who_it              : named list; WHO interim target thresholds.
# @Arg mem_gb              : numeric; DuckDB memory ceiling in GB. Default 40.
# @Arg n_threads           : integer; DuckDB worker threads. Default 2.
# @Arg duckdb_temp_dir     : string or NULL; DuckDB spill directory.
# @Arg out_dir             : string; output directory.
# @Arg out_name            : string; output file prefix.
# @Arg overwrite           : logical; skip computation if outputs exist.
# @Arg quiet               : logical; suppress messages. Default FALSE.
# @Arg return_data         : logical; return data.tables in memory. Default FALSE.
# @Arg fail_on_query_error : logical; stop if a SQL query fails. Default TRUE.
# @Arg chunk_by_month      : logical; process each year-pollutant by month.
# @Arg edu_col             : string; deprecated alias for group_var, kept so old
#                            calls still run. Used only if group_var is NULL.
#
# @Output : Named list with saved file paths and, optionally, data.tables.
#
# @Details:
#   Aggregates hourly station pollution to geographic units using missingness-aware
#   inverse-distance weighting. For each geo-hour-pollutant cell, only stations
#   within buffer_km and with non-missing readings enter the numerator and
#   denominator. Socioeconomic groups are assigned generically: any continuous
#   group_var is cut into n_groups equal-population bins (group 1 = lowest), so the
#   same code path serves education quintiles and income deciles. One grouping is
#   produced per call (run separately for edu_quintile and income_decile).
#
# @Written_on : 02/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
aggregate_idw_exposure <- function(
    arrow_dir,
    geo_sta_pq,
    census_col,
    geo_id_col          = "GEO_ID",
    pop_col             = "n",
    group_var           = NULL,
    n_groups            = 5L,
    group_name          = "edu_quintile",
    quintile_level      = c("geo", "individual"),
    indiv_adult_col     = "adult",
    buffer_km           = 3,
    distance_power      = 1,
    target_years        = NULL,
    pollutants          = c("pm10", "pm25"),
    who_it              = list(
      pm10 = c(it1 = 150, it2 = 100, it3 = 75,  it4 = 50),
      pm25 = c(it1 = 75,  it2 = 50,  it3 = 37.5, it4 = 25)
    ),
    mem_gb              = 40,
    n_threads           = 2L,
    duckdb_temp_dir     = NULL,
    out_dir             = "data/interim/exposure",
    out_name,
    overwrite           = TRUE,
    quiet               = FALSE,
    return_data         = FALSE,
    fail_on_query_error = TRUE,
    chunk_by_month      = TRUE,
    edu_col             = "escolaridad_avg"
) {
  
  # 0. Dependencies and argument matching
  # -----------------------------------------------------------------------
  pkgs <- c("duckdb", "DBI", "arrow", "data.table", "dplyr", "stringi")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  # Check DuckDB version for out-of-core stability.
  if (utils::packageVersion("duckdb") < "0.9.2") {
    stop("DuckDB >= 0.9.2 required.")
  }
  
  quintile_level <- match.arg(quintile_level)
  
  # Resolve the grouping variable: prefer group_var, fall back to edu_col so
  # that older education-only calls keep working unchanged.
  if (is.null(group_var)) {
    group_var <- edu_col
  }
  
  n_groups <- as.integer(n_groups)
  
  if (is.na(n_groups) || n_groups < 2L) {
    stop("`n_groups` must be an integer >= 2.")
  }
  
  # Validate main inputs.
  if (!dir.exists(arrow_dir)) {
    stop("`arrow_dir` not found: ", arrow_dir)
  }
  
  if (!file.exists(geo_sta_pq)) {
    stop("`geo_sta_pq` not found: ", geo_sta_pq)
  }
  
  # Check census required columns.
  for (col in c(geo_id_col, pop_col, group_var)) {
    if (!col %in% names(census_col)) {
      stop("Column '", col, "' missing.")
    }
  }
  
  # Individual mode requires an adult indicator.
  if (quintile_level == "individual" &&
      !indiv_adult_col %in% names(census_col)) {
    stop("Column '", indiv_adult_col, "' not found for individual mode.")
  }
  
  # 1. Output paths and early exit
  # -----------------------------------------------------------------------
  out_path   <- file.path(out_dir, paste0(out_name, "_idw_exposure.parquet"))
  indiv_path <- file.path(out_dir, paste0(out_name, "_indiv_groups.parquet"))
  
  # Skip computation if all relevant outputs already exist.
  if (!overwrite) {
    geo_done   <- file.exists(out_path)
    indiv_done <- quintile_level == "geo" || file.exists(indiv_path)
    
    if (geo_done && indiv_done) {
      if (!quiet) {
        message("Outputs exist; skipping.")
      }
      
      out <- list(exposure_path = out_path)
      
      if (isTRUE(return_data)) {
        out$exposure_yearly <- data.table::as.data.table(
          arrow::read_parquet(out_path)
        )
      }
      
      if (quintile_level == "individual") {
        out$individual_path <- indiv_path
        
        if (isTRUE(return_data)) {
          out$individual_quintiles <- data.table::as.data.table(
            arrow::read_parquet(indiv_path)
          )
        }
      }
      
      return(invisible(out))
    }
  }
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # 2. Helpers
  # -----------------------------------------------------------------------
  .dq_path <- function(p) {
    paste0("'", gsub("'", "''", gsub("\\\\", "/", p)), "'")
  }
  
  # Normalize station identifiers consistently with the distance step.
  
  # Convert geographic IDs to strings without corrupting integer64 IDs.
  
  # Query helper: fail by default to avoid incomplete output files.
  .run_query <- function(con, query, context) {
    tryCatch(
      data.table::as.data.table(DBI::dbGetQuery(con, query)),
      error = function(e) {
        msg <- paste0("Query failed for ", context, ": ", e$message)
        
        if (isTRUE(fail_on_query_error)) {
          stop(msg, call. = FALSE)
        }
        
        warning(msg)
        NULL
      }
    )
  }
  
  # 3. DuckDB disk-backed connection
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[", out_name, "] Starting DuckDB engine ...")
  }
  
  # Temporary database; DuckDB may use this file during execution.
  dbdir <- tempfile("idw_duck_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  
  # Track whether the function created its own DuckDB spill directory.
  delete_duckdb_temp <- is.null(duckdb_temp_dir)
  duckdb_temp_root   <- NULL
  
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    try(unlink(dbdir, recursive = TRUE, force = TRUE), silent = TRUE)
    
    if (isTRUE(delete_duckdb_temp) && exists("duckdb_temp_dir")) {
      try(unlink(duckdb_temp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
    }
    
    if (isTRUE(delete_duckdb_temp) && exists("duckdb_temp_root")) {
      if (!is.null(duckdb_temp_root) && dir.exists(duckdb_temp_root)) {
        remaining_files <- list.files(
          duckdb_temp_root,
          all.files = TRUE,
          no.. = TRUE
        )
        
        if (length(remaining_files) == 0L) {
          try(unlink(duckdb_temp_root, recursive = TRUE, force = TRUE),
              silent = TRUE)
        }
      }
    }
  }, add = TRUE)
  
  # Configure DuckDB with conservative memory settings.
  n_threads <- as.integer(max(1L, n_threads))
  mem_gb    <- as.integer(mem_gb)
  
  DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", n_threads))
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", mem_gb))
  
  # Disable insertion-order preservation to reduce memory pressure.
  DBI::dbExecute(con, "SET preserve_insertion_order = false;")
  
  # Allow DuckDB to spill intermediate results to disk.
  if (is.null(duckdb_temp_dir)) {
    duckdb_temp_root <- file.path(out_dir, "_duckdb_tmp")
    duckdb_temp_dir  <- file.path(duckdb_temp_root, out_name)
  }
  
  dir.create(duckdb_temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  DBI::dbExecute(
    con,
    paste0("SET temp_directory = ", .dq_path(duckdb_temp_dir), ";")
  )

  # Install and load the ICU extension
  DBI::dbExecute(con, "INSTALL icu;")
  DBI::dbExecute(con, "LOAD icu;")

  # 4. Load and normalize distance matrix
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[", out_name, "] Loading and normalizing distances ...")
  }
  
  dist_dt <- data.table::as.data.table(arrow::read_parquet(geo_sta_pq))
  
  # Require the schema produced by compute_distance_matrices().
  req_dist_cols <- c("geo_id", "station_id", "distance_km")
  miss_dist_cols <- setdiff(req_dist_cols, names(dist_dt))
  
  if (length(miss_dist_cols) > 0L) {
    stop("Distance table is missing: ", paste(miss_dist_cols, collapse = ", "))
  }
  
  # Normalize join keys before registering the table in DuckDB.
  dist_dt[, geo_id := safe_chr(geo_id)]
  dist_dt[, station_id := normalize_station(station_id)]

  # Capture station_id, unique value for all stations, before the buffer filter below. 
  # The name-mismatch check in VI needs this or distant stations might have issues.
  matrix_stations <- unique(dist_dt$station_id)

  # Diagnose zero-distance pairs before dropping them from IDW denominators.
  n_zero_dist <- dist_dt[!is.na(distance_km) & distance_km == 0, .N]
  
  if (n_zero_dist > 0L && !quiet) {
    message(
      "[", out_name, "] Diagnostic: ", n_zero_dist,
      " geo-station pair(s) have distance_km == 0 and will be excluded."
    )
  }
  
  # Keep positive distances inside the requested buffer.
  dist_dt <- dist_dt[
    !is.na(distance_km) & distance_km > 0 & distance_km <= buffer_km
  ]
  
  if (nrow(dist_dt) == 0L) {
    stop("No geo-station pairs within ", buffer_km, " km.")
  }
  
  # Pre-compute inverse-distance weights in R.
  dist_dt[, inv_d := 1 / (distance_km ^ distance_power)]
  dist_dt <- dist_dt[, .(geo_id, station_id, inv_d)]
  
  # Register normalized distance table in DuckDB.
  DBI::dbWriteTable(con, "dist_tbl", dist_dt, overwrite = TRUE)
  
  # Validate matrix dimensions after filtering.
  n_geo <- DBI::dbGetQuery(
    con, "SELECT COUNT(DISTINCT geo_id) AS n FROM dist_tbl;"
  )$n
  
  n_sta <- DBI::dbGetQuery(
    con, "SELECT COUNT(DISTINCT station_id) AS n FROM dist_tbl;"
  )$n
  
  if (n_geo == 0L || n_sta == 0L) {
    stop("Distance table is empty after filtering.")
  }
  
  if (!quiet) {
    message(
      "[", out_name, "] Distance table: ", n_geo,
      " geo unit(s), ", n_sta, " station(s)."
    )
  }
  
  # Release R-side distance object after registering it in DuckDB.
  rm(dist_dt)
  gc(verbose = FALSE)
  
  # 5. Pollution view and station crosswalk
  # -----------------------------------------------------------------------
  poll_glob <- paste0(gsub("\\\\", "/", arrow_dir), "/**/*.parquet")
  
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS\n",
    "SELECT * FROM read_parquet(",
    .dq_path(poll_glob), ", hive_partitioning = true);"
  ))
  
  # Read distinct raw station names only; this is small relative to the data.
  station_xwalk <- data.table::as.data.table(
    DBI::dbGetQuery(
      con,
      "SELECT DISTINCT CAST(station AS VARCHAR) AS station_raw FROM pollution;"
    )
  )
  
  if (!"station_raw" %in% names(station_xwalk)) {
    stop("Column `station` not found in pollution dataset.")
  }
  
  # Normalize raw pollution station names in R, not inside DuckDB SQL.
  station_xwalk[, station_id := normalize_station(station_raw)]
  
  # Remove missing or empty station identifiers.
  station_xwalk <- station_xwalk[
    !is.na(station_raw) & !is.na(station_id) & station_id != ""
  ]
  
  DBI::dbWriteTable(con, "station_xwalk", station_xwalk, overwrite = TRUE)
  
  # Validate station overlap between pollution and distance matrix.
  station_overlap <- DBI::dbGetQuery(con, paste0(
    "SELECT\n",
    "  (SELECT COUNT(DISTINCT station_id) FROM station_xwalk) AS n_poll_sta,\n",
    "  (SELECT COUNT(DISTINCT station_id) FROM dist_tbl) AS n_dist_sta,\n",
    "  (SELECT COUNT(DISTINCT x.station_id)\n",
    "   FROM station_xwalk x\n",
    "   INNER JOIN dist_tbl d ON x.station_id = d.station_id) AS n_overlap;"
  ))
  
  if (!quiet) {
    message(
      "[", out_name, "] Station overlap: ",
      station_overlap$n_overlap, " of ", station_overlap$n_poll_sta,
      " pollution station(s) overlap distance matrix."
    )
  }
  
  if (station_overlap$n_overlap == 0L) {
    stop(
      "No station overlap between pollution data and distance matrix after ",
      "normalization. Check station names in `arrow_dir` and `geo_sta_pq`."
    )
  }
  
  rm(station_xwalk)
  gc(verbose = FALSE)
  
  # 6. Year list filtering
  # -----------------------------------------------------------------------
  avail_years <- sort(
    DBI::dbGetQuery(con, "SELECT DISTINCT year FROM pollution ORDER BY year;")$year
  )
  
  # Subset to requested target years if specified.
  years <- if (!is.null(target_years)) {
    intersect(avail_years, target_years)
  } else {
    avail_years
  }
  
  if (length(years) == 0L) {
    stop("No data found for requested target_years.")
  }
  
  if (!quiet) {
    message("[", out_name, "] Processing ", length(years), " year(s).")
  }

  # 6b. Station name-join check
  # -----------------------------------------------------------------------
  # A station absent in the distance matrix is dropped from every geo_unit, even if
  # it hourly data is not zero. Thus, we count the readings of stations, so the warning
  # separates real data loss from stations that simply never reported.
  check_polls <- intersect(pollutants, DBI::dbListFields(con, "pollution"))

  cnt_frag <- paste(sprintf(
    "SUM(CASE WHEN p.%s IS NOT NULL THEN 1 ELSE 0 END) AS n_%s",
    check_polls, check_polls
  ), collapse = ",\n       ")

  sta_counts <- data.table::as.data.table(DBI::dbGetQuery(con, paste0(
    "SELECT x.station_id,\n       ", cnt_frag, "\n",
    "FROM pollution p\n",
    "INNER JOIN station_xwalk x ON CAST(p.station AS VARCHAR) = x.station_raw\n",
    "WHERE p.year IN (", paste(years, collapse = ", "), ")\n",
    "GROUP BY x.station_id;"
  )))

  # Only unmatched stations carrying readings represent lost data; warn on those.
  sta_counts[, n_obs := rowSums(.SD), .SDcols = paste0("n_", check_polls)]
  orphan_poll <- sta_counts[!station_id %in% matrix_stations & n_obs > 0]

  if (nrow(orphan_poll) > 0L) {
    warning(
      "[", out_name, "] ", nrow(orphan_poll), " station(s) with readings are missing ",
      "from the distance matrix, so their data is dropped: ",
      paste(sprintf("%s (%d obs)", orphan_poll$station_id, orphan_poll$n_obs),
            collapse = "; "),
      call. = FALSE
    )
  }

  # The reverse direction is expected: many catalogued stations never report a pollutant.
  orphan_dist <- setdiff(matrix_stations, sta_counts$station_id)

  if (length(orphan_dist) > 0L && !quiet) {
    message("[", out_name, "] ", length(orphan_dist),
            " matrix station(s) have no rows in the hourly data (expected).")
  }

  # 7. Year x pollutant loop
  # -----------------------------------------------------------------------
  yearly_list <- vector("list", length(years))
  names(yearly_list) <- as.character(years)
  
  poll_fields <- DBI::dbListFields(con, "pollution")
  
  for (yr in years) {
    if (!quiet) {
      message("[", out_name, "] Year ", yr, " ...")
    }
    
    poll_results <- vector("list", length(pollutants))
    names(poll_results) <- pollutants
    
    for (poll in pollutants) {
      
      # Skip absent pollutants gracefully.
      if (!poll %in% poll_fields) {
        if (!quiet) {
          message("[", out_name, "] Pollutant absent, skipping: ", poll)
        }
        next
      }
      
      # Build WHO threshold columns for annual reconstruction.
      thr <- who_it[[poll]]
      who_cols <- character(0)
      who_frag <- ""
      
      if (!is.null(thr) && length(thr) > 0L) {
        who_cols <- paste0("hrs_d_", poll, "_", names(thr))
        
        who_frag <- paste(
          vapply(seq_along(thr), function(i) {
            sprintf(
              paste0(
                ",\n       SUM(CASE WHEN idw >= %s ",
                "THEN 1 ELSE 0 END) AS %s"
              ),
              thr[[i]],
              who_cols[[i]]
            )
          }, character(1)),
          collapse = ""
        )
      }
      
      # Use monthly chunks for memory-heavy city-buffer combinations.
      month_ids <- if (isTRUE(chunk_by_month)) seq_len(12L) else NA_integer_
      month_list <- vector("list", length(month_ids))
      
      for (i in seq_along(month_ids)) {
        mo <- month_ids[[i]]
        
        if (!quiet && isTRUE(chunk_by_month)) {
          message(
            "[", out_name, "] Year ", yr,
            ", ", poll, ", month ", sprintf("%02d", mo), " ..."
          )
        }
        
        # Add a month filter only in monthly mode.
        month_filter <- if (isTRUE(chunk_by_month)) {
          paste0("    AND EXTRACT(month FROM p.datetime) = ", mo, "\n")
        } else {
          ""
        }
        
        # Compute monthly or annual IDW summaries.
        query <- paste0(
          "WITH h AS (\n",
          "  SELECT x.station_id, p.datetime, p.", poll, " AS val\n",
          "  FROM pollution p\n",
          "  INNER JOIN station_xwalk x\n",
          "    ON CAST(p.station AS VARCHAR) = x.station_raw\n",
          "  WHERE p.year = ", yr, "\n",
          month_filter,
          "    AND p.", poll, " IS NOT NULL\n",
          "),\n",
          "hr_geo AS (\n",
          "  SELECT d.geo_id, h.datetime,\n",
          "         SUM(h.val * d.inv_d) / SUM(d.inv_d) AS idw\n",
          "  FROM h\n",
          "  INNER JOIN dist_tbl d ON h.station_id = d.station_id\n",
          "  GROUP BY d.geo_id, h.datetime\n",
          ")\n",
          "SELECT geo_id,\n",
          "       SUM(idw) AS sum_idw_", poll, ",\n",
          "       COUNT(*) AS total_hrs_", poll,
          who_frag, "\n",
          "FROM hr_geo\n",
          "GROUP BY geo_id;"
        )
        
        context <- if (isTRUE(chunk_by_month)) {
          paste0(poll, " in ", yr, ", month ", mo)
        } else {
          paste0(poll, " in ", yr)
        }
        
        res <- .run_query(con, query, context)
        
        if (!is.null(res) && nrow(res) > 0L) {
          month_list[[i]] <- res
        }
        
        rm(res)
        gc(verbose = FALSE)
      }
      
      # Combine monthly or annual chunks into one annual pollutant table.
      valid_chunks <- Filter(Negate(is.null), month_list)
      
      if (length(valid_chunks) == 0L) {
        next
      }
      
      chunk_dt <- data.table::rbindlist(valid_chunks, fill = TRUE)
      
      sum_col <- paste0("sum_idw_", poll)
      hrs_col <- paste0("total_hrs_", poll)
      avg_col <- paste0("avg_", poll)
      
      agg_cols <- c(sum_col, hrs_col, who_cols)
      agg_cols <- intersect(agg_cols, names(chunk_dt))
      
      annual_dt <- chunk_dt[
        ,
        lapply(.SD, sum, na.rm = TRUE),
        by = geo_id,
        .SDcols = agg_cols
      ]
      
      annual_dt[
        get(hrs_col) > 0,
        (avg_col) := get(sum_col) / get(hrs_col)
      ]
      
      annual_dt[, (sum_col) := NULL]
      poll_results[[poll]] <- annual_dt
      
      rm(month_list, valid_chunks, chunk_dt, annual_dt)
      gc(verbose = FALSE)
    }
    
    # Merge pollutant-specific exposure tables for the active year.
    valid <- Filter(Negate(is.null), poll_results)
    
    if (length(valid) == 0L) {
      next
    }
    
    yr_exp <- Reduce(
      function(a, b) merge(a, b, by = "geo_id", all = TRUE),
      valid
    )
    
    yr_exp[, year := yr]
    yearly_list[[as.character(yr)]] <- yr_exp
    
    rm(poll_results, valid, yr_exp)
    gc(verbose = FALSE)
  }
  
  # Stack yearly results.
  all_years <- data.table::rbindlist(
    Filter(Negate(is.null), yearly_list),
    fill = TRUE
  )
  
  if (nrow(all_years) == 0L) {
    stop("No exposure data produced.")
  }
  
  all_years[, geo_id := as.character(geo_id)]
  
  # 8. Census processing and group assignment
  # -----------------------------------------------------------------------
  census_dt <- data.table::copy(data.table::as.data.table(census_col))
  data.table::setnames(census_dt, geo_id_col, "geo_id")
  census_dt[, geo_id := safe_chr(geo_id)]
  
  if (quintile_level == "geo") {
    
    # Assign groups from the geo-level group_var using population shares.
    assign_socio_group(census_dt, group_var, pop_col, n_groups, group_name)

    # Repair spelling differences between the spatial and census ID conventions
    # before judging the match. Every repair is verified against the census.
    all_years[, geo_id := reconcile_geo_ids(
      geo_id, census_dt$geo_id, label = paste0("[", out_name, "]"), quiet = quiet
    )]

    # Report how many exposure units find a census row. Unmatched units keep
    # their exposure but carry a missing group, so a silent join here would
    # quietly shrink the estimation sample.
    n_unmatched <- length(setdiff(all_years$geo_id, census_dt$geo_id))

    if (!quiet) {
      message(
        "[", out_name, "] Census match: ",
        data.table::uniqueN(all_years$geo_id) - n_unmatched, " of ",
        data.table::uniqueN(all_years$geo_id),
        " exposure geo unit(s) matched (", n_unmatched, " unmatched)."
      )
    }

    result <- merge(all_years, census_dt, by = "geo_id", all.x = TRUE)
    arrow::write_parquet(result, out_path)
    
    out <- list(exposure_path = out_path)
    
    if (isTRUE(return_data)) {
      out$exposure_yearly <- result
    }
    
    return(invisible(out))
    
  } else {
    
    # Filter to adult individuals only.
    census_dt <- census_dt[get(indiv_adult_col) == 1]

    if (nrow(census_dt) == 0L) {
      stop("No adult rows after filtering.")
    }

    # Assign groups from the individual group_var using expansion weights.
    assign_socio_group(census_dt, group_var, pop_col, n_groups, group_name)

    # Repair spelling differences between the spatial and census ID conventions,
    # exactly as the "geo" branch above does.
    all_years[, geo_id := reconcile_geo_ids(
      geo_id, census_dt$geo_id, label = paste0("[", out_name, "]"), quiet = quiet
    )]

    # Report how many exposure units find a census row. Unmatched units keep
    # their exposure but have no population, so they drop out downstream.
    n_unmatched <- length(setdiff(all_years$geo_id, census_dt$geo_id))

    if (!quiet) {
      message(
        "[", out_name, "] Census match: ",
        data.table::uniqueN(all_years$geo_id) - n_unmatched, " of ",
        data.table::uniqueN(all_years$geo_id),
        " exposure geo unit(s) matched (", n_unmatched, " unmatched)."
      )
    }

    # Save datasets independently to avoid a huge year-individual matrix.
    arrow::write_parquet(all_years, out_path)
    arrow::write_parquet(census_dt, indiv_path)
    
    out <- list(
      exposure_path   = out_path,
      individual_path = indiv_path
    )
    
    if (isTRUE(return_data)) {
      out$exposure_yearly <- all_years
      out$individual_quintiles <- census_dt
    }
    
    return(invisible(out))
  }
}


# --------------------------------------------------------------------------------------------
# Function: run_idw_city
#
# @Arg city_label     : string; city name used in progress messages.
# @Arg city_id        : string; city identifier used in output folders and files.
# @Arg arrow_dir      : string; path to cleaned partitioned Arrow/Parquet data.
# @Arg distance_power : numeric; IDW distance exponent.
# @Arg geo_sta_pq     : string; path to geo-station distance Parquet file.
# @Arg geo_census     : data.frame; collapsed geographic-unit census data.
# @Arg micro_census   : data.frame; individual-level census microdata.
# @Arg geo_id_col     : string; geographic ID column in collapsed census data.
# @Arg geo_pop_col    : string; population column in collapsed census data.
# @Arg geo_group_var  : string; group variable in collapsed census data
#                       (e.g. "education_mean" or "income").
# @Arg micro_id_col   : string; geographic ID column in individual census data.
# @Arg micro_pop_col  : string; weight column in individual census data.
# @Arg micro_group_var: string; group variable in individual census data
#                       (e.g. "escolaridad" or "income").
# @Arg n_groups       : integer; number of equal-population groups (5 or 10).
# @Arg group_name     : string; output group column name
#                       (e.g. "edu_quintile" or "income_decile").
# @Arg buffer_km      : numeric; maximum geo-to-station distance.
# @Arg outdir_exp     : string; root output directory for IDW estimates.
# @Arg out_suffix     : string or NULL; extra tag in file names to separate
#                       groupings (e.g. "income"). NULL keeps the plain name.
# @Arg mem_gb         : numeric; DuckDB memory ceiling in GB. Default 40.
# @Arg n_threads      : integer; DuckDB worker threads. Default 2.
# @Arg overwrite      : logical; overwrite existing outputs. Default TRUE.
# @Arg return_data    : logical; return data objects in memory. Default FALSE.
#
# @Output : Named list with geo and individual output paths.
#
# @Details:
#   Computes the expensive IDW exposure table once using individual mode for the
#   requested grouping, then builds the geo-level exposure output by merging the
#   same exposure table with collapsed census and assigning geo-level groups.
#   One grouping is produced per call; call once for edu_quintile and once for
#   income_decile to obtain separate files.
#
# @Written_on : April 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
run_idw_city <- function(
    city_label,
    city_id,
    arrow_dir,
    distance_power,
    geo_sta_pq,
    geo_census,
    micro_census,
    geo_id_col,
    geo_pop_col,
    geo_group_var,
    micro_id_col,
    micro_pop_col,
    micro_group_var,
    n_groups       = 5L,
    group_name     = "edu_quintile",
    buffer_km,
    outdir_exp,
    out_suffix     = NULL,
    mem_gb      = 40,
    n_threads   = 8L,
    overwrite   = TRUE,
    return_data = FALSE
) {
  
  message("\n--- Processing ", city_label, " | ", buffer_km, " km | ",
          group_name, " ---")
  
  n_groups <- as.integer(n_groups)
  
  # Define output folder and common output prefix.
  city_out_dir <- here::here(outdir_exp, city_id)
  dir.create(city_out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Add an optional suffix so income and education outputs do not overwrite.
  out_base <- sprintf("%s_%dkm", city_id, buffer_km)
  
  if (!is.null(out_suffix) && nzchar(out_suffix)) {
    out_base <- paste0(out_base, "_", out_suffix)
  }
  
  # Compute IDW exposure once and save individual groups.
  exp_indiv <- aggregate_idw_exposure(
    arrow_dir      = arrow_dir,
    geo_sta_pq     = geo_sta_pq,
    census_col     = micro_census,
    geo_id_col     = micro_id_col,
    pop_col        = micro_pop_col,
    group_var      = micro_group_var,
    n_groups       = n_groups,
    group_name     = group_name,
    quintile_level = "individual",
    buffer_km      = buffer_km,
    distance_power = distance_power,
    mem_gb         = mem_gb,
    n_threads      = n_threads,
    out_dir        = city_out_dir,
    out_name       = out_base,
    overwrite      = overwrite,
    return_data    = FALSE
  )
  
  message(city_label, " exposure: ", exp_indiv$exposure_path)
  message(city_label, " individual groups: ", exp_indiv$individual_path)
  
  # Read the saved geo-level exposure table.
  exposure_dt <- data.table::as.data.table(
    arrow::read_parquet(exp_indiv$exposure_path)
  )
  
  # Prepare collapsed census data for geo-level groups.
  geo_dt <- data.table::copy(data.table::as.data.table(geo_census))
  
  data.table::setnames(geo_dt, geo_id_col, "geo_id")
  geo_dt[, geo_id := as.character(geo_id)]
  
  # Assign population-weighted groups to geographic units.
  assign_socio_group(geo_dt, geo_group_var, geo_pop_col, n_groups, group_name)

  # Repair spelling differences between the spatial and census ID conventions
  # before judging the match. Every repair is verified against the census.
  exposure_dt[, geo_id := reconcile_geo_ids(
    geo_id, geo_dt$geo_id, label = city_label
  )]

  # Merge exposure with collapsed census. Report the match rate.
  n_exp   <- data.table::uniqueN(exposure_dt$geo_id)
  n_miss  <- length(setdiff(exposure_dt$geo_id, geo_dt$geo_id))

  message(city_label, " census match: ", n_exp - n_miss, " of ", n_exp,
          " exposure geo unit(s) matched (", n_miss, " unmatched).")

  geo_result <- merge(exposure_dt, geo_dt, by = "geo_id", all.x = TRUE)
  
  geo_path <- file.path(
    city_out_dir,
    paste0(out_base, "_geo_idw_exposure.parquet")
  )
  
  arrow::write_parquet(geo_result, geo_path)
  message(city_label, " geo exposure: ", geo_path)
  
  # Return paths only by default to avoid retaining large objects.
  if (!isTRUE(return_data)) {
    rm(exposure_dt, geo_dt, geo_result)
    gc(verbose = FALSE)
    
    return(invisible(list(
      geo = list(exposure_path = geo_path),
      individual = exp_indiv
    )))
  }
  
  invisible(list(
    geo = list(
      exposure_yearly = geo_result,
      exposure_path = geo_path
    ),
    individual = exp_indiv
  ))
}
