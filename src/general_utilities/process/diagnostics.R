# ============================================================================================
# IDB: Air monitoring — coverage and exceedance diagnostics
# ============================================================================================
# @Goal: Functions for coverage and exceedance diagnostics.
#
# @Description: Counts reporting stations, WHO exceedances and missingness. These feed the paper's
#   descriptive tables rather than the exposure estimates.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
# @Summary:
#   1. summarize_stations_by_pollutant
#   2. compute_who_exceedances
#   3. compute_missing_proportions
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: summarize_stations_by_pollutant
#
# @Arg arrow_dir     : string; path to the Arrow pollution dataset.
# @Arg city_label    : string; city identifier.
# @Arg pollutants    : character; default c("pm10", "pm25", "o3", "no2", "co").
# @Arg year_filter   : integer|NULL; restrict to one year. Default NULL.
# @Arg min_valid_pct : numeric [0,1]; minimum share of expected hours per year.
# @Arg mem_gb        : numeric; DuckDB memory ceiling in GB. Default 4.
# @Arg quiet         : logical; suppress messages. Default FALSE.
#
# @Output : Named list ($wide and $long) of data.tables.
#
# @Details:
#   Implements Algebraic Balancing: Instead of physically imputing missing rows 
#   to fix implicit missingness, the SQL dynamically calculates the exact number 
#   of expected hours in the year (accounting for leap years) as the denominator.
#   This guarantees mathematically perfect coverage percentages at near-zero 
#   computational cost. Matches legacy behavior perfectly at min_valid_pct = 0.0.
#
# @Written_on : 17/04/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
summarize_stations_by_pollutant <- function(
    arrow_dir,
    city_label,
    pollutants    = c("pm10", "pm25", "o3", "no2", "co"),
    year_filter   = NULL,
    min_valid_pct = 0.0,
    mem_gb        = 32,
    quiet         = FALSE
) {
  # Check required packages before initiating database connections
  pkgs <- c("duckdb", "DBI", "data.table")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  # Validate inputs to prevent downstream SQL failures
  stopifnot(
    dir.exists(arrow_dir), nzchar(city_label),
    is.numeric(min_valid_pct), min_valid_pct >= 0, min_valid_pct <= 1
  )
  
  # Initialize DuckDB connection with strict memory limits
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), 
    add = TRUE
  )
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  
  # Create a virtual view over the partitioned Parquet files
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(", 
    glob_q, ", hive_partitioning = true);"
  ))
  
  # Extract available columns to dynamically filter requested pollutants
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present  <- tolower(col_info$name)
  pollutants <- intersect(tolower(pollutants), present)
  
  if (length(pollutants) == 0L) {
    stop("None of the requested pollutants are present in the dataset.")
  }
  
  # Build optional year filter for the SQL query
  yr_filter_sql <- if (is.null(year_filter)) {
    "" 
  } else {
    sprintf("WHERE EXTRACT(year FROM datetime) = %d", as.integer(year_filter))
  }
  
  res <- vector("list", length(pollutants))
  
  # Iterate over each valid pollutant to calculate balanced completeness
  for (i in seq_along(pollutants)) {
    poll <- pollutants[[i]]
    
    # ALGEBRAIC BALANCING LOGIC:
    # date_diff() calculates exact expected hours for that specific year.
    # This prevents implicit missingness from inflating valid_pct.
    q <- sprintf(
      "WITH per_sy AS (
         SELECT station,
                CAST(EXTRACT(year FROM datetime) AS INTEGER) AS yr,
                COUNT(%s) AS valid_n
         FROM pollution %s
         GROUP BY station, EXTRACT(year FROM datetime)
       )
       SELECT yr AS year,
              COUNT(DISTINCT station) AS n_stations,
              STRING_AGG(DISTINCT station, ';' ORDER BY station) AS stations
       FROM per_sy
       WHERE (valid_n * 1.0 / date_diff('hour', 
                                        make_date(yr, 1, 1), 
                                        make_date(yr + 1, 1, 1))) >= %f
       GROUP BY yr
       ORDER BY yr;",
      poll, yr_filter_sql, min_valid_pct
    )
    
    # Execute query and convert to data.table
    r <- data.table::as.data.table(DBI::dbGetQuery(con, q))
    
    if (nrow(r) == 0L) next
    
    # Append tracking metadata
    r[, `:=`(city = city_label, pollutant = poll)]
    res[[i]] <- r
  }
  
  # Combine results for all pollutants
  long <- data.table::rbindlist(Filter(Negate(is.null), res), fill = TRUE)
  
  # Handle empty returns gracefully
  if (nrow(long) == 0L) {
    if (!quiet) {
      message("[stations] ", city_label, ": no rows passed the filter.")
    }
    return(invisible(list(wide = data.table::data.table(), long = long)))
  }
  
  # Enforce standard column ordering for the long table
  data.table::setcolorder(
    long, 
    c("city", "year", "pollutant", "n_stations", "stations")
  )
  
  # Pivot to wide format for easy latex table generation
  wide <- data.table::dcast(
    long, city + year ~ pollutant, 
    value.var = "n_stations", fill = 0L
  )
  
  if (!quiet) message(
    "[stations] ", city_label, ": ", 
    data.table::uniqueN(long$year), " year(s), ", 
    length(pollutants), " pollutant(s)"
  )
  
  invisible(list(wide = wide, long = long))
}


# --------------------------------------------------------------------------------------------
# Function: compute_who_exceedances
#
# @Arg arrow_dir    : string; path to the cleaned Arrow pollution dataset.
# @Arg city_label   : string; city identifier added as a column in the output.
# @Arg pollutants   : character; default c("pm10","pm25").
# @Arg year_filter  : integer|NULL; restrict to a single year. Default NULL.
# @Arg who_annual   : named numeric; WHO AQG annual averages in μg/m³. 
# @Arg station_aggr : string; "mean" or "median". 
# @Arg legacy_mode  : logical; if TRUE, uses a pooled grand mean (duration-weighted).
#                     if FALSE, uses mean-of-means (spatial-weighted). Default FALSE.
# @Arg mem_gb       : numeric; DuckDB memory ceiling in GB. Default 12.
# @Arg quiet        : logical; suppress info messages. Default FALSE.
#
# @Output : data.table with city, year, pollutant, city_avg, who_aqg, 
#           exceedance_factor, n_stations, n_valid_hrs.
#
# @Written_on : 17/04/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_who_exceedances <- function(
    arrow_dir,
    city_label,
    pollutants   = c("pm10", "pm25"),
    year_filter  = NULL,
    who_annual   = c(pm10 = 15, pm25 = 5),
    station_aggr = c("mean", "median"),
    legacy_mode  = FALSE,
    mem_gb       = 12,
    quiet        = FALSE
) {
  # 1. Validate dependencies
  # ----------------------------------------------------------------------------------
  pkgs <- c("duckdb", "DBI", "data.table")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  station_aggr <- match.arg(station_aggr)
  stopifnot(dir.exists(arrow_dir), nzchar(city_label))
  
  # 2. Database Initialization
  # ----------------------------------------------------------------------------------
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown=TRUE), silent=TRUE), add=TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  
  # Install and load ICU for safe datetime extraction
  DBI::dbExecute(con, "INSTALL icu;")
  DBI::dbExecute(con, "LOAD icu;")
  
  # 3. Mount Parquet Dataset
  # ----------------------------------------------------------------------------------
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(",
    glob_q, ", hive_partitioning = true);"
  ))
  
  # 4. Schema Validation
  # ----------------------------------------------------------------------------------
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present  <- tolower(col_info$name)
  pollutants <- intersect(tolower(pollutants), present)
  
  if (length(pollutants) == 0L) stop("Requested pollutants are missing.")
  
  # 5. Query Preparation
  # ----------------------------------------------------------------------------------
  yr_filter_sql <- if (is.null(year_filter)) {
    ""
  } else {
    sprintf("AND EXTRACT(year FROM datetime) = %d", as.integer(year_filter))
  }
  
  collapse_fun <- if (station_aggr == "median") "MEDIAN" else "AVG"
  out_rows <- vector("list", length(pollutants))
  
  # 6. Execute Aggregation Loop
  # ----------------------------------------------------------------------------------
  for (i in seq_along(pollutants)) {
    poll <- pollutants[[i]]
    
    if (legacy_mode) {
      # -----------------------------------------------------------------------
      # LEGACY MODE (Pooled Grand Mean):
      # Pools all hourly observations together. Heavily biased towards stations 
      # with longer sensor uptimes. Replicates the older methodology exactly.
      # -----------------------------------------------------------------------
      q <- sprintf(
        "SELECT EXTRACT(year FROM datetime) AS year,
                %s(%s) AS city_avg,
                COUNT(DISTINCT station) AS n_stations,
                COUNT(%s) AS n_valid_hrs
         FROM pollution
         WHERE %s IS NOT NULL %s
         GROUP BY EXTRACT(year FROM datetime)
         ORDER BY year;",
        collapse_fun, poll, poll, poll, yr_filter_sql
      )
    } else {
      # -----------------------------------------------------------------------
      # UNBIASED MODE (Spatial Mean-of-Means):
      # CTE (station_year) calculates the standardized mean per physical station.
      # Main SELECT collapses these spatial means to prevent uptime bias.
      # -----------------------------------------------------------------------
      q <- sprintf(
        "WITH station_year AS (
           SELECT station,
                  EXTRACT(year FROM datetime) AS year,
                  AVG(%s) AS station_avg,
                  COUNT(%s) AS n_valid
           FROM pollution
           WHERE %s IS NOT NULL %s
           GROUP BY station, EXTRACT(year FROM datetime)
         )
         SELECT year,
                %s(station_avg)  AS city_avg,
                COUNT(DISTINCT station) AS n_stations,
                SUM(n_valid)     AS n_valid_hrs
         FROM station_year
         GROUP BY year
         ORDER BY year;",
        poll, poll, poll, yr_filter_sql, collapse_fun
      )
    }
    
    # Execute the query
    r <- data.table::as.data.table(DBI::dbGetQuery(con, q))
    if (nrow(r) == 0L) next
    
    # 7. Post-Processing
    # --------------------------------------------------------------------------------
    r[, `:=`(
      city      = city_label,
      pollutant = poll,
      who_aqg   = unname(who_annual[poll])
    )]
    
    r[, exceedance_factor := city_avg / who_aqg]
    
    data.table::setcolorder(r, c(
      "city", "year", "pollutant", "city_avg",
      "who_aqg", "exceedance_factor", "n_stations", "n_valid_hrs"
    ))
    
    out_rows[[i]] <- r
  }
  
  # 8. Finalize Output
  # ----------------------------------------------------------------------------------
  res <- data.table::rbindlist(Filter(Negate(is.null), out_rows), fill = TRUE)
  
  if (!quiet) {
    message(
      "[exceedances] ", city_label,
      " (Legacy: ", legacy_mode, "): ", 
      nrow(res), " city-year-pollutant rows"
    )
  }
  
  return(res)
}


# --------------------------------------------------------------------------------------------
# Function: compute_missing_proportions
#
# @Arg arrow_dir   : string; path to partitioned Arrow/Parquet pollution dataset.
# @Arg pollutants  : character; default c("pm10","pm25","o3","no2","co").
# @Arg dims        : character; dimensions to aggregate by. Valid values:
#                    "station", "month", "hour", "day_of_week", "year".
# @Arg year_filter : integer|NULL; restrict to a single year. Default NULL.
# @Arg out_dir     : string|NULL; directory to write Parquet files.
# @Arg out_name    : string; file prefix. Required if out_dir is provided.
# @Arg mem_gb      : numeric; DuckDB memory ceiling in GB. Default 8.
# @Arg quiet       : logical; suppress info messages. Default FALSE.
#
# @Output : Named list of data.tables.
# @Details: Calculates structural and algorithmic missingness cleanly inside DuckDB.
#           Avoids RAM bottlenecks by resolving all math before pulling to R.
#
# @Written_on : 17/04/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_missing_proportions <- function(
    arrow_dir,
    pollutants  = c("pm10", "pm25", "o3", "no2", "co"),
    dims        = c("station", "month", "hour"),
    year_filter = NULL,
    out_dir     = NULL,
    out_name    = NULL,
    mem_gb      = 8,
    quiet       = FALSE
) {
  # 1. Dependency and Input Validation
  # ----------------------------------------------------------------------------------
  pkgs <- c("duckdb", "DBI", "arrow", "data.table")
  for (p in pkgs) {
  }
  
  if (!dir.exists(arrow_dir)) stop("`arrow_dir` not found: ", arrow_dir)
  
  valid_dims <- c("station", "month", "hour", "day_of_week", "year")
  dims <- intersect(dims, valid_dims)
  
  if (length(dims) == 0L) {
    stop("No valid dims. Use: ", paste(valid_dims, collapse = ", "))
  }
  
  if (!is.null(out_dir)) {
    if (is.null(out_name) || !nzchar(out_name)) {
      stop("`out_name` is required when `out_dir` is provided.")
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # 2. Database Initialization
  # ----------------------------------------------------------------------------------
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  
  # Install ICU for safe datetime extraction (consistent with pipeline standards)
  DBI::dbExecute(con, "INSTALL icu;")
  DBI::dbExecute(con, "LOAD icu;")
  
  # Mount the Parquet dataset as a virtual table
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(",
    glob_q, ", hive_partitioning = true);"
  ))
  
  # 3. Schema Validation
  # ----------------------------------------------------------------------------------
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present_cols <- tolower(col_info$name)
  pollutants <- intersect(tolower(pollutants), present_cols)
  
  if (length(pollutants) == 0L) {
    stop("None of the requested pollutants are present in the dataset.")
  }
  
  # 4. Query Preparation
  # ----------------------------------------------------------------------------------
  # Map requested dimensions to their exact SQL extraction syntax
  dim_expr <- list(
    station     = "station",
    year        = "EXTRACT(year FROM datetime)",
    month       = "EXTRACT(month FROM datetime)",
    hour        = "EXTRACT(hour FROM datetime)",
    day_of_week = "EXTRACT(isodow FROM datetime)"
  )
  
  yr_filter_sql <- if (is.null(year_filter)) {
    ""
  } else {
    sprintf("WHERE EXTRACT(year FROM datetime) = %d", as.integer(year_filter))
  }
  
  out <- list()
  
  # 5. Execute Aggregation Loop
  # ----------------------------------------------------------------------------------
  for (d in dims) {
    if (!quiet) message("[missing] Aggregating dimension: ", d)
    
    # Build dynamic SQL columns to calculate NA ratios for all pollutants at once
    cols_sql <- paste(
      vapply(pollutants, function(p) {
        paste0(
          "100.0 * SUM(CASE WHEN ", p, " IS NULL THEN 1 ELSE 0 END) / ",
          "COUNT(*) AS ", p, "_missing_pct, ",
          "COUNT(*) AS ", p, "_total_hrs"
        )
      }, character(1)),
      collapse = ", "
    )
    
    # Construct the final aggregation query
    q <- sprintf(
      "SELECT %s AS %s, %s \nFROM pollution \n%s \nGROUP BY %s \nORDER BY %s;",
      dim_expr[[d]], d, cols_sql, yr_filter_sql, dim_expr[[d]], dim_expr[[d]]
    )
    
    # Execute query and pull into a data.table
    res <- data.table::as.data.table(DBI::dbGetQuery(con, q))
    
    # 6. Post-Processing
    # --------------------------------------------------------------------------------
    # Since each pollutant generates a 'total_hrs' column, we collapse duplicates
    dup_totals <- grep("_total_hrs$", names(res), value = TRUE)
    
    if (length(dup_totals) > 1L) {
      keep <- dup_totals[1L]
      res[, (setdiff(dup_totals, keep)) := NULL]
      data.table::setnames(res, keep, "total_hrs")
      
    } else if (length(dup_totals) == 1L) {
      data.table::setnames(res, dup_totals, "total_hrs")
    }
    
    out[[d]] <- res
    
    # 7. File Export
    # --------------------------------------------------------------------------------
    if (!is.null(out_dir)) {
      pth <- file.path(out_dir, paste0(out_name, "_missing_by_", d, ".parquet"))
      arrow::write_parquet(res, pth)
      if (!quiet) message("  -> Wrote: ", pth)
    }
  }
  
  invisible(out)
}
