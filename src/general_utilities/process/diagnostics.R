# ============================================================================================
# IDB: Air monitoring — coverage and exceedance diagnostics
# ============================================================================================
# @Goal: Functions for coverage and exceedance diagnostics.
#
# @Description: Counts reporting stations, WHO exceedances and missingness. These feed the
# paper's
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


# --------------------------------------------------------------------------------------------
# Function: compute_city_census_summary
#
# @Arg census_path : string; path to a city's collapsed census file.
# @Arg city        : string; display name for the output row.
# @Arg city_latex  : string; the same name, LaTeX-escaped for the table.
# @Arg census_year : integer; census vintage.
# @Arg census_level: string; the geographic level, e.g. "Municipality".
# @Arg geo_id_col  : string; geographic identifier column in the census file.
# @Arg pop_col     : string; population weight column.
#
# @Output : one-row data.table with population, unit count and mean population per unit.
#
# @Details:
#   Arguments are named rather than taken as a spec row, so the signature documents what
#   the
#   function needs. Units with a missing id, missing weight or non-positive weight are
#   dropped
#   before counting, which is what makes n_census_geographic_units the estimation-relevant
#   count rather than the file's row count.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
compute_city_census_summary <- function(census_path, city, city_latex, census_year,
                                        census_level, geo_id_col, pop_col) {
  if (!file.exists(census_path)) {
    stop("Census file not found: ", census_path)
  }

  dt <- data.table::fread(census_path)

  missing_cols <- setdiff(c(geo_id_col, pop_col), names(dt))

  if (length(missing_cols) > 0L) {
    stop("Missing column(s) in ", city, ": ", paste(missing_cols, collapse = ", "))
  }

  dt <- dt[!is.na(get(geo_id_col)) & !is.na(get(pop_col)) & get(pop_col) > 0]

  total_population <- sum(dt[[pop_col]], na.rm = TRUE)
  n_geo_units      <- data.table::uniqueN(dt[[geo_id_col]])

  data.table::data.table(
    city                        = city,
    city_latex                  = city_latex,
    year                        = census_year,
    total_population            = total_population,
    census_geographic_level     = census_level,
    n_census_geographic_units   = n_geo_units,
    average_population_per_unit = total_population / n_geo_units
  )
}


# --------------------------------------------------------------------------------------------
# Function: count_stations_reporting
#
# @Arg arrow_dir   : string; path to the city's hive-partitioned Arrow pollution dataset.
# @Arg pollutants  : character; pollutants to count. Default c("pm10", "pm25").
# @Arg year_filter : integer; year to restrict to. Default 2023.
# @Arg mem_gb      : numeric; DuckDB memory ceiling in GB. Default 8.
#
# @Output : one-row data.table with one integer column per pollutant.
#
# @Details:
#   Counts distinct stations reporting at least one non-missing value. This is a different
#   question from summarize_stations_by_pollutant(), which counts stations meeting a
#   minimum
#   coverage share of the year; a station reporting one hour counts here and not there.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
count_stations_reporting <- function(arrow_dir,
                                         pollutants = c("pm10", "pm25"),
                                         year_filter = 2023L,
                                         mem_gb = 8) {
  if (!dir.exists(arrow_dir)) {
    stop("`arrow_dir` not found: ", arrow_dir)
  }
  
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  DBI::dbExecute(con, "INSTALL icu;")
  DBI::dbExecute(con, "LOAD icu;")
  
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(",
    glob_q, ", hive_partitioning = true);"
  ))
  
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present_cols <- tolower(col_info$name)
  pollutants <- intersect(tolower(pollutants), present_cols)
  
  if (length(pollutants) == 0L) {
    stop("None of the requested pollutants are present in: ", arrow_dir)
  }
  
  count_sql <- paste(
    vapply(pollutants, function(p) {
      paste0(
        "COUNT(DISTINCT CASE WHEN ", p,
        " IS NOT NULL THEN station END) AS ", p
      )
    }, character(1)),
    collapse = ", "
  )
  
  q <- sprintf(
    paste0(
      "SELECT %s ",
      "FROM pollution ",
      "WHERE EXTRACT(year FROM datetime) = %d;"
    ),
    count_sql,
    as.integer(year_filter)
  )
  
  res <- data.table::as.data.table(DBI::dbGetQuery(con, q))
  
  for (p in c("pm10", "pm25")) {
    if (!p %in% names(res)) {
      res[, (p) := NA_integer_]
    }
  }
  
  res[, .(pm10 = as.integer(pm10), pm25 = as.integer(pm25))]
}


# --------------------------------------------------------------------------------------------
# Function: station_education_quintile
#
# @Arg dist_pq     : string; path to the geo-to-station distance matrix.
# @Arg census_file : string; path to the city's individual census file.
# @Arg geo_id_col  : string; geographic identifier column in the census.
#
# @Output : data.table mapping each station to the education quintile of its nearest unit.
#
# @Details:
#   Collapses the census to geographic units, ranks them by population-weighted mean years
#   of
#   schooling, cuts into five equal-count bins, then assigns each station the quintile of
#   the
#   unit whose representative point is nearest to it. Nearest unit, not units within a
#   buffer:
#   this asks which population a station sits among, not which population it measures.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
station_education_quintile <- function(dist_pq, census_file, geo_id_col) {
  dist <- data.table::as.data.table(arrow::read_parquet(dist_pq))
  census <- data.table::fread(census_file)
  
  station_col <- find_col(
    dist,
    c("station", "station_id", "id_station", "station_code", "codigo_estacion"),
    dist_pq
  )
  
  dist_geo_col <- find_col(
    dist,
    c("geo_id", geo_id_col, "GEO_ID", "CVE_MUN", "CUT", "code_weighting"),
    dist_pq
  )
  
  dist_col <- find_col(
    dist,
    c("distance_km", "dist_km", "distance", "dist"),
    dist_pq
  )
  
  census_geo_col <- find_col(
    census,
    c(geo_id_col, "geo_id", "GEO_ID", "CVE_MUN", "CUT", "code_weighting"),
    census_file
  )
  
  edu_col <- find_col(
    census,
    c("education_mean", "escolaridad", "educ_years", "years_schooling"),
    census_file
  )
  
  weight_col <- find_col(
    census,
    c("weight", "weights", "fe", "factor_expansion", "n", "FACTOR"),
    census_file
  )
  
  data.table::setnames(dist, station_col, "station")
  data.table::setnames(dist, dist_geo_col, "geo_id")
  data.table::setnames(census, census_geo_col, "geo_id")
  data.table::setnames(census, edu_col, "education")
  data.table::setnames(census, weight_col, "weight")
  
  dist[, station := as.character(station)]
  dist[, geo_id := as.character(geo_id)]
  census[, geo_id := as.character(geo_id)]
  
  census <- census[
    !is.na(geo_id) &
      !is.na(education) &
      !is.na(weight) &
      weight > 0
  ]
  
  census_geo <- census[
    ,
    .(
      education_mean = stats::weighted.mean(
        education,
        weight,
        na.rm = TRUE
      ),
      population = sum(weight, na.rm = TRUE)
    ),
    by = geo_id
  ]
  
  census_geo <- census_geo[
    !is.na(education_mean) &
      !is.na(population) &
      population > 0
  ]
  
  census_geo[, rank_edu := data.table::frank(
    education_mean,
    ties.method = "average"
  )]
  
  census_geo[, edu_quintile := ceiling(5 * rank_edu / .N)]
  census_geo[edu_quintile < 1, edu_quintile := 1L]
  census_geo[edu_quintile > 5, edu_quintile := 5L]
  
  nearest <- dist[
    !is.na(station) &
      !is.na(geo_id) &
      !is.na(get(dist_col))
  ]
  
  nearest <- nearest[
    order(get(dist_col)),
    .SD[1L],
    by = station
  ]
  
  nearest <- nearest[, .(station, geo_id)]
  
  census_q <- census_geo[
    edu_quintile %in% 1:5,
    .(geo_id, quintile = edu_quintile)
  ]
  
  out <- merge(
    nearest,
    census_q,
    by = "geo_id",
    all.x = FALSE,
    all.y = FALSE
  )
  
  unique(out[quintile %in% 1:5, .(station, quintile)])
}


# --------------------------------------------------------------------------------------------
# Function: compute_missing_by_quintile
#
# @Arg city          : string; display name for the output row.
# @Arg city_order    : integer; row order in the final table.
# @Arg pollution_dir : string; hive-partitioned Arrow dataset of cleaned hourly readings.
# @Arg dist_pq       : string; geo-to-station distance matrix.
# @Arg census_file   : string; individual census file for the city.
# @Arg geo_id_col    : string; geographic identifier column in the census.
# @Arg pollutants    : character; pollutants to report.
# @Arg year          : integer; year to restrict to.
# @Arg report        : string; "available" or "missing" shares.
# @Arg mem_gb        : numeric; DuckDB memory ceiling in GB. Default 8.
#
# @Output : data.table; one row per pollutant with the share by education quintile Q1..Q5.
#
# @Details:
#   Answers whether monitoring coverage itself is unequal: if stations in poorer quintiles
#   report a smaller share of their expected hours, the exposure estimates are less
#   reliable
#   exactly where the paper's question bites. Arguments are named rather than taken as a
#   spec
#   row, so the signature documents what the function needs.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
compute_missing_by_quintile <- function(city, city_order, pollution_dir, dist_pq,
                                        census_file, geo_id_col, pollutants, year,
                                        report, mem_gb = 8) {
  if (!dir.exists(pollution_dir)) {
    stop("Pollution directory not found for ", city, ": ",
         pollution_dir)
  }
  
  station_q <- station_education_quintile(
    dist_pq = dist_pq,
    census_file = census_file,
    geo_id_col = geo_id_col
  )
  
  if (nrow(station_q) == 0L) {
    stop("No station-quintile matches for ", city, ".")
  }
  
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  DBI::dbWriteTable(con, "station_q", station_q, overwrite = TRUE)
  
  glob <- paste0("'", gsub("\\\\", "/", pollution_dir), "/**/*.parquet'")
  
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution_raw AS SELECT * FROM read_parquet(",
    glob, ", hive_partitioning = true);"
  ))
  
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution_raw');")$name
  cols_lower <- tolower(cols)
  
  station_col <- cols[match(
    TRUE,
    cols_lower %in% c(
      "station",
      "station_id",
      "id_station",
      "station_code",
      "codigo_estacion"
    )
  )]
  
  if (is.na(station_col)) {
    stop("No station column found for ", city, ".")
  }
  
  datetime_col <- cols[match(TRUE, cols_lower == "datetime")]
  if (is.na(datetime_col)) {
    stop("No datetime column found for ", city, ".")
  }
  
  present_pollutants <- pollutants[pollutants %in% cols_lower]
  if (length(present_pollutants) == 0L) {
    stop("No PM columns found for ", city, ".")
  }
  
  pollutant_select <- paste(
    vapply(present_pollutants, function(p) {
      original <- cols[match(p, cols_lower)]
      paste0('"', original, '" AS ', p)
    }, character(1)),
    collapse = ", "
  )
  
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT ",
    "CAST(\"", station_col, "\" AS VARCHAR) AS station, ",
    "\"", datetime_col, "\" AS datetime, ",
    pollutant_select, " ",
    "FROM pollution_raw;"
  ))
  
  sql_cols <- paste(
    vapply(present_pollutants, function(p) {
      paste0(
        "SUM(CASE WHEN p.", p, " IS NULL THEN 1 ELSE 0 END) AS ", p, "_miss, ",
        "SUM(CASE WHEN p.", p, " IS NOT NULL THEN 1 ELSE 0 END) AS ", p, "_obs"
      )
    }, character(1)),
    collapse = ", "
  )
  
  q <- sprintf(
    paste0(
      "SELECT sq.quintile, COUNT(*) AS total_obs, ", sql_cols, " ",
      "FROM pollution p ",
      "INNER JOIN station_q sq ON p.station = sq.station ",
      "WHERE EXTRACT(year FROM p.datetime) = %d ",
      "GROUP BY sq.quintile ",
      "ORDER BY sq.quintile;"
    ),
    as.integer(year)
  )
  
  wide <- data.table::as.data.table(DBI::dbGetQuery(con, q))
  
  out <- data.table::rbindlist(lapply(present_pollutants, function(p) {
    num <- if (report == "missing") paste0(p, "_miss") else paste0(p, "_obs")
    
    wide[, .(
      pollutant = p,
      quintile = quintile,
      value = get(num) / total_obs,
      total_obs = total_obs
    )]
  }))
  
  out[, `:=`(
    city = city,
    city_order = city_order,
    year = year,
    statistic = report
  )]
  
  message("[missing by quintile] ", city, " done.")
  out[]
}
