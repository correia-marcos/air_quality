# --------------------------------------------------------------------------------
# Script: compute_missing_by_education_quintile.R
#
# Purpose:
#   Compute PM10 and PM2.5 availability by education quintile for each city.
#   The output is a LaTeX table with rows for pollutant-city pairs and columns
#   Q1--Q5.
#
# Main idea:
#   1. Assign each monitoring station to the education quintile of its nearest
#      census geographic unit, using the distance matrix.
#   2. Join station-hour pollution data to station quintiles.
#   3. Compute non-missing shares by pollutant, city, and quintile.
#
# Output:
#   results/tables/missing_proportions/missing_by_education_quintile_2023.tex
#
# Written_by: Marcos Paulo Rodrigues Correia
# Updated_on: June 2026
# --------------------------------------------------------------------------------

# 0. Setup
# --------------------------------------------------------------------------------
rm(list = ls())

pkgs <- c("arrow", "data.table", "duckdb", "DBI")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("Package '", p, "' required but not installed.")
  }
}

root_dir <- getwd()

analysis_year <- 2023L
pollutants <- c("pm10", "pm25")
report <- "available"  # Use "missing" for missing shares.

out_dir <- file.path(root_dir, "results", "tables", "missing_proportions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. City inputs
# --------------------------------------------------------------------------------
city_specs <- data.table::data.table(
  city = c("Bogota", "Mexico City", "Santiago", "Sao Paulo"),
  city_order = 1:4,
  geo_id_col = c("GEO_ID", "CVE_MUN", "comuna", "code_weighting"),
  pollution_dir = file.path(
    root_dir,
    "data", "processed", "outlier_detection",
    c(
      "bogota_metro_clean",
      "cdmx_metro_clean",
      "santiago_metro_clean",
      "sao_paulo_metro_clean"
    )
  ),
  dist_pq = file.path(
    root_dir,
    "data", "processed", "distances_matrices",
    c(
      "bogota_2018/matrix_geo_station_distances.parquet",
      "cdmx_2020/matrix_geo_station_distances.parquet",
      "santiago_2024/matrix_geo_station_distances.parquet",
      "sao_paulo_2010/matrix_geo_station_distances.parquet"
    )
  ),
  census_file = file.path(
    root_dir,
    "data", "interim", "census",
    c("bogota_2018", "cdmx_extended_2020", "santiago_2024", "sao_paulo_2010"),
    c(
      "census_2018_metro_individual.csv",
      "census_metro_individual_2020.csv",
      "census_santiago_individual_2024.csv",
      "census_sp_individual_2010.csv"
    )
  )
)

# 2. Helpers
# --------------------------------------------------------------------------------
.read_table <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "parquet") {
    return(data.table::as.data.table(arrow::read_parquet(path)))
  }
  
  if (ext %in% c("csv", "txt")) {
    return(data.table::fread(path, showProgress = FALSE))
  }
  
  stop("Unsupported file type: ", path)
}

.find_col <- function(dt, candidates, file_label) {
  hit <- candidates[candidates %in% names(dt)]
  
  if (length(hit) == 0L) {
    stop(
      "None of these columns were found in ", file_label, ": ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns are: ",
      paste(names(dt), collapse = ", ")
    )
  }
  
  hit[1L]
}

.station_quintile <- function(dist_pq, census_file, geo_id_col) {
  dist <- data.table::as.data.table(arrow::read_parquet(dist_pq))
  census <- .read_table(census_file)
  
  station_col <- .find_col(
    dist,
    c("station", "station_id", "id_station", "station_code", "codigo_estacion"),
    dist_pq
  )
  
  dist_geo_col <- .find_col(
    dist,
    c("geo_id", geo_id_col, "GEO_ID", "CVE_MUN", "CUT", "code_weighting"),
    dist_pq
  )
  
  dist_col <- .find_col(
    dist,
    c("distance_km", "dist_km", "distance", "dist"),
    dist_pq
  )
  
  census_geo_col <- .find_col(
    census,
    c(geo_id_col, "geo_id", "GEO_ID", "CVE_MUN", "CUT", "code_weighting"),
    census_file
  )
  
  edu_col <- .find_col(
    census,
    c("education_mean", "escolaridad", "educ_years", "years_schooling"),
    census_file
  )
  
  weight_col <- .find_col(
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

.city_missing <- function(spec, pollutants, year, report, mem_gb = 8) {
  if (!dir.exists(spec$pollution_dir)) {
    stop("Pollution directory not found for ", spec$city, ": ",
         spec$pollution_dir)
  }
  
  station_q <- .station_quintile(
    dist_pq = spec$dist_pq,
    census_file = spec$census_file,
    geo_id_col = spec$geo_id_col
  )
  
  if (nrow(station_q) == 0L) {
    stop("No station-quintile matches for ", spec$city, ".")
  }
  
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  DBI::dbWriteTable(con, "station_q", station_q, overwrite = TRUE)
  
  glob <- paste0("'", gsub("\\\\", "/", spec$pollution_dir), "/**/*.parquet'")
  
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
    stop("No station column found for ", spec$city, ".")
  }
  
  datetime_col <- cols[match(TRUE, cols_lower == "datetime")]
  if (is.na(datetime_col)) {
    stop("No datetime column found for ", spec$city, ".")
  }
  
  present_pollutants <- pollutants[pollutants %in% cols_lower]
  if (length(present_pollutants) == 0L) {
    stop("No PM columns found for ", spec$city, ".")
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
    city = spec$city,
    city_order = spec$city_order,
    year = year,
    statistic = report
  )]
  
  message("[missing by quintile] ", spec$city, " done.")
  out[]
}

.make_latex <- function(dt, digits = 3L) {
  wide <- data.table::dcast(
    dt,
    pollutant + city_order + city ~ quintile,
    value.var = "value"
  )
  
  data.table::setorder(wide, pollutant, city_order)
  
  q_cols <- as.character(1:5)
  for (q in q_cols) {
    if (!q %in% names(wide)) {
      wide[, (q) := NA_real_]
    }
  }
  
  fmt <- function(x) {
    out <- sprintf(paste0("%0.", digits, "f"), x)
    out[is.na(x)] <- "--"
    out
  }
  
  pol_lab <- c(pm10 = "$PM_{10}$", pm25 = "$PM_{2.5}$")
  
  lines <- c(
    "\\begin{tabular}{llccccc}",
    "\\toprule",
    "Pollutant & City & Q1 & Q2 & Q3 & Q4 & Q5 \\\\",
    "\\midrule"
  )
  
  pollutants_in_table <- unique(wide$pollutant)
  
  for (p in pollutants_in_table) {
    block <- wide[pollutant == p]
    
    for (i in seq_len(nrow(block))) {
      vals <- fmt(as.numeric(block[i, ..q_cols]))
      pol <- if (i == 1L) pol_lab[[p]] else ""
      
      line <- paste0(
        pol, " & ", block$city[i], " & ",
        paste(vals, collapse = " & "), " \\\\"
      )
      
      lines <- c(lines, line)
    }
    
    if (p != tail(pollutants_in_table, 1L)) {
      lines <- c(lines, "\\addlinespace")
    }
  }
  
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  paste(lines, collapse = "\n")
}

# 3. Compute and save
# --------------------------------------------------------------------------------
missing_by_quintile <- data.table::rbindlist(lapply(
  seq_len(nrow(city_specs)),
  function(i) {
    .city_missing(
      spec = city_specs[i],
      pollutants = pollutants,
      year = analysis_year,
      report = report
    )
  }
))

csv_path <- file.path(
  out_dir,
  paste0("missing_by_education_quintile_", analysis_year, ".csv")
)

tex_path <- file.path(
  out_dir,
  paste0("missing_by_education_quintile_", analysis_year, ".tex")
)

pq_path <- file.path(
  out_dir,
  paste0("missing_by_education_quintile_", analysis_year, ".parquet")
)

data.table::fwrite(missing_by_quintile, csv_path)
arrow::write_parquet(missing_by_quintile, pq_path)
writeLines(.make_latex(missing_by_quintile), tex_path)

message("Wrote: ", csv_path)
message("Wrote: ", pq_path)
message("Wrote: ", tex_path)