# ====================================================================================
# IDB: Air monitoring
# ====================================================================================
# @Goal: Produce missing-data diagnostics and a LaTeX station-count table.
#
# @Description:
#   This script runs `compute_missing_proportions()` for PM10 and PM2.5 in each
#   city. It also creates a LaTeX table with the number of monitoring stations
#   with at least one valid PM10 or PM2.5 observation in the selected year.
#
# @Summary:
#   I.    Import functions and define paths.
#   II.   Define small script-level helpers.
#   III.  Compute structural missingness from raw hourly datasets.
#   IV.   Compute post-outlier missingness from cleaned hourly datasets.
#   V.    Count monitoring stations by pollutant and write a LaTeX table.
#
# @Date: April 2026
# @Updated_on: June 2026
# @Author: Marcos Paulo
# ====================================================================================

# Get all libraries and functions.
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ====================================================================================
# I: Import data
# ====================================================================================
# Define the general input and output folders.
dir_raw        <- here::here("data", "raw", "monitoring_stations")
dir_clean      <- here::here("data", "processed", "outlier_detection")
outdir_missing <- here::here("data", "processed", "missing_proportions")
outdir_tables  <- here::here("results", "tables", "station_counts")

# Main analysis parameters.
analysis_year <- 2023L
pollutants    <- c("pm10", "pm25")

# Arrow raw hourly datasets. These define the monitoring infrastructure table.
arrow_raw_dirs <- list(
  bogota          = here::here(dir_raw, "bogota_metro_dataset"),
  cdmx            = here::here(dir_raw, "cdmx_metro_dataset"),
  santiago        = here::here(dir_raw, "santiago_metro_dataset"),
  sao_paulo_metro = here::here(dir_raw, "sao_paulo_metro_dataset")
)

# Arrow cleaned hourly datasets. These define post-outlier missingness.
arrow_clean_dirs <- list(
  bogota          = here::here(dir_clean, "bogota_metro_clean"),
  cdmx            = here::here(dir_clean, "cdmx_metro_clean"),
  santiago        = here::here(dir_clean, "santiago_metro_clean"),
  sao_paulo_metro = here::here(dir_clean, "sao_paulo_metro_clean")
)

# Table labels and order used in the presentation/paper output.
city_labels <- data.table::data.table(
  city_id = c("santiago", "bogota", "cdmx", "sao_paulo_metro"),
  city    = c("Santiago", "Bogotá", "Mexico City", "São Paulo")
)

# ====================================================================================
# II: Small script-level helpers
# ====================================================================================
# Count stations with at least one valid value for each requested pollutant.
.count_stations_by_pollutant <- function(arrow_dir,
                                         pollutants = c("pm10", "pm25"),
                                         year_filter = 2023L,
                                         mem_gb = 8) {
  for (p in c("duckdb", "DBI", "data.table")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required but not installed.")
    }
  }
  
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

# Write the monitoring-station count table in presentation-ready LaTeX.
.write_station_count_latex <- function(station_counts,
                                       out_file,
                                       table_size = "\\tiny") {
  station_counts <- data.table::copy(station_counts)
  
  lines_body <- apply(station_counts, 1, function(x) {
    paste0("  ", x[["city"]], " &  ", x[["pm10"]], " &  ",
           x[["pm25"]], " \\\\ ")
  })
  
  latex_lines <- c(
    "\\vspace{0.1cm}",
    "\\begin{center}",
    table_size,
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "\\toprule",
    "\\multicolumn{2}{c}{\\textbf{Number of monitoring stations}} \\",
    "\\cmidrule{2-3}",
    "\\textbf{City} & $PM_{10}$ & $PM_{2.5}$ \\",
    "\\midrule",
    lines_body,
    "\\bottomrule",
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{center}"
  )
  
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(latex_lines, out_file, useBytes = TRUE)
  
  invisible(out_file)
}

# ====================================================================================
# III: Process raw data: structural missingness
# ====================================================================================
dir.create(outdir_missing, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_tables, recursive = TRUE, showWarnings = FALSE)

for (city in names(arrow_raw_dirs)) {
  if (!dir.exists(arrow_raw_dirs[[city]])) {
    message("[", city, "] Raw Arrow dataset not found — skipping.")
    next
  }
  
  compute_missing_proportions(
    arrow_dir   = arrow_raw_dirs[[city]],
    pollutants  = pollutants,
    dims        = c("station", "month", "hour", "day_of_week"),
    year_filter = analysis_year,
    out_dir     = outdir_missing,
    out_name    = paste0(city, "_raw")
  )
}

# ====================================================================================
# IV: Process cleaned data: algorithmic missingness
# ====================================================================================
for (city in names(arrow_clean_dirs)) {
  if (!dir.exists(arrow_clean_dirs[[city]])) {
    message("[", city, "] Clean Arrow dataset not found — skipping.")
    next
  }
  
  compute_missing_proportions(
    arrow_dir   = arrow_clean_dirs[[city]],
    pollutants  = pollutants,
    dims        = c("station", "month", "hour", "day_of_week"),
    year_filter = analysis_year,
    out_dir     = outdir_missing,
    out_name    = paste0(city, "_clean")
  )
}

# ====================================================================================
# V: Generate LaTeX table: number of monitoring stations by pollutant
# ====================================================================================
station_count_list <- list()

for (city in names(arrow_raw_dirs)) {
  if (!dir.exists(arrow_raw_dirs[[city]])) {
    message("[", city, "] Raw Arrow dataset not found — skipping table count.")
    next
  }
  
  tmp <- .count_stations_by_pollutant(
    arrow_dir    = arrow_raw_dirs[[city]],
    pollutants   = pollutants,
    year_filter  = analysis_year,
    mem_gb       = 8
  )
  
  tmp[, city_id := city]
  station_count_list[[city]] <- tmp
}

station_counts <- data.table::rbindlist(station_count_list, fill = TRUE)
station_counts <- merge(city_labels, station_counts, by = "city_id", all.x = TRUE)

# Keep the presentation order: Santiago, Bogotá, Mexico City, São Paulo.
station_counts[, city_id := factor(city_id, levels = city_labels$city_id)]
data.table::setorder(station_counts, city_id)

station_counts <- station_counts[, .(city, pm10, pm25)]

# Save machine-readable and LaTeX versions of the station-count table.
arrow::write_parquet(
  station_counts,
  file.path(outdir_tables, "stations_by_pollutant_2023.parquet")
)

data.table::fwrite(
  station_counts,
  file.path(outdir_tables, "stations_by_pollutant_2023.csv")
)

.write_station_count_latex(
  station_counts = station_counts,
  out_file = file.path(outdir_tables, "stations_by_pollutant_2023.tex")
)

cat("Script from the IDB project executed successfully in the Docker container!\n")
