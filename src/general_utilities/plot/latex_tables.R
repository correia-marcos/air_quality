# ============================================================================================
# IDB: Air monitoring — LaTeX tables
# ============================================================================================
#' @Goal: Functions for LaTeX tables.
#
#' @Description: Renders the paper's tables to .tex. These read tables the process stage
# already computed; no statistics are calculated here. Sourced by
# config_utils_plot_tables.R; never sourced directly by a script.
#
#' @Summary:
#   1. table_state_metro_distances
#   2. table_who_exceedances
#   3. table_stations_by_pollutant
#   4. table_missing_by_dimension
#   5. write_exposure_summary_table_tex
#   6. plot_missing_heatmap
#   7. write_station_count_latex
#   8. latex_missing_by_quintile
#   9. latex_census_summary
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# ############################################################################################
# Main Functions
# ############################################################################################

# --------------------------------------------------------------------------------------------
# Function: table_state_metro_distances
#' @param national_states_sf  sf MULTIPOLYGON of country states (any CRS)
#' @param metro_area_sf       sf (MULTI)POLYGON for the metro area (any CRS)
#' @param state_name_col      column in `national_states_sf` with state names
#                             (default "name"; falls back to common variants)
#' @param caption             LaTeX caption (default auto: country/neutral text)
#' @param save_latex_table    write LaTeX to file? (default FALSE)
#' @param out_file            path to .tex file if saving
#' @param overwrite_tex       overwrite existing .tex? (default FALSE)
#' @param quiet               suppress info messages (default FALSE)
#' @return  data.frame with columns: state_name, distance_km, Potential_source
#           (If save_latex_table = TRUE, also writes a .tex file.)
#' @Purpose: Min distance (km) from each state to the metro area (0 for overlaps).
#           Adds an indicator (≤ 20 km) as Potential_source (1/0).
#' @Notes  : Distances computed in a local UTM for accuracy; then converted to km.
#           Uses st_make_valid() as a guard for tricky polygons.
#' @Written_on: 28/09/2025
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
table_state_metro_distances <- function(
    national_states_sf,
    metro_area_sf,
    state_name_col   = "name",
    caption          = NULL,
    save_latex_table = FALSE,
    out_file         = NULL,
    overwrite_tex    = FALSE,
    quiet            = FALSE
) {
  # ---- 0) deps + input validation --------------------------------------------
  
  stopifnot(inherits(national_states_sf, "sf"),
            inherits(metro_area_sf, "sf"))
  if (nrow(national_states_sf) == 0)
    stop("`national_states_sf` has zero rows.")
  if (nrow(metro_area_sf) == 0)
    stop("`metro_area_sf` has zero rows.")
  
  # ---- 1) pick state-name column (with fallbacks) ----------------------------
  nm_col <- state_name_col
  if (!nm_col %in% names(national_states_sf)) {
    fallbacks <- c("name", "name_es", "name_en", "NAME", "STATE_NAME")
    avail <- intersect(fallbacks, names(national_states_sf))
    if (length(avail) > 0) {
      nm_col <- avail[1]
      if (!quiet) message("Using state name column: '", nm_col, "' (fallback).")
    } else {
      stop("Column '", state_name_col, "' not found. Available: ",
           paste(names(national_states_sf), collapse = ", "))
    }
  }
  
  # ---- 2) make geometries valid and project to local UTM ---------------------
  states_ok <- sf::st_make_valid(national_states_sf)
  metro_ok  <- sf::st_make_valid(metro_area_sf)

  crs_utm   <- utm_epsg(metro_ok)
  states_utm <- sf::st_transform(states_ok, crs_utm)
  metro_utm  <- sf::st_transform(metro_ok,  crs_utm)
  
  # Treat metro as one geometry
  metro_union <- sf::st_union(metro_utm)
  
  # ---- 3) min distance (km) per state; 0 if intersects ----------------------
  # st_distance returns an n×1 matrix (units in meters)
  dist_m  <- as.numeric(sf::st_distance(states_utm, metro_union))
  dist_km <- dist_m / 1000
  
  # Set overlaps to 0 (robust vs float equality)
  overlaps <- sf::st_intersects(states_utm, metro_union, sparse = FALSE)[, 1]
  dist_km[overlaps] <- 0
  
  # Build result df
  result_df <- dplyr::tibble(
    state_name       = as.character(states_utm[[nm_col]]),
    distance_km      = dist_km,
    Potential_source = as.integer(dist_km <= 20)
  ) |>
    dplyr::arrange(distance_km) %>% 
    dplyr::filter(!is.na(state_name))
  
  # ---- 4) Optional LaTeX export (pretty-printed, booktabs) -------------------
  # ---- 4) Optional LaTeX export (pretty-printed, booktabs) -------------------
  if (isTRUE(save_latex_table)) {
    if (is.null(out_file))
      stop("Provide `out_file` when `save_latex_table = TRUE`.")
    
    
    # (b) caption (auto if missing)
    if (is.null(caption)) {
      caption <- paste(
        "Administrative states and distance to metropolitan area",
        "(distance in km; Potential source = 1 if $\\leq 20$ km)"
      )
    }
    
    # (c) format table data
    fmt_km <- function(v) format(round(v, 2), big.mark = ",", trim = TRUE)
    df_tbl <- result_df |>
      dplyr::mutate(
        State              = latex_escape(as.character(state_name)),
        `Distance (km)`    = fmt_km(distance_km),
        `Potential source` = ifelse(Potential_source == 1, "1", "0")
      ) |>
      dplyr::select(State, `Distance (km)`, `Potential source`)
    
    # (d) build LaTeX as a vector of lines (pretty-printed)
    # NOTE: requires \usepackage{booktabs} in your preamble
    lines <- c(
      "\\begin{table}[htbp]",
      "  \\centering",
      paste0("  \\caption{", latex_escape(caption), "}"),
      "  \\begin{tabular}{lrr}",
      "    \\midrule",
      "    \\midrule",
      "    \\multicolumn{1}{c}{\\textbf{State}} &",
      "    \\multicolumn{1}{c}{\\textbf{Distance to metro area}} &",
      "    \\multicolumn{1}{c}{\\textbf{Potential pollution source?}} \\\\",
      "    \\multicolumn{1}{c}{} &",
      "    \\multicolumn{1}{c}{\\textbf{(km)}} &",
      "    \\multicolumn{1}{c}{\\textbf{($\\leq 20$ km)}} \\\\",
      "    \\midrule"
    )
    
    # (e) append one line per data row (indented, readable)
    if (nrow(df_tbl) > 0) {
      row_lines <- apply(df_tbl, 1, function(r)
        paste0("    ", r[1], " & ", r[2], " & ", r[3], " \\\\"))
      lines <- c(lines, row_lines)
    }
    
    # (f) close the environment
    lines <- c(
      lines,
      "    \\bottomrule",
      "    \\bottomrule",
      "  \\end{tabular}",
      "  \\label{table_state_metro_distances}",
      "\\end{table}"
    )
    
    # (g) write to file (preserves indentation / one row per line)
    if (file.exists(out_file) && !overwrite_tex) {
      stop("File exists and `overwrite_tex = FALSE`: ", out_file)
    }
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, out_file)
    
    if (!quiet) message("LaTeX table saved → ", normalizePath(out_file))
  }
  
  return(result_df)
}


# --------------------------------------------------------------------------------------------
# Function: table_who_exceedances
#
#' @param exceedances_dt data.table from compute_who_exceedances() (possibly row-bound
#                        across cities).
#' @param save_latex_table logical; write LaTeX to file? Default FALSE.
#' @param out_file       path to .tex file if saving.
#' @param caption        LaTeX caption.
#' @param label          LaTeX label.
#' @param overwrite_tex  logical; overwrite existing .tex file. Default FALSE.
#' @param digits         integer; decimal digits in the printed numbers. Default 2.
#' @param quiet          logical; suppress info messages. Default FALSE.
#
#' @return  data.table (wide: city × year rows, pollutant columns for city_avg
#           and exceedance_factor). Optionally writes a booktabs-style LaTeX
#           table.
#
#' @Purpose: LaTeX table for legacy inputs/1_AQG_guidelines.R results.
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
table_who_exceedances <- function(
    exceedances_dt,
    save_latex_table = FALSE,
    out_file         = NULL,
    caption          = "Annual PM concentrations vs. WHO AQG (2021).",
    label            = "tab:who_exceedances",
    overwrite_tex    = FALSE,
    digits           = 2,
    quiet            = FALSE
) {
  stopifnot(is.data.frame(exceedances_dt))
  req <- c("city","year","pollutant","city_avg","who_aqg","exceedance_factor")
  if (!all(req %in% names(exceedances_dt)))
    stop("`exceedances_dt` missing required columns.")
  dt <- data.table::as.data.table(exceedances_dt)
  
  wide <- data.table::dcast(
    dt, city + year ~ pollutant,
    value.var = c("city_avg", "exceedance_factor")
  )
  data.table::setorder(wide, city, year)
  
  if (isTRUE(save_latex_table)) {
    if (is.null(out_file)) stop("`out_file` is required when save_latex_table = TRUE.")
    if (file.exists(out_file) && !overwrite_tex)
      stop("File exists: ", out_file, " (set overwrite_tex = TRUE).")
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    
    num_cols <- setdiff(names(wide), c("city","year"))
    fmt <- wide[, lapply(.SD, function(x) formatC(x, format = "f", digits = digits)),
                .SDcols = num_cols]
    fmt <- cbind(wide[, .(city, year)], fmt)
    
    header <- c(
      "\\begin{table}[!htbp]\\centering",
      sprintf("\\caption{%s}", caption),
      sprintf("\\label{%s}", label),
      "\\begin{tabular}{ll" ,
      paste(rep("r", length(num_cols)), collapse = ""),
      "}",
      "\\toprule",
      paste(c("City","Year", num_cols), collapse = " & "),
      "\\\\",
      "\\midrule"
    )
    body <- apply(fmt, 1L, function(r) paste(paste(r, collapse = " & "), "\\\\"))
    footer <- c("\\bottomrule", "\\end{tabular}", "\\end{table}")
    
    writeLines(c(header, body, footer), out_file)
    if (!quiet) message("📝 Wrote LaTeX table → ", out_file)
  }
  
  invisible(wide)
}


# --------------------------------------------------------------------------------------------
# Function: table_stations_by_pollutant
#
#' @param stations_long data.table, the $long element from
# summarize_stations_by_pollutant().
#                        Must contain (city, year, pollutant, n_stations).
#' @param save_latex_table logical. Default FALSE.
#' @param out_file      path to .tex file.
#' @param caption       LaTeX caption.
#' @param label         LaTeX label.
#' @param overwrite_tex logical. Default FALSE.
#' @param quiet         logical. Default FALSE.
#
#' @return  data.table (wide): city × year × pollutant counts. Side effect: a
#           booktabs-style LaTeX table when requested.
#
#' @Purpose: LaTeX-ready rebuild of inputs/1_number_stations_pollutant.R.
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
table_stations_by_pollutant <- function(
    stations_long,
    save_latex_table = FALSE,
    out_file         = NULL,
    caption          = "Number of monitoring stations reporting each pollutant by city-year.",
    label            = "tab:stations_by_pollutant",
    overwrite_tex    = FALSE,
    quiet            = FALSE
) {
  stopifnot(is.data.frame(stations_long))
  req <- c("city","year","pollutant","n_stations")
  if (!all(req %in% names(stations_long)))
    stop("`stations_long` missing required columns.")
  dt <- data.table::as.data.table(stations_long)
  
  wide <- data.table::dcast(
    dt, city + year ~ pollutant, value.var = "n_stations", fill = 0L
  )
  data.table::setorder(wide, city, year)
  
  if (isTRUE(save_latex_table)) {
    if (is.null(out_file)) stop("`out_file` is required.")
    if (file.exists(out_file) && !overwrite_tex)
      stop("File exists: ", out_file)
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    num_cols <- setdiff(names(wide), c("city","year"))
    header <- c(
      "\\begin{table}[!htbp]\\centering",
      sprintf("\\caption{%s}", caption),
      sprintf("\\label{%s}", label),
      paste0("\\begin{tabular}{ll", paste(rep("r", length(num_cols)),
                                          collapse = ""), "}"),
      "\\toprule",
      paste(c("City", "Year", toupper(num_cols)), collapse = " & "),
      "\\\\",
      "\\midrule"
    )
    body <- apply(wide, 1L, function(r) paste(paste(r, collapse = " & "), "\\\\"))
    footer <- c("\\bottomrule", "\\end{tabular}", "\\end{table}")
    writeLines(c(header, body, footer), out_file)
    if (!quiet) message("📝 Wrote LaTeX table → ", out_file)
  }
  
  invisible(wide)
}


# --------------------------------------------------------------------------------------------
# Function: table_missing_by_dimension
#
#' @param missing_list   list; output of compute_missing_proportions() (names are dims).
#' @param dim            string; which dimension to render. Must be a name in
# `missing_list`.
#' @param city_label     string; first column ("City") value in the rendered table.
#' @param save_latex_table logical; default FALSE.
#' @param out_file       path to .tex file.
#' @param caption        LaTeX caption.
#' @param label          LaTeX label.
#' @param overwrite_tex  logical. Default FALSE.
#' @param digits         integer; decimal digits. Default 1.
#' @param quiet          logical. Default FALSE.
#
#' @return  data.table; the selected dimension table plus a city column.
#           Side effect: a booktabs LaTeX table when requested.
#
#' @Purpose: LaTeX rebuild of the missing-proportion tables from legacy
#           Missing analysis/auxiliar_missings.R and 5_stats_non_missing.R.
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
table_missing_by_dimension <- function(
    missing_list,
    dim,
    city_label,
    save_latex_table = FALSE,
    out_file         = NULL,
    caption          = NULL,
    label            = NULL,
    overwrite_tex    = FALSE,
    digits           = 1,
    quiet            = FALSE
) {
  stopifnot(is.list(missing_list), dim %in% names(missing_list))
  dt <- data.table::copy(data.table::as.data.table(missing_list[[dim]]))
  dt[, city := city_label]
  data.table::setcolorder(dt, c("city", setdiff(names(dt), "city")))
  
  pct_cols <- grep("_missing_pct$", names(dt), value = TRUE)
  if (length(pct_cols) == 0L)
    stop("No *_missing_pct columns found in missing_list[[dim]].")
  
  if (isTRUE(save_latex_table)) {
    if (is.null(out_file)) stop("`out_file` is required.")
    if (is.null(caption))
      caption <- sprintf("Share (%%) of missing observations by %s — %s.",
                         dim, city_label)
    if (is.null(label))
      label <- sprintf("tab:missing_%s_%s", dim,
                       gsub("[^a-z0-9]", "_", tolower(city_label)))
    if (file.exists(out_file) && !overwrite_tex)
      stop("File exists: ", out_file)
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    
    pretty <- data.table::copy(dt)
    for (c in pct_cols)
      pretty[[c]] <- formatC(pretty[[c]], format = "f", digits = digits)
    
    n_cols <- ncol(pretty)
    header <- c(
      "\\begin{table}[!htbp]\\centering",
      sprintf("\\caption{%s}", caption),
      sprintf("\\label{%s}", label),
      paste0("\\begin{tabular}{", paste(rep("l", n_cols), collapse = ""), "}"),
      "\\toprule",
      paste(toupper(names(pretty)), collapse = " & "),
      "\\\\",
      "\\midrule"
    )
    body <- apply(pretty, 1L, function(r) paste(paste(r, collapse = " & "), "\\\\"))
    footer <- c("\\bottomrule", "\\end{tabular}", "\\end{table}")
    writeLines(c(header, body, footer), out_file)
    if (!quiet) message("📝 Wrote LaTeX table → ", out_file)
  }
  invisible(dt)
}


# --------------------------------------------------------------------------------------------
# Function: write_exposure_summary_table_tex
#
#' @param summary_dt data.table; output from compute_exposure_group_summaries().
#' @param out_path  string; path to save .tex table.
#' @param digits    integer; number of decimal places. Default 2.
#' @param caption   string; LaTeX table caption.
#' @param label     string; LaTeX table label.
#
#' @return  Invisibly returns out_path.
#
#' @details
#   Writes a compact LaTeX table with weighted means and medians by group.
#
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
write_exposure_summary_table_tex <- function(
    summary_dt,
    out_path,
    digits  = 2,
    caption = "Exposure summary by socioeconomic group",
    label   = "tab:exposure_summary"
) {
  
  dt <- data.table::copy(data.table::as.data.table(summary_dt))
  
  req_cols <- c("outcome", "pollutant", "group", "weighted_mean",
                "weighted_median")
  miss_cols <- setdiff(req_cols, names(dt))
  
  if (length(miss_cols) > 0L) {
    stop("Missing columns: ", paste(miss_cols, collapse = ", "))
  }
  
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  
  dt[, weighted_mean := round(weighted_mean, digits)]
  dt[, weighted_median := round(weighted_median, digits)]
  
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{llrrr}",
    "\\toprule",
    "Outcome & Pollutant & Group & Mean & Median \\\\",
    "\\midrule"
  )
  
  body <- dt[
    ,
    sprintf(
      "%s & %s & %s & %s & %s \\\\",
      outcome,
      pollutant,
      group,
      format(weighted_mean, nsmall = digits),
      format(weighted_median, nsmall = digits)
    )
  ]
  
  lines <- c(
    lines,
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  writeLines(lines, out_path)
  invisible(out_path)
}


# --------------------------------------------------------------------------------------------
# Function: plot_missing_heatmap
#
#' @param missing_list list; output of compute_missing_proportions() with at least two
#                      dimensions of interest (default: "month" and "hour").
#' @param row_dim     string; dimension on the y-axis. Default "month".
#' @param col_dim     string; dimension on the x-axis. Default "hour".
#' @param pollutant   string; which {pollutant}_missing_pct column to render.
#                      Default "pm25".
#' @param city_label  string; plot title.
#' @param arrow_dir   string|NULL; if given, a secondary query is run to get the
#                      two-way aggregation directly (recommended: ignoring row_dim/col_dim
#                      in missing_list). If NULL, the function falls back to a naive
#                      outer-join reconstruction, which only works when the dims are
#                      independent.
#' @param mem_gb      numeric; DuckDB memory ceiling. Default 4.
#
#' @return  ggplot2 heatmap.
#
#' @Purpose: Quick visual rebuild of the "missing by month × hour" diagnostic from
#           legacy 7_missing_analysis.do. Prefer passing `arrow_dir` so the
#           two-way shares are computed exactly.
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_missing_heatmap <- function(
    missing_list,
    row_dim    = "month",
    col_dim    = "hour",
    pollutant  = "pm25",
    city_label = "",
    arrow_dir  = NULL,
    mem_gb     = 4
) {
  pct_col <- paste0(tolower(pollutant), "_missing_pct")
  
  if (!is.null(arrow_dir)) {
    stopifnot(dir.exists(arrow_dir))
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
    DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
    glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
    DBI::dbExecute(con, paste0(
      "CREATE VIEW pollution AS SELECT * FROM read_parquet(",
      glob_q, ", hive_partitioning = true);"
    ))
    dim_expr <- list(
      month = "EXTRACT(month FROM datetime)",
      hour  = "EXTRACT(hour FROM datetime)",
      day_of_week = "EXTRACT(isodow FROM datetime)",
      year  = "EXTRACT(year FROM datetime)"
    )
    if (!(row_dim %in% names(dim_expr) && col_dim %in% names(dim_expr)))
      stop("row_dim / col_dim must be month, hour, day_of_week, or year.")
    q <- sprintf(
      "SELECT %s AS %s, %s AS %s,
              100.0 * SUM(CASE WHEN %s IS NULL THEN 1 ELSE 0 END) / COUNT(*) AS pct
       FROM pollution
       GROUP BY 1, 2 ORDER BY 1, 2;",
      dim_expr[[row_dim]], row_dim,
      dim_expr[[col_dim]], col_dim,
      tolower(pollutant)
    )
    d <- data.table::as.data.table(DBI::dbGetQuery(con, q))
  } else {
    # Reconstruct a 2-way view by "outer product" of the 1-way tables. This
    # assumes independence between row_dim and col_dim and is only an
    # approximation — hence the warning.
    warning("arrow_dir not provided; the heatmap assumes independence between ",
            row_dim, " and ", col_dim, ".")
    r <- data.table::as.data.table(missing_list[[row_dim]])
    c <- data.table::as.data.table(missing_list[[col_dim]])
    d <- data.table::CJ(
      row = r[[row_dim]], col = c[[col_dim]]
    )
    data.table::setnames(d, c("row","col"), c(row_dim, col_dim))
    d <- merge(d, r[, .SD, .SDcols = c(row_dim, pct_col)], by = row_dim)
    d <- merge(d, c[, .SD, .SDcols = c(col_dim, pct_col)], by = col_dim,
               suffixes = c(".r", ".c"))
    d[, pct := (get(paste0(pct_col,".r")) + get(paste0(pct_col,".c"))) / 2]
  }
  
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data[[col_dim]], y = .data[[row_dim]], fill = pct)
  ) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_viridis_c(
      option = "C", name = "% missing", limits = c(0, 100)
    ) +
    ggplot2::labs(
      x     = tools::toTitleCase(col_dim),
      y     = tools::toTitleCase(row_dim),
      title = sprintf("%s — %s missing heatmap",
                      city_label, toupper(pollutant))
    ) +
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 13) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
}



# --------------------------------------------------------------------------------------------
# Function: write_station_count_latex
#
#' @param station_counts data.table with columns city, pm10, pm25.
#' @param out_file      string; destination .tex path.
#' @param table_size    string; LaTeX size macro. Default "\\tiny".
#
#' @return  invisible out_file. Writes a three-column city/PM10/PM2.5 table.
#
#' @details
#   Not the same table as table_stations_by_pollutant(), which renders city x year x
#   pollutant
#   from a coverage summary. This one is the paper's compact station count for a single
#   year.
#   The two header rows previously ended in a single backslash, which LaTeX reads as
#   escaping
#   the newline rather than ending the row; both now emit the required double backslash.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
write_station_count_latex <- function(station_counts,
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
    "\\multicolumn{2}{c}{\\textbf{Number of monitoring stations}} \\\\",
    "\\cmidrule{2-3}",
    "\\textbf{City} & $PM_{10}$ & $PM_{2.5}$ \\\\",
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


# --------------------------------------------------------------------------------------------
# Function: latex_missing_by_quintile
#
#' @param dt    data.table from compute_missing_by_quintile(), stacked across cities.
#' @param digits integer; decimal places for the shares. Default 3.
#
#' @return  character scalar; the LaTeX tabular, ready for writeLines().
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
latex_missing_by_quintile <- function(dt, digits = 3L) {
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


# --------------------------------------------------------------------------------------------
# Function: latex_census_summary
#
#' @param dt data.table from compute_city_census_summary(), stacked across cities. Needs
#           city_latex, year, total_population, census_geographic_level,
#           n_census_geographic_units and average_population_per_unit.
#
#' @return  character vector; the LaTeX tabular lines, ready for writeLines().
#
#' @details
#   The body is deliberately a plain tabular with no table float or caption, so the paper
#   can wrap it and the row order is whatever the caller stacked. Counts go through
#   format_int_latex() for thousands separators and the level label through
#   latex_escape(), because accented level names would otherwise break the build.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
latex_census_summary <- function(dt) {
  tbl <- data.table::copy(data.table::as.data.table(dt))

  tbl[, total_population_fmt := format_int_latex(total_population)]
  tbl[, n_units_fmt          := format_int_latex(n_census_geographic_units)]
  tbl[, avg_pop_fmt          := format_int_latex(average_population_per_unit)]
  tbl[, census_level_latex   := latex_escape(census_geographic_level)]

  rows <- vapply(seq_len(nrow(tbl)), function(i) {
    paste0(
      "    ", tbl$city_latex[i], " & ", tbl$year[i], " & ",
      tbl$total_population_fmt[i], " & ", tbl$census_level_latex[i], " & ",
      tbl$n_units_fmt[i], " & ", tbl$avg_pop_fmt[i], " \\\\"
    )
  }, character(1))

  c(
    "\\begin{tabular}{lccccc}",
    "    \\toprule",
    "    \\toprule",
    paste0(
      "    \\multicolumn{1}{c}{\\textbf{City}} & ",
      "\\multicolumn{1}{c}{\\textbf{Year}} & ",
      "\\multicolumn{1}{c}{\\textbf{Total}} & ",
      "\\multicolumn{1}{c}{\\textbf{Census}} & ",
      "\\multicolumn{1}{c}{\\textbf{Number of census}} & ",
      "\\multicolumn{1}{c}{\\textbf{Average population per}} \\\\"
    ),
    paste0(
      "    & & ",
      "\\multicolumn{1}{c}{\\textbf{population}} & ",
      "\\multicolumn{1}{c}{\\textbf{geographic level}} & ",
      "\\multicolumn{1}{c}{\\textbf{geographic units}} & ",
      "\\multicolumn{1}{c}{\\textbf{census geographic unit}} \\\\"
    ),
    "    \\midrule",
    rows,
    "    \\bottomrule",
    "    \\bottomrule",
    "\\end{tabular}"
  )
}
