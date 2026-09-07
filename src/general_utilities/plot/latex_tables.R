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
#   5. latex_exposure_means_by_group
#   6. plot_missing_heatmap
#   7. write_station_count_latex
#   8. latex_missing_by_quintile
#   9. latex_census_summary
#  10. render_missing_dimension_table
#  11. latex_distance_band_table
#  12. latex_exposure_hours_by_group
#  13. latex_threshold_exceedance_table
#
#' @Date: September 2026
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
#' @Purpose: LaTeX table of the WHO-exceedance results (city × year × pollutant).
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
#' @Purpose: LaTeX rebuild of the missing-proportion tables.
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
# Function: latex_exposure_means_by_group
#
#' @param summary_dt data.table; exposure_group_summaries_* from estimate_exposure.R.
#' @param ci_dt      data.table; exposure_ci_estimates_* from the same run.
#' @param panel_cities character; cities to print, in panel order (the `city` values).
#' @param panel_labels named character; city -> panel heading. Defaults to the city value,
#                      which is the artefact's internal name ("CDMX"), not the paper's.
#' @param n_groups   integer; widest group count across the panels, 5 or 10.
#' @param digits     integer; decimal places for the concentrations. Default 2.
#
#' @return  character vector; a bare LaTeX tabular, ready for writeLines().
#
#' @details
#   The appendix's "mean and median concentration by group" table: one city panel per
#   entry of panel_cities, four value rows per panel (PM10 mean/median, PM2.5 mean/median)
#   and two p-value rows. Groups run across the columns.
#
#   The p-value tests the lowest group against the top group, which is the omitted
#   reference of the exposure regression, so it is read off that regression's coefficient
#   rather than recomputed: p = 2 * pt(-|estimate / std_error|, df = n_clusters - 1). The
#   t(G-1) reference distribution matches the one the confidence intervals already use.
#   Cities whose panel has fewer groups than n_groups leave the surplus cells blank, which
#   is what lets CDMX's income quintiles and Sao Paulo's income deciles share one tabular.
#
#' @Written_on : September 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
latex_exposure_means_by_group <- function(summary_dt, ci_dt, panel_cities,
                                          panel_labels = NULL,
                                          n_groups = 5L, digits = 2L) {
  means <- data.table::as.data.table(summary_dt)[outcome == "avg"]
  ci    <- data.table::as.data.table(ci_dt)[outcome == "avg" & group == 1]

  if (is.null(panel_labels)) panel_labels <- stats::setNames(panel_cities, panel_cities)

  groups <- seq_len(n_groups)
  col_spec <- paste0("l", strrep("r", n_groups))

  header <- c(
    paste0("\\begin{tabular}{", col_spec, "}"),
    "    \\toprule",
    "    \\toprule",
    paste0("    Group & ", paste(groups, collapse = " & "), " \\\\"),
    "    \\midrule")

  # One row of the panel: a statistic for one pollutant, across the group columns.
  value_row <- function(sub, poll, stat_col, label) {
    vals <- vapply(groups, function(g) {
      v <- sub[pollutant == poll & group == g, get(stat_col)]
      if (length(v) == 0L || is.na(v[1])) "" else formatC(v[1], format = "f",
                                                          digits = digits)
    }, character(1))
    paste0("    ", label, " & ", paste(vals, collapse = " & "), " \\\\")
  }

  body <- character(0)

  for (i in seq_along(panel_cities)) {
    city_i <- panel_cities[i]
    sub    <- means[city == city_i]

    if (nrow(sub) == 0L) stop("No exposure summary rows for city: ", city_i)

    body <- c(body,
              sprintf("    \\multicolumn{%d}{l}{\\textbf{Panel %s. %s}} \\\\",
                      n_groups + 1L, LETTERS[i], latex_escape(panel_labels[[city_i]])),
              "    \\midrule",
              value_row(sub, "pm10", "weighted_mean",   "$PM_{10}$ Mean"),
              value_row(sub, "pm10", "weighted_median", "$PM_{10}$ Median"),
              value_row(sub, "pm25", "weighted_mean",   "$PM_{2.5}$ Mean"),
              value_row(sub, "pm25", "weighted_median", "$PM_{2.5}$ Median"))

    # The lowest-vs-top-group test, one row per pollutant; blank where not estimated.
    for (poll in c("pm10", "pm25")) {
      est <- ci[city == city_i & pollutant == poll]
      lab <- if (poll == "pm10") "$PM_{10}$" else "$PM_{2.5}$"

      p_cell <- if (nrow(est) == 0L || is.na(est$std_error[1]) ||
                    est$std_error[1] <= 0) {
        ""
      } else {
        formatC(2 * stats::pt(-abs(est$estimate[1] / est$std_error[1]),
                              df = est$n_clusters[1] - 1L),
                format = "f", digits = 2L)
      }

      body <- c(body, paste0(
        "    P-value (lowest vs. top group, ", lab, ") & ", p_cell,
        strrep(" & ", n_groups - 1L), " \\\\"))
    }

    if (i < length(panel_cities)) body <- c(body, "    \\midrule")
  }

  c(header, body, "    \\bottomrule", "    \\bottomrule", "\\end{tabular}")
}


# --------------------------------------------------------------------------------------------
# Function: latex_exposure_hours_by_group
#
#' @param summary_dt data.table; exposure_group_summaries_* from estimate_exposure.R.
#' @param panel_cities character; cities to print, in panel order (the `city` values).
#' @param panel_labels named character; city -> panel heading. Defaults to the city value,
#                      which is the artefact's internal name ("CDMX"), not the paper's.
#
#' @return  character vector; a bare LaTeX tabular, ready for writeLines().
#
#' @details
#   The appendix's "average hours above IT1 and IT2 by group" table. Groups run down the
#   rows here rather than across the columns, so panels with different group counts need
#   no padding: CDMX prints five income rows and Sao Paulo ten. The four value columns are
#   IT1 PM10/PM2.5 then IT2 PM10/PM2.5, taken from the weighted means of the hrs_d_it1 and
#   hrs_d_it2 outcomes.
#
#' @Written_on : September 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
latex_exposure_hours_by_group <- function(summary_dt, panel_cities,
                                          panel_labels = NULL) {
  dt <- data.table::as.data.table(summary_dt)[outcome %in% c("hrs_d_it1", "hrs_d_it2")]

  if (is.null(panel_labels)) panel_labels <- stats::setNames(panel_cities, panel_cities)

  header <- c(
    "\\begin{tabular}{lcc|cc}",
    "    \\toprule",
    "    \\toprule",
    paste0("    Group & \\multicolumn{2}{c|}{Average hours $\\geq$ IT1} & ",
           "\\multicolumn{2}{c}{Average hours $\\geq$ IT2} \\\\"),
    "    & $PM_{10}$ & $PM_{2.5}$ & $PM_{10}$ & $PM_{2.5}$ \\\\",
    "    \\midrule")

  body <- character(0)

  for (i in seq_along(panel_cities)) {
    city_i <- panel_cities[i]
    sub    <- dt[city == city_i]

    if (nrow(sub) == 0L) stop("No exposure summary rows for city: ", city_i)

    body <- c(body,
              sprintf("    \\multicolumn{5}{l}{\\textbf{Panel %s. %s}} \\\\",
                      LETTERS[i], latex_escape(panel_labels[[city_i]])),
              "    \\midrule")

    # Each city prints as many group rows as its own regression estimated.
    for (g in sort(unique(sub$group))) {
      vals <- vapply(
        list(c("hrs_d_it1", "pm10"), c("hrs_d_it1", "pm25"),
             c("hrs_d_it2", "pm10"), c("hrs_d_it2", "pm25")),
        function(k) {
          v <- sub[outcome == k[1] & pollutant == k[2] & group == g, weighted_mean]
          if (length(v) == 0L || is.na(v[1])) "" else formatC(v[1], format = "f",
                                                              digits = 2L)
        }, character(1))

      body <- c(body, paste0("    ", g, " & ", paste(vals, collapse = " & "), " \\\\"))
    }

    if (i < length(panel_cities)) body <- c(body, "    \\midrule")
  }

  c(header, body, "    \\bottomrule", "    \\bottomrule", "\\end{tabular}")
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
#' @Purpose: Quick visual rebuild of the "missing by month × hour" diagnostic.
#           Prefer passing `arrow_dir` so the two-way shares are computed exactly.
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
#   Emits a bare tabular, like the census and distance-band producers, so the manuscript
#   supplies the float, caption and label. The former \begin{center} + \tiny wrapper and
#   the \multicolumn{2} banner (which spanned two of three columns) are gone: the banner
#   duplicated the caption and made the header row invalid.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : September 2026
# --------------------------------------------------------------------------------------------
write_station_count_latex <- function(station_counts, out_file) {
  station_counts <- data.table::copy(station_counts)

  lines_body <- apply(station_counts, 1, function(x) {
    paste0("  ", x[["city"]], " &  ", x[["pm10"]], " &  ",
           x[["pm25"]], " \\\\ ")
  })

  latex_lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "\\toprule",
    "\\textbf{City} & $PM_{10}$ & $PM_{2.5}$ \\\\",
    "\\midrule",
    lines_body,
    "\\bottomrule",
    "\\bottomrule",
    "\\end{tabular}"
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


# --------------------------------------------------------------------------------------------
# Function: render_missing_dimension_table
#
#' @param dir_missing string; folder holding the missing-proportion Parquet files.
#' @param city_id     string; city file prefix, e.g. "sao_paulo_metro".
#' @param panel       string; "raw" or "clean", the panel the shares describe.
#' @param dim         string; the dimension to tabulate, e.g. "station".
#' @param out_dir     string; folder to write the .tex into.
#
#' @return  invisible path of the .tex written.
#
#' @details
#   Reads one <city>_<panel>_missing_by_<dim>.parquet and writes the matching .tex, so a
#   calling script states only which city, panel and dimension it wants. File naming is
#   the reason this exists: the input stem and the output stem must agree, and pairing
#   them here keeps a rename from silently producing a table of the wrong panel.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
render_missing_dimension_table <- function(dir_missing, city_id, panel, dim, out_dir) {
  stem <- sprintf("%s_%s_missing_by_%s", city_id, panel, dim)

  missing_list <- list(arrow::read_parquet(
    file.path(dir_missing, paste0(stem, ".parquet"))))
  names(missing_list) <- dim

  out_file <- file.path(out_dir, paste0(stem, ".tex"))

  table_missing_by_dimension(
    missing_list     = missing_list,
    dim              = dim,
    city_label       = city_id,
    save_latex_table = TRUE,
    out_file         = out_file,
    overwrite_tex    = TRUE)

  invisible(out_file)
}


# --------------------------------------------------------------------------------------------
# Function: latex_distance_band_table
#
#' @param bands_dt   data.table from compute_distance_band_summary(), stacked across
#                    cities. Needs city, band, statistic and value_label.
#' @param panel_cities character of length two; the cities for Panel A and Panel B.
#
#' @return  character vector; the LaTeX tabular lines, ready for writeLines().
#
#' @details
#   Two city panels in one tabular, which is how the manuscript prints these. Rows follow
#   the reading order of the published table -- counts, then population, then composition,
#   then the schooling range and the two densities -- rather than the alphabetical order
#   the long input happens to carry. A statistic absent for a city is simply not printed
#   for that panel, because the censuses do not all record the same variables.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
latex_distance_band_table <- function(bands_dt, panel_cities) {
  dt <- data.table::as.data.table(bands_dt)

  band_order <- c("All", "Within 1 km", "Within 3 km", "Within 5 km",
                  "Within 10 km", "Within 20 km")

  # Reading order of the published table; anything unlisted follows in place.
  stat_order <- c("Population", "Share of adults", "Mean age", "Share of women",
                  "Share of HH women", "Share of indigenous", "Share of whites",
                  "Share of blacks", "Share of employed", "Share of formal employees",
                  "Share of informal employees", "Mean years of schooling",
                  "Mean income", "Share with no education",
                  "Share with graduate education", "Range years of schooling",
                  "Total population density (pop/km2)",
                  "Average population density (pop/km2)")

  header <- c(
    "\\begin{tabular}{lrrrrrr}",
    "    \\midrule",
    "    \\midrule",
    paste0("    \\textbf{Variable} & ",
           paste0("\\textbf{", band_order, "}", collapse = " & "), " \\\\"),
    "    \\midrule")

  body <- character(0)

  for (i in seq_along(panel_cities)) {
    city_i <- panel_cities[i]
    sub <- dt[city == city_i]

    if (nrow(sub) == 0L) stop("No distance-band rows for city: ", city_i)

    body <- c(body,
              sprintf("    \\multicolumn{7}{l}{\\textbf{Panel %s. %s}} \\\\",
                      LETTERS[i], latex_escape(city_i)),
              "    \\midrule")

    # The unit-count row is named after the unit, so it is found rather than listed.
    count_stat <- grep("^Number of ", unique(sub$statistic), value = TRUE)
    ordered <- c(count_stat, intersect(stat_order, unique(sub$statistic)))

    for (st in ordered) {
      vals <- vapply(band_order, function(b) {
        v <- sub[statistic == st & band == b, value_label]
        if (length(v) == 0L) "" else v[1]
      }, character(1))

      # Only the population cells carry a percent sign, which LaTeX reads as a comment.
      vals <- gsub("%", "\\%", vals, fixed = TRUE)

      body <- c(body, paste0("    ", latex_escape(st), " & ",
                             paste(vals, collapse = " & "), " \\\\"))
    }

    if (i < length(panel_cities)) body <- c(body, "    \\midrule")
  }

  c(header, body, "    \\bottomrule", "    \\bottomrule", "\\end{tabular}")
}


# --------------------------------------------------------------------------------------------
# Function: latex_threshold_exceedance_table
#
#' @param exceed_dt data.table; compute_threshold_exceedance_days() stacked across cities.
#' @param measure   string; "days" for the day counts, "hours" for the mean hours per
#                    exceeding day.
#' @param city_order character; cities in the order the manuscript prints them.
#
#' @return  character vector; a bare LaTeX tabular, ready for writeLines().
#
#' @details
#   Both appendix threshold tables come off the same artefact, so one emitter serves them
#   and they cannot fall out of step. measure = "days" prints eight columns -- at least
#   one
#   and at least two hours, for IT1 and IT2, for each pollutant -- and measure = "hours"
#   prints four, the mean exceeding hours per exceeding day. Each has a city-hour panel
#   and
#   a station-hour panel, in that order.
#
#   An empty cell in the hours table means nothing exceeded that threshold, so no
#   conditional mean exists; it is a result, not a gap.
#
#' @Written_on : September 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
latex_threshold_exceedance_table <- function(exceed_dt, measure = c("days", "hours"),
                                             city_order = c("Bogota", "Santiago",
                                                            "Mexico City",
                                                            "Sao Paulo")) {
  measure <- match.arg(measure)
  dt      <- data.table::as.data.table(exceed_dt)

  series_panels <- c(city_hour = "Panel a. City-hour series",
                     station_hour = "Panel b. Station-hour series")

  if (measure == "days") {
    # Column key: one (column, pollutant, threshold, statistic) tuple per value column.
    keys <- list(c("pm10", "it1", "days_ge1"), c("pm10", "it2", "days_ge1"),
                 c("pm10", "it1", "days_ge2"), c("pm10", "it2", "days_ge2"),
                 c("pm25", "it1", "days_ge1"), c("pm25", "it2", "days_ge1"),
                 c("pm25", "it1", "days_ge2"), c("pm25", "it2", "days_ge2"))

    header <- c(
      "\\begin{tabular}{lcccc|cccc}",
      "    \\toprule",
      "    \\toprule",
      paste0("     & \\multicolumn{2}{c}{At least 1 hour} & ",
             "\\multicolumn{2}{c}{At least 2 hours} & ",
             "\\multicolumn{2}{c}{At least 1 hour} & ",
             "\\multicolumn{2}{c}{At least 2 hours} \\\\"),
      paste0("    City & IT1 PM10 & IT2 PM10 & IT1 PM10 & IT2 PM10 & ",
             "IT1 PM2.5 & IT2 PM2.5 & IT1 PM2.5 & IT2 PM2.5 \\\\"),
      "    \\midrule")

    n_col <- 9L
  } else {
    keys <- list(c("pm10", "it1", "mean_hours"), c("pm10", "it2", "mean_hours"),
                 c("pm25", "it1", "mean_hours"), c("pm25", "it2", "mean_hours"))

    header <- c(
      "\\begin{tabular}{lcccc}",
      "    \\toprule",
      "    \\toprule",
      "     & \\multicolumn{4}{c}{Average hours above:} \\\\",
      "    City & IT1 PM10 & IT2 PM10 & IT1 PM2.5 & IT2 PM2.5 \\\\",
      "    \\midrule")

    n_col <- 5L
  }

  body <- character(0)

  for (s in names(series_panels)) {
    body <- c(body, sprintf("    \\multicolumn{%d}{l}{\\textit{%s}} \\\\",
                            n_col, series_panels[[s]]),
              "    \\midrule")

    for (city_i in city_order) {
      sub <- dt[city == city_i & series == s]

      if (nrow(sub) == 0L) stop("No exceedance rows for city: ", city_i)

      vals <- vapply(keys, function(k) {
        v <- sub[pollutant == k[1] & threshold == k[2], get(k[3])]
        if (length(v) == 0L || is.na(v[1])) {
          ""
        } else if (measure == "days") {
          format(as.integer(v[1]), big.mark = ",")
        } else {
          formatC(v[1], format = "f", digits = 2L)
        }
      }, character(1))

      body <- c(body, paste0("    ", latex_escape(city_i), " & ",
                             paste(vals, collapse = " & "), " \\\\"))
    }

    if (s == "city_hour") body <- c(body, "    \\midrule")
  }

  c(header, body, "    \\bottomrule", "    \\bottomrule", "\\end{tabular}")
}
