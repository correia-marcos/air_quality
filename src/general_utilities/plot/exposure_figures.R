# ============================================================================================
# IDB: Air monitoring — exposure-by-group figures
# ============================================================================================
#' @Goal: Functions for exposure-by-group figures.
#
#' @Description: Exposure across the socioeconomic distribution: quintile levels, kernel
# densities, hours
#   above WHO targets, and the regression gaps with clustered intervals.
#   Sourced by config_utils_plot_tables.R; never sourced directly by a script.
#
#' @Summary:
#   1. plot_exposure_by_quintile
#   2. plot_exposure_by_quintile_with_ci
#   3. plot_kernel_density_by_quintile
#   4. plot_scatter_pollutants
#   5. plot_hours_above_target_by_quintile
#   6. plot_group_ci
#   7. plot_group_levels
#   8. .format_pollutant_label
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# ---------------------------------------------------------------------------
# Function: plot_exposure_by_quintile
#
#' @param exposure_dir string; folder where aggregate_idw_exposure()
#                      wrote its Parquet outputs.
#' @param out_name     string; file prefix used in aggregate_idw_exposure()
#                      (used to locate the correct Parquet files).
#' @param quintile_level string; "geo" or "individual". Must match the
#                      mode used when aggregate_idw_exposure() was called.
#                      Default "geo".
#                      "geo"        — reads {out_name}_idw_exposure.parquet,
#                                     which already carries edu_quintile.
#                      "individual" — additionally reads
#                                     {out_name}_individual_quintiles.parquet
#                                     and joins it to exposure by geo_id.
#' @param pop_col      string; population weight column present in the
#                      Parquet file(s). Default "n" (geo mode) — set to
#                      "fe" for individual mode.
#' @param year_filter  integer or NULL; restrict to one year. If NULL,
#                      all available years are used.
#' @param pollutants   character vector; default c("pm10", "pm25").
#' @param who_it_plot  character vector; which WHO ITs to tabulate.
#                      Default c("it1", "it2").
#' @param city_label   string; city name shown on the plot title.
#' @param quiet        logical; suppress messages. Default FALSE.
#
#' @return  Named list (nothing is written to disk):
#   $plot       — ggplot object. NULL if no matching avg_* columns found.
#   $table_mean — data.table; weighted mean concentration by quintile.
#   $table_hrs  — data.table; weighted mean hours-above-IT by quintile.
#                 NULL if no hrs_d_* columns are present.
#   $data       — data.table; the analysis-ready panel used for all
#                 computations (useful for ad-hoc checks).
#
#' @details
#   INDIVIDUAL MODE JOIN
#   In individual mode, aggregate_idw_exposure() stores geo-level
#   exposure and individual quintile assignments in two separate files.
#   This function joins them by geo_id and then computes weighted means
#   across individuals (using pop_col = "fe") within each quintile ×
#   geo combination before aggregating to the quintile level. This
#   replicates the coauthor's individual-level approach exactly.
#
#   DUAL-AXIS SCALING
#   The PM10/PM2.5 scaling factor for the dual-axis plot is derived
#   automatically as the ratio of their Q5 weighted means, so no
#   hard-coded multiplier is needed.
#
#' @Written_on : 02/02/2026
#' @Written_by : Marcos Paulo
# ---------------------------------------------------------------------------
plot_exposure_by_quintile <- function(
    exposure_dir,
    out_name,
    quintile_level = c("geo", "individual"),
    pop_col        = "n",
    year_filter    = NULL,
    pollutants     = c("pm10", "pm25"),
    who_it_plot    = c("it1", "it2"),
    city_label     = "",
    quiet          = FALSE
) {
  
  # -------------------------------------------------------------------------
  # 0. Dependencies
  # -------------------------------------------------------------------------
  pkgs <- c("arrow", "data.table", "ggplot2")
  quintile_level <- match.arg(quintile_level)
  
  # 1. Locate Parquet files
  # -------------------------------------------------------------------------
  exp_pq <- file.path(
    exposure_dir,
    paste0(out_name, "_idw_exposure.parquet")
  )
  ind_pq <- file.path(
    exposure_dir,
    paste0(out_name, "_individual_quintiles.parquet")
  )
  
  if (!file.exists(exp_pq))
    stop("Exposure Parquet not found:\n  ", exp_pq)
  
  if (quintile_level == "individual" && !file.exists(ind_pq))
    stop(
      "Individual quintiles Parquet not found:\n  ", ind_pq,
      "\n  Was aggregate_idw_exposure() called with ",
      "quintile_level = 'individual'?"
    )
  
  # 2. Load data
  # -------------------------------------------------------------------------
  if (!quiet) message("[plot] Loading exposure data ...")
  
  dt <- data.table::as.data.table(
    arrow::read_parquet(exp_pq)
  )
  
  # Ensure geo_id is character for safe joining
  dt[, geo_id := as.character(geo_id)]
  
  if (quintile_level == "individual") {
    
    if (!quiet) message("[plot] Loading individual quintiles ...")
    ind <- data.table::as.data.table(
      arrow::read_parquet(ind_pq)
    )
    ind[, geo_id := as.character(geo_id)]
    
    # Join: every individual gets the exposure of their geo unit.
    # pop_col (expansion factor, "fe") is already in ind.
    # Keep only geo_id + edu_quintile + pop_col from ind so we
    # don't carry all microdata columns into the exposure panel.
    ind_slim <- ind[
      ,
      .SD,
      .SDcols = unique(c("geo_id", "edu_quintile", pop_col))
    ]
    
    # One row per individual × year (cartesian: each person gets
    # the exposure for every year available in the exposure panel).
    dt <- merge(
      dt,
      ind_slim,
      by     = "geo_id",
      all.x  = FALSE,   # drop geo units with no matched individuals
      allow.cartesian = TRUE
    )
    
    if (nrow(dt) == 0L)
      stop(
        "Join between exposure and individual quintiles is empty. ",
        "Check that geo_id types match in both Parquet files."
      )
    
  } else {
    # GEO MODE: edu_quintile already present in the exposure Parquet
    if (!"edu_quintile" %in% names(dt))
      stop(
        "'edu_quintile' not found in ", exp_pq, ". ",
        "Was aggregate_idw_exposure() called with ",
        "quintile_level = 'geo'?"
      )
    if (!pop_col %in% names(dt))
      stop(
        "pop_col '", pop_col, "' not found in exposure Parquet."
      )
  }

  # 3. Optional year filter
  # -------------------------------------------------------------------------
  if (!is.null(year_filter)) {
    dt <- dt[year == year_filter]
    if (nrow(dt) == 0L)
      stop("No data for year_filter = ", year_filter)
    yr_label <- as.character(year_filter)
  } else {
    yr_label <- paste0(min(dt$year), "-", max(dt$year))
  }
  
  dt <- dt[!is.na(edu_quintile)]
  
  # 4. Identify columns to summarise
  # -------------------------------------------------------------------------
  avg_cols <- intersect(
    paste0("avg_", pollutants),
    names(dt)
  )
  hrs_pat  <- paste0(
    "hrs_d_(", paste(pollutants, collapse = "|"), ")_(",
    paste(who_it_plot, collapse = "|"), ")"
  )
  hrs_cols <- grep(hrs_pat, names(dt), value = TRUE)
  
  if (length(avg_cols) == 0L)
    stop(
      "No avg_* columns found for pollutants: ",
      paste(pollutants, collapse = ", ")
    )
  
  # 5. Weighted means by education quintile
  # -------------------------------------------------------------------------
  .wm <- function(x, w) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(x[ok] * w[ok]) / sum(w[ok])
  }
  
  mean_tbl <- dt[
    !is.na(edu_quintile),
    lapply(
      stats::setNames(avg_cols, avg_cols),
      function(col) .wm(get(col), get(pop_col))
    ),
    by = edu_quintile
  ]
  data.table::setorder(mean_tbl, edu_quintile)
  
  hrs_tbl <- NULL
  if (length(hrs_cols) > 0L) {
    hrs_tbl <- dt[
      !is.na(edu_quintile),
      lapply(
        stats::setNames(hrs_cols, hrs_cols),
        function(col) .wm(get(col), get(pop_col))
      ),
      by = edu_quintile
    ]
    data.table::setorder(hrs_tbl, edu_quintile)
  }
  
  # 6. Build plot
  # -------------------------------------------------------------------------
  p <- NULL
  
  has_pm10 <- "avg_pm10" %in% names(mean_tbl)
  has_pm25 <- "avg_pm25" %in% names(mean_tbl)
  
  if (has_pm10 && has_pm25) {
    
    # Auto-scaling from Q5 means — no hard-coded multipliers
    q5_pm10 <- mean_tbl[edu_quintile == 5L, avg_pm10]
    q5_pm25 <- mean_tbl[edu_quintile == 5L, avg_pm25]
    scale_f  <- if (
      length(q5_pm10) > 0L && length(q5_pm25) > 0L &&
      !is.na(q5_pm25)       && q5_pm25 > 0
    ) {
      q5_pm10 / q5_pm25
    } else {
      2   # sensible fallback for LAC cities
    }
    
    pd <- mean_tbl[
      ,
      .(edu_quintile, avg_pm10, avg_pm25)
    ]
    
    p <- ggplot2::ggplot(
      pd,
      ggplot2::aes(x = factor(edu_quintile))
    ) +
      
      # PM10 — left axis
      ggplot2::geom_line(
        ggplot2::aes(
          y = avg_pm10, linetype = "PM10", group = 1
        ),
        color = "black", linewidth = 0.9
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = avg_pm10),
        color = "black", size = 2.2
      ) +
      
      # PM2.5 — scaled for display; right axis label un-scales it
      ggplot2::geom_line(
        ggplot2::aes(
          y = avg_pm25 * scale_f,
          linetype = "PM2.5", group = 1
        ),
        color = "black", linewidth = 0.9
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = avg_pm25 * scale_f),
        color = "black", size = 2.2
      ) +
      
      ggplot2::scale_y_continuous(
        name = expression(PM[10] ~ "(μg/m³)"),
        sec.axis = ggplot2::sec_axis(
          ~ . / scale_f,
          name = expression(PM[2.5] ~ "(μg/m³)")
        )
      ) +
      ggplot2::scale_linetype_manual(
        values = c("PM10" = "solid", "PM2.5" = "dashed")
      ) +
      ggplot2::labs(
        x        = "Education quintile",
        linetype = "Pollutant",
        title    = city_label,
        subtitle = yr_label
      ) +
      ggplot2::theme_minimal(
        base_family = "Palatino", base_size = 12
      ) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position  = "bottom",
        legend.title     = ggplot2::element_text(size = 11),
        legend.text      = ggplot2::element_text(size = 11),
        axis.title       = ggplot2::element_text(size = 13),
        axis.text        = ggplot2::element_text(size = 11),
        plot.title       = ggplot2::element_text(
          size = 13, face = "bold"
        )
      )
    
  } else if (length(avg_cols) >= 1L) {
    
    # Single-pollutant fallback
    poll <- sub("avg_", "", avg_cols[[1]])
    pd   <- mean_tbl[
      ,
      .(edu_quintile, value = get(avg_cols[[1]]))
    ]
    
    p <- ggplot2::ggplot(
      pd,
      ggplot2::aes(
        x = factor(edu_quintile), y = value, group = 1
      )
    ) +
      ggplot2::geom_line(
        color = "black", linewidth = 0.9
      ) +
      ggplot2::geom_point(color = "black", size = 2.2) +
      ggplot2::labs(
        x        = "Education quintile",
        y        = paste0(toupper(poll), " mean (μg/m³)"),
        title    = city_label,
        subtitle = yr_label
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank()
      )
  }

  # 7. Return (caller decides if / how to save)
  # -------------------------------------------------------------------------
  invisible(list(
    plot       = p,
    table_mean = mean_tbl,
    table_hrs  = hrs_tbl,
    data       = dt
  ))
}


# --------------------------------------------------------------------------------------------
# Function: plot_exposure_by_quintile_with_ci
#
#' @param ci_table   data.table as produced by compute_exposure_ci_regression().
#                    Must contain columns: outcome, pollutant, quintile, estimate,
#                    ci_low, ci_high.
#' @param outcome    string; which outcome to plot (e.g. "avg" for concentration,
#                    "hrs_d_it1" for hours above WHO IT1). Must match a value of
#                    `ci_table$outcome`.
#' @param pollutant  string; filter to one pollutant. Default "pm25".
#' @param city_label string; shown as plot title.
#' @param y_label    string|NULL; y-axis title. If NULL, a sensible default is picked
#                    from `outcome`/`pollutant`.
#' @param color_line string; line/point color. Default "black".
#
#' @return  ggplot2 object. Error bars are 95% CIs (or whatever was used upstream).
#
#' @Purpose: Rebuild of the quintile plots in legacy 4_exposure_plots_*_regCI.R.
#           Designed to pair 1-to-1 with compute_exposure_ci_regression().
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_exposure_by_quintile_with_ci <- function(
    ci_table,
    outcome,
    pollutant   = "pm25",
    city_label  = "",
    y_label     = NULL,
    color_line  = "black"
) {
  stopifnot(is.data.frame(ci_table),
            all(c("outcome","pollutant","quintile","estimate","ci_low","ci_high")
                %in% names(ci_table)))
  # Rename locals so the data.table i-expression below is not shadowed
  # by the matching column names.
  oc_  <- outcome
  pol_ <- pollutant
  dt <- data.table::as.data.table(ci_table)
  dt <- dt[outcome == oc_ & pollutant == pol_]
  if (nrow(dt) == 0L)
    stop("No rows match outcome='", oc_, "' & pollutant='", pol_, "'.")
  data.table::setorder(dt, quintile)
  
  if (is.null(y_label)) {
    y_label <- if (startsWith(outcome, "avg"))
      bquote(.(toupper(pollutant)) ~ "(μg/m³)")
    else if (grepl("^hrs_d", outcome))
      paste0("Hours above ", toupper(sub("hrs_d_", "", outcome)))
    else outcome
  }
  
  ggplot2::ggplot(dt, ggplot2::aes(x = factor(quintile), y = estimate, group = 1)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      width = 0.15, linewidth = 0.6, color = color_line
    ) +
    ggplot2::geom_line(linewidth = 0.9, color = color_line) +
    ggplot2::geom_point(size = 2.3, color = color_line) +
    ggplot2::labs(
      x     = "Education quintile",
      y     = y_label,
      title = city_label
    ) +
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold")
    )
}


# --------------------------------------------------------------------------------------------
# Function: plot_kernel_density_by_quintile
#
#' @param exposure_dir   string; folder with aggregate_idw_exposure() outputs.
#' @param out_name       string; prefix (same as in aggregate_idw_exposure()).
#' @param pollutant      string; one of the columns "avg_<pollutant>" in the file.
#                         Default "pm25".
#' @param quintile_level string; "geo" or "individual" (see aggregate_idw_exposure()).
#' @param pop_col        string; weight column. Default "n".
#' @param year_filter    integer|NULL; restrict to one year.
#' @param city_label     string; plot title.
#' @param bw_adjust      numeric; ggplot2::geom_density(adjust = ...). Default 1.
#' @param x_trim_q       numeric in (0,1); trim x above this weighted quantile to
#                         avoid long tails dominating the plot. Default 0.995.
#
#' @return  ggplot2 object (overlaid weighted kernel densities coloured by
#           education quintile).
#
#' @Purpose: Rebuild of inputs/1_kernel_plots_quintiles_3km.R / _20km.R.
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_kernel_density_by_quintile <- function(
    exposure_dir,
    out_name,
    pollutant      = "pm25",
    quintile_level = c("geo", "individual"),
    pop_col        = "n",
    year_filter    = NULL,
    city_label     = "",
    bw_adjust      = 1,
    x_trim_q       = 0.995
) {
  quintile_level <- match.arg(quintile_level)
  exp_pq <- file.path(exposure_dir, paste0(out_name, "_idw_exposure.parquet"))
  ind_pq <- file.path(exposure_dir, paste0(out_name, "_indiv_quintiles.parquet"))
  if (!file.exists(exp_pq)) stop("Exposure Parquet not found: ", exp_pq)
  
  dt <- data.table::as.data.table(arrow::read_parquet(exp_pq))
  dt[, geo_id := as.character(geo_id)]
  if (!is.null(year_filter)) dt <- dt[year == year_filter]
  
  if (quintile_level == "individual") {
    if (!file.exists(ind_pq)) stop("Individual file missing: ", ind_pq)
    ind <- data.table::as.data.table(arrow::read_parquet(ind_pq))
    ind[, geo_id := as.character(geo_id)]
    dt <- merge(dt, ind[, .SD, .SDcols = c("geo_id","edu_quintile", pop_col)],
                by = "geo_id", allow.cartesian = TRUE)
  }
  
  x_col <- paste0("avg_", pollutant)
  if (!x_col %in% names(dt)) stop("Column ", x_col, " not found.")
  dt <- dt[!is.na(get(x_col)) & !is.na(edu_quintile)
           & !is.na(get(pop_col)) & get(pop_col) > 0]
  
  # Trim the upper tail by weighted quantile
  xs <- dt[[x_col]]
  ws <- dt[[pop_col]]
  ord <- order(xs); xs_s <- xs[ord]; ws_s <- ws[ord]
  cutoff <- xs_s[which(cumsum(ws_s) / sum(ws_s) >= x_trim_q)[1L]]
  if (is.finite(cutoff)) dt <- dt[get(x_col) <= cutoff]
  
  ggplot2::ggplot(
    dt,
    ggplot2::aes(
      x      = .data[[x_col]],
      weight = .data[[pop_col]],
      colour = factor(edu_quintile),
      fill   = factor(edu_quintile)
    )
  ) +
    ggplot2::geom_density(alpha = 0.15, adjust = bw_adjust, linewidth = 0.8) +
    ggplot2::scale_colour_viridis_d(name = "Edu. quintile", option = "D") +
    ggplot2::scale_fill_viridis_d(name = "Edu. quintile", option = "D") +
    ggplot2::labs(
      x     = bquote(.(toupper(pollutant)) ~ "(μg/m³)"),
      y     = "Density",
      title = city_label
    ) +
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 13) +
    ggplot2::theme(
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold")
    )
}


# --------------------------------------------------------------------------------------------
# Function: plot_scatter_pollutants
#
#' @param arrow_dir   string; Arrow dataset (hourly).
#' @param x_pol       string; pollutant on x-axis. Default "pm10".
#' @param y_pol       string; pollutant on y-axis. Default "pm25".
#' @param city_label  string; shown in plot title.
#' @param year_filter integer|NULL; restrict to one year.
#' @param by_station  logical; facet by station if TRUE (and few enough stations).
#                      Default FALSE.
#' @param sample_n    integer; random subsample of hourly points for plotting (rendering
#                      millions of points is slow). Default 50000. Use NA for full data.
#' @param point_alpha numeric in (0,1]. Default 0.3.
#' @param add_45      logical; overlay the y = x reference line. Default TRUE.
#' @param mem_gb      numeric; DuckDB memory ceiling in GB. Default 4.
#
#' @return  ggplot2 object (scatter + optional facets + 45° reference).
#
#' @Purpose: Rebuild of the scatter plots in legacy 6_scatter_plots.do.
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_scatter_pollutants <- function(
    arrow_dir,
    x_pol       = "pm10",
    y_pol       = "pm25",
    city_label  = "",
    year_filter = NULL,
    by_station  = FALSE,
    sample_n    = 50000L,
    point_alpha = 0.3,
    add_45      = TRUE,
    mem_gb      = 4
) {
  stopifnot(dir.exists(arrow_dir))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(",
    glob_q, ", hive_partitioning = true);"
  ))
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present  <- tolower(col_info$name)
  x_pol <- tolower(x_pol); y_pol <- tolower(y_pol)
  if (!all(c(x_pol, y_pol) %in% present))
    stop("Requested pollutant columns not found in dataset.")
  
  yr_filter_sql <- if (is.null(year_filter)) ""
  else sprintf("AND EXTRACT(year FROM datetime) = %d", as.integer(year_filter))
  sample_sql <- if (is.na(sample_n) || is.null(sample_n)) ""
  else sprintf("USING SAMPLE %d ROWS", as.integer(sample_n))
  
  q <- sprintf(
    "SELECT station, %s AS x, %s AS y
     FROM pollution
     WHERE %s IS NOT NULL AND %s IS NOT NULL %s
     %s;",
    x_pol, y_pol, x_pol, y_pol, yr_filter_sql, sample_sql
  )
  d <- data.table::as.data.table(DBI::dbGetQuery(con, q))
  if (nrow(d) == 0L) stop("No data returned.")
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = point_alpha, size = 0.6) +
    ggplot2::labs(
      x     = bquote(.(toupper(x_pol)) ~ "(μg/m³)"),
      y     = bquote(.(toupper(y_pol)) ~ "(μg/m³)"),
      title = city_label
    ) +
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold")
    )
  
  if (add_45) {
    p <- p + ggplot2::geom_abline(slope = 1, intercept = 0,
                                  linetype = "dashed", linewidth = 0.4)
  }
  if (isTRUE(by_station) && data.table::uniqueN(d$station) <= 30L) {
    p <- p + ggplot2::facet_wrap(~ station, scales = "free")
  }
  p
}


# --------------------------------------------------------------------------------------------
# Function: plot_hours_above_target_by_quintile
#
#' @param exposure_dir   string; folder with aggregate_idw_exposure() outputs.
#' @param out_name       string; prefix used in aggregate_idw_exposure().
#' @param quintile_level string; "geo" or "individual".
#' @param pop_col        string; weight column. Default "n".
#' @param pollutant      string; "pm10" or "pm25". Default "pm25".
#' @param who_it         string; which interim target. Default "it1".
#' @param year_filter    integer|NULL; restrict to one year.
#' @param city_label     string; plot title.
#' @param bar_fill       string; fill color. Default "grey35".
#
#' @return  ggplot2 object — bar chart of population-weighted mean hours above the
#           requested WHO interim target, by education quintile.
#
#' @Purpose: Complements plot_exposure_by_quintile() with a WHO-target view,
#           which is the second panel in the legacy 4_exposure_plots_*_PM.R scripts.
#
#' @Written_on : 17/04/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_hours_above_target_by_quintile <- function(
    exposure_dir,
    out_name,
    quintile_level = c("geo", "individual"),
    pop_col        = "n",
    pollutant      = "pm25",
    who_it         = "it1",
    year_filter    = NULL,
    city_label     = "",
    bar_fill       = "grey35"
) {
  quintile_level <- match.arg(quintile_level)
  exp_pq <- file.path(exposure_dir, paste0(out_name, "_idw_exposure.parquet"))
  ind_pq <- file.path(exposure_dir, paste0(out_name, "_indiv_quintiles.parquet"))
  if (!file.exists(exp_pq)) stop("Exposure Parquet not found: ", exp_pq)
  
  dt <- data.table::as.data.table(arrow::read_parquet(exp_pq))
  dt[, geo_id := as.character(geo_id)]
  if (!is.null(year_filter)) dt <- dt[year == year_filter]
  
  if (quintile_level == "individual") {
    if (!file.exists(ind_pq)) stop("Individual file missing: ", ind_pq)
    ind <- data.table::as.data.table(arrow::read_parquet(ind_pq))
    ind[, geo_id := as.character(geo_id)]
    dt <- merge(dt, ind[, .SD, .SDcols = c("geo_id","edu_quintile", pop_col)],
                by = "geo_id", allow.cartesian = TRUE)
  }
  
  y_col <- paste0("hrs_d_", pollutant, "_", who_it)
  if (!y_col %in% names(dt))
    stop("Column ", y_col, " not in exposure file — re-run aggregate_idw_exposure() ",
         "with this interim target.")
  dt <- dt[!is.na(get(y_col)) & !is.na(edu_quintile)
           & !is.na(get(pop_col)) & get(pop_col) > 0]
  
  agg <- dt[,
            .(hrs = sum(get(y_col) * get(pop_col)) / sum(get(pop_col))),
            by = edu_quintile]
  data.table::setorder(agg, edu_quintile)
  
  ggplot2::ggplot(agg, ggplot2::aes(x = factor(edu_quintile), y = hrs)) +
    ggplot2::geom_col(fill = bar_fill, width = 0.7) +
    ggplot2::labs(
      x     = "Education quintile",
      y     = sprintf("Hours above %s (%s)", toupper(who_it), toupper(pollutant)),
      title = city_label
    ) +
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold")
    )
}


# ------------------------------------------------------------------------------------
# Function: plot_group_ci
#
#' @param ci_table        data.table from compute_exposure_regressions(). Must contain
#                         outcome, pollutant, group, estimate, ci_low, ci_high.
#' @param outcome         string; outcome to plot, e.g. "hrs_d_it1".
#' @param pollutant       string vector; pollutant(s) to plot. Use c("pm25", "pm10")
#                         to place both pollutants in the same figure.
#' @param group_label     string; x-axis label, e.g. "Education quintile".
#' @param city_label      string; plot title.
#' @param y_label         string or NULL; y-axis title. If NULL, a default is derived.
#' @param pollutant_colors named character vector with pollutant colors.
#' @param color_line      string or NULL; backward-compatible single-pollutant color.
#
#' @return  ggplot2 object. Error bars are the intervals already in ci_table.
#
#' @details
#   Draws regression gaps relative to the base group with confidence intervals.
#   The function now supports one or more pollutants in the same plot. This is
#   useful for exceedance-hour outcomes, where PM2.5 and PM10 should be compared
#   directly. By default, PM2.5 is dark red and PM10 is black.
#   When ci_table carries n_clusters and n_coef, the caption reports both: a G = 6
#   interval is otherwise drawn exactly like a G = 37,000 one. No threshold is applied,
#   because none is defensible -- the caption informs, it does not adjudicate.
#
#' @Written_on : June 2026
#' @Written_by : Marcos Paulo
#' @Updated_on : June 2026
# ------------------------------------------------------------------------------------
plot_group_ci <- function(ci_table,
                          outcome,
                          pollutant = c("pm25", "pm10"),
                          group_label = "Group",
                          city_label = "",
                          y_label = NULL,
                          pollutant_colors = c(pm25 = "darkred",
                                               pm10 = "black"),
                          color_line = NULL) {

  # 0. Dependencies and input check
  # -----------------------------------------------------------------------
  req_cols <- c("outcome", "pollutant", "group", "estimate", "ci_low", "ci_high")
  if (!all(req_cols %in% names(ci_table))) {
    stop("ci_table must contain: ", paste(req_cols, collapse = ", "), ".")
  }
  
  # 1. Subset to one outcome and one or more pollutants
  # -----------------------------------------------------------------------
  oc_ <- outcome
  pol_ <- pollutant
  
  dt <- data.table::as.data.table(ci_table)[
    outcome == oc_ & pollutant %in% pol_
  ]
  
  if (nrow(dt) == 0L) {
    stop("No rows match outcome='", oc_, "' and the requested pollutant(s).")
  }
  
  data.table::setorder(dt, pollutant, group)
  
  # Preserve the requested pollutant order where possible.
  dt[, pollutant_label := factor(
    pollutant,
    levels = pol_,
    labels = .format_pollutant_label(pol_)
  )]
  
  # 2. Default y-axis label derived from the outcome name
  # -----------------------------------------------------------------------
  if (is.null(y_label)) {
    if (grepl("^avg", outcome)) {
      y_label <- "Normalized concentration gap"
    } else if (grepl("^hrs_d", outcome)) {
      y_label <- paste0(
        "Normalized hours above ",
        toupper(sub("hrs_d_", "", outcome))
      )
    } else {
      y_label <- outcome
    }
  }
  
  # 3. Keep backward compatibility with old single-color calls
  # -----------------------------------------------------------------------
  if (!is.null(color_line) && length(pol_) == 1L) {
    pollutant_colors[pol_] <- color_line
  }
  
  color_values <- pollutant_colors[intersect(pol_, names(pollutant_colors))]
  names(color_values) <- .format_pollutant_label(names(color_values))

  show_legend <- length(unique(dt$pollutant)) > 1L

  # 3b. Report the cluster count the intervals rest on
  # -----------------------------------------------------------------------
  # No threshold used: no defensible cutoff for "too few clusters" universally accepted. 
  # Caption shows G and the coefficient count and lets reader judge. Not required to run.
  cap <- NULL

  if (all(c("n_clusters", "n_coef") %in% names(dt))) {
    g_dt <- dt[, .(g = max(n_clusters), k = max(n_coef)), by = pollutant_label]

    # trim = TRUE, otherwise format() pads the vector to a common width and the
    # smaller count reaches the caption as "G =  6".
    cap <- paste0("Clusters (coefficients): ",
                  paste(sprintf("%s G = %s (k = %d)", g_dt$pollutant_label,
                                format(g_dt$g, big.mark = ",", trim = TRUE),
                                g_dt$k),
                        collapse = "; "), ".")
  }

  # 4. Build the plot
  # -----------------------------------------------------------------------
  ggplot2::ggplot(
    dt,
    ggplot2::aes(
      x = factor(group),
      y = estimate,
      color = pollutant_label,
      group = pollutant_label
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      width = 0.15,
      linewidth = 0.6
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
    ggplot2::labs(
      x = group_label,
      y = y_label,
      title = city_label,
      color = "Pollutant",
      caption = cap
    ) +
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = if (show_legend) "bottom" else "none",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 8, colour = "grey35",
                                           hjust = 0)
    )
}


# ------------------------------------------------------------------------------------
# Function: plot_group_levels
#
#' @param summary_table    data.table from compute_exposure_summaries(). Must contain
#                          outcome, pollutant, group, weighted_mean.
#' @param group_label      string; x-axis label, e.g. "Education quintile".
#' @param city_label       string; plot title.
#' @param year_label       string; subtitle, e.g. "2023".
#' @param base_group       integer; top group used to derive the dual-axis scale.
#' @param pollutant_colors named character vector with pollutant colors.
#
#' @return  ggplot2 object, or NULL if the mean concentration columns are absent.
#
#' @details
#   Draws population-weighted mean PM10 and PM2.5 by socioeconomic group on a
#   dual-axis figure. The right-axis scale factor is the ratio of the two
#   pollutants' base-group means, so no multiplier is hard-coded. By default,
#   PM2.5 is dark red and PM10 is black.
#
#' @Written_on : June 2026
#' @Written_by : Marcos Paulo
#' @Updated_on : June 2026
# ------------------------------------------------------------------------------------
plot_group_levels <- function(summary_table,
                              group_label = "Group",
                              city_label = "",
                              year_label = "",
                              base_group = NULL,
                              pollutant_colors = c(pm25 = "darkred",
                                                   pm10 = "black")) {
  
  # 0. Dependencies and input check
  # -----------------------------------------------------------------------
  req_cols <- c("outcome", "pollutant", "group", "weighted_mean")
  if (!all(req_cols %in% names(summary_table))) {
    stop("summary_table must contain: ", paste(req_cols, collapse = ", "), ".")
  }
  
  # 1. Keep the annual-mean concentration rows
  # -----------------------------------------------------------------------
  dt <- data.table::as.data.table(summary_table)[outcome == "avg"]
  
  if (nrow(dt) == 0L) {
    return(NULL)
  }
  
  wide <- data.table::dcast(dt, group ~ pollutant, value.var = "weighted_mean")
  data.table::setorder(wide, group)
  
  has_pm10 <- "pm10" %in% names(wide)
  has_pm25 <- "pm25" %in% names(wide)
  
  # 2. Dual-axis layout when both pollutants are present
  # -----------------------------------------------------------------------
  if (has_pm10 && has_pm25) {
    if (is.null(base_group)) {
      base_group <- max(wide$group, na.rm = TRUE)
    }
    
    base_pm10 <- wide[group == base_group, pm10]
    base_pm25 <- wide[group == base_group, pm25]
    
    scale_f <- if (length(base_pm10) > 0L && length(base_pm25) > 0L &&
                   !is.na(base_pm25) && base_pm25 > 0) {
      base_pm10 / base_pm25
    } else {
      2
    }
    
    ggplot2::ggplot(wide, ggplot2::aes(x = factor(group))) +
      ggplot2::geom_line(
        ggplot2::aes(y = pm10, color = "PM10", group = 1),
        linewidth = 0.9
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = pm10, color = "PM10"),
        size = 2.2
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = pm25 * scale_f, color = "PM2.5", group = 1),
        linewidth = 0.9
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = pm25 * scale_f, color = "PM2.5"),
        size = 2.2
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "PM10" = pollutant_colors[["pm10"]],
          "PM2.5" = pollutant_colors[["pm25"]]
        )
      ) +
      ggplot2::scale_y_continuous(
        name = expression(PM[10] ~ "(\u00b5g/m\u00b3)"),
        sec.axis = ggplot2::sec_axis(
          ~ . / scale_f,
          name = expression(PM[2.5] ~ "(\u00b5g/m\u00b3)")
        )
      ) +
      ggplot2::labs(
        x = group_label,
        color = "Pollutant",
        title = city_label,
        subtitle = year_label
      ) +
      ggplot2::theme_minimal(base_family = "Palatino", base_size = 12) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        plot.title = ggplot2::element_text(size = 13, face = "bold")
      )
  } else {
    # 3. Single-pollutant fallback when only one pollutant is present
    # ---------------------------------------------------------------------
    poll <- if (has_pm10) "pm10" else names(wide)[2]
    color_poll <- pollutant_colors[[poll]]
    
    ggplot2::ggplot(
      wide,
      ggplot2::aes(x = factor(group), y = get(poll), group = 1)
    ) +
      ggplot2::geom_line(color = color_poll, linewidth = 0.9) +
      ggplot2::geom_point(color = color_poll, size = 2.2) +
      ggplot2::labs(
        x = group_label,
        y = paste0(toupper(poll), " (\u00b5g/m\u00b3)"),
        title = city_label,
        subtitle = year_label
      ) +
      ggplot2::theme_minimal(base_family = "Palatino", base_size = 12) +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
}


.format_pollutant_label <- function(pollutants) {
  data.table::fcase(
    pollutants == "pm25", "PM2.5",
    pollutants == "pm10", "PM10",
    default = toupper(pollutants)
  )
}


# --------------------------------------------------------------------------------------------
# Function: save_plot_pdf
#
#' @param plot_obj ggplot object, or NULL.
#' @param path    string; destination PDF path.
#' @param width   numeric; figure width in inches. Default 6.
#' @param height  numeric; figure height in inches. Default 4.5.
#
#' @return  invisible NULL. Writes the PDF, or does nothing when plot_obj is NULL.
#
#' @details
#   The NULL case is not an error: the plot builders return NULL when a city lacks the
#   columns
#   a figure needs, and skipping quietly keeps one missing city from stopping a whole run.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
save_plot_pdf <- function(plot_obj, path, width = 6, height = 4.5) {
  if (is.null(plot_obj)) {
    return(invisible(NULL))
  }

  ggplot2::ggsave(filename = path, plot = plot_obj, device = cairo_pdf,
                  width = width, height = height, dpi = 300, bg = "white")

  invisible(NULL)
}


# --------------------------------------------------------------------------------------------
# Function: exposure_group_axis_label
#
#' @param socio_var string; "income" or "education".
#' @param group_type string; "quintile" or "decile".
#
#' @return  string; x-axis label, e.g. "Income quintile".
#
#' @details
#   Keys on both fields because the two no longer imply each other: CDMX income runs on
#   quintiles while Sao Paulo income runs on deciles, so keying on group_type alone would
#   label a CDMX income figure "Education quintile".
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
exposure_group_axis_label <- function(socio_var, group_type) {
  var_label <- if (identical(socio_var, "income")) "Income" else "Education"
  paste(var_label, group_type)
}


# --------------------------------------------------------------------------------------------
# Function: save_exposure_ci_figures
#
#' @param ci_dt      data.table; CI estimates from estimate_exposure.R.
#' @param tag        string; grouping tag used in the file name, e.g. "education".
#' @param out_dir    string; folder for the PDFs.
#' @param city_labels named character; city -> display label.
#' @param city_files named character; city -> file-safe name.
#
#' @return  invisible NULL. Writes one PDF per city x outcome.
#
#' @details
#   Exceedance-hour outcomes carry both pollutants, so PM2.5 and PM10 are drawn in the
#   same
#   figure and the file name records both. Labels and paths are arguments, not captured
#   from
#   the calling script, so the function is readable on its own.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
save_exposure_ci_figures <- function(ci_dt, tag, out_dir, city_labels, city_files) {
  combos <- unique(ci_dt[!is.na(city) & !is.na(outcome),
                         .(city, outcome, group_type, socioeconomic_var)])

  for (j in seq_len(nrow(combos))) {
    city_j <- combos$city[j]
    out_j  <- combos$outcome[j]
    poll_j <- intersect(c("pm25", "pm10"),
                        ci_dt[city == city_j & outcome == out_j, unique(pollutant)])

    if (length(poll_j) == 0L) next

    p <- plot_group_ci(
      ci_table    = ci_dt[city == city_j],
      outcome     = out_j,
      pollutant   = poll_j,
      group_label = exposure_group_axis_label(combos$socioeconomic_var[j],
                                              combos$group_type[j]),
      city_label  = city_labels[[city_j]])

    poll_tag <- if (length(poll_j) > 1L) "pm25_pm10" else poll_j

    fname <- sprintf("%s_%s_%s_%s_ci.pdf", city_files[[city_j]], tag, out_j, poll_tag)
    save_plot_pdf(p, file.path(out_dir, fname))
  }

  invisible(NULL)
}


# --------------------------------------------------------------------------------------------
# Function: save_exposure_level_figures
#
#' @param sum_dt     data.table; group summaries from estimate_exposure.R.
#' @param tag        string; grouping tag used in the file name, e.g. "education".
#' @param out_dir    string; folder for the PDFs.
#' @param city_labels named character; city -> display label.
#' @param city_files named character; city -> file-safe name.
#
#' @return  invisible NULL. Writes one dual-axis PM10/PM2.5 PDF per city.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
save_exposure_level_figures <- function(sum_dt, tag, out_dir, city_labels, city_files) {
  for (city_j in unique(sum_dt[!is.na(city), city])) {
    sub <- sum_dt[city == city_j]

    p <- plot_group_levels(
      summary_table = sub,
      group_label   = exposure_group_axis_label(sub$socioeconomic_var[1],
                                                sub$group_type[1]),
      city_label    = city_labels[[city_j]],
      year_label    = as.character(sub$year[1]))

    save_plot_pdf(p, file.path(out_dir, sprintf("%s_%s_levels.pdf",
                                                city_files[[city_j]], tag)))
  }

  invisible(NULL)
}
