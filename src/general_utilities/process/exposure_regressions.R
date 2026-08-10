# ============================================================================================
# IDB: Air monitoring — exposure summaries, regressions and coverage
# ============================================================================================
# @Goal: Functions for exposure summaries, regressions and coverage.
#
# @Description: Weighted exposure summaries by socioeconomic group, the regression gaps
# relative to the
#   top group with clustered intervals, and the geographic coverage behind each estimate.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
# @Summary:
#   1. compute_exposure_summaries
#   2. compute_exposure_coverage
#   3. compute_exposure_regressions
#   4. .exposure_merge_geo_group
#   5. .exposure_outcome_cols
#   6. .exposure_parse_outcome
#   7. .exposure_weighted_median
#   8. .exposure_coef_table
#   9. .exposure_fit_one
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: compute_exposure_summaries
#
# @Arg exposure_dt   : data.table; geo-level IDW exposure (one row per geo unit-year).
# @Arg individual_dt : data.table; geo-by-group population/expansion weights.
# @Arg geo_id_col    : string; geographic identifier column. Default "geo_id".
# @Arg pop_col       : string; population or expansion-weight column. Default "n".
# @Arg group_col     : string; socioeconomic group column. Default "edu_quintile".
# @Arg group_values  : integer vector; valid groups, e.g. 1:5.
# @Arg pollutants    : character vector; pollutants to keep, e.g. pm10/pm25.
# @Arg outcome_pattern : string; regex selecting exposure outcome columns.
# @Arg year_filter   : integer or NULL; if set, keeps only this exposure year.
# @Arg quiet         : logical; suppress progress messages. Default FALSE.
#
# @Output : data.table with weighted mean, weighted median, population, and counts
#           by outcome, pollutant, and group.
#
# @Details:
#   Raw exposure levels by socioeconomic group. Merges geo-level exposure with the
#   geo-by-group population, collapses to geo-unit-by-group cells (exposure is
#   constant within a geo unit), then aggregates to group level. Cells are weighted
#   by population so groups reflect the population they represent.
#
# @Written_by : Marcos Paulo
# @Updated_on : June 2026
# --------------------------------------------------------------------------------------------
compute_exposure_summaries <- function(exposure_dt,
                                       individual_dt,
                                       geo_id_col      = "geo_id",
                                       pop_col         = "n",
                                       group_col       = "edu_quintile",
                                       group_values    = 1:5,
                                       pollutants      = c("pm10", "pm25"),
                                       outcome_pattern = "^(avg|hrs_d)_",
                                       year_filter     = NULL,
                                       quiet           = FALSE) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  
  # 1. Merge exposure with the geo-by-group population
  # -----------------------------------------------------------------------
  # Both inputs are in-memory data.tables; copy so we never edit the caller's data.
  dt <- .exposure_merge_geo_group(
    exposure_dt   = exposure_dt,
    individual_dt = individual_dt,
    geo_id_col    = geo_id_col,
    group_col     = group_col,
    pop_col       = pop_col,
    group_values  = group_values,
    year_filter   = year_filter,
    quiet         = quiet
  )
  
  # 2. Pick outcome columns that match the pattern and the pollutants
  # -----------------------------------------------------------------------
  out_cols <- .exposure_outcome_cols(dt, outcome_pattern, pollutants)
  by_cols  <- c(geo_id_col, group_col)
  
  # 3. Collapse to geo-by-group cells (exposure is constant within a geo unit)
  # -----------------------------------------------------------------------
  geo_group <- dt[
    ,
    c(
      list(group_pop = sum(get(pop_col), na.rm = TRUE)),
      lapply(.SD, function(x) stats::weighted.mean(x, get(pop_col), na.rm = TRUE))
    ),
    by = by_cols,
    .SDcols = out_cols
  ]
  
  geo_group <- geo_group[!is.na(group_pop) & group_pop > 0]
  
  # 4. Aggregate each outcome to group-level summaries
  # -----------------------------------------------------------------------
  res <- data.table::rbindlist(
    lapply(out_cols, function(col) {
      meta <- .exposure_parse_outcome(col, pollutants)
      
      tmp <- geo_group[
        !is.na(get(col)),
        .(
          weighted_mean       = stats::weighted.mean(get(col), group_pop, na.rm = TRUE),
          weighted_median     = .exposure_weighted_median(get(col), group_pop),
          weighted_population = sum(group_pop, na.rm = TRUE),
          n_geo_group_cells   = .N,
          n_geo_units         = data.table::uniqueN(get(geo_id_col))
        ),
        by = group_col
      ]
      
      # Stamp outcome/pollutant labels and standardize the group column name.
      data.table::setnames(tmp, group_col, "group")
      tmp[, `:=`(outcome = meta$outcome, pollutant = meta$pollutant,
                 group_col = group_col)]
      tmp
    }),
    fill = TRUE
  )
  
  data.table::setorder(res, outcome, pollutant, group)
  
  if (!quiet) {
    message("[summary] ", length(out_cols), " outcome(s) summarized.")
  }
  
  return(res[])
}


# --------------------------------------------------------------------------------------------
# Function: compute_exposure_coverage
#
# @Arg exposure_dt    : data.table; geo-level IDW exposure (one row per geo unit-year).
# @Arg individual_dt  : data.table; geo-by-group population/expansion weights.
# @Arg geo_station_pq : string; path to the matrix_geo_station_distances.parquet used
#                       to build this city's exposure.
# @Arg geo_id_col     : string; geographic identifier column. Default "geo_id".
# @Arg pop_col        : string; population or expansion-weight column. Default "n".
# @Arg group_col      : string; socioeconomic group column. Default "edu_quintile".
# @Arg group_values   : integer vector; valid groups, e.g. 1:5.
# @Arg pollutants     : character vector; pollutants to report.
# @Arg buffer_km      : numeric; buffer used to build the exposure. Default 3.
# @Arg year_filter     : integer or NULL; if set, keeps only this exposure year.
#
# @Output : data.table with one row per pollutant, tracing how many geographic units
#           survive each stage between the full metro area and the estimation sample.
#
# @Details:
#   Records the attrition that determines the cluster count. A metro geo unit is lost
#   when no station falls inside buffer_km of its representative point, then when no
#   in-buffer station reports the pollutant, then when it has no census row. For CDMX
#   the chain runs 63 -> 18 -> 11 -> 10 (PM10) / 6 (PM2.5), which is why its clustered
#   intervals are fragile. n_geo_estimation is computed through the same merge helper
#   the regressions use, so it must equal the n_clusters they report.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
compute_exposure_coverage <- function(exposure_dt,
                                      individual_dt,
                                      geo_station_pq,
                                      geo_id_col   = "geo_id",
                                      pop_col      = "n",
                                      group_col    = "edu_quintile",
                                      group_values = 1:5,
                                      pollutants   = c("pm10", "pm25"),
                                      buffer_km    = 3,
                                      year_filter  = NULL) {

  # 1. Metro-wide reach: the distance matrix holds every geo unit and station pair
  # -----------------------------------------------------------------------
  dist_dt <- data.table::as.data.table(arrow::read_parquet(geo_station_pq))
  dist_dt[, geo_id := as.character(geo_id)]

  # Same filter the IDW applies: positive distances inside the buffer.
  in_buffer <- dist_dt[!is.na(distance_km) & distance_km > 0 &
                         distance_km <= buffer_km]

  # 2. Exposure stage: geo units that came back with any interpolated hour
  # -----------------------------------------------------------------------
  exp_dt <- data.table::copy(data.table::as.data.table(exposure_dt))

  if (!is.null(year_filter)) {
    exp_dt <- exp_dt[year == year_filter]
  }

  exp_dt[, (geo_id_col) := as.character(get(geo_id_col))]

  # 3. Estimation stage: run the regressions' own merge so the counts cannot drift
  # -----------------------------------------------------------------------
  merged <- .exposure_merge_geo_group(
    exposure_dt   = exp_dt,
    individual_dt = individual_dt,
    geo_id_col    = geo_id_col,
    group_col     = group_col,
    pop_col       = pop_col,
    group_values  = group_values,
    year_filter   = NULL,
    quiet         = TRUE
  )

  # Total metro population, taken from the census side before any exposure filter.
  ind_dt   <- data.table::as.data.table(individual_dt)
  pop_all  <- sum(ind_dt[[pop_col]], na.rm = TRUE)

  # 4. One row per pollutant, since coverage differs by what each station measures
  # -----------------------------------------------------------------------
  res <- data.table::rbindlist(lapply(pollutants, function(poll) {
    avg_col <- paste0("avg_", poll)

    # Geo units holding a value for this pollutant, before and after the census merge.
    geo_poll <- exp_dt[!is.na(get(avg_col)), unique(get(geo_id_col))]
    est_rows <- merged[!is.na(get(avg_col))]
    pop_est  <- est_rows[, sum(get(pop_col), na.rm = TRUE)]

    # buffer_km is deliberately not returned: the caller stamps it alongside the other
    # run labels, and two columns of the same name would silently overwrite each other.
    data.table::data.table(
      pollutant           = poll,
      n_geo_metro         = data.table::uniqueN(dist_dt$geo_id),
      n_station_metro     = data.table::uniqueN(dist_dt$station_id),
      n_geo_in_buffer     = data.table::uniqueN(in_buffer$geo_id),
      n_station_in_buffer = data.table::uniqueN(in_buffer$station_id),
      n_geo_exposure      = data.table::uniqueN(exp_dt[[geo_id_col]]),
      n_geo_pollutant     = length(geo_poll),
      n_geo_estimation    = data.table::uniqueN(est_rows[[geo_id_col]]),
      pop_metro           = pop_all,
      pop_estimation      = pop_est,
      share_pop_estimation = pop_est / pop_all
    )
  }), fill = TRUE)

  return(res[])
}


# --------------------------------------------------------------------------------------------
# Function: compute_exposure_regressions
#
# @Arg exposure_dt   : data.table; geo-level IDW exposure (one row per geo unit-year).
# @Arg individual_dt : data.table or NULL; geo-by-group population/expansion weights.
# @Arg geo_id_col    : string; geographic identifier column. Default "geo_id".
# @Arg pop_col       : string; population or expansion-weight column. Default "n".
# @Arg group_col     : string; socioeconomic group column. Default "edu_quintile".
# @Arg group_values  : integer vector; valid groups, e.g. 1:5.
# @Arg base_group    : integer; omitted reference group. Default max(group_values).
# @Arg pollutants    : character vector; pollutants to keep.
# @Arg outcome_pattern : string; regex selecting exposure outcome columns.
# @Arg year_filter   : integer or NULL; if set, keeps only this exposure year.
# @Arg conf_level    : numeric; confidence level for intervals. Default 0.95.
# @Arg normalized    : logical; if TRUE, divide each outcome by the base-group mean.
# @Arg regression_unit : string; "geo_group" (main), "individual", or "geo".
# @Arg se_type       : string; "cluster_geo" (preferred) or "classic" (legacy CI).
# @Arg quiet         : logical; suppress progress messages. Default FALSE.
#
# @Output : data.table with one row per outcome, pollutant, and group, giving the
#           gap relative to base_group with confidence interval. Carries n_units
#           (cells in the fit), n_clusters (distinct geographic units) and n_coef
#           (coefficients the cluster sandwich has to support).
#
# @Details:
#   Estimates exposure gaps versus base_group. The main paper estimator is
#   regression_unit = "geo_group": collapse merged data to geo-unit-by-group cells,
#   then weight each cell by its population share within group. "individual" runs
#   one row per individual; "geo" runs one row per geo unit (no group merge).
#   classic SEs use the t-distribution; cluster_geo clusters by geographic unit
#   and uses a t(G-1) critical value. With G <= number of coefficients the
#   clustered variance is not identified, so SEs and intervals come back NA with
#   a warning rather than as small numbers.
#
# @Written_by : Marcos Paulo
# @Updated_on : July 2026
# --------------------------------------------------------------------------------------------
compute_exposure_regressions <- function(exposure_dt,
                                         individual_dt   = NULL,
                                         geo_id_col      = "geo_id",
                                         pop_col         = "n",
                                         group_col       = "edu_quintile",
                                         group_values    = 1:5,
                                         base_group      = max(group_values),
                                         pollutants      = c("pm10", "pm25"),
                                         outcome_pattern = "^hrs_d_.*_it[12]$",
                                         year_filter     = NULL,
                                         conf_level      = 0.95,
                                         normalized      = TRUE,
                                         regression_unit = c("geo_group",
                                                             "individual", "geo"),
                                         se_type         = c("cluster_geo",
                                                             "classic"),
                                         quiet           = FALSE) {
  
  # 0. Dependencies and argument checks
  # -----------------------------------------------------------------------
  
  regression_unit <- match.arg(regression_unit)
  se_type         <- match.arg(se_type)
  
  # Clustered SEs need the sandwich package; classic SEs do not.
  
  if (!(conf_level > 0 && conf_level < 1)) {
    stop("`conf_level` must be between 0 and 1.")
  }
  
  if (!base_group %in% group_values) {
    stop("`base_group` must belong to `group_values`.")
  }
  
  # 1. Merge exposure with the geo-by-group population (skip for "geo" unit)
  # -----------------------------------------------------------------------
  dt <- .exposure_merge_geo_group(
    exposure_dt      = exposure_dt,
    individual_dt    = individual_dt,
    geo_id_col       = geo_id_col,
    group_col        = group_col,
    pop_col          = pop_col,
    group_values     = group_values,
    year_filter      = year_filter,
    merge_individual = regression_unit != "geo",
    quiet            = quiet
  )
  
  # 2. Pick outcome columns and fit one model per outcome
  # -----------------------------------------------------------------------
  out_cols <- .exposure_outcome_cols(dt, outcome_pattern, pollutants)
  
  res <- data.table::rbindlist(
    lapply(
      out_cols,
      .exposure_fit_one,
      dt              = dt,
      geo_id_col      = geo_id_col,
      group_col       = group_col,
      pop_col         = pop_col,
      group_values    = group_values,
      base_group      = base_group,
      pollutants      = pollutants,
      regression_unit = regression_unit,
      se_type         = se_type,
      conf_level      = conf_level,
      normalized      = normalized
    ),
    fill = TRUE
  )
  
  if (nrow(res) == 0L) {
    if (!quiet) {
      message("[ci] Warning: insufficient data to fit any exposure models.")
    }
    return(data.table::data.table())
  }
  
  data.table::setorder(res, outcome, pollutant, group)

  # Report G unconditionally rather than against a threshold. There is no defensible
  # cutoff for "too few clusters" -- it depends on cluster-size heterogeneity and the
  # design, not on G alone -- so the function states the count, the coefficients it
  # supports and the t degrees of freedom, and leaves the judgement to the reader.
  g_by_poll <- res[, .(g = max(n_clusters), k = max(n_coef)), by = pollutant]

  if (!quiet) {
    message("[ci] ", length(out_cols), " outcome(s) fit | unit = '",
            regression_unit, "' | se = '", se_type, "' | clusters: ",
            paste(sprintf("%s G=%d (k=%d, df=%d)", g_by_poll$pollutant,
                          g_by_poll$g, g_by_poll$k, g_by_poll$g - 1L),
                  collapse = ", "), ".")
  }

  return(res[])
}


# --------------------------------------------------------------------------------------------
# Internal helpers for the two exposure functions above.
# Kept small and shared so both functions read the data the same way.
# --------------------------------------------------------------------------------------------

# Merge geo-level exposure with the geo-by-group population table.
# Returns a filtered data.table ready for collapsing or fitting.
.exposure_merge_geo_group <- function(exposure_dt, individual_dt, geo_id_col,
                                      group_col, pop_col, group_values,
                                      year_filter = NULL, merge_individual = TRUE,
                                      quiet = FALSE) {

  # Copy so the caller's in-memory tables are never modified.
  dt <- data.table::copy(data.table::as.data.table(exposure_dt))

  # Optional single-year filter.
  if (!is.null(year_filter)) {
    dt <- dt[year == year_filter]
  }

  dt[, (geo_id_col) := as.character(get(geo_id_col))]

  # Attach group population unless this is the geo-only unit.
  if (isTRUE(merge_individual)) {
    ind <- data.table::copy(data.table::as.data.table(individual_dt))
    ind[, (geo_id_col) := as.character(get(geo_id_col))]

    # Keep valid groups with positive weight only.
    ind <- ind[
      get(group_col) %in% group_values &
        !is.na(get(pop_col)) & get(pop_col) > 0,
      .SD,
      .SDcols = c(geo_id_col, group_col, pop_col)
    ]

    # Drop any pre-existing group column in exposure before the merge.
    if (group_col %in% names(dt)) {
      dt[, (group_col) := NULL]
    }

    # This is an inner join: an exposure unit with no census row leaves the
    # sample here. Report the match rate so a silent ID mismatch (zero padding,
    # width) shows up as a number rather than as quietly smaller regressions.
    exp_ids   <- unique(dt[[geo_id_col]])
    n_matched <- length(intersect(exp_ids, unique(ind[[geo_id_col]])))

    if (!quiet) {
      message("[merge] Census match: ", n_matched, " of ", length(exp_ids),
              " exposure geo unit(s) (", length(exp_ids) - n_matched,
              " unmatched).")
    }

    dt <- merge(dt, ind, by = geo_id_col, allow.cartesian = TRUE)
  }

  # Final filter to valid groups with positive weight.
  dt <- dt[
    get(group_col) %in% group_values &
      !is.na(get(pop_col)) & get(pop_col) > 0
  ]
  
  return(dt)
}


# Select exposure outcome columns matching the pattern and the pollutants.
.exposure_outcome_cols <- function(dt, outcome_pattern, pollutants) {
  candidate <- grep(outcome_pattern, names(dt), value = TRUE)
  out_cols  <- candidate[grepl(paste(pollutants, collapse = "|"), candidate)]
  
  if (length(out_cols) == 0L) {
    stop("No outcome columns match `outcome_pattern` and `pollutants`.")
  }
  
  return(out_cols)
}


# Split an outcome column name into its outcome label and pollutant.
.exposure_parse_outcome <- function(col, pollutants) {
  hits <- which(vapply(pollutants, function(p) grepl(p, col, fixed = TRUE),
                       logical(1)))

  # Longest match wins, so a pollutant whose name prefixes another (e.g. "pm2"
  # inside "pm25") cannot claim the column first.
  hit <- hits[which.max(nchar(pollutants[hits]))]
  pollutant <- pollutants[hit]
  
  # Remove the pollutant token and tidy leftover underscores.
  outcome <- sub(paste0("_", pollutant, "_?"), "_", col)
  outcome <- sub("_$", "", sub("^_", "", outcome))
  
  return(list(outcome = outcome, pollutant = pollutant))
}


# Population-weighted median (used for the raw summaries only).
.exposure_weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  x[which(cumsum(w) / sum(w) >= 0.5)[1L]]
}


# Build the coefficient table from a fitted lm with the requested SE type.
.exposure_coef_table <- function(fit, model_dt, se_type, conf_level) {

  # classic: model-based vcov with the t critical value (reproduces confint()).
  if (se_type == "classic") {
    vcov_mat <- stats::vcov(fit)
    crit <- stats::qt(1 - (1 - conf_level) / 2, stats::df.residual(fit))
  }

  # cluster_geo: vcovCL clustered by geo unit. See the @Details of
  # compute_exposure_regressions for why the sandwich is refused when G <= k.
  if (se_type == "cluster_geo") {
    n_clusters <- data.table::uniqueN(model_dt$.cluster_geo)
    n_coef     <- length(stats::coef(fit))

    if (n_clusters <= n_coef) {
      # G <= k: clustered SEs are not identified; return NA instead.
      warning("Only ", n_clusters, " geographic cluster(s) for ", n_coef,
              " coefficients; clustered standard errors are not identified. ",
              "Returning NA standard errors and intervals.", call. = FALSE)

      estimate <- stats::coef(fit)

      return(data.table::data.table(
        term      = names(estimate),
        estimate  = unname(estimate),
        std_error = NA_real_,
        ci_low    = NA_real_,
        ci_high   = NA_real_
      ))
    }

    vcov_mat <- sandwich::vcovCL(fit, cluster = model_dt$.cluster_geo, type = "HC1")

    # t(G - 1), not the normal quantile: with a moderate number of clusters the
    # normal critical value understates every interval (G = 12 => 1.96 vs 2.20).
    crit <- stats::qt(1 - (1 - conf_level) / 2, n_clusters - 1L)
  }

  estimate  <- stats::coef(fit)
  std_error <- sqrt(diag(vcov_mat))
  
  data.table::data.table(
    term      = names(estimate),
    estimate  = unname(estimate),
    std_error = unname(std_error),
    ci_low    = unname(estimate - crit * std_error),
    ci_high   = unname(estimate + crit * std_error)
  )
}


# Fit one outcome and return tidy rows (base group plus each comparison group).
.exposure_fit_one <- function(outcome_col, dt, geo_id_col, group_col, pop_col,
                              group_values, base_group, pollutants,
                              regression_unit, se_type, conf_level, normalized) {
  
  d0 <- dt[!is.na(get(outcome_col))]
  
  if (nrow(d0) < length(group_values)) {
    return(NULL)
  }
  
  # Optional normalization: divide the outcome by the base-group weighted mean.
  if (isTRUE(normalized)) {
    base_mean <- d0[
      get(group_col) == base_group,
      stats::weighted.mean(get(outcome_col), get(pop_col), na.rm = TRUE)
    ]
    
    if (is.na(base_mean) || base_mean == 0) {
      return(NULL)
    }
    
    d0[, y_model := get(outcome_col) / base_mean]
  } else {
    d0[, y_model := get(outcome_col)]
  }
  
  # Build the modeling table for the requested regression unit. Both branches
  # produce the same four columns: y, w, .cluster_geo, and the group column.
  if (regression_unit == "geo_group") {

    # Collapse to geo-by-group cells, then weight by population share in group.
    # The share denominator is taken over the cells that actually enter this
    # outcome's regression, so the weights sum to one within each group.
    model_dt <- d0[
      ,
      .(geo_population = sum(get(pop_col), na.rm = TRUE),
        y = stats::weighted.mean(y_model, get(pop_col), na.rm = TRUE)),
      by = c(geo_id_col, group_col)
    ]

    model_dt <- model_dt[!is.na(y) & !is.na(geo_population) & geo_population > 0]
    model_dt[, total_population_g := sum(geo_population), by = group_col]
    model_dt[, w := geo_population / total_population_g]
    model_dt <- model_dt[w > 0]

    model_dt[, .cluster_geo := get(geo_id_col)]

  } else {

    # individual/geo: one row per observation, weighted by population. The
    # cluster key is carried here because geo_id_col itself is not kept.
    model_dt <- d0[
      ,
      .(y = y_model, w = get(pop_col),
        .cluster_geo = get(geo_id_col), group_value = get(group_col))
    ]
    data.table::setnames(model_dt, "group_value", group_col)
    model_dt <- model_dt[!is.na(w) & w > 0]
  }

  if (nrow(model_dt) < length(group_values)) {
    return(NULL)
  }

  # Group factor with base_group as the reference (omitted) level.
  model_dt[, g := factor(get(group_col),
                         levels = c(base_group,
                                    setdiff(group_values, base_group)))]
  
  fit     <- stats::lm(y ~ g, data = model_dt, weights = w)
  coef_dt <- .exposure_coef_table(fit, model_dt, se_type, conf_level)
  meta    <- .exposure_parse_outcome(outcome_col, pollutants)
  
  # Cluster count travels with every row: it is what makes a degenerate
  # clustered fit visible in the output. n_coef travels with it because the
  # sandwich has rank at most G - 1, so the two numbers only mean something
  # side by side -- G = 6 is comfortable for 2 coefficients and hopeless for 5.
  n_clusters <- data.table::uniqueN(model_dt$.cluster_geo)
  n_coef     <- length(stats::coef(fit))

  # One assembled row builder reused for the base and comparison groups.
  make_row <- function(grp, est, se, lo, hi) {
    data.table::data.table(
      outcome = meta$outcome, pollutant = meta$pollutant, group = grp,
      estimate = est, std_error = se, ci_low = lo, ci_high = hi,
      n_units = nrow(model_dt), n_clusters = n_clusters, n_coef = n_coef,
      base_group = base_group, group_col = group_col,
      regression_unit = regression_unit, se_type = se_type, normalized = normalized
    )
  }
  
  # Base group has a zero gap by construction.
  out <- make_row(base_group, 0, 0, 0, 0)
  
  # Append each comparison group's coefficient and interval.
  for (grp in setdiff(group_values, base_group)) {
    row <- coef_dt[term == paste0("g", grp)]
    
    if (nrow(row) == 0L) {
      next
    }
    
    out <- data.table::rbindlist(
      list(out, make_row(grp, row$estimate, row$std_error,
                         row$ci_low, row$ci_high)),
      fill = TRUE
    )
  }
  
  return(out)
}


# --------------------------------------------------------------------------------------------
# Function: idw_artifact_path
#
# @Arg dir_idw   : string; root folder of the IDW estimates.
# @Arg city_id   : string; city folder and file prefix, e.g. "cdmx_2020".
# @Arg what      : string; artifact suffix, "idw_exposure" or "indiv_groups".
# @Arg buffer_km : numeric; buffer the artifact was built with.
# @Arg suffix    : string; optional grouping tag, e.g. "_income". Default "".
#
# @Output : string; full path to the requested Parquet file.
#
# @Details:
#   estimate_idw_exposure.R names every artifact
#   <city_id>/<city_id>_<buffer>km[_suffix]_<what>
#   .parquet. Building the name in one place keeps the readers from drifting from the
#   writer.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
idw_artifact_path <- function(dir_idw, city_id, what, buffer_km, suffix = "") {
  here::here(dir_idw, city_id,
             sprintf("%s_%dkm%s_%s.parquet", city_id, buffer_km, suffix, what))
}


# --------------------------------------------------------------------------------------------
# Function: read_idw_artifact
#
# @Arg dir_idw   : string; root folder of the IDW estimates.
# @Arg city_id   : string; city folder and file prefix.
# @Arg what      : string; "idw_exposure" or "indiv_groups".
# @Arg buffer_km : numeric; buffer the artifact was built with.
# @Arg suffix    : string; optional grouping tag, e.g. "_income". Default "".
#
# @Output : data.table with the artifact's contents.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
read_idw_artifact <- function(dir_idw, city_id, what, buffer_km, suffix = "") {
  path <- idw_artifact_path(dir_idw, city_id, what, buffer_km, suffix)

  if (!file.exists(path)) {
    stop("IDW artifact not found: ", path)
  }

  data.table::as.data.table(arrow::read_parquet(path))
}


# --------------------------------------------------------------------------------------------
# Function: run_city_exposure
#
# @Arg city           : string; display label stamped on every output row.
# @Arg city_id        : string; machine id stamped on every output row.
# @Arg exposure_dt    : data.table; geo-level IDW exposure for this city.
# @Arg individual_dt  : data.table; geo-by-group population weights.
# @Arg geo_station_pq : string; path to this city's geo-to-station distance matrix.
# @Arg pop_col        : string; population or expansion-weight column.
# @Arg socio_var      : string; "education" or "income", stamped on every row.
# @Arg group_col      : string; socioeconomic group column.
# @Arg group_values   : integer vector; valid groups, e.g. 1:5.
# @Arg base_group     : integer; omitted reference group.
# @Arg group_type     : string; "quintile" or "decile", stamped on every row.
# @Arg year           : integer; exposure year to keep.
# @Arg buffer_km      : numeric; buffer the exposure was built with.
# @Arg pollutants     : character vector; pollutants to keep.
# @Arg summary_pattern: string; regex selecting summary outcome columns.
# @Arg ci_pattern     : string; regex selecting regression outcome columns.
# @Arg conf_level     : numeric; confidence level for intervals.
# @Arg normalized     : logical; divide each outcome by the base-group mean.
# @Arg regression_unit: string; "geo_group" (main), "individual", or "geo".
# @Arg se_type        : string; "cluster_geo" (preferred) or "classic".
#
# @Output : list(summary, ci, coverage); the three tables for this city, each carrying the
# same
#           run labels (city, city_id, year, buffer_km, socioeconomic_var, group_type) so
#           they
#           can be stacked across cities without further work.
#
# @Details:
#   One city-grouping run of the exposure stage. Every methodological value is a required
#   argument rather than a default: those are the paper's specification and belong where a
#   referee reads them, in the calling script. Labelling happens here rather than in the
#   caller,
#   which is what stops the three tables from disagreeing about which run produced them.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
run_city_exposure <- function(city, city_id, exposure_dt, individual_dt, geo_station_pq,
                              pop_col, socio_var, group_col, group_values, base_group,
                              group_type, year, buffer_km, pollutants, summary_pattern,
                              ci_pattern, conf_level, normalized, regression_unit,
                              se_type) {

  # Stamp the run labels on a table, so every family carries the same provenance.
  label <- function(dt) {
    dt[, `:=`(city = city, city_id = city_id, year = year, buffer_km = buffer_km,
              socioeconomic_var = socio_var, group_type = group_type)]
    dt[]
  }

  list(
    summary = label(compute_exposure_summaries(
      exposure_dt = exposure_dt, individual_dt = individual_dt, pop_col = pop_col,
      group_col = group_col, group_values = group_values, pollutants = pollutants,
      outcome_pattern = summary_pattern, year_filter = year)),

    ci = label(compute_exposure_regressions(
      exposure_dt = exposure_dt, individual_dt = individual_dt, pop_col = pop_col,
      group_col = group_col, group_values = group_values, base_group = base_group,
      pollutants = pollutants, outcome_pattern = ci_pattern, year_filter = year,
      conf_level = conf_level, normalized = normalized,
      regression_unit = regression_unit, se_type = se_type)),

    coverage = label(compute_exposure_coverage(
      exposure_dt = exposure_dt, individual_dt = individual_dt,
      geo_station_pq = geo_station_pq, pop_col = pop_col, group_col = group_col,
      group_values = group_values, pollutants = pollutants, buffer_km = buffer_km,
      year_filter = year)))
}


# --------------------------------------------------------------------------------------------
# Function: stack_city_tables
#
# @Arg runs : list; each element a list returned by run_city_exposure().
# @Arg what : string; which table to pull, "summary", "ci" or "coverage".
#
# @Output : data.table stacking that table across all runs.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
stack_city_tables <- function(runs, what) {
  data.table::rbindlist(lapply(runs, `[[`, what), fill = TRUE)
}


# --------------------------------------------------------------------------------------------
# Function: set_meta_cols_first
#
# @Arg dt        : data.table; modified in place.
# @Arg meta_cols : character vector; columns to move to the front, in order.
#
# @Output : the same data.table, invisibly; column order changed by reference.
#
# @Details:
#   Puts the run labels ahead of the numbers so a reader opening the artifact sees which
#   city
#   and specification a row belongs to before its values.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
set_meta_cols_first <- function(dt, meta_cols) {
  data.table::setcolorder(dt, c(meta_cols, setdiff(names(dt), meta_cols)))
}


# --------------------------------------------------------------------------------------------
# Function: save_table_parquet_csv
#
# @Arg dt      : data.table to write.
# @Arg out_dir : string; destination folder.
# @Arg name    : string; file stem, without extension.
# @Arg quiet   : logical; suppress the confirmation message. Default FALSE.
#
# @Output : invisible NULL. Writes <out_dir>/<name>.parquet and <name>.csv.
#
# @Details:
#   Parquet is the file of record because it keeps column types; the CSV copy exists so
#   coauthors can open the same table in a spreadsheet.
#
# @Written_by : Marcos Paulo
# @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
save_table_parquet_csv <- function(dt, out_dir, name, quiet = FALSE) {
  arrow::write_parquet(dt, file.path(out_dir, paste0(name, ".parquet")))
  data.table::fwrite(dt, file.path(out_dir, paste0(name, ".csv")))

  if (!quiet) {
    cat("Saved:", file.path(out_dir, name), "(.parquet and .csv)\n")
  }

  invisible(NULL)
}
