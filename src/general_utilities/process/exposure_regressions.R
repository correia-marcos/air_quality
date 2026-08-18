# ============================================================================================
# IDB: Air monitoring — exposure summaries, regressions and coverage
# ============================================================================================
#' @Goal: Functions for exposure summaries, regressions and coverage.
#
#' @Description: Weighted exposure summaries by socioeconomic group, the regression gaps
# relative to the top group with clustered intervals, and the geographic coverage behind
# each estimate, together with the helpers that name and read the IDW artifacts feeding
# them and that write the stacked result tables. Sourced by config_utils_process_data.R;
# never sourced directly by a script.
#
#' @Summary:
#   1. compute_exposure_summaries
#   2. compute_exposure_coverage
#   3. compute_exposure_regressions
#   4. .exposure_merge_geo_group
#   5. .exposure_outcome_cols
#   6. .exposure_parse_outcome
#   7. .exposure_weighted_median
#   8. .exposure_coef_table
#   9. .exposure_fit_one
#   10. idw_artifact_path / read_idw_artifact
#   11. run_city_exposure
#   12. stack_city_tables
#   13. stack_exposure_runs
#   14. set_meta_cols_first
#   15. save_table_parquet_csv / save_exposure_tables
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: compute_exposure_summaries
#
#' @param exposure_dt  data.table; geo-level IDW exposure (one row per geo unit-year).
#' @param individual_dt data.table; individual census rows carrying geo_id,
#                       person_weight and the group column.
#' @param group_col    string; socioeconomic group column. Default "edu_quintile".
#' @param group_values integer vector; valid groups, e.g. 1:5.
#' @param pollutants   character vector; pollutants to keep, e.g. pm10/pm25.
#' @param outcome_pattern string; regex selecting exposure outcome columns.
#' @param year_filter  integer or NULL; if set, keeps only this exposure year.
#' @param quiet        logical; suppress progress messages. Default FALSE.
#
#' @return  data.table with weighted mean, weighted median, population, and counts
#           by outcome, pollutant, and group.
#
#' @details
#   Raw exposure levels by socioeconomic group. Merges geo-level exposure with the
#   geo-by-group population, collapses to geo-unit-by-group cells (exposure is
#   constant within a geo unit), then aggregates to group level. Cells are weighted
#   by population so groups reflect the population they represent.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : June 2026
# --------------------------------------------------------------------------------------------
compute_exposure_summaries <- function(exposure_dt,
                                       individual_dt,
                                       group_col       = "edu_quintile",
                                       group_values    = 1:5,
                                       pollutants      = c("pm10", "pm25"),
                                       outcome_pattern = "^(avg|hrs_d)_",
                                       year_filter     = NULL,
                                       quiet           = FALSE) {

  # 1. Merge exposure with the geo-by-group population
  # -----------------------------------------------------------------------
  # Both inputs are in-memory data.tables; copy so we never edit the caller's data.
  dt <- .exposure_merge_geo_group(
    exposure_dt   = exposure_dt,
    individual_dt = individual_dt,
    group_col     = group_col,
    group_values  = group_values,
    year_filter   = year_filter,
    quiet         = quiet
  )

  # 2. Pick outcome columns that match the pattern and the pollutants
  # -----------------------------------------------------------------------
  out_cols <- .exposure_outcome_cols(dt, outcome_pattern, pollutants)
  by_cols  <- c("geo_id", group_col)

  # 3. Collapse to geo-by-group cells (exposure is constant within a geo unit)
  # -----------------------------------------------------------------------
  geo_group <- dt[
    ,
    c(
      list(group_pop = sum(person_weight, na.rm = TRUE)),
      lapply(.SD, function(x) stats::weighted.mean(x, person_weight, na.rm = TRUE))
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
          n_geo_units         = data.table::uniqueN(geo_id)
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
#' @param exposure_dt   data.table; geo-level IDW exposure (one row per geo unit-year).
#' @param individual_dt data.table; individual census rows carrying geo_id,
#                       person_weight and the group column.
#' @param geo_station_pq string; path to the matrix_geo_station_distances.parquet used
#                       to build this city's exposure.
#' @param group_col     string; socioeconomic group column. Default "edu_quintile".
#' @param group_values  integer vector; valid groups, e.g. 1:5.
#' @param pollutants    character vector; pollutants to report.
#' @param buffer_km     numeric; buffer used to build the exposure. Default 3.
#' @param year_filter    integer or NULL; if set, keeps only this exposure year.
#
#' @return  data.table with one row per pollutant, tracing how many geographic units
#           survive each stage between the full metro area and the estimation sample.
#           pop_metro and pop_estimation count adults only: individual_dt is the
#           adult-filtered census written by aggregate_idw_exposure().
#
#' @details
#   Records the attrition that determines the cluster count. A metro geo unit is lost
#   when no station falls inside buffer_km of its representative point, then when no
#   in-buffer station reports the pollutant, then when it has no census row. For CDMX
#   the chain runs 63 -> 18 -> 11 -> 10 (PM10) / 6 (PM2.5), which is why its clustered
#   intervals are fragile. n_geo_estimation is computed through the same merge helper
#   the regressions use, so it must equal the n_clusters they report.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
compute_exposure_coverage <- function(exposure_dt,
                                      individual_dt,
                                      geo_station_pq,
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

  exp_dt[, geo_id := as.character(geo_id)]

  # 3. Estimation stage: run the regressions' own merge so the counts cannot drift
  # -----------------------------------------------------------------------
  merged <- .exposure_merge_geo_group(
    exposure_dt   = exp_dt,
    individual_dt = individual_dt,
    group_col     = group_col,
    group_values  = group_values,
    year_filter   = NULL,
    quiet         = TRUE
  )

  # Total metro population, taken from the census side before any exposure filter.
  ind_dt   <- data.table::as.data.table(individual_dt)
  pop_all  <- sum(ind_dt$person_weight, na.rm = TRUE)

  # 4. One row per pollutant, since coverage differs by what each station measures
  # -----------------------------------------------------------------------
  res <- data.table::rbindlist(lapply(pollutants, function(poll) {
    avg_col <- paste0("avg_", poll)

    # Geo units holding a value for this pollutant, before and after the census merge.
    geo_poll <- exp_dt[!is.na(get(avg_col)), unique(geo_id)]
    est_rows <- merged[!is.na(get(avg_col))]
    pop_est  <- est_rows[, sum(person_weight, na.rm = TRUE)]

    # buffer_km is deliberately not returned: the caller stamps it alongside the other
    # run labels, and two columns of the same name would silently overwrite each other.
    data.table::data.table(
      pollutant           = poll,
      n_geo_metro         = data.table::uniqueN(dist_dt$geo_id),
      n_station_metro     = data.table::uniqueN(dist_dt$station_id),
      n_geo_in_buffer     = data.table::uniqueN(in_buffer$geo_id),
      n_station_in_buffer = data.table::uniqueN(in_buffer$station_id),
      n_geo_exposure      = data.table::uniqueN(exp_dt$geo_id),
      n_geo_pollutant     = length(geo_poll),
      n_geo_estimation    = data.table::uniqueN(est_rows$geo_id),
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
#' @param exposure_dt  data.table; geo-level IDW exposure (one row per geo unit-year).
#' @param individual_dt data.table or NULL; individual census rows carrying geo_id,
#                       person_weight and the group column.
#' @param group_col    string; socioeconomic group column. Default "edu_quintile".
#' @param group_values integer vector; valid groups, e.g. 1:5.
#' @param base_group   integer; omitted reference group. Default max(group_values).
#' @param pollutants   character vector; pollutants to keep.
#' @param outcome_pattern string; regex selecting exposure outcome columns.
#' @param year_filter  integer or NULL; if set, keeps only this exposure year.
#' @param conf_level   numeric; confidence level for intervals. Default 0.95.
#' @param normalized   logical; if TRUE, divide each outcome by the base-group mean.
#' @param se_type      string; "cluster_geo" (preferred) or "classic" (legacy CI).
#' @param quiet        logical; suppress progress messages. Default FALSE.
#
#' @return  data.table with one row per outcome, pollutant, and group, giving the
#           gap relative to base_group with confidence interval. Carries n_units
#           (cells in the fit), n_clusters (distinct geographic units) and n_coef
#           (coefficients the cluster sandwich has to support).
#
#' @details
#   Estimates exposure gaps versus base_group. The estimator collapses the merged
#   data to geo-unit-by-group cells, then weights each cell by its population share
#   within group -- the paper's specification, and the only one implemented. Rows
#   carry a constant regression_unit = "geo_group" column so they stack with the
#   validation track's legacy estimates.
#   classic SEs use the t-distribution; cluster_geo clusters by geographic unit
#   and uses a t(G-1) critical value. With G <= number of coefficients the
#   clustered variance is not identified, so SEs and intervals come back NA with
#   a warning rather than as small numbers.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : July 2026
# --------------------------------------------------------------------------------------------
compute_exposure_regressions <- function(exposure_dt,
                                         individual_dt   = NULL,
                                         group_col       = "edu_quintile",
                                         group_values    = 1:5,
                                         base_group      = max(group_values),
                                         pollutants      = c("pm10", "pm25"),
                                         outcome_pattern = "^hrs_d_.*_it[12]$",
                                         year_filter     = NULL,
                                         conf_level      = 0.95,
                                         normalized      = TRUE,
                                         se_type         = c("cluster_geo",
                                                             "classic"),
                                         quiet           = FALSE) {

  # 0. Dependencies and argument checks
  # -----------------------------------------------------------------------

  se_type <- match.arg(se_type)

  if (!(conf_level > 0 && conf_level < 1)) {
    stop("`conf_level` must be between 0 and 1.")
  }
  
  if (!base_group %in% group_values) {
    stop("`base_group` must belong to `group_values`.")
  }
  
  # 1. Merge exposure with the geo-by-group population
  # -----------------------------------------------------------------------
  dt <- .exposure_merge_geo_group(
    exposure_dt   = exposure_dt,
    individual_dt = individual_dt,
    group_col     = group_col,
    group_values  = group_values,
    year_filter   = year_filter,
    quiet         = quiet
  )

  # 2. Pick outcome columns and fit one model per outcome
  # -----------------------------------------------------------------------
  out_cols <- .exposure_outcome_cols(dt, outcome_pattern, pollutants)

  res <- data.table::rbindlist(
    lapply(
      out_cols,
      .exposure_fit_one,
      dt           = dt,
      group_col    = group_col,
      group_values = group_values,
      base_group   = base_group,
      pollutants   = pollutants,
      se_type      = se_type,
      conf_level   = conf_level,
      normalized   = normalized
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
    message("[ci] ", length(out_cols), " outcome(s) fit | se = '", se_type,
            "' | clusters: ",
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
.exposure_merge_geo_group <- function(exposure_dt, individual_dt,
                                      group_col, group_values,
                                      year_filter = NULL, quiet = FALSE) {

  # Copy so the caller's in-memory tables are never modified.
  dt <- data.table::copy(data.table::as.data.table(exposure_dt))

  # Optional single-year filter.
  if (!is.null(year_filter)) {
    dt <- dt[year == year_filter]
  }

  dt[, geo_id := as.character(geo_id)]

  ind <- data.table::copy(data.table::as.data.table(individual_dt))
  ind[, geo_id := as.character(geo_id)]

  # Keep valid groups with positive weight only.
  ind <- ind[
    get(group_col) %in% group_values &
      !is.na(person_weight) & person_weight > 0,
    .SD,
    .SDcols = c("geo_id", group_col, "person_weight")
  ]

  # Drop any pre-existing group column in exposure before the merge.
  if (group_col %in% names(dt)) {
    dt[, (group_col) := NULL]
  }

  # This is an inner join: an exposure unit with no census row leaves the
  # sample here. Report the match rate so a silent ID mismatch (zero padding,
  # width) shows up as a number rather than as quietly smaller regressions.
  exp_ids   <- unique(dt$geo_id)
  n_matched <- length(intersect(exp_ids, unique(ind$geo_id)))

  if (!quiet) {
    message("[merge] Census match: ", n_matched, " of ", length(exp_ids),
            " exposure geo unit(s) (", length(exp_ids) - n_matched,
            " unmatched).")
  }

  dt <- merge(dt, ind, by = "geo_id", allow.cartesian = TRUE)

  # Final filter to valid groups with positive weight.
  dt <- dt[
    get(group_col) %in% group_values &
      !is.na(person_weight) & person_weight > 0
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
.exposure_fit_one <- function(outcome_col, dt, group_col,
                              group_values, base_group, pollutants,
                              se_type, conf_level, normalized) {
  
  d0 <- dt[!is.na(get(outcome_col))]
  
  if (nrow(d0) < length(group_values)) {
    return(NULL)
  }
  
  # Optional normalization: divide the outcome by the base-group weighted mean.
  if (isTRUE(normalized)) {
    base_mean <- d0[
      get(group_col) == base_group,
      stats::weighted.mean(get(outcome_col), person_weight, na.rm = TRUE)
    ]
    
    if (is.na(base_mean) || base_mean == 0) {
      return(NULL)
    }
    
    d0[, y_model := get(outcome_col) / base_mean]
  } else {
    d0[, y_model := get(outcome_col)]
  }
  
  # Collapse to geo-by-group cells, then weight by population share in group.
  # The share denominator is taken over the cells that actually enter this
  # outcome's regression, so the weights sum to one within each group.
  model_dt <- d0[
    ,
    .(geo_population = sum(person_weight, na.rm = TRUE),
      y = stats::weighted.mean(y_model, person_weight, na.rm = TRUE)),
    by = c("geo_id", group_col)
  ]

  model_dt <- model_dt[!is.na(y) & !is.na(geo_population) & geo_population > 0]
  model_dt[, total_population_g := sum(geo_population), by = group_col]
  model_dt[, w := geo_population / total_population_g]
  model_dt <- model_dt[w > 0]

  model_dt[, .cluster_geo := geo_id]

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

  # One assembled row builder reused for the base and comparison groups. The
  # constant regression_unit column keeps the schema of the legacy estimates.
  make_row <- function(grp, est, se, lo, hi) {
    data.table::data.table(
      outcome = meta$outcome, pollutant = meta$pollutant, group = grp,
      estimate = est, std_error = se, ci_low = lo, ci_high = hi,
      n_units = nrow(model_dt), n_clusters = n_clusters, n_coef = n_coef,
      base_group = base_group, group_col = group_col,
      regression_unit = "geo_group", se_type = se_type, normalized = normalized
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
#' @param dir_idw  string; root folder of the IDW estimates.
#' @param city_id  string; city folder and file prefix, e.g. "cdmx_2020".
#' @param what     string; artifact suffix, "idw_exposure" or "indiv_groups".
#' @param buffer_km numeric or NULL; buffer the artifact was built with. NULL for the
#                   artifacts that do not depend on it.
#' @param suffix   string; optional grouping tag, e.g. "_income". Default "".
#
#' @return  string; full path to the requested Parquet file.
#
#' @details
#   Each artifact is named for what it actually depends on, and this function is the one
#   place that spells the convention out for readers:
#
#     <city_id>_<buffer>km_idw_exposure.parquet   exposure: city x buffer
#     <city_id>[_suffix]_indiv_groups.parquet     grouped census: city x grouping
#
#   The interpolated exposure never sees a grouping, and the grouped census never sees a
#   buffer, so neither name carries the dimension it does not depend on. Building the
#   name in one place keeps the readers from drifting from the writer in run_idw_city().
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
idw_artifact_path <- function(dir_idw, city_id, what, buffer_km = NULL, suffix = "") {
  buffer_tag <- if (is.null(buffer_km)) "" else sprintf("_%dkm", buffer_km)

  here::here(dir_idw, city_id,
             sprintf("%s%s%s_%s.parquet", city_id, buffer_tag, suffix, what))
}


# --------------------------------------------------------------------------------------------
# Function: read_idw_artifact
#
#' @param dir_idw  string; root folder of the IDW estimates.
#' @param city_id  string; city folder and file prefix.
#' @param what     string; "idw_exposure" or "indiv_groups".
#' @param buffer_km numeric or NULL; buffer the artifact was built with. Pass NULL for
#                   "indiv_groups", which does not depend on it.
#' @param suffix   string; optional grouping tag, e.g. "_income". Default "".
#
#' @return  data.table with the artifact's contents.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
read_idw_artifact <- function(dir_idw, city_id, what, buffer_km = NULL, suffix = "") {
  path <- idw_artifact_path(dir_idw, city_id, what, buffer_km, suffix)

  if (!file.exists(path)) {
    stop("IDW artifact not found: ", path)
  }

  data.table::as.data.table(arrow::read_parquet(path))
}


# --------------------------------------------------------------------------------------------
# Function: run_city_exposure
#
#' @param city          string; display label stamped on every output row.
#' @param city_id       string; machine id stamped on every output row.
#' @param exposure_dt   data.table; geo-level IDW exposure for this city.
#' @param individual_dt data.table; individual census rows carrying geo_id,
#                       person_weight and the group column.
#' @param geo_station_pq string; path to this city's geo-to-station distance matrix.
#' @param socio_var     string; "education" or "income", stamped on every row.
#' @param group_col     string; socioeconomic group column.
#' @param n_groups      integer; number of equal-population groups, 5 or 10.
#' @param year          integer; exposure year to keep.
#' @param buffer_km     numeric; buffer the exposure was built with.
#' @param pollutants    character vector; pollutants to keep. Default pm10/pm25.
#' @param summary_pattern string; regex selecting summary outcome columns. Default
#                       "^(avg|hrs_d)_" (means and exceedance hours).
#' @param ci_pattern    string; regex selecting regression outcome columns. Default
#                       "^hrs_d_.*_it[12]$" (IT1/IT2 exceedance hours only).
#' @param conf_level    numeric; confidence level for intervals. Default 0.95.
#' @param normalized    logical; divide each outcome by the base-group mean.
#                       Default TRUE.
#' @param se_type       string; "cluster_geo" (preferred) or "classic". Default
#                       "cluster_geo".
#
#' @return  list(summary, ci, coverage); the three tables for this city, each carrying
#           the same run labels (city, city_id, year, buffer_km, socioeconomic_var,
#           group_type) so they can be stacked across cities without further work.
#
#' @details
#   One city-grouping run of the exposure stage. Labelling happens here rather than in
#   the caller, which is what stops the three tables from disagreeing about which run
#   produced them.
#
#   Groups are always 1..n_groups and the reference is always the top group, so a single
#   n_groups fixes group_values, base_group and the "quintile"/"decile" label stamped on
#   the output. pollutants/summary_pattern/ci_pattern/conf_level/normalized/se_type
#   default to the paper's specification; this doc block is their one home, so the
#   calling script states only what actually varies by city -- its data, geography and
#   grouping -- plus the year and buffer it was built with.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
run_city_exposure <- function(city, city_id, exposure_dt, individual_dt, geo_station_pq,
                              socio_var, group_col, n_groups, year, buffer_km,
                              pollutants      = c("pm10", "pm25"),
                              summary_pattern = "^(avg|hrs_d)_",
                              ci_pattern      = "^hrs_d_.*_it[12]$",
                              conf_level      = 0.95,
                              normalized      = TRUE,
                              se_type         = "cluster_geo") {

  # Groups run 1..n_groups with the top group as the omitted reference; see @details.
  n_groups     <- as.integer(n_groups)
  group_values <- seq_len(n_groups)
  base_group   <- n_groups
  group_type   <- switch(as.character(n_groups),
                         "5" = "quintile", "10" = "decile",
                         stop("`n_groups` must be 5 or 10."))

  # Stamp the run labels on a table, so every family carries the same provenance.
  label <- function(dt) {
    dt[, `:=`(city = city, city_id = city_id, year = year, buffer_km = buffer_km,
              socioeconomic_var = socio_var, group_type = group_type)]
    dt[]
  }

  list(
    summary = label(compute_exposure_summaries(
      exposure_dt = exposure_dt, individual_dt = individual_dt,
      group_col = group_col, group_values = group_values, pollutants = pollutants,
      outcome_pattern = summary_pattern, year_filter = year)),

    ci = label(compute_exposure_regressions(
      exposure_dt = exposure_dt, individual_dt = individual_dt,
      group_col = group_col, group_values = group_values, base_group = base_group,
      pollutants = pollutants, outcome_pattern = ci_pattern, year_filter = year,
      conf_level = conf_level, normalized = normalized, se_type = se_type)),

    coverage = label(compute_exposure_coverage(
      exposure_dt = exposure_dt, individual_dt = individual_dt,
      geo_station_pq = geo_station_pq, group_col = group_col,
      group_values = group_values, pollutants = pollutants, buffer_km = buffer_km,
      year_filter = year)))
}


# --------------------------------------------------------------------------------------------
# Function: stack_city_tables
#
#' @param runs list; each element a list returned by run_city_exposure().
#' @param what string; which table to pull, "summary", "ci" or "coverage".
#
#' @return  data.table stacking that table across all runs.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
stack_city_tables <- function(runs, what) {
  data.table::rbindlist(lapply(runs, `[[`, what), fill = TRUE)
}


# --------------------------------------------------------------------------------------------
# Function: stack_exposure_runs
#
#' @param edu_runs list; run_city_exposure() results for the education-quintile runs.
#' @param inc_runs list; run_city_exposure() results for the income runs.
#
#' @return  named list of five data.tables, ready for save_exposure_tables():
#           ci_estimates_education, group_summaries_education, ci_estimates_income,
#           group_summaries_income, coverage.
#
#' @details
#   Education and income stack into separate ci/summary tables because their group
#   definitions differ (1:5 versus 1:10); coverage stacks across both, since its rows
#   are one per pollutant regardless of grouping.
#
#   Also merges into coverage the cluster count each regression actually used, taken as
#   the max across its outcomes' n_clusters/n_units/n_coef. n_geo_estimation (from the
#   coverage merge) and n_clusters (from the fitted models) come from independent
#   paths, so a disagreement between them is a silent sample loss made visible rather
#   than hidden. Coverage is returned ordered thinnest-sample-first.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
stack_exposure_runs <- function(edu_runs, inc_runs) {
  ci_all             <- stack_city_tables(edu_runs, "ci")
  summary_all        <- stack_city_tables(edu_runs, "summary")
  ci_income_all      <- stack_city_tables(inc_runs, "ci")
  summary_income_all <- stack_city_tables(inc_runs, "summary")
  coverage_all       <- stack_city_tables(c(edu_runs, inc_runs), "coverage")

  # Cluster count per regression, taken as the max across the IT1/IT2 outcomes.
  g_used <- data.table::rbindlist(list(ci_all, ci_income_all), fill = TRUE)[
    , .(n_clusters = max(n_clusters), n_units = max(n_units), n_coef = max(n_coef)),
    by = .(city_id, socioeconomic_var, pollutant)]

  coverage_all <- merge(coverage_all, g_used,
                        by = c("city_id", "socioeconomic_var", "pollutant"), all.x = TRUE)

  data.table::setorder(coverage_all, n_clusters)

  list(
    ci_estimates_education    = ci_all,
    group_summaries_education = summary_all,
    ci_estimates_income       = ci_income_all,
    group_summaries_income    = summary_income_all,
    coverage                  = coverage_all
  )
}


# --------------------------------------------------------------------------------------------
# Function: set_meta_cols_first
#
#' @param dt       data.table; modified in place.
#' @param meta_cols character vector; columns to move to the front, in order.
#
#' @return  the same data.table, invisibly; column order changed by reference.
#
#' @details
#   Puts the run labels ahead of the numbers so a reader opening the artifact sees which
#   city and specification a row belongs to before its values.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
set_meta_cols_first <- function(dt, meta_cols) {
  data.table::setcolorder(dt, c(meta_cols, setdiff(names(dt), meta_cols)))
}


# --------------------------------------------------------------------------------------------
# Function: save_table_parquet_csv
#
#' @param dt     data.table to write.
#' @param out_dir string; destination folder.
#' @param name   string; file stem, without extension.
#' @param quiet  logical; suppress the confirmation message. Default FALSE.
#
#' @return  invisible NULL. Writes <out_dir>/<name>.parquet and <name>.csv.
#
#' @details
#   Parquet is the file of record because it keeps column types; the CSV copy exists so
#   coauthors can open the same table in a spreadsheet.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
save_table_parquet_csv <- function(dt, out_dir, name, quiet = FALSE) {
  arrow::write_parquet(dt, file.path(out_dir, paste0(name, ".parquet")))
  data.table::fwrite(dt, file.path(out_dir, paste0(name, ".csv")))

  if (!quiet) {
    cat("Saved:", file.path(out_dir, name), "(.parquet and .csv)\n")
  }

  invisible(NULL)
}


# --------------------------------------------------------------------------------------------
# Function: save_exposure_tables
#
#' @param tables    named list of data.tables; the name becomes the file stem's middle
#                   segment, e.g. "coverage" -> exposure_coverage_3km_2023.
#' @param out_dir   string; destination folder.
#' @param buffer_km numeric; buffer this run used, written into every file name.
#' @param year      integer; analysis year, written into every file name.
#
#' @return  invisible NULL. Writes one Parquet and one CSV per element of `tables`, and
#           reorders each input table's columns by reference.
#
#' @details
#   Moves the run labels ahead of the numbers in each table, then writes it as
#   exposure_<name>_<buffer>km_<year>. Buffer and year ride in the file name so a
#   result can never be read out of context: the 3 km and 5 km tables are the paper's
#   specification and its robustness check, and they are otherwise indistinguishable
#   once opened.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
save_exposure_tables <- function(tables, out_dir, buffer_km, year) {
  meta_cols <- c("city", "city_id", "year", "buffer_km",
                 "socioeconomic_var", "group_type")

  for (nm in names(tables)) {
    set_meta_cols_first(tables[[nm]], meta_cols)

    save_table_parquet_csv(
      tables[[nm]], out_dir,
      sprintf("exposure_%s_%dkm_%d", nm, buffer_km, year)
    )
  }

  invisible(NULL)
}
