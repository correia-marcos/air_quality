# ============================================================================================
# IDB: Air monitoring — the Step 0-4 progression
# ============================================================================================
# @Goal: Functions for the Step 0-4 progression.
#
# @Description: Builds the additive Step 0-4 specifications and reports how each result moves as one input
#   layer at a time is updated. See .claude/rules/validation.md.
#   Sourced by config_utils_validation_old_version.R; never sourced directly.
#
# @Summary:
#   1. build_bogota_progression_specs
#   2. compare_results_progression
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function   : build_bogota_progression_specs
# @Arg cfg   : city cfg with $compare sublist (paths).
# @Output    : named list of 4 step specs. Each has:
#                id, label, mode ("precomputed" | "compute"), enabled (logical),
#                reason (NULL or skip explanation), and either `parquet_path`
#                (precomputed exposure parquet) or a full argument bundle for
#                `aggregate_idw_exposure()`.
# @Purpose   : Declaratively describe the 4-step methodological progression for
#              Bogotá so `compare_results_progression()` stays generic. Steps:
#                1 — Everything legacy (legacy census, legacy metro, legacy stations).
#                2 — New 2005 metro gpkg (40 GEO_IDs) + legacy census + legacy stations.
#                3 — Step 2 + new 2018 metro (manzana-level) + new 2018 census.
#                4 — Step 3 + new station set.
#              A step with any missing input is marked enabled=FALSE with a reason.
# @Written_on: 17/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
build_bogota_progression_specs <- function(cfg, buffer_km = 5L) {
  cmp <- cfg$compare %||% list()
  
  idw_root  <- here::here("data", "processed", "idw_estimates")
  dist_root <- here::here("data", "processed", "distances_matrices")
  
  # Step 4 is the canonical new-pipeline output; reuse a precomputed parquet
  # produced by aggregate_exposure_all_cities.R so we don't rerun DuckDB.
  step4_tag     <- paste0(buffer_km, "km")
  step4_parquet <- file.path(idw_root, "bogota_2018",
                             sprintf("bogota_2018_%s_idw_exposure.parquet", step4_tag))
  step4_indiv   <- file.path(idw_root, "bogota_2018",
                             sprintf("bogota_2018_%s_individual_quintiles.parquet", step4_tag))
  # For Step 4 day-level metrics we re-query the same inputs
  # aggregate_exposure_all_cities.R used: the cleaned Arrow panel and the
  # new-pipeline 2018 geo-station distance parquet.
  step4_arrow_clean <- here::here("data", "processed", "monitoring_stations_outliers",
                                  "bogota_metro_clean")
  step4_arrow_raw   <- here::here("data", "raw", "monitoring_stations",
                                  "bogota_metro_dataset")
  step4_arrow_dir   <- if (dir.exists(step4_arrow_clean)) step4_arrow_clean
  else if (dir.exists(step4_arrow_raw)) step4_arrow_raw
  else NULL
  # Same nested layout generate_distance_matrices.R writes: <city>/matrix_*.parquet.
  step4_geo_dist_path <- file.path(dist_root, "bogota_2018",
                                   "matrix_geo_station_distances.parquet")
  has_step4_geo_dist  <- file.exists(step4_geo_dist_path)
  step4_geo_dist      <- if (has_step4_geo_dist) step4_geo_dist_path else NULL

  # Step 4 still runs without this file, but its day-level metrics silently
  # disappear. Say so out loud rather than dropping them unannounced.
  if (!has_step4_geo_dist) {
    warning("[progression] Step 4 day-level metrics disabled — missing ",
            step4_geo_dist_path, " (run generate_distance_matrices.R).",
            call. = FALSE)
  }
  
  # Steps 1-3 reuse the legacy Arrow panel (legacy raw CSV passed through the
  # new outlier-detection-on-legacy-input toggle produces a mock clean dir on
  # disk). If compare_outlier_procedure hasn't run, we fall back to the new
  # clean panel for steps 2-3 and skip step 1.
  legacy_mock_clean <- file.path(tempdir(), "legacy_arrow_test", "legacy_mock_clean")
  new_clean_dir     <- cmp$new_clean_dir
  
  # Legacy census (geo mode) — CSV produced by legacy Stata pipeline, one row per locality.
  legacy_census_csv <- cmp$legacy_census_collapsed
  has_legacy_census <- !is.null(legacy_census_csv) && file.exists(legacy_census_csv)
  
  # Legacy station × legacy metro distance (RDS long): dt_distances.rds
  legacy_geo_dist_rds <- cmp$legacy_geo_dist
  has_legacy_geo_dist <- !is.null(legacy_geo_dist_rds) && file.exists(legacy_geo_dist_rds)
  
  # New 2005 metro geometry (40 GEO_IDs). Parquet: bogota_2005_geo_station_distances.parquet.
  new_2005_geo_dist <- file.path(dist_root, "bogota_2005_geo_station_distances.parquet")
  has_new_2005_dist <- file.exists(new_2005_geo_dist)
  
  # Legacy stations × new 2018 manzana (44,979) — produced by
  # generate_distance_matrices.R (see the bogota_2018_legacy_stations block).
  legacy_sta_2018_dist <- file.path(dist_root,
                                    "bogota_2018_legacy_stations_geo_station_distances.parquet")
  has_legacy_sta_2018_dist <- file.exists(legacy_sta_2018_dist)
  
  # New 2018 census individual microdata (adult flag + years of schooling).
  new_census_indiv_pq <- here::here("data", "interim", "census", "bogota_2018",
                                    "census_2018_metro_individual.parquet")
  has_new_census_indiv <- file.exists(new_census_indiv_pq)
  
  # ----------------------------------------------------------------------
  # Legacy geo-station distance RDS → parquet (tempdir cache). The RDS
  # from dt_distances.rds stores (station_code, locality, distance) plus
  # d_any_Xkm flags; aggregate_idw_exposure() only needs geo_id/station_id/
  # distance_km, so we transform and persist a minimal parquet.
  # ----------------------------------------------------------------------
  .legacy_dist_to_parquet <- function() {
    out_pq <- file.path(tempdir(), "legacy_geo_station_distances.parquet")
    if (file.exists(out_pq)) return(out_pq)
    if (!has_legacy_geo_dist) return(NULL)
    dd <- tryCatch(readRDS(legacy_geo_dist_rds), error = function(e) NULL)
    if (is.null(dd)) return(NULL)
    dd <- data.table::as.data.table(dd)
    geo_col  <- intersect(c("geo_id", "GEO_ID", "locality", "LocCodigo"), names(dd))[1]
    sta_col  <- intersect(c("station_id", "station_code", "station"), names(dd))[1]
    dist_col <- intersect(c("distance_km", "distance"), names(dd))[1]
    if (is.na(geo_col) || is.na(sta_col) || is.na(dist_col)) return(NULL)
    dd <- dd[, .(geo_id      = as.character(get(geo_col)),
                 station_id  = toupper(trimws(as.character(get(sta_col)))),
                 distance_km = as.numeric(get(dist_col)))]
    # Heuristic: legacy distances were in metres in some extracts.
    if (max(dd$distance_km, na.rm = TRUE) > 1000)
      dd[, distance_km := distance_km / 1000]
    arrow::write_parquet(dd, out_pq, compression = "zstd")
    out_pq
  }
  
  legacy_geo_dist_pq <- if (has_legacy_geo_dist) .legacy_dist_to_parquet() else NULL
  
  # ----------------------------------------------------------------------
  # Legacy census recoders.
  #   * Step 1 uses the legacy distance matrix whose geo_id is LocCodigo
  #     (01..20 for Bogotá localidades, 25xxx for surrounding municipalities).
  #   * Step 2 uses the new 2005 gpkg distance matrix whose geo_id is
  #     "11001<LocCodigo>" for Bogotá localidades, and 5-digit DANE code
  #     for the surrounding municipalities. So for Step 2 the census needs
  #     an augmented GEO_ID column.
  # ----------------------------------------------------------------------
  legacy_census_raw <- if (has_legacy_census)
    readr::read_csv(legacy_census_csv, show_col_types = FALSE,
                    col_types = readr::cols(LocCodigo = readr::col_character())) else NULL
  
  legacy_census_for_2005 <- if (!is.null(legacy_census_raw)) {
    tmp <- data.table::as.data.table(data.table::copy(legacy_census_raw))
    tmp[, GEO_ID := ifelse(nchar(LocCodigo) <= 2L,
                           paste0("11001", LocCodigo),
                           LocCodigo)]
    dplyr::as_tibble(tmp)
  } else NULL
  
  list(
    # Legacy collapsed census uses LocCodigo (01–20 + 25xxx) as geo id and
    # `escolaridad` as pre-averaged years of schooling; schema:
    # data/_legacy/census/collapse_bogota_metro.csv.
    "1_legacy" = list(
      id    = "1_legacy",
      label = "Step 1 — Legacy (census, metro, stations)",
      mode  = "compute",
      enabled = has_legacy_census && !is.null(legacy_geo_dist_pq) &&
        dir.exists(legacy_mock_clean),
      reason  = if (!has_legacy_census)                    "missing legacy census CSV"
      else if (is.null(legacy_geo_dist_pq))      "missing legacy geo-station distances"
      else if (!dir.exists(legacy_mock_clean))   "run compare_outlier_procedure() first (creates legacy mock Arrow)"
      else NULL,
      args    = list(
        arrow_dir      = legacy_mock_clean,
        geo_sta_pq     = legacy_geo_dist_pq %||% "",
        census_col     = legacy_census_raw,
        geo_id_col     = "LocCodigo",
        pop_col        = "n",
        edu_col        = "escolaridad",
        quintile_level = "geo",
        buffer_km      = buffer_km
      )
    ),
    "2_new_metro_2005" = list(
      id    = "2_new_metro_2005",
      label = "Step 2 — New 2005 metro gpkg (40 GEO_IDs) + legacy census + legacy stations",
      mode  = "compute",
      enabled = has_legacy_census && has_new_2005_dist && dir.exists(legacy_mock_clean),
      reason  = if (!has_legacy_census)              "missing legacy census CSV"
      else if (!has_new_2005_dist)         "missing bogota_2005_geo_station_distances.parquet (run generate_distance_matrices.R)"
      else if (!dir.exists(legacy_mock_clean)) "run compare_outlier_procedure() first"
      else NULL,
      args    = list(
        arrow_dir      = legacy_mock_clean,
        geo_sta_pq     = new_2005_geo_dist,
        census_col     = legacy_census_for_2005,  # LocCodigo → 11001XX
        geo_id_col     = "GEO_ID",
        pop_col        = "n",
        edu_col        = "escolaridad",
        quintile_level = "geo",
        buffer_km      = buffer_km
      )
    ),
    "3_new_metro_census_2018" = list(
      id    = "3_new_metro_census_2018",
      label = "Step 3 — 2018 manzana metro + 2018 census + legacy stations",
      mode  = "compute",
      enabled = has_new_census_indiv && has_legacy_sta_2018_dist &&
        dir.exists(legacy_mock_clean),
      reason  =
        if (!has_new_census_indiv)
          "missing 2018 manzana-level census Parquet"
        else if (!has_legacy_sta_2018_dist)
          paste("missing bogota_2018_legacy_stations_geo_station_distances.parquet",
                "(run generate_distance_matrices.R)")
        else if (!dir.exists(legacy_mock_clean))
          "run compare_outlier_procedure() first"
        else NULL,
      args    = list(
        arrow_dir      = legacy_mock_clean,
        geo_sta_pq     = legacy_sta_2018_dist,
        census_col     = if (has_new_census_indiv)
          arrow::read_parquet(new_census_indiv_pq) else NULL,
        geo_id_col     = "GEO_ID",
        pop_col        = "fe",
        edu_col        = "escolaridad",
        quintile_level = "individual",
        buffer_km      = buffer_km
      )
    ),
    "4_new_stations" = list(
      id    = "4_new_stations",
      label = "Step 4 — Everything new (2018 census + 2018 metro + new stations)",
      mode  = "precomputed",
      enabled = file.exists(step4_parquet),
      reason  = if (!file.exists(step4_parquet))
        sprintf("missing %s (run aggregate_exposure_all_cities.R for buffer=%dkm)",
                basename(step4_parquet), buffer_km) else NULL,
      parquet_path            = step4_parquet,
      individual_parquet_path = if (file.exists(step4_indiv)) step4_indiv else NULL,
      quintile_level          = "individual",
      # For day-level metrics: rerun the IDW query against the cleaned Arrow
      # dataset + new 2018 geo-station distance matrix (same inputs as the
      # canonical aggregate_exposure_all_cities.R call).
      arrow_dir_for_days      = step4_arrow_dir,
      geo_sta_pq_for_days     = step4_geo_dist
    )
  )
}


# --------------------------------------------------------------------------------------------
# Function   : compare_results_progression
# @Arg cfg        : city cfg with $id and $compare sublist.
# @Arg out_root   : results root; {out_root}/results_progression/ is created.
# @Arg step_specs : list of step specs (see build_bogota_progression_specs).
#                   If NULL, auto-built for Bogotá via build_bogota_progression_specs(cfg).
# @Arg year       : integer; focus year for the summary (default 2023).
# @Arg pollutants : character vector; pollutants to summarise. Default c("pm25","pm10").
# @Arg buffer_km  : integer; buffer (km) applied to geo-station pairs inside
#                   aggregate_idw_exposure(). Must match the precomputed Step 4 parquet.
# @Arg quiet      : logical; suppress messages. Default FALSE.
#
# @Output : invisible list with
#            $progression_long — (step_id, step_label, pollutant, edu_quintile,
#                                 n_geo, weighted_mean_conc, share_hrs_above_it1,
#                                 share_hrs_above_it4, days_with_hr_above_it1,
#                                 days_with_hr_above_it2, days_with_2hr_above_it1)
#            $progression_gap  — one row per (step, pollutant) with q1, q5, gap, ratio
#            $step_status      — (step_id, label, status, reason) audit log
#            $out_dir          — path to results_progression directory
#           Writes to results_progression/:
#             progression_long.parquet, progression_gap.parquet, step_status.parquet,
#             <step_id>_exposure.parquet (per-step exposure dump)
#
# @Purpose : Run `aggregate_idw_exposure()` across a 4-step methodological progression
#            (legacy → new metro → new census → new stations) and measure how the
#            headline results — population-weighted mean PM concentration, share of
#            hours above WHO interim targets, and "days with ≥1 / ≥2 hours above IT"
#            — shift at each step, stratified by education quintile (Q1..Q5).
#
# @Details :
#   For each enabled step:
#     - mode = "precomputed": read parquet at parquet_path; if individual mode
#       and individual_parquet_path is supplied, derive a modal geo-quintile.
#     - mode = "compute": call aggregate_idw_exposure() with args; the per-step
#       output is cached under <out_dir>/<step_id>/.
#
#   Per-day metrics: the aggregate_idw_exposure output only carries hour-level
#   IT-threshold counts. To recover day-level counts (e.g. "days with ≥1 hour
#   above IT1"), this function re-queries the per-step clean Arrow dataset with
#   the same IDW formula — only if `step_days` is computable (step provides a
#   valid geo_sta_pq + arrow_dir).
#
# @Written_on: 17/04/2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_results_progression <- function(
    cfg,
    out_root,
    step_specs = NULL,
    year       = 2023L,
    pollutants = c("pm25", "pm10"),
    buffer_km  = 5L,
    quiet      = FALSE
) {
  req_pkgs <- c("dplyr", "tibble", "arrow", "data.table", "tidyr")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  pollutants <- tolower(pollutants)
  stopifnot(all(pollutants %in% c("pm25", "pm10")))
  
  out_dir <- file.path(out_root, "results_progression")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(step_specs)) {
    if (cfg$id != "bogota")
      stop("Auto-built step specs only available for Bogotá; pass step_specs explicitly.")
    step_specs <- build_bogota_progression_specs(cfg, buffer_km = buffer_km)
  }
  
  status_rows       <- list()
  per_step_quintile <- list()
  .yr               <- as.integer(year)
  
  # WHO interim-target thresholds (µg/m³). Mirrors aggregate_idw_exposure()'s defaults.
  who_it <- list(
    pm10 = c(it1 = 150, it2 = 100, it3 = 75,  it4 = 50),
    pm25 = c(it1 = 75,  it2 = 50,  it3 = 37.5, it4 = 25)
  )
  
  # Helper: compute day-level IT-exceedance counts for one (step, pollutant) by
  # re-running the IDW query hour-by-hour, then collapsing to daily max. Returns
  # a data.table keyed by geo_id with days_with_hr_above_{it1,it2} columns.
  .compute_day_metrics <- function(arrow_dir, geo_sta_pq, pollutant, year,
                                   buffer_km) {
    if (is.null(arrow_dir) || is.null(geo_sta_pq) ||
        !nzchar(arrow_dir)  || !nzchar(geo_sta_pq)) return(NULL)
    if (!requireNamespace("duckdb", quietly = TRUE) ||
        !dir.exists(arrow_dir) || !file.exists(geo_sta_pq)) return(NULL)
    thr1 <- who_it[[pollutant]][["it1"]]
    thr2 <- who_it[[pollutant]][["it2"]]
    con <- tryCatch(
      DBI::dbConnect(duckdb::duckdb(dbdir = tempfile("prog_days_", fileext = ".db"))),
      error = function(e) NULL)
    if (is.null(con)) return(NULL)
    on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
    poll_glob <- paste0(gsub("\\\\", "/", arrow_dir), "/**/*.parquet")
    q <- sprintf("
      WITH dist AS (
        SELECT geo_id, station_id, 1.0 / distance_km AS inv_d
        FROM read_parquet('%s')
        WHERE distance_km > 0 AND distance_km <= %d
      ),
      poll AS (
        SELECT station, datetime, %s AS val,
               CAST(strftime(datetime, '%%Y-%%m-%%d') AS VARCHAR) AS dt_date
        FROM read_parquet('%s', hive_partitioning = true)
        WHERE year = %d AND %s IS NOT NULL
      ),
      hr_geo AS (
        SELECT d.geo_id, p.dt_date, p.datetime,
               SUM(p.val * d.inv_d) / SUM(d.inv_d) AS idw
        FROM poll p JOIN dist d ON p.station = d.station_id
        GROUP BY d.geo_id, p.dt_date, p.datetime
      ),
      day_geo AS (
        SELECT geo_id, dt_date,
               SUM(CASE WHEN idw >= %s THEN 1 ELSE 0 END) AS n_hr_it1,
               SUM(CASE WHEN idw >= %s THEN 1 ELSE 0 END) AS n_hr_it2
        FROM hr_geo
        GROUP BY geo_id, dt_date
      )
      SELECT geo_id,
             SUM(CASE WHEN n_hr_it1 >= 1 THEN 1 ELSE 0 END)
               AS days_with_hr_above_it1,
             SUM(CASE WHEN n_hr_it1 >= 2 THEN 1 ELSE 0 END)
               AS days_with_2hr_above_it1,
             SUM(CASE WHEN n_hr_it2 >= 1 THEN 1 ELSE 0 END)
               AS days_with_hr_above_it2
      FROM day_geo
      GROUP BY geo_id;",
                 gsub("'", "''", geo_sta_pq),
                 as.integer(buffer_km),
                 pollutant,
                 gsub("'", "''", poll_glob),
                 as.integer(year),
                 pollutant,
                 format(thr1, nsmall = 1),
                 format(thr2, nsmall = 1))
    res <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) NULL)
    if (is.null(res) || !nrow(res)) return(NULL)
    data.table::as.data.table(res)[, geo_id := as.character(geo_id)]
  }
  
  for (sp in step_specs) {
    if (!isTRUE(sp$enabled)) {
      status_rows[[sp$id]] <- tibble::tibble(
        step_id = sp$id, label = sp$label, status = "skipped",
        reason  = sp$reason %||% "disabled"
      )
      if (!quiet) message(sprintf("[progression] Skip %s — %s", sp$id, sp$reason %||% "disabled"))
      next
    }
    
    if (!quiet) message(sprintf("[progression] %s ...", sp$label))
    
    exp_dt <- NULL; indiv_dt <- NULL
    arrow_dir_for_days <- NULL; geo_sta_pq_for_days <- NULL
    step_buffer <- buffer_km
    
    if (identical(sp$mode, "precomputed")) {
      exp_dt <- data.table::as.data.table(arrow::read_parquet(sp$parquet_path))
      if (!is.null(sp$individual_parquet_path))
        indiv_dt <- data.table::as.data.table(arrow::read_parquet(sp$individual_parquet_path))
      # For Step 4 we need arrow_dir + geo_sta_pq to compute day metrics; these
      # are carried on the spec for reuse.
      arrow_dir_for_days  <- sp$arrow_dir_for_days  %||% NULL
      geo_sta_pq_for_days <- sp$geo_sta_pq_for_days %||% NULL
    } else {
      step_out_dir <- file.path(out_dir, sp$id)
      args <- sp$args
      args$out_dir   <- step_out_dir
      args$out_name  <- sp$id
      args$quiet     <- quiet
      args$overwrite <- FALSE
      step_buffer <- args$buffer_km %||% buffer_km
      args$buffer_km <- step_buffer
      res <- tryCatch(do.call(aggregate_idw_exposure, args),
                      error = function(e) { warning(e$message); NULL })
      if (is.null(res)) {
        status_rows[[sp$id]] <- tibble::tibble(
          step_id = sp$id, label = sp$label, status = "error",
          reason = "aggregate_idw_exposure failed — see warning"
        )
        next
      }
      exp_dt   <- data.table::as.data.table(res$exposure_yearly)
      indiv_dt <- if (!is.null(res$individual_quintiles))
        data.table::as.data.table(res$individual_quintiles) else NULL
      arrow_dir_for_days  <- args$arrow_dir
      geo_sta_pq_for_days <- args$geo_sta_pq
    }
    
    # Persist the per-step exposure for downstream reports
    arrow::write_parquet(
      dplyr::as_tibble(exp_dt),
      file.path(out_dir, paste0(sp$id, "_exposure.parquet")),
      compression = "zstd"
    )
    
    if ("year" %in% names(exp_dt)) exp_dt <- exp_dt[year == .yr]
    if (nrow(exp_dt) == 0L) {
      status_rows[[sp$id]] <- tibble::tibble(
        step_id = sp$id, label = sp$label, status = "empty",
        reason = sprintf("no rows for year=%d", .yr)
      )
      next
    }
    
    exp_dt[, geo_id := as.character(geo_id)]
    
    if (!"edu_quintile" %in% names(exp_dt)) {
      # Individual mode: derive a geo-level quintile from individual_quintiles.
      if (is.null(indiv_dt) || !"edu_quintile" %in% names(indiv_dt)) {
        status_rows[[sp$id]] <- tibble::tibble(
          step_id = sp$id, label = sp$label, status = "error",
          reason = "edu_quintile missing in both exposure and individual outputs"
        )
        next
      }
      indiv_dt[, geo_id := as.character(geo_id)]
      weight_col <- intersect(c("fe", "FACTOR", "weight", "n"), names(indiv_dt))[1]
      if (is.na(weight_col)) {
        indiv_dt[, fe := 1L]
        weight_col <- "fe"
      }
      # Modal quintile per geo_id weighted by the expansion factor.
      q_geo <- indiv_dt[!is.na(edu_quintile),
                        .(w = sum(get(weight_col), na.rm = TRUE)),
                        by = .(geo_id, edu_quintile)]
      data.table::setorder(q_geo, geo_id, -w)
      q_geo <- q_geo[, .SD[1], by = geo_id][, .(geo_id, edu_quintile)]
      pop_geo <- indiv_dt[, .(pop_w = sum(get(weight_col), na.rm = TRUE)), by = geo_id]
      exp_dt <- merge(exp_dt, q_geo,   by = "geo_id", all.x = TRUE)
      exp_dt <- merge(exp_dt, pop_geo, by = "geo_id", all.x = TRUE)
    } else {
      pop_candidates <- intersect(c("n", "pop", "fe", "FACTOR", "weight"), names(exp_dt))
      if (length(pop_candidates) && !"pop_w" %in% names(exp_dt))
        exp_dt[, pop_w := as.numeric(get(pop_candidates[1]))]
      if (!"pop_w" %in% names(exp_dt)) exp_dt[, pop_w := 1]
    }
    
    # --------------------------------------------------------------------
    # Per-pollutant quintile summary. Fall through pollutants that are not
    # present in the exposure output (e.g. some legacy years lack PM25).
    # --------------------------------------------------------------------
    poll_tabs <- list()
    for (pol in pollutants) {
      conc_col  <- paste0("avg_",       pol)
      total_col <- paste0("total_hrs_", pol)
      it1_col   <- paste0("hrs_d_",     pol, "_it1")
      it4_col   <- paste0("hrs_d_",     pol, "_it4")
      if (!conc_col %in% names(exp_dt)) next
      
      # Day-level metrics (optional; skipped if Arrow/distance unavailable).
      days_dt <- .compute_day_metrics(
        arrow_dir  = arrow_dir_for_days,
        geo_sta_pq = geo_sta_pq_for_days,
        pollutant  = pol,
        year       = .yr,
        buffer_km  = step_buffer
      )
      
      one <- exp_dt[!is.na(get(conc_col)),
                    .(geo_id, edu_quintile, pop_w,
                      conc_col  = get(conc_col),
                      hrs_it1   = if (it1_col %in% names(exp_dt))
                        get(it1_col) else NA_real_,
                      hrs_it4   = if (it4_col %in% names(exp_dt))
                        get(it4_col) else NA_real_,
                      total_hrs = if (total_col %in% names(exp_dt))
                        get(total_col) else NA_real_)]
      if (!is.null(days_dt))
        one <- merge(one, days_dt, by = "geo_id", all.x = TRUE)
      
      qsum <- one[!is.na(edu_quintile),
                  .(n_geo              = .N,
                    weighted_mean_conc = stats::weighted.mean(conc_col, pop_w, na.rm = TRUE),
                    share_hrs_above_it1 = sum(hrs_it1, na.rm = TRUE) /
                      max(sum(total_hrs, na.rm = TRUE), 1),
                    share_hrs_above_it4 = sum(hrs_it4, na.rm = TRUE) /
                      max(sum(total_hrs, na.rm = TRUE), 1),
                    days_with_hr_above_it1 = if ("days_with_hr_above_it1" %in% names(one))
                      stats::weighted.mean(days_with_hr_above_it1, pop_w,
                                           na.rm = TRUE) else NA_real_,
                    days_with_2hr_above_it1 = if ("days_with_2hr_above_it1" %in% names(one))
                      stats::weighted.mean(days_with_2hr_above_it1, pop_w,
                                           na.rm = TRUE) else NA_real_,
                    days_with_hr_above_it2 = if ("days_with_hr_above_it2" %in% names(one))
                      stats::weighted.mean(days_with_hr_above_it2, pop_w,
                                           na.rm = TRUE) else NA_real_),
                  by = edu_quintile][order(edu_quintile)]
      qsum[, `:=`(step_id = sp$id, step_label = sp$label, pollutant = pol)]
      poll_tabs[[pol]] <- qsum
    }
    if (!length(poll_tabs)) {
      status_rows[[sp$id]] <- tibble::tibble(
        step_id = sp$id, label = sp$label, status = "error",
        reason  = "no avg_pm* columns in exposure output"
      )
      next
    }
    per_step_quintile[[sp$id]] <- data.table::rbindlist(poll_tabs, fill = TRUE)
    
    status_rows[[sp$id]] <- tibble::tibble(
      step_id = sp$id, label = sp$label, status = "ok", reason = NA_character_
    )
  }
  
  progression_long <- data.table::rbindlist(per_step_quintile, fill = TRUE)
  progression_long <- dplyr::as_tibble(progression_long) |>
    dplyr::select(step_id, step_label, pollutant, edu_quintile,
                  n_geo, weighted_mean_conc,
                  share_hrs_above_it1, share_hrs_above_it4,
                  dplyr::any_of(c("days_with_hr_above_it1",
                                  "days_with_2hr_above_it1",
                                  "days_with_hr_above_it2")))
  
  # Headline gap Q5-Q1 and ratio Q5/Q1 per (step, pollutant).
  progression_gap <- progression_long |>
    dplyr::filter(edu_quintile %in% c(1L, 5L)) |>
    dplyr::select(step_id, step_label, pollutant, edu_quintile, weighted_mean_conc) |>
    tidyr::pivot_wider(names_from = edu_quintile,
                       names_prefix = "q",
                       values_from = weighted_mean_conc) |>
    dplyr::mutate(
      gap_q5_minus_q1  = q5 - q1,
      ratio_q5_over_q1 = dplyr::if_else(!is.na(q1) & q1 > 0, q5 / q1, NA_real_)
    )
  
  step_status <- dplyr::bind_rows(status_rows)
  
  arrow::write_parquet(progression_long,
                       file.path(out_dir, "progression_long.parquet"),
                       compression = "zstd")
  arrow::write_parquet(progression_gap,
                       file.path(out_dir, "progression_gap.parquet"),
                       compression = "zstd")
  arrow::write_parquet(step_status,
                       file.path(out_dir, "step_status.parquet"),
                       compression = "zstd")
  
  if (!quiet) {
    message("[", cfg$id, "] Progression comparison saved to: ", out_dir)
    n_ok <- sum(step_status$status == "ok")
    message(sprintf("  %d of %d steps completed (%s)",
                    n_ok, nrow(step_status),
                    paste(step_status$step_id[step_status$status == "ok"], collapse = ", ")))
  }
  
  invisible(list(
    progression_long = progression_long,
    progression_gap  = progression_gap,
    step_status      = step_status,
    out_dir          = out_dir
  ))
}
