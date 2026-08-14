# ============================================================================================
# IDB: Air monitoring — result comparisons
# ============================================================================================
#' @Goal: Functions for result comparisons.
#
#' @Description: Reproduces the legacy IDW and regression estimators, then quantifies how the new pipeline's
#   results differ from them.
#   Sourced by config_utils_validation_old_version.R; never sourced directly.
#
#' @Summary:
#   1. aggregate_idw_exposure_legacy
#   2. compare_idw
#   3. compare_outlier_procedure
#   4. compute_exposure_regressions_legacy
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# ----------------------------------------------------------------------------------------
# Function: aggregate_idw_exposure_legacy
#
#' @param arrow_dir     string; cleaned partitioned Arrow/Parquet hourly data.
#' @param geo_sta_pq    string; geo-station distance Parquet (geo_id, station_id,
#                       distance_km) from compute_distance_matrices().
#' @param census_col    data.frame; individual-level census (one row per person).
#' @param geo_id_col    string; geo ID column in census_col.
#' @param pop_col       string; expansion-weight column in census_col.
#' @param group_var     string; schooling variable to quintile (e.g. "escolaridad").
#' @param adult_col     string; adult filter column. Default "adult".
#' @param target_year   integer; the single year to process (legacy ran 2023).
#' @param buffer_km     numeric; max geo-to-station distance. Default 3.
#' @param out_dir       string; output directory.
#' @param out_name      string; output file prefix.
#' @param quiet         logical; suppress messages. Default FALSE.
#
#' @return  list(exposure_path, individual_path); writes two parquet files.
#
#' @details
#   STEP-0 LEGACY REPLICATION ONLY — reproduces the *old* IDW scheme so the
#   Quarto report can compare it to aggregate_idw_exposure() on identical data.
#   It is intentionally NOT missingness-aware: inverse-distance weights are
#   normalized over ALL in-buffer stations in a geo-hour (including those
#   missing the pollutant that hour), then missing products are dropped by a
#   na.rm sum. The kept denominator therefore includes absent stations, which
#   deflates the estimate relative to the corrected method. Quintiles follow
#   the legacy weighted rule: sort adults by schooling, cut on the cumulative
#   expansion-weight share cumsum(fe)/sum(fe) at 0.2 steps. This is the legacy
#   Bogota/CDMX construction; Santiago (fe == 1) is the special case that
#   reduces to equal individual counts. Single year, PM10/PM2.5, power-1
#   weights — matching the 2023 legacy script.
#   Do not use for paper results; use aggregate_idw_exposure() instead.
#
#' @Written_on : June 2026
#' @Written_by : Marcos Paulo
# ----------------------------------------------------------------------------------------
aggregate_idw_exposure_legacy <- function(
    arrow_dir,
    geo_sta_pq,
    census_col,
    geo_id_col   = "GEO_ID",
    pop_col      = "fe",
    group_var    = "escolaridad",
    adult_col    = "adult",
    target_year  = 2023L,
    buffer_km    = 3,
    out_dir,
    out_name,
    quiet        = FALSE
) {
  
  pkgs <- c("arrow", "data.table", "stringi")
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("Missing: ", p)
  
  # Normalize station IDs the same way as the distance/IDW steps.
  .norm_sta <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  pollutants <- c("pm10", "pm25")
  # Only it1/it2 reach the yearly table in the legacy script.
  who_it <- list(pm10 = c(it1 = 150, it2 = 100),
                 pm25 = c(it1 = 75,  it2 = 50))
  
  # 1. Distances: keep in-buffer pairs. Legacy used `distance_km <= 3` with no
  # positive-distance filter, so a d == 0 pair would give inv_d = Inf. Match
  # legacy literally but warn if any d == 0 exists.
  dist_dt <- data.table::as.data.table(arrow::read_parquet(geo_sta_pq))
  dist_dt[, geo_id := as.character(geo_id)]
  dist_dt[, station_id := .norm_sta(station_id)]
  dist_dt <- dist_dt[!is.na(distance_km) & distance_km <= buffer_km]
  n_zero <- dist_dt[distance_km == 0, .N]
  if (n_zero > 0L && !quiet) {
    message("[", out_name, "] WARNING: ", n_zero,
            " pair(s) with distance_km == 0 (inv_d = Inf), kept to match legacy.")
  }
  if (nrow(dist_dt) == 0L) stop("No geo-station pairs within ", buffer_km, " km.")
  dist_dt[, inv_d := 1 / distance_km]              # power 1, as legacy
  dist_dt <- dist_dt[, .(geo_id, station_id, inv_d)]
  
  # 2. Hourly pollution for the single target year.
  ds <- arrow::open_dataset(arrow_dir)
  poll <- ds |>
    dplyr::filter(year == target_year) |>
    dplyr::select(station, datetime, dplyr::all_of(pollutants)) |>
    dplyr::collect() |>
    data.table::as.data.table()
  if (nrow(poll) == 0L) stop("No pollution rows for year ", target_year, ".")
  poll[, station_id := .norm_sta(station)]
  
  # 3. Per-pollutant legacy IDW. The denominator is normalized over ALL
  # in-buffer stations present in the geo-hour BEFORE dropping missing values,
  # reproducing the deflating-denominator behavior.
  yearly <- vector("list", length(pollutants))
  names(yearly) <- pollutants
  
  for (pol in pollutants) {
    # Long station-hour readings (value may be NA).
    ph <- poll[, .(station_id, datetime, val = get(pol))]
    
    # Cartesian merge of in-buffer stations to each geo via station_id.
    # Every in-buffer station-hour row is kept, missing val included.
    gh <- merge(ph, dist_dt, by = "station_id", allow.cartesian = TRUE)
    
    # Weight normalized over ALL in-buffer stations in the geo-hour
    # (na.rm only guards Inf/NA in inv_d, not missing values).
    gh[, weight := inv_d / sum(inv_d, na.rm = TRUE),
       by = .(geo_id, datetime)]
    
    # Weighted value; missing val -> NA product, dropped by the na.rm sum.
    gh[, wval := val * weight]
    agg <- gh[, .(idw = sum(wval, na.rm = TRUE)),
              by = .(geo_id, datetime)]
    
    # WHO indicators on the (deflated) hourly estimate.
    thr <- who_it[[pol]]
    agg[, d_it1 := as.integer(idw >= thr[["it1"]])]
    agg[, d_it2 := as.integer(idw >= thr[["it2"]])]
    
    # Annual: mean over all geo-hours (legacy used mean(agg, na.rm=TRUE) with
    # .N total hours; every geo-hour here has a value, possibly 0).
    yr <- agg[, .(
      avg          = mean(idw, na.rm = TRUE),
      hrs_d_it1    = sum(d_it1, na.rm = TRUE),
      hrs_d_it2    = sum(d_it2, na.rm = TRUE),
      total_hrs    = .N
    ), by = geo_id]
    
    data.table::setnames(yr, c("avg", "hrs_d_it1", "hrs_d_it2", "total_hrs"),
                         paste0(c("avg_", "hrs_d_it1_", "hrs_d_it2_",
                                  "total_hrs_"), pol))
    yearly[[pol]] <- yr
  }
  
  # Merge pollutants into one geo-level table.
  exposure <- Reduce(function(a, b) merge(a, b, by = "geo_id", all = TRUE),
                     yearly)
  exposure[, year := target_year]
  
  # 4. Legacy quintiles: adults only, expansion-weighted cumulative cut. Sort by
  # schooling, take cumsum(fe)/sum(fe), cut on seq(0,1,0.2). This is the legacy
  # Bogota/CDMX method; Santiago is the fe == 1 special case (then it reduces to
  # equal individual counts, matching legacy's frank-random partition).
  ce <- data.table::copy(data.table::as.data.table(census_col))
  data.table::setnames(ce, geo_id_col, "geo_id")
  ce[, geo_id := as.character(geo_id)]
  ce <- ce[get(adult_col) == 1]
  if (nrow(ce) == 0L) stop("No adult rows after filtering.")
  
  # Drop NA schooling (no quintile, no contribution to any quintile mean).
  ce <- ce[!is.na(get(group_var))]
  data.table::setorderv(ce, group_var)
  ce[, .cum_w := cumsum(get(pop_col)) / sum(get(pop_col))]
  # include.lowest puts cum_w == 0.2 in bin 1, matching the legacy cut.
  ce[, edu_quintile := as.integer(cut(.cum_w, breaks = seq(0, 1, 0.2),
                                      include.lowest = TRUE, labels = 1:5))]
  ce[, .cum_w := NULL]
  
  # 5. Write outputs (exposure + individual quintiles), mirroring the
  # individual-mode outputs of aggregate_idw_exposure().
  exp_path   <- file.path(out_dir, paste0(out_name, "_idw_exposure.parquet"))
  indiv_path <- file.path(out_dir, paste0(out_name, "_indiv_groups.parquet"))
  arrow::write_parquet(exposure, exp_path)
  arrow::write_parquet(ce, indiv_path)
  
  if (!quiet) message("[", out_name, "] Legacy IDW written: ", exp_path)
  
  invisible(list(exposure_path = exp_path, individual_path = indiv_path))
}


# --------------------------------------------------------------------------------------------
# compare_idw
#' @param cfg                    city cfg list. Must contain a $compare sublist with:
#                                new_station_dist, legacy_station_dist, and optionally
#                                new_geo_dist, legacy_geo_dist, station_tol_km,
#                                and geo_tol_km.
#' @param out_root               root output folder; {out_root}/{cfg$id}/ is created.
#' @param station_audit          data.frame; output from compare_ground_stations().
#                                Used to restrict the new pipeline's distance matrix to 
#                                the legacy station universe for fair comparison.
#' @param quiet                  logical; suppress messages. Default FALSE.
#
#' @return    named list (invisible) with:
#   $station_dist_summary — tibble; per-station-pair comparison statistics
#   $station_dist_diffs   — tibble; station pairs differing > station_tol_km
#   $geo_dist_summary     — tibble or NULL; per-geo-unit comparison statistics,
#                           reported for all pairs and for pairs within 5 km
#   $geo_dist_diffs       — tibble or NULL; geo-station pairs differing > geo_tol_km
#   $method_note          — character; explains the methodological differences
#   $out_dir              — path to the output directory
#   Parquet files written to {out_root}/{cfg$id}/distance_comparison/.
#
#' @Purpose  : Compare distance matrices between the new pipeline and Dropbox legacy.
#' @details
#   METHODOLOGICAL DIFFERENCES
#   The new pipeline projects to an Azimuthal Equidistant (AEQD) projection whose
#   origin is the midpoint of the COMBINED bounding box of stations and geographic
#   units (not the station centroid), then measures planar distance. AEQD is exact
#   only radially from that origin; its off-centre scale factor is 1 + (rho/R)^2/6,
#   about 1e-5 at rho = 50 km — roughly 3 cm on a 3 km distance. So the projection
#   itself is NOT a meaningful source of disagreement at metro scale.
#
#   The real gaps are metric- and method-specific, which is why the two matrices
#   carry separate tolerances:
#     - Stations: legacy geosphere::distHaversine assumes a sphere of radius
#       6378137 m (the EQUATORIAL radius). Near the equator the meridional radius
#       of curvature is ~6335 km, so north-south pairs can differ from the
#       ellipsoidal value by ~0.5% — ~150 m over 30 km. Expected, not an error.
#       Hence station_tol_km = 0.25.
#     - Geo units: dominated by the representative-point change (legacy centroid
#       vs new st_point_on_surface), routinely 100-500 m. That is the quantity the
#       Step 0-4 ladder exists to measure, so geo_tol_km = 0.05 keeps it visible
#       rather than absorbing it into a "match".
#
#   STATION MATCHING
#   The new pipeline normalises station names (uppercase, no accents). The legacy
#   uses manually recoded names (e.g. "Usaquén" → "Usaquen"). Matching is done
#   after normalising both sides to uppercase ASCII.
#
#   GEO-UNIT DISTANCES
#   The new pipeline measures from st_point_on_surface() representative points.
#   The legacy measures from st_centroid() via sf::st_distance() on EPSG:4674
#   (SIRGAS 2000), using the s2 spherical engine. The summary reports match rates
#   twice: over all pairs, and over pairs within 5 km — only the latter can change
#   which units fall inside the 3 km buffer.
#
#   LEGACY DISTANCE UNITS
#   Legacy geo distances are always km, but 5_stats.R stored them differently per city.
#   Santiago saved before its own as.numeric() call, so that file keeps the units label
#   ("42.88 [km]") as character or factor; the other three saved after and are plain
#   numeric. as.numeric() on a factor returns level codes — small, plausible, and wrong
#   — so .parse_km() strips the label before converting.
#
#' @Written_on: 10/04/2026
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
compare_idw <- function(
    cfg,
    out_root,
    station_audit = NULL,
    quiet         = FALSE
) {
  
  # 0) Dependencies
  req_pkgs <- c("dplyr", "tidyr", "tibble", "arrow", "data.table")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  # 1) Validate cfg and extract paths
  cmp <- cfg$compare
  if (is.null(cmp)) stop("[", cfg$id, "] cfg$compare is NULL.")
  
  req_fields <- c("new_station_dist", "legacy_station_dist")
  missing_f <- setdiff(req_fields, names(cmp))
  if (length(missing_f) > 0) {
    stop("[", cfg$id, "] cfg$compare missing: ", paste(missing_f, collapse = ", "))
  }
  
  new_station_dist    <- cmp$new_station_dist
  legacy_station_dist <- cmp$legacy_station_dist
  new_geo_dist        <- cmp$new_geo_dist
  legacy_geo_dist     <- cmp$legacy_geo_dist
  station_tol_km      <- if (!is.null(cmp$station_tol_km)) cmp$station_tol_km else 0.25
  geo_tol_km          <- if (!is.null(cmp$geo_tol_km))     cmp$geo_tol_km     else 0.05
  # Geo pairs beyond this distance can never decide 3 km buffer eligibility, so
  # they are reported separately from the pairs that actually matter.
  near_km             <- 5
  
  if (!file.exists(new_station_dist))
    stop("[", cfg$id, "] New station dist file not found: ", new_station_dist)
  if (!file.exists(legacy_station_dist))
    stop("[", cfg$id, "] Legacy station dist file not found: ", legacy_station_dist)
  
  out_dir <- file.path(out_root, "distance_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!quiet) message("[", cfg$id, "] Comparing distance matrices ...")
  
  # 2) Normalisation helper
  .norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    if (requireNamespace("stringi", quietly = TRUE))
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    gsub('["\']', "", x)
  }

  # Strip any units label before converting, so a factor never yields level codes.
  # See @Details: legacy distance units.
  .parse_km <- function(x) {
    if (inherits(x, "units") || is.numeric(x)) return(as.numeric(x))
    as.numeric(gsub("[^0-9.eE+-]", "", as.character(x)))
  }

  # 3) Load new pipeline station distances
  # The new pipeline stores distances in "long" format (station_from, station_to, dist)
  new_sta <- arrow::read_parquet(new_station_dist) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      station_from = .norm(station_from),
      station_to   = .norm(station_to)
    )
  
  # 4) Load legacy station distances
  ext <- tools::file_ext(legacy_station_dist)
  leg_wide <- if (ext == "rds") {
    readRDS(legacy_station_dist)
  } else data.table::fread(legacy_station_dist)
  
  # The legacy pipeline stores distances as a wide matrix. We need to convert it 
  # to long format so we can join it easily with the new pipeline data.
  id_col <- if ("station_code" %in% names(leg_wide)) "station_code" else names(leg_wide)[1]
  sta_cols <- setdiff(names(leg_wide), id_col)
  
  # pivot_longer collapses the matrix columns down into rows
  leg_long <- leg_wide |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(sta_cols),
      names_to = "station_to",
      values_to = "distance_m_legacy"
    ) |>
    dplyr::rename(station_from = !!id_col) |>
    dplyr::mutate(
      station_from       = .norm(station_from),
      station_to         = .norm(station_to),
      distance_km_legacy = as.numeric(distance_m_legacy) / 1000
    ) |>
    dplyr::select(station_from, station_to, distance_km_legacy)
  
  # 5) Restrict new data to legacy station universe
  if (!is.null(station_audit)) {
    legacy_names <- .norm(station_audit$station[station_audit$in_legacy])
  } else {
    legacy_names <- unique(c(leg_long$station_from, leg_long$station_to))
  }
  
  new_sta_filtered <- new_sta |>
    dplyr::filter(station_from %in% legacy_names, station_to %in% legacy_names) |>
    dplyr::rename(distance_km_new = distance_km)
  
  if (!quiet) {
    message(sprintf("  Station pairs — new: %s | legacy: %s",
                    format(nrow(new_sta_filtered), big.mark = ","),
                    format(nrow(leg_long), big.mark = ",")))
  }
  
  # 6) Join and compare
  # full_join matches rows based on the origin and destination stations.
  joined <- dplyr::full_join(
    new_sta_filtered, leg_long, by = c("station_from", "station_to")
  ) |>
    dplyr::mutate(
      diff_km    = distance_km_new - distance_km_legacy,
      abs_diff   = abs(diff_km),
      within_tol = abs_diff <= station_tol_km |
        (is.na(distance_km_new) & is.na(distance_km_legacy))
    )
  
  # Summary: filter out self-distances (station A to station A)
  sta_summary <- joined |>
    dplyr::filter(!is.na(distance_km_new) & !is.na(distance_km_legacy)) |>
    dplyr::filter(station_from != station_to) |>
    dplyr::summarise(
      n_pairs       = dplyr::n(),
      n_match       = sum(within_tol, na.rm = TRUE),
      n_diff        = sum(!within_tol, na.rm = TRUE),
      mean_abs_diff = mean(abs_diff, na.rm = TRUE),
      max_abs_diff  = max(abs_diff, na.rm = TRUE),
      share_match   = n_match / n_pairs
    )
  
  sta_diffs <- joined |>
    dplyr::filter(!within_tol) |>
    dplyr::arrange(dplyr::desc(abs_diff))
  
  if (!quiet) {
    message(sprintf("  Station distances: %d pairs match (%.1f%%), %d differ > %.2f km",
                    sta_summary$n_match, 100 * sta_summary$share_match,
                    sta_summary$n_diff, station_tol_km))
    message(sprintf("  Mean abs diff: %.3f km | Max: %.3f km",
                    sta_summary$mean_abs_diff, sta_summary$max_abs_diff))
  }
  
  # 7) Geo-unit distances (optional)
  geo_summary <- NULL
  geo_diffs   <- NULL
  
  if (!is.null(new_geo_dist) && !is.null(legacy_geo_dist) &&
      file.exists(new_geo_dist) && file.exists(legacy_geo_dist)) {
    if (!quiet) message("  Comparing geo-unit distances ...")
    
    new_geo <- arrow::read_parquet(new_geo_dist) |>
      dplyr::as_tibble() |>
      dplyr::mutate(station_id = .norm(station_id)) |>
      dplyr::rename(distance_km_new = distance_km)
    
    leg_geo <- if (tools::file_ext(legacy_geo_dist) == "rds") {
      data.table::as.data.table(readRDS(legacy_geo_dist))
    } else data.table::fread(legacy_geo_dist)
    
    geo_id_col <- intersect(c("locality", "geo_id", "LocCodigo"), names(leg_geo))[1]
    sta_id_col <- intersect(c("station_code", "station_id"), names(leg_geo))[1]
    dist_col   <- intersect(c("distance", "distance_km"), names(leg_geo))[1]
    
    leg_geo_clean <- leg_geo |>
      dplyr::as_tibble() |>
      dplyr::transmute(
        geo_id             = as.character(.data[[geo_id_col]]),
        station_id         = .norm(.data[[sta_id_col]]),
        distance_km_legacy = .parse_km(.data[[dist_col]])
      )

    # A fully unparseable column would otherwise sail through as 0% match.
    if (all(is.na(leg_geo_clean$distance_km_legacy))) {
      stop("[", cfg$id, "] Legacy geo distances in '", dist_col,
           "' parsed to all NA. Check the column format in ", legacy_geo_dist)
    }

    new_geo_filtered <- new_geo |> dplyr::filter(station_id %in% legacy_names)
    
    geo_joined <- dplyr::inner_join(
      new_geo_filtered, leg_geo_clean, by = c("geo_id", "station_id")
    ) |>
      dplyr::mutate(
        diff_km    = distance_km_new - distance_km_legacy,
        abs_diff   = abs(diff_km),
        within_tol = abs_diff <= geo_tol_km,
        # Only pairs this close can flip 3 km buffer eligibility.
        is_near    = distance_km_new <= near_km | distance_km_legacy <= near_km
      )

    geo_summary <- geo_joined |>
      dplyr::summarise(
        n_pairs         = dplyr::n(),
        n_match         = sum(within_tol, na.rm = TRUE),
        mean_abs_diff   = mean(abs_diff, na.rm = TRUE),
        max_abs_diff    = max(abs_diff, na.rm = TRUE),
        share_match     = n_match / n_pairs,
        # Same statistics restricted to the pairs that decide eligibility. A
        # 40 km pair off by 200 m changes nothing; a 3 km pair off by 200 m can
        # move a geographic unit in or out of the buffer.
        n_pairs_near    = sum(is_near, na.rm = TRUE),
        n_match_near    = sum(within_tol & is_near, na.rm = TRUE),
        mean_abs_diff_near = mean(abs_diff[is_near], na.rm = TRUE),
        max_abs_diff_near  = max(abs_diff[is_near], na.rm = TRUE),
        share_match_near   = n_match_near / n_pairs_near,
        # Carried so the report can label the near-pair row without hardcoding.
        near_km            = near_km
      )

    geo_diffs <- geo_joined |>
      dplyr::filter(!within_tol) |>
      dplyr::arrange(dplyr::desc(abs_diff))

    if (!quiet) {
      message(sprintf("  Geo distances: %d pairs, %.1f%% match within %.2f km",
                      geo_summary$n_pairs, 100 * geo_summary$share_match, geo_tol_km))
      message(sprintf("  Geo (<= %d km): %d pairs, %.1f%% match | max diff %.3f km",
                      near_km, geo_summary$n_pairs_near,
                      100 * geo_summary$share_match_near,
                      geo_summary$max_abs_diff_near))
    }
  }
  
  # 8) Method note
  method_note <- paste0(
    "The new pipeline measures planar distance in an AEQD (Azimuthal ",
    "Equidistant) projection; the legacy uses great-circle distance ",
    "(geosphere::distm for stations, sf/s2 for geographic units). The ",
    "projection contributes almost nothing at metro scale (~3 cm on a 3 km ",
    "distance). Station-pair differences are driven by geosphere's equatorial ",
    "Earth radius (~150 m over 30 km near the equator); geo-unit differences ",
    "are driven by the representative point (legacy st_centroid vs new ",
    "st_point_on_surface), routinely 100-500 m. Tolerances are therefore split: ",
    sprintf("%.2f km for stations, %.2f km for geographic units.",
            station_tol_km, geo_tol_km)
  )
  
  # 9) Persist
  write_pq(sta_summary, out_dir, "station_dist_summary")
  if (nrow(sta_diffs) > 0) write_pq(sta_diffs, out_dir, "station_dist_diffs")
  if (!is.null(geo_summary)) write_pq(geo_summary, out_dir, "geo_dist_summary")
  if (!is.null(geo_diffs) && nrow(geo_diffs) > 0) {
    write_pq(geo_diffs, out_dir, "geo_dist_diffs")
  }
  
  if (!quiet) message("[", cfg$id, "] Distance comparison saved to: ", out_dir)
  
  invisible(list(
    station_dist_summary = sta_summary,
    station_dist_diffs   = sta_diffs,
    geo_dist_summary     = geo_summary,
    geo_dist_diffs       = geo_diffs,
    method_note          = method_note,
    out_dir              = out_dir
  ))
}


# ----------------------------------------------------------------------------------
# compare_outlier_procedure
#' @param cfg          city cfg list (must contain $id and $compare sublist).
#' @param out_root     root output folder; {out_root}/{cfg$id}/ is created.
#' @param station_audit data.frame; from compare_ground_stations().
#' @param quiet        logical; suppress messages. Default FALSE.
#
#' @return  named list (invisible) with step_summary, comparison, out_dir.
#           Parquet files written to {out_root}/{cfg$id}/outlier_comparison/.
# ----------------------------------------------------------------------------------
compare_outlier_procedure <- function(
    cfg,
    out_root,
    station_audit = NULL,
    quiet         = FALSE
) {
  
  req_pkgs <- c("dplyr", "tibble", "arrow", "data.table")
  for (p in req_pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  cmp <- cfg$compare
  if (is.null(cmp)) stop("[", cfg$id, "] cfg$compare is NULL.")
  
  new_clean_dir     <- cmp$new_clean_dir
  new_raw_dir       <- here::here("data", "raw", "monitoring_stations", 
                                  paste0(cfg$id, "_metro_dataset"))
  legacy_clean_path <- cmp$legacy_clean_path
  legacy_raw_path   <- cmp$legacy_single_csv
  station_dist_path <- cmp$new_station_dist
  
  pollutants    <- cmp$focus_pollutants %||% c("pm10", "pm25")
  compare_years <- cmp$compare_years
  
  out_dir <- file.path(out_root, "outlier_comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  .norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    if (requireNamespace("stringi", quietly = TRUE))
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
    gsub('["\']', "", x)
  }
  
  # 1) Check Configuration Toggle for Legacy Test
  out_p <- cmp$outlier_params
  if (isTRUE(out_p$use_legacy_input) && file.exists(legacy_raw_path)) {
    if (!quiet) message("  [Toggle] Running NEW algorithm on LEGACY raw data...")
    if (is.null(station_dist_path)) stop("station_dist_path required for test.")
    
    # Manually and safely load legacy data to bypass external pipeline bugs
    leg_raw <- data.table::fread(legacy_raw_path)
    names(leg_raw) <- tolower(names(leg_raw))
    
    sta_col <- intersect(c("station_code", "station"), names(leg_raw))[1]
    if (!is.na(sta_col)) leg_raw[, station := .norm(get(sta_col))]
    
    if (length(cmp$residual_map) > 0L) {
      leg_raw[, station := dplyr::recode(station, !!!cmp$residual_map)]
    }
    
    # ROBUST DATE MATH: Extracts date, mathematically adds hours (fixes hour==24)
    if (!"datetime" %in% names(leg_raw)) {
      if ("datehour" %in% names(leg_raw)) {
        leg_raw[, raw_date_str := sub(" .*$", "", trimws(datehour))]
        parsed_date <- as.Date(leg_raw$raw_date_str, format = "%d%b%Y")
      } else if ("date" %in% names(leg_raw)) {
        parsed_date <- as.Date(leg_raw$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
      } else {
        parsed_date <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
      }
      
      # Convert Date to POSIXct and add seconds. 
      leg_raw[, datetime := as.POSIXct(format(parsed_date, "%Y-%m-%d"), 
                                       format = "%Y-%m-%d", 
                                       tz = "UTC") + (as.integer(hour) * 3600)]
      attr(leg_raw$datetime, "tzone") <- cmp$pipeline_tz %||% cfg$tz
    }
    
    if (!"year" %in% names(leg_raw)) {
      leg_raw[, year := as.integer(format(datetime, "%Y"))]
    }
    
    mock_raw <- file.path(tempdir(), "legacy_arrow_raw")
    mock_out <- file.path(tempdir(), "legacy_arrow_test")
    if (dir.exists(mock_raw)) unlink(mock_raw, recursive = TRUE)
    
    arrow::write_dataset(leg_raw, mock_raw, format="parquet", partitioning="year")
    
    detect_pollution_outliers(
      arrow_dir           = mock_raw,
      station_dist_path   = station_dist_path,
      out_dir             = mock_out,
      out_name            = "legacy_mock",
      pollutants          = pollutants,
      pct_flag            = out_p$pct_flag %||% 0.99,
      n_sd                = out_p$n_sd %||% 2,
      on_missing_temporal = out_p$on_missing_temporal %||% "finish",
      on_missing_neighbor = out_p$on_missing_neighbor %||% "finish",
      neighbor_eligibility = out_p$neighbor_eligibility %||% "all",
      quiet               = quiet
    )
    
    new_raw_dir   <- mock_raw
    new_clean_dir <- file.path(mock_out, "legacy_mock_clean")
  }
  
  if (!dir.exists(new_clean_dir)) stop("New clean dir not found: ", new_clean_dir)
  if (!dir.exists(new_raw_dir)) stop("New raw dir not found: ", new_raw_dir)
  
  if (!quiet) message("[", cfg$id, "] Comparing outlier detection procedures ...")
  
  legacy_names <- if (!is.null(station_audit)) {
    .norm(station_audit$station[station_audit$in_legacy])
  } else NULL
  
  # 2) Load target datasets
  new_raw <- arrow::open_dataset(new_raw_dir) |>
    dplyr::filter(year %in% compare_years) |>
    dplyr::collect() |> data.table::as.data.table()
  new_raw[, station := .norm(station)]
  
  new_clean <- arrow::open_dataset(new_clean_dir) |>
    dplyr::filter(year %in% compare_years) |>
    dplyr::collect() |> data.table::as.data.table()
  new_clean[, station := .norm(station)]
  
  if (!is.null(legacy_names)) {
    new_raw   <- new_raw[station %in% legacy_names]
    new_clean <- new_clean[station %in% legacy_names]
  }
  
  # 3) Build step-level summary pulling the new Diagnostic Reason Codes
  step_list <- list()
  for (pol in pollutants) {
    flag_col   <- paste0(pol, "_outlier")
    reason_col <- paste0(pol, "_outlier_reason")
    if (!pol %in% names(new_raw) || !flag_col %in% names(new_clean)) next
    
    raw_vals     <- new_raw[[pol]]
    outlier_flag <- new_clean[[flag_col]]
    
    n_total     <- sum(!is.na(raw_vals))
    n_outlier   <- sum(outlier_flag == 1L, na.rm = TRUE)
    pct_removed <- round(100 * n_outlier / max(n_total, 1), 3)
    
    # Extract failure point breakdowns based on new detect_pollution_outliers logic
    if (reason_col %in% names(new_clean)) {
      reason_vals <- new_clean[[reason_col]]
      n_r1 <- sum(reason_vals == 1L, na.rm = TRUE)
      n_r2 <- sum(reason_vals == 2L, na.rm = TRUE)
      n_r3 <- sum(reason_vals == 3L, na.rm = TRUE)
    } else {
      n_r1 <- NA_integer_; n_r2 <- NA_integer_; n_r3 <- NA_integer_
    }
    
    step_list[[pol]] <- tibble::tibble(
      pollutant        = toupper(pol),
      total_obs        = n_total,
      outliers_removed = n_outlier,
      pct_removed      = pct_removed,
      reason_no_temp   = n_r1,
      reason_no_spat   = n_r2,
      reason_fail_both = n_r3
    )
  }
  step_summary <- dplyr::bind_rows(step_list)
  
  # 4) Load legacy cleaned data and compare
  comparison <- NULL
  if (file.exists(legacy_clean_path)) {
    ext <- tools::file_ext(legacy_clean_path)
    leg_clean <- if (ext == "rds") {
      data.table::as.data.table(readRDS(legacy_clean_path))
    } else data.table::fread(legacy_clean_path)
    
    names(leg_clean) <- tolower(names(leg_clean))
    sta_col <- intersect(c("station_code", "station"), names(leg_clean))[1]
    if (!is.na(sta_col)) leg_clean[, station := .norm(get(sta_col))]
    
    leg_raw_exists <- file.exists(legacy_raw_path)
    if (leg_raw_exists) {
      ext_raw <- tools::file_ext(legacy_raw_path)
      leg_raw_chk <- if (ext_raw == "rds") {
        data.table::as.data.table(readRDS(legacy_raw_path))
      } else data.table::fread(legacy_raw_path)
      
      names(leg_raw_chk) <- tolower(names(leg_raw_chk))
      sta_col_raw <- intersect(c("station_code", "station"), names(leg_raw_chk))[1]
      if (!is.na(sta_col_raw)) leg_raw_chk[, station := .norm(get(sta_col_raw))]
      if ("year" %in% names(leg_raw_chk)) leg_raw_chk <- leg_raw_chk[year %in% compare_years]
    }
    
    comp_list <- list()
    for (pol in pollutants) {
      flag_col <- paste0(pol, "_outlier")
      if (flag_col %in% names(new_clean)) {
        new_out <- new_clean[, .(
          outliers_new = sum(get(flag_col) == 1L, na.rm = TRUE),
          total_new    = sum(!is.na(new_raw[[pol]][
            match(paste(station, datetime), paste(new_raw$station, new_raw$datetime))
          ]))
        ), by = station]
      } else next
      
      leg_out <- tibble::tibble(station = character(), outliers_legacy = integer())
      if (pol %in% names(leg_clean) && leg_raw_exists && pol %in% names(leg_raw_chk)) {
        if ("will_be_na" %in% names(leg_clean)) {
          will_na_col <- if (pol == "pm25") "will_be_na_pm25" else "will_be_na"
          if (will_na_col %in% names(leg_clean)) {
            leg_out <- leg_clean[, .(
              outliers_legacy = sum(get(will_na_col) == 1L, na.rm = TRUE)
            ), by = station]
          }
        }
      }
      
      comp_list[[pol]] <- dplyr::full_join(
        dplyr::as_tibble(new_out), dplyr::as_tibble(leg_out), by = "station"
      ) |> dplyr::mutate(pollutant = toupper(pol))
    }
    comparison <- dplyr::bind_rows(comp_list)
  }
  
  # 5) Persist
  write_pq(step_summary, out_dir, "step_summary")
  if (!is.null(comparison) && nrow(comparison) > 0) {
    write_pq(comparison, out_dir, "outlier_comparison")
  }
  
  if (!quiet) message("[", cfg$id, "] Outlier comparison saved to: ", out_dir)
  
  invisible(list(
    step_summary = step_summary, comparison = comparison, out_dir = out_dir
  ))
}


# ----------------------------------------------------------------------------------------
# Function: compute_exposure_regressions_legacy
#
#' @param exposure_dt  data.table; geo-level IDW exposure (one row per geo unit-year).
#' @param individual_dt data.table; individual census microdata with group and weight.
#' @param geo_id_col   string; geographic identifier column. Default "geo_id".
#' @param pop_col      string; expansion-weight column (Santiago: a column of 1s).
#' @param group_col    string; socioeconomic group column. Default "edu_quintile".
#' @param group_values integer vector; valid groups, e.g. 1:5.
#' @param base_group   integer; omitted reference group. Default max(group_values).
#' @param outcomes     character vector; exposure columns to regress, one model each.
#' @param year_filter  integer; exposure year to keep. Default 2023.
#' @param conf_level   numeric; confidence level. Default 0.95.
#' @param listwise     logical; drop cells missing ANY outcome before weighting.
#                      TRUE reproduces legacy Santiago; FALSE the other three cities.
#' @param quiet        logical; suppress messages. Default FALSE.
#
#' @return  data.table with the same columns as compute_exposure_regressions(), so the
#           two can be stacked and differenced directly.
#
#' @details
#   STEP-0 LEGACY REPLICATION ONLY — reproduces the coauthor's exposure regression so
#   the Quarto reports can difference it against compute_exposure_regressions() on
#   identical inputs. It differs from the current estimator in exactly three ways, all
#   deliberate:
#     1. Standard errors are homoskedastic (plain confint() on lm), not clustered by
#        geographic unit. This is the change the paper's inference appendix describes.
#     2. The within-group weight denominator is summed over ALL geo-by-group cells,
#        including those whose exposure is NaN for the outcome being fit. Because the
#        model is saturated the coefficients are unaffected, but the standard errors
#        are: a per-group rescale of the weights does not cancel in the variance.
#     3. Individuals whose geographic unit never receives an exposure value are kept
#        through the collapse (legacy used a left join), so they enter the denominator
#        in (2) even though lm() later drops them.
#   The adult 25+ restriction is NOT applied here. Legacy applied it in all
#   four cities, in both the quintile and the decile scripts, so to reproduce
#   the legacy numbers pass individual_dt already filtered to adults; pass it
#   unfiltered only to audit the restriction's own effect.
#   Do not use for paper results; use compute_exposure_regressions() instead.
#
#' @Written_on : July 2026
#' @Written_by : Marcos Paulo
# ----------------------------------------------------------------------------------------
compute_exposure_regressions_legacy <- function(
    exposure_dt,
    individual_dt,
    geo_id_col   = "geo_id",
    pop_col      = "fe",
    group_col    = "edu_quintile",
    group_values = 1:5,
    base_group   = max(group_values),
    outcomes,
    year_filter  = 2023L,
    conf_level   = 0.95,
    listwise     = FALSE,
    quiet        = FALSE
) {


  # 1. Left join exposure onto individuals, exactly as the legacy script did.
  # -----------------------------------------------------------------------
  # The direction matters: individuals in geographic units that never received an
  # exposure value stay in the table with NA outcomes, and they keep counting
  # toward the group population totals computed in step 4.
  exp_dt <- data.table::copy(data.table::as.data.table(exposure_dt))
  exp_dt <- exp_dt[year == year_filter]
  exp_dt[, (geo_id_col) := as.character(get(geo_id_col))]

  # Only three columns of the microdata matter here; dropping the rest keeps the
  # individual-level join small enough to inspect (Bogota is 5.7M rows).
  ind_cols <- c(geo_id_col, group_col, pop_col)
  ind <- data.table::as.data.table(individual_dt)[, ..ind_cols]
  ind[, (geo_id_col) := as.character(get(geo_id_col))]
  ind <- ind[get(group_col) %in% group_values &
               !is.na(get(pop_col)) & get(pop_col) > 0]

  keep <- c(geo_id_col, outcomes)
  merged <- merge(ind, exp_dt[, ..keep], by = geo_id_col, all.x = TRUE)

  # 2. Reference mean of the base group, at the individual level, per outcome.
  # -----------------------------------------------------------------------
  # Legacy took this over every individual in the top quintile with a non-missing
  # value for that outcome, weighting by the expansion factor.
  base_rows <- merged[get(group_col) == base_group]

  base_means <- vapply(outcomes, function(col) {
    stats::weighted.mean(base_rows[[col]], base_rows[[pop_col]], na.rm = TRUE)
  }, numeric(1))

  # 3. Normalize each outcome so the reference group averages one.
  # -----------------------------------------------------------------------
  norm_cols <- paste0("norm_", outcomes)

  for (i in seq_along(outcomes)) {
    merged[, (norm_cols[i]) := get(outcomes[i]) / base_means[i]]
  }

  # 4. Collapse to geo-by-group cells and build the legacy weights.
  # -----------------------------------------------------------------------
  # An all-NA cell collapses to NaN here, which is what keeps it in the table but
  # out of the model. geo_population is the cell head count regardless.
  cells <- merged[
    ,
    c(list(geo_population = sum(get(pop_col), na.rm = TRUE)),
      lapply(.SD, function(x) stats::weighted.mean(x, get(pop_col), na.rm = TRUE))),
    by = c(geo_id_col, group_col),
    .SDcols = norm_cols
  ]

  # Legacy Santiago dropped any cell missing ANY outcome, so all four of its
  # regressions share one sample; the other three cities dropped per outcome.
  if (isTRUE(listwise)) {
    complete <- Reduce(`&`, lapply(norm_cols, function(c) !is.na(cells[[c]])))
    cells <- cells[complete]
  }

  # The denominator spans every cell, including the NaN ones lm() will drop.
  cells[, total_population_q := sum(geo_population, na.rm = TRUE), by = group_col]
  cells[, weight2 := geo_population / total_population_q]

  # 5. One weighted regression per outcome, with homoskedastic intervals.
  # -----------------------------------------------------------------------
  res <- data.table::rbindlist(lapply(seq_along(outcomes), function(i) {

    model_dt <- cells[!is.na(get(norm_cols[i])) & weight2 > 0]

    if (nrow(model_dt) < length(group_values)) {
      return(NULL)
    }

    # Name the outcome plainly so the fitted object reads like the legacy one.
    model_dt[, y := get(norm_cols[i])]
    model_dt[, g := factor(get(group_col),
                           levels = c(base_group,
                                      setdiff(group_values, base_group)))]

    fit <- stats::lm(y ~ g, data = model_dt, weights = weight2)
    ci  <- stats::confint(fit, level = conf_level)
    cf  <- stats::coef(summary(fit))

    # Split "hrs_d_pm10_it1" into outcome "hrs_d_it1" and pollutant "pm10".
    pollutant <- if (grepl("pm25", outcomes[i], fixed = TRUE)) "pm25" else "pm10"
    outcome   <- sub("_$", "", sub("^_", "",
                                   sub(paste0("_", pollutant, "_?"), "_",
                                       outcomes[i])))

    # Rows are assembled in the schema of compute_exposure_regressions() so the
    # legacy and current estimates can be stacked without renaming anything.
    make_row <- function(grp, est, se, lo, hi) {
      data.table::data.table(
        outcome = outcome, pollutant = pollutant, group = grp,
        estimate = est, std_error = se, ci_low = lo, ci_high = hi,
        n_units = nrow(model_dt),
        n_clusters = data.table::uniqueN(model_dt[[geo_id_col]]),
        base_group = base_group, group_col = group_col,
        regression_unit = "geo_group", se_type = "legacy_classic",
        normalized = TRUE
      )
    }

    out <- make_row(base_group, 0, 0, 0, 0)

    for (grp in setdiff(group_values, base_group)) {
      term <- paste0("g", grp)

      if (!term %in% rownames(cf)) {
        next
      }

      out <- data.table::rbindlist(
        list(out, make_row(grp, cf[term, "Estimate"], cf[term, "Std. Error"],
                           ci[term, 1], ci[term, 2])),
        fill = TRUE
      )
    }

    out
  }), fill = TRUE)

  data.table::setorder(res, outcome, pollutant, group)

  if (!quiet) {
    message("[ci-legacy] ", length(outcomes), " outcome(s) fit | listwise = ",
            listwise, " | homoskedastic intervals.")
  }

  return(res[])
}
