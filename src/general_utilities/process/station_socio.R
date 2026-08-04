# ============================================================================================
# IDB: Air monitoring — station socioeconomic context
# ============================================================================================
# @Goal: Functions for station socioeconomic context.
#
# @Description: Summarises pollution per station and attaches the socioeconomic profile of the census
#   units around it, which feeds the station-level scatter figures.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
# @Summary:
#   1. compute_station_pollution_summary
#   2. compute_station_socio_context
#   3. build_station_scatter_inputs
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: compute_station_pollution_summary
#
# @Arg arrow_dir        : string; path to partitioned Arrow/Parquet hourly data.
# @Arg year_filter      : integer; year to process. Default 2023.
# @Arg station_col      : string; station column in the pollution data.
# @Arg pollutants       : character vector; pollutant columns to summarize.
# @Arg who_it           : named list; WHO interim target thresholds.
# @Arg min_obs_active   : integer; minimum observations to define active station.
# @Arg quiet            : logical; suppress messages. Default FALSE.
#
# @Output : data.table with one row per active station.
#
# @Details:
#   Computes station-level annual means and hours above WHO thresholds. A station
#   is considered active if it has at least min_obs_active non-missing observation
#   for at least one pollutant in the requested year. Hours above a threshold count
#   hourly observations at or above it; the 24-hour IT values are used as a proxy
#   for an hourly extreme-pollution threshold, consistent with the IDW step.
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_station_pollution_summary <- function(
    arrow_dir,
    year_filter    = 2023L,
    station_col    = "station",
    pollutants     = c("pm10", "pm25"),
    who_it         = list(
      pm10 = c(it1 = 150, it2 = 100),
      pm25 = c(it1 = 75,  it2 = 50)
    ),
    min_obs_active = 1L,
    quiet          = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  pkgs <- c("arrow", "dplyr", "data.table", "stringi")
  
  for (p in pkgs) {
  }
  
  # 1. Validate inputs
  # -----------------------------------------------------------------------
  if (!dir.exists(arrow_dir)) {
    stop("`arrow_dir` not found: ", arrow_dir)
  }
  
  # Normalize station identifiers consistently across the pipeline.
  
  # 2. Open Arrow dataset and collect the requested year
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[station_summary] Reading hourly data for year ", year_filter, ".")
  }
  
  ds <- arrow::open_dataset(arrow_dir)
  
  fields <- names(ds)
  keep_pollutants <- intersect(pollutants, fields)
  
  if (!station_col %in% fields) {
    stop("Column '", station_col, "' not found in pollution dataset.")
  }
  
  if (!"year" %in% fields) {
    stop("Column 'year' not found in pollution dataset.")
  }
  
  if (length(keep_pollutants) == 0L) {
    stop("None of the requested pollutants were found in the dataset.")
  }
  
  keep_cols <- unique(c(station_col, "year", keep_pollutants))
  
  dt <- ds |>
    dplyr::filter(year == year_filter) |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::collect() |>
    data.table::as.data.table()
  
  if (nrow(dt) == 0L) {
    stop("No pollution data found for year ", year_filter, ".")
  }
  
  data.table::setnames(dt, station_col, "station_raw")
  dt[, station_id := normalize_station(station_raw)]
  
  # 3. Compute station-level outcomes for each pollutant
  # -----------------------------------------------------------------------
  out_list <- vector("list", length(keep_pollutants))
  names(out_list) <- keep_pollutants
  
  for (pol in keep_pollutants) {
    
    # Build threshold summaries for the pollutant.
    thr <- who_it[[pol]]
    
    stat_dt <- dt[
      ,
      .(
        station_name_raw = station_raw[which(!is.na(station_raw))[1]],
        n_obs = sum(!is.na(get(pol))),
        avg = mean(get(pol), na.rm = TRUE)
      ),
      by = station_id
    ]
    
    data.table::setnames(
      stat_dt,
      c("n_obs", "avg"),
      c(paste0("n_obs_", pol), paste0("avg_", pol))
    )
    
    # Add hours above each WHO threshold.
    if (!is.null(thr) && length(thr) > 0L) {
      for (nm in names(thr)) {
        col_nm <- paste0("hrs_d_", pol, "_", nm)
        
        tmp <- dt[
          ,
          .(value = sum(get(pol) >= thr[[nm]], na.rm = TRUE)),
          by = station_id
        ]
        
        data.table::setnames(tmp, "value", col_nm)
        stat_dt <- merge(stat_dt, tmp, by = "station_id", all.x = TRUE)
      }
    }
    
    out_list[[pol]] <- stat_dt
  }
  
  # 4. Merge pollutant-level summaries and define active stations
  # -----------------------------------------------------------------------
  out <- Reduce(
    function(a, b) merge(a, b, by = "station_id", all = TRUE),
    out_list
  )
  
  # Coalesce raw station names after pollutant-level merges.
  raw_cols <- grep("^station_name_raw", names(out), value = TRUE)
  
  if (length(raw_cols) > 1L) {
    out[, station_name := do.call(data.table::fcoalesce, .SD), .SDcols = raw_cols]
    out[, (raw_cols) := NULL]
  } else if (length(raw_cols) == 1L) {
    data.table::setnames(out, raw_cols, "station_name")
  }
  
  obs_cols <- grep("^n_obs_", names(out), value = TRUE)
  
  out[
    ,
    active_2023 := as.integer(
      rowSums(.SD >= min_obs_active, na.rm = TRUE) > 0
    ),
    .SDcols = obs_cols
  ]
  
  out <- out[active_2023 == 1L]
  
  if (!quiet) {
    message("[station_summary] Active stations: ", nrow(out), ".")
  }
  
  return(out[])
}


# --------------------------------------------------------------------------------------------
# Function: compute_station_socio_context
#
# @Arg stations_sf        : sf POINT object; monitoring stations.
# @Arg geo_sf             : sf POLYGON object; geographic units.
# @Arg census_col         : data.frame; collapsed census data by geographic unit.
# @Arg station_id_col     : string; station ID/name column in stations_sf.
# @Arg geo_id_col         : string; geographic unit ID column.
# @Arg pop_col            : string; population or expansion-weight column.
# @Arg socio_vars         : character vector; socioeconomic variables to attach.
# @Arg context_method     : string; "containing_geo" or "buffer".
# @Arg buffer_km          : numeric; buffer radius when context_method = "buffer".
# @Arg representative_pt  : string; "point_on_surface" or "centroid".
# @Arg geo_id_repair      : string; "none", "bogota", or "suffix".
# @Arg bogota_max_suffix  : integer; maximum suffix repair for Bogota IDs.
# @Arg bogota_broad_ids   : logical; allow broad Bogota ID repairs?
# @Arg quiet              : logical; suppress messages. Default FALSE.
#
# @Output : data.table with one row per station and socioeconomic context.
#
# @Details:
#   In containing_geo mode, each station receives the characteristics of the
#   polygon containing it; if a station falls on a shared boundary and matches
#   more than one polygon, only the first match is kept and a message reports it.
#   In buffer mode, each station receives population-weighted averages across
#   geographic units whose representative point lies within buffer_km. Buffer mode
#   suits cities with very small units (e.g. Bogota), where a single containing
#   unit is a noisy descriptor of the station's local socioeconomic context.
#
#   geo_id_repair = "suffix" handles cases where the spatial layer stores only the
#   municipality component while the census stores a full state-municipality code.
#   A repair is accepted only when the suffix match is unique.
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_station_socio_context <- function(
    stations_sf,
    geo_sf,
    census_col,
    station_id_col,
    geo_id_col,
    pop_col,
    socio_vars,
    context_method    = c("containing_geo", "buffer"),
    buffer_km         = 3,
    representative_pt = c("point_on_surface", "centroid"),
    geo_id_repair     = c("none", "bogota", "suffix"),
    bogota_max_suffix = 2L,
    bogota_broad_ids  = FALSE,
    quiet             = FALSE
) {
  
  # 0. Dependencies and argument matching
  # -----------------------------------------------------------------------
  pkgs <- c("sf", "data.table", "stringi")
  
  for (p in pkgs) {
  }
  
  context_method <- match.arg(context_method)
  representative_pt <- match.arg(representative_pt)
  geo_id_repair <- match.arg(geo_id_repair)
  
  # 1. Input checks
  # -----------------------------------------------------------------------
  if (!inherits(stations_sf, "sf")) {
    stop("`stations_sf` must be an sf object.")
  }
  
  if (!inherits(geo_sf, "sf")) {
    stop("`geo_sf` must be an sf object.")
  }
  
  if (!station_id_col %in% names(stations_sf)) {
    stop("Column '", station_id_col, "' not found in stations_sf.")
  }
  
  if (!geo_id_col %in% names(geo_sf)) {
    stop("Column '", geo_id_col, "' not found in geo_sf.")
  }
  
  req_census_cols <- c(geo_id_col, pop_col, socio_vars)
  miss_census <- setdiff(req_census_cols, names(census_col))
  
  if (length(miss_census) > 0L) {
    stop("census_col is missing: ", paste(miss_census, collapse = ", "))
  }
  
  # 2. Helpers
  # -----------------------------------------------------------------------
  
  
  # Left-pad a character vector with zeros to a fixed width (base-R only).
  .pad_left0 <- function(x, width) {
    x <- as.character(x)
    need <- pmax(0L, width - nchar(x))
    paste0(strrep("0", need), x)
  }
  
  .weighted_mean <- function(x, w) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    
    if (!any(ok)) {
      return(NA_real_)
    }
    
    sum(x[ok] * w[ok]) / sum(w[ok])
  }
  
  .repair_suffix_ids <- function(geo_ids, census_ids) {
    
    geo_chr <- safe_chr(geo_ids)
    census_chr <- safe_chr(census_ids)
    
    out <- data.table::data.table(
      geo_id_original = geo_chr,
      geo_id_repaired = geo_chr,
      repair_method = "exact",
      matched_repaired = geo_chr %in% census_chr
    )
    
    unmatched <- which(!out$matched_repaired & !is.na(out$geo_id_original))
    
    if (length(unmatched) == 0L) {
      return(out)
    }
    
    census_unique <- unique(census_chr[!is.na(census_chr)])
    
    # Modal census width used for left-pad attempts.
    width_modal <- as.integer(names(sort(
      table(nchar(census_unique)),
      decreasing = TRUE
    ))[1])
    
    for (i in unmatched) {
      id_i <- out$geo_id_original[i]
      
      # Try left-padding to the modal census ID width (base-R zero pad).
      id_pad <- .pad_left0(id_i, width_modal)
      
      if (id_pad %in% census_unique) {
        out$geo_id_repaired[i] <- id_pad
        out$repair_method[i] <- "left_pad"
        out$matched_repaired[i] <- TRUE
        next
      }
      
      # Try unique suffix matching. This fixes cases like 002 -> 9002.
      suffix_matches <- census_unique[endsWith(census_unique, id_i)]
      
      if (length(suffix_matches) == 1L) {
        out$geo_id_repaired[i] <- suffix_matches
        out$repair_method[i] <- "unique_suffix"
        out$matched_repaired[i] <- TRUE
        next
      }
      
      out$repair_method[i] <- "unmatched"
    }
    
    out[]
  }
  
  # 3. Prepare spatial and census data
  # -----------------------------------------------------------------------
  stations_wgs <- sf::st_transform(stations_sf, crs = 4326)

  # Repair on a UTM grid before moving to lon/lat
  geo_utm <- sf::st_make_valid(sf::st_transform(geo_sf, crs = utm_epsg(geo_sf)))
  geo_wgs <- sf::st_transform(geo_utm, crs = 4326)
  
  stations_wgs$station_id <- normalize_station(stations_wgs[[station_id_col]])
  geo_wgs[[geo_id_col]] <- safe_chr(geo_wgs[[geo_id_col]])
  
  census_dt <- data.table::copy(data.table::as.data.table(census_col))
  census_dt[, (geo_id_col) := safe_chr(get(geo_id_col))]
  
  # Keep only variables needed for this step.
  census_keep <- unique(c(geo_id_col, pop_col, socio_vars))
  census_dt <- census_dt[, ..census_keep]
  
  # 4. Repair geographic IDs before merging census attributes
  # -----------------------------------------------------------------------
  if (geo_id_repair == "bogota") {
    
    if (!exists("repair_bogota_geo_ids", mode = "function")) {
      stop(
        "geo_id_repair = 'bogota' requires repair_bogota_geo_ids() ",
        "to be defined in config_utils_process_data.R."
      )
    }
    
    id_xwalk <- repair_bogota_geo_ids(
      geo_ids = geo_wgs[[geo_id_col]],
      census_ids = census_dt[[geo_id_col]],
      max_zero_suffix = bogota_max_suffix,
      allow_broad_ids = bogota_broad_ids
    )
    
    repair_cols <- c(
      "geo_id_original",
      "geo_id_repaired",
      "repair_method",
      "matched_repaired"
    )
    
    id_xwalk <- id_xwalk[, ..repair_cols]
    
    geo_wgs$geo_id_original <- geo_wgs[[geo_id_col]]
    geo_dt <- data.table::as.data.table(sf::st_drop_geometry(geo_wgs))
    
    geo_dt <- merge(
      geo_dt,
      id_xwalk,
      by.x = "geo_id_original",
      by.y = "geo_id_original",
      all.x = TRUE
    )
    
    geo_wgs[[geo_id_col]] <- geo_dt$geo_id_repaired
    
    if (!quiet) {
      msg <- id_xwalk[
        ,
        .N,
        by = repair_method
      ][order(repair_method)]
      
      message("[station_context] Bogota ID repair summary:")
      print(msg)
    }
  }
  
  if (geo_id_repair == "suffix") {
    
    id_xwalk <- .repair_suffix_ids(
      geo_ids = geo_wgs[[geo_id_col]],
      census_ids = census_dt[[geo_id_col]]
    )
    
    geo_wgs$geo_id_original <- geo_wgs[[geo_id_col]]
    geo_dt <- data.table::as.data.table(sf::st_drop_geometry(geo_wgs))
    
    geo_dt <- merge(
      geo_dt,
      id_xwalk,
      by.x = "geo_id_original",
      by.y = "geo_id_original",
      all.x = TRUE
    )
    
    geo_wgs[[geo_id_col]] <- geo_dt$geo_id_repaired
    
    if (!quiet) {
      msg <- id_xwalk[
        ,
        .N,
        by = repair_method
      ][order(repair_method)]
      
      message("[station_context] Suffix ID repair summary:")
      print(msg)
    }
  }
  
  # 5. Merge census attributes into geographic units
  # -----------------------------------------------------------------------
  geo_wgs <- merge(
    geo_wgs,
    census_dt,
    by = geo_id_col,
    all.x = TRUE
  )
  
  # 6. Method 1: socioeconomic context from containing polygon
  # -----------------------------------------------------------------------
  if (context_method == "containing_geo") {
    
    if (!quiet) {
      message("[station_context] Using containing geographic unit.")
    }
    
    joined <- suppressWarnings(
      sf::st_join(
        stations_wgs[, c("station_id", station_id_col)],
        geo_wgs[, c(geo_id_col, pop_col, socio_vars)],
        join = sf::st_intersects,
        left = TRUE
      )
    )
    
    out <- data.table::as.data.table(sf::st_drop_geometry(joined))
    
    # Guarantee one row per station. A station on a shared boundary can match
    # more than one polygon; keep the first match and report how many.
    n_before <- nrow(out)
    data.table::setkey(out, station_id)
    out <- out[, .SD[1L], by = station_id]
    n_dups <- n_before - nrow(out)
    
    if (n_dups > 0L && !quiet) {
      message(
        "[station_context] ", n_dups, " station(s) matched multiple ",
        "polygons on a boundary; kept the first match per station."
      )
    }
    
    data.table::setnames(out, geo_id_col, "station_geo_id")
    data.table::setnames(out, pop_col, "context_population")
    
    out[, context_method := "containing_geo"]
    out[, context_buffer_km := NA_real_]
    out[, n_geo_context := as.integer(!is.na(station_geo_id))]
    
    return(out[])
  }
  
  # 7. Method 2: population-weighted buffer context
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[station_context] Using buffer context: ", buffer_km, " km.")
  }
  
  # Build representative points for geographic units.
  if (representative_pt == "point_on_surface") {
    geo_pts <- suppressWarnings(sf::st_point_on_surface(geo_wgs))
  } else {
    geo_pts <- suppressWarnings(sf::st_centroid(geo_wgs))
  }
  
  # Use a local metric projection centered on the stations.
  cen <- sf::st_coordinates(sf::st_centroid(sf::st_union(stations_wgs)))
  
  proj_m <- aeqd_crs(
    lon0 = cen[1, "X"],
    lat0 = cen[1, "Y"]
  )
  
  stations_m <- sf::st_transform(stations_wgs, crs = proj_m)
  geo_pts_m <- sf::st_transform(geo_pts, crs = proj_m)
  
  # Spatial relation: geo representative points within station buffer.
  idx <- sf::st_is_within_distance(
    stations_m,
    geo_pts_m,
    dist = buffer_km * 1000
  )
  
  # Build one output row per station.
  out_list <- vector("list", length(idx))
  
  for (i in seq_along(idx)) {
    
    station_i <- stations_wgs[i, ]
    geo_idx_i <- idx[[i]]
    
    base <- data.table::data.table(
      station_id = station_i$station_id,
      station_name = as.character(station_i[[station_id_col]]),
      context_method = "buffer",
      context_buffer_km = buffer_km,
      n_geo_context = length(geo_idx_i)
    )
    
    if (length(geo_idx_i) == 0L) {
      base[, context_population := NA_real_]
      
      for (v in socio_vars) {
        base[, (v) := NA_real_]
      }
      
      out_list[[i]] <- base
      next
    }
    
    geo_i <- data.table::as.data.table(
      sf::st_drop_geometry(geo_pts[geo_idx_i, ])
    )
    
    w <- geo_i[[pop_col]]
    base[, context_population := sum(w, na.rm = TRUE)]
    
    for (v in socio_vars) {
      base[, (v) := .weighted_mean(geo_i[[v]], w)]
    }
    
    out_list[[i]] <- base
  }
  
  out <- data.table::rbindlist(out_list, fill = TRUE)
  
  return(out[])
}


# ------------------------------------------------------------------------------------
# Function: build_station_scatter_inputs
#
# @Arg arrow_dir        : string; path to partitioned Arrow/Parquet hourly data.
# @Arg stations_sf      : sf POINT object; monitoring stations.
# @Arg geo_sf           : sf POLYGON object; geographic units.
# @Arg census_col       : data.frame; collapsed census data by geographic unit.
# @Arg station_id_col   : string; station ID/name column in stations_sf.
# @Arg geo_id_col       : string; geographic unit ID column.
# @Arg pop_col          : string; population or expansion-weight column.
# @Arg socio_vars       : character vector; socioeconomic variables to attach.
# @Arg year_filter      : integer; year to process. Default 2023.
# @Arg context_method   : string; "containing_geo" or "buffer".
# @Arg context_buffer_km: numeric; buffer radius when context_method = "buffer".
# @Arg geo_id_repair    : string; "none", "bogota", or "suffix".
# @Arg bogota_max_suffix: integer; maximum suffix repair for Bogota IDs.
# @Arg bogota_broad_ids : logical; allow broad Bogota ID repairs?
# @Arg pollutants       : character vector; pollutant columns to summarize.
# @Arg who_it           : named list; WHO interim target thresholds.
# @Arg out_dir          : string; output directory.
# @Arg out_name         : string; output file prefix.
# @Arg overwrite        : logical; overwrite existing output. Default TRUE.
# @Arg quiet            : logical; suppress messages. Default FALSE.
# @Arg return_data      : logical; return data.table in memory. Default TRUE.
#
# @Output : Named list with output path and, optionally, station-level data.
#
# @Details:
#   Produces the station-level scatterplot inputs used in the exposure section:
#   one row per active station with annual mean concentration, hours above WHO
#   targets, and the socioeconomic context of the geographic unit where the
#   station is located (or a buffer around it). It does not produce maps,
#   distance-by-radius tables, or the share-of-non-missing-by-quintile table.
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# ------------------------------------------------------------------------------------
build_station_scatter_inputs <- function(
    arrow_dir,
    stations_sf,
    geo_sf,
    census_col,
    station_id_col,
    geo_id_col,
    pop_col,
    socio_vars,
    year_filter       = 2023L,
    context_method    = c("containing_geo", "buffer"),
    context_buffer_km = 3,
    geo_id_repair     = c("none", "bogota", "suffix"),
    bogota_max_suffix = 2L,
    bogota_broad_ids  = FALSE,
    pollutants        = c("pm10", "pm25"),
    who_it            = list(
      pm10 = c(it1 = 150, it2 = 100),
      pm25 = c(it1 = 75,  it2 = 50)
    ),
    out_dir,
    out_name,
    overwrite         = TRUE,
    quiet             = FALSE,
    return_data       = TRUE
) {
  
  # 0. Dependencies and argument matching
  # -----------------------------------------------------------------------
  pkgs <- c("arrow", "data.table")
  
  for (p in pkgs) {
  }
  
  context_method <- match.arg(context_method)
  geo_id_repair <- match.arg(geo_id_repair)
  
  # 1. Output path and early exit
  # -----------------------------------------------------------------------
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  out_path <- file.path(out_dir, paste0(out_name, "_station_socio.parquet"))
  
  if (!overwrite && file.exists(out_path)) {
    if (!quiet) {
      message("[station_socio] Output exists; skipping.")
    }
    
    out <- list(station_socio_path = out_path)
    
    if (isTRUE(return_data)) {
      out$station_socio <- data.table::as.data.table(
        arrow::read_parquet(out_path)
      )
    }
    
    return(invisible(out))
  }
  
  # 2. Compute pollution summaries
  # -----------------------------------------------------------------------
  pol_dt <- compute_station_pollution_summary(
    arrow_dir    = arrow_dir,
    year_filter  = year_filter,
    station_col  = "station",
    pollutants   = pollutants,
    who_it       = who_it,
    quiet        = quiet
  )
  
  # 3. Compute socioeconomic context
  # -----------------------------------------------------------------------
  socio_dt <- compute_station_socio_context(
    stations_sf       = stations_sf,
    geo_sf            = geo_sf,
    census_col        = census_col,
    station_id_col    = station_id_col,
    geo_id_col        = geo_id_col,
    pop_col           = pop_col,
    socio_vars        = socio_vars,
    context_method    = context_method,
    buffer_km         = context_buffer_km,
    geo_id_repair     = geo_id_repair,
    bogota_max_suffix = bogota_max_suffix,
    bogota_broad_ids  = bogota_broad_ids,
    quiet             = quiet
  )
  
  # 4. Merge station-level pollution and socioeconomic context
  # -----------------------------------------------------------------------
  out_dt <- merge(
    pol_dt,
    socio_dt,
    by = "station_id",
    all.x = TRUE
  )
  
  out_dt[, year := year_filter]
  
  # A station is socioeconomically matched only if a spatial context exists
  # and at least one requested socioeconomic variable is non-missing.
  socio_present <- intersect(socio_vars, names(out_dt))
  
  if (length(socio_present) == 0L) {
    stop("None of `socio_vars` are present after merging station context.")
  }
  
  out_dt[
    ,
    matched_socio_context := as.integer(
      !is.na(n_geo_context) &
        n_geo_context > 0L &
        rowSums(!is.na(.SD)) > 0L
    ),
    .SDcols = socio_present
  ]
  
  if (!quiet) {
    n_good <- out_dt[matched_socio_context == 1L, .N]
    
    message(
      "[station_socio] Valid socioeconomic matches: ",
      n_good, " of ", nrow(out_dt), "."
    )
  }
  
  # 5. Save output
  # -----------------------------------------------------------------------
  arrow::write_parquet(out_dt, out_path)
  
  out <- list(station_socio_path = out_path)
  
  if (isTRUE(return_data)) {
    out$station_socio <- out_dt
  }
  
  return(invisible(out))
}
