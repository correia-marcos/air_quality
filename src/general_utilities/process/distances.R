# ============================================================================================
# IDB: Air monitoring — geo-to-station distance matrices
# ============================================================================================
#' @Goal: Functions for geo-to-station distance matrices.
#
#' @Description: Builds the station-to-station and geographic-unit-to-station distance matrices
#   every later stage joins on, on an AEQD metre grid centered on each metro area.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
#' @Summary:
#   1. compute_distance_matrices
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: compute_distance_matrices
#
#' @param stations_sf         sf POINT object; monitoring stations.
#' @param station_id_col      string; column in stations_sf with station IDs.
#' @param geo_sf              sf POLYGON object or NULL; geographic units.
#' @param geo_id_col          string or NULL; unique ID column in geo_sf.
#' @param out_dir             string; output directory.
#' @param out_name            string; prefix, e.g. "bogota_2018".
#' @param distance_metric     string; "aeqd", "haversine", or "geosphere".
#' @param representative_point string; "point_on_surface", "math_centroid",
#                             or "math_centroid_legacy".
#' @param overwrite           logical; skip if output exists. Default TRUE.
#' @param quiet               logical; suppress messages. Default FALSE.
#
#' @return  Named list of data.tables for station and geo distances.
#' @details
#   Calculates station-to-station and geo-to-station distance matrices. If
#   representative_point = "point_on_surface", it uses an internal point returned by
#   st_point_on_surface() for every geo unit. If representative_point = "math_centroid",
#   it uses st_centroid() with an internal-point fallback for polygons whose centroid is
#   outside. If representative_point = "math_centroid_legacy", it uses plain
#   st_centroid() with no fallback. The function also linearizes curved geometries
#   such as MULTISURFACE/CURVEPOLYGON before validity repair and distance calculation.
#   The "geosphere" metric is included for legacy replication because the old
#   station-distance scripts used geosphere::distm(..., fun = distHaversine).
#
#   Distance metrics:
#     - "geosphere" : great-circle Haversine via geosphere::distm. Legacy-matching
#                     metric for the station-to-station matrix. Note distm defaults
#                     to the EQUATORIAL radius (6378137 m); near the equator that
#                     overstates north-south pairs by ~0.5% (~150 m over 30 km).
#     - "haversine" : spherical (S2) great-circle via sf::st_distance on WGS84.
#                     Legacy-matching metric for the geo-to-station matrix. This
#                     is the only metric sensitive to sf_use_s2(), which the stage
#                     config pins to TRUE; with s2 off, sf would return ellipsoidal
#                     geodesics instead and stop matching legacy.
#     - "aeqd"      : planar distance in an Azimuthal Equidistant projection centered
#                     on the midpoint of the combined station + geo bounding box.
#                     This is the intended/updated metric. AEQD is exact only along
#                     rays from its origin; for two off-centre points the scale factor
#                     is 1 + (rho/R)^2/6, about 1e-5 at rho = 50 km — roughly 3 cm on
#                     a 3 km distance. Safe at metro extent for that reason, and only
#                     for that reason: do not reuse it at national extent.
#
#' @Written_on : 01/02/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_distance_matrices <- function(
    stations_sf,
    station_id_col,
    geo_sf               = NULL,
    geo_id_col           = NULL,
    out_dir,
    out_name,
    distance_metric      = c("aeqd", "haversine", "geosphere"),
    representative_point = c("point_on_surface", "math_centroid",
                             "math_centroid_legacy"),
    overwrite            = TRUE,
    quiet                = FALSE
) {
  
  # 0. Match requested methods
  # -----------------------------------------------------------------------
  dist_metric <- match.arg(distance_metric)
  representative_point <- match.arg(representative_point)
  
  # 1. Check required packages
  # -----------------------------------------------------------------------
  pkgs <- c("sf", "data.table", "arrow", "stringi")

  if (dist_metric == "geosphere") {
    pkgs <- c(pkgs, "geosphere")
  }

  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }

  # 2. Inner helpers
  # -----------------------------------------------------------------------
  # Normalize station IDs: uppercase, strip accents and quotes.
  .normalize <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  # Linearize curved geometries and prepare polygon layer.
  .prepare_geo_geometry <- function(x) {
    
    # Drop Z/M dimensions if present.
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    
    # Detect curved or surface geometry types.
    geom_types <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
    
    has_curves <- any(
      grepl("CURVE|SURFACE|CIRCULAR|COMPOUND", geom_types,
            ignore.case = TRUE)
    )
    
    # GDAL linearization avoids st_make_valid() failures on CURVEPOLYGON.
    if (has_curves) {
      tmp_in  <- tempfile("geo_curved_", fileext = ".gpkg")
      tmp_out <- tempfile("geo_linear_", fileext = ".gpkg")
      
      on.exit(unlink(c(tmp_in, tmp_out), recursive = TRUE, force = TRUE),
              add = TRUE)
      
      sf::st_write(
        x,
        tmp_in,
        layer = "geo",
        delete_dsn = TRUE,
        quiet = TRUE
      )
      
      sf::gdal_utils(
        util = "vectortranslate",
        source = tmp_in,
        destination = tmp_out,
        options = c(
          "-f", "GPKG",
          "-nlt", "CONVERT_TO_LINEAR",
          "-nln", "geo"
        )
      )
      
      x <- sf::st_read(tmp_out, layer = "geo", quiet = TRUE)
    }
    
    # Repair validity after curved geometries are converted. Repair is planar, so it
    # runs on a UTM grid: on lon/lat sf routes it to s2, whose rebuild snaps vertices
    # to a ~1.1 cm cell and turns the sub-centimetre edges some providers ship into
    # degenerate (duplicate) vertices, which then abort the next union.
    x <- sf::st_transform(x, crs = utm_epsg(x))
    x <- sf::st_make_valid(x)

    # Extract polygonal components if validation creates collections.
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))

    # Promote to MULTIPOLYGON for stable downstream processing.
    x <- suppressWarnings(sf::st_cast(x, "MULTIPOLYGON", warn = FALSE))

    # Work in WGS84 after regularization.
    x <- sf::st_transform(x, crs = 4326)

    return(x)
  }
  
  # Create one representative point per polygon.
  .representative_points <- function(poly, method) {
    
    # Use guaranteed internal points for all polygons.
    if (method == "point_on_surface") {
      return(suppressWarnings(sf::st_point_on_surface(poly)))
    }
    
    # Legacy behavior: plain st_centroid() over all parts, no internal fallback.
    if (method == "math_centroid_legacy") {
      return(suppressWarnings(
        sf::st_centroid(poly, of_largest_polygon = FALSE)
      ))
    }
    
    # Calculate mathematical centroids first.
    cents <- suppressWarnings(
      sf::st_centroid(poly, of_largest_polygon = TRUE)
    )
    
    # Check whether centroid i intersects polygon i.
    inside_mat <- suppressWarnings(
      sf::st_intersects(cents, poly, sparse = FALSE)
    )
    
    # Extract diagonal relation: point i versus polygon i.
    is_inside <- as.logical(
      inside_mat[cbind(seq_len(nrow(poly)), seq_len(nrow(poly)))]
    )
    
    # Replace external centroids with guaranteed internal points.
    if (any(!is_inside, na.rm = TRUE)) {
      bad_idx <- which(!is_inside)
      
      cents[bad_idx, ] <- suppressWarnings(
        sf::st_point_on_surface(poly[bad_idx, ])
      )
    }
    
    return(cents)
  }
  
  # Calculate geosphere Haversine distances from two sf POINT objects.
  .geosphere_distance_km <- function(from_sf, to_sf) {
    
    # geosphere expects longitude-latitude coordinates in WGS84.
    from_wgs <- sf::st_transform(from_sf, crs = 4326)
    to_wgs   <- sf::st_transform(to_sf,   crs = 4326)
    
    # Extract coordinates as lon-lat matrices.
    from_xy <- sf::st_coordinates(from_wgs)[, c("X", "Y"), drop = FALSE]
    to_xy   <- sf::st_coordinates(to_wgs)[,   c("X", "Y"), drop = FALSE]
    
    # geosphere::distm returns meters.
    dist_m <- geosphere::distm(
      x = from_xy,
      y = to_xy,
      fun = geosphere::distHaversine
    )
    
    return(as.numeric(dist_m) / 1000)
  }
  
  # 3. Validate inputs
  # -----------------------------------------------------------------------
  if (!inherits(stations_sf, "sf")) {
    stop("`stations_sf` must be an sf object.")
  }
  
  if (!station_id_col %in% names(stations_sf)) {
    stop("Column '", station_id_col, "' not found.")
  }
  
  if (!is.null(geo_sf)) {
    if (!inherits(geo_sf, "sf")) {
      stop("`geo_sf` must be an sf object.")
    }
    
    if (is.null(geo_id_col)) {
      stop("`geo_id_col` is required.")
    }
    
    if (!geo_id_col %in% names(geo_sf)) {
      stop("Column '", geo_id_col, "' not found.")
    }
  }
  
  # 4. Output paths and early exit
  # -----------------------------------------------------------------------
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    
    if (!quiet) {
      message("Created output directory: ", out_dir)
    }
  }
  
  path_sta <- file.path(out_dir, paste0(out_name, "_station_distances.parquet"))
  path_geo <- file.path(out_dir, paste0(out_name, "_geo_station_distances.parquet"))
  
  geo_ready <- is.null(geo_sf) || file.exists(path_geo)
  
  if (!overwrite && file.exists(path_sta) && geo_ready) {
    if (!quiet) {
      message("Files exist and overwrite = FALSE.")
    }
    
    return(invisible(list(
      station_matrix = data.table::as.data.table(
        arrow::read_parquet(path_sta)
      ),
      geo_station_matrix = if (!is.null(geo_sf)) {
        data.table::as.data.table(arrow::read_parquet(path_geo))
      } else {
        NULL
      }
    )))
  }
  
  if (!quiet) {
    message("[", out_name, "] Metric: ", dist_metric)
    message("[", out_name, "] Representative point: ", representative_point)
  }
  
  # 5. Prepare stations
  # -----------------------------------------------------------------------
  # Enforce WGS84 and keep only the station ID column.
  stations_wgs <- sf::st_transform(stations_sf, crs = 4326)
  stations_wgs <- stations_wgs[, station_id_col]
  
  # Handle projection based on selected metric.
  if (dist_metric == "aeqd") {
    
    # Center AEQD on the full extent being measured: stations plus geo units
    # when geo_sf is supplied.
    sta_bbox <- sf::st_bbox(stations_wgs)
    
    if (!is.null(geo_sf)) {
      geo_bbox <- sf::st_bbox(sf::st_transform(geo_sf, crs = 4326))
      
      # Outer envelope spanning both layers.
      xmin <- min(sta_bbox["xmin"], geo_bbox["xmin"])
      xmax <- max(sta_bbox["xmax"], geo_bbox["xmax"])
      ymin <- min(sta_bbox["ymin"], geo_bbox["ymin"])
      ymax <- max(sta_bbox["ymax"], geo_bbox["ymax"])
    } else {
      xmin <- sta_bbox["xmin"]; xmax <- sta_bbox["xmax"]
      ymin <- sta_bbox["ymin"]; ymax <- sta_bbox["ymax"]
    }
    
    # Bounding-box midpoint defines the AEQD origin.
    lon0 <- as.numeric((xmin + xmax) / 2)
    lat0 <- as.numeric((ymin + ymax) / 2)
    
    proj_aeqd <- aeqd_crs(lon0 = lon0, lat0 = lat0)
    stations_eval <- sf::st_transform(stations_wgs, crs = proj_aeqd)
    
  } else {
    stations_eval <- stations_wgs
  }
  
  # Extract and normalize station IDs.
  station_ids <- .normalize(as.character(stations_wgs[[station_id_col]]))
  n_sta <- length(station_ids)
  
  # 6. Station-to-station distances
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[", out_name, "] Station distances.")
  }
  
  if (dist_metric == "geosphere") {
    dist_sta_km <- .geosphere_distance_km(stations_wgs, stations_wgs)
    
  } else {
    # "aeqd" (meters) or "haversine" (S2 meters).
    dist_sta_raw <- as.numeric(
      sf::st_distance(stations_eval, stations_eval)
    )
    
    dist_sta_km <- dist_sta_raw / 1000
  }
  
  # Generate a data table with stations and distances. The square matrix is
  # unrolled column-major, so station_to (column) varies slowest (each) and
  # station_from (row) varies fastest (times).
  station_dt <- data.table::data.table(
    station_from = rep(station_ids, times = n_sta),
    station_to   = rep(station_ids, each  = n_sta),
    distance_km  = dist_sta_km
  )
  
  if (!quiet) {
    message("[", out_name, "] Writing: ", path_sta)
  }
  
  arrow::write_parquet(station_dt, path_sta)
  
  geo_station_dt <- NULL
  
  # 7. Geo-to-station distances
  # -----------------------------------------------------------------------
  if (!is.null(geo_sf)) {
    
    if (!quiet) {
      message("[", out_name, "] Geo distances.")
    }
    
    # Linearize curved geometries, fix validity, and transform to WGS84.
    geo_wgs <- .prepare_geo_geometry(geo_sf)
    
    # Extract one representative point per geographic unit.
    geo_points <- .representative_points(
      poly = geo_wgs,
      method = representative_point
    )
    
    # Apply AEQD projection if requested.
    if (dist_metric == "aeqd") {
      geo_eval <- sf::st_transform(geo_points, crs = proj_aeqd)
    } else {
      geo_eval <- geo_points
    }
    
    # Extract geographic unit IDs after geometry preparation.
    geo_ids <- as.character(geo_points[[geo_id_col]])

    # Validity repair can split a unit into extra rows; stop before double-counting.
    if (anyDuplicated(geo_ids) > 0) {
      dup_ids <- unique(geo_ids[duplicated(geo_ids)])
      stop("Duplicated geo ids after geometry preparation: ",
           paste(dup_ids, collapse = ", "))
    }

    n_geo   <- length(geo_ids)
    
    # Calculate distances from representative points to stations. Both paths
    # produce an n_geo x n_sta matrix unrolled column-major (geo rows fastest,
    # station columns slowest).
    if (dist_metric == "geosphere") {
      dist_geo_km <- .geosphere_distance_km(geo_points, stations_wgs)
      
    } else {
      # "aeqd" (meters) or "haversine" (S2 meters).
      dist_geo_raw <- as.numeric(
        sf::st_distance(geo_eval, stations_eval)
      )
      
      dist_geo_km <- dist_geo_raw / 1000
    }
    
    # Generate a data table with geo ids, stations and distances. To match the
    # column-major unroll, geo_id (row) varies fastest -> times = n_sta, and
    # station_id (column) varies slowest -> each = n_geo.
    geo_station_dt <- data.table::data.table(
      geo_id      = rep(geo_ids,     times = n_sta),
      station_id  = rep(station_ids, each  = n_geo),
      distance_km = dist_geo_km
    )
    
    if (!quiet) {
      message("[", out_name, "] Writing: ", path_geo)
    }
    
    arrow::write_parquet(geo_station_dt, path_geo)
  }
  
  # 8. Return both matrices invisibly
  # -----------------------------------------------------------------------
  invisible(list(
    station_matrix     = station_dt,
    geo_station_matrix = geo_station_dt
  ))
}
