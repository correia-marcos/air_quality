# ============================================================================================
# IDB: Air monitoring — shared geospatial helpers
# ============================================================================================
# @Goal: One definition of the two metric projections the pipeline picks at runtime.
#
# @Description: Sourced by config_utils_process_data.R, config_utils_plot_tables.R and
#   config_utils_validation_old_version.R. Loads no packages and has no side effects, so it
#   is safe to source more than once; scripts never source it directly.
#
# @Summary:
#   I. aeqd_crs  — metre grid centred on a study area (distances, buffers)
#   II. utm_epsg — UTM zone holding a layer (areas, planar geometry repair)
#
# @Date: July 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: aeqd_crs
#
# @Arg       : lon0 — numeric; WGS84 longitude of the projection origin.
# @Arg       : lat0 — numeric; WGS84 latitude of the projection origin.
#
# @Output    : character; proj4 string to hand to `crs =`.
#
# @Purpose   : Puts a layer on a metre grid centred on the study area, so a 20 km ring is
#              20 000 ground metres whatever CRS the provider shipped. A provider's own
#              projected CRS carries its own scale factor and does not give that: EPSG:6372
#              is 0.99712 at Mexico City's latitude, which stretched a "20 000 m" ring to
#              20 058 m on the ground.
#
# @Details   : Exact along rays from the origin, so the origin only has to be near the area.
#              Metro extent only — error grows with distance off-axis.
#
# @Written_on: July 2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
aeqd_crs <- function(lon0, lat0) {
  sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs", lat0, lon0)
}


# --------------------------------------------------------------------------------------------
# Function: utm_epsg
#
# @Arg       : x — sf or sfc object, in any CRS.
#
# @Output    : integer; EPSG code of the UTM zone holding the layer's bounding-box midpoint.
#
# @Purpose   : Gives one metric CRS for a metro-scale layer, used for area, distance and any
#              geometry repair that must run planar (GEOS) rather than spherical (s2).
#
# @Details   : 326xx north of the equator, 327xx south. The zone formula is defined on
#              longitude, so the bounding box is taken in lon/lat first.
#
# @Written_on: July 2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
utm_epsg <- function(x) {
  bb   <- sf::st_bbox(sf::st_transform(x, 4326))
  lon  <- as.numeric((bb[["xmin"]] + bb[["xmax"]]) / 2)
  lat  <- as.numeric((bb[["ymin"]] + bb[["ymax"]]) / 2)
  zone <- floor((lon + 180) / 6) + 1

  if (lat >= 0) 32600 + zone else 32700 + zone
}


# --------------------------------------------------------------------------------------------
# Function: aeqd_for
#
# @Arg       : x — sf or sfc object, in any CRS.
#
# @Output    : character; proj4 string of an AEQD grid centred on the layer.
#
# @Purpose   : Picks the AEQD origin from the bounding-box midpoint instead of the centroid.
#              A centroid needs st_union() first, and on a lon/lat layer that runs through
#              s2, whose rebuild snaps vertices to a ~1.1 cm grid — enough to collapse the
#              sub-centimetre edges some providers ship into degenerate (duplicate) vertices.
#              A bounding box touches no vertices, so the layer never reaches s2.
#
# @Written_on: July 2026
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
aeqd_for <- function(x) {
  bb <- sf::st_bbox(sf::st_transform(x, 4326))

  aeqd_crs(lon0 = (bb[["xmin"]] + bb[["xmax"]]) / 2,
           lat0 = (bb[["ymin"]] + bb[["ymax"]]) / 2)
}
