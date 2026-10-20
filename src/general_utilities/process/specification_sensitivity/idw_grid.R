# ==========================================================================================
# IDB: Air monitoring — IDW buffer and exponent grid of the specification analysis
# ==========================================================================================
#' @Goal: Station-weight descriptors and manuscript anchors for the IDW grid.
#' @Description: The grid reruns aggregate_idw_exposure() at other buffers and exponents.
#   These helpers describe the weights that each specification gives to stations and
#   compare grid exposures with the manuscript files where both exist.
#' @Summary:
#   1. idw_station_weights
#   2. idw_grid_anchor
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: idw_station_weights
#
#' @param distances      data.table; geo_id, station_id and distance_km for every pair.
#' @param active         character; stations that reported the pollutant in the year.
#' @param buffer_km      numeric; eligibility distance.
#' @param distance_power numeric; IDW exponent.
#
#' @return  data.table per geo_id with at least one eligible active station:
#   eligible_stations, nearest_eligible_km, static_neff and max_weight_share.
#
#' @details
#   Weights are 1 / d^p over stations with 0 < d <= buffer that reported in the year, as
#   in aggregate_idw_exposure(). Hourly IDW renormalises over the stations reporting each
#   hour, so these static weights describe the structure, not each hour.
#   static_neff = (sum w)^2 / sum w^2 is 1 with one station and the station count with
#   equal weights. The exponent changes weights only where two or more stations are
#   eligible; a unit with one station has the same exposure under any exponent.
# ------------------------------------------------------------------------------------------
idw_station_weights <- function(distances, active, buffer_km, distance_power) {
  d <- distances[is.finite(distance_km) & distance_km > 0 & distance_km <= buffer_km &
                   station_id %chin% active]
  d[, w := 1 / distance_km^distance_power]
  d[, .(eligible_stations = .N, nearest_eligible_km = min(distance_km),
        static_neff = sum(w)^2 / sum(w^2), max_weight_share = max(w) / sum(w)),
    by = geo_id]
}

# ------------------------------------------------------------------------------------------
# Function: idw_grid_anchor
#
#' @param grid       data.table; grid exposure for one city and specification (year 2023).
#' @param manuscript data.table; the manuscript exposure file of the same specification,
#   with geo_id already in the grid's format.
#' @param outcomes   character; outcome columns compared.
#
#' @return  data.table per outcome: units with values in each file, whether the missing
#   masks agree, the largest absolute difference, and passed.
#
#' @details
#   Both files come from aggregate_idw_exposure() with the same panel, buffer and
#   exponent; only the distance file differs (the frozen fine matrix versus the
#   manuscript matrix). Agreement uses abs(grid - manuscript) <= 1e-10 + 1e-8 *
#   abs(manuscript), the structural-parity tolerance of doc/ai/methods-tests.md, and
#   identical missing masks over the union of units.
# ------------------------------------------------------------------------------------------
idw_grid_anchor <- function(grid, manuscript, outcomes) {
  x <- merge(grid[, c("geo_id", outcomes), with = FALSE],
             manuscript[, c("geo_id", outcomes), with = FALSE],
             by = "geo_id", all = TRUE, suffixes = c(".grid", ".manuscript"))
  data.table::rbindlist(lapply(outcomes, function(oc) {
    a <- as.numeric(x[[paste0(oc, ".grid")]])
    b <- as.numeric(x[[paste0(oc, ".manuscript")]])
    same_na <- identical(is.na(a), is.na(b))
    error <- if (any(!is.na(a) & !is.na(b))) max(abs(a - b), na.rm = TRUE) else NA_real_
    data.table::data.table(outcome = oc, units_grid = sum(!is.na(a)),
      units_manuscript = sum(!is.na(b)), same_na = same_na, max_error = error,
      passed = same_na && all(abs(a - b) <= 1e-10 + 1e-8 * abs(b), na.rm = TRUE))
  }))
}
