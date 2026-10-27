# ==========================================================================================
# IDB: Air monitoring — station sets of the specification-sensitivity analysis
# ==========================================================================================
#' @Goal: Describe which stations report each pollutant and restrict the IDW stations.
#' @Description: The grid's default interpolates every station with a valid reading of a
#   pollutant in the year. A station set keeps fewer stations, by a declared rule on
#   their reporting (resolution_station_sets in config/analysis_settings.R). These
#   functions count the reporting, apply the rule and blank the readings outside the set,
#   so aggregate_idw_exposure() runs unchanged on the restricted panel.
#' @Summary:
#   1. station_reporting
#   2. station_set_members
#   3. restrict_station_panel
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: station_reporting
#
#' @param panel      data.table; hourly readings of one year with station, datetime and
#                    one column per pollutant (e.g. the frozen year=2023 partition).
#' @param pollutants character; pollutant columns to describe.
#
#' @return  data.table with one row per station and pollutant: valid_hours, months
#   (calendar months with at least one valid hour), first_hour and last_hour (NA when
#   the station never reports the pollutant).
#
#' @details
#   A valid hour is a finite reading. Months use the stored clock (formatted in UTC), the
#   clock DuckDB reads in aggregate_idw_exposure(). Every station in the panel keeps a row
#   for every pollutant, with zero hours if it never reports it, so a reader sees the
#   stations a rule removes as well as those it keeps.
# ------------------------------------------------------------------------------------------
station_reporting <- function(panel, pollutants = c("pm10", "pm25")) {
  x <- data.table::as.data.table(panel)[, c("station", "datetime", pollutants),
                                         with = FALSE]
  x[, month := as.integer(format(datetime, "%m", tz = "UTC"))]
  long <- data.table::melt(x, id.vars = c("station", "datetime", "month"),
                           measure.vars = pollutants, variable.name = "pollutant",
                           value.name = "value", variable.factor = FALSE)

  # Hours and months for every station; first and last hour where it reports
  counts <- long[, .(valid_hours = sum(is.finite(value)),
                     months = data.table::uniqueN(month[is.finite(value)])),
                 by = .(station, pollutant)]
  span <- long[is.finite(value), .(first_hour = min(datetime), last_hour = max(datetime)),
                by = .(station, pollutant)]
  out <- merge(counts, span, by = c("station", "pollutant"), all.x = TRUE)
  data.table::setorder(out, station, pollutant)
  out[]
}

# ------------------------------------------------------------------------------------------
# Function: station_set_members
#
#' @param reporting  data.table from station_reporting().
#' @param require    character; pollutants a station must report to enter any network.
#' @param min_months integer; minimum months with a valid hour of each pollutant checked.
#
#' @return  data.table with station, pollutant and in_set, one row per row of reporting.
#
#' @details
#   A station enters the network of pollutant p when every pollutant in union(p, require)
#   has valid hours in at least min_months months. With require = c("pm10", "pm25") and
#   min_months = 1, a station is kept for both pollutants only if it reports both in the
#   year. With require = character() and min_months = 3, each pollutant keeps the stations
#   that report it in three or more months, whatever they report of the other. The rule
#   reads the reporting counts only, never the concentrations.
# ------------------------------------------------------------------------------------------
station_set_members <- function(reporting, require = character(), min_months = 1L) {
  wide <- data.table::dcast(reporting, station ~ pollutant, value.var = "months",
                            fill = 0L)
  stopifnot(all(require %in% names(wide)))
  members <- data.table::rbindlist(lapply(unique(reporting$pollutant), function(p) {
    checked <- union(p, require)
    data.table::data.table(station = wide$station, pollutant = p,
      in_set = Reduce(`&`, lapply(checked, function(q) wide[[q]] >= min_months)))
  }))
  data.table::setorder(members, station, pollutant)
  members[]
}

# ------------------------------------------------------------------------------------------
# Function: restrict_station_panel
#
#' @param panel   data.table; hourly readings with station and the pollutant columns.
#' @param members data.table from station_set_members().
#
#' @return  a copy of panel in which the readings of a pollutant at stations outside its
#   network are NA. Rows, stations and every other column are unchanged.
#
#' @details
#   aggregate_idw_exposure() drops a missing reading from the numerator and the
#   denominator of the hourly weighted mean, so a blanked reading is the same as removing
#   that station for that pollutant. Blanking keeps the frozen distance matrix for every
#   set and lets one network differ by pollutant.
# ------------------------------------------------------------------------------------------
restrict_station_panel <- function(panel, members) {
  x <- data.table::copy(data.table::as.data.table(panel))
  for (p in unique(members$pollutant)) {
    outside <- members[pollutant == p & !in_set, station]
    data.table::set(x, i = which(x$station %chin% outside), j = p, value = NA_real_)
  }
  x[]
}
