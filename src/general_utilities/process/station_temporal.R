# Hourly metropolitan PM2.5 summaries from observed station partitions.

# ----------------------------------------------------------------------------------------
# Function: summarize_city_hourly_pm25
#' @param station_data Arrow Dataset or data frame with station, datetime, pm25 and year.
#' @param year Integer analysis year, selected from the existing partition column.
#' @return Bounded data frame: datetime, Date, Hour, pm25_stations and n_reporting.
#' @details Each finite station reading receives equal weight at its timestamp. Duplicate
# station/timestamp records are errors. The full UTC-labelled calendar preserves the
# current stored timestamp convention; it does not convert source wall-clock times.
# Missing city-hours have NA concentration and zero reporting stations. No files are
# written, and no imputed or satellite readings are introduced.
# ----------------------------------------------------------------------------------------
summarize_city_hourly_pm25 <- function(station_data, year) {
  if (length(year) != 1L || is.na(year) || year != as.integer(year)) {
    stop("Select one integer analysis year.")
  }
  required <- c("station", "datetime", "pm25", "year")
  if (!all(required %in% names(station_data))) {
    stop("Station data must contain: ", paste(required, collapse = ", "))
  }
  start <- as.POSIXct(sprintf("%d-01-01", year), tz = "UTC")
  finish <- as.POSIXct(sprintf("%d-01-01", year + 1L), tz = "UTC")
  selected <- dplyr::filter(station_data, .data$year == .env$year)
  selected <- dplyr::select(selected, dplyr::all_of(required[1:3]))

  invalid <- dplyr::filter(selected, is.na(.data$station) | is.na(.data$datetime) |
    .data$datetime < .env$start | .data$datetime >= .env$finish)
  if (nrow(dplyr::collect(utils::head(invalid, 1L)))) {
    stop("Station identifiers and timestamps must belong to the selected year.")
  }
  duplicate <- dplyr::group_by(selected, .data$station, .data$datetime)
  duplicate <- dplyr::summarise(duplicate, records = dplyr::n(), .groups = "drop")
  duplicate <- dplyr::filter(duplicate, .data$records > 1L)
  if (nrow(dplyr::collect(utils::head(duplicate, 1L)))) {
    stop("Duplicate station/timestamp records would change station weights.")
  }

  observed <- dplyr::filter(selected, is.finite(.data$pm25))
  hourly <- dplyr::group_by(observed, .data$datetime)
  hourly <- dplyr::summarise(hourly, pm25_stations = mean(.data$pm25),
    n_reporting = dplyr::n(), .groups = "drop")
  hourly <- dplyr::collect(hourly)
  if (any(as.numeric(hourly$datetime) %% 3600 != 0)) {
    stop("Station timestamps must be aligned to whole hours.")
  }
  calendar <- seq(start, finish - 3600, by = "hour")
  index <- match(calendar, hourly$datetime)
  reporting <- as.integer(hourly$n_reporting[index])
  reporting[is.na(reporting)] <- 0L
  data.frame(datetime = calendar, Date = as.Date(calendar, tz = "UTC"),
    Hour = as.integer(format(calendar, "%H", tz = "UTC")),
    pm25_stations = hourly$pm25_stations[index], n_reporting = reporting)
}

# ----------------------------------------------------------------------------------------
# Function: write_station_hourly
#' @param hourly Hourly metropolitan summary returned by summarize_city_hourly_pm25().
#' @param file Explicit destination filename.
#' @return Destination path. Writes a small Parquet checkpoint; does not recompute data.
# ----------------------------------------------------------------------------------------
write_station_hourly <- function(hourly, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(hourly, file)
  unname(file)
}

# ----------------------------------------------------------------------------------------
# Function: write_station_episodes
#' @param episodes Named city episode table with start date/hour and consecutive duration.
#' @param file Explicit destination CSV path, separate from the PDF rendering.
#' @return Destination path, after saving the underlying appendix episode data.
# ----------------------------------------------------------------------------------------
write_station_episodes <- function(episodes, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(episodes, file, row.names = FALSE)
  unname(file)
}
