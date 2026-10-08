# Screening and audit tables for standardized hourly particulate concentrations.
#' @param arrow_dir Directory of the original year-partitioned hourly input.
#' @return Year and SHA-256 input identities, independent of the absolute directory.
pollution_input_identities <- function(arrow_dir) {
  files <- sort(list.files(arrow_dir, pattern = "[.]parquet$", recursive = TRUE,
                           full.names = TRUE))
  if (!length(files)) stop("No partitioned Parquet inputs for quality audit.")
  relative <- substring(files, nchar(sub("/$", "", arrow_dir)) + 2L)
  years <- as.integer(sub("^year=([0-9]{4})/.*", "\\1", relative))
  if (anyNA(years)) stop("Quality audit requires year-partitioned Parquet inputs.")
  hashes <- vapply(files, digest::digest, character(1), file = TRUE, algo = "sha256")
  data.table::data.table(year = years, path = relative, sha256 = hashes)[,
    .(input_id = digest::digest(paste(path, sha256, collapse = "\n"),
                                algo = "sha256", serialize = FALSE)), by = year]
}

#' @param upper_bounds NULL or uniquely named positive bounds; Inf disables one bound.
#' @param pollutants Requested pollutant columns.
#' @return Invisibly TRUE; invalid bounds fail before output replacement.
validate_pollution_quality_options <- function(upper_bounds, pollutants) {
  if (!is.null(upper_bounds) &&
      (!is.numeric(upper_bounds) || is.null(names(upper_bounds)) ||
       anyDuplicated(names(upper_bounds)) || any(!nzchar(names(upper_bounds))) ||
       anyNA(names(upper_bounds)) || anyNA(upper_bounds) || any(upper_bounds <= 0) ||
       any(!names(upper_bounds) %in% union(pollutants, c("pm25", "pm10"))))) {
    stop("upper_bounds must be NULL or uniquely named positive numeric bounds.")
  }
  invisible(TRUE)
}

#' @param data Standardized hourly table; copied, never changed in place.
#' @param pollutants Pollutant columns to screen.
#' @param upper_bounds Named bounds in ug/m3; equality and zero pass, NULL disables bounds.
#' @return Original values, source validation, integer screening reasons and masked values.
#' @details Codes: 0 within bounds, 1 above bound, 2 negative, 3 missing/nonfinite.
#   Source validation describes acquisition evidence, not project acceptance. Bounds are
#   uniform project choices. No per-reading overrides or concentration capping are used.
screen_pollution_quality <- function(data, pollutants = c("pm10", "pm25"),
    upper_bounds = c(pm25 = 2000, pm10 = 6000)) {
  validate_pollution_quality_options(upper_bounds, pollutants)
  out <- data.table::as.data.table(data.table::copy(data))
  for (pol in intersect(pollutants, names(out))) {
    value <- out[[pol]]
    bound <- if (pol %in% names(upper_bounds)) upper_bounds[[pol]] else Inf
    reason <- rep(0L, nrow(out))
    reason[is.finite(value) & value > bound] <- 1L
    reason[is.finite(value) & value < 0] <- 2L
    reason[!is.finite(value)] <- 3L
    source <- paste0(pol, "_source_validation")
    old_source <- paste0(pol, "_source_status")
    if (!source %in% names(out)) {
      out[, (source) := if (old_source %in% names(out)) get(old_source) else "unknown"]
    }
    out[is.na(get(source)), (source) := "unknown"]
    if (old_source %in% names(out)) out[, (old_source) := NULL]
    out[, (paste0(pol, c("_original", "_screen_reason"))) := list(value, reason)]
    out[, (pol) := data.table::fifelse(reason == 0L, as.numeric(value), NA_real_)]
  }
  out
}

#' @param data One cleaned year, before removing repeated diagnostics and source IDs.
#' @param audit_dir The cleaned dataset's _audit directory, ignored by Arrow discovery.
#' @param pollutants Requested pollutant columns.
#' @return Bounded station-month diagnostics; source contributions are written by year.
write_pollution_audit_partition <- function(data, audit_dir, pollutants) {
  data <- data.table::copy(data)
  data[, datetime := as.POSIXct(as.numeric(datetime), origin = "1970-01-01", tz = "UTC")]
  data[, month := as.integer(format(datetime, "%m", tz = "UTC"))]
  diagnostics <- data.table::rbindlist(lapply(intersect(pollutants, names(data)),
    function(pol) {
      labels <- c("n_missing_temporal_sd", "n_zero_temporal_sd",
                  "n_missing_spatial_sd", "n_zero_spatial_sd")
      fields <- c("station", "year", "month", paste0(pol, "_", labels))
      counts <- unique(data[, ..fields])
      data.table::setnames(counts, paste0(pol, "_", labels), labels)
      counts[, pollutant := pol]
      counts
    }))
  sources <- data.table::rbindlist(lapply(intersect(pollutants, names(data)),
    function(pol) {
      source <- paste0(pol, "_source_ids")
      if (!source %in% names(data)) return(NULL)
      fields <- c("station", "station_original", "datetime", "year", "input_id",
                  paste0(pol, "_source_validation"), source)
      links <- data[, ..fields]
      data.table::setnames(links, paste0(pol, c("_source_validation", "_source_ids")),
                          c("source_validation", "source_ids"))
      links[, pollutant := pol]
      links
    }))
  if (ncol(sources)) {
    path <- file.path(audit_dir, "source_contributions", paste0("year=", data$year[1]))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(sources, file.path(path, "data.parquet"), compression = "snappy")
  }
  diagnostics
}

#' @param arrow_dir Cleaned observed partitions with the compact audit schema.
#' @param pollutants Pollutants to summarize.
#' @return Bounded station/year/pollutant counts by screening and statistical reason.
summarize_pollution_quality <- function(arrow_dir, pollutants = c("pm10", "pm25")) {
  dataset <- arrow::open_dataset(arrow_dir)
  data.table::rbindlist(lapply(pollutants, function(pol) {
    if (!paste0(pol, "_screen_reason") %in% dataset$schema$names) return(NULL)
    labels <- c("source_validation", "screen_reason", "outlier_reason")
    fields <- c("station", "year", paste0(pol, "_", labels))
    counts <- dataset |>
      dplyr::select(dplyr::all_of(fields)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(fields))) |>
      dplyr::summarise(hours = dplyr::n(), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
    data.table::setnames(counts, paste0(pol, "_", labels), labels)
    counts[, pollutant := pol]
    data.table::setcolorder(counts, c("station", "year", "pollutant", labels, "hours"))
    data.table::setorderv(counts, c("station", "year", "pollutant", labels))
    counts
  }))
}

#' @param summary Named quality-count or screened-record table.
#' @param path CSV destination outside the Arrow dataset.
#' @return Saved filename.
write_pollution_quality_summary <- function(summary, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(summary, path)
  path
}

#' @param arrow_dir Cleaned partitions, with original values and partition identities.
#' @param pollutants Pollutants whose bound/negative exclusions require a linked record.
#' @return Above-bound and negative records, with actual contributing source IDs if known.
collect_pollution_screening_records <- function(arrow_dir, pollutants = c("pm10", "pm25")) {
  dataset <- arrow::open_dataset(arrow_dir)
  source_path <- file.path(arrow_dir, "_audit", "source_contributions")
  sources <- if (dir.exists(source_path)) arrow::open_dataset(source_path) else NULL
  data.table::rbindlist(lapply(pollutants, function(pol) {
    reason <- paste0(pol, "_screen_reason")
    if (!reason %in% dataset$schema$names) return(NULL)
    fields <- c("station", "station_original", "datetime", "year", "input_id",
                paste0(pol, c("_original", "_source_validation", "_screen_reason")))
    records <- dataset |>
      dplyr::filter(.data[[reason]] %in% c(1L, 2L)) |>
      dplyr::select(dplyr::all_of(fields)) |>
      dplyr::collect() |>
      data.table::as.data.table()
    data.table::setnames(records,
      paste0(pol, c("_original", "_source_validation", "_screen_reason")),
      c("value", "source_validation", "screen_reason"))
    records[, pollutant := pol]
    if (!is.null(sources) && nrow(records)) {
      # Collect links only for stations/years with excluded observations.
      stations <- unique(records$station)
      years <- unique(records$year)
      links <- sources |>
        dplyr::filter(pollutant == pol, station %in% stations, year %in% years) |>
        dplyr::select(station, datetime, year, input_id, source_ids) |>
        dplyr::collect() |>
        data.table::as.data.table()
      records <- merge(records, links, by = c("station", "datetime", "year", "input_id"),
                       all.x = TRUE)
    } else records[, source_ids := NA_character_]
    records[, station := station_original]
    records[, station_original := NULL]
    records[, datetime := format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")]
    data.table::setcolorder(records, c("station", "datetime", "year", "pollutant",
      "input_id", "value", "source_validation", "source_ids", "screen_reason"))
    data.table::setorder(records, station, datetime)
    records
  }), fill = TRUE)
}
