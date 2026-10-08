# Parameterized review of standardized hourly particulate concentrations.
#' @param arrow_dir Directory of the original year-partitioned hourly input.
#' @return Year and SHA-256 input identities, independent of the absolute directory.
#' @details A changed file anywhere in a partition invalidates its review decisions.
pollution_input_identities <- function(arrow_dir) {
  files <- sort(list.files(arrow_dir, pattern = "[.]parquet$", recursive = TRUE,
                           full.names = TRUE))
  if (!length(files)) stop("No partitioned Parquet inputs for quality review.")
  relative <- substring(files, nchar(sub("/$", "", arrow_dir)) + 2L)
  years <- as.integer(sub("^year=([0-9]{4})/.*", "\\1", relative))
  if (anyNA(years)) stop("Quality review requires year-partitioned Parquet inputs.")
  hashes <- vapply(files, digest::digest, character(1), file = TRUE, algo = "sha256")
  data.table::data.table(year = years, path = relative, sha256 = hashes)[,
    .(input_id = digest::digest(paste(path, sha256, collapse = "\n"),
                                algo = "sha256", serialize = FALSE)), by = year]
}

#' @param upper_bounds NULL, or named positive bounds; Inf disables one pollutant bound.
#' @param eligibility_cols NULL, or named pollutant-to-logical-column mapping.
#' @param pollutants Pollutants requested by the caller.
#' @return Invisibly TRUE; invalid options fail before any analytical output is replaced.
validate_pollution_quality_options <- function(upper_bounds, eligibility_cols, pollutants) {
  if (!is.null(upper_bounds) &&
      (!is.numeric(upper_bounds) || is.null(names(upper_bounds)) ||
       anyDuplicated(names(upper_bounds)) || any(!nzchar(names(upper_bounds))) ||
       anyNA(names(upper_bounds)) || anyNA(upper_bounds) || any(upper_bounds <= 0) ||
       any(!names(upper_bounds) %in% union(pollutants, c("pm25", "pm10"))))) {
    stop("upper_bounds must be NULL or uniquely named positive numeric bounds.")
  }
  if (!is.null(eligibility_cols) &&
      (!is.character(eligibility_cols) || is.null(names(eligibility_cols)) ||
       anyDuplicated(names(eligibility_cols)) || anyNA(eligibility_cols) ||
       any(!nzchar(eligibility_cols)) || any(!names(eligibility_cols) %in% pollutants))) {
    stop("eligibility_cols must map requested pollutants to logical column names.")
  }
  invisible(TRUE)
}

#' @param review_decisions NULL or a data frame of documented retain/exclude decisions.
#' @return Decisions with source-clock POSIXct datetimes, or NULL.
#' @details Keys use original station labels, before harmonization. The required value
#   and partition input_id bind decisions to the exact input being reviewed.
pollution_review_decisions <- function(review_decisions) {
  if (is.null(review_decisions)) return(NULL)
  reviews <- data.table::as.data.table(data.table::copy(review_decisions))
  required <- c("station", "datetime", "pollutant", "input_id", "value", "decision",
                "evidence", "reviewer", "review_date")
  missing <- setdiff(required, names(reviews))
  if (length(missing)) stop("Review decisions lack: ", paste(missing, collapse = ", "))
  if (!nrow(reviews)) return(NULL)
  if (!inherits(reviews$datetime, "POSIXct")) {
    reviews[, datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
  }
  text <- setdiff(required, c("datetime", "value"))
  if (any(vapply(reviews[, ..text], function(x)
    anyNA(x) || any(!nzchar(trimws(as.character(x)))), logical(1))) ||
    anyNA(reviews$datetime) || !is.numeric(reviews$value) ||
    any(!is.finite(reviews$value)) ||
    any(!reviews$decision %in% c("retain", "exclude")) ||
    anyNA(as.Date(reviews$review_date, format = "%Y-%m-%d"))) {
    stop("Reviews require complete keys, value, decision, evidence, reviewer and date.")
  }
  if (anyDuplicated(reviews[, .(station, datetime, pollutant)])) {
    stop("Duplicate or conflicting quality review decisions.")
  }
  if (any(reviews$decision == "retain" & reviews$value < 0)) {
    stop("A quality review cannot retain a negative concentration.")
  }
  reviews
}

#' @param path CSV of decisions, including a city column for recipe routing.
#' @param city Canonical city identifier.
#' @return Documented decisions for that city, or NULL when no decisions exist.
read_pollution_quality_reviews <- function(path, city) {
  reviews <- data.table::fread(path)
  if (!"city" %in% names(reviews)) stop("The decision registry requires a city column.")
  pollution_review_decisions(reviews[reviews$city == city])
}

#' @param data Hourly table with original station, datetime, pollutants and input_id.
#' @param reviews Normalized review-decision table.
#' @return Matching original row indices; unmatched or stale reviews fail.
match_pollution_reviews <- function(data, reviews) {
  if (is.null(reviews) || !nrow(reviews)) return(integer())
  if (!"input_id" %in% names(data)) stop("Reviews require an input_id on original rows.")
  if (any(!reviews$pollutant %in% names(data))) stop("Reviewed pollutant is absent.")
  rows <- data[reviews, on = .(station, datetime), which = TRUE]
  if (length(rows) != nrow(reviews) || anyNA(rows)) {
    stop("Review keys are unmatched or ambiguous in the original input.")
  }
  values <- vapply(seq_len(nrow(reviews)), function(i)
    as.numeric(data[[reviews$pollutant[i]]][rows[i]]), numeric(1))
  if (any(data$input_id[rows] != reviews$input_id) ||
      any(!is.finite(values)) || any(values != reviews$value)) {
    stop("Quality review input identity or original value does not match.")
  }
  rows
}

#' @param data Standardized hourly table; copied, never changed in place.
#' @param pollutants Pollutant columns to annotate and screen.
#' @param upper_bounds Named hourly review bounds in ug/m3; equality is eligible.
#' @param eligibility_cols Optional pollutant-to-logical-column mapping; only TRUE passes.
#' @param review_decisions Documented decisions tied to original labels and input identity.
#' @return Table with preserved input values, source status and project QA fields.
#' @details Bounds are project review choices, not universal instrument maxima. A
#   documented retain clears this quality hold, but does not bypass statistical cleaning
#   or an independent eligibility hold. Original values stay in {pollutant}_input.
screen_pollution_quality <- function(data, pollutants = c("pm10", "pm25"),
    upper_bounds = c(pm25 = 500, pm10 = 1000), eligibility_cols = NULL,
    review_decisions = NULL) {
  validate_pollution_quality_options(upper_bounds, eligibility_cols, pollutants)
  out <- data.table::as.data.table(data.table::copy(data))
  reviews <- pollution_review_decisions(review_decisions)
  rows <- match_pollution_reviews(out, reviews)
  for (pol in intersect(pollutants, names(out))) {
    value <- out[[pol]]
    missing <- is.na(value)
    invalid <- !missing & (!is.finite(value) | value < 0)
    bound <- if (pol %in% names(upper_bounds)) upper_bounds[[pol]] else Inf
    above <- !missing & is.finite(value) & value > bound
    gate <- rep(TRUE, nrow(out))
    if (pol %in% names(eligibility_cols)) {
      column <- eligibility_cols[[pol]]
      if (!column %in% names(out) || !is.logical(out[[column]])) {
        stop("Eligibility column must exist and be logical: ", column)
      }
      gate <- out[[column]]
    }
    status <- rep("not_flagged", nrow(out))
    reason <- rep("not_flagged", nrow(out))
    status[above | is.na(gate)] <- "pending_review"
    reason[above] <- "above_bound"
    reason[is.na(gate)] <- "eligibility_unknown"
    status[!is.na(gate) & !gate] <- "excluded"
    reason[!is.na(gate) & !gate] <- "eligibility_false"
    eligible <- !missing & !invalid & !above & !is.na(gate) & gate
    if (!is.null(reviews)) {
      chosen <- which(reviews$pollutant == pol)
      retained <- rows[chosen[reviews$decision[chosen] == "retain"]]
      excluded <- rows[chosen[reviews$decision[chosen] == "exclude"]]
      cleared <- retained[!is.na(gate[retained]) & gate[retained]]
      eligible[cleared] <- TRUE
      status[cleared] <- "reviewed_retained"
      reason[cleared] <- "documented_retain"
      eligible[excluded] <- FALSE
      status[excluded] <- "excluded"
      reason[excluded] <- "documented_exclude"
    }
    status[invalid] <- "excluded"
    reason[invalid] <- "negative_or_nonfinite"
    status[missing] <- "missing"
    reason[missing] <- "missing_input"
    eligible[missing | invalid] <- FALSE
    source_column <- paste0(pol, "_source_status")
    if (!source_column %in% names(out)) out[, (source_column) := "unknown"]
    out[, (source_column) := data.table::fifelse(
      is.na(get(source_column)), "unknown", get(source_column))]
    out[, (paste0(pol, c("_input", "_qa_status", "_qa_reason", "_qa_eligible"))) :=
      list(value, status, reason, eligible)]
    out[, (pol) := data.table::fifelse(eligible, as.numeric(value), NA_real_)]
  }
  out
}

#' @param arrow_dir Cleaned partitions with QA and final observed-use fields.
#' @param pollutants Pollutants to summarize.
#' @return Bounded station/year/pollutant counts by quality and statistical reason.
summarize_pollution_quality <- function(arrow_dir, pollutants = c("pm10", "pm25")) {
  dataset <- arrow::open_dataset(arrow_dir)
  data.table::rbindlist(lapply(pollutants, function(pol) {
    if (!paste0(pol, "_qa_status") %in% dataset$schema$names) return(NULL)
    labels <- c("source_status", "qa_status", "qa_reason", "qa_eligible",
                "outlier_reason", "use")
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

#' @param summary Named quality-count table.
#' @param path CSV destination outside the Arrow dataset.
#' @return Saved filename.
write_pollution_quality_summary <- function(summary, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(summary, path)
  path
}

#' @param arrow_dir Cleaned partitions, retaining original values and partition identities.
#' @param pollutants Pollutants whose project decisions require a linked record.
#' @return Records requiring review or documenting explicit project dispositions.
collect_pollution_review_records <- function(arrow_dir, pollutants = c("pm10", "pm25")) {
  dataset <- arrow::open_dataset(arrow_dir)
  data.table::rbindlist(lapply(pollutants, function(pol) {
    status <- paste0(pol, "_qa_status")
    if (!status %in% dataset$schema$names) return(NULL)
    fields <- c("station_original", "datetime", "year", "input_id",
      paste0(pol, c("_input", "_source_status", "_source_ids", "_qa_status", "_qa_reason")))
    records <- dataset |>
      dplyr::filter(.data[[status]] %in%
        c("pending_review", "reviewed_retained", "excluded")) |>
      dplyr::select(dplyr::all_of(intersect(fields, dataset$schema$names))) |>
      dplyr::collect() |>
      data.table::as.data.table()
    data.table::setnames(records, paste0(pol, c("_input", "_source_status",
      "_qa_status", "_qa_reason")), c("value", "source_status", "qa_status", "qa_reason"))
    if (paste0(pol, "_source_ids") %in% names(records)) {
      data.table::setnames(records, paste0(pol, "_source_ids"), "source_ids")
    } else records[, source_ids := NA_character_]
    data.table::setnames(records, "station_original", "station")
    records[, `:=`(pollutant = pol,
      datetime = format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"))]
    data.table::setcolorder(records, c("station", "datetime", "year", "pollutant",
      "input_id", "value", "source_status", "source_ids", "qa_status", "qa_reason"))
    data.table::setorder(records, station, datetime)
    records
  }), fill = TRUE)
}
