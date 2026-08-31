# ============================================================================================
# IDB: Air monitoring — legacy hourly imputation
# ============================================================================================
#' @Goal: Reproduce the coauthor's original hourly imputation, for comparison only.
#
#' @Description: The legacy pipeline's OLS gap filler, kept here so the public estimator
#   in src/general_utilities/process/imputation.R carries no switch. Nothing in the
#   paper's results path calls this; it exists so the Step 0-4 ladder can measure what
#   the corrected estimator changed. Sourced by
#   config_utils_validation_old_version.R; never sourced directly by a script.
#
#' @Summary:
#   1. impute_missing_hourly_ols_legacy
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: impute_missing_hourly_ols_legacy
#
#' @param arrow_dir  string; Arrow dataset (hourly).
#' @param out_dir    string; output directory.
#' @param out_name   string; prefix for the output folder.
#' @param pollutants character; pollutants to fill. Default c("pm10", "pm25").
#' @param id_col     string; station identifier column. Default "station".
#' @param overwrite  logical; skip if the output already exists. Default TRUE.
#' @param quiet      logical; suppress messages. Default FALSE.
#
#' @return  invisible list with out_path, n_imputed, per_poll and per_year.
#
#' @details
#   Replicates the legacy compaction exactly, including its defect. For each station-hour
#   it collects the non-missing readings of the other stations and packs them leftward
#   into anonymous other_1 ... other_k columns, so a given column holds a different
#   station from one row to the next. Spatial identity is destroyed and the missingness
#   dummies degenerate into a count of how many stations were offline. One pooled model
#   is fitted for the whole city rather than one per station.
#
#   Kept faithful on purpose: the point of the comparison is to measure this behaviour, so
#   the defect must not be repaired here. The corrected estimator is
#   impute_missing_hourly_ols() in src/general_utilities/process/imputation.R.
#
#' @Written_on : February 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
impute_missing_hourly_ols_legacy <- function(
    arrow_dir,
    out_dir,
    out_name,
    pollutants = c("pm10", "pm25"),
    id_col     = "station",
    overwrite  = TRUE,
    quiet      = FALSE
) {

  out_path <- file.path(out_dir, out_name)

  if (!overwrite && dir.exists(out_path)) {
    if (!quiet) message("Output exists; skipping.")
    return(invisible(list(out_path = out_path, n_imputed = NA_integer_)))
  }

  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

  # Station key normalisation, matching compute_distance_matrices().
  .normalize_st <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub("[^A-Z0-9_]", "_", x)
  }

  if (!quiet) message("[impute-legacy] Scanning dataset ...")
  ds <- arrow::open_dataset(arrow_dir)

  if (!id_col %in% names(ds)) stop("Column '", id_col, "' not found in data.")

  has_yr <- "year" %in% names(ds)

  if (has_yr) {
    unique_years <- ds |> dplyr::select(year) |> dplyr::distinct() |>
      dplyr::collect() |> dplyr::pull() |> sort()
  } else {
    dts <- ds |> dplyr::select(datetime) |> dplyr::collect()
    unique_years <- sort(unique(lubridate::year(dts$datetime)))
  }

  pollutants <- intersect(pollutants, names(ds))
  if (length(pollutants) == 0L) stop("No requested pollutants found.")

  all_per_poll <- list()

  for (yr in unique_years) {
    if (!quiet) message("\n[impute-legacy] --- Year: ", yr, " ---")

    if (has_yr) {
      dt <- ds |> dplyr::filter(year == yr) |> dplyr::collect()
    } else {
      yr_s <- as.POSIXct(paste0(yr, "-01-01 00:00:00"), tz = "UTC")
      yr_e <- as.POSIXct(paste0(yr + 1, "-01-01 00:00:00"), tz = "UTC")
      dt <- ds |> dplyr::filter(datetime >= yr_s, datetime < yr_e) |>
        dplyr::collect()
    }

    data.table::setDT(dt)

    # Inf and NaN are missing readings, not extreme ones.
    for (p in pollutants) {
      if (p %in% names(dt)) {
        dt[!is.finite(get(p)) & !is.na(get(p)), (p) := NA_real_]
      }
    }

    if (!has_yr) dt[, year := yr]
    dt[, month := as.factor(lubridate::month(datetime))]
    dt[, hour := as.factor(lubridate::hour(datetime))]
    dt[, day_week := as.factor(lubridate::wday(datetime, week_start = 1))]
    dt[, station_code := as.factor(.normalize_st(get(id_col)))]

    for (poll in pollutants) {
      if (!quiet) message("         Fitting pooled OLS for: ", poll)

      st_names <- sort(unique(as.character(dt$station_code)))
      n_st <- length(st_names)

      if (n_st < 2) {
        if (!quiet) message("         < 2 IDs. Skipping.")
        next
      }

      w_dt <- data.table::dcast(
        dt, datetime ~ station_code, value.var = poll,
        fun.aggregate = function(x) {
          v <- x[!is.na(x)]
          if (length(v) == 0L) NA_real_ else mean(v)
        }
      )

      dt_reg <- w_dt[dt, on = "datetime"]

      mat_all <- as.matrix(dt_reg[, ..st_names])
      row_idx <- match(as.character(dt_reg$station_code), st_names)

      mask <- matrix(TRUE, nrow = nrow(mat_all), ncol = ncol(mat_all))
      mask[cbind(seq_len(nrow(mat_all)), row_idx)] <- FALSE

      mat_other <- matrix(mat_all[mask], nrow = nrow(mat_all), ncol = n_st - 1)

      # The defect this function exists to reproduce: readings are packed leftward,
      # so other_1 is a different station on every row.
      shift_na <- function(x) {
        v <- x[!is.na(x)]
        c(v, rep(NA_real_, length(x) - length(v)))
      }

      mat_shifted <- t(apply(mat_other, 1, shift_na))

      other_cols <- paste0("other_", seq_len(n_st - 1))
      dt_other <- data.table::as.data.table(mat_shifted)
      data.table::setnames(dt_other, other_cols)

      dt_reg <- cbind(dt_reg, dt_other)

      other_m_cols <- paste0(other_cols, "_m")

      for (col in other_cols) {
        m_col <- paste0(col, "_m")
        dt_reg[, (m_col) := as.integer(is.na(get(col)))]
        dt_reg[is.na(get(col)), (col) := 0]
      }

      f_str <- paste(
        poll, "~", paste(c(other_cols, other_m_cols), collapse = " + "),
        "+ station_code + month*day_week + hour*day_week + month*hour"
      )

      dt_reg[, prediction := NA_real_]

      n_miss_by_sta <- dt_reg[, .(n_miss = sum(is.na(get(poll)))), by = station_code]
      keep_sta <- n_miss_by_sta[n_miss < .N - 1, station_code]
      train_idx <- which(dt_reg$station_code %in% keep_sta)

      if (length(train_idx) > 50) {
        model <- tryCatch({
          stats::lm(stats::as.formula(f_str), data = dt_reg[train_idx])
        }, warning = function(w) {
          suppressWarnings(stats::lm(stats::as.formula(f_str), data = dt_reg[train_idx]))
        }, error = function(e) NULL)

        if (!is.null(model)) {
          valid <- rep(TRUE, nrow(dt_reg))

          for (fac in names(model$xlevels)) {
            valid <- valid & (as.character(dt_reg[[fac]]) %in% model$xlevels[[fac]])
          }

          if (any(valid)) {
            dt_reg[valid, prediction := suppressWarnings(
              stats::predict(model, newdata = dt_reg[valid])
            )]
          }
        }
      }

      dt[, prediction := dt_reg$prediction]

      is_miss <- is.na(dt[[poll]])
      n_imp <- sum(is_miss & !is.na(dt$prediction))
      dt[is_miss, (poll) := dt$prediction[is_miss]]

      t_col <- paste0(poll, "_imputed_from")
      if (!t_col %in% names(dt)) dt[, (t_col) := NA_character_]
      dt[is_miss & !is.na(prediction), (t_col) := "OLS_Legacy"]

      all_per_poll[[length(all_per_poll) + 1]] <- data.table::data.table(
        year = yr, pollutant = poll, n_imputed = n_imp
      )

      if (!quiet) message("         Filled ", n_imp, " obs.")
    }

    drop <- intersect(c("month", "hour", "day_week", "station_code", "prediction"),
                      names(dt))
    dt[, (drop) := NULL]

    arrow::write_dataset(
      dataset = dt,
      path    = out_path,
      format  = "parquet",
      partitioning = "year",
      existing_data_behavior = "overwrite"
    )
  }

  pp <- data.table::rbindlist(all_per_poll)

  pp_summary <- if (nrow(pp) > 0) {
    pp[, .(n_imputed = sum(n_imputed)), by = pollutant]
  } else {
    data.table::data.table(pollutant = character(), n_imputed = integer())
  }

  invisible(list(out_path = out_path, n_imputed = sum(pp_summary$n_imputed),
                 per_poll = pp_summary, per_year = pp))
}
