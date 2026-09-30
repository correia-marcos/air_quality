# ==========================================================================================
# IDB: Air monitoring — hourly imputation
# ==========================================================================================
#' @Goal: Functions for hourly imputation.
#
#' @Description: Fills missing hourly readings by OLS on neighbouring stations, used for
#   the imputed robustness specification. Scripts and targets call the same model directly.
#
#' @Summary:
#   1. impute_missing_hourly_ols
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: impute_missing_hourly_ols
#
#' @param arrow_dir   string; cleaned, balanced station-hour Arrow dataset.
#' @param out_dir     string; output directory.
#' @param out_name    string; name of the output folder inside out_dir.
#' @param pollutants  character; pollutants to fill. Default c("pm10", "pm25").
#' @param id_col      string; station identifier column. Default "station".
#' @param years       integer vector or NULL; years to impute. NULL does every year in
#                     the dataset. Default NULL.
#' @param diag_year   integer or NULL; year whose fitted values are saved for the
#                     diagnostics figures. NULL saves none. Default 2023.
#' @param overwrite   logical; skip if the output already exists. Default TRUE.
#' @param quiet       logical; suppress messages. Default FALSE.
#
#' @return Invisible list with out_path, diag_path, summary_path, n_imputed, per_poll
# and per_year, plus per_station with window counts, fitting status, rank and residual
# degrees of freedom. Its station_id uses the model's underscored station keys.
# n_supported_gaps counts calendar-supported gaps before checking estimability; NA means
# that check was not needed. fitted records a computed model, including rejected models.
# Processing writes the panel, fitted values and pollutant-level counts; per_station
# remains an inspectable table in the returned list.
# Filled readings have `OLS_imputed` in the pollutant's `_imputed_from` column;
# this label identifies the method, not a claim of unbiasedness.
# With overwrite = FALSE and an existing panel, returns only out_path and n_imputed = NA.
#
#' @details
#   Implements the paper's imputation equation: for each station, the pollutant is
#   regressed on the contemporaneous readings of every other station, on an indicator for
#   each of those being missing, and on month, day-of-week and hour effects with their
#   interactions. A missing predictor reading enters as zero alongside its indicator,
#   which lets an hour be predicted from whichever stations happened to be reporting.
#
#   One model per station, with the neighbours named. Fitting a single pooled model over
#   anonymous neighbour columns would destroy the spatial correlation the equation relies
#   on; that variant is the legacy behaviour and lives in
#   src/general_utilities/validation/imputation_legacy.R, outside the results path.
#
#   dt retains the yearly observations; w_dt holds each station's readings by hour.
#   dt_reg joins these inputs for one station at a time. Neighbour predictors are prepared
#   before fitting, so a filled reading never becomes another station's predictor.
#   .station_model_key() gives model columns underscored names; normalize_station() gives
#   diagnostics the shared identifiers used in downstream joins. Both use id_col.
#
#   For each station, pollutant and year, the window starts at its first finite reading
#   in the cleaned panel and extends through year-end. Earlier gaps remain missing.
#   This observed-data boundary is not a commissioning date; trailing gaps are eligible.
#   A complete window skips fitting, while its readings remain available to other models.
#   The input must already contain the hourly grid; this function does not add absent rows.
#
#   Models use only finite outcomes. Calendar terms and their interactions require at
#   least two observed levels. Predictions must use observed month, weekday and hour
#   levels, even when a constant factor was omitted from the formula. Numeric predictors
#   constant during training stay in the model if they vary elsewhere, so unsupported
#   changes in those predictors can be identified by the estimability check.
#   Fewer than 50 observed readings, no varying training neighbour, or no supported gaps
#   skips fitting. Models with no residual degrees of freedom are rejected; non-estimable
#   predictions stay missing. These checks do not establish out-of-sample accuracy.
#
#   `years` exists because the cost is one model per station per pollutant per year, and
#   the panels run from 2000. Imputing every year of every city is hours of fitting for
#   results the paper never reports, so the pipeline asks for the analysis year alone.
#
#   The fitted values for diag_year are written to a separate Parquet rather than kept in
#   the panel. Accepted models retain predictions at supported observed and missing hours.
#   Skipped models have no fitted values; observed readings remain unchanged in all cases.
#
#' @Written_on : February 2026
#' @Written_by : Marcos Paulo
#' @Updated_on : September 2026
# ------------------------------------------------------------------------------------------
impute_missing_hourly_ols <- function(
    arrow_dir,
    out_dir,
    out_name,
    pollutants = c("pm10", "pm25"),
    id_col     = "station",
    years      = NULL,
    diag_year  = 2023L,
    overwrite  = TRUE,
    quiet      = FALSE
) {

  out_path  <- file.path(out_dir, out_name)
  diag_path <- file.path(out_dir, paste0(out_name, "_predictions.parquet"))

  if (!overwrite && dir.exists(out_path)) {
    if (!quiet) message("Output exists; skipping.")
    return(invisible(list(out_path = out_path, n_imputed = NA_integer_)))
  }

  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

  # Give model columns station keys without spaces or accents.
  .station_model_key <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub("[^A-Z0-9_]", "_", x)
  }

  # 1. Scan dataset for years
  if (!quiet) message("[impute] Scanning dataset ...")
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

  if (!is.null(years)) {
    missing_years <- setdiff(years, unique_years)
    if (length(missing_years) > 0L) {
      stop("Year(s) not in the dataset: ", paste(missing_years, collapse = ", "))
    }
    unique_years <- sort(years)
  }

  pollutants <- intersect(pollutants, names(ds))
  if (length(pollutants) == 0L) stop("No requested pollutants found.")

  all_per_poll <- list()
  diag_rows <- list()
  station_summaries <- list()

  # 2. Year loop
  for (yr in unique_years) {
    if (!quiet) message("\n[impute] --- Year: ", yr, " ---")

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
    dt[, station_code := as.factor(.station_model_key(get(id_col)))]
    st_names <- sort(unique(as.character(dt$station_code)))

    save_diagnostics <- !is.null(diag_year) && yr == diag_year
    if (save_diagnostics) diag_station_ids <- normalize_station(dt[[id_col]])

    # 3. Pollutant loop
    for (poll in pollutants) {
      if (!quiet) message("         Fitting OLS for: ", poll)

      # One column per station, so a neighbour keeps its identity in the model.
      w_dt <- data.table::dcast(
        dt, datetime ~ station_code, value.var = poll,
        fun.aggregate = function(x) {
          v <- x[!is.na(x)]
          if (length(v) == 0L) NA_real_ else mean(v)
        }
      )

      # A missing neighbour enters as zero plus its own indicator.
      for (col in st_names) {
        m_col <- paste0(col, "_m")
        w_dt[, (m_col) := as.integer(is.na(get(col)))]
        w_dt[is.na(get(col)), (col) := 0]
      }

      dt[, prediction := NA_real_]

      fit_summary <- data.table::data.table(
        year = yr, pollutant = poll, station_id = st_names,
        first_observed = dt$datetime[rep(NA_integer_, length(st_names))],
        n_observed = 0L, n_window_gaps = 0L, n_supported_gaps = NA_integer_,
        fitted = FALSE, rank = NA_integer_, residual_df = NA_integer_,
        n_imputed = 0L, status = "no_observations")

      for (i in seq_along(st_names)) {
        st <- st_names[i]
        idx_station <- which(dt$station_code == st)

        # Join neighbouring readings to this station's hours, preserving its row order.
        dt_reg <- w_dt[dt[idx_station], on = "datetime"]
        idx_fit <- which(is.finite(dt_reg[[poll]]))
        fit_summary$n_observed[i] <- length(idx_fit)
        if (length(idx_fit) == 0L) next

        # Start this pollutant's window at its first observed hour.
        first_reading <- min(dt_reg$datetime[idx_fit])
        idx_window <- which(dt_reg$datetime >= first_reading)
        gaps <- is.na(dt_reg[[poll]][idx_window])
        fit_summary$first_observed[i] <- first_reading
        fit_summary$n_window_gaps[i] <- sum(gaps)

        if (!any(gaps)) {
          fit_summary$n_supported_gaps[i] <- 0L
          fit_summary$status[i] <- "complete_window"
          next
        }

        if (length(idx_fit) < 50L) {
          fit_summary$status[i] <- "insufficient_observations"
          next
        }

        # Keep numeric columns that vary anywhere; check training variation separately.
        pred_cols <- setdiff(st_names, st)
        keep_p <- vapply(pred_cols, function(col) {
          length(unique(dt_reg[[col]])) > 1L
        }, logical(1))
        pred_cols <- pred_cols[keep_p]
        varies_in_fit <- vapply(pred_cols, function(col) {
          length(unique(dt_reg[[col]][idx_fit])) > 1L
        }, logical(1))
        if (!any(varies_in_fit)) {
          fit_summary$status[i] <- "no_varying_neighbors"
          next
        }
        pred_m <- paste0(pred_cols, "_m")

        # Check calendar support even for factors omitted from the formula.
        valid <- rep(TRUE, length(idx_window))
        for (fac in c("month", "day_week", "hour")) {
          valid <- valid & (dt_reg[[fac]][idx_window] %in% dt_reg[[fac]][idx_fit])
        }
        fit_summary$n_supported_gaps[i] <- sum(gaps & valid)
        if (!any(gaps & valid)) {
          fit_summary$status[i] <- "unsupported_calendar"
          next
        }

        # Build calendar effects from the observed outcomes only.
        t_terms <- character()
        has_m <- length(unique(dt_reg$month[idx_fit])) > 1L
        has_d <- length(unique(dt_reg$day_week[idx_fit])) > 1L
        has_h <- length(unique(dt_reg$hour[idx_fit])) > 1L

        if (has_m) t_terms <- c(t_terms, "month")
        if (has_d) t_terms <- c(t_terms, "day_week")
        if (has_h) t_terms <- c(t_terms, "hour")

        if (has_m && has_d) t_terms <- c(t_terms, "month:day_week")
        if (has_h && has_d) t_terms <- c(t_terms, "hour:day_week")
        if (has_m && has_h) t_terms <- c(t_terms, "month:hour")

        temp_str <- if (length(t_terms) > 0) paste(t_terms, collapse = " + ") else "1"

        f_str <- paste(
          poll, "~", paste(c(pred_cols, pred_m), collapse = " + "), "+", temp_str
        )

        model <- tryCatch({
          stats::lm(stats::as.formula(f_str), data = dt_reg[idx_fit],
                    na.action = stats::na.fail)
        }, error = function(e) {
          if (!quiet) message("         [!] Error on ", st, ": ", e$message)
          NULL
        })

        if (is.null(model)) {
          fit_summary$status[i] <- "fit_error"
          next
        }

        fit_summary$fitted[i] <- TRUE
        fit_summary$rank[i] <- model$rank
        fit_summary$residual_df[i] <- stats::df.residual(model)
        if (stats::df.residual(model) <= 0L) {
          fit_summary$status[i] <- "no_residual_df"
          next
        }

        idx_predict <- idx_window[valid]
        predictions <- stats::predict(model, newdata = dt_reg[idx_predict],
                                      rankdeficient = "NA")
        predictions[!is.finite(predictions)] <- NA_real_
        dt[idx_station[idx_predict], prediction := predictions]
        n_filled <- sum(gaps[valid] & is.finite(predictions))
        fit_summary$n_imputed[i] <- n_filled
        fit_summary$status[i] <- if (n_filled == 0L) "no_estimable_gaps" else
          if (n_filled < sum(gaps)) "partially_imputed" else "imputed"
      }

      station_summaries[[length(station_summaries) + 1L]] <- fit_summary

      # 4. Keep this pollutant's fitted values before the next one overwrites them
      if (save_diagnostics) {
        diag_rows[[length(diag_rows) + 1L]] <- data.table::data.table(
          datetime    = dt$datetime,
          station_id  = diag_station_ids,
          pollutant   = poll,
          observed    = dt[[poll]],
          predicted   = dt$prediction,
          was_missing = is.na(dt[[poll]])
        )
      }

      # 5. Apply predictions to gaps
      is_miss <- is.na(dt[[poll]])
      n_imp <- sum(is_miss & !is.na(dt$prediction))
      dt[is_miss, (poll) := dt$prediction[is_miss]]

      t_col <- paste0(poll, "_imputed_from")
      if (!t_col %in% names(dt)) dt[, (t_col) := NA_character_]
      dt[is_miss & !is.na(prediction), (t_col) := "OLS_imputed"]

      all_per_poll[[length(all_per_poll) + 1]] <- data.table::data.table(
        year = yr, pollutant = poll, n_imputed = n_imp
      )

      if (!quiet) {
        message("         Filled ", n_imp, " obs.; skipped ",
                sum(fit_summary$status == "complete_window"), " complete window(s).")
      }
    }

    # 6. Write year partition
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

  # 7. Diagnostics and summary
  if (length(diag_rows) > 0L) {
    arrow::write_parquet(data.table::rbindlist(diag_rows), diag_path)
    if (!quiet) message("[impute] Wrote fitted values to ", diag_path)
  } else {
    diag_path <- NA_character_
  }

  pp <- data.table::rbindlist(all_per_poll)

  pp_summary <- if (nrow(pp) > 0) {
    pp[, .(n_imputed = sum(n_imputed)), by = pollutant]
  } else {
    data.table::data.table(pollutant = character(), n_imputed = integer())
  }

  summary_path <- file.path(out_dir, paste0(out_name, "_counts.parquet"))
  arrow::write_parquet(pp_summary, summary_path)

  invisible(list(out_path = out_path, diag_path = diag_path, summary_path = summary_path,
                 n_imputed = sum(pp_summary$n_imputed),
                 per_poll = pp_summary, per_year = pp,
                 per_station = data.table::rbindlist(station_summaries)))
}
