# ============================================================================================
# IDB: Air monitoring — hourly imputation
# ============================================================================================
#' @Goal: Functions for hourly imputation.
#
#' @Description: Fills missing hourly readings by OLS on neighbouring stations, used for
#   the imputed robustness specification. Sourced by config_utils_process_data.R; never
#   sourced directly by a script.
#
#' @Summary:
#   1. impute_missing_hourly_ols
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: impute_missing_hourly_ols
#
#' @param arrow_dir   string; Arrow dataset (hourly).
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
#' @return  invisible list with out_path, diag_path, n_imputed, per_poll and per_year.
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
#   Only gaps are filled. Where a station has fewer than 50 rows, fewer than 50 observed
#   readings, or no neighbour that varies, no model is fitted and its gaps stay missing.
#
#   `years` exists because the cost is one model per station per pollutant per year, and
#   the panels run from 2000. Imputing every year of every city is hours of fitting for
#   results the paper never reports, so the pipeline asks for the analysis year alone.
#
#   The fitted values for diag_year are written to a separate Parquet rather than kept in
#   the panel, because the diagnostics figures need the prediction at every hour, observed
#   or not, while the panel itself must carry one column per pollutant.
#
#' @Written_on : February 2026
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
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

  # Station key normalisation, matching compute_distance_matrices().
  .normalize_st <- function(x) {
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
    dt[, station_code := as.factor(.normalize_st(get(id_col)))]

    # 3. Pollutant loop
    for (poll in pollutants) {
      if (!quiet) message("         Fitting OLS for: ", poll)

      st_names <- sort(unique(as.character(dt$station_code)))
      n_st <- length(st_names)

      if (n_st < 2) {
        if (!quiet) message("         < 2 IDs. Skipping.")
        next
      }

      # One column per station, so a neighbour keeps its identity in the model.
      w_dt <- data.table::dcast(
        dt, datetime ~ station_code, value.var = poll,
        fun.aggregate = function(x) {
          v <- x[!is.na(x)]
          if (length(v) == 0L) NA_real_ else mean(v)
        }
      )

      dt_reg <- w_dt[dt, on = "datetime"]

      # A missing neighbour enters as zero plus its own indicator.
      for (col in st_names) {
        m_col <- paste0(col, "_m")
        dt_reg[, (m_col) := as.integer(is.na(get(col)))]
        dt_reg[is.na(get(col)), (col) := 0]
      }

      dt_reg[, prediction := NA_real_]

      for (st in st_names) {
        pred_cols <- setdiff(st_names, st)

        idx_fit <- which(dt_reg$station_code == st)
        if (length(idx_fit) < 50) next

        # A neighbour that never varies for this station carries no information.
        keep_p <- vapply(pred_cols, function(col) {
          v <- dt_reg[[col]][idx_fit]
          length(unique(v[!is.na(v)])) > 1L
        }, logical(1))

        pred_cols <- pred_cols[keep_p]
        if (length(pred_cols) == 0L) next
        pred_m <- paste0(pred_cols, "_m")

        # Only include a temporal factor this station actually varies over, or the
        # model matrix is rank deficient before it is fitted.
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

        if (sum(!is.na(dt_reg[[poll]][idx_fit])) < 50) next

        model <- tryCatch({
          stats::lm(stats::as.formula(f_str), data = dt_reg[idx_fit])
        }, warning = function(w) {
          suppressWarnings(stats::lm(stats::as.formula(f_str), data = dt_reg[idx_fit]))
        }, error = function(e) {
          if (!quiet) message("         [!] Error on ", st, ": ", e$message)
          NULL
        })

        if (!is.null(model)) {
          valid <- rep(TRUE, length(idx_fit))

          for (fac in names(model$xlevels)) {
            valid <- valid & (as.character(dt_reg[[fac]][idx_fit]) %in%
                                model$xlevels[[fac]])
          }

          if (any(valid)) {
            dt_reg[idx_fit[valid], prediction := suppressWarnings(
              stats::predict(model, newdata = dt_reg[idx_fit[valid]])
            )]
          }
        }
      }

      dt[, prediction := dt_reg$prediction]

      # 4. Keep this pollutant's fitted values before the next one overwrites them
      if (!is.null(diag_year) && yr == diag_year) {
        # station_id, not the model's own factor: the underscored form the formulas
        # need would not join to the station-level socioeconomic data downstream.
        diag_rows[[length(diag_rows) + 1L]] <- data.table::data.table(
          datetime    = dt$datetime,
          station_id  = normalize_station(dt$station),
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
      dt[is_miss & !is.na(prediction), (t_col) := "OLS_Unbiased"]

      all_per_poll[[length(all_per_poll) + 1]] <- data.table::data.table(
        year = yr, pollutant = poll, n_imputed = n_imp
      )

      if (!quiet) message("         Filled ", n_imp, " obs.")
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

  invisible(list(out_path = out_path, diag_path = diag_path,
                 n_imputed = sum(pp_summary$n_imputed),
                 per_poll = pp_summary, per_year = pp))
}
