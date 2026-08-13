# ============================================================================================
# IDB: Air monitoring — hourly imputation
# ============================================================================================
#' @Goal: Functions for hourly imputation.
#
#' @Description: Fills missing hourly readings by OLS on neighbouring stations, used for the imputed
#   robustness specification.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
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
#' @param arrow_dir  string; Arrow dataset (hourly).
#' @param out_dir    string; output directory.
#' @param out_name   string; prefix for output folder.
#' @param pollutants character; default c("pm10","pm25").
#' @param id_col     string; unique station column. Default "station_code".
#' @param legacy_mode logical; if TRUE, replicates the shifting-identity
#                    compaction bug of the legacy pipeline. Default FALSE.
#' @param overwrite  logical; skip if output exists. Default TRUE.
#' @param quiet      logical; suppress messages. Default FALSE.
#
#' @details
#   LEGACY MODE (TRUE):
#     Replicates the Dropbox pipeline exactly. It collects non-NA readings from 
#     other stations and compacts them leftward into anonymous `other_X` columns.
#     This destroys spatial identity and changes missingness dummies into a simple 
#     count of offline stations. Fits a single pooled OLS.
#
#   UNBIASED MODE (FALSE):
#     Correctly implements the paper's intended Eq(1). Fits separate models per 
#     station, using explicitly named neighboring stations as predictors, 
#     preserving exact spatial correlation and distinct missingness states.
#' @return      List with out_path = out_path, n_imputed = sum(pp_summary$n_imputed),
#               per_poll = pp_summary, per_year = pp)
#' @Written_on : 02/02/2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
impute_missing_hourly_ols <- function(
    arrow_dir,
    out_dir,
    out_name,
    pollutants  = c("pm10", "pm25"),
    id_col      = "station",
    legacy_mode = FALSE,
    overwrite   = TRUE,
    quiet       = FALSE
) {
  pkgs <- c("arrow", "data.table", "stats", "lubridate", "dplyr", "stringi")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Missing: ", p)
  }
  
  out_path <- file.path(out_dir, out_name)
  if (!overwrite && dir.exists(out_path)) {
    if (!quiet) message("Output exists; skipping.")
    return(invisible(list(out_path = out_path, n_imputed = NA_integer_)))
  }
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  
  # Normalization Helper (Matches compute_distance_matrices logic)
  .normalize_st <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    x <- gsub("[^A-Z0-9_]", "_", x) # Replace spaces/special chars with underscore
    return(x)
  }
  
  # ── 1. Scan dataset for years ───────────────────────────────────────────────
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
  
  pollutants <- intersect(pollutants, names(ds))
  if (length(pollutants) == 0L) stop("No requested pollutants found.")
  
  all_per_poll <- list()
  
  # ── 2. Year loop ────────────────────────────────────────────────────────────
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
    
    # Sanitise: Inf/-Inf/NaN → NA
    for (p in pollutants) {
      if (p %in% names(dt)) {
        dt[!is.finite(get(p)) & !is.na(get(p)), (p) := NA_real_]
      }
    }
    
    # Temporal factors & Normalized Station ID
    if (!has_yr) dt[, year := yr]
    dt[, month := as.factor(lubridate::month(datetime))]
    dt[, hour := as.factor(lubridate::hour(datetime))]
    dt[, day_week := as.factor(lubridate::wday(datetime, week_start = 1))]
    dt[, station_code := as.factor(.normalize_st(get(id_col)))]
    
    # ── 3. Pollutant loop ─────────────────────────────────────────────────────
    for (poll in pollutants) {
      if (!quiet) message("         Fitting OLS for: ", poll)
      
      st_names <- sort(unique(as.character(dt$station_code)))
      n_st <- length(st_names)
      if (n_st < 2) {
        if (!quiet) message("         < 2 IDs. Skipping.")
        next
      }
      
      if (legacy_mode) {
        # ── LEGACY MODE ───────────────────────────────────────────────────────
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
            stats::lm(as.formula(f_str), data = dt_reg[train_idx])
          }, warning = function(w) {
            suppressWarnings(stats::lm(as.formula(f_str), data = dt_reg[train_idx]))
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
        
      } else {
        # ── UNBIASED MODE ─────────────────────────────────────────────────────
        w_dt <- data.table::dcast(
          dt, datetime ~ station_code, value.var = poll,
          fun.aggregate = function(x) {
            v <- x[!is.na(x)]
            if (length(v) == 0L) NA_real_ else mean(v)
          }
        )
        dt_reg <- w_dt[dt, on = "datetime"]
        
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
          
          keep_p <- vapply(pred_cols, function(col) {
            v <- dt_reg[[col]][idx_fit]
            length(unique(v[!is.na(v)])) > 1L
          }, logical(1))
          
          pred_cols <- pred_cols[keep_p]
          if (length(pred_cols) == 0L) next
          pred_m <- paste0(pred_cols, "_m")
          
          # Dynamic Formula Builder (Prevents 'contrasts' error on sparse data)
          t_terms <- character()
          has_m <- length(unique(dt_reg$month[idx_fit])) > 1L
          has_d <- length(unique(dt_reg$day_week[idx_fit])) > 1L
          has_h <- length(unique(dt_reg$hour[idx_fit])) > 1L
          
          if (has_m) t_terms <- c(t_terms, "month")
          if (has_d) t_terms <- c(t_terms, "day_week")
          if (has_h) t_terms <- c(t_terms, "hour")
          
          # Only add interactions if both main factors exist for this station
          if (has_m && has_d) t_terms <- c(t_terms, "month:day_week")
          if (has_h && has_d) t_terms <- c(t_terms, "hour:day_week")
          if (has_m && has_h) t_terms <- c(t_terms, "month:hour")
          
          temp_str <- if (length(t_terms) > 0) paste(t_terms, collapse=" + ") else "1"
          
          f_str <- paste(
            poll, "~", paste(c(pred_cols, pred_m), collapse = " + "), "+", temp_str
          )
          
          if (sum(!is.na(dt_reg[[poll]][idx_fit])) < 50) next
          
          model <- tryCatch({
            stats::lm(as.formula(f_str), data = dt_reg[idx_fit])
          }, warning = function(w) {
            suppressWarnings(stats::lm(as.formula(f_str), data = dt_reg[idx_fit]))
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
              # Suppress the multicollinearity rank-deficient warnings
              dt_reg[idx_fit[valid], prediction := suppressWarnings(
                stats::predict(model, newdata = dt_reg[idx_fit[valid]])
              )]
            }
          }
        }
        
        dt[, prediction := dt_reg$prediction]
      }
      
      # ── 4. Apply predictions to gaps ────────────────────────────────────────
      is_miss <- is.na(dt[[poll]])
      n_imp <- sum(is_miss & !is.na(dt$prediction))
      dt[is_miss, (poll) := dt$prediction[is_miss]]
      
      t_col <- paste0(poll, "_imputed_from")
      if (!t_col %in% names(dt)) dt[, (t_col) := NA_character_]
      mode_lbl <- if (legacy_mode) "OLS_Legacy" else "OLS_Unbiased"
      dt[is_miss & !is.na(prediction), (t_col) := mode_lbl]
      
      all_per_poll[[length(all_per_poll) + 1]] <- data.table::data.table(
        year = yr, pollutant = poll, n_imputed = n_imp
      )
      if (!quiet) message("         Filled ", n_imp, " obs.")
    }
    
    # ── 5. Write year partition ───────────────────────────────────────────────
    drop <- c("month", "hour", "day_week", "station_code", "prediction")
    drop <- intersect(drop, names(dt))
    dt[, (drop) := NULL]
    
    arrow::write_dataset(
      dataset  = dt,
      path     = out_path,
      format   = "parquet",
      partitioning = "year",
      existing_data_behavior = "overwrite"
    )
  }
  
  # ── 6. Summary ──────────────────────────────────────────────────────────────
  pp <- data.table::rbindlist(all_per_poll)
  if (nrow(pp) > 0) {
    pp_summary <- pp[, .(n_imputed = sum(n_imputed)), by = pollutant]
  } else {
    pp_summary <- data.table::data.table(pollutant=character(), n_imputed=integer())
  }
  
  invisible(list(out_path = out_path, n_imputed = sum(pp_summary$n_imputed),
                 per_poll = pp_summary, per_year = pp))
}
