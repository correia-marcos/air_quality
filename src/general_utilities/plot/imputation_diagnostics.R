# ==========================================================================================
# IDB: Air monitoring — imputation diagnostics figures
# ==========================================================================================
#' @Goal: Functions for the figures that show what the hourly imputation predicted.
#
#' @Description: Two views of the fitted values written by impute_missing_hourly.R. The
#   first puts the prediction against the observed series, station by station, to inspect
#   how closely the fitted series follows the observations. The second asks whether the
#   missing-hour predictions differ from observed-hour means across stations ordered
#   by nearby education. These functions return tables or plots; the caller saves figures.
#
#' @Summary:
#   1. plot_imputation_series
#   2. summarize_imputation_ratios
#   3. plot_imputation_ratio_by_station
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# --------------------------------------------------------------------------------------
# Function: plot_imputation_series
#
#' @param pred_dt    data.table; fitted values from impute_missing_hourly_ols(), with
#                    datetime, station_id, pollutant, observed, predicted, was_missing.
#' @param pollutant  string; the pollutant to draw, "pm10" or "pm25".
#' @param city_label string; city name for the panel title.
#' @param n_stations integer or NULL; draw only this many stations, the ones with the
#                    most observed readings. NULL draws all of them.
#' @param obs_color  string; colour of the observed series.
#' @param pred_color string; colour of the fitted series.
#
#' @return ggplot object; no files are written.
#
#' @details
#   Each panel overlays observed readings and predictions, including predictions at
#   observed hours. Separate y scales make within-station patterns easier to see.
#   Agreement at fitted observations is not evidence of out-of-sample prediction accuracy.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------
plot_imputation_series <- function(pred_dt, pollutant, city_label,
                                  n_stations = NULL, obs_color = "grey25",
                                  pred_color = "#4C9BE8") {

  # `pollutant` names both the argument and a column; the local keeps them apart.
  poll <- pollutant
  dt <- data.table::as.data.table(pred_dt)[pollutant == poll]

  if (nrow(dt) == 0L) {
    stop("No fitted values for pollutant ", pollutant, " in ", city_label, ".")
  }

  # Stations with no prediction at all carry no information for this figure.
  keep <- dt[!is.na(predicted), .N, by = station_id][N > 0, station_id]
  dt <- dt[station_id %in% keep]

  if (!is.null(n_stations)) {
    rank_dt <- dt[!is.na(observed), .N, by = station_id][order(-N)]
    dt <- dt[station_id %in% rank_dt$station_id[seq_len(min(n_stations, nrow(rank_dt)))]]
  }

  p <- ggplot2::ggplot(dt, ggplot2::aes(x = datetime)) +
    ggplot2::geom_line(ggplot2::aes(y = observed), color = obs_color,
                       linewidth = 0.2, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = predicted), color = pred_color,
                       linewidth = 0.25, na.rm = TRUE) +
    ggplot2::facet_wrap(~ station_id, scales = "free_y") +
    ggplot2::labs(
      title = paste0(city_label, ": linear prediction by station"),
      subtitle = paste0(toupper(pollutant),
                        "; observed in grey, prediction in blue"),
      x = NULL, y = paste(toupper(pollutant), "concentration")) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 6),
                   axis.text = ggplot2::element_text(size = 5))

  return(p)
}

# --------------------------------------------------------------------------------------
# Function: summarize_imputation_ratios
# 
# Compute the missing-hour prediction / observed-hour mean ratio for each station.
#' @param pred_dt Fitted values: station_id, pollutant, observed, predicted, was_missing.
#' @param station_dt Station socioeconomic table: station_id and education_mean.
#' @param pollutant Pollutant to summarize, such as "pm10".
#
#' @return data.table with means, missing-hour counts, ratio and education rank.
#' @details Retain stations with missing hours, finite means and positive observed means.
# Missing education is ranked last. No files are written and inputs are not modified.
# 
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------
summarize_imputation_ratios <- function(pred_dt, station_dt, pollutant) {
  # `pollutant` names both the argument and a column; the local keeps them apart.
  poll <- pollutant
  dt <- data.table::as.data.table(pred_dt)[pollutant == poll]

  ratio_dt <- dt[, .(
    mean_predicted_missing = mean(predicted[was_missing], na.rm = TRUE),
    mean_observed = mean(observed, na.rm = TRUE),
    n_missing = sum(was_missing)
  ), by = station_id]

  ratio_dt <- ratio_dt[n_missing > 0 & is.finite(mean_predicted_missing) &
                         is.finite(mean_observed) & mean_observed > 0]

  if (nrow(ratio_dt) == 0L) {
    stop("No station has both filled and observed hours for ", pollutant, ".")
  }

  ratio_dt[, ratio := mean_predicted_missing / mean_observed]

  socio <- data.table::as.data.table(station_dt)[, .(station_id, education_mean)]
  ratio_dt <- merge(ratio_dt, socio, by = "station_id", all.x = TRUE)

  # Order on education, so the x axis is the ranking the caption describes.
  data.table::setorder(ratio_dt, education_mean, na.last = TRUE)
  ratio_dt[, education_rank := seq_len(.N)]

  ratio_dt
}

# --------------------------------------------------------------------------------------
# Function: plot_imputation_ratio_by_station
#
#' @param ratio_dt Station summaries returned by summarize_imputation_ratios().
#' @param pollutant  string; the pollutant to draw.
#' @param city_label string; city name for the panel title.
#' @param point_color string; colour of the station points.
#
#' @return ggplot object; no files are written.
#
#' @details
#   Divide the predicted mean during missing hours by the mean during observed hours.
#   For example, means of 30 and 15 give a ratio of 2. A ratio of 1 means the two means
#   agree; it does not validate the unobserved values. Order stations by nearby education,
#   lowest first, to show how this ratio varies along that ranking.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------
plot_imputation_ratio_by_station <- function(ratio_dt, pollutant, city_label,
                                           point_color = "darkblue") {

  p <- ggplot2::ggplot(ratio_dt,
                       ggplot2::aes(x = education_rank, y = ratio)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    ggplot2::geom_point(color = point_color, size = 2.4) +
    ggplot2::labs(
      title = paste0(city_label, ": predicted missing vs observed means"),
      subtitle = paste0(toupper(pollutant),
                        "; stations ordered from lowest to highest education"),
      x = "Station, ordered by mean years of schooling of its area",
      y = "Mean predicted (missing) / mean observed")

  return(p)
}
