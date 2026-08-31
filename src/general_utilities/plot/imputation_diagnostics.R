# ============================================================================================
# IDB: Air monitoring — imputation diagnostics figures
# ============================================================================================
#' @Goal: Functions for the figures that show what the hourly imputation predicted.
#
#' @Description: Two views of the fitted values written by impute_missing_hourly.R. The
#   first puts the prediction against the observed series, station by station, to show
#   that the model tracks the data. The second asks whether the hours the model had to
#   fill were unusual ones, and whether that differs by the education of the area a
#   station sits in. Sourced by config_utils_plot_tables.R; never sourced directly.
#
#' @Summary:
#   1. plot_imputation_series
#   2. plot_imputation_ratio_by_station
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------
# Function: plot_imputation_series
#
#' @param pred_dt    data.table; fitted values from impute_missing_hourly_ols(), with
#                    datetime, station_id, pollutant, observed, predicted, was_missing.
#' @param pollutant  string; the pollutant to draw, "pm10" or "pm25".
#' @param city_label string; city name for the panel title.
#' @param out_file   string; where to write the figure.
#' @param n_stations integer or NULL; draw only this many stations, the ones with the
#                    most observed readings. NULL draws all of them.
#' @param obs_color  string; colour of the observed series.
#' @param pred_color string; colour of the fitted series.
#' @param width      numeric; figure width in inches.
#' @param height     numeric; figure height in inches.
#' @param dpi        numeric; raster resolution.
#
#' @return  ggplot object; also written to out_file.
#
#' @details
#   One small panel per station, observed in dark grey and the linear prediction over it.
#   The prediction is drawn at every hour, including the hours the station reported, which
#   is the point: a model that only appeared where data was missing could not be judged.
#   Panels are free-scaled, because a station's level says nothing about whether its own
#   series is tracked.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------
plot_imputation_series <- function(
    pred_dt,
    pollutant,
    city_label,
    out_file,
    n_stations  = NULL,
    obs_color   = "grey25",
    pred_color  = "#4C9BE8",
    width       = 12,
    height      = 8,
    dpi         = 300
) {

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

  ggplot2::ggsave(filename = out_file, plot = p, width = width, height = height,
                  dpi = dpi, device = grDevices::cairo_pdf, limitsize = FALSE,
                  bg = "white")

  return(p)
}


# --------------------------------------------------------------------------------------
# Function: plot_imputation_ratio_by_station
#
#' @param pred_dt    data.table; fitted values, as for plot_imputation_series().
#' @param station_dt data.table; station-level socioeconomic data carrying station_id
#                    and education_mean.
#' @param pollutant  string; the pollutant to draw.
#' @param city_label string; city name for the panel title.
#' @param out_file   string; where to write the figure.
#' @param point_color string; colour of the station points.
#' @param width      numeric; figure width in inches.
#' @param height     numeric; figure height in inches.
#' @param dpi        numeric; raster resolution.
#
#' @return  ggplot object; also written to out_file.
#
#' @details
#   For each station, the mean prediction over the hours it did not report, divided by the
#   mean of the hours it did. A value of one says the missing hours looked like the
#   observed ones. Stations are ordered by the mean years of schooling of the area they
#   sit in, lowest first, so that a systematic relationship between missingness and
#   education would appear as a trend rather than as scatter.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------
plot_imputation_ratio_by_station <- function(
    pred_dt,
    station_dt,
    pollutant,
    city_label,
    out_file,
    point_color = "darkblue",
    width       = 8.5,
    height      = 5.8,
    dpi         = 300
) {

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

  ggplot2::ggsave(filename = out_file, plot = p, width = width, height = height,
                  dpi = dpi, device = grDevices::cairo_pdf, limitsize = FALSE,
                  bg = "white")

  return(p)
}
