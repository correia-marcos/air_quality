# ============================================================================================
# IDB: Air monitoring — pooled concentration distributions (paper Figure 1)
# ============================================================================================
#' @Goal: Functions for the pooled PM10/PM2.5 concentration figures across the four
#   metropolitan areas.
#
#' @Description: Two views of the same hourly station values: kernel densities with the
#   WHO 24-hour interim targets as dashed reference lines (the paper's Figure 1), and the
#   companion share of station-hours at or above each target. Both share one reader and
#   one threshold lookup, so the two figures always describe the same observations.
#   Sourced by config_utils_plot_tables.R; never sourced directly by a script.
#
#' @Summary:
#   1. .who_24h_targets
#   2. .read_city_values
#   3. plot_kernel_density_by_city
#   4. plot_exceedance_shares
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: .who_24h_targets
#
#' @param pollutant string; "pm10" or "pm25".
#'
#' @return    Named numeric vector c(it1, it2) in µg/m³.
#'
#' @details   WHO 2021 24-hour interim targets — the values the paper's Figure 1 draws.
#'            Same numbers as who_it in process/idw_exposure.R; restated here so the
#'            plot stage does not source a process module.
#'
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
.who_24h_targets <- function(pollutant) {
  tab <- list(pm10 = c(it1 = 150, it2 = 100),
              pm25 = c(it1 = 75, it2 = 50))
  if (!pollutant %in% names(tab)) {
    stop("pollutant must be one of: ", paste(names(tab), collapse = ", "))
  }
  tab[[pollutant]]
}

# --------------------------------------------------------------------------------------------
# Function: .read_city_values
#
#' @param city_data named list; names become legend labels (list order = legend order),
#'                 values are either a path to an Arrow dataset directory (string) or a
#'                 data.frame holding a column named after `pollutant`.
#' @param pollutant string; the column to pull from each element.
#' @param year      integer or NULL; row filter, applied only to dataset-path inputs.
#'
#' @return    data.frame with columns city (factor, list order) and value (numeric,
#'            non-missing values only).
#'
#' @details   One reader for both figure functions, so densities and exceedance shares
#'            are always computed on exactly the same observations. Outlier-flagged
#'            hours are already NA in the metro_clean panels and drop out here.
#'
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
.read_city_values <- function(city_data, pollutant, year = NULL) {

  read_one <- function(x) {
    if (is.character(x)) {
      ds <- arrow::open_dataset(x)
      if (!is.null(year)) ds <- dplyr::filter(ds, year == .env$year)
      out <- dplyr::collect(dplyr::select(ds, dplyr::all_of(pollutant)))
    } else {
      out <- as.data.frame(x)[pollutant]
    }
    as.numeric(out[[pollutant]])
  }

  long <- do.call(rbind, lapply(names(city_data), function(nm) {
    data.frame(city = nm, value = read_one(city_data[[nm]]))
  }))
  long <- long[is.finite(long$value), ]
  long$city <- factor(long$city, levels = names(city_data))
  long
}

# --------------------------------------------------------------------------------------------
# Function: plot_kernel_density_by_city
#
#' @param city_data       named list; same contract as in .read_city_values().
#' @param pollutant       string; "pm10" or "pm25" — the column to plot.
#' @param year            integer or NULL; restrict dataset-path inputs to one year.
#' @param it1_value       numeric; position of the WHO IT1 line in µg/m³. NULL = the WHO
#'                        24-hour default for the pollutant (150 pm10 / 75 pm25).
#' @param it2_value       numeric; position of the WHO IT2 line in µg/m³. NULL = the WHO
#'                        24-hour default (100 pm10 / 50 pm25).
#' @param x_max           numeric or NULL; right edge of the x axis, and the right edge of
#'                        the density evaluation grid. The bandwidth still comes from all
#'                        of a city's values; only the grid is bounded, so the mass beyond
#'                        x_max is estimated but not drawn. NULL uses the data range.
#' @param pollutant_label string; x-axis label. Default "PM10 (µg/m³)" / "PM2.5 (µg/m³)".
#' @param city_colours    named character vector or NULL; city -> colour. NULL = the Set1
#'                        brewer palette. Names must match city_data's names exactly
#'                        (accents included).
#' @param city_linetypes  named character vector or NULL; city -> linetype ("solid",
#'                        "dashed", "dotdash", ...). NULL = every curve solid and no
#'                        linetype scale.
#' @param fill_alpha      numeric in [0, 1]; transparency of the density fill. 0 draws
#'                        line-only curves and line-only legend keys.
#' @param legend_position string; passed to theme(legend.position = ...).
#'
#' @return    A ggplot, invisibly; printed when the session is interactive. Nothing is
#'            written to disk.
#'
#' @details   The paper's Figure 1: one density curve per metropolitan area on a shared
#'            axis with the WHO 24-hour interim targets as dashed lines. The bandwidth
#'            is ggplot2's default (bw.nrd0, Silverman's rule of thumb), the kernel is
#'            Gaussian, and each curve is an unweighted station-hour density: stations
#'            with more valid hours contribute more mass, and every city's curve
#'            integrates to 1. The curves are evaluated with stats::density() on a grid
#'            bounded by x_max rather than by geom_density(), which spans the panel-wide
#'            data range: one 79,999 µg/m³ station-hour makes that range 80,002 wide, and
#'            at a grid spacing ~150x the bandwidth R's FFT convolution stops
#'            renormalising and inflates every curve by two orders of magnitude. Bounding
#'            the grid keeps the spacing near the bandwidth and the y scale correct.
#'            The published figure's look — Santiago red, the other
#'            three black and told apart by linetype, no fill — is a caller choice,
#'            set through city_colours / city_linetypes / fill_alpha in
#'            figure_kernel_distributions.R, not a default of this function.
#'
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_kernel_density_by_city <- function(city_data,
                                        pollutant = c("pm10", "pm25"),
                                        year = NULL,
                                        it1_value = NULL,
                                        it2_value = NULL,
                                        x_max = NULL,
                                        pollutant_label = NULL,
                                        city_colours = NULL,
                                        city_linetypes = NULL,
                                        fill_alpha = 0.3,
                                        legend_position = "top") {
  pollutant <- match.arg(pollutant)
  its <- .who_24h_targets(pollutant)
  if (is.null(it1_value)) it1_value <- unname(its["it1"])
  if (is.null(it2_value)) it2_value <- unname(its["it2"])
  if (is.null(pollutant_label)) {
    pollutant_label <- switch(pollutant,
                              pm10 = "PM10 (µg/m³)",
                              pm25 = "PM2.5 (µg/m³)")
  }

  long <- .read_city_values(city_data, pollutant, year)

  # One density per city, evaluated on [0, x_max] instead of the panel-wide data range.
  # See @details: an unbounded grid inflates the y scale when one far outlier survives.
  dens <- do.call(rbind, lapply(levels(long$city), function(nm) {
    v <- long$value[long$city == nm]
    d <- if (is.null(x_max)) {
      stats::density(v, n = 512)
    } else {
      stats::density(v, from = 0, to = x_max, n = 512)
    }
    data.frame(city = nm, x = d$x, y = d$y)
  }))
  dens$city <- factor(dens$city, levels = levels(long$city))

  # Linetype joins colour as a discriminator only when the caller differentiates cities
  # by line format; both aesthetics map to `city`, so one legend entry shows both.
  city_aes <- if (is.null(city_linetypes)) {
    ggplot2::aes(x = x, y = y, colour = city, fill = city)
  } else {
    ggplot2::aes(x = x, y = y, colour = city, fill = city, linetype = city)
  }

  p <- ggplot2::ggplot(dens, city_aes)
  if (fill_alpha > 0) {
    p <- p + ggplot2::geom_area(alpha = fill_alpha, position = "identity",
                                colour = NA, key_glyph = "polygon")
  }
  p <- p +
    ggplot2::geom_line(linewidth = 1, key_glyph = "path") +
    ggplot2::geom_vline(xintercept = it1_value, linetype = "dashed",
                        colour = "#B2182B", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = it2_value, linetype = "dashed",
                        colour = "#2166AC", linewidth = 0.5) +
    ggplot2::annotate("text", x = it1_value, y = Inf, vjust = 1.2, hjust = -0.1,
                      label = "IT1", colour = "#B2182B", fontface = "bold") +
    ggplot2::annotate("text", x = it2_value, y = Inf, vjust = 1.2, hjust = -0.1,
                      label = "IT2", colour = "#2166AC", fontface = "bold") +
    ggplot2::labs(x = pollutant_label, y = "Density")

  # Scales: Set1 by default, the caller's named colours/linetypes otherwise. Named
  # vectors keep colour assignment fixed while city_data's order sets the legend.
  if (is.null(city_colours)) {
    p <- p + ggplot2::scale_colour_brewer(palette = "Set1") +
      ggplot2::scale_fill_brewer(palette = "Set1")
  } else {
    p <- p + ggplot2::scale_colour_manual(values = city_colours) +
      ggplot2::scale_fill_manual(values = city_colours)
  }
  if (!is.null(city_linetypes)) {
    p <- p + ggplot2::scale_linetype_manual(values = city_linetypes)
  }

  # A line-only curve needs a line-only key, but draw_key_path() inherits alpha = 0
  # and would vanish; the guide overrides the key back to opaque.
  if (fill_alpha == 0) {
    p <- p + ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  }

  p <- p + ggplot2::theme_minimal(base_family = "Palatino", base_size = 14) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = legend_position,
      axis.title = ggplot2::element_text(face = "bold")
    )

  if (!is.null(x_max)) {
    p <- p + ggplot2::coord_cartesian(xlim = c(0, x_max))
  }

  if (interactive()) print(p)
  invisible(p)
}

# --------------------------------------------------------------------------------------------
# Function: plot_exceedance_shares
#
#' @param city_data       named list; same contract as in .read_city_values().
#' @param pollutant       string; "pm10" or "pm25".
#' @param year            integer or NULL; restrict dataset-path inputs to one year.
#' @param it1_value       numeric; NULL = the WHO 24-hour default for the pollutant.
#' @param it2_value       numeric; NULL = the WHO 24-hour default.
#' @param pollutant_label string; panel title. Default "PM10 (µg/m³)" / "PM2.5 (µg/m³)".
#' @param legend_position string; passed to theme(legend.position = ...). Mirrors
#'                        plot_kernel_density_by_city()'s knob so the two figures can
#'                        move their legends together.
#'
#' @return    A ggplot, invisibly; printed when the session is interactive. Nothing is
#'            written to disk.
#'
#' @details   Companion to the density figure: the share of valid station-hours at or
#'            above each WHO 24-hour target, which the density tails show only
#'            qualitatively. Bars reuse the density figure's IT line colours so the two
#'            artefacts read as one family. The shares nest — IT2 is the stricter
#'            target, so share ≥ IT2 is at least share ≥ IT1 — and percentages are
#'            printed on the bars because most cities sit near zero.
#'
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_exceedance_shares <- function(city_data,
                                   pollutant = c("pm10", "pm25"),
                                   year = NULL,
                                   it1_value = NULL,
                                   it2_value = NULL,
                                   pollutant_label = NULL,
                                   legend_position = "top") {
  pollutant <- match.arg(pollutant)
  its <- .who_24h_targets(pollutant)
  if (is.null(it1_value)) it1_value <- unname(its["it1"])
  if (is.null(it2_value)) it2_value <- unname(its["it2"])
  if (is.null(pollutant_label)) {
    pollutant_label <- switch(pollutant,
                              pm10 = "PM10 (µg/m³)",
                              pm25 = "PM2.5 (µg/m³)")
  }

  long <- .read_city_values(city_data, pollutant, year)

  # Per city: share of valid hours at or above each target, in percentage points
  shares <- do.call(rbind, lapply(levels(long$city), function(cty) {
    v <- long$value[long$city == cty]
    data.frame(
      city   = cty,
      target = c(">= IT1", ">= IT2"),
      share  = c(mean(v >= it1_value), mean(v >= it2_value)) * 100
    )
  }))
  shares$target <- factor(shares$target, levels = c(">= IT1", ">= IT2"))

  p <- ggplot2::ggplot(shares, ggplot2::aes(x = city, y = share, fill = target)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.9), width = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", share)),
      position = ggplot2::position_dodge(0.9), vjust = -0.4, size = 3.8) +
    ggplot2::labs(title = pollutant_label,
                  y = "Share of valid station-hours (%)") +
    ggplot2::scale_fill_manual(
      values = c(">= IT1" = "#B2182B", ">= IT2" = "#2166AC"),
      labels = c(sprintf("≥ IT1 (%s)", it1_value),
                 sprintf("≥ IT2 (%s)", it2_value))) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 14) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = legend_position,
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )

  if (interactive()) print(p)
  invisible(p)
}
