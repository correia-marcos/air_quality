# ==========================================================================================
# IDB: Air monitoring — figures for the review of the resolution-sensitivity analysis
# ==========================================================================================
#' @Goal: Plot the review tables built by src/general_utilities/process/resolution_review.R.
#' @Description: Each function takes a saved-table shape and returns a ggplot object; the
#   calling script saves it. Education quintiles use one diverging blue-red scale with a
#   grey midpoint (Q1 red, Q5 blue); buffers and groups use two fixed colours.
#' @Summary:
#   1. resolution_quintile_colours
#   2. plot_resolution_unit_composition
#   3. plot_resolution_sample_shares
#   4. plot_resolution_monitoring
#   5. plot_resolution_buffer_gaps
#   6. plot_resolution_grouping_comparison
#' @Date: September 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: resolution_quintile_colours
#
#' @return  named character vector Q1..Q5: red arm, grey midpoint, blue arm.
# ------------------------------------------------------------------------------------------
resolution_quintile_colours <- function() {
  c(Q1 = "#b83a3a", Q2 = "#eb9a97", Q3 = "#d6d5d0", Q4 = "#86b6ef", Q5 = "#256abf")
}

# ------------------------------------------------------------------------------------------
# Function: plot_resolution_unit_composition
#
#' @param composition Output of resolution_unit_composition() with a unit_label column.
#' @param title       Plot title.
#
#' @return  ggplot: one 100% bar per unit, ordered by schooling, faceted by coverage.
#
#' @details
#   Units are ordered by mean schooling (lowest at the bottom). The right-hand label gives
#   education-reporting adults and the share of them inside the Design A sample, so the
#   reader sees which units each design reaches.
# ------------------------------------------------------------------------------------------
plot_resolution_unit_composition <- function(composition, title) {
  x <- data.table::copy(composition)
  x[, quintile := factor(paste0("Q", edu_quintile), levels = paste0("Q", 5:1))]
  order_units <- unique(x[order(education_mean), unit_label])
  x[, unit_label := factor(unit_label, levels = order_units)]
  status <- c("Comuna point within 3 km (native B)",
              "Some zonas covered (A), comuna point not", "No covered zona")
  x[, coverage := factor(data.table::fifelse(b_covered, status[1],
    data.table::fifelse(a_share > 0, status[2], status[3])), levels = status)]
  labels <- unique(x[, .(unit_label, coverage, text = paste0(
    format(round(unit_population / 1000), big.mark = ","), "k; A ",
    round(100 * a_share), "%"))])
  ggplot2::ggplot(x, ggplot2::aes(share, unit_label, fill = quintile)) +
    ggplot2::geom_col(width = .8, colour = "white", linewidth = .3) +
    ggplot2::geom_text(data = labels,
                       ggplot2::aes(x = 1.02, y = unit_label, label = text),
                       inherit.aes = FALSE, hjust = 0, size = 2.3, colour = "grey30") +
    ggplot2::facet_grid(coverage ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(values = resolution_quintile_colours(), name = NULL,
                               breaks = paste0("Q", 1:5)) +
    ggplot2::scale_x_continuous(labels = function(v) paste0(100 * v, "%"),
                                breaks = c(0, .5, 1),
                                expand = ggplot2::expansion(mult = c(0, .32))) +
    ggplot2::labs(title = title, x = "Share of education-reporting adults", y = NULL,
      caption = paste("Individual city-wide education quintiles (Q1 lowest).",
        "Units ordered by mean years of schooling, lowest at the bottom.",
        "\nLabel: education-reporting adults; share of them in the 3 km",
        "Design A sample. Quintile cuts inside tied schooling values follow",
        "\ngeographic-code order, so Q2-Q4 blocks partly track comuna codes.")) +
    ggplot2::theme(legend.position = "top", panel.grid = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, size = 8),
      axis.text.y = ggplot2::element_text(size = 7),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}

# ------------------------------------------------------------------------------------------
# Function: plot_resolution_sample_shares
#
#' @param shares Output of resolution_sample_shares() with buffer and sample_label columns.
#' @param title  Plot title.
#
#' @return  ggplot: one 100% bar per estimation sample, faceted by buffer.
# ------------------------------------------------------------------------------------------
plot_resolution_sample_shares <- function(shares, title) {
  x <- data.table::melt(shares, measure.vars = paste0("Q", 1:5),
                        variable.name = "quintile", value.name = "share")
  x[, quintile := factor(quintile, levels = paste0("Q", 5:1))]
  x[, sample_label := factor(sample_label, levels = rev(unique(shares$sample_label)))]
  ggplot2::ggplot(x, ggplot2::aes(share, sample_label, fill = quintile)) +
    ggplot2::geom_col(width = .75, colour = "white", linewidth = .3) +
    ggplot2::geom_vline(xintercept = c(.2, .4, .6, .8), colour = "grey40",
                        linewidth = .2, linetype = "dotted") +
    ggplot2::facet_wrap(ggplot2::vars(buffer), ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(values = resolution_quintile_colours(), name = NULL,
                               breaks = paste0("Q", 1:5)) +
    ggplot2::scale_x_continuous(labels = function(v) paste0(100 * v, "%"),
                                expand = ggplot2::expansion(mult = c(0, .03))) +
    ggplot2::labs(title = title, x = "Share of the sample's education-reporting adults",
      y = NULL, caption = paste("Dotted lines mark equal 20% quintile shares.",
        "Design C bars show area quintiles, not individual quintiles.")) +
    ggplot2::theme(legend.position = "top", panel.grid = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}

# ------------------------------------------------------------------------------------------
# Function: plot_resolution_monitoring
#
#' @param summary Monitoring summary with city_label, level_label (ordered factor),
#'   group and weighted nearest-distance quantiles.
#' @param title   Plot title.
#
#' @return  ggplot: median and p10-p90 nearest active-station distance for Q1 and Q5.
# ------------------------------------------------------------------------------------------
plot_resolution_monitoring <- function(summary, title) {
  x <- summary[group %in% c("Q1", "Q5")]
  ggplot2::ggplot(x, ggplot2::aes(y = level_label, colour = group)) +
    ggplot2::geom_vline(xintercept = 3, colour = "grey60", linewidth = .3,
                        linetype = "dashed") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = nearest_p10_km, xmax = nearest_p90_km),
      linewidth = .6, position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_point(ggplot2::aes(x = nearest_median_km), size = 2.2,
                        position = ggplot2::position_dodge(width = .5)) +
    ggplot2::facet_wrap(ggplot2::vars(city_label), ncol = 1, scales = "free_y") +
    ggplot2::scale_colour_manual(values = resolution_quintile_colours()[c("Q1", "Q5")],
      labels = c(Q1 = "Q1 (lowest education)", Q5 = "Q5 (highest education)"),
      name = NULL) +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::labs(title = title, y = NULL,
      x = paste("Distance from the unit's representative point to the nearest",
                "active station (km, log scale)"),
      caption = paste("Point: population-weighted median; bar: 10th-90th percentile.",
        "Dashed line: 3 km. Weights are education-reporting adults of each quintile.",
        "\nAll units with adults are included, whether or not they have an exposure",
        "estimate. Mexico City has one support (63 municipalities).")) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}

# ------------------------------------------------------------------------------------------
# Function: plot_resolution_buffer_gaps
#
#' @param comparison Long table: panel, level_label (ordered), buffer, gap, population.
#' @param title      Plot title.
#' @param gap_label  Headline contrast on the y axis, e.g. "Q1 - Q5".
#
#' @return  ggplot: native-B headline gap by support under the two buffers.
#
#' @details
#   Panels have free scales because units differ by outcome and supports differ by city.
#   Labels give the native sample in millions of education-reporting adults: 3 km above
#   the point, 20 km below.
# ------------------------------------------------------------------------------------------
plot_resolution_buffer_gaps <- function(comparison, title, gap_label = "Q1 - Q5") {
  ggplot2::ggplot(comparison, ggplot2::aes(level_label, gap, colour = buffer,
                                           group = buffer)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = .3) +
    ggplot2::geom_line(linewidth = .5, na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("N=", format(round(population / 1e6, 2),
                                                                nsmall = 2), "m"),
                                    vjust = ifelse(buffer == "3 km", -1, 2)),
                       size = 2.1, show.legend = FALSE, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(panel), scales = "free", ncol = 3) +
    ggplot2::scale_colour_manual(values = c("3 km" = "#2a78d6", "20 km" = "#eb6834"),
                                 name = "IDW buffer") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.15, .25))) +
    ggplot2::labs(title = title, x = "Geographic support (coarse to fine)",
      y = paste(gap_label, "(outcome units)"),
      caption = paste("Native Design B: exposure rebuilt at each support. Point labels",
        "(N) are sample sizes, not gaps: millions of education-reporting adults in covered",
        "units.",
        "\nAvg: concentration units; IT1/IT2: hours at or above the threshold in 2023.")) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 7, angle = 30, hjust = 1),
      strip.text = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}

# ------------------------------------------------------------------------------------------
# Function: plot_resolution_grouping_comparison
#
#' @param comparison Long table: panel, level_label (ordered), definition (ordered), gap.
#' @param title      Plot title.
#
#' @return  ggplot: Design A headline gap by support under each education definition.
#
#' @details
#   One colour and one point shape per definition, in the fixed order of the factor, so
#   identity never depends on colour alone. Gaps share the outcome's units across
#   definitions, but each definition compares a different part of the population.
# ------------------------------------------------------------------------------------------
plot_resolution_grouping_comparison <- function(comparison, title) {
  colours <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
  ggplot2::ggplot(comparison, ggplot2::aes(level_label, gap, colour = definition,
                                           shape = definition, group = definition)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = .3) +
    ggplot2::geom_line(linewidth = .5, na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(panel), scales = "free", ncol = 3) +
    ggplot2::scale_colour_manual(values = colours, name = NULL, drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 18), name = NULL, drop = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.15, .2))) +
    ggplot2::labs(title = title, x = "Geographic support (coarse to fine)",
      y = "Lowest minus highest education group (outcome units)",
      caption = paste("Design A: fixed people, exposure aggregated by support.",
        "Each definition compares a different share of adults; see the comparison table.",
        "\nAvg: concentration units; IT1/IT2: hours at or above the threshold in 2023.")) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 7, angle = 30, hjust = 1),
      strip.text = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}
