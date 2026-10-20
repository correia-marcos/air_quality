# ==========================================================================================
# IDB: Air monitoring — figures of the Design A buffer x exponent grid
# ==========================================================================================
#' @Goal: Plot the Design A grid tables built by estimate_design_a_grid.R.
#' @Description: Each function takes a saved-table shape and returns a ggplot object; the
#   calling script saves it. Categorical colours follow one fixed order (blue, orange,
#   aqua, yellow, magenta), and every series also has its own point shape or line type,
#   so identity never depends on colour alone. Panels have free scales because units
#   differ by outcome.
#' @Summary:
#   1. design_a_panel_labels
#   2. plot_design_a_gap_by_buffer
#   3. plot_design_a_samples
#   4. plot_design_a_decomposition
#   5. plot_variance_shares
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

design_a_colours <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")

# ------------------------------------------------------------------------------------------
# Function: design_a_panel_labels
#
#' @param x           data.table with city_id and outcome.
#' @param city_labels named character; city names for the panel titles.
#
#' @return  x with panel ("<City>: annual mean | hours >= IT1 | hours >= IT2") ordered by
#   city, then outcome. Labelling only; values are unchanged.
# ------------------------------------------------------------------------------------------
design_a_panel_labels <- function(x, city_labels) {
  x <- data.table::copy(x)
  metrics <- c("annual mean", "hours >= IT1", "hours >= IT2")
  metric <- data.table::fcase(grepl("^avg", x$outcome), metrics[1],
                              grepl("it1$", x$outcome), metrics[2],
                              grepl("it2$", x$outcome), metrics[3])
  panels <- as.vector(t(outer(city_labels, metrics, paste, sep = ": ")))
  x[, panel := factor(paste0(city_labels[city_id], ": ", metric), levels = panels)]
  x[]
}

# ------------------------------------------------------------------------------------------
# Function: plot_design_a_gap_by_buffer
#
#' @param x         data.table: panel, buffer_km, gap and the series column.
#' @param series    string; column that identifies the lines (e.g. exponent or support).
#' @param title     plot title.
#' @param gap_label headline contrast on the y axis, e.g. "Q1 - Q5".
#' @param legend    legend title.
#' @param caption   caption text.
#
#' @return  ggplot: Design A gap against the IDW buffer, one line per series, one panel
#   per city and outcome.
# ------------------------------------------------------------------------------------------
plot_design_a_gap_by_buffer <- function(x, series, title, gap_label = "Q1 - Q5",
                                        legend = NULL, caption = NULL) {
  ggplot2::ggplot(x, ggplot2::aes(buffer_km, gap, colour = .data[[series]],
                                  shape = .data[[series]], group = .data[[series]])) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = .3) +
    ggplot2::geom_vline(xintercept = 3, colour = "grey80", linewidth = .3,
                        linetype = "dashed") +
    ggplot2::geom_line(linewidth = .5, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(panel), scales = "free_y", ncol = 3) +
    ggplot2::scale_colour_manual(values = design_a_colours, name = legend) +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 4), name = legend) +
    ggplot2::scale_x_continuous(breaks = c(1, 3, 5, 10, 15, 20)) +
    ggplot2::labs(title = title, x = "IDW buffer (km)",
                  y = paste(gap_label, "(outcome units)"), caption = caption) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}

# ------------------------------------------------------------------------------------------
# Function: plot_design_a_samples
#
#' @param x     long data.table: city, pollutant, buffer_km, measure (ordered) and value.
#' @param title plot title.
#
#' @return  ggplot: sample and station-weight descriptors against the buffer, one row of
#   panels per measure and one column per city; colour and line type give the pollutant.
# ------------------------------------------------------------------------------------------
plot_design_a_samples <- function(x, title) {
  ggplot2::ggplot(x, ggplot2::aes(buffer_km, value, colour = pollutant,
                                  linetype = pollutant, group = pollutant)) +
    ggplot2::geom_vline(xintercept = 3, colour = "grey80", linewidth = .3,
                        linetype = "dashed") +
    ggplot2::geom_line(linewidth = .5, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.2, na.rm = TRUE) +
    ggplot2::facet_grid(ggplot2::vars(measure), ggplot2::vars(city), scales = "free_y",
                        switch = "y") +
    ggplot2::scale_colour_manual(values = design_a_colours[1:2], name = NULL) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    ggplot2::scale_x_continuous(breaks = c(1, 3, 5, 10, 15, 20)) +
    ggplot2::labs(title = title, x = "IDW buffer (km)", y = NULL,
      caption = paste("Shares and means weight education-reporting adults in the",
                      "Design A sample. The sample does not depend on the exponent;",
                      "station weights are shown for exponent 1.")) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      strip.placement = "outside", strip.text = ggplot2::element_text(size = 7),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}

# ------------------------------------------------------------------------------------------
# Function: plot_design_a_decomposition
#
#' @param x         long data.table: panel, buffer_km, component (ordered), value.
#' @param title     plot title.
#' @param gap_label headline contrast, e.g. "Q1 - Q5".
#
#' @return  ggplot: change in the gap from the reference (3 km, exponent 1) against the
#   buffer, split into the exposure change on fixed people and the sample change.
#
#' @details
#   The total is the sum of the two parts (design_a_decomposition()). Components use a
#   fixed colour and line type each; the total is the solid dark line.
# ------------------------------------------------------------------------------------------
plot_design_a_decomposition <- function(x, title, gap_label = "Q1 - Q5") {
  ggplot2::ggplot(x, ggplot2::aes(buffer_km, value, colour = component,
                                  linetype = component, group = component)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = .3) +
    ggplot2::geom_line(linewidth = .5, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.2, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(panel), scales = "free_y", ncol = 3) +
    ggplot2::scale_colour_manual(values = c("grey15", design_a_colours[1:2]), name = NULL) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotdash"), name = NULL) +
    ggplot2::scale_x_continuous(breaks = c(1, 3, 5, 10, 15, 20)) +
    ggplot2::labs(title = title, x = "IDW buffer (km)",
      y = paste("Change in", gap_label, "from 3 km (outcome units)"),
      caption = paste("Finest support, exponent 1. Exposure change: both exposures on",
        "the adults covered by both buffers. Sample change: adults added (b > 3) or",
        "removed (b < 3), measured with the larger buffer's exposure (or the reference's).",
        "\nThe two parts add to the total exactly.")) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}

# ------------------------------------------------------------------------------------------
# Function: plot_variance_shares
#
#' @param x     data.table: city, outcome_label (ordered), choice (ordered), share_type
#'   ("First-order" or "Total"), share.
#' @param title plot title.
#
#' @return  ggplot: share of the variance of the gap attributable to each choice, by
#   outcome; first-order shares filled, total shares (with interactions) as outlines.
# ------------------------------------------------------------------------------------------
plot_variance_shares <- function(x, title) {
  ggplot2::ggplot(x, ggplot2::aes(outcome_label, share, fill = choice)) +
    ggplot2::geom_col(data = x[share_type == "Total"], ggplot2::aes(group = choice),
                      position = ggplot2::position_dodge(width = .8), width = .75,
                      fill = NA, colour = "grey35", linewidth = .3) +
    ggplot2::geom_col(data = x[share_type == "First-order"],
                      position = ggplot2::position_dodge(width = .8), width = .75,
                      colour = "white", linewidth = .25) +
    ggplot2::facet_wrap(ggplot2::vars(city), ncol = 1) +
    ggplot2::scale_fill_manual(values = design_a_colours[1:4], name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(v) paste0(100 * v, "%"),
                                expand = ggplot2::expansion(mult = c(0, .02))) +
    ggplot2::labs(title = title, x = NULL, y = "Share of the variance of the gap",
      caption = paste("Filled bars: first-order (main-effect) share of each choice.",
        "Outlines: total share, adding every interaction that involves the choice.",
        "\nExact decomposition of the complete Design A grid; each chosen value weighs",
        "equally, so shares describe this grid of choices.")) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 7, angle = 25, hjust = 1),
      plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}
