# ==========================================================================================
# IDB: Air monitoring — figures of the station sets
# ==========================================================================================
#' @Goal: Map the monitoring stations by what they report, over the education of residents.
#' @Description: The station-set comparison (estimate_idw_station_sets.R) keeps stations
#   by their 2023 reporting. This map shows which stations report each pollutant and
#   whether a record is part-year, over the mean education quintile of each unit's
#   adults. The calling script saves the ggplot object.
#' @Summary:
#   1. plot_station_set_map
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: plot_station_set_map
#
#' @param units    sf polygons with mean_quintile (education-reporting adults, 1 to 5).
#' @param stations sf points with category (factor: what the station reports) and record
#                  (factor: full-year or part-year record).
#' @param title    plot title.
#' @param caption  caption text.
#
#' @return  ggplot: units shaded by mean quintile, stations shaped by category and
#   coloured by record, in the units' coordinate system.
#
#' @details
#   Shape carries the category and colour carries the record, so neither depends on
#   colour alone. Units without education-reporting adults are left blank.
# ------------------------------------------------------------------------------------------
plot_station_set_map <- function(units, stations, title, caption = NULL) {
  stations <- sf::st_transform(stations, sf::st_crs(units))
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = units, ggplot2::aes(fill = mean_quintile), colour = NA) +
    ggplot2::geom_sf(data = stations, ggplot2::aes(shape = category, colour = record),
                     size = 2.2, stroke = .8) +
    ggplot2::scale_fill_gradient(low = "grey96", high = "grey35", limits = c(1, 5),
                                 name = "Mean education\nquintile of adults",
                                 na.value = "white") +
    ggplot2::scale_shape_manual(values = c("Both" = 16, "PM10 only" = 17,
                                           "PM2.5 only" = 15, "Neither" = 4),
                                name = "Reports in 2023") +
    ggplot2::scale_colour_manual(values = c("Three months or more" = "#2a78d6",
                                            "Part-year (under three months)" = "#eb6834"),
                                 name = "Record") +
    ggplot2::labs(title = title, caption = caption) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   plot.caption = ggplot2::element_text(hjust = 0, size = 7))
}
