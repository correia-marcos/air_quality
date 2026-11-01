# ==========================================================================================
# IDB: Air monitoring — figures of the A/B/C robustness designs
# ==========================================================================================
#' @Goal: Read one specification's saved city tables and plot its review packet.
#' @Description: Reads completed three-city products, separates designs A/B/C, shows
#   monitoring diagnostics and valid paired differences. Plotting never recomputes
#   estimates and never exports to the manuscript.
#' @Summary:
#   1. resolution_figure_tables
#   2. plot_resolution_designs
#   3. plot_resolution_monitoring
#   4. save_resolution_figures
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: resolution_figure_tables
#
#' @param specification list; from resolution_specification().
#
#' @return  list: levels (each city's supports, with axis ids), axis_order, axis_labels,
#   objects (contrasts, profiles, differences, variances and matrices of all cities, with
#   city, pollutant and axis factors) and matrix_units (unit-level matrix diagnostics).
#
#' @details
#   Every city run must be complete. Supports are ordered coarse to fine within each
#   city; Bogota's locality is a separate branch.
# ------------------------------------------------------------------------------------------
resolution_figure_tables <- function(specification) {
  cities <- specification$cities
  city_labels <- c(bogota_2018 = "Bogota", santiago_2017 = "Santiago",
                   sao_paulo_2010 = "Sao Paulo")[cities]
  dir_in <- specification$processed_root
  for (city in cities) {
    if (!identical(readLines(file.path(dir_in, city, "STATUS.txt"))[1], "complete")) {
      stop("Incomplete resolution run: ", city)
    }
  }
  levels <- rbindlist(lapply(cities, function(city) {
    x <- fread(file.path(specification$frozen_root, city, "levels.csv"))
    x[, city_id := city]
    x
  }))
  levels[, axis_id := paste(city_id, level, sep = "/")]
  axis_order <- levels[order(city_id, -fine_to_coarse), axis_id]
  axis_labels <- setNames(levels$label, levels$axis_id)
  objects <- list()
  for (name in c("contrasts", "profiles", "differences", "variances", "matrices")) {
    x <- rbindlist(lapply(cities, function(city) {
      z <- fread(file.path(dir_in, city, paste0(name, ".csv")))
      z[, city_id := city]
      z
    }), fill = TRUE)
    x[, city := factor(city_labels[city_id], levels = unname(city_labels))]
    if (!"pollutant" %in% names(x)) {
      x[, pollutant := ifelse(grepl("pm25", outcome), "PM2.5", "PM10")]
    } else x[, pollutant := ifelse(pollutant == "pm25", "PM2.5", "PM10")]
    x[, axis := factor(paste(city_id, level, sep = "/"), levels = axis_order)]
    objects[[name]] <- x
  }
  matrix_units <- rbindlist(lapply(cities, function(city) {
    rbindlist(lapply(levels[city_id == city, level], function(lv) {
      rbindlist(lapply(c("pm10", "pm25"), function(poll) {
        z <- as.data.table(arrow::read_parquet(file.path(dir_in, city, "B", lv,
          paste0(poll, "_matrix_units.parquet"))))
        z[, city_id := city]
        z
      }))
    }))
  }), fill = TRUE)
  matrix_units[, city := factor(city_labels[city_id], levels = unname(city_labels))]
  matrix_units[, axis := factor(paste(city_id, level, sep = "/"), levels = axis_order)]
  matrix_units[, eligible_stations := as.numeric(eligible_stations)]
  list(levels = levels, axis_order = axis_order, axis_labels = axis_labels,
       objects = objects, matrix_units = matrix_units)
}

# ------------------------------------------------------------------------------------------
# Function: plot_resolution_designs
#
#' @param tables       list; from resolution_figure_tables().
#' @param group_col    string; profile group column, the specification's grouping.
#' @param group_labels character; axis labels of the groups, lowest first.
#' @param group_name   string; plural group noun in captions ("quintiles", "levels").
#' @param gap_label    string; headline contrast, e.g. "Q1 - Q5".
#' @param profile_note string; caption of the A and B profile figures.
#
#' @return  named list of 12 ggplots: A and B gaps for avg, it1 and it2; the Bogota
#   locality branch; A and common-B paired differences; A, native-B and C profiles.
# ------------------------------------------------------------------------------------------
plot_resolution_designs <- function(tables, group_col, group_labels, group_name,
                                    gap_label, profile_note) {
  objects <- tables$objects
  axis_labels <- tables$axis_labels
  plots <- list()
  notes_a <- paste(paste0("Fixed education-reporting adults and individual ",
    group_name, "."),
    "Exposure aggregated with all-adult weights; locality branch shown separately.")
  notes_b <- paste("Native and common samples vary by level;",
    "P = education-reporting adults.",
    "\nCommon samples are pairwise intersections. Paired changes are shown separately.")
  for (design in c("A", "B")) for (outcome_type in c("avg", "it1", "it2")) {
    selected_designs <- if (design == "A") "A" else c("B_native", "B_common")
    x <- objects$contrasts[design %chin% selected_designs]
    x <- x[grepl(if (outcome_type == "avg") "^avg" else paste0(outcome_type, "$"),
      outcome)]
    x <- x[!(city_id == "bogota_2018" & level == "localidad")]
    if (design == "A") x[, point_label := paste0("n=", n_clusters)] else {
      x[, point_label := paste0("n=", n_clusters, "; P=",
        format(round(population / 1e6, 2), nsmall = 2), "m")]
    }
    x[, series := factor(design, levels = c("A", "B_native", "B_common"),
      labels = c("Fixed population", "Native B sample", "Pairwise common sample"))]
    p <- ggplot(x, aes(axis, gap, group = series, colour = series, shape = series)) +
      geom_hline(yintercept = 0, colour = "grey70", linewidth = .3) +
      geom_line(linewidth = .4, na.rm = TRUE) + geom_point(size = 2, na.rm = TRUE) +
      geom_text(aes(label = point_label), vjust = -1,
                size = 2.4, show.legend = FALSE, check_overlap = TRUE, na.rm = TRUE) +
      facet_wrap(vars(city, pollutant), ncol = 2, scales = "free") +
      scale_x_discrete(labels = axis_labels) +
      scale_colour_manual(values = c("Fixed population" = "#263746",
        "Native B sample" = "#263746", "Pairwise common sample" = "#9B6137")) +
      scale_y_continuous(expand = expansion(mult = c(.12, .23))) +
      labs(title = paste("Design", design, "-", if (outcome_type == "avg")
        "annual mean exposure" else paste("annual hours at or above",
          toupper(outcome_type))),
        x = "Geographic support (coarse to fine)",
        y = if (outcome_type == "avg") {
          paste(gap_label, "(micrograms per cubic metre)")
        } else paste(gap_label, "(hours)"), colour = NULL, shape = NULL,
        caption = if (design == "A") notes_a else notes_b) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 8),
          strip.text = element_text(face = "bold"))
    plots[[paste(design, outcome_type, sep = "_")]] <- p
  }
  # The parallel Bogota locality branch is never drawn as a nested sector-locality path.
  x <- objects$contrasts[city_id == "bogota_2018" & level %chin% c("fine", "localidad",
    "municipio") & grepl("^avg", outcome) & design %chin% c("A", "B_native", "B_common")]
  plots$bogota_locality <- ggplot(x, aes(axis, gap, group = design, colour = design)) +
    geom_hline(yintercept = 0, colour = "grey70") + geom_line(linewidth = .4) +
    geom_point(size = 2) + facet_grid(design ~ pollutant, scales = "free_y",
      labeller = labeller(design = c(A = "Design A", B_native = "Native B",
                                     B_common = "Common B"))) +
    scale_colour_manual(values = c(A = "#263746", B_native = "#9B6137",
                                   B_common = "#718355")) +
    scale_x_discrete(labels = axis_labels, drop = TRUE) +
    labs(title = "Bogota: separate locality branch",
      x = "Geographic support (coarse to fine)",
         y = paste(gap_label, "(micrograms per cubic metre)"),
           caption = "Descriptive comparisons.") +
    theme(legend.position = "none", panel.grid.minor = element_blank())
  for (design_name in c("A", "B_common")) {
    x <- objects$differences[design == design_name & grepl("^avg", outcome) &
                              !(city_id == "bogota_2018" & level == "localidad")]
    plots[[paste0(design_name, "_differences")]] <-
      ggplot(x, aes(axis, delta, group = 1)) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = .4) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = .12, na.rm = TRUE) +
      geom_line(linewidth = .4) + geom_point(size = 2) +
      facet_wrap(vars(city, pollutant), ncol = 2, scales = "free") +
      scale_x_discrete(labels = axis_labels) +
      labs(title = paste(if (design_name == "A") "Design A" else "Common-sample B",
                      "- change relative to finest support"),
        x = "Geographic support (coarse to fine)",
        y = paste("Change in absolute", gap_label, "gap"),
        caption = paste("95% conditional geographic-cluster bootstrap intervals;",
          "999 draws.",
          "Coarse comparisons are descriptive.",
          "B endpoints use the same pairwise population.")) +
      theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8,
        hjust = 0))
  }
  for (design_name in c("A", "B_native", "C")) {
    x <- objects$profiles[design == design_name & grepl("^avg", outcome)]
    x <- merge(x, tables$levels[, .(city_id, level, label)], by = c("city_id", "level"))
    plots[[paste0(design_name, "_profiles")]] <-
      ggplot(x, aes(.data[[group_col]], mean, colour = label, group = label)) +
      geom_line(linewidth = .5, na.rm = TRUE) + geom_point(size = 1.6, na.rm = TRUE) +
      facet_wrap(vars(city, pollutant), ncol = 2, scales = "free_y") +
      scale_x_continuous(breaks = seq_along(group_labels), labels = group_labels) +
      scale_colour_manual(values = c("Fine census unit" = "#263746",
        "Zona censal" = "#263746", "Area de ponderacao" = "#263746",
        "Seccion" = "#527D93", "Distrito censal" = "#527D93", "Sector" = "#9B6137",
        "Comuna" = "#9B6137", "Localidad/municipio" = "#718355",
          "Municipio" = "#A06C7D")) +
      labs(title = paste(if (design_name == "B_native") "Native B" else design_name,
                           "- full education-group profiles"),
        x = if (design_name == "C") "Area education group" else
          paste("Individual education", sub("s$", "", group_name)),
        y = "Mean exposure (micrograms per cubic metre)", colour = "Geographic support",
        caption = if (design_name == "C") paste(
          "Classification only; finest exposure fixed.",
          "Empty area groups remain unavailable.",
          paste0("These groups differ from A/B ", group_name, ".")) else
          profile_note) +
      theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8),
            panel.grid.minor = element_blank())
  }
  plots
}

# ------------------------------------------------------------------------------------------
# Function: plot_resolution_monitoring
#
#' @param tables    list; from resolution_figure_tables().
#' @param gap_label string; headline contrast, e.g. "Q1 - Q5".
#
#' @return  named list of 4 ggplots: matrix summary, matrix distributions over populated
#   units, covered-unit counts against the A gap, and the A variance retained.
# ------------------------------------------------------------------------------------------
plot_resolution_monitoring <- function(tables, gap_label) {
  objects <- tables$objects
  axis_labels <- tables$axis_labels
  plots <- list()
  x <- objects$matrices[!(city_id == "bogota_2018" & level == "localidad")]
  m <- melt(x, id.vars = c("city", "pollutant", "axis"),
    measure.vars = c("adult_coverage_share", "median_nearest_active_km",
                     "mean_eligible_stations", "mean_hourly_neff"))
  metric_labels <- c(adult_coverage_share = "Adult population covered\n(share)",
    median_nearest_active_km = "Nearest active station\n(median km)",
    mean_eligible_stations = "Eligible stations (mean)",
    mean_hourly_neff = "Effective stations\n(hourly mean)",
    nearest_active_km = "Nearest active station (km)",
    eligible_stations = "Eligible stations", static_neff = "Effective stations\n(static)",
    hourly_neff_mean = "Effective stations\n(hourly)")
  m[, variable := factor(variable, levels = names(metric_labels), labels = metric_labels)]
  plots$matrix_summary <- ggplot(m, aes(axis, value, colour = pollutant,
    group = pollutant)) +
    geom_line(linewidth = .4) + geom_point(size = 1.8) +
    scale_colour_manual(values = c(PM10 = "#263746", `PM2.5` = "#9B6137")) +
    facet_wrap(vars(city, variable), ncol = 4, scales = "free") +
    scale_x_discrete(labels = axis_labels) +
    labs(title = "Monitoring-matrix sensitivity under Design B", x = "Coarse to fine",
         y = NULL, colour = NULL,
         caption = paste("Catalog, active, eligible and contributing",
           "station counts are distinct in the tables.")) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1,
      size = 8),
          strip.text = element_text(size = 8), plot.caption = element_text(size = 8,
            hjust = 0))
  # Unit-level distributions expose coverage and station concentration hidden by averages.
  m <- melt(tables$matrix_units[adult_population > 0],
    id.vars = c("city", "pollutant", "axis"),
    measure.vars = c("nearest_active_km", "eligible_stations", "static_neff",
                     "hourly_neff_mean"))
  m[, variable := factor(variable, levels = names(metric_labels), labels = metric_labels)]
  plots$matrix_distributions <- ggplot(m, aes(axis, value, fill = pollutant)) +
    geom_boxplot(outlier.shape = NA, linewidth = .3, na.rm = TRUE) +
    scale_fill_manual(values = c(pm10 = "#7C94A4", pm25 = "#BA926F")) +
    facet_wrap(vars(city, variable), ncol = 4, scales = "free") +
    scale_x_discrete(labels = axis_labels) +
    labs(title = "Monitoring-matrix distributions across populated units",
      x = "Coarse to fine",
         y = NULL, fill = NULL,
         caption = paste("Boxes: unweighted unit quartiles; whiskers: 1.5 IQR.",
           "Outliers omitted from view only.")) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1,
      size = 8),
          strip.text = element_text(size = 8), plot.caption = element_text(size = 8,
            hjust = 0))
  x <- objects$contrasts[design == "A" & grepl("^avg", outcome)]
  plots$covered_units <- ggplot(x, aes(n_clusters, gap, colour = city, group = city)) +
    geom_point(size = 2) + geom_text(aes(label = level), size = 2.5, vjust = -1,
                                   check_overlap = TRUE) +
    scale_x_log10() + facet_wrap(vars(pollutant), scales = "free_y") +
    scale_colour_manual(values = c(Bogota = "#263746", Santiago = "#9B6137",
                                   `Sao Paulo` = "#718355")) +
    labs(title = "Design A: covered-unit counts as a supplementary resolution measure",
         x = "Covered estimation units (log scale; coarse to fine)",
         y = gap_label,
           colour = NULL,
         caption = paste(
           "Unit counts do not harmonize geographic concepts across cities.",
           "No pooled estimate.")) +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8))
  x <- objects$variances[grepl("^avg",
    outcome) & !(city_id == "bogota_2018" & level == "localidad")]
  plots$variance <- ggplot(x, aes(axis, share_retained, group = 1)) +
    geom_line(linewidth = .4) + geom_point(size = 2) +
    facet_wrap(vars(city, pollutant), ncol = 2, scales = "free_x") +
    scale_x_discrete(labels = axis_labels) +
    labs(title = "Design A: exposure variance retained", x = "Coarse to fine",
         y = "Share of finest-level variance",
         caption = paste("All-adult aggregation weights and fixed covered support.",
           "Declining variance does not imply a monotone gap.")) +
    theme(plot.caption = element_text(hjust = 0, size = 8))
  plots
}

# ------------------------------------------------------------------------------------------
# Function: save_resolution_figures
#
#' @param plots         named list of ggplots, in packet order.
#' @param specification list; from resolution_specification().
#
#' @return  character vector of the written files, invisibly.
#
#' @details
#   Writes one vector PDF per plot (<prefix><name>.pdf) and the review packet
#   (<prefix without its last underscore>.pdf) in the figure folder, and the figure index
#   (<prefix>figure_index.csv) in the table folder. Never exports to the manuscript.
# ------------------------------------------------------------------------------------------
save_resolution_figures <- function(plots, specification) {
  dir_out <- specification$figure_dir
  prefix <- specification$prefix
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  dir.create(specification$table_dir, recursive = TRUE, showWarnings = FALSE)
  files <- file.path(dir_out, paste0(prefix, names(plots), ".pdf"))
  for (i in seq_along(plots)) {
    ggsave(files[i], plots[[i]],
           width = 11.7, height = 8.3, units = "in", device = grDevices::cairo_pdf)
  }
  packet <- file.path(dir_out, paste0(sub("_$", "", prefix), ".pdf"))
  grDevices::cairo_pdf(packet, width = 11.7, height = 8.3)
  for (p in plots) print(p)
  grDevices::dev.off()
  index <- file.path(specification$table_dir, paste0(prefix, "figure_index.csv"))
  fwrite(data.table(figure = names(plots), file = paste0(prefix, names(plots), ".pdf")),
         index)
  invisible(c(files, packet, index))
}
