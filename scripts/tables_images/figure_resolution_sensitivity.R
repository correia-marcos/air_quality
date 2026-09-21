# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Plot optional geographic-resolution sensitivity products.
#
#' @Description: Default reference mode retains the historical Bogota figures.
# --scope=multicity reads completed three-city products, separates A/B/C, shows
# monitoring diagnostics and valid paired differences, and writes a review packet.
# It never recomputes estimates or exports to the manuscript.
#
#' @Summary:
#   I.   Read reference or completed multicity products.
#   II.  Plot city-specific supports and the separate Bogota locality branch.
#   III. Save vector figures and a multipage methodological review packet.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

resolution_args <- commandArgs(trailingOnly = TRUE)
resolution_scope <- sub("^--scope=", "", grep("^--scope=", resolution_args, value = TRUE))
if (!length(resolution_scope)) resolution_scope <- "reference"
if (length(resolution_scope) != 1L ||
    !resolution_scope %in% c("reference", "multicity")) stop("Invalid --scope.")
if (any(!grepl("^--scope=", resolution_args))) stop("Use --scope=reference|multicity.")
if (resolution_scope == "reference") {
# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
dir_in  <- here::here("data", "processed", "resolution_sensitivity")
dir_out <- here::here("results", "figures", "diagnostics")

ci_pq       <- here::here(dir_in, "resolution_ci_bogota_3km_2023.parquet")
boot_pq     <- here::here(dir_in, "resolution_bootstrap_bogota_3km_2023.parquet")
variance_pq <- here::here(dir_in, "resolution_variance_bogota_3km_2023.parquet")

for (pq in c(ci_pq, boot_pq, variance_pq)) {
  if (!file.exists(pq)) stop("Required artifact not found: ", pq)
}

ci_dt   <- data.table::as.data.table(arrow::read_parquet(ci_pq))
boot_dt <- data.table::as.data.table(arrow::read_parquet(boot_pq))
var_dt  <- data.table::as.data.table(arrow::read_parquet(variance_pq))

# ============================================================================================
# II: Shared labels
# ============================================================================================
# Resolution ladder, coarse to fine; every plot uses this ordered factor for its
# level axis or its facet order derives from it.
level_order  <- c("municipio", "localidad", "sector_urbano",
                  "seccion_urbana", "manzana")
level_labels <- c("Municipio", "Localidad", "Sector urbano",
                  "Sección urbana", "Manzana")

as_level_factor <- function(x) {
  factor(x, levels = level_order, labels = level_labels)
}

outcome_labels    <- c(avg = "Mean concentration", hrs_d_it1 = "Hours > IT-1",
                       hrs_d_it2 = "Hours > IT-2")
pollutant_labels  <- c(pm10 = "PM10", pm25 = "PM2.5")

as_outcome_factor <- function(x) {
  factor(x, levels = names(outcome_labels), labels = unname(outcome_labels))
}
as_pollutant_factor <- function(x) {
  factor(x, levels = names(pollutant_labels), labels = unname(pollutant_labels))
}

# Fixed caption text shared by every figure (sample, city, year, buffer, quintiles).
base_caption <- paste0(
  "Bogotá, 2023, 3 km buffer, education quintiles. Estimation sample: 4,255,038 ",
  "adults (74.7% of metro adults aged 25+)."
)

# Cluster count at the coarsest level (municipio), reported for Fig. 3.
clusters_at <- function(dt, design_) {
  g <- sort(unique(dt[design == design_ & resolution_level == "municipio",
                      n_clusters]))
  if (length(g) == 1L) as.character(g) else paste(range(g), collapse = "-")
}
historical_c_population <- ci_dt[design == "C",
  .(population = unique(pop_estimation)), by = pollutant]
caption_design_c <- paste(strwrap(paste0(
  "Historical joint design: exposure aggregation and area-education classification. ",
  "Bogota, 2023, 3 km. Estimation population: ",
  format(historical_c_population[pollutant == "pm10", population], big.mark = ","),
  " (PM10) / ",
  format(historical_c_population[pollutant == "pm25", population], big.mark = ","),
  " (PM2.5). Municipio clusters: G = ", clusters_at(ci_dt, "C"), ". ",
  "Groups and populations differ from A; normalized magnitudes are not comparable."
), width = 120), collapse = "\n")

# Caption for the three per-outcome split figures: states both pollutant sample
# sizes explicitly, since the manzana unit count also differs slightly by pollutant.
# Wrapped to keep it inside the plot width instead of running off the page edge.
caption_split <- paste(strwrap(paste0(
  "Bogotá, 2023, 3 km buffer, education quintiles, reference group quintile 5. ",
  "Estimation sample 4,255,038 adults (PM10) / 4,251,366 (PM2.5), held fixed ",
  "across all five geographies."
), width = 100), collapse = "\n")

# The two stacked panels of each split figure; used as facet_grid(panel ~ .) rows
# so each panel carries its own axis label via the (outside, left) strip text —
# patchwork is not in the project's package list, so this stands in for it.
panel_labels <- c(
  "Quintile 1 vs quintile 5, % difference",
  "Change vs manzana (percentage points)"
)

# WHO threshold text, quoted in the IT-1/IT-2 figure subtitles.
subtitle_it1 <- "WHO interim target 1: PM10 150 ug/m3, PM2.5 75 ug/m3"
subtitle_it2 <- "WHO interim target 2: PM10 100 ug/m3, PM2.5 50 ug/m3"

# ============================================================================================
# III: Build figures
# ============================================================================================
plots <- list()

# --------------------------------------------------------------------------------------------
# Function: build_resolution_split_figure
#
#' @param outcome_key string; one of the `outcome` values in ci_dt/boot_dt
#   ("avg", "hrs_d_it1", "hrs_d_it2").
#' @param title_      string; figure title.
#' @param subtitle_   string or NULL; figure subtitle (WHO threshold text).
#
#' @return  a ggplot object with two stacked panels sharing a log10 x-axis of
#   n_resolution_units: the Design A Q1-vs-Q5 gap (top) and the paired-bootstrap
#   change vs. manzana (bottom), both for group == 1, both pollutants.
#
#' @details
#   Reuses the already-estimated ci_dt/boot_dt rows for the requested outcome; no
#   estimate is recomputed. The bottom panel has no n_resolution_units of its own,
#   so it is merged in from the matching top-panel rows (same resolution_level x
#   pollutant). Axis breaks sit at the mean of the two pollutants' actual unit
#   counts per level (they differ slightly, e.g. manzana: 37,416 PM10 vs 37,326
#   PM2.5), labelled with the geography name so the reader never decodes a count.
#
#' @Written_on : September 2026
#' @Written_by : Marcos
# --------------------------------------------------------------------------------------------
build_resolution_split_figure <- function(outcome_key, title_, subtitle_ = NULL) {
  top_dt <- ci_dt[design == "A" & group == 1 & outcome == outcome_key]
  top_dt[, `:=`(
    pollutant_label = as_pollutant_factor(pollutant),
    level_label     = as_level_factor(resolution_level),
    panel           = factor(panel_labels[1], levels = panel_labels),
    value = 100 * estimate, ymin = 100 * ci_low, ymax = 100 * ci_high
  )]

  units_by_level <- unique(top_dt[, .(resolution_level, pollutant, n_resolution_units)])

  bottom_dt <- boot_dt[group == 1 & outcome == outcome_key]
  bottom_dt <- merge(bottom_dt, units_by_level, by = c("resolution_level", "pollutant"))
  bottom_dt[, `:=`(
    pollutant_label = as_pollutant_factor(pollutant),
    level_label     = as_level_factor(resolution_level),
    panel           = factor(panel_labels[2], levels = panel_labels),
    value = 100 * delta_point, ymin = 100 * delta_boot_low, ymax = 100 * delta_boot_high
  )]

  keep_cols <- c("pollutant_label", "level_label", "panel", "n_resolution_units",
                 "value", "ymin", "ymax")
  split_dt <- rbind(top_dt[, ..keep_cols], bottom_dt[, ..keep_cols])

  brk_dt <- unique(top_dt[, .(resolution_level, level_label, n_resolution_units)])
  brk_dt <- brk_dt[, .(brk = mean(n_resolution_units)), by = .(resolution_level,
    level_label)]
  brk_dt <- brk_dt[order(brk)]

  # The unit count rides under the geography name: the log axis is there to show how much
  # aggregation each step represents, which is unreadable without the counts themselves.
  # The tick shows PM10's covered-unit count rather than the two pollutants' mean, so the
  # printed number is one that actually exists and matches the audit note's tables.
  lab_dt <- unique(top_dt[pollutant_label == pollutant_labels[["pm10"]],
                          .(resolution_level, n_pm10 = n_resolution_units)])
  brk_dt <- merge(brk_dt, lab_dt, by = "resolution_level")
  brk_dt <- brk_dt[order(brk)]

  brk_dt[, tick := paste0(level_label, "\n(",
                          format(n_pm10, big.mark = ",", trim = TRUE), ")")]

  # Force the top panel's free_y range to include 0, without drawing a line there.
  zero_dt <- data.table::data.table(
    panel = factor(panel_labels[1], levels = panel_labels),
    x0 = brk_dt$brk[1], value = 0
  )
  hline_dt <- data.table::data.table(
    panel = factor(panel_labels[2], levels = panel_labels), value = 0
  )

  ggplot2::ggplot(split_dt, ggplot2::aes(
    x = n_resolution_units, y = value,
    colour = pollutant_label, shape = pollutant_label, group = pollutant_label
  )) +
    ggplot2::geom_hline(
      data = hline_dt, ggplot2::aes(yintercept = value), inherit.aes = FALSE,
      linetype = "dashed", colour = "grey40", linewidth = 0.4
    ) +
    ggplot2::geom_blank(
      data = zero_dt, ggplot2::aes(x = x0, y = value), inherit.aes = FALSE
    ) +
    ggplot2::geom_line(linewidth = 0.3, alpha = 0.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax), width = 0.05, linewidth = 0.5
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_log10(breaks = brk_dt$brk, labels = brk_dt$tick) +
    ggplot2::scale_colour_viridis_d(name = "Pollutant") +
    ggplot2::scale_shape_manual(name = "Pollutant", values = c(16, 17)) +
    ggplot2::facet_grid(panel ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(
      x = "Number of geographic units (log scale)", y = NULL,
      title = title_, subtitle = subtitle_, caption = caption_split
    ) +
    ggplot2::theme(
      strip.placement = "outside",
      strip.background.y = ggplot2::element_blank(),
      # The rotated panel labels are the outermost element on the left, so the margin has to
      # clear them or the first character is cut off at the image edge.
      strip.text.y.left = ggplot2::element_text(angle = 90, size = 8.5,
                                                margin = ggplot2::margin(r = 4,
                                                  unit = "pt")),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 26, unit = "pt")
    )
}

# --- Fig. 1a-c: Design A, Q1 vs Q5 gap + bootstrap change, one figure per outcome -
plots[["resolution_sensitivity_mean_concentration"]] <- build_resolution_split_figure(
  "avg", "Resolution sensitivity: mean concentration, Design A"
)
plots[["resolution_sensitivity_hours_it1"]] <- build_resolution_split_figure(
  "hrs_d_it1", "Resolution sensitivity: hours above WHO IT-1, Design A", subtitle_it1
)
plots[["resolution_sensitivity_hours_it2"]] <- build_resolution_split_figure(
  "hrs_d_it2", "Resolution sensitivity: hours above WHO IT-2, Design A", subtitle_it2
)

# --- Fig. 2: paired-bootstrap change relative to manzana --------------------------
fig2_dt <- boot_dt[group == 1]
fig2_dt[, outcome_label   := as_outcome_factor(outcome)]
fig2_dt[, pollutant_label := as_pollutant_factor(pollutant)]
fig2_dt[, level_label     := as_level_factor(resolution_level)]

plots[["resolution_sensitivity_delta"]] <- ggplot2::ggplot(
  fig2_dt, ggplot2::aes(x = level_label, y = 100 * delta_point)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = 100 * delta_boot_low, ymax = 100 * delta_boot_high),
    width = 0.15, linewidth = 0.5
  ) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::facet_grid(pollutant_label ~ outcome_label) +
  ggplot2::labs(
    x = "Resolution level (coarse to fine →)",
    y = "Change in Q1 vs Q5 gap vs. manzana (pp)",
    title = "Paired-bootstrap change in the exposure gap, Design A",
    caption = base_caption
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    plot.caption = ggplot2::element_text(hjust = 0)
  )

# --- Fig. 3: Design C, Q1 vs Q5, own axis (own reference group) -------------------
fig3_dt <- ci_dt[design == "C" & group == 1]
fig3_dt[, outcome_label   := as_outcome_factor(outcome)]
fig3_dt[, pollutant_label := as_pollutant_factor(pollutant)]
fig3_dt[, level_label     := as_level_factor(resolution_level)]

manzana_ref_c <- fig3_dt[resolution_level == "manzana",
                         .(pollutant_label, outcome_label,
                           ref = 100 * estimate)]

plots[["resolution_sensitivity_design_c"]] <- ggplot2::ggplot(
  fig3_dt, ggplot2::aes(x = n_resolution_units, y = 100 * estimate)
) +
  ggplot2::geom_hline(
    data = manzana_ref_c, ggplot2::aes(yintercept = ref),
    linetype = "dashed", colour = "grey40", linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.3, alpha = 0.5) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = 100 * ci_low, ymax = 100 * ci_high),
    width = 0.05, linewidth = 0.5
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_text(
    ggplot2::aes(label = level_label), size = 2.4, vjust = -1.2,
    check_overlap = TRUE
  ) +
  ggplot2::scale_x_log10() +
  ggplot2::expand_limits(y = 0) +
  ggplot2::facet_grid(pollutant_label ~ outcome_label) +
  ggplot2::labs(
    x = "Resolution units (log scale, coarse to fine →)",
    y = "Q1 vs Q5 gap (%), Design C reference group",
    title = "Historical C: joint exposure aggregation and area classification",
    subtitle = "Not comparable in magnitude with Design A; see caption",
    caption = caption_design_c
  ) +
  ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))

# --- Fig. 4: mechanism -- variance retained by resolution --------------------------
fig4_dt <- data.table::copy(var_dt)
fig4_dt[, pollutant := data.table::fcase(
  grepl("pm10", outcome), "pm10",
  grepl("pm25", outcome), "pm25"
)]
fig4_dt[, outcome_short := sub("_pm(10|25)", "", outcome)]
fig4_dt[, outcome_label   := as_outcome_factor(outcome_short)]
fig4_dt[, pollutant_label := as_pollutant_factor(pollutant)]

plots[["resolution_sensitivity_variance"]] <- ggplot2::ggplot(
  fig4_dt, ggplot2::aes(x = n_resolution_units, y = share_retained)
) +
  ggplot2::geom_line(linewidth = 0.3, alpha = 0.5) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::facet_grid(pollutant_label ~ outcome_label) +
  ggplot2::labs(
    x = "Resolution units (log scale, coarse to fine →)",
    y = "Share of population-weighted exposure variance retained",
    title = "Variance retained by aggregation level",
    caption = base_caption
  ) +
  ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))

# --- Fig. 5: Design A, full Q1..Q5 profile (supplementary) ------------------------
fig5_dt <- ci_dt[design == "A"]
fig5_dt[, outcome_label   := as_outcome_factor(outcome)]
fig5_dt[, pollutant_label := as_pollutant_factor(pollutant)]
fig5_dt[, level_label     := as_level_factor(resolution_level)]
fig5_dt[, group_label     := factor(group, levels = 1:5)]

plots[["resolution_sensitivity_quintile_profile"]] <- ggplot2::ggplot(
  fig5_dt, ggplot2::aes(x = level_label, y = 100 * estimate,
                        colour = group_label, shape = group_label,
                        group = group_label)
) +
  ggplot2::geom_line(linewidth = 0.4, alpha = 0.7) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::scale_colour_viridis_d(name = "Quintile") +
  ggplot2::scale_shape_discrete(name = "Quintile") +
  ggplot2::facet_grid(pollutant_label ~ outcome_label) +
  ggplot2::labs(
    x = "Resolution level (coarse to fine →)",
    y = "Gap vs. Q5 (%)",
    title = "Full quintile profile across resolution levels, Design A",
    caption = base_caption
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    plot.caption = ggplot2::element_text(hjust = 0)
  )

# ============================================================================================
# IV: Save figures
# ============================================================================================
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# The three split figures stack two panels, so they get extra height; every other
# figure keeps the previous 9x6 size.
split_fig_names <- c("resolution_sensitivity_mean_concentration",
                     "resolution_sensitivity_hours_it1",
                     "resolution_sensitivity_hours_it2")

for (fname in names(plots)) {
  height <- if (fname %in% split_fig_names) 7.5 else 6
  save_plot_pdf(plots[[fname]], here::here(dir_out, paste0(fname, ".pdf")),
                width = 9, height = height)
}

cat("Saved", length(plots), "resolution-sensitivity figures (PDF) to:", dir_out, "\n")
cat("Rows: fig_mean_top =", nrow(ci_dt[design == "A" & group == 1 & outcome == "avg"]),
   "| fig2 =", nrow(fig2_dt), "| fig3 =", nrow(fig3_dt), "| fig4 =", nrow(fig4_dt),
   "| fig5 =", nrow(fig5_dt), "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")

} else {
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
set_paper_theme(base_size = 11)
cities <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
city_labels <- c(bogota_2018 = "Bogota", santiago_2017 = "Santiago",
                 sao_paulo_2010 = "Sao Paulo")
dir_in <- here::here("data", "processed", "resolution_sensitivity")
dir_out <- here::here("results", "figures", "diagnostics")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
for (city in cities) {
  if (!identical(readLines(file.path(dir_in, city, "STATUS.txt"))[1], "complete")) {
    stop("Incomplete resolution run: ", city)
  }
}
# Named supports are ordered separately within each city; locality is a separate branch.
levels <- rbindlist(lapply(cities, function(city) {
  x <- fread(here::here("data", "interim", "resolution_sensitivity", city, "levels.csv"))
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
plots <- list()
notes_a <- paste("Fixed education-reporting adults and individual quintiles.",
  "Exposure aggregated with all-adult weights; locality branch shown separately.")
notes_b <- paste("Native and common samples vary by level;",
  "P = education-reporting adults.",
  "\nCommon samples are pairwise intersections. Paired changes are shown separately.")
for (design in c("A", "B")) for (outcome_type in c("avg", "it1", "it2")) {
  selected_designs <- if (design == "A") "A" else c("B_native", "B_common")
  x <- objects$contrasts[design %chin% selected_designs]
  x <- x[grepl(if (outcome_type == "avg") "^avg" else paste0(outcome_type, "$"), outcome)]
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
      y = if (outcome_type == "avg") "Q1 - Q5 (micrograms per cubic metre)" else
        "Q1 - Q5 (hours)", colour = NULL, shape = NULL,
      caption = if (design == "A") notes_a else notes_b) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 9),
      plot.caption = element_text(hjust = 0, size = 8),
        strip.text = element_text(face = "bold"))
  plots[[paste(design, outcome_type, sep = "_")]] <- p
}
# The parallel Bogota locality branch never masquerades as a nested sector-to-locality path.
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
       y = "Q1 - Q5 (micrograms per cubic metre)", caption = "Descriptive comparisons.") +
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
      x = "Geographic support (coarse to fine)", y = "Change in absolute Q1 - Q5 gap",
      caption = paste("95% conditional geographic-cluster bootstrap intervals;",
        "999 draws.",
        "Coarse comparisons are descriptive.",
        "B endpoints use the same pairwise population.")) +
    theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8,
      hjust = 0))
}
for (design_name in c("A", "B_native", "C")) {
  x <- objects$profiles[design == design_name & grepl("^avg", outcome)]
  x <- merge(x, levels[, .(city_id, level, label)], by = c("city_id", "level"))
  plots[[paste0(design_name, "_profiles")]] <-
    ggplot(x, aes(edu_quintile, mean, colour = label, group = label)) +
    geom_line(linewidth = .5, na.rm = TRUE) + geom_point(size = 1.6, na.rm = TRUE) +
    facet_wrap(vars(city, pollutant), ncol = 2, scales = "free_y") +
    scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
    scale_colour_manual(values = c("Fine census unit" = "#263746",
      "Zona censal" = "#263746", "Area de ponderacao" = "#263746",
      "Seccion" = "#527D93", "Distrito censal" = "#527D93", "Sector" = "#9B6137",
      "Comuna" = "#9B6137", "Localidad/municipio" = "#718355", "Municipio" = "#A06C7D")) +
    labs(title = paste(if (design_name == "B_native") "Native B" else design_name,
                         "- full education-group profiles"),
      x = if (design_name == "C") "Area education group" else
        "Individual education quintile",
      y = "Mean exposure (micrograms per cubic metre)", colour = "Geographic support",
      caption = if (design_name == "C") paste(
        "Classification only; finest exposure fixed.",
        "Empty area groups remain unavailable.",
        "These groups differ from A/B quintiles.") else
        paste("Group means use existing person weights;",
          "consult coverage tables for each population.")) +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8),
          panel.grid.minor = element_blank())
}
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
m <- melt(matrix_units[adult_population > 0],
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
       x = "Covered estimation units (log scale; coarse to fine)", y = "Q1 - Q5",
         colour = NULL,
       caption = paste("Unit counts do not harmonize geographic concepts across cities.",
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
# Save individual vector figures and a single review packet; never export to the manuscript.
for (name in names(plots)) {
  ggsave(file.path(dir_out, paste0("resolution_multicity_", name, ".pdf")), plots[[name]],
         width = 11.7, height = 8.3, units = "in", device = grDevices::cairo_pdf)
}
grDevices::cairo_pdf(file.path(dir_out, "resolution_multicity.pdf"), width = 11.7,
  height = 8.3)
for (p in plots) print(p)
grDevices::dev.off()
fwrite(data.table(figure = names(plots), file = paste0("resolution_multicity_",
  names(plots),
  ".pdf")), here::here("results", "tables", "resolution_multicity_figure_index.csv"))

}
