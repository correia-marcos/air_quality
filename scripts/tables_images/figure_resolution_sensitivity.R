# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Draw the resolution-sensitivity diagnostic figures for Bogota: how the
#   education-quintile exposure gap changes as the geographic unit of analysis is
#   coarsened from manzana up to municipio.
#
#' @Description: Reads the four artifacts already written under
#   data/processed/resolution_sensitivity/ (CI estimates by resolution level, the
#   paired-bootstrap change relative to manzana, the retained-variance decomposition,
#   and unit coverage counts). No estimate, model, or interval is recomputed here;
#   the script only reshapes for plotting. Seven figures are produced: three
#   split-panel Design A figures, one per outcome (mean concentration, hours above
#   WHO IT-1, hours above WHO IT-2), each stacking the Q1-vs-Q5 gap on top of its
#   paired-bootstrap change relative to manzana on the same log-scale resolution
#   axis; the paired-bootstrap delta relative to manzana (all outcomes, faceted);
#   the same Q1-vs-Q5 profile for Design C (its own axis, not comparable to
#   Design A); the share of population-weighted exposure variance retained at each
#   resolution; and the full Q1..Q5 profile for Design A (supplementary). Figures
#   are built into a named list first and written to PDF only in the last
#   section.
#
#' @Summary:
#   I.   Setup: load dependencies, utilities, city config.
#   II.  Import data: read the four resolution-sensitivity artifacts.
#   III. Shared labels: resolution-level order, outcome/pollutant labels, captions.
#   IV.  Build figures: draw each figure into a named list; nothing written yet.
#   V.   Save figures: write each figure to results/figures/diagnostics/ as PDF.
#
#' @Date: September 2026
#' @Author: Marcos
# ============================================================================================

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
caption_design_c <- paste0(
  base_caption, " Municipio clusters: G = ", clusters_at(ci_dt, "C"), ". ",
  "Design C's gaps are normalized against a different reference group ",
  "(area-mean-education quintiles, whose Q5 mean exposure is far lower), so its ",
  "magnitudes are NOT comparable with Design A's."
)

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
  brk_dt <- brk_dt[, .(brk = mean(n_resolution_units)), by = .(resolution_level, level_label)]
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
                                                margin = ggplot2::margin(r = 4, unit = "pt")),
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
    title = "Resolution sensitivity, Design C (area-education quintiles)",
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
