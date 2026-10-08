# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Plot optional geographic-resolution sensitivity products.
#
#' @Description: Default reference mode retains the historical Bogota figures.
# --scope=multicity reads completed three-city products, separates A/B/C, shows
# monitoring diagnostics and valid paired differences, and writes a review packet.
# --grouping= plots a multicity run with another education grouping: edu_group3,
# edu_level or edu_quintile_split. It never recomputes estimates or exports to the
# manuscript.
#
#' @Summary:
#   I.   Import functions and select reference or multicity products.
#   II.  Read plotting data and build named plot objects.
#   III. Save the selected vector figures and review packet.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

# ============================================================================================
# I: Import data
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "general_utilities", "plot", "resolution_workflow.R"))
source(here::here("config", "analysis_settings.R"))
resolution_args <- commandArgs(trailingOnly = TRUE)
resolution_scope <- sub("^--scope=", "", grep("^--scope=", resolution_args, value = TRUE))
if (!length(resolution_scope)) resolution_scope <- "reference"
if (length(resolution_scope) != 1L ||
    !resolution_scope %in% c("reference", "multicity")) stop("Invalid --scope.")
groupings <- c("edu_quintile", "edu_quintile_split", "edu_level", "edu_group3")
if (any(!grepl("^--scope=|^--grouping=", resolution_args)) ||
    !all(sub("^--grouping=", "", grep("^--grouping=", resolution_args, value = TRUE))
         %in% groupings)) {
  stop("Use --scope=reference|multicity and --grouping=",
       paste(groupings, collapse = "|"), ".")
}

# Individual education groups: frozen quintiles, or an alternative (multicity only).
grouping <- sub("^--grouping=", "", grep("^--grouping=", resolution_args, value = TRUE))
if (!length(grouping)) grouping <- "edu_quintile"
if (grouping != "edu_quintile" && resolution_scope != "multicity") {
  stop("--grouping needs --scope=multicity.")
}

# Group labels, headline contrast, captions and file prefix of this grouping.
if (grouping == "edu_quintile") {
  group_labels <- paste0("Q", 1:5)
  group_name   <- "quintiles"
  profile_note <- paste("Group means use existing person weights;",
                        "consult coverage tables for each population.")
  file_prefix  <- "resolution_multicity_"
  gap_label    <- "Q1 - Q5"
} else if (grouping == "edu_quintile_split") {
  group_labels <- paste0("Q", 1:5)
  group_name   <- "quintiles"
  profile_note <- paste("Quintiles with tied schooling values split proportionally",
                        "across adjacent quintiles; group means use person weights.")
  file_prefix  <- "resolution_multicity_quintile_split_"
  gap_label    <- "Q1 - Q5 (split ties)"
} else if (grouping == "edu_group3") {
  group_labels <- education_group3_labels
  group_name   <- "groups"
  profile_note <- paste("Three harmonized attainment groups; group means use person",
                        "weights. Bogota codes the highest level attended, so its top",
                        "group includes adults who did not complete a degree.")
  file_prefix  <- "resolution_multicity_education_group3_"
  gap_label    <- "Below secondary - Bachelor's+"
} else {
  group_labels <- education_level_labels
  group_name   <- "levels"
  profile_note <- paste("Harmonized education levels; group means use existing person",
                        "weights. Sao Paulo's census cannot code 'Some tertiary'.",
                        "Bogota's 'Some tertiary' includes completed tecnica and",
                        "normalista.")
  file_prefix  <- "resolution_multicity_education_level_"
  gap_label    <- "None - Graduate"
}
set_paper_theme(base_size = if (resolution_scope == "multicity") 11 else 14)

# ============================================================================================
# II: Build figures
# ============================================================================================
if (resolution_scope == "reference") {
  inputs_result <- resolution_reference_figure_inputs()
  dir_out <- inputs_result$dir_out
  ci_dt <- inputs_result$ci_dt
  boot_dt <- inputs_result$boot_dt
  var_dt <- inputs_result$var_dt

  labels_result <- resolution_reference_figure_labels(
    ci_dt = ci_dt)
  as_level_factor <- labels_result$as_level_factor
  pollutant_labels <- labels_result$pollutant_labels
  as_outcome_factor <- labels_result$as_outcome_factor
  as_pollutant_factor <- labels_result$as_pollutant_factor
  base_caption <- labels_result$base_caption
  caption_design_c <- labels_result$caption_design_c
  caption_split <- labels_result$caption_split
  panel_labels <- labels_result$panel_labels
  subtitle_it1 <- labels_result$subtitle_it1
  subtitle_it2 <- labels_result$subtitle_it2

  plots_result <- resolution_reference_figure_plots(
    ci_dt = ci_dt,
    boot_dt = boot_dt,
    var_dt = var_dt,
    as_level_factor = as_level_factor,
    pollutant_labels = pollutant_labels,
    as_outcome_factor = as_outcome_factor,
    as_pollutant_factor = as_pollutant_factor,
    base_caption = base_caption,
    caption_design_c = caption_design_c,
    caption_split = caption_split,
    panel_labels = panel_labels,
    subtitle_it1 = subtitle_it1,
    subtitle_it2 = subtitle_it2)
  plots <- plots_result$plots
  fig2_dt <- plots_result$fig2_dt
  fig3_dt <- plots_result$fig3_dt
  fig4_dt <- plots_result$fig4_dt
  fig5_dt <- plots_result$fig5_dt

} else {
  inputs_result <- resolution_multicity_figure_inputs(
    grouping = grouping)
  cities <- inputs_result$cities
  city_labels <- inputs_result$city_labels
  dir_in <- inputs_result$dir_in
  dir_out <- inputs_result$dir_out
  levels <- inputs_result$levels
  axis_order <- inputs_result$axis_order
  axis_labels <- inputs_result$axis_labels
  objects <- inputs_result$objects
  x <- inputs_result$x

  contrasts_result <- resolution_multicity_figure_contrasts(
    levels = levels,
    axis_labels = axis_labels,
    objects = objects,
    x = x,
    group_col = grouping,
    group_labels = group_labels,
    group_name = group_name,
    gap_label = gap_label,
    profile_note = profile_note)
  plots <- contrasts_result$plots
  x <- contrasts_result$x
  p <- contrasts_result$p

  diagnostics_result <- resolution_multicity_figure_diagnostics(
    cities = cities,
    city_labels = city_labels,
    dir_in = dir_in,
    levels = levels,
    axis_order = axis_order,
    axis_labels = axis_labels,
    objects = objects,
    x = x,
    plots = plots,
    gap_label = gap_label)
  plots <- diagnostics_result$plots
  matrix_units <- diagnostics_result$matrix_units

}

# ============================================================================================
# III: Save figures
# ============================================================================================
if (resolution_scope == "reference") {
  save_result <- resolution_reference_figure_save(
    dir_out = dir_out,
    ci_dt = ci_dt,
    plots = plots,
    fig2_dt = fig2_dt,
    fig3_dt = fig3_dt,
    fig4_dt = fig4_dt,
    fig5_dt = fig5_dt)

} else {
  save_result <- resolution_multicity_figure_save(
    dir_out = dir_out,
    plots = plots,
    p = p,
    file_prefix = file_prefix)

}
