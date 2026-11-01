# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot the review packets of the A/B/C robustness designs.
#
#' @Description: For each specification in resolution_figure_specifications, read the
# completed three-city products, separate designs A/B/C, show monitoring diagnostics and
# valid paired differences. Writes vector PDFs and a packet to results/figures/
# specification_sensitivity/robustness_designs/. Never recomputes estimates.
#
#' @Summary:
#   I.   Import data: source functions and settings, validate the specifications.
#   II.  Process data: read each specification's tables and build its plots.
#   III. Save outputs: save each specification's figures, packet and index.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions, their packages and the declared variants.
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "general_utilities", "plot", "specification_sensitivity",
                  "resolution_workflow.R"))
source(here::here("config", "analysis_settings.R"))

# Validate the specifications that have review figures.
specifications <- lapply(resolution_figure_specifications, resolution_specification,
                         specifications = resolution_specifications,
                         cities         = resolution_cities)
names(specifications) <- resolution_figure_specifications

# Set the figure theme of the review packets.
set_paper_theme(base_size = 11)

# ========================================================================================
# II: Process data
# ========================================================================================
figure_tables <- list()
plots         <- list()

# Select the first specification when executing the loop body line by line in RStudio.
id <- names(specifications)[1]

for (id in names(specifications)) {
  # Read the specification's saved city tables and matrix diagnostics.
  figure_tables[[id]] <- resolution_figure_tables(specifications[[id]])

  # Plot the designs, then the monitoring diagnostics, with the grouping's labels.
  labels <- resolution_grouping_labels[[specifications[[id]]$grouping]]
  plots[[id]] <- c(
    plot_resolution_designs(tables       = figure_tables[[id]],
                            group_col    = specifications[[id]]$grouping,
                            group_labels = labels$group_labels,
                            group_name   = labels$group_name,
                            gap_label    = labels$gap_label,
                            profile_note = labels$profile_note),
    plot_resolution_monitoring(tables    = figure_tables[[id]],
                               gap_label = labels$gap_label))
}

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save each specification's vector figures, review packet and figure index.
figure_files <- list()
for (id in names(specifications)) {
  figure_files[[id]] <- save_resolution_figures(plots         = plots[[id]],
                                                specification = specifications[[id]])
}
