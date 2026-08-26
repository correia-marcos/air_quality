# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Produce the paper's Figure 1 — pooled PM10 and PM2.5 concentration distributions
#   in the four metropolitan areas — plus its exceedance companion.
#
#' @Description: Kernel densities of hourly 2023 station concentrations from the four
#   cleaned metro panels (monitoring_stations_outliers/<city>_metro_clean), one curve per
#   city, with the WHO 24-hour interim targets (IT1/IT2) as dashed lines; and a bar chart
#   of the share of station-hours at or above each target, which the density tails show
#   only qualitatively. Santiago is listed first so it keeps the red curve the paper's
#   text refers to. Three PDFs land in results/figures/kernel_plots/.
#
#' @Summary:
#   I.   Import data: the four cities' cleaned station panels.
#   II.  One pooled density and one exceedance panel per pollutant.
#   III. Save the two density PDFs and the stacked exceedance PDF.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
dir_stations <- here::here("data", "processed", "monitoring_stations_outliers")
outdir       <- here::here("results", "figures", "kernel_plots")

fig_width  <- 16
fig_height <- 9
fig_dpi    <- 300

# Santiago first: it takes Set1's red, which is the colour the paper's text refers to.
city_data <- list(
  "Santiago"    = file.path(dir_stations, "santiago_metro_clean"),
  "Bogotá"      = file.path(dir_stations, "bogota_metro_clean"),
  "Mexico City" = file.path(dir_stations, "cdmx_metro_clean"),
  "São Paulo"   = file.path(dir_stations, "sao_paulo_metro_clean")
)

# ============================================================================================
# II: Process data
# ============================================================================================
# One row per pollutant. x_max is a display-only zoom: the density is estimated on all
# 2023 hours and the axis simply stops at the last WHO-relevant range, because a handful
# of unflagged sentinel values live far beyond it and would otherwise stretch the axis.
figure_specs <- data.frame(
  pollutant = c("pm10", "pm25"),
  file_stem = c("all_pm10", "all_pm25"),
  x_max     = c(500, 250),
  stringsAsFactors = FALSE
)

density_plots    <- list()
exceedance_plots <- list()

for (i in seq_len(nrow(figure_specs))) {
  density_plots[[i]] <- plot_kernel_density_by_city(
    city_data,
    pollutant = figure_specs$pollutant[i],
    year      = 2023,
    x_max     = figure_specs$x_max[i])

  exceedance_plots[[i]] <- plot_exceedance_shares(
    city_data,
    pollutant = figure_specs$pollutant[i],
    year      = 2023)
}

# ============================================================================================
# III: Save figures
# ============================================================================================
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(figure_specs))) {
  ggplot2::ggsave(
    filename = file.path(outdir, paste0(figure_specs$file_stem[i], ".pdf")),
    plot     = density_plots[[i]], device = cairo_pdf,
    width    = fig_width, height = fig_height, dpi = fig_dpi)
}

# The two exceedance panels stack into one figure; each panel is titled by its pollutant.
exceedance_combined <- cowplot::plot_grid(plotlist = exceedance_plots, nrow = 2)

ggplot2::ggsave(
  filename = file.path(outdir, "exceedance_shares.pdf"),
  plot     = exceedance_combined, device = cairo_pdf,
  width    = fig_width, height = 12, dpi = fig_dpi)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
