# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Plot the distribution of individual-hour exposure by education quintile.
#
#' @Description: For each city and pollutant, draws population-weighted densities of the
# interpolated exposure, one curve per education quintile, at the two radii the
# manuscript prints: 3 km in the main panels and 20 km in the panels it tags "_v2". Reads
# the IDW estimates written by estimate_idw.R; the quintiles come from that stage's
# individual artifact, so this figure and the exposure regressions share one definition.
#
#' @Summary:
#   I.   Setup: load dependencies, set the paper theme, define paths.
#   II.  Densities at each radius, one pass per radius.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Setup
# ============================================================================================
# Define input and output folders
dir_idw    <- here::here("data", "processed", "idw_estimates")
outdir_fig <- here::here("results", "paper", "figures", "exposure_densities")

dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

analysis_year <- 2023L

# Figure geometry shared by every panel.
fig_width  <- 8
fig_height <- 5.5
fig_dpi    <- 300

# The manuscript keeps "3km" in every file name and marks the wider radius with "_v2",
# so the tag is not simply the radius.
radii_km <- c(3, 20)

# ============================================================================================
# II: Densities by education quintile
# ============================================================================================
for (radius_km in radii_km) {

  radius_tag <- if (radius_km == 3) "" else "_v2"

  for (pol in c("pm10", "pm25")) {

    density_bogota <- plot_exposure_density_by_quintile(
      dir_idw     = dir_idw,
      city_id     = "bogota_2018",
      buffer_km   = radius_km,
      pollutant   = pol,
      city_label  = "Bogotá",
      year_filter = analysis_year)

    ggplot2::ggsave(
      file.path(outdir_fig,
                paste0("distribution_3km_bogota_", pol, radius_tag, ".pdf")),
      plot = density_bogota, device = cairo_pdf,
      width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")

    density_cdmx <- plot_exposure_density_by_quintile(
      dir_idw     = dir_idw,
      city_id     = "cdmx_2020",
      buffer_km   = radius_km,
      pollutant   = pol,
      city_label  = "Mexico City",
      year_filter = analysis_year)

    ggplot2::ggsave(
      file.path(outdir_fig,
                paste0("distribution_3km_mexico_", pol, radius_tag, ".pdf")),
      plot = density_cdmx, device = cairo_pdf,
      width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")

    density_santiago <- plot_exposure_density_by_quintile(
      dir_idw     = dir_idw,
      city_id     = "santiago_2017",
      buffer_km   = radius_km,
      pollutant   = pol,
      city_label  = "Santiago",
      year_filter = analysis_year)

    ggplot2::ggsave(
      file.path(outdir_fig,
                paste0("distribution_3km_santiago_", pol, radius_tag, ".pdf")),
      plot = density_santiago, device = cairo_pdf,
      width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")

    density_sp <- plot_exposure_density_by_quintile(
      dir_idw     = dir_idw,
      city_id     = "sao_paulo_2010",
      buffer_km   = radius_km,
      pollutant   = pol,
      city_label  = "São Paulo",
      year_filter = analysis_year)

    ggplot2::ggsave(
      file.path(outdir_fig,
                paste0("distribution_3km_saopaulo_", pol, radius_tag, ".pdf")),
      plot = density_sp, device = cairo_pdf,
      width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  }
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
