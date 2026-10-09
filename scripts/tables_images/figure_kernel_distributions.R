# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Compare hourly PM distributions across cities.
#
#' @Description: Read cleaned station readings for the analysis year and the two
# reference years. Density bandwidths use all finite readings; x_max bounds the density
# evaluation grid. Keep the density curves and threshold-share plots before saving.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read inputs.
#   II.  Process data: build density curves and exceedance-share plots.
#   III. Save outputs: save the results in the same order.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the scientific functions and their shared helpers
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "plot", "concentration_distributions.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))

# Use the manuscript font and theme
set_paper_theme()

# Set the cleaned hourly panels and figure destination
dir_stations <- here::here("data", "processed", "monitoring_stations_outliers")
outdir_fig   <- here::here("results", "figures", "temporal")
city_panels <- list(
  "Bogotá"      = here::here(dir_stations, "bogota_metro_clean"),
  "Mexico City" = here::here(dir_stations, "cdmx_metro_clean"),
  "São Paulo"   = here::here(dir_stations, "sao_paulo_metro_clean"),
  "Santiago"    = here::here(dir_stations, "santiago_metro_clean"))

# ========================================================================================
# II: Process data
# ========================================================================================
# Compute the two pollutant densities for each year; keep them in named lists
density_pm10 <- density_pm25 <- list()
for (year in c(analysis_year, kernel_reference_years)) {
  key <- as.character(year)
  pm25_limit <- if (year == analysis_year) kernel_pm25_limit else
    kernel_pm25_reference_limit
  density_pm10[[key]] <- plot_kernel_density_by_city(city_data       = city_panels,
    pollutant       = "pm10",
    year            = year,
    x_max           = kernel_pm10_limit,
    city_colours    = kernel_city_colours,
    city_linetypes  = kernel_city_linetypes,
    fill_alpha      = 0,
    legend_position = "bottom")

  density_pm25[[key]] <- plot_kernel_density_by_city(city_data       = city_panels,
    pollutant       = "pm25",
    year            = year,
    x_max           = pm25_limit,
    city_colours    = kernel_city_colours,
    city_linetypes  = kernel_city_linetypes,
    fill_alpha      = 0,
    legend_position = "bottom")

}

# Compare the shares of readings above each WHO threshold in the analysis year
exceedance_pm10 <- plot_exceedance_shares(city_data       = city_panels,
                                          pollutant       = "pm10",
                                          year            = analysis_year,
                                          legend_position = "bottom")

exceedance_pm25 <- plot_exceedance_shares(city_data       = city_panels,
                                          pollutant       = "pm25",
                                          year            = analysis_year,
                                          legend_position = "bottom")

exceedance_combined <- cowplot::plot_grid(exceedance_pm10,
                                          exceedance_pm25,
                                          nrow = 2)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the paired density figures for each year, then the combined threshold figure
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)
for (year in c(analysis_year, kernel_reference_years)) {
  key <- as.character(year)
  tag <- if (year == analysis_year) "" else paste0("_", year)
  ggplot2::ggsave(filename  = here::here(outdir_fig, paste0("all", tag, ".pdf")),
                  plot      = density_pm10[[key]],
                  device    = grDevices::cairo_pdf,
                  width     = 16, height = 9, dpi = 300, limitsize = FALSE)

  ggplot2::ggsave(filename  = here::here(outdir_fig, paste0("all_pm25", tag, ".pdf")),
                  plot      = density_pm25[[key]],
                  device    = grDevices::cairo_pdf,
                  width     = 16, height = 9, dpi = 300, limitsize = FALSE)

}

ggplot2::ggsave(filename  = here::here(outdir_fig, "exceedance_shares.pdf"),
                plot      = exceedance_combined,
                device    = grDevices::cairo_pdf,
                width     = 16, height = 12, dpi = 300, limitsize = FALSE)
