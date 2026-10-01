# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot population-weighted exposure densities by education quintile.
#
#' @Description: Use the interpolation stage's adult education groups and geographic
# exposure estimates at 3 and 20 km. Sum each group's population within geographic units,
# then plot weighted densities with the existing upper-tail trimming rule. Individual
# group records stay on disk; retain the smaller weight tables and plots in memory.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read inputs.
#   II.  Process data: sum group weights and build exposure-density plots.
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
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "plot", "exposure_figures.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))

# Use the manuscript font and theme
set_paper_theme()

# Set the IDW checkpoints and the figure destination
dir_idw    <- here::here("data", "processed", "idw_estimates")
outdir_fig <- here::here("results", "figures", "exposure")

# Define individual-group files; each file is shared by both interpolation radii
groups_bogota   <- here::here(dir_idw, "bogota_2018", "bogota_2018_indiv_groups.parquet")
groups_cdmx     <- here::here(dir_idw, "cdmx_2020", "cdmx_2020_indiv_groups.parquet")
groups_santiago <- here::here(dir_idw, "santiago_2017",
                              "santiago_2017_indiv_groups.parquet")
groups_sp <- here::here(dir_idw, "sao_paulo_2010", "sao_paulo_2010_indiv_groups.parquet")

# Read the geographic exposure tables for both radii
exposure_bogota   <- list()
exposure_cdmx     <- list()
exposure_santiago <- list()
exposure_sp       <- list()
for (radius_km in exposure_density_radii_km) {
  key <- as.character(radius_km)
  exposure_bogota[[key]] <- arrow::read_parquet(here::here(dir_idw,
    "bogota_2018", sprintf("bogota_2018_%dkm_idw_exposure.parquet", radius_km)))
  exposure_cdmx[[key]] <- arrow::read_parquet(here::here(dir_idw,
    "cdmx_2020", sprintf("cdmx_2020_%dkm_idw_exposure.parquet", radius_km)))
  exposure_santiago[[key]] <- arrow::read_parquet(here::here(dir_idw,
    "santiago_2017", sprintf("santiago_2017_%dkm_idw_exposure.parquet", radius_km)))
  exposure_sp[[key]] <- arrow::read_parquet(here::here(dir_idw,
    "sao_paulo_2010", sprintf("sao_paulo_2010_%dkm_idw_exposure.parquet", radius_km)))
}

# ========================================================================================
# II: Process data
# ========================================================================================
# Sum adult population weights within each geographic unit and education group
weights_bogota   <- compute_exposure_quintile_weights(groups_file = groups_bogota)
weights_cdmx     <- compute_exposure_quintile_weights(groups_file = groups_cdmx)
weights_santiago <- compute_exposure_quintile_weights(groups_file = groups_santiago)
weights_sp       <- compute_exposure_quintile_weights(groups_file = groups_sp)

# Keep each radius and pollutant in a named plot collection
density_bogota   <- list()
density_cdmx     <- list()
density_santiago <- list()
density_sp       <- list()
for (radius_km in exposure_density_radii_km) {
  for (pollutant in summary_pollutants) {
    key <- paste(radius_km, pollutant, sep = "_")

    density_bogota[[key]] <- plot_exposure_density_by_quintile(
      exposure         = exposure_bogota[[as.character(radius_km)]],
      quintile_weights = weights_bogota,
      city_id          = "bogota_2018",
      buffer_km        = radius_km,
      pollutant        = pollutant,
      city_label       = "Bogotá",
      year_filter      = analysis_year)

    density_cdmx[[key]] <- plot_exposure_density_by_quintile(
      exposure         = exposure_cdmx[[as.character(radius_km)]],
      quintile_weights = weights_cdmx,
      city_id          = "cdmx_2020",
      buffer_km        = radius_km,
      pollutant        = pollutant,
      city_label       = "Mexico City",
      year_filter      = analysis_year)

    density_santiago[[key]] <- plot_exposure_density_by_quintile(
      exposure         = exposure_santiago[[as.character(radius_km)]],
      quintile_weights = weights_santiago,
      city_id          = "santiago_2017",
      buffer_km        = radius_km,
      pollutant        = pollutant,
      city_label       = "Santiago",
      year_filter      = analysis_year)

    density_sp[[key]] <- plot_exposure_density_by_quintile(
      exposure         = exposure_sp[[as.character(radius_km)]],
      quintile_weights = weights_sp,
      city_id          = "sao_paulo_2010",
      buffer_km        = radius_km,
      pollutant        = pollutant,
      city_label       = "São Paulo",
      year_filter      = analysis_year)

  }
}

# ========================================================================================
# III: Save outputs
# ========================================================================================
# The manuscript uses a _v2 suffix for the 20 km figures, retaining 3km in their names
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)
for (radius_km in exposure_density_radii_km) {
  tag <- if (radius_km == 3) "" else "_v2"
  for (pollutant in summary_pollutants) {
    key <- paste(radius_km, pollutant, sep = "_")
    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("distribution_3km_bogota_", pollutant, tag, ".pdf")),
      plot      = density_bogota[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8, height = 5.5, dpi = 300,
      bg        = "white", limitsize = FALSE)

    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("distribution_3km_mexico_", pollutant, tag, ".pdf")),
      plot      = density_cdmx[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8, height = 5.5, dpi = 300,
      bg        = "white", limitsize = FALSE)

    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("distribution_3km_santiago_", pollutant, tag, ".pdf")),
      plot      = density_santiago[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8, height = 5.5, dpi = 300,
      bg        = "white", limitsize = FALSE)

    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("distribution_3km_saopaulo_", pollutant, tag, ".pdf")),
      plot      = density_sp[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8, height = 5.5, dpi = 300,
      bg        = "white", limitsize = FALSE)

  }
}
