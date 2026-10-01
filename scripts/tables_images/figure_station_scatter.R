# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot station pollution outcomes against education and income.
#
#' @Description: Read the four station-summary checkpoints. Each point is one station
# with both axis values observed; the line is an OLS fit. Bogotá uses buffer context,
# while the other cities use the containing area. Keep the existing schooling-scale
# correction: divide by 1,000 if a table's maximum exceeds 100. Income panels are
# available for CDMX and São Paulo.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read inputs.
#   II.  Process data: rescale schooling and build named scatter plots.
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
source(here::here("src", "general_utilities", "plot", "station_monitoring.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))

# Use the manuscript font and theme
set_paper_theme()

# Set the station-summary folder and figure destination
dir_station <- here::here("data", "processed", "station_socio_exposure")
outdir_fig  <- here::here("results", "figures", "monitoring")

# Define station-level socioeconomic exposure paths
station_bogota_pq   <- here::here(dir_station, "bogota_2018",
                                  "bogota_2018_2023_3km_station_socio.parquet")
station_cdmx_pq     <- here::here(dir_station, "cdmx_2020",
                                  "cdmx_2020_2023_station_socio.parquet")
station_santiago_pq <- here::here(dir_station, "santiago_2017",
                                  "santiago_2017_2023_station_socio.parquet")
station_sp_pq       <- here::here(dir_station, "sao_paulo_2010",
                                  "sao_paulo_2010_2023_station_socio.parquet")

# Read the four station tables
station_bogota   <- safe_read_parquet(station_bogota_pq)
station_cdmx     <- safe_read_parquet(station_cdmx_pq)
station_santiago <- safe_read_parquet(station_santiago_pq)
station_sp       <- safe_read_parquet(station_sp_pq)

# ========================================================================================
# II: Process data
# ========================================================================================
# Apply the existing schooling-unit correction before plotting
station_bogota   <- rescale_station_education(station_bogota, "Bogota")
station_cdmx     <- rescale_station_education(station_cdmx, "Mexico City")
station_santiago <- rescale_station_education(station_santiago, "Gran Santiago")
station_sp       <- rescale_station_education(station_sp, "Sao Paulo")

# Keep six education plots per city, named by the pollution outcome
plots_bogota   <- list()
plots_cdmx     <- list()
plots_santiago <- list()
plots_sp       <- list()
for (outcome in names(station_scatter_labels)) {
  plots_bogota[[outcome]] <- plot_station_scatter(station_dt = station_bogota,
    y_col      = outcome,
    x_col      = "education_mean",
    y_label    = station_scatter_labels[[outcome]],
    x_label    = "Average years of schooling")

  plots_cdmx[[outcome]] <- plot_station_scatter(station_dt = station_cdmx,
    y_col      = outcome,
    x_col      = "education_mean",
    y_label    = station_scatter_labels[[outcome]],
    x_label    = "Average years of schooling")

  plots_santiago[[outcome]] <- plot_station_scatter(station_dt = station_santiago,
    y_col      = outcome,
    x_col      = "education_mean",
    y_label    = station_scatter_labels[[outcome]],
    x_label    = "Average years of schooling")

  plots_sp[[outcome]] <- plot_station_scatter(station_dt = station_sp,
    y_col      = outcome,
    x_col      = "education_mean",
    y_label    = station_scatter_labels[[outcome]],
    x_label    = "Average years of schooling")

}

# Plot income only where the census provides it
income_cdmx_pm10 <- plot_station_scatter(station_dt = station_cdmx,
  y_col      = "hrs_d_pm10_it1",
  x_col      = "income_mean",
  y_label    = station_scatter_labels[["hrs_d_pm10_it1"]],
  x_label    = "Average monthly labour income")

income_cdmx_pm25 <- plot_station_scatter(station_dt = station_cdmx,
  y_col      = "hrs_d_pm25_it1",
  x_col      = "income_mean",
  y_label    = station_scatter_labels[["hrs_d_pm25_it1"]],
  x_label    = "Average monthly labour income")

income_sp_pm10 <- plot_station_scatter(station_dt = station_sp,
  y_col      = "hrs_d_pm10_it1",
  x_col      = "income_mean",
  y_label    = station_scatter_labels[["hrs_d_pm10_it1"]],
  x_label    = "Average monthly labour income")

income_sp_pm25 <- plot_station_scatter(station_dt = station_sp,
  y_col      = "hrs_d_pm25_it1",
  x_col      = "income_mean",
  y_label    = station_scatter_labels[["hrs_d_pm25_it1"]],
  x_label    = "Average monthly labour income")

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save education panels, retaining the manuscript's Santiago filename spelling
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

for (outcome in names(station_scatter_tags)) {
  tag <- station_scatter_tags[[outcome]]
  santiago_tag <- if (tag == "IT2_pm25_2023") "pm25_IT2_2023" else tag

  ggplot2::ggsave(
    filename  = here::here(outdir_fig, paste0("scatter_plot_bogota_", tag, ".pdf")),
    plot      = plots_bogota[[outcome]],
    device    = grDevices::cairo_pdf,
    width     = 8.5, height = 5.8, dpi = 300,
    bg        = "white", limitsize = FALSE)

  ggplot2::ggsave(
    filename  = here::here(outdir_fig, paste0("scatter_plot_mexico_", tag, ".pdf")),
    plot      = plots_cdmx[[outcome]],
    device    = grDevices::cairo_pdf,
    width     = 8.5, height = 5.8, dpi = 300,
    bg        = "white", limitsize = FALSE)

  ggplot2::ggsave(
    filename  = here::here(outdir_fig,
      paste0("scatter_plot_santiago_", santiago_tag, ".pdf")),
    plot      = plots_santiago[[outcome]],
    device    = grDevices::cairo_pdf,
    width     = 8.5, height = 5.8, dpi = 300,
    bg        = "white", limitsize = FALSE)

  ggplot2::ggsave(
    filename  = here::here(outdir_fig, paste0("scatter_plot_saopaulo_", tag, ".pdf")),
    plot      = plots_sp[[outcome]],
    device    = grDevices::cairo_pdf,
    width     = 8.5, height = 5.8, dpi = 300,
    bg        = "white", limitsize = FALSE)

}

# Save the four income panels
ggplot2::ggsave(filename  = here::here(outdir_fig, "scatter_plot_mexico_2023_income.pdf"),
                plot      = income_cdmx_pm10,
                device    = grDevices::cairo_pdf,
                width     = 8.5, height = 5.8, dpi = 300,
                bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "scatter_plot_mexico_pm25_2023_income.pdf"),
  plot      = income_cdmx_pm25,
  device    = grDevices::cairo_pdf,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(filename  = here::here(outdir_fig, "scatter_plot_saopaulo_2023_income.pdf"),
                plot      = income_sp_pm10,
                device    = grDevices::cairo_pdf,
                width     = 8.5, height = 5.8, dpi = 300,
                bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "scatter_plot_saopaulo_pm25_2023_income.pdf"),
  plot      = income_sp_pm25,
  device    = grDevices::cairo_pdf,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

