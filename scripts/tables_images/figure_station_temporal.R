# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Render station hourly profiles and WHO episode durations.
#
#' @Description: Read the preserved merged city series from prepare_station_temporal.R.
# The four ridgelines and IT2 figure are manuscript artifacts; hourly bars and IT1
# are companion figures. The MERRA-2 timestamps remain part of the sample definition.
#
#' @Summary:
#   I.   Read prepared city series.
#   II.  Render hourly profiles.
#   III. Render episode durations.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
dir_series     <- here::here("data", "processed", "merra2_stations_pm25")

outdir_hourly  <- here::here("results", "figures", "temporal")
outdir_targets <- here::here("results", "figures", "temporal")

# Of the figures below, the manuscript prints only the ridgelines and the IT2 episode
# durations; those go to results/, the rest stay with the repo's own outputs.
outdir_paper   <- here::here("results", "figures", "temporal")

# Figure geometry and series styling, shared by every figure here.
fig_width      <- 16
fig_height     <- 9
fig_dpi        <- 300

bogota_pm25        <- read.csv(file.path(dir_series, "bogota_pm25_stations_merra2.csv"))
santiago_pm25      <- read.csv(file.path(dir_series, "santiago_pm25_stations_merra2.csv"))
ciudad_mexico_pm25 <- read.csv(file.path(dir_series,
                                         "ciudad_mexico_pm25_stations_merra2.csv"))
sao_paulo_pm25     <- read.csv(file.path(dir_series,
                                         "sao_paulo_pm25_stations_merra2.csv"))

# One row per city: the display label, the ASCII file stem and the merged series.
city_specs <- list(
  list(label = "Bogota",           stem = "bogota",        df = bogota_pm25),
  list(label = "Santiago",         stem = "santiago",      df = santiago_pm25),
  list(label = "Ciudad de México", stem = "ciudad_mexico", df = ciudad_mexico_pm25),
  list(label = "São Paulo",        stem = "sao_paulo",     df = sao_paulo_pm25)
)

# ============================================================================================
# II: Average pollution by hour of day
# ============================================================================================
dir.create(outdir_hourly, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_paper, recursive = TRUE, showWarnings = FALSE)

for (s in city_specs) {
  bar <- plot_hourly_avg_pollution(
    df = s$df, region_name = s$label, plot_ci = TRUE, bar_width = 0.7)

  ridge <- plot_hourly_ridgeline_pollution(
    df = s$df, region_name = s$label, pollution_var = "pm25_stations")

  ggplot2::ggsave(
    file.path(outdir_hourly, paste0(s$stem, "_bar_plot.pdf")),
    bar, device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)

  # Titles are dropped on save: the paper captions these figures itself.
  ggplot2::ggsave(
    file.path(outdir_paper, paste0(s$stem, "_ridge_plot.pdf")),
    ridge + ggplot2::labs(title = NULL) +
      ggplot2::theme(plot.title = ggplot2::element_blank()),
    device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)
}

# ============================================================================================
# III: Duration of episodes above the WHO interim targets
# ============================================================================================
dir.create(outdir_targets, recursive = TRUE, showWarnings = FALSE)

city_dfs <- stats::setNames(lapply(city_specs, `[[`, "df"),
                            vapply(city_specs, `[[`, character(1), "label"))

for (target in c("IT1", "IT2")) {
  p <- plot_time_spans_ridgeline(
    list_of_dfs   = city_dfs,
    target        = target,
    pollution_var = "pm25_stations")

  # The manuscript prints IT2 only; IT1 is the repo's own companion.
  out_dir <- if (target == "IT2") outdir_paper else outdir_targets

  ggplot2::ggsave(
    file.path(out_dir, paste0("distribution_hours_above_", target, ".pdf")),
    p + ggplot2::labs(title = NULL) +
      ggplot2::theme(plot.title = ggplot2::element_blank()),
    device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)
}

