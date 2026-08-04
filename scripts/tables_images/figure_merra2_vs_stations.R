# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Show how satellite-derived (MERRA-2) and ground-station PM2.5 compare, over time
#   and over the day, and what area one MERRA-2 cell actually covers.
#
# @Description: Every figure here answers the same question: can MERRA-2 stand in for a
# ground station? Sections II-V read the merged series from process_merra2_panels.R and
# compare the two sources at different aggregations; section VI draws the MERRA-2 grid
# over each metro area, which is the spatial version of the same caveat. Section III
# repeats the Santiago series with thermal-inversion hours dropped, because inversion days
# are where the two sources diverge most.
#
# @Summary:
#   I.   Import data: merged MERRA-2/station series, inversion flags, rasters, shapefiles.
#   II.  Time series per city, smoothed and raw.
#   III. Santiago excluding thermal-inversion hours.
#   IV.  Average pollution by hour of day.
#   V.   Duration of episodes above the WHO interim targets.
#   VI.  MERRA-2 grid footprint over each metro area.
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
dir_series     <- here::here("data", "processed", "merra2_stations_pm25")
dir_shapefiles <- here::here("data", "raw", "cities_shapefiles")

outdir_series  <- here::here("results", "figures", "time_series")
outdir_ti      <- here::here("results", "figures", "m2_stations_removed_ti_days")
outdir_hourly  <- here::here("results", "figures", "hour_average")
outdir_targets <- here::here("results", "figures", "hour_above_iterim_target")
outdir_maps    <- here::here("results", "figures", "maps")

# Figure geometry and series styling, shared by every figure here.
fig_width      <- 16
fig_height     <- 9
fig_dpi        <- 300
window_hours   <- 24
corr_method    <- "pearson"
color_merra2   <- "darkred"
color_stations <- "darkblue"

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

# Hourly thermal-inversion flags for Santiago, used by section III only.
santiago_thermal <- read.csv(here::here(
  "data", "processed", "thermal_inversion_dataframes",
  "Santiago_2023-01-01_2023-12-31_inversion.csv"))

# MERRA-2 rasters and metro boundaries for the grid figure
nc_files <- list.files(here::here("data", "raw", "merra2_aerosol_products"),
                       full.names = TRUE)

metro_specs <- list(
  list(sf = sf::st_read(file.path(dir_shapefiles, "Bogota_metro")),
       stem = "bogota",        label = "Bogotá Metro Area"),
  list(sf = sf::st_read(file.path(dir_shapefiles, "Mexico_city")),
       stem = "ciudad_mexico", label = "Ciudad de México"),
  list(sf = sf::st_read(file.path(dir_shapefiles, "Santiago")),
       stem = "santiago",      label = "Santiago"),
  list(sf = sf::st_read(file.path(dir_shapefiles, "Sao_Paulo")),
       stem = "sao_paulo",     label = "Sao Paulo")
)

# ============================================================================================
# II: Time series per city, smoothed and raw
# ============================================================================================
dir.create(outdir_series, recursive = TRUE, showWarnings = FALSE)

# Two versions of each series. The rolling mean is what the paper shows; the raw series is
# kept because the correlation annotation differs sharply between them.
for (s in city_specs) {
  smoothed <- plot_pm25_timeseries_smooth(
    df = s$df, region_name = s$label, apply_rolling = TRUE,
    window_hours = window_hours, corr_method = corr_method,
    color_merra2 = color_merra2, color_stations = color_stations)

  raw <- plot_pm25_timeseries_smooth(
    df = s$df, region_name = s$label, apply_rolling = FALSE,
    window_hours = window_hours, corr_method = corr_method,
    color_merra2 = color_merra2, color_stations = color_stations)

  ggplot2::ggsave(
    file.path(outdir_series, paste0(s$stem, "_moving_average_pm25.pdf")),
    smoothed, device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)

  ggplot2::ggsave(
    file.path(outdir_series, paste0(s$stem, "_time_series_pm25.pdf")),
    raw, device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)
}

# ============================================================================================
# III: Santiago excluding thermal-inversion hours
# ============================================================================================
dir.create(outdir_ti, recursive = TRUE, showWarnings = FALSE)

# Drop the hours flagged as thermal inversions, keeping unflagged and unmatched hours.
santiago_inversion <- santiago_thermal %>%
  dplyr::select(Day, Hour, Inversion_dummy)

santiago_pm25_no_ti <- santiago_pm25 %>%
  dplyr::left_join(santiago_inversion, by = c("Date" = "Day", "Hour" = "Hour")) %>%
  dplyr::filter(Inversion_dummy != 1 | is.na(Inversion_dummy)) %>%
  dplyr::select(-Inversion_dummy)

santiago_ti_smoothed <- plot_pm25_timeseries_smooth(
  df = santiago_pm25_no_ti, region_name = "Santiago", apply_rolling = TRUE,
  window_hours = window_hours, corr_method = corr_method,
  color_merra2 = color_merra2, color_stations = color_stations)

santiago_ti_raw <- plot_pm25_timeseries_smooth(
  df = santiago_pm25_no_ti, region_name = "Santiago", apply_rolling = FALSE,
  window_hours = window_hours, corr_method = corr_method,
  color_merra2 = color_merra2, color_stations = color_stations)

ggplot2::ggsave(
  file.path(outdir_ti, "santiago_24_hrs_ma_m2_stations_thermal_inversion.pdf"),
  santiago_ti_smoothed, device = cairo_pdf,
  width = fig_width, height = fig_height, dpi = fig_dpi)

ggplot2::ggsave(
  file.path(outdir_ti, "santiago_raw_m2_stations_thermal_inversion.pdf"),
  santiago_ti_raw, device = cairo_pdf,
  width = fig_width, height = fig_height, dpi = fig_dpi)

# ============================================================================================
# IV: Average pollution by hour of day
# ============================================================================================
dir.create(outdir_hourly, recursive = TRUE, showWarnings = FALSE)

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
    file.path(outdir_hourly, paste0(s$stem, "_ridge_plot.pdf")),
    ridge + ggplot2::labs(title = NULL) +
      ggplot2::theme(plot.title = ggplot2::element_blank()),
    device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)
}

# ============================================================================================
# V: Duration of episodes above the WHO interim targets
# ============================================================================================
dir.create(outdir_targets, recursive = TRUE, showWarnings = FALSE)

city_dfs <- stats::setNames(lapply(city_specs, `[[`, "df"),
                            vapply(city_specs, `[[`, character(1), "label"))

for (target in c("IT1", "IT2")) {
  p <- plot_time_spans_ridgeline(
    list_of_dfs   = city_dfs,
    target        = target,
    pollution_var = "pm25_stations")

  ggplot2::ggsave(
    file.path(outdir_targets, paste0("distribution_hours_above_", target, ".pdf")),
    p + ggplot2::labs(title = NULL) +
      ggplot2::theme(plot.title = ggplot2::element_blank()),
    device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)
}

# ============================================================================================
# VI: MERRA-2 grid footprint over each metro area
# ============================================================================================
dir.create(outdir_maps, recursive = TRUE, showWarnings = FALSE)

# Any raster shows the same cell geometry, so take the first for a reproducible figure.
# This was sample(nc_files, 1) with no seed, which redrew a different file each run.
merra_raster <- nc_files[1]

for (m in metro_specs) {
  p <- plot_merra2_grid_city(m$sf, merra_raster, m$label)

  ggplot2::ggsave(
    file.path(outdir_maps, paste0(m$stem, "_grid.pdf")),
    p, device = cairo_pdf,
    width = fig_width, height = fig_height, dpi = fig_dpi)
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
