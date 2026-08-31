# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Produce the paper's Figure 1 — pooled PM10 and PM2.5 concentration distributions
#   in the four metropolitan areas — its appendix 2019/2022 panels, and the exceedance
#   companion.
#
#' @Description: Kernel densities of hourly station concentrations from the four cleaned
#   metro panels (monitoring_stations_outliers/<city>_metro_clean), one curve per city,
#   with the WHO 24-hour interim targets (IT1/IT2) as dashed lines; and a bar chart of
#   the share of station-hours at or above each target. The published figure's look —
#   Santiago red for contrast, the other three black and told apart by linetype, no fill
#   under the curves — is set once in the knob block in Section I and passed to every
#   call, so a trial run means editing a knob and re-running one block. Seven PDFs land
#   in results/figures/kernel_plots/.
#
#' @Summary:
#   I.    Import data: the four cities' cleaned station panels + the styling knobs.
#   II.   Figure 1 (2023): one pooled density per pollutant.
#   III.  Appendix panels: the same densities for 2019 and 2022.
#   IV.   Exceedance companion (2023): share of station-hours at or above IT1/IT2.
#   V.    Save the six density PDFs and the stacked exceedance PDF.
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

# The six density panels are manuscript figures; the exceedance-share bar chart is the
# repo's own companion, so the two go to different places.
outdir_paper <- here::here("results", "paper", "figures", "city_distributions")
outdir       <- here::here("results", "figures", "kernel_plots")

fig_width  <- 16
fig_height <- 9
fig_dpi    <- 300

# The trial-and-error panel: the figure's look lives here, every call below reads it.
city_colours <- c(
  "Bogotá"      = "black",
  "Mexico City" = "black",
  "São Paulo"   = "black",
  "Santiago"    = "red"
)

city_linetypes <- c(
  "Bogotá"      = "solid",
  "Mexico City" = "dashed",
  "São Paulo"   = "dotdash",
  "Santiago"    = "solid"
)

fill_alpha      <- 0
legend_position <- "bottom"

# List order is the legend order (the published figure's); colours come from the knobs,
# so reordering this list never reassigns a colour.
city_data <- list(
  "Bogotá"      = file.path(dir_stations, "bogota_metro_clean"),
  "Mexico City" = file.path(dir_stations, "cdmx_metro_clean"),
  "São Paulo"   = file.path(dir_stations, "sao_paulo_metro_clean"),
  "Santiago"    = file.path(dir_stations, "santiago_metro_clean")
)

# ============================================================================================
# II: Process data — Figure 1 (2023)
# ============================================================================================
# x_max is a display-only zoom: the density is estimated on all hours of the year. A handful
# of unflagged values live far IT2 and would stretch the axis.
p_pm10 <- plot_kernel_density_by_city(
  city_data,
  pollutant       = "pm10",
  year            = 2023,
  x_max           = 500,
  city_colours    = city_colours,
  city_linetypes  = city_linetypes,
  fill_alpha      = fill_alpha,
  legend_position = legend_position)

p_pm25 <- plot_kernel_density_by_city(
  city_data,
  pollutant       = "pm25",
  year            = 2023,
  x_max           = 100,
  city_colours    = city_colours,
  city_linetypes  = city_linetypes,
  fill_alpha      = fill_alpha,
  legend_position = legend_position)

# ============================================================================================
# III: Process data — appendix panels (2019 and 2022)
# ============================================================================================
p_pm10_2019 <- plot_kernel_density_by_city(
  city_data,
  pollutant       = "pm10",
  year            = 2019,
  x_max           = 500,
  city_colours    = city_colours,
  city_linetypes  = city_linetypes,
  fill_alpha      = fill_alpha,
  legend_position = legend_position)

p_pm25_2019 <- plot_kernel_density_by_city(
  city_data,
  pollutant       = "pm25",
  year            = 2019,
  x_max           = 250,
  city_colours    = city_colours,
  city_linetypes  = city_linetypes,
  fill_alpha      = fill_alpha,
  legend_position = legend_position)

p_pm10_2022 <- plot_kernel_density_by_city(
  city_data,
  pollutant       = "pm10",
  year            = 2022,
  x_max           = 500,
  city_colours    = city_colours,
  city_linetypes  = city_linetypes,
  fill_alpha      = fill_alpha,
  legend_position = legend_position)

p_pm25_2022 <- plot_kernel_density_by_city(
  city_data,
  pollutant       = "pm25",
  year            = 2022,
  x_max           = 250,
  city_colours    = city_colours,
  city_linetypes  = city_linetypes,
  fill_alpha      = fill_alpha,
  legend_position = legend_position)

# ============================================================================================
# IV: Process data — exceedance companion (2023)
# ============================================================================================
e_pm10 <- plot_exceedance_shares(
  city_data,
  pollutant       = "pm10",
  year            = 2023,
  legend_position = legend_position)

e_pm25 <- plot_exceedance_shares(
  city_data,
  pollutant       = "pm25",
  year            = 2023,
  legend_position = legend_position)

# The two exceedance panels stack into one figure; each panel is titled by its pollutant.
exceedance_combined <- cowplot::plot_grid(e_pm10, e_pm25, nrow = 2)

# ============================================================================================
# V: Save figures
# ============================================================================================
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_paper, recursive = TRUE, showWarnings = FALSE)

# Name = file stem; the list answers "which plot -> which file" at a glance. The PM10
# stems carry no pollutant token because that is how the manuscript cites them.
density_figures <- list(
  all           = p_pm10,
  all_pm25      = p_pm25,
  all_2019      = p_pm10_2019,
  all_pm25_2019 = p_pm25_2019,
  all_2022      = p_pm10_2022,
  all_pm25_2022 = p_pm25_2022
)

for (stem in names(density_figures)) {
  ggplot2::ggsave(
    filename = file.path(outdir_paper, paste0(stem, ".pdf")),
    plot     = density_figures[[stem]], device = cairo_pdf,
    width    = fig_width, height = fig_height, dpi = fig_dpi)
}

ggplot2::ggsave(
  filename = file.path(outdir, "exceedance_shares.pdf"),
  plot     = exceedance_combined, device = cairo_pdf,
  width    = fig_width, height = 12, dpi = fig_dpi)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
