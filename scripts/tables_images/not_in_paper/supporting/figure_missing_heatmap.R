# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Month × hour heatmaps of the share of missing PM readings per city.
#
#' @Description: Month × hour missing-pattern diagnostic. For each city, DuckDB computes
# the exact two-way share of missing hours (month × hour) in the standardized station
# panel, before cleaning and over all years, and `plot_missing_heatmap()` renders it.
# One PDF per city and pollutant in results/figures/diagnostics/.
#
#' @Summary:
#   I.   Import data: source functions, set the theme and declare paths.
#   II.  Build figures: one heatmap per city and pollutant.
#   III. Save figures.
#
#' @Date: April 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# Set the source folder and the output folder.
dir_pollution <- here::here("data", "interim", "monitoring_stations")
outdir_figs   <- here::here("results", "figures", "diagnostics")
dir.create(outdir_figs, recursive = TRUE, showWarnings = FALSE)

# Set each city's station dataset, named by the city id used in file names.
arrow_dirs <- c(
  bogota    = here::here(dir_pollution, "bogota_metro_dataset"),
  cdmx      = here::here(dir_pollution, "cdmx_metro_dataset"),
  santiago  = here::here(dir_pollution, "santiago_metro_dataset"),
  sao_paulo = here::here(dir_pollution, "sao_paulo_metro_dataset"))

# Set the plot titles.
city_labels <- c(bogota = "Bogotá", cdmx = "Mexico City", santiago = "Santiago",
                 sao_paulo = "São Paulo")

# ========================================================================================
# II: Build figures
# ========================================================================================
# Compute the two-way missing share from each dataset and draw it, per pollutant.
plots <- list()
for (city in names(arrow_dirs)) {
  if (!dir.exists(arrow_dirs[[city]])) {
    message("[", city, "] Arrow dataset not found — skipping.")
    next
  }

  for (pol in c("pm10", "pm25")) {
    name <- sprintf("missing_%s_month_hour_%s", city, pol)
    plots[[name]] <- plot_missing_heatmap(missing_list = list(),
                                          row_dim      = "month",
                                          col_dim      = "hour",
                                          pollutant    = pol,
                                          city_label   = city_labels[[city]],
                                          arrow_dir    = arrow_dirs[[city]])
  }
}

# ========================================================================================
# III: Save figures
# ========================================================================================
# Save each heatmap as a PDF.
for (name in names(plots)) {
  ggplot2::ggsave(here::here(outdir_figs, paste0(name, ".pdf")),
    plot = plots[[name]], device = cairo_pdf,
    width = 7, height = 5, dpi = 300, bg = "white")
}
