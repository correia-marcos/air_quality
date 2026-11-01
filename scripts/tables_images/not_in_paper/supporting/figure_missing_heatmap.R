# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Month × hour heatmaps of the share of missing PM readings per city.
#
#' @Description: Month × hour missing-pattern diagnostic. For each city we query
# DuckDB for the exact two-way share of missing observations (month × hour) and
# render it with `plot_missing_heatmap()`. One PDF per (city × pollutant) in
# results/figures/diagnostics/.
#
#' @Summary:
#   I. Import data.
#   II. Build figures.
#   III. Save figures.
#
#' @Date: April 2026
#' @Author: Marcos
# ============================================================================================

# ============================================================================================
# I: Import data
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

dir_pollution  <- here::here("data", "interim", "monitoring_stations")
dir_missing    <- here::here("data", "processed", "missing_proportions")
outdir_figs    <- here::here("results", "figures", "diagnostics")
dir.create(outdir_figs, recursive = TRUE, showWarnings = FALSE)

arrow_dirs <- list(
  "Bogotá"      = here::here(dir_pollution, "bogota_metro_dataset"),
  "Mexico City" = here::here(dir_pollution, "cdmx_metro_dataset"),
  "Santiago"    = here::here(dir_pollution, "santiago_metro_dataset"),
  "São Paulo"   = here::here(dir_pollution, "sao_paulo_metro_dataset")
)

# ============================================================================================
# II: Build figures
# ============================================================================================
plots <- list()
for (city in names(arrow_dirs)) {
  adir <- arrow_dirs[[city]]
  if (!dir.exists(adir)) {
    message("[", city, "] Arrow dataset not found — skipping.")
    next
  }
  slug <- gsub("[^a-z0-9]", "_", tolower(city))

  # Use the existing 1-way tables only as a fallback; the DuckDB path below is
  # authoritative because it computes the exact two-way share.
  missing_1way <- tryCatch(
    list(
      month = arrow::read_parquet(file.path(dir_missing,
                                            paste0(slug, "_missing_by_month.parquet"))),
      hour  = arrow::read_parquet(file.path(dir_missing,
                                            paste0(slug, "_missing_by_hour.parquet")))
    ),
    error = function(e) NULL
  )

  for (pol in c("pm10", "pm25")) {
    p <- plot_missing_heatmap(
      missing_list = missing_1way %||% list(),
      row_dim      = "month",
      col_dim      = "hour",
      pollutant    = pol,
      city_label   = city,
      arrow_dir    = adir
    )
    name <- sprintf("missing_%s_month_hour_%s", slug, pol)
    plots[[name]] <- p
  }
}

# ============================================================================================
# III: Save figures
# ============================================================================================
for (name in names(plots)) {
  ggplot2::ggsave(here::here(outdir_figs, paste0(name, ".pdf")),
    plot = plots[[name]], device = cairo_pdf,
    width = 7, height = 5, dpi = 300, bg = "white")
}

cat("Script from the IDB project executed successfully in the Docker container!\n")
