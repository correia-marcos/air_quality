# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Month × hour heatmaps of the share of missing PM readings per city.
#
# @Description: Rebuild of the missing-pattern diagnostic from legacy
# 7_missing_analysis.do. For each city we query DuckDB for the exact two-way
# share of missing observations (month × hour) and render it with
# `plot_missing_heatmap()`. One PDF per (city × pollutant) in
# results/figures/joint_plots/missing_heatmap/.
#
# @Summary:
#   I.   Define Arrow datasets + output folder
#   II.  Call plot_missing_heatmap() per (city × pollutant)
#   III. Save PDFs
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
dir_pollution  <- here::here("data", "raw", "monitoring_stations")
dir_missing    <- here::here("data", "processed", "missing_proportions")
outdir_figs    <- here::here("results", "figures", "joint_plots", "missing_heatmap")
dir.create(outdir_figs, recursive = TRUE, showWarnings = FALSE)

arrow_dirs <- list(
  "Bogotá"      = here::here(dir_pollution, "bogota_metro_dataset"),
  "Mexico City" = here::here(dir_pollution, "cdmx_metro_dataset"),
  "Santiago"    = here::here(dir_pollution, "santiago_metro_dataset"),
  "São Paulo"   = here::here(dir_pollution, "sao_paulo_metro_dataset")
)

# ============================================================================================
# II and III: Plot + save
# ============================================================================================
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
    ggplot2::ggsave(
      filename = file.path(outdir_figs,
                           sprintf("missing_%s_month_hour_%s.pdf", slug, pol)),
      plot     = p, device = cairo_pdf,
      width    = 7, height = 5, dpi = 300, bg = "white"
    )
  }
}

cat("Script from the IDB project executed successfully in the Docker container!\n")
