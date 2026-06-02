# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Bar charts of population-weighted hours above WHO interim targets (IT1, IT2)
#         by education quintile.
#
# @Description: Calls `plot_hours_above_target_by_quintile()` across the 4 cities × 2
# pollutants (pm10, pm25) × 2 WHO interim targets (it1, it2). Outputs land in
# results/figures/hour_above_iterim_target/by_quintile/.
#
# @Summary:
#   I.   Load the exposure Parquets produced by aggregate_exposure_all_cities.R
#   II.  Plot each (city × pollutant × WHO IT) combination
#   III. Save PDFs
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
dir_exp     <- here::here("data", "processed", "idw_estimates")
outdir_figs <- here::here("results", "figures", "hour_above_iterim_target", "by_quintile")
dir.create(outdir_figs, recursive = TRUE, showWarnings = FALSE)

specs <- list(
  list(label = "Bogotá",      dir = here::here(dir_exp, "bogota_2018"),
       prefix = "bogota_2018_3km",    pop_col = "fe", mode = "individual") #,
  #list(label = "Mexico City", dir = here::here(dir_exp, "cdmx"),
  #     prefix = "cdmx_3km",           pop_col = "n",  mode = "geo"),
  #list(label = "Santiago",    dir = here::here(dir_exp, "santiago_2024"),
  #     prefix = "santiago_2024_3km",  pop_col = "n",  mode = "geo"),
  #list(label = "São Paulo",   dir = here::here(dir_exp, "sao_paulo_2010"),
  #     prefix = "sao_paulo_2010_3km", pop_col = "n",  mode = "geo")
)

# ============================================================================================
# II and III: Render + save
# ============================================================================================
for (s in specs) {
  exp_pq <- file.path(s$dir, paste0(s$prefix, "_idw_exposure.parquet"))
  if (!file.exists(exp_pq)) {
    message("[", s$label, "] Missing exposure Parquet — skipping.")
    next
  }
  for (pol in c("pm10", "pm25"))
    for (tgt in c("it1", "it2")) {
      p <- plot_hours_above_target_by_quintile(
        exposure_dir   = s$dir,
        out_name       = s$prefix,
        quintile_level = s$mode,
        pop_col        = s$pop_col,
        pollutant      = pol,
        who_it         = tgt,
        year_filter    = 2023,
        city_label     = s$label
      )
      out_file <- file.path(
        outdir_figs,
        sprintf("%s_%s_%s_hours_above.pdf", s$prefix, pol, tgt)
      )
      ggplot2::ggsave(out_file, p, device = cairo_pdf,
                      width = 6, height = 4.5, dpi = 300, bg = "white")
    }
  message("[", s$label, "] wrote hours-above plots → ", outdir_figs)
}

cat("Script from the IDB project executed successfully in the Docker container!\n")
