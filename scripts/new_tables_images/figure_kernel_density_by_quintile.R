# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Weighted kernel-density plots of PM exposure by education quintile.
#
# @Description: Rebuild of the legacy inputs/1_kernel_plots_quintiles_3km.R and
# _20km.R. One overlay plot per city × pollutant × buffer lands in
# results/figures/exposure_by_quintiles/kernel/.
#
# @Summary:
#   I.   Define exposure directories produced by aggregate_exposure_all_cities.R
#   II.  Call plot_kernel_density_by_quintile() for each configuration
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
outdir_figs <- here::here("results", "figures", "exposure_by_quintiles", "kernel")
dir.create(outdir_figs, recursive = TRUE, showWarnings = FALSE)

specs <- list(
  list(label = "Bogotá",      dir = here::here(dir_exp, "bogota_2018"),
       prefix = "bogota_2018_3km",    pop_col = "fe", mode = "individual")#,
#   list(label = "Bogotá",      dir = here::here(dir_exp, "bogota_2018"),
#        prefix = "bogota_2018_5km",    pop_col = "fe", mode = "individual"),
#   list(label = "Mexico City", dir = here::here(dir_exp, "cdmx"),
#        prefix = "cdmx_3km",           pop_col = "n",  mode = "geo"),
#   list(label = "Santiago",    dir = here::here(dir_exp, "santiago_2024"),
#        prefix = "santiago_2024_3km",  pop_col = "n",  mode = "geo"),
#   list(label = "São Paulo",   dir = here::here(dir_exp, "sao_paulo_2010"),
#        prefix = "sao_paulo_2010_3km", pop_col = "n",  mode = "geo")
)

# ============================================================================================
# II and III: Render + save
# ============================================================================================
for (s in specs) {
  exp_pq <- file.path(s$dir, paste0(s$prefix, "_idw_exposure.parquet"))
  if (!file.exists(exp_pq)) {
    message("[", s$label, "] Missing exposure Parquet — skipping (", exp_pq, ")")
    next
  }
  for (pol in c("pm10", "pm25")) {
    p <- plot_kernel_density_by_quintile(
      exposure_dir   = s$dir,
      out_name       = s$prefix,
      pollutant      = pol,
      quintile_level = s$mode,
      pop_col        = s$pop_col,
      year_filter    = 2023,
      city_label     = s$label
    )
    out_file <- file.path(
      outdir_figs,
      sprintf("%s_%s_kernel.pdf",
              s$prefix, pol)
    )
    ggplot2::ggsave(out_file, p, device = cairo_pdf,
                    width = 6, height = 4.5, dpi = 300, bg = "white")
  }
  message("[", s$label, "] wrote kernel plots → ", outdir_figs)
}

cat("Script from the IDB project executed successfully in the Docker container!\n")
