# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Bar/line plots of PM exposure by education quintile with regression-based 95 % CIs.
#
# @Description: For each city, fits a weighted linear model with education-quintile fixed
# effects (reference = Q5) on each exposure outcome (avg_pm10, avg_pm25, hrs_d_*_it1,
# hrs_d_*_it2). The resulting coefficients + CIs drive `plot_exposure_by_quintile_with_ci()`.
# Outputs one PDF per (city × outcome × pollutant) combination in
# results/figures/exposure_by_quintiles/ci/.
#
# @Summary:
#   I.   Find the exposure Parquets produced by aggregate_exposure_all_cities.R
#   II.  Fit the CI regressions and render one plot per outcome
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
dir_exp     <- here::here("data", "processed", "idw_estimates")
outdir_figs <- here::here("results", "figures", "exposure_by_quintiles", "ci")
dir.create(outdir_figs, recursive = TRUE, showWarnings = FALSE)

# (city_label, exposure folder, out_name prefix, pop_col, quintile mode)
specs <- list(
  list(label = "Bogotá",      dir = here::here(dir_exp, "bogota_2018"),
       prefix = "bogota_2018_3km",    pop_col = "fe", mode = "individual")#,
#  list(label = "Mexico City", dir = here::here(dir_exp, "cdmx"),
#       prefix = "cdmx_3km",           pop_col = "n",  mode = "geo"),
#  list(label = "Santiago",    dir = here::here(dir_exp, "santiago_2024"),
#       prefix = "santiago_2024_3km",  pop_col = "n",  mode = "geo"),
#  list(label = "São Paulo",   dir = here::here(dir_exp, "sao_paulo_2010"),
#       prefix = "sao_paulo_2010_3km", pop_col = "n",  mode = "geo")
)

# ============================================================================================
# II and III: Fit + plot + save
# ============================================================================================
for (s in specs) {
  exp_pq <- file.path(s$dir, paste0(s$prefix, "_idw_exposure.parquet"))
  ind_pq <- file.path(s$dir, paste0(s$prefix, "_indiv_quintiles.parquet"))
  if (!file.exists(exp_pq)) {
    message("[", s$label, "] Missing exposure Parquet — skipping (", exp_pq, ")")
    next
  }
  exp_pq
  ind_pq
  
  ci_tbl <- compute_exposure_ci_regression(
    exposure_parquet = exp_pq,
    individual_pq    = if (s$mode == "individual") ind_pq else NULL,
    pop_col          = s$pop_col,
    pollutants       = c("pm10", "pm25"),
    year_filter      = 2023,
    base_quintile    = 5L
  )

  for (outcome in unique(ci_tbl$outcome)) {
    for (pol in c("pm10", "pm25")) {
      p <- plot_exposure_by_quintile_with_ci(
        ci_table   = ci_tbl,
        outcome    = outcome,
        pollutant  = pol,
        city_label = s$label
      )
      out_file <- file.path(
        outdir_figs,
        sprintf("%s_%s_%s_ci.pdf",
                gsub("[^a-z0-9]", "_", tolower(s$label)),
                outcome, pol)
      )
      ggplot2::ggsave(out_file, p, device = cairo_pdf,
                      width = 6, height = 4.5, dpi = 300, bg = "white")
    }
  }
  message("[", s$label, "] wrote CI plots → ", outdir_figs)
}

cat("Script from the IDB project executed successfully in the Docker container!\n")
