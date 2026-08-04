# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Show how PM exposure differs across the education distribution, as densities and
#   as hours above the WHO interim targets.
#
# @Description: Two views of one question, so they share one specification list rather
# than drifting apart in two files. Kernel densities show the whole exposure distribution
# per quintile; the bar charts reduce it to the policy-relevant count of hours above WHO
# IT1 and IT2. Both read the IDW exposure Parquets from estimate_idw_exposure.R and write
# PDFs under results/figures/.
#
# @Summary:
#   I.   Import data: one spec per city, naming its exposure folder and weight column.
#   II.  Kernel densities by quintile.
#   III. Hours above WHO interim targets by quintile.
#
# @Date: August 2026
# @Author: Marcos
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
dir_exp        <- here::here("data", "processed", "idw_estimates")
outdir_kernel  <- here::here("results", "figures", "exposure_by_quintiles", "kernel")
outdir_hours   <- here::here("results", "figures", "hour_above_iterim_target",
                             "by_quintile")

analysis_year <- 2023
pollutants    <- c("pm10", "pm25")
who_targets   <- c("it1", "it2")

# Plot geometry, shared by both figure families.
fig_width  <- 6
fig_height <- 4.5
fig_dpi    <- 300

# One row per city. Bogota carries individual-level microdata so its quintiles are cut on
# people; the other three only reach geographic units, so they are cut on units. Santiago
# uses the 2017 zonas censales, the paper's main specification, not the 2024 communes.
specs <- list(
  list(label = "Bogotá",      dir = here::here(dir_exp, "bogota_2018"),
       prefix = "bogota_2018_3km",    pop_col = "fe", mode = "individual"),
  list(label = "Mexico City", dir = here::here(dir_exp, "cdmx_2020"),
       prefix = "cdmx_2020_3km",      pop_col = "n",  mode = "geo"),
  list(label = "Santiago",    dir = here::here(dir_exp, "santiago_2017"),
       prefix = "santiago_2017_3km",  pop_col = "n",  mode = "geo"),
  list(label = "São Paulo",   dir = here::here(dir_exp, "sao_paulo_2010"),
       prefix = "sao_paulo_2010_3km", pop_col = "n",  mode = "geo")
)

# Kernel densities currently cover Bogota only. The other three were commented out during
# development and have not been re-checked; widen to seq_along(specs) once they are.
kernel_specs <- specs[1]

# ============================================================================================
# II: Kernel densities by quintile
# ============================================================================================
dir.create(outdir_kernel, recursive = TRUE, showWarnings = FALSE)

for (s in kernel_specs) {
  for (pol in pollutants) {
    p <- plot_kernel_density_by_quintile(
      exposure_dir   = s$dir,
      out_name       = s$prefix,
      pollutant      = pol,
      quintile_level = s$mode,
      pop_col        = s$pop_col,
      year_filter    = analysis_year,
      city_label     = s$label)

    ggplot2::ggsave(
      file.path(outdir_kernel, sprintf("%s_%s_kernel.pdf", s$prefix, pol)),
      p, device = cairo_pdf,
      width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  }
  message("[", s$label, "] wrote kernel plots → ", outdir_kernel)
}

# ============================================================================================
# III: Hours above WHO interim targets by quintile
# ============================================================================================
dir.create(outdir_hours, recursive = TRUE, showWarnings = FALSE)

for (s in specs) {
  for (pol in pollutants) {
    for (tgt in who_targets) {
      p <- plot_hours_above_target_by_quintile(
        exposure_dir   = s$dir,
        out_name       = s$prefix,
        quintile_level = s$mode,
        pop_col        = s$pop_col,
        pollutant      = pol,
        who_it         = tgt,
        year_filter    = analysis_year,
        city_label     = s$label)

      ggplot2::ggsave(
        file.path(outdir_hours,
                  sprintf("%s_%s_%s_hours_above.pdf", s$prefix, pol, tgt)),
        p, device = cairo_pdf,
        width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
    }
  }
  message("[", s$label, "] wrote hours-above plots → ", outdir_hours)
}

cat("Script from the IDB project executed successfully in the Docker container!\n")
