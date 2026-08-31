# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Plot each station's 2023 pollution against the socioeconomic profile of the unit
#   it sits in, one pollutant and one outcome at a time.
#
#' @Description: Reads the station-level socioeconomic exposure data written by
# compute_station_scatter_inputs.R and draws the manuscript's scatter family: hours above
# each WHO interim target and the annual mean, for PM10 and PM2.5, against mean years of
# schooling, plus the income variants for the two cities whose census carries income.
# One station per point with an ordinary least squares fit. Every figure is written under
# the file name the manuscript cites, in results/paper/figures/station_scatters/.
#
#' @Summary:
#   I.   Setup: load dependencies, set the paper theme, define paths.
#   II.  Read the station-level inputs.
#   III. Education scatters: hours above IT1 and IT2, then annual means.
#   IV.  Income scatters: Mexico City and Sao Paulo only.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Setup
# ============================================================================================
# Define input and output folders
dir_station <- here::here("data", "processed", "station_socio_exposure")
outdir_fig  <- here::here("results", "paper", "figures", "station_scatters")

dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

# Axis labels shared by every panel of a given outcome.
lab_education <- "Average years of schooling"
lab_income    <- "Average monthly labour income"

# Define station-level socioeconomic exposure paths
station_bogota_pq   <- here::here(dir_station, "bogota_2018",
                                  "bogota_2018_2023_3km_station_socio.parquet")
station_cdmx_pq     <- here::here(dir_station, "cdmx_2020",
                                  "cdmx_2020_2023_station_socio.parquet")
station_santiago_pq <- here::here(dir_station, "santiago_2017",
                                  "santiago_2017_2023_station_socio.parquet")
station_sp_pq       <- here::here(dir_station, "sao_paulo_2010",
                                  "sao_paulo_2010_2023_station_socio.parquet")

# ============================================================================================
# II: Read processed data
# ============================================================================================
station_bogota   <- safe_read_parquet(station_bogota_pq)
station_cdmx     <- safe_read_parquet(station_cdmx_pq)
station_santiago <- safe_read_parquet(station_santiago_pq)
station_sp       <- safe_read_parquet(station_sp_pq)

# Sao Paulo stores education x1000 in the source census; correct it once per city.
station_bogota   <- rescale_station_education(station_bogota, "Bogota")
station_cdmx     <- rescale_station_education(station_cdmx, "Mexico City")
station_santiago <- rescale_station_education(station_santiago, "Gran Santiago")
station_sp       <- rescale_station_education(station_sp, "Sao Paulo")

# ============================================================================================
# III: Station outcomes versus education
# ============================================================================================
# The six education panels differ only in which outcome column they read and what the
# manuscript calls the file, so the outcome is the loop and the cities are written out.
scatter_specs <- list(
  list(y_col = "hrs_d_pm10_it1", tag = "2023",
       y_label = "Hours above PM10 IT1 in 2023"),
  list(y_col = "hrs_d_pm25_it1", tag = "pm25_2023",
       y_label = "Hours above PM2.5 IT1 in 2023"),
  list(y_col = "hrs_d_pm10_it2", tag = "IT2_2023",
       y_label = "Hours above PM10 IT2 in 2023"),
  list(y_col = "hrs_d_pm25_it2", tag = "IT2_pm25_2023",
       y_label = "Hours above PM2.5 IT2 in 2023"),
  list(y_col = "avg_pm10", tag = "2023_pm10mean",
       y_label = "Mean annual PM10 concentration in 2023"),
  list(y_col = "avg_pm25", tag = "2023_pm25mean",
       y_label = "Mean annual PM2.5 concentration in 2023")
)

for (spec in scatter_specs) {

  plot_station_scatter(
    station_dt = station_bogota,
    y_col      = spec$y_col,
    x_col      = "education_mean",
    y_label    = spec$y_label,
    x_label    = lab_education,
    out_file   = file.path(outdir_fig,
                           paste0("scatter_plot_bogota_", spec$tag, ".pdf")))

  plot_station_scatter(
    station_dt = station_cdmx,
    y_col      = spec$y_col,
    x_col      = "education_mean",
    y_label    = spec$y_label,
    x_label    = lab_education,
    out_file   = file.path(outdir_fig,
                           paste0("scatter_plot_mexico_", spec$tag, ".pdf")))

  # Santiago's PM2.5 IT2 file is the one place the manuscript swaps the two tokens.
  santiago_tag <- if (spec$tag == "IT2_pm25_2023") "pm25_IT2_2023" else spec$tag

  plot_station_scatter(
    station_dt = station_santiago,
    y_col      = spec$y_col,
    x_col      = "education_mean",
    y_label    = spec$y_label,
    x_label    = lab_education,
    out_file   = file.path(outdir_fig,
                           paste0("scatter_plot_santiago_", santiago_tag, ".pdf")))

  plot_station_scatter(
    station_dt = station_sp,
    y_col      = spec$y_col,
    x_col      = "education_mean",
    y_label    = spec$y_label,
    x_label    = lab_education,
    out_file   = file.path(outdir_fig,
                           paste0("scatter_plot_saopaulo_", spec$tag, ".pdf")))
}

# ============================================================================================
# IV: Station outcomes versus income
# ============================================================================================
# Only Mexico City and Sao Paulo have income in the census.
plot_station_scatter(
  station_dt = station_cdmx,
  y_col      = "hrs_d_pm10_it1",
  x_col      = "income_mean",
  y_label    = "Hours above PM10 IT1 in 2023",
  x_label    = lab_income,
  out_file   = file.path(outdir_fig, "scatter_plot_mexico_2023_income.pdf"))

plot_station_scatter(
  station_dt = station_cdmx,
  y_col      = "hrs_d_pm25_it1",
  x_col      = "income_mean",
  y_label    = "Hours above PM2.5 IT1 in 2023",
  x_label    = lab_income,
  out_file   = file.path(outdir_fig, "scatter_plot_mexico_pm25_2023_income.pdf"))

plot_station_scatter(
  station_dt = station_sp,
  y_col      = "hrs_d_pm10_it1",
  x_col      = "income_mean",
  y_label    = "Hours above PM10 IT1 in 2023",
  x_label    = lab_income,
  out_file   = file.path(outdir_fig, "scatter_plot_saopaulo_2023_income.pdf"))

plot_station_scatter(
  station_dt = station_sp,
  y_col      = "hrs_d_pm25_it1",
  x_col      = "income_mean",
  y_label    = "Hours above PM2.5 IT1 in 2023",
  x_label    = lab_income,
  out_file   = file.path(outdir_fig, "scatter_plot_saopaulo_pm25_2023_income.pdf"))

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
