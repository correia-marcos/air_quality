# ======================================================================================
# IDB: Air monitoring
# ======================================================================================
# @Goal:
#   Generate station-monitoring figures for the paper.
#
# @Description:
#   This script uses processed distance matrices and station-level socioeconomic
#   exposure data to create:
#   1. Station coverage and distance plots by education.
#   2. Station-level average pollution versus education plots.
#   3. Station-level hours above WHO thresholds versus education plots.
#
# @Date: June 2026
# @Author: Marcos
# ======================================================================================


# ======================================================================================
# I. Setup
# ======================================================================================

# Source project functions, if needed for paths/configs.
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# Required packages for the plotting script.
req_pkgs <- c("arrow", "data.table", "ggplot2", "scales", "stringi", "here")

# Check package availability before running.
for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("Package missing: ", p)
  }
}

# Optional package for cleaner station labels.
# Define input folders.
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_station   <- here::here("data", "processed", "station_socio_exposure")
dir_census    <- here::here("data", "interim", "census")

# Define output folder.
outdir_fig <- here::here("results", "figures", "station_monitoring")
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

# Visual style now lives with the plotting functions in src/, as defaulted arguments.



# Define distance matrix paths.
dist_bogota <- here::here(
  dir_distances, "bogota_2018", "matrix_geo_station_distances.parquet"
)
dist_cdmx <- here::here(
  dir_distances, "cdmx_2020", "matrix_geo_station_distances.parquet"
)
dist_santiago <- here::here(
  dir_distances, "santiago_2024", "matrix_geo_station_distances.parquet"
)
dist_sp <- here::here(
  dir_distances, "sao_paulo_2010", "matrix_geo_station_distances.parquet"
)

# Define station-socioeconomic paths.
station_bogota_pq <- here::here(
  dir_station, "bogota_2018", "bogota_2018_2023_3km_station_socio.parquet"
)
station_cdmx_pq <- here::here(
  dir_station, "cdmx_2020", "cdmx_2020_2023_station_socio.parquet"
)
station_santiago_pq <- here::here(
  dir_station, "santiago_2024", "santiago_2024_2023_station_socio.parquet"
)
station_sp_pq <- here::here(
  dir_station, "sao_paulo_2010", "sao_paulo_2010_2023_station_socio.parquet"
)

# Define collapsed census paths.
census_bogota_pq <- here::here(
  dir_census, "bogota_2018", "census_2018_metro_collapsed.parquet"
)
census_cdmx_pq <- here::here(
  dir_census, "cdmx_extended_2020", "collapse_metro_area_2020.parquet"
)
census_santiago_pq <- here::here(
  dir_census, "santiago_2024", "census_santiago_collapsed_2024.parquet"
)
census_sp_pq <- here::here(
  dir_census, "sao_paulo_2010", "census_sp_collapsed_2010.parquet"
)


# ======================================================================================
# III. Read processed data
# ======================================================================================

# Read processed station-socioeconomic data.
station_bogota <- safe_read_parquet(station_bogota_pq)
station_cdmx <- safe_read_parquet(station_cdmx_pq)
station_santiago <- safe_read_parquet(station_santiago_pq)
station_sp <- safe_read_parquet(station_sp_pq)

# Read collapsed census data.
census_bogota <- safe_read_parquet(census_bogota_pq)
census_cdmx <- safe_read_parquet(census_cdmx_pq)
census_santiago <- safe_read_parquet(census_santiago_pq)
census_sp <- safe_read_parquet(census_sp_pq)


# ======================================================================================
# IV. Generate and save figures
# ======================================================================================

# 1. Bogotá.
plots_bogota <- save_city_monitoring_figures(
  city_label = "Bogota",
  city_id = "bogota_2018",
  dist_pq = dist_bogota,
  census_dt = census_bogota,
  station_dt = station_bogota,
  geo_id_col = "GEO_ID",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

# 2. Mexico City.
plots_cdmx <- save_city_monitoring_figures(
  city_label = "Mexico City",
  city_id = "cdmx_2020",
  dist_pq = dist_cdmx,
  census_dt = census_cdmx,
  station_dt = station_cdmx,
  geo_id_col = "CVE_MUN",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

# 3. Gran Santiago.
plots_santiago <- save_city_monitoring_figures(
  city_label = "Gran Santiago",
  city_id = "santiago_2024",
  dist_pq = dist_santiago,
  census_dt = census_santiago,
  station_dt = station_santiago,
  geo_id_col = "CUT",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

# 4. Sao Paulo.
plots_sp <- save_city_monitoring_figures(
  city_label = "Sao Paulo",
  city_id = "sao_paulo_2010",
  dist_pq = dist_sp,
  census_dt = census_sp,
  station_dt = station_sp,
  geo_id_col = "code_weighting",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

cat("Station-monitoring figures saved to: ", outdir_fig, "\n")
