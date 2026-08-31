# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Plot how monitoring coverage and station-level pollution relate to education.
#
#' @Description: Reads the distance matrices and the station-level socioeconomic exposure
# data from compute_station_scatter_inputs.R and draws two families. The first counts the
# stations within a radius of each census unit and the distance to the nearest one, at
# 3, 5 and 10 km, ordered by the unit's mean years of schooling; the paper prints the PM10
# panel of each. The second puts station-level annual means and hours above the WHO
# interim targets against the education of the unit the station sits in. Santiago uses the
# 2017 zonas censales, matching the main exposure specification.
#
#' @Summary:
#   I.   Setup: load dependencies, set the paper theme, define paths.
#   II.  Read the station, census and distance inputs.
#   III. Station coverage and nearest-distance figures, one pass per radius.
#   IV.  Station-level pollution versus education figures.
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
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_station   <- here::here("data", "processed", "station_socio_exposure")
dir_census    <- here::here("data", "interim", "census")

outdir_paper <- here::here("results", "paper", "figures", "monitoring_coverage")
outdir_fig   <- here::here("results", "figures", "station_monitoring")

dir.create(outdir_paper, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

# Radii the paper reports: 3 km in the main text, 5 and 10 km in the appendix.
radii_km <- c(3, 5, 10)

# Define geo-to-station distance matrix paths
dist_bogota   <- here::here(dir_distances, "bogota_2018",
                            "matrix_geo_station_distances.parquet")
dist_cdmx     <- here::here(dir_distances, "cdmx_2020",
                            "matrix_geo_station_distances.parquet")
dist_santiago <- here::here(dir_distances, "santiago_2017",
                            "matrix_geo_station_distances.parquet")
dist_sp       <- here::here(dir_distances, "sao_paulo_2010",
                            "matrix_geo_station_distances.parquet")

# Define station-level socioeconomic exposure paths
station_bogota_pq   <- here::here(dir_station, "bogota_2018",
                                  "bogota_2018_2023_3km_station_socio.parquet")
station_cdmx_pq     <- here::here(dir_station, "cdmx_2020",
                                  "cdmx_2020_2023_station_socio.parquet")
station_santiago_pq <- here::here(dir_station, "santiago_2017",
                                  "santiago_2017_2023_station_socio.parquet")
station_sp_pq       <- here::here(dir_station, "sao_paulo_2010",
                                  "sao_paulo_2010_2023_station_socio.parquet")

# Define collapsed census paths
census_bogota_pq   <- here::here(dir_census, "bogota_2018",
                                 "census_2018_metro_collapsed.parquet")
census_cdmx_pq     <- here::here(dir_census, "cdmx_extended_2020",
                                 "collapse_metro_area_2020.parquet")
census_santiago_pq <- here::here(dir_census, "santiago_2017",
                                 "census_collapsed_2017.parquet")
census_sp_pq       <- here::here(dir_census, "sao_paulo_2010",
                                 "census_sp_collapsed_2010.parquet")

# ============================================================================================
# II: Read processed data
# ============================================================================================
station_bogota   <- safe_read_parquet(station_bogota_pq)
station_cdmx     <- safe_read_parquet(station_cdmx_pq)
station_santiago <- safe_read_parquet(station_santiago_pq)
station_sp       <- safe_read_parquet(station_sp_pq)

census_bogota   <- safe_read_parquet(census_bogota_pq)
census_cdmx     <- safe_read_parquet(census_cdmx_pq)
census_santiago <- safe_read_parquet(census_santiago_pq)
census_sp       <- safe_read_parquet(census_sp_pq)

# ============================================================================================
# III: Station coverage and nearest-distance figures
# ============================================================================================
# Only the station count depends on the radius, so the radius is the loop and the cities
# are written out. File names use the manuscript's city spelling.
for (radius_km in radii_km) {

  dist_figs_bogota <- save_station_distance_figures(
    city_label   = "Bogota",
    paper_city   = "bogota",
    dist_pq      = dist_bogota,
    census_dt    = census_bogota,
    station_dt   = station_bogota,
    radius_km    = radius_km,
    outdir_paper = outdir_paper,
    outdir_repo  = outdir_fig)

  dist_figs_cdmx <- save_station_distance_figures(
    city_label   = "Mexico City",
    paper_city   = "mexico",
    dist_pq      = dist_cdmx,
    census_dt    = census_cdmx,
    station_dt   = station_cdmx,
    radius_km    = radius_km,
    outdir_paper = outdir_paper,
    outdir_repo  = outdir_fig)

  dist_figs_santiago <- save_station_distance_figures(
    city_label   = "Gran Santiago",
    paper_city   = "santiago",
    dist_pq      = dist_santiago,
    census_dt    = census_santiago,
    station_dt   = station_santiago,
    radius_km    = radius_km,
    outdir_paper = outdir_paper,
    outdir_repo  = outdir_fig)

  dist_figs_sp <- save_station_distance_figures(
    city_label   = "Sao Paulo",
    paper_city   = "saopaulo",
    dist_pq      = dist_sp,
    census_dt    = census_sp,
    station_dt   = station_sp,
    radius_km    = radius_km,
    outdir_paper = outdir_paper,
    outdir_repo  = outdir_fig)
}

# ============================================================================================
# IV: Station-level pollution versus education
# ============================================================================================
# Nothing here depends on the radius: each point is one station's own 2023 record.
edu_figs_bogota <- save_station_education_figures(
  city_label = "Bogota",
  city_id    = "bogota_2018",
  station_dt = station_bogota,
  outdir_fig = outdir_fig)

edu_figs_cdmx <- save_station_education_figures(
  city_label = "Mexico City",
  city_id    = "cdmx_2020",
  station_dt = station_cdmx,
  outdir_fig = outdir_fig)

edu_figs_santiago <- save_station_education_figures(
  city_label = "Gran Santiago",
  city_id    = "santiago_2017",
  station_dt = station_santiago,
  outdir_fig = outdir_fig)

edu_figs_sp <- save_station_education_figures(
  city_label = "Sao Paulo",
  city_id    = "sao_paulo_2010",
  station_dt = station_sp,
  outdir_fig = outdir_fig)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
