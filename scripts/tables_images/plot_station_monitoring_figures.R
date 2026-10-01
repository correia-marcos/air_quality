# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Plot station coverage and station outcomes by education.
#
#' @Description: Count active PM stations within 3, 5 and 10 km of each geographic
# unit and retain its nearest-station distance. Rank units by mean schooling and show
# cubic fitted trends. Companion plots compare each station's pollution with its census
# context, using separate PM10 and PM2.5 axes. Preserve the existing axis rescaling.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read inputs.
#   II.  Process data: build coverage tables, distance trends and education scatter plots.
#   III. Save outputs: save the results in the same order.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the scientific functions and their shared helpers
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "plot", "station_monitoring.R"))
source(here::here("src", "general_utilities", "theme_paper.R"))
source(here::here("config", "analysis_settings.R"))

# Use the manuscript font and theme
set_paper_theme()

# Set distance, station-summary and census folders, plus the figure destination
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_station   <- here::here("data", "processed", "station_socio_exposure")
dir_census    <- here::here("data", "interim", "census")
outdir_fig    <- here::here("results", "figures", "monitoring")

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

# Read station outcomes and collapsed census characteristics
station_bogota   <- safe_read_parquet(station_bogota_pq)
station_cdmx     <- safe_read_parquet(station_cdmx_pq)
station_santiago <- safe_read_parquet(station_santiago_pq)
station_sp       <- safe_read_parquet(station_sp_pq)

census_bogota   <- safe_read_parquet(census_bogota_pq)
census_cdmx     <- safe_read_parquet(census_cdmx_pq)
census_santiago <- safe_read_parquet(census_santiago_pq)
census_sp       <- safe_read_parquet(census_sp_pq)

# ========================================================================================
# II: Process data
# ========================================================================================
# Apply the schooling-unit correction and identify active stations by pollutant
station_bogota   <- rescale_station_education(station_bogota, "Bogota")
active_bogota    <- list(pm10 = get_active_station_ids(station_bogota, "pm10"),
                         pm25 = get_active_station_ids(station_bogota, "pm25"))

station_cdmx     <- rescale_station_education(station_cdmx, "Mexico City")
active_cdmx      <- list(pm10 = get_active_station_ids(station_cdmx, "pm10"),
                         pm25 = get_active_station_ids(station_cdmx, "pm25"))

station_santiago <- rescale_station_education(station_santiago, "Gran Santiago")
active_santiago  <- list(pm10 = get_active_station_ids(station_santiago, "pm10"),
                         pm25 = get_active_station_ids(station_santiago, "pm25"))

station_sp       <- rescale_station_education(station_sp, "Sao Paulo")
active_sp        <- list(pm10 = get_active_station_ids(station_sp, "pm10"),
                         pm25 = get_active_station_ids(station_sp, "pm25"))

# Keep the coverage tables and fitted plots for every radius and pollutant
coverage_bogota   <- distance_bogota <- list()
coverage_cdmx     <- distance_cdmx <- list()
coverage_santiago <- distance_santiago <- list()
coverage_sp       <- distance_sp <- list()

for (radius_km in monitoring_radii_km) {
  for (pollutant in summary_pollutants) {
    key <- paste(radius_km, pollutant, sep = "_")
    label <- if (pollutant == "pm10") "PM10" else "PM2.5"

    coverage_bogota[[key]] <- build_station_distance_trend_data(
      dist_pq    = dist_bogota,
      census_dt  = census_bogota,
      active_ids = active_bogota[[pollutant]],
      radius_km  = radius_km)

    distance_bogota[[key]] <- plot_station_distance_trend(
      dt         = coverage_bogota[[key]],
      city_label = "Bogota",
      pollutant  = label,
      radius_km  = radius_km)

    coverage_cdmx[[key]] <- build_station_distance_trend_data(
      dist_pq    = dist_cdmx,
      census_dt  = census_cdmx,
      active_ids = active_cdmx[[pollutant]],
      radius_km  = radius_km)

    distance_cdmx[[key]] <- plot_station_distance_trend(
      dt         = coverage_cdmx[[key]],
      city_label = "Mexico City",
      pollutant  = label,
      radius_km  = radius_km)

    coverage_santiago[[key]] <- build_station_distance_trend_data(
      dist_pq    = dist_santiago,
      census_dt  = census_santiago,
      active_ids = active_santiago[[pollutant]],
      radius_km  = radius_km)

    distance_santiago[[key]] <- plot_station_distance_trend(
      dt         = coverage_santiago[[key]],
      city_label = "Gran Santiago",
      pollutant  = label,
      radius_km  = radius_km)

    coverage_sp[[key]] <- build_station_distance_trend_data(
      dist_pq    = dist_sp,
      census_dt  = census_sp,
      active_ids = active_sp[[pollutant]],
      radius_km  = radius_km)

    distance_sp[[key]] <- plot_station_distance_trend(
      dt         = coverage_sp[[key]],
      city_label = "Sao Paulo",
      pollutant  = label,
      radius_km  = radius_km)

  }
}

# Compare concentration and threshold hours with station-area schooling
education_bogota <- list()
education_bogota$avg_pollution <- plot_dual_pollutant_station_scatter(
  station_dt = station_bogota,
  city_label = "Bogota",
  y_pm10     = "avg_pm10",
  y_pm25     = "avg_pm25",
  title      = "Annual average concentration in 2023",
  y_left     = "PM10 annual average",
  y_right    = "PM2.5 annual average")

education_bogota$hours_it1 <- plot_dual_pollutant_station_scatter(
  station_dt = station_bogota,
  city_label = "Bogota",
  y_pm10     = "hrs_d_pm10_it1",
  y_pm25     = "hrs_d_pm25_it1",
  title      = "Hours above WHO IT1 threshold in 2023",
  y_left     = "PM10 hours above IT1",
  y_right    = "PM2.5 hours above IT1")

education_bogota$hours_it2 <- plot_dual_pollutant_station_scatter(
  station_dt = station_bogota,
  city_label = "Bogota",
  y_pm10     = "hrs_d_pm10_it2",
  y_pm25     = "hrs_d_pm25_it2",
  title      = "Hours above WHO IT2 threshold in 2023",
  y_left     = "PM10 hours above IT2",
  y_right    = "PM2.5 hours above IT2")

education_cdmx <- list()
education_cdmx$avg_pollution <- plot_dual_pollutant_station_scatter(
  station_dt = station_cdmx,
  city_label = "Mexico City",
  y_pm10     = "avg_pm10",
  y_pm25     = "avg_pm25",
  title      = "Annual average concentration in 2023",
  y_left     = "PM10 annual average",
  y_right    = "PM2.5 annual average")

education_cdmx$hours_it1 <- plot_dual_pollutant_station_scatter(station_dt = station_cdmx,
  city_label = "Mexico City",
  y_pm10     = "hrs_d_pm10_it1",
  y_pm25     = "hrs_d_pm25_it1",
  title      = "Hours above WHO IT1 threshold in 2023",
  y_left     = "PM10 hours above IT1",
  y_right    = "PM2.5 hours above IT1")

education_cdmx$hours_it2 <- plot_dual_pollutant_station_scatter(station_dt = station_cdmx,
  city_label = "Mexico City",
  y_pm10     = "hrs_d_pm10_it2",
  y_pm25     = "hrs_d_pm25_it2",
  title      = "Hours above WHO IT2 threshold in 2023",
  y_left     = "PM10 hours above IT2",
  y_right    = "PM2.5 hours above IT2")

education_santiago <- list()
education_santiago$avg_pollution <- plot_dual_pollutant_station_scatter(
  station_dt = station_santiago,
  city_label = "Gran Santiago",
  y_pm10     = "avg_pm10",
  y_pm25     = "avg_pm25",
  title      = "Annual average concentration in 2023",
  y_left     = "PM10 annual average",
  y_right    = "PM2.5 annual average")

education_santiago$hours_it1 <- plot_dual_pollutant_station_scatter(
  station_dt = station_santiago,
  city_label = "Gran Santiago",
  y_pm10     = "hrs_d_pm10_it1",
  y_pm25     = "hrs_d_pm25_it1",
  title      = "Hours above WHO IT1 threshold in 2023",
  y_left     = "PM10 hours above IT1",
  y_right    = "PM2.5 hours above IT1")

education_santiago$hours_it2 <- plot_dual_pollutant_station_scatter(
  station_dt = station_santiago,
  city_label = "Gran Santiago",
  y_pm10     = "hrs_d_pm10_it2",
  y_pm25     = "hrs_d_pm25_it2",
  title      = "Hours above WHO IT2 threshold in 2023",
  y_left     = "PM10 hours above IT2",
  y_right    = "PM2.5 hours above IT2")

education_sp <- list()
education_sp$avg_pollution <- plot_dual_pollutant_station_scatter(station_dt = station_sp,
  city_label = "Sao Paulo",
  y_pm10     = "avg_pm10",
  y_pm25     = "avg_pm25",
  title      = "Annual average concentration in 2023",
  y_left     = "PM10 annual average",
  y_right    = "PM2.5 annual average")

education_sp$hours_it1 <- plot_dual_pollutant_station_scatter(station_dt = station_sp,
  city_label = "Sao Paulo",
  y_pm10     = "hrs_d_pm10_it1",
  y_pm25     = "hrs_d_pm25_it1",
  title      = "Hours above WHO IT1 threshold in 2023",
  y_left     = "PM10 hours above IT1",
  y_right    = "PM2.5 hours above IT1")

education_sp$hours_it2 <- plot_dual_pollutant_station_scatter(station_dt = station_sp,
  city_label = "Sao Paulo",
  y_pm10     = "hrs_d_pm10_it2",
  y_pm25     = "hrs_d_pm25_it2",
  title      = "Hours above WHO IT2 threshold in 2023",
  y_left     = "PM10 hours above IT2",
  y_right    = "PM2.5 hours above IT2")

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save both pollutant panels for every radius
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)
for (radius_km in monitoring_radii_km) {
  radius_tag <- if (radius_km == 3) "3km_v2" else paste0(radius_km, "km")
  for (pollutant in summary_pollutants) {
    key <- paste(radius_km, pollutant, sep = "_")
    tag <- if (pollutant == "pm10") "" else "_pm25"
    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("stations_dis_num_bogota_", radius_tag, tag, ".pdf")),
      plot      = distance_bogota[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8.5, height = 5.8, dpi = 300,
      bg        = "white", limitsize = FALSE)

    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("stations_dis_num_mexico_", radius_tag, tag, ".pdf")),
      plot      = distance_cdmx[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8.5, height = 5.8, dpi = 300,
      bg        = "white", limitsize = FALSE)

    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("stations_dis_num_santiago_", radius_tag, tag, ".pdf")),
      plot      = distance_santiago[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8.5, height = 5.8, dpi = 300,
      bg        = "white", limitsize = FALSE)

    ggplot2::ggsave(
      filename  = here::here(outdir_fig,
        paste0("stations_dis_num_saopaulo_", radius_tag, tag, ".pdf")),
      plot      = distance_sp[[key]],
      device    = grDevices::cairo_pdf,
      width     = 8.5, height = 5.8, dpi = 300,
      bg        = "white", limitsize = FALSE)

  }
}

# Save the three education comparisons per city
ggplot2::ggsave(
  filename  = here::here(outdir_fig, "bogota_2018_avg_pm10_pm25_vs_education.png"),
  plot      = education_bogota$avg_pollution,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "bogota_2018_hours_it1_pm10_pm25_vs_education.png"),
  plot      = education_bogota$hours_it1,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "bogota_2018_hours_it2_pm10_pm25_vs_education.png"),
  plot      = education_bogota$hours_it2,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "cdmx_2020_avg_pm10_pm25_vs_education.png"),
  plot      = education_cdmx$avg_pollution,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "cdmx_2020_hours_it1_pm10_pm25_vs_education.png"),
  plot      = education_cdmx$hours_it1,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "cdmx_2020_hours_it2_pm10_pm25_vs_education.png"),
  plot      = education_cdmx$hours_it2,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "santiago_2017_avg_pm10_pm25_vs_education.png"),
  plot      = education_santiago$avg_pollution,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "santiago_2017_hours_it1_pm10_pm25_vs_education.png"),
  plot      = education_santiago$hours_it1,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "santiago_2017_hours_it2_pm10_pm25_vs_education.png"),
  plot      = education_santiago$hours_it2,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "sao_paulo_2010_avg_pm10_pm25_vs_education.png"),
  plot      = education_sp$avg_pollution,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "sao_paulo_2010_hours_it1_pm10_pm25_vs_education.png"),
  plot      = education_sp$hours_it1,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

ggplot2::ggsave(
  filename  = here::here(outdir_fig, "sao_paulo_2010_hours_it2_pm10_pm25_vs_education.png"),
  plot      = education_sp$hours_it2,
  width     = 8.5, height = 5.8, dpi = 300,
  bg        = "white", limitsize = FALSE)

