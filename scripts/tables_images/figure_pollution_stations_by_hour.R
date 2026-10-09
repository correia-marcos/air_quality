# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Inspect Santiago station concentrations by hour in 2013.
#
#' @Description: Use current cleaned observed partitions for this optional diagnostic.
# The 35/25 comparison levels are retained from its historical annual-target diagnostic;
# they are separate from the manuscript episode thresholds of 75/50.
#
#' @Summary:
#   I. Import data.
#   II. Process data.
#   III. Save data.
#
#' @Date: Mar 2025
#' @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
# ============================================================================================
# I: Import data
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

diagnostic_year <- 2013L
station_path <- here::here("data", "processed", "monitoring_stations_outliers",
  "santiago_metro_clean")
santiago_stations_2013 <- arrow::open_dataset(station_path) |>
  dplyr::filter(year == diagnostic_year) |>
  dplyr::select(station, datetime, pm25, pm10) |>
  dplyr::collect()
if (!nrow(santiago_stations_2013)) stop("Santiago 2013 observations are required.")

# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function to generate a bar plot
# 1) Raw averages per station & hour (no filter)
h_raw_pm25 <- summarize_hourly_by_station(
  santiago_stations_2013,
  station_col  = "station",
  datetime_col = "datetime",
  value_col    = "pm25",
  filter_type  = "none"
)

h_raw_pm10 <- summarize_hourly_by_station(
  santiago_stations_2013,
  station_col  = "station",
  datetime_col = "datetime",
  value_col    = "pm10",
  filter_type  = "none"
)

p_raw_pm25 <- plot_hourly_stacked_stations(
  h_raw_pm25,
  region_name     = "Santiago",
  pollutant_label = "PM2.5",
  filter_label    = "All values",
  year            = 2013
)

p_raw_pm10 <- plot_hourly_stacked_stations(
  h_raw_pm10,
  region_name     = "Santiago",
  pollutant_label = "PM10",
  filter_label    = "All values",
  year            = 2013
)

p_raw_pm25
p_raw_pm10

# Condition on > IT1
h_it1 <- summarize_hourly_by_station(santiago_stations_2013, station_col = "station",
  datetime_col = "datetime", value_col = "pm25", filter_type = "gt_it1",
  it1 = 35)
p_it1 <- plot_hourly_stacked_stations(h_it1, pollutant_label = "PM2.5",
                                      filter_label = "Values > IT1 (35 µg/m³)",
                                        year = 2013)

# Condition on > IT2
h_it2 <- summarize_hourly_by_station(santiago_stations_2013, station_col = "station",
  datetime_col = "datetime", value_col = "pm25", filter_type = "gt_it2",
  it2 = 25)
p_it2 <- plot_hourly_stacked_stations(h_it2, pollutant_label = "PM2.5",
                                      filter_label = "Values > IT2 (25 µg/m³)",
                                        year = 2013)

p_it1
p_it2
# ============================================================================================
# III: Save data
# ============================================================================================

# Ensure output folder exists
outdir <- here::here("results", "figures", "temporal")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save plot of time span distribution of high pollution episodes
ggsave(filename = here::here(outdir, "distribution_all_values_santiago_pm25.pdf"),
       plot     = p_raw_pm25,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

ggsave(filename = here::here(outdir, "distribution_all_values_santiago_pm10.pdf"),
       plot     = p_raw_pm10,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

ggsave(filename = here::here(outdir, "distribution_values_above_it1_santiago_pm25.pdf"),
       plot     = p_it1,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

ggsave(filename = here::here(outdir, "distribution_values_above_it2_santiago_pm25.pdf"),
       plot     = p_it2,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")
