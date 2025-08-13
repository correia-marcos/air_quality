# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: G
# 
# @Description: ss
# 
# @Summary: This program performs the following steps:
#   I.   Import and Load the processed time series data for each city from CSV files.
#   II.  Create bar plots and ridgeline plots for PM2.5 data using the custom functions 
#        'plot_hourly_avg_pollution' and 'plot_hourly_ridgeline_pollution'.
#   III. Save the generated plots as PDF files.
# 
# @Date: Mar 2025
# @author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Open time-series dataframe of MERRA-2 and ground-based station measurements of PM 2.5
santiago_stations_2023       <- readRDS(here::here("data",
                                                   "raw",
                                                   "pollution_ground_stations",
                                                   "Santiago",
                                                   "pollution_data_balanced_2023_pm25.rds"))

santiago_stations_2013       <- readRDS(here::here("data",
                                                   "raw",
                                                   "pollution_ground_stations",
                                                   "Santiago",
                                                   "pollution_data_balanced_2013.rds"))

# Define the map between station code and station names
SANTIAGO_STATION_LOOKUP <- tibble::tribble(
  ~Station, ~StationName,
  "1",  "Cerrillos II",
  "2",  "Cerrillos I",
  "3",  "Cerro Navia",
  "4",  "El Bosque",
  "5",  "Independencia",
  "6",  "La Florida",
  "7",  "Las Condes",
  "8",  "Pudahuel",
  "9",  "Puente Alto",
  "10", "Quilicura",
  "11", "Quilicura I",
  "12", "Parque O'Higgins",
  "13", "Talagante",
  "14", "La Pintana"
)

# ============================================================================================
# II:Process data
# ============================================================================================
# Apply function to generate a bar plot
# 1) Raw averages per station & hour (no filter)
h_raw_pm25 <- summarize_hourly_by_station(
  santiago_stations_2013,
  station_col  = "station_code",
  datetime_col = "date2_hour",
  value_col    = "pm25_validated",
  filter_type  = "none"
)

h_raw_pm10 <- summarize_hourly_by_station(
  santiago_stations_2013,
  station_col  = "station_code",
  datetime_col = "date2_hour",
  value_col    = "pm10_validated",
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
h_it1 <- summarize_hourly_by_station(santiago_stations_2013, filter_type = "gt_it1", it1 = 35)
p_it1 <- plot_hourly_stacked_stations(h_it1, pollutant_label = "PM2.5",
                                      filter_label = "Values > IT1 (35 µg/m³)", year = 2013)

# Condition on > IT2
h_it2 <- summarize_hourly_by_station(santiago_stations_2013, filter_type = "gt_it2", it2 = 25)
p_it2 <- plot_hourly_stacked_stations(h_it2, pollutant_label = "PM2.5",
                                      filter_label = "Values > IT2 (25 µg/m³)", year = 2013)

p_it1
p_it2
# ============================================================================================
# II: Save data
# ============================================================================================

# Ensure output folder exists
outdir <- here("results", "figures", "hour_pollution_by_station")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save plot of time span distribution of high pollution episodes
ggsave(filename = here(outdir, "distribution_all_values_santiago_pm25.pdf"),
       plot     = p_raw_pm25,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

ggsave(filename = here(outdir, "distribution_all_values_santiago_pm10.pdf"),
       plot     = p_raw_pm10,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

ggsave(filename = here(outdir, "distribution_values_above_it1_santiago_pm25.pdf"),
       plot     = p_it1,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

ggsave(filename = here(outdir, "distribution_values_above_it2_santiago_pm25.pdf"),
       plot     = p_it2,
       device   = cairo_pdf,
       width    = 16,
       height   = 9,
       dpi      = 300)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")