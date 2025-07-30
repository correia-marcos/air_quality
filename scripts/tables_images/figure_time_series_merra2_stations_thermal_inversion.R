# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Visualize and compare PM2.5 time series data from MERRA-2 and ground station 
# measurements in Santiago for days without thermal inversion.
# 
# @Description: This script imports processed time series data for MERRA-2 and ground-based
# PM2.5 measurements for Santiago. It then generates two sets of visualizations: one set using a
# rolling window (smoothed data) and another set with raw data. Each plot is annotated with the 
# Pearson (or specified) correlation between the MERRA-2 and station series. The plots measure 
# the relation between both datasets when there is no Thermal Inversion in Santiago.
# Finally, the plots are saved as PDF files in the designated results folder.
# 
# @Summary: This program performs the following steps:
#   I.   Import and Load the processed time series data for each city.
#   II.  Generate time series plots for each city with both rolling and raw PM2.5 series 
#   III. Save the generated plots as PDF files.
# 
# @Date: Feb 2025
# @author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Open time-series dataframe of MERRA-2 and ground-based station measurements of PM 2.5
santiago_pm25      <- read.csv(here::here("data", 
                                          "processed",
                                          "merra2_stations_pm25",
                                          "santiago_pm25_stations_merra2.csv"))
santiago_thermal  <- read.csv(here::here("data", 
                                         "processed",
                                         "thermal_inversion_dataframes",
                                         "Santiago_2023-01-01_2023-12-31_inversion.csv"))

# ============================================================================================
# II:Process data
# ============================================================================================
# Convert Kelvin values to Celsius and focus on the necessary columns
santiago_thermal_celsius <- santiago_thermal %>% 
  dplyr::mutate(temp_2m     = temp_2m - 273.15,
                temp_850hPa = temp_850hPa - 273.15) 

santiago_thermal_celsius <- santiago_thermal_celsius %>% 
  dplyr::select(Day, Hour, Inversion_dummy)


# left_join onto your PM₂.₅ df, filter out the flagged rows, then drop the helper column
santiago_pm25_filtered <- santiago_pm25 %>%
  left_join(santiago_thermal_celsius,
            by = c("Date" = "Day", "Hour" = "Hour")) %>%
  filter(Inversion_dummy != 1 | is.na(Inversion_dummy)) %>%
  select(-Inversion_dummy)


# Apply function to generate plot with rolling window
santiago_plot_series      <- plot_pm25_timeseries_smooth(
  df             = santiago_pm25_filtered,
  region_name    = "Santiago",
  apply_rolling  = TRUE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue")

# Apply function to generate plot with "raw" values (no moving windows)
santiago_raw_series <- plot_pm25_timeseries_smooth(
  df             = santiago_pm25_filtered,
  region_name    = "Santiago",
  apply_rolling  = FALSE,
  window_hours   = 24,
  corr_method    = "pearson",
  color_merra2   = "darkred",
  color_stations = "darkblue"
)

# ============================================================================================
# III: Save data
# ============================================================================================
# Ensure output folder exists
outdir <- here("results", "figures", "m2_stations_removed_ti_days")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save the plots
ggsave(
  filename = here::here(outdir, "santiago_24_hrs_ma_m2_stations_thermal_inversion.pdf"),
  plot = santiago_plot_series,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

ggsave(
  filename = here::here(outdir, "santiago_raw_m2_stations_thermal_inversion.pdf"),
  plot = santiago_plot_series,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)