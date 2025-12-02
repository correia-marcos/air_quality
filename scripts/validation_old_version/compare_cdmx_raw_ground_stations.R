# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: COMPARE LASTEST VERSION AND THE NEW ONE
# 
# @Description: From 2000 to 2023 (NEED TO FINISH DOCUMENTATION)
# 
# @Summary: 
#   I.   Load libraries, utility functions and necessary data
#   II.  
#   III. 
# 
# @Date: May 2025
# @Author: Marcos
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "general_utilities", "config_utils_validation_old_version.R"))

# Make sure the Arrow package will interpret the time of dataframes in the correct TZ
options(arrow.local_tz = "UTC")

# ============================================================================================
# I: Import  data
# ============================================================================================
# Define the location of datasets
dir_cdmx_stations_data   <- here::here("data", "raw","air_monitoring_stations",
                                       "cdmx_metro_buffer_stations_dataset")
dir_l_cdmx_stations_data <- here::here("data", "_legacy", "pollution", "Mexico_city",
                                       "Air_Pollution_Mexico_2010_2023.dta")
dir_cdmx_area            <- here::here("data", "raw", "geospatial_data",
                                       "metro_areas", "cdmx_metro.gpkg")
dir_l_cdmx_area          <- here::here("data", "raw",
                                       "cities_shapefiles(old)",
                                       "Mexico_city")
dir_cdmx_station_loc     <- here::here("data", "interim", "spatial_filtered_stations",
                                       "CDMX_stations.gpkg")

# Open air pollution dataframes
cdmx_stations_data        <- arrow::open_dataset(dir_cdmx_stations_data)
legacy_cdmx_stations_data <- read_dta(dir_l_cdmx_stations_data)

# Open spatial data
cdmx              <- sf::st_read(dir_cdmx_area)
legacy_cdmx       <- sf::st_read(dir_l_cdmx_area)
stations_in_metro <- sf::st_read(dir_cdmx_station_loc)
# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function harmonize values, types and the order of the old dataframe
cdmx_old <- prepare_legacy_cdmx(legacy_cdmx_stations_data)

# Apply function to harmonize the new dataframe making it like the legacy one
# - restrict to 2010–2023
cdmx_new <- prepare_new_panel_like_legacy(
  new_data         = cdmx_stations_data,   # Arrow Dataset
  year_keep        = 2010:2023,
  tz               = "America/Mexico_City",
  return           = "tibble"              # collect; normalize station_code in R
)

# Apply function to Compare both datasets without shifting hours
raw_comparison <- compare_panels(
  old_df                = cdmx_old,
  new_df                = cdmx_new,
  keys                  = c("station_code", "year", "month", "day", "hour"),
  values                = c("pm10", "pm25", "o3", "co", "no2"),
  restrict_to_old_codes = TRUE,
  prefer_station        = c(ATI = "Atizapán"),
  new_shift_hours       = 0L 
)

# Apply function to Compare both datasets shifting hours
res <- compare_panels(
  old_df                = cdmx_old,
  new_df                = cdmx_new,
  keys                  = c("station_code", "year", "month", "day", "hour"),
  values                = c("pm10", "pm25", "o3", "co", "no2"),
  restrict_to_old_codes = TRUE,
  prefer_station        = c(ATI = "Atizapán"),
  new_shift_hours       = +1L 
)

# Quick summaries for the console
message("\nRows only in legacy: ", nrow(res$only_old))
message("Rows only in new    : ", nrow(res$only_new))
print(res$diff_summary)

# Separate values to save
differences_shifting_hour <- res$diffs_long %>% 
  filter(year == 2023)
differences_no_shifting   <- raw_comparison$diffs_long %>% 
  filter(year == 2023)

# Quick check on the non missing data
differences_shifting_hour_pm25 <- differences_shifting_hour %>%
  filter(within_tol == FALSE) %>%
  filter(variable == "pm25")

differences_shifting_hour_pm10 <- differences_shifting_hour %>%
  filter(within_tol == FALSE) %>%
  filter(variable == "pm10")

# Quick check on the missing data from before
new_only <- res$only_new %>%
  filter(year == 2023)

# Check the rows that exist only on the old data
old_only <- res$only_old

# ============================================================================================
# III: Generate figures/tables
# ============================================================================================
# 3) Stations now vs before
cdmx_has_pm10_2023_stations_legacy_scheme <- plot_metro_area_interactive(
  metro_area_sf = legacy_cdmx,
  stations_sf   = stations_in_metro,
  pollution_ds  = cdmx_stations_data,
  legacy_df     = legacy_cdmx_stations_data,
  filter_type   = "has_pm_in_year",
  filter_year   = 2023,
  pollutant     = "pm10",
  color_scheme  = "legacy2023",
  city_name     = "Mexico City"
)

cdmx_has_pm25_2023_stations_legacy_scheme <- plot_metro_area_interactive(
  metro_area_sf = legacy_cdmx,
  stations_sf   = stations_in_metro,
  pollution_ds  = cdmx_stations_data,
  legacy_df     = legacy_cdmx_stations_data,
  filter_type   = "has_pm_in_year",
  filter_year   = 2023,
  pollutant     = "pm25",
  color_scheme  = "legacy2023",
  city_name     = "Mexico City"
)

cdmx_has_pm10_2022_stations_legacy_scheme <- plot_metro_area_interactive(
  metro_area_sf = legacy_cdmx,
  stations_sf   = stations_in_metro,
  pollution_ds  = cdmx_stations_data,
  legacy_df     = legacy_cdmx_stations_data,
  filter_type   = "has_pm_in_year",
  filter_year   = 2022,
  pollutant     = "pm10",
  color_scheme  = "legacy2023",
  city_name     = "Mexico City"
)

cdmx_has_pm25_2022_stations_legacy_scheme <- plot_metro_area_interactive(
  metro_area_sf = legacy_cdmx,
  stations_sf   = stations_in_metro,
  pollution_ds  = cdmx_stations_data,
  legacy_df     = legacy_cdmx_stations_data,
  filter_type   = "has_pm_in_year",
  filter_year   = 2022,
  pollutant     = "pm25",
  color_scheme  = "legacy2023",
  city_name     = "Mexico City"
)


# ============================================================================================
# IV: Save figures/data
# ============================================================================================
# Ensure output folder exists
outdir <- here("results", "validation_rep_package", "CDMX")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save plot
htmlwidgets::saveWidget(
  cdmx_has_pm10_2023_stations_legacy_scheme,
  here::here(outdir, "cdmx_pm10_stations_in_2023_by_presence.html"),
  selfcontained = TRUE)

htmlwidgets::saveWidget(
  cdmx_has_pm25_2023_stations_legacy_scheme,
  here::here(outdir, "cdmx_pm25_stations_in_2023_by_presence.html"),
  selfcontained = TRUE)

htmlwidgets::saveWidget(
  cdmx_has_pm10_2022_stations_legacy_scheme,
  here::here(outdir, "cdmx_pm10_stations_in_2022_by_presence.html"),
  selfcontained = TRUE)

htmlwidgets::saveWidget(
  cdmx_has_pm25_2022_stations_legacy_scheme,
  here::here(outdir, "cdmx_pm25_stations_in_2022_by_presence.html"),
  selfcontained = TRUE)
# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
cdmx_new_2023 <- cdmx_new %>% 
  filter(year == 2023) %>%
  select(datehour, station_code, pm25, pm10)

stations_2023_with_data <- cdmx_new_2023 %>%
  group_by(station_code) %>%
  summarise(has_data = any(!is.na(pm10) | !is.na(pm25)), .groups = "drop") %>%
  filter(has_data) %>%
  pull(station_code)

stations_2023_with_pm10_data <- cdmx_new_2023 %>%
  group_by(station_code) %>%
  filter() %>%
  pull(station_code)

checkson <- readr::read_csv("/Users/correia-marcos/Downloads/contaminantes_2010.csv",
                            skip = 10)

checkson <- checkson %>% 
  arrange(cve_station)

renv::snapshot(prompt = FALSE)