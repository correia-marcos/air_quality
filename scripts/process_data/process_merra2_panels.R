# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Turn the MERRA-2 aerosol panels into PM2.5 and check that estimate against both
#   ground stations and NASA's own published country aggregate.
#
# @Description: Three steps that used to be three scripts with an order nothing recorded.
# Section II converts each city's hourly aerosol panel to PM2.5; section III merges that
# with the ground-station readings and computes hourly/daily/monthly correlations; section
# IV repeats it at country level against NASA's monthly product. Sections III and
# IV are the validation half — they are what justifies using MERRA-2 where stations are
# absent. Section III reads section II's objects from memory, so the chain is visible here
# rather than implied by a filename.
#
# @Summary:
#   I.   Import data: aerosol panels, station readings, MERRA-2 rasters, NASA reference.
#   II.  Convert city aerosol panels to PM2.5 and save.
#   III. Merge with ground stations, compute correlations and save.
#   IV.  Compare the country-level monthly aggregate against NASA and save.
#
# @Date: August 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_panels   <- here::here("data", "interim", "cities_m2_aerosols")
dir_stations <- here::here("data", "raw", "pollution_ground_stations")
dir_country  <- here::here("data", "raw", "merra2_country_pm")

outdir_pm25         <- here::here("data", "processed", "merra2_pm25")
outdir_m2_stations  <- here::here("data", "processed", "merra2_stations_pm25")
outdir_comparisons  <- here::here("data", "processed", "comparisons")

# City aerosol panels, written by generate_panel_air_quality.R
bogota_panel        <- read.csv(file.path(dir_panels, "bogota_panel.csv"))
ciudad_mexico_panel <- read.csv(file.path(dir_panels, "ciudad_mexico_panel.csv"))
santiago_panel      <- read.csv(file.path(dir_panels, "santiago_panel.csv"))
sao_paulo_panel     <- read.csv(file.path(dir_panels, "sao_paulo_panel.csv"))

# Ground-station readings. Each city names its datetime and PM2.5 columns differently, so
# those names are passed explicitly in section III rather than harmonized here.
bogota_stations        <- readRDS(file.path(
  dir_stations, "Bogota", "pollution_pm10_pm25_data_balanced_2023.rds"))
ciudad_mexico_stations <- readRDS(file.path(
  dir_stations, "Mexico_city", "pollution_pm25_data_balanced_2023.rds"))
santiago_stations      <- readRDS(file.path(
  dir_stations, "Santiago", "pollution_data_balanced_2023_pm25.rds"))
sao_paulo_stations     <- readRDS(file.path(
  dir_stations, "Sao_paulo", "pollution_data_balanced_2023_pm25.rds"))

# Sao Paulo's station file covers the whole state; section III cuts it to the metro area.
stations_in_sp_metro <- sf::st_read(here::here(
  "data", "raw", "cities_shapefiles", "Sao_Paulo_metro_stations"))

# Country-level MERRA-2 rasters and NASA's published monthly PM2.5 by country. The NASA
# file carries 13 header lines of provenance before the table starts.
nc_files <- list.files(dir_country, pattern = "\\.nc4$", full.names = TRUE)

nasa_pm25_countries <- read.csv(
  file.path(dir_country, "MERRA2.avgM_2d_pm25_admin0x.v01.19800101-20221231.csv"),
  sep = ",", skip = 13)

south_america <- ne_countries(continent = "South America", returnclass = "sf")
north_america <- ne_countries(continent = "North America", returnclass = "sf")

# ============================================================================================
# II: Convert city aerosol panels to PM2.5
# ============================================================================================
dir.create(outdir_pm25, recursive = TRUE, showWarnings = FALSE)

bogota_pm25        <- convert_and_add_pm25(bogota_panel)
ciudad_mexico_pm25 <- convert_and_add_pm25(ciudad_mexico_panel)
santiago_pm25      <- convert_and_add_pm25(santiago_panel)
sao_paulo_pm25     <- convert_and_add_pm25(sao_paulo_panel)

write.csv(bogota_pm25, file.path(outdir_pm25, "bogota_pm25.csv"), row.names = FALSE)
write.csv(ciudad_mexico_pm25, file.path(outdir_pm25, "ciudad_mexico_pm25.csv"),
          row.names = FALSE)
write.csv(santiago_pm25, file.path(outdir_pm25, "santiago_pm25.csv"), row.names = FALSE)
write.csv(sao_paulo_pm25, file.path(outdir_pm25, "sao_paulo_pm25.csv"), row.names = FALSE)

# ============================================================================================
# III: Merge with ground stations and compute correlations
# ============================================================================================
dir.create(outdir_m2_stations, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_comparisons, recursive = TRUE, showWarnings = FALSE)

# Keep only the Sao Paulo stations that sit inside the metro area.
sao_paulo_stations <- sao_paulo_stations %>%
  filter(station_code %in% stations_in_sp_metro$sttn_cd)

bogota_pollution <- combine_station_merra2_pm25(
  station_df           = bogota_stations,
  station_datetime_col = "datetime",
  station_pm25_col     = "pm25",
  merra2_df            = bogota_pm25)

ciudad_mexico_pollution <- combine_station_merra2_pm25(
  station_df           = ciudad_mexico_stations,
  station_datetime_col = "datetime",
  station_pm25_col     = "pm25",
  merra2_df            = ciudad_mexico_pm25)

santiago_pollution <- combine_station_merra2_pm25(
  station_df           = santiago_stations,
  station_datetime_col = "date2_hour",
  station_pm25_col     = "pm25_validated",
  merra2_df            = santiago_pm25)

sao_paulo_pollution <- combine_station_merra2_pm25(
  station_df           = sao_paulo_stations,
  station_datetime_col = "datetime",
  station_pm25_col     = "pm25",
  merra2_df            = sao_paulo_pm25)

# Correlations at hourly, daily and monthly scales: agreement improves with aggregation,
# which is the argument for using MERRA-2 monthly rather than hourly.
city_pollution_list <- list(
  Bogota        = bogota_pollution,
  Ciudad_Mexico = ciudad_mexico_pollution,
  Santiago      = santiago_pollution,
  Sao_Paulo     = sao_paulo_pollution)

correlation_results <- compute_correlations_for_cities(city_pollution_list)

write.csv(correlation_results,
          file.path(outdir_comparisons, "correlation_pm25_stations_merra2.csv"),
          row.names = FALSE)

write.csv(bogota_pollution,
          file.path(outdir_m2_stations, "bogota_pm25_stations_merra2.csv"),
          row.names = FALSE)
write.csv(ciudad_mexico_pollution,
          file.path(outdir_m2_stations, "ciudad_mexico_pm25_stations_merra2.csv"),
          row.names = FALSE)
write.csv(santiago_pollution,
          file.path(outdir_m2_stations, "santiago_pm25_stations_merra2.csv"),
          row.names = FALSE)
write.csv(sao_paulo_pollution,
          file.path(outdir_m2_stations, "sao_paulo_pm25_stations_merra2.csv"),
          row.names = FALSE)

# ============================================================================================
# IV: Compare the country-level monthly aggregate against NASA
# ============================================================================================
# Same MERRA-2 processing chain applied to whole countries, where NASA publishes a monthly
# figure we can check against. Extraction is parallel; num_cores = NULL uses all but one.
brazil_comparison <- generate_region_comparison(
  shapefile         = south_america,
  filter_field      = "sov_a3",
  filter_value      = "BRA",
  region_name       = "Brazil",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE)

chile_comparison <- generate_region_comparison(
  shapefile         = south_america,
  filter_field      = "sov_a3",
  filter_value      = "CHL",
  region_name       = "Chile",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE)

colombia_comparison <- generate_region_comparison(
  shapefile         = south_america,
  filter_field      = "sov_a3",
  filter_value      = "COL",
  region_name       = "Colombia",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE)

mexico_comparison <- generate_region_comparison(
  shapefile         = north_america,
  filter_field      = "sov_a3",
  filter_value      = "MEX",
  region_name       = "Mexico",
  nc_files          = nc_files,
  nasa_monthly_data = nasa_pm25_countries,
  num_cores         = NULL,
  extraction_fun    = "mean",
  parallel          = TRUE)

countries_comparison <- rbind(brazil_comparison, chile_comparison,
                              colombia_comparison, mexico_comparison)

write.csv(countries_comparison,
          file.path(outdir_comparisons, "countries_comparison_month_idb_nasa_merra2.csv"),
          row.names = FALSE)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
