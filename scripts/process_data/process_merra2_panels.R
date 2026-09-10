# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Compute optional MERRA-2 station and country comparisons.
#
#' @Description: Read the city series written by prepare_station_temporal.R.
# Compute station correlations and compare country estimates with NASA reference data.
#
#' @Summary:
#   I.   Read prepared city series and country inputs.
#   II.  Compute station correlations.
#   III. Compare country estimates with NASA.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_process_data.R"))

dir_series <- here::here("data", "processed", "merra2_stations_pm25")
dir_country <- here::here("data", "raw", "merra2_country_pm")
outdir_comparisons <- here::here("data", "processed", "comparisons")
dir.create(outdir_comparisons, recursive = TRUE, showWarnings = FALSE)

bogota_pollution <- read.csv(file.path(
  dir_series, "bogota_pm25_stations_merra2.csv"))
ciudad_mexico_pollution <- read.csv(file.path(
  dir_series, "ciudad_mexico_pm25_stations_merra2.csv"))
santiago_pollution <- read.csv(file.path(
  dir_series, "santiago_pm25_stations_merra2.csv"))
sao_paulo_pollution <- read.csv(file.path(
  dir_series, "sao_paulo_pm25_stations_merra2.csv"))

# Restore the Date class used by monthly aggregation before the extraction.
bogota_pollution$Date <- as.Date(bogota_pollution$Date)
ciudad_mexico_pollution$Date <- as.Date(ciudad_mexico_pollution$Date)
santiago_pollution$Date <- as.Date(santiago_pollution$Date)
sao_paulo_pollution$Date <- as.Date(sao_paulo_pollution$Date)

# Country-level MERRA-2 rasters and NASA's published monthly PM2.5 by country. The NASA
# file carries 13 header lines of provenance before the table starts.
nc_files <- list.files(dir_country, pattern = "\\.nc4$", full.names = TRUE)

nasa_pm25_countries <- read.csv(
  file.path(dir_country, "MERRA2.avgM_2d_pm25_admin0x.v01.19800101-20221231.csv"),
  sep = ",", skip = 13)

south_america <- ne_countries(continent = "South America", returnclass = "sf")
north_america <- ne_countries(continent = "North America", returnclass = "sf")


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
