# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Apply function to crop information in the 4 cities area
#
# @Date: Nov 2024
# @Author: Marcos

# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Create list of all raster files from MERRA2
nc_files  <- list.files(here::here("data", "merra2"), full.names = TRUE)

# Open city shapefiles
bogota               <- sf::st_read(here::here("data", "cities", "Bogota_metro"))
ciudad_mexico        <- sf::st_read(here::here("data", "cities", "Mexico_city"))
santiago             <- sf::st_read(here::here("data", "cities", "Santiago")) 
sao_paulo            <- sf::st_read(here::here("data", "cities", "Sao_Paulo"))
chile_states         <- sf::st_read(here::here("data", "chile_regions"))

# ============================================================================================
# II: Process data
# ============================================================================================

# Apply function to crop data for each city and generate a panel
bogota_results <- process_merra2_city_hourly_parallel(
  shapefile = bogota,
  nc_files = nc_files,
  city_name = "Bogotá",
  num_cores = NULL)

ciudad_mexico_results <- process_merra2_city_hourly_parallel(
  shapefile = ciudad_mexico,
  nc_files = nc_files,
  city_name = "Ciudad de México",
  num_cores = NULL)

santiago_results <- process_merra2_city_hourly_parallel(
  shapefile = santiago,
  nc_files = nc_files,
  city_name = "Santiago",
  num_cores = NULL)

sao_paulo_results <- process_merra2_city_hourly_parallel(
  shapefile = sao_paulo,
  nc_files = nc_files,
  city_name = "São Paulo",
  num_cores = NULL)

# ============================================================================================
# III: Save data
# ============================================================================================
# Save processed dataframes
write.csv(bogota_results, file = "results/panel_data/bogota_panel.csv",
          row.names = FALSE)
write.csv(ciudad_mexico_results, file = "results/panel_data/ciudad_mexico_panel.csv", 
          row.names = FALSE)
write.csv(santiago_results, file = "results/panel_data/santiago_panel.csv",
          row.names = FALSE)
write.csv(sao_paulo_results, file = "results/panel_data/sao_paulo_panel.csv", 
          row.names = FALSE)
