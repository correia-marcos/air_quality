# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Download all required data for the metro area of São Paulo
# 
# @Description: This functions uses previous created functions on city specific src  utilities 
# to download all the necessary data for the project. It is based on downloading three major 
# sources:
#     1 - Geo-referenced administrative data to construct the metro area of São Paulo
#           obs: This function download data for the country and then filter metro area
#     2 - All ground station air pollution data inside the metro area
#     3 - CENSUS microdata for the country
# 
# @Summary: 
#   I.   Load all the sources in the correct order (order matters) for functions and variables
#   II.  Use the named list in city_specific/sao_paulo.R to define parameters for functions
#   III. Download all data (metro area -> stations -> census)
# 
# @Date: November 2025
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions - config_utils_plot_tables to generate one LaTeX table
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "sao_paulo.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
brazil <- rnaturalearth::ne_states(country = "Brazil", returnclass = "sf")

# Show parameters imported on src/city_specific_sao_paulo.R
print(sao_paulo_cfg$base_url_shp)
print(sao_paulo_cfg$years)
print(sao_paulo_cfg$dl_dir)
print(sao_paulo_cfg$out_dir)

# ============================================================================================
# I: Download data
# ============================================================================================
# Apply function to download shapefiles for São Paulo metro area - municipalities
sao_paulo_metro_2010 <- sao_paulo_download_metro_area(
  level              = "mpio",
  base_url          = sao_paulo_cfg$base_url_shp,
  keep_municipality = sao_paulo_cfg$cities_in_metro,
  download_dir      = here::here(sao_paulo_cfg$dl_dir, "metro_area"),
  out_file          = here::here(sao_paulo_cfg$out_dir, "geospatial_data",
                                 "sao_paulo", "sao_paulo_metro_2010.gpkg"))

# Apply function to download shapefiles for São Paulo metro area - census tracts
sao_paulo_metro_2010_census_tracts <- sao_paulo_download_metro_area(
  level             = "setor_censitario",
  base_url          = sao_paulo_cfg$base_url_shp,
  keep_municipality = sao_paulo_cfg$cities_in_metro,
  download_dir      = here::here(sao_paulo_cfg$dl_dir, "metro_area"),
  out_file          = here::here(sao_paulo_cfg$out_dir, "geospatial_data",
                                 "sao_paulo", "sao_paulo_metro_2010_census_tracts.gpkg"))

# Apply function to download shapefiles for São Paulo metro area - weighting areas
sao_paulo_metro_2010_weighting <- sao_paulo_download_weighting_areas(
  keep_municipality = sao_paulo_cfg$cities_in_metro,
  year              = 2010,
  out_file          = here::here(sao_paulo_cfg$out_dir, "geospatial_data",
                                 "sao_paulo", "sao_paulo_metro_2010_weighting_areas.gpkg")
)


# Apply function to save a LaTeX table of the states that we must download stations data
table_states_to_download <- table_state_metro_distances(
  national_states_sf = brazil,
  metro_area_sf = sao_paulo_metro_2010,
  save_latex_table = TRUE,
  caption = "Administrative states and distance to metropolitan area (in Km)",
  out_file = here::here("results", "tables", "states_to_get_stations", "sao_paulo.tex"),
  overwrite_tex = TRUE)

# Apply function to create Selenium server and download station data + save logs
logs_qualar_stations_hourly_data <- sao_paulo_download_pollution(
  base_url = sao_paulo_cfg$base_url_qualar,
  subdir   = file.path("sao_paulo", "ground_stations"),
  years    = sao_paulo_cfg$years)
write.csv(logs_qualar_stations_hourly_data, row.names = FALSE,
          file = here::here(sao_paulo_cfg$dl_dir, "log_qualar_ground_stations.csv"))

# Apply function to create Selenium server and download the stations metadata
logs_qualar_stations_metadata <- sao_paulo_download_metadata(
  base_url   = sao_paulo_cfg$base_url_qualar,
  search_url = sao_paulo_cfg$metadata_url_qualar,
  out_file   = here::here(sao_paulo_cfg$dl_dir,
                          "stations_metadata", "stations_metadata.csv"))

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")