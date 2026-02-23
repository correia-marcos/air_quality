# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Visualize inequality and pollution monitoring distribution.
# 
# @Description: Creates high-resolution maps of Latin American metro areas 
#               highlighting census education quintiles, active monitoring 
#               stations, and their respective spatial buffers.
# 
# @Summary: 
#   I.   Import shapefiles for city boundaries and continents
#   II.  Process the data to exclude unnecessary regions and combine shapefiles
#   III. Create and export a map of Latin America with the highlighted cities
# 
# @Date: January 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define the output general folders
dir_pollution  <- here::here("data", "raw", "monitoring_stations")
dir_geospatial <- here::here("data", "raw", "geospatial_data")
dir_census     <- here::here("data", "interim", "census")
outdir_figures <- here::here("results", "figures", "maps")

# --- Bogotá Data ---
bogota_census_tracts_2005_sf <- 
  sf::st_read(here::here(dir_geospatial, "bogota", "bogota_area_metro_census_tracts_2005.gpkg"))
bogota_stations_2018_sf <- 
  sf::st_read(here::here(dir_geospatial, "bogota", "bogota_2018_stations_buffer_metro.gpkg"))
bogota_census_basic_2005 <-
  readr::read_csv(here::here(dir_census, "bogota_basic_2005",
                             "collapse_metro_area_basic.csv"), col_types = "c")
bogota_census_extend_2005 <-
  readr::read_csv(here::here(dir_census, "bogota_extended_2005",
                             "collapse_metro_area_extended.csv"), col_types = "c")
bogota_arrow_dir <- here::here(dir_pollution, "bogota_metro_dataset")

# --- São Paulo Data ---
sp_metro_sf    <- sf::st_read(here::here(dir_geospatial, "sao_paulo",
                                         "sao_paulo_metro_2010_weighting_areas.gpkg"))
sp_stations_sf <- sf::st_read(here::here(dir_geospatial, "sao_paulo",
                                         "sao_paulo_stations_buffer_metro_2010.gpkg"))
sp_census      <- readr::read_csv(here::here(dir_census, "sao_paulo_2010",
                                             "census_sp_collapsed_2010.csv"), col_types = "c")
sp_arrow_dir   <- here::here(dir_pollution, "sao_paulo_metro_dataset")

# ============================================================================================
# II: Process & Plot
# ============================================================================================

# --- Bogotá Plots (2023, PM10 & PM2.5) ---
map_bogota_5km <- plot_inequality_pollution(
  metro_sf    = bogota_census_tracts_2005_sf,
  stations_sf = bogota_stations_2018_sf,
  arrow_dir   = bogota_arrow_dir,
  census_df   = bogota_census_basic_2005,
  join_sf_col = "GEO_ID",          # <-- Fixed the underscore here
  join_df_col = "GEO_ID",          # <-- Fixed the underscore here
  station_col = "station_name",    # Ensure this matches the column in bogota_stations_sf
  ed_col      = "escolaridad_avg", 
  pop_col     = "n",               
  pollutants  = c("pm25", "pm10"),
  year_filter = 2023,
  buffer_km   = 5,
  city_label  = ""
)

map_bogota_5km

map_bogota_3km <- plot_inequality_pollution(
  metro_sf    = bogota_metro_sf,
  stations_sf = bogota_stations_sf,
  arrow_dir   = bogota_arrow_dir,
  census_df   = bogota_census,
  join_sf_col = "GEOID",
  join_df_col = "GEOID",
  ed_col      = "escolaridad_avg",
  pop_col     = "n",
  pollutants  = c("pm25", "pm10"),
  year_filter = 2023,
  buffer_km   = 3,
  city_label  = "Bogotá Metropolitan Area"
)

# --- São Paulo Plots (2023, PM10 & PM2.5) ---
cat("Generating São Paulo maps...\n")
map_sp_5km <- plot_inequality_pollution(
  metro_sf    = sp_metro_sf,
  stations_sf = sp_stations_sf,
  arrow_dir   = sp_arrow_dir,
  census_df   = sp_census,
  join_sf_col = "code_weighting",    # From geobr
  join_df_col = "code_weighting",    # From censobr collapse
  ed_col      = "avg_escolaridad",   # Updated SP schooling variable
  pop_col     = "total_adult_pop",   # Updated SP population variable
  pollutants  = c("pm25", "pm10"),
  year_filter = 2023,
  buffer_km   = 5,
  city_label  = "São Paulo Metropolitan Area"
)

# ============================================================================================
# III: Save Data
# ============================================================================================

# Ensure output folder exists
dir.create(outdir_figures, recursive = TRUE, showWarnings = FALSE)
cat("Saving maps to", outdir_figures, "\n")

# Save Bogotá
ggsave(here::here(outdir_figures, "map_bogota_5km.png"), map_bogota_5km,
       width = 12, height = 8, dpi = 300, bg = "white")
ggsave(here::here(outdir_figures, "map_bogota_3km.png"), map_bogota_3km,
       width = 12, height = 8, dpi = 300, bg = "white")

# Save São Paulo
ggsave(here::here(outdir_figures, "map_sao_paulo_5km.png"), map_sp_5km,
       width = 12, height = 8, dpi = 300, bg = "white")

cat("Script from the IDB project executed successfully in the Docker container!\n")