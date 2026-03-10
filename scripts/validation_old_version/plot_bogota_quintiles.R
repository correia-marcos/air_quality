# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Visualize inequality and pollution monitoring distribution for the 4 metro areas.
# 
# @Description: Creates high-resolution maps of Latin American metro areas highlighting census 
#               education quintiles, active monitoring stations, and their respective spatial 
#               buffers.
# 
# @Summary: 
#   I.   Import shapefiles, census collapsed microdata and pollution information for Bogotá
#   II.  Process the data to exclude unnecessary regions and combine shapefiles
#   III. Create and export a map of the metro areas with the quintiles of education and 
# stations buffers
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
dir_pollution   <- here::here("data", "raw", "monitoring_stations")
dir_geospatial  <- here::here("data", "raw", "geospatial_data")
dir_census      <- here::here("data", "interim", "census")
outdir_figures  <- here::here("results", "validation_rep_package", "Bogota")
legacy_stations <- here::here("data", "_legacy", "ground_stations", "bogota")

# Define files' specifc location
gpkg_bogota_2005_metro_area    <- here::here(dir_geospatial, "bogota",
                                             "bogota_area_metro_2005.gpkg")
gpkg_stations_bogota_2005      <- here::here(dir_geospatial, "bogota",
                                             "bogota_2005_stations_buffer_metro.gpkg")
csv_bogota_extended_2005       <- here::here(dir_census, "bogota_extended_2005",
                                             "collapse_metro_area_extended.csv")
arrow_bogota_dir               <- here::here(dir_pollution, "bogota_metro_dataset")

# Read the necessary files
bogota_metro_2005_sf         <- sf::st_read(gpkg_bogota_2005_metro_area)
bogota_stations_2005_sf      <- sf::st_read(gpkg_stations_bogota_2005)
bogota_census_extend_2005    <- readr::read_csv(csv_bogota_extended_2005, col_types = "c")
legacy_stations_sf           <- sf::st_read(legacy_stations, options = "ENCODING=LATIN1")

# ============================================================================================
# II: Process & Plot
# ============================================================================================
# Filter stations that are in the legacy data
bogota_stations_small <- bogota_stations_2005_sf[1:19, ]

# Apply function to plot Bogotá metro area by education quintiles (2023, PM10 & PM2.5)
map_bogota_5km_extended_census_2005 <- plot_inequality_pollution(
  metro_sf    = bogota_metro_2005_sf,
  stations_sf = bogota_stations_small,
  arrow_dir   = arrow_bogota_dir,
  census_df   = bogota_census_extend_2005,
  join_sf_col = "GEO_ID",          # <-- Fixed the underscore here
  join_df_col = "GEO_ID",          # <-- Fixed the underscore here
  station_col = "station_name",    # Ensure this matches the column in bogota_stations_sf
  ed_col      = "escolaridad_avg", 
  pop_col     = "n",               
  pollutants  = c("pm25", "pm10"),
  year_filter = 2023,
  buffer_km   = 5,
  city_label  = "",
  label_x_pct = 0.45,
  label_y_pct = 0.99,
  legend_pos  = c(0.95, 0.25)
)

map_bogota_3km_extended_census_2005 <- plot_inequality_pollution(
  metro_sf    = bogota_metro_2005_sf,
  stations_sf = bogota_stations_small,
  arrow_dir   = arrow_bogota_dir,
  census_df   = bogota_census_extend_2005,
  join_sf_col = "GEO_ID",          # <-- Fixed the underscore here
  join_df_col = "GEO_ID",          # <-- Fixed the underscore here
  station_col = "station_name",    # Ensure this matches the column in bogota_stations_sf
  ed_col      = "escolaridad_avg", 
  pop_col     = "n",               
  pollutants  = c("pm25", "pm10"),
  year_filter = 2023,
  buffer_km   = 3,
  city_label  = "",
  label_x_pct = 0.45,
  label_y_pct = 0.99,
  legend_pos  = c(0.95, 0.25)
)

# ============================================================================================
# III: Save Data
# ============================================================================================
# Ensure output folder exists
dir.create(outdir_figures, recursive = TRUE, showWarnings = FALSE)
cat("Saving maps to", outdir_figures, "\n")

# Save Bogotá
ggsave(filename = here::here(outdir_figures, "map_bogota_new_metro_old_stations_2005_5km.pdf"), 
       plot     = map_bogota_5km_extended_census_2005,
       device   = cairo_pdf,
       width    = 12, height = 8, dpi = 300, bg = "white")
ggsave(filename = here::here(outdir_figures, "map_bogota_new_metro_old_stations_2005_3km.pdf"), 
       plot     = map_bogota_3km_extended_census_2005,
       device   = cairo_pdf,
       width    = 12, height = 8, dpi = 300, bg = "white")
