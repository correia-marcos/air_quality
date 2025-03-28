# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Generate grid visualizations of MERRA-2 raster data for four Latin American cities
# 
# @Description: This script processes MERRA-2 raster data and shapefiles for Bogotá, Ciudad de 
# México, Santiago, and São Paulo to create grid-based visualizations of atmospheric variables 
# over each city’s metropolitan area. The plots provide spatial insights into the data and 
# are saved as high-resolution PDFs for reporting and analysis.
# 
# @Summary: This program performs the following steps:
#   I.   Import MERRA-2 raster data and city shapefiles
#   II.  Process data and generate grid plots for each city
#   III. Save the resulting plots as high-resolution PDFs
# 
# @Date: Nov 2024
# @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Create list of all raster files from MERRA2
nc_files      <- list.files(here::here("data", "raw", "merra2"), full.names = TRUE)

# Open city shapefiles
bogota        <- sf::st_read(here::here("data", "raw", "cities", "Bogota_metro"))
ciudad_mexico <- sf::st_read(here::here("data", "raw", "cities", "Mexico_city"))
santiago      <- sf::st_read(here::here("data", "raw", "cities", "Santiago")) 
sao_paulo     <- sf::st_read(here::here("data", "raw", "cities", "Sao_Paulo"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Define the file to be used for plot - it doesn't matter which is selected
merra_raster <- sample(nc_files, 1)

# Apply function to generate plots
bogota_plot        <- plot_merra2_grid_city(bogota, merra_raster, "Bogotá Metro Area")
ciudad_mexico_plot <- plot_merra2_grid_city(ciudad_mexico, merra_raster, "Ciudad de México")
santiago_plot      <- plot_merra2_grid_city(santiago, merra_raster, "Santiago")
sao_paulo_plot     <- plot_merra2_grid_city(sao_paulo, merra_raster, "Sao Paulo")

# ============================================================================================
# III: Save data
# ============================================================================================
ggsave(here::here("results", "figures", "maps", "bogota_grid.pdf"), bogota_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
ggsave(here::here("results", "figures", "maps", "ciudad_mexico_grid.pdf"), ciudad_mexico_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
ggsave(here::here("results", "figures", "maps", "santiago_grid.pdf"), santiago_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
ggsave(here::here("results", "figures", "maps", "sao_paulo_grid.pdf"), sao_paulo_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")