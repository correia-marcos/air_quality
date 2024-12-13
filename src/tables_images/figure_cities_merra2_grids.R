# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create image of the four cities placed on Latin American Continent
#
# @Date: Nov 2024
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))
library("ggplot2") # incompatibility issue with R version and "Groundhog"

# ============================================================================================
# I: Import data
# ============================================================================================
# Create list of all raster files from MERRA2
nc_files      <- list.files(here::here("data", "merra2"), full.names = TRUE)

# Open city shapefiles
bogota        <- sf::st_read(here::here("data", "cities", "Bogota_metro"))
ciudad_mexico <- sf::st_read(here::here("data", "cities", "Mexico_city"))
santiago      <- sf::st_read(here::here("data", "cities", "Santiago")) 
sao_paulo     <- sf::st_read(here::here("data", "cities", "Sao_Paulo"))

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
ggsave(here::here("results", "figures", "bogota_grid.pdf"), bogota_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
ggsave(here::here("results", "figures", "ciudad_mexico_grid.pdf"), ciudad_mexico_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
ggsave(here::here("results", "figures", "santiago_grid.pdf"), santiago_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
ggsave(here::here("results", "figures", "sao_paulo_grid.pdf"), sao_paulo_plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
