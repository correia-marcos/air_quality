# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Visualize the location of four metropolitan areas on Latin American
# 
# @Description: This script creates a high-resolution map of Latin America, highlighting the 
# metropolitan areas of Bogotá, Ciudad de México, Santiago, and São Paulo. It processes
# shapefiles for individual cities and merges them with continental data to produce a clear
# visual context. The resulting map is saved as a PDF for use in reports or presentations.
# 
# @Summary: This program performs the following steps:
#   I.   Import shapefiles for city boundaries and continents
#   II.  Process the data to exclude unnecessary regions and combine shapefiles
#   III. Create and export a map of Latin America with the highlighted cities
# 
# @Date: Nov 2024
# @author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Open city shapefiles and convert their CRS
bogota        <- sf::st_read(here::here("data", "raw", "cities", "Bogota_metro"))
ciudad_mexico <- sf::st_read(here::here("data", "raw", "cities", "Mexico_city"))
santiago      <- sf::st_read(here::here("data", "raw", "cities", "Santiago"))
sao_paulo     <- sf::st_read(here::here("data", "raw", "cities", "Sao_Paulo"))

# Open Countries and continent shapefiles
north_america <- ne_countries(continent = "North America", returnclass = "sf")
south_america <- ne_countries(continent = "South America", returnclass = "sf")

# ============================================================================================
# II: Process data
# ============================================================================================
# Remove USA, Canada and Greenland rows from the north america dataframe
countries_to_remove <- c("Canada", "United States of America", "Greenland")
north_america_filtered <- north_america[!(north_america$admin %in% countries_to_remove), ]

# Join both dataframe of America
latin_america <- rbind(south_america, north_america_filtered)

# Create the list of metro regions
regions <- list(bogota, ciudad_mexico, santiago, sao_paulo)
region_names <- c("Bogotá", "Ciudad de México", "Santiago", "São Paulo")

# Apply function to generate the map
latin_america_map <- plot_latin_america_map(
  latin_america = latin_america,
  regions = regions,
  region_names = region_names,
  outline = TRUE)

# ============================================================================================
# II: Save data
# ============================================================================================
# Save plot
ggsave(here::here("results", "figures", "maps", "latin_america_cities.pdf"),
       latin_america_map,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
