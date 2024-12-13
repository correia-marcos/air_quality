# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create image of the four cities placed on Latin American Continent
#
# @Date: Nov 2024
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define the standard CRS for all spatial data
crs_value <- 4674

# Open city shapefiles and convert their CRS
bogota        <- sf::st_read(here::here("data", "cities", "Bogota")) %>% 
  sf::st_transform(, crs_value)
cidade_mexico <- sf::st_read(here::here("data", "cities", "Mexico city")) %>% 
  sf::st_transform(, crs_value)
santiago      <- sf::st_read(here::here("data", "cities", "Santiago")) %>% 
  sf::st_transform(, crs_value)
sao_paulo     <- sf::st_read(here::here("data", "cities", "Sao Paulo")) %>%
  sf::st_transform(, crs_value)

# Open Countries and continent shapefiles
north_america <- ne_countries(continent = "North America", returnclass = "sf")
south_america <- ne_countries(continent = "South America", returnclass = "sf")

# ============================================================================================
# II: Process data
# ============================================================================================

# Remove Us and Canada from the north america dataframe
