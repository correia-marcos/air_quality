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
# Open panel data for each city
bogota        <- read.csv(here::here("results", "panel_data", "bogota_panel_pm25.csv"))
ciudad_mexico <- read.csv(here::here("results", "panel_data", "ciudad_mexico_panel_pm25.csv"))
santiago      <- read.csv(here::here("results", "panel_data", "santiago_panel_pm25.csv"))
sao_paulo     <- read.csv(here::here("results", "panel_data", "sao_paulo_panel_pm25.csv"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Define the file to be used for plot - it doesn't matter which is selected
bogota_distribution        <- plot_city_distributions(bogota, city_name = "Bogotá")
ciudad_mexico_distribution <- plot_city_distributions(bogota, city_name = "Ciudad de México")
santiago_distribution      <- plot_city_distributions(santiago, city_name = "Santiago")
sao_paulo_distribution     <- plot_city_distributions(sao_paulo, city_name = "São Paulo")

# ============================================================================================
# III: Save data
# ============================================================================================