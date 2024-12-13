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
# Open panel data for each city
bogota        <- read.csv(here::here("results", "panel_data", "bogota_panel.csv"))
ciudad_mexico <- read.csv(here::here("results", "panel_data", "ciudad_mexico_panel.csv"))
santiago      <- read.csv(here::here("results", "panel_data", "santiago_panel.csv"))
sao_paulo     <- read.csv(here::here("results", "panel_data", "sao_paulo_panel.csv"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function to generate a new column with PM 2.5 measure
bogota        <- convert_and_add_pm25(bogota)
ciudad_mexico <- convert_and_add_pm25(ciudad_mexico)
santiago      <- convert_and_add_pm25(santiago)
sao_paulo     <- convert_and_add_pm25(sao_paulo)

# ============================================================================================
# III: Save data
# ============================================================================================
# Save processed dataframes
write.csv(bogota, file = "results/panel_data/bogota_panel_pm25.csv",
          row.names = FALSE)
write.csv(ciudad_mexico, file = "results/panel_data/ciudad_mexico_panel_pm25.csv", 
          row.names = FALSE)
write.csv(santiago, file = "results/panel_data/santiago_panel_pm25.csv",
          row.names = FALSE)
write.csv(sao_paulo, file = "results/panel_data/sao_paulo_panel_pm25.csv", 
          row.names = FALSE)
