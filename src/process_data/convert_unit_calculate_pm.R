# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Enhance panel datasets with PM2.5 conversion and save finalized outputs
# 
# @Description: This script processes previously prepared panel datasets for Bogotá, Ciudad de 
# México, Santiago, and São Paulo by applying a function to calculate and add a new column 
# for PM2.5 concentrations. The processed datasets are saved for further analysis.
# 
# @Summary: This program performs the following steps:
#   I.   Import panel datasets for the 4 cities
#   II.  Apply a function to add a PM2.5 variable to each dataset
#   III. Save the finalized datasets in a new directory
# 
# @Date: Nov 2024
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config", "config_utils_process_data.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Open panel data for each city
bogota        <- read.csv(here::here("data",
                                     "processed",
                                     "bogota_panel.csv"))
ciudad_mexico <- read.csv(here::here("data",
                                     "processed",
                                     "ciudad_mexico_panel.csv"))
santiago      <- read.csv(here::here("data",
                                     "processed",
                                     "santiago_panel.csv"))
sao_paulo     <- read.csv(here::here("data",
                                     "processed",
                                     "sao_paulo_panel.csv"))
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
write.csv(bogota,
          file = here::here("data", "processed", "bogota_pm25.csv"),
          row.names = FALSE)
write.csv(ciudad_mexico,
          file = here::here("data", "processed", "ciudad_mexico_pm25.csv"), 
          row.names = FALSE)
write.csv(santiago,
          file = here::here("data", "processed", "santiago_pm25.csv"),
          row.names = FALSE)
write.csv(sao_paulo,
          file = here::here("data", "processed", "sao_paulo_pm25.csv"), 
          row.names = FALSE)
