# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Generate and save distribution plots for atmospheric variables in four metro areas
# 
# @Description: This script processes PM2.5 panel data for Bogotá, Ciudad de México, Santiago, 
# and São Paulo to generate distribution plots for key atmospheric variables such as Dust Mass 
# (DUSMASS25), Organic Carbon (OCSMASS), Black Carbon (BCSMASS), Sea Salt (SSSMASS25), and 
# PM2.5 estimates. For PM2.5, WHO guideline lines (IT1 and IT2) are added for reference. 
# The resulting plots are saved in separate PDFs for each city.
# 
# @Summary: This program performs the following steps:
#   I.   Import PM2.5 panel data for the four cities
#   II.  Generate distribution plots for selected variables using a pre-defined function
#   III. Save the plots for each city as high-resolution PDFs in the results directory
# 
# @Date: Nov 2024
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Open panel data for each city
bogota        <- read.csv(here("data", "processed", "merra2_pm25", "bogota_pm25.csv"))
ciudad_mexico <- read.csv(here("data", "processed", "merra2_pm25", "ciudad_mexico_pm25.csv"))
santiago      <- read.csv(here("data", "processed", "merra2_pm25", "santiago_pm25.csv"))
sao_paulo     <- read.csv(here("data", "processed", "merra2_pm25", "sao_paulo_pm25.csv"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function to generate a list of plots
bogota_distribution        <- plot_city_distributions(bogota, city_name = "Bogotá")
ciudad_mexico_distribution <- plot_city_distributions(bogota, city_name = "Ciudad de México")
santiago_distribution      <- plot_city_distributions(santiago, city_name = "Santiago")
sao_paulo_distribution     <- plot_city_distributions(sao_paulo, city_name = "São Paulo")

# ============================================================================================
# III: Save data - we need to apply a function to save the lists of plots
# ============================================================================================
# Combine all plot lists into a named list
all_plot_lists <- list(
  Bogota = bogota_distribution,
  Ciudad_Mexico = ciudad_mexico_distribution,
  Santiago = santiago_distribution,
  Sao_Paulo = sao_paulo_distribution)

# Define the output directory
output_dir <- here::here("results", "figures", "cities_aerosols")

# Save each list of plots into a separate PDF
for (city_name in names(all_plot_lists)) {
  save_plot_list_to_pdf(all_plot_lists[[city_name]], city_name, output_dir)
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")