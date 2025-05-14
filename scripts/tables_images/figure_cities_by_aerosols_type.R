# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Generate and save distribution plots of aerosol variables across four metro areas
# 
# @Description: This script imports processed panel data for Bogotá, Ciudad de México, Santiago, 
# and São Paulo (metro areas) to create distribution plots of key aerosol variables.
# 
# @Summary: This program performs the following steps:
#   I.   Import processed panel data for the four cities
#   II.  Generate distribution plots for aerosol variables using a standardized function
#   III. Save the resulting plots as high-resolution PDFs
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
# Create a list of dataframes for the plotting function
city_data     <- list("Bogota" = bogota,
                      "Ciudad de México" = ciudad_mexico,
                      "Santiago" = santiago,
                      "São Paulo" = sao_paulo)

# Apply function to generate the distribution of plots
dust_plot     <- plot_variable_across_cities(
  city_data,
  variable    = "DUSMASS25",
  var_label   = "Dust Surface Mass concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 7.5)

organic_carbon_plot <- plot_variable_across_cities(
  city_data,
  variable    = "OCSMASS",
  var_label   = "Organic Carbon Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 30)

black_carbon_plot <- plot_variable_across_cities(
  city_data,
  variable    = "BCSMASS",
  var_label   = "Black Carbon Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 10)

sea_salt_plot <- plot_variable_across_cities(
  city_data,
  variable    = "SSSMASS25",
  var_label   = "Sea Salt Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 15)

sulfate_plot <- plot_variable_across_cities(
  city_data,
  variable    = "SO4SMASS",
  var_label   = "SO4 Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 15)

pm_25_plot    <- plot_variable_across_cities(
  city_data,
  variable    = "pm25_estimate",
  var_label   = "PM 2.5 (µg/m³)",
  max_y_limit = 0.2,
  max_x_limit = 90)

# ============================================================================================
# III: Save data - we need to apply a function to save the lists of plots
# ============================================================================================

# Save the Dust Surface Mass concentration plot
ggsave(
  filename = here::here("results", "figures", "joint_plots", "dust_plot.pdf"),
  plot = dust_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the Organic Carbon Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "joint_plots", "organic_carbon_plot.pdf"),
  plot = organic_carbon_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the Black Carbon Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "joint_plots", "black_carbon_plot.pdf"),
  plot = black_carbon_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the Sea Salt Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "joint_plots", "sea_salt_plot.pdf"),
  plot = sea_salt_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the SO4 Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "joint_plots", "sulfate_plot.pdf"),
  plot = sulfate_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the PM 2.5 plot
ggsave(
  filename = here::here("results", "figures", "joint_plots", "pm_25_plot.pdf"),
  plot = pm_25_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")