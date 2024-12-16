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
# Create a list of dataframes for the plotting function
city_data     <- list("Bogota" = bogota,
                  "Ciudad de México" = ciudad_mexico,
                  "Santiago" = santiago,
                  "São Paulo" = sao_paulo)

# Apply function to generate the distribution of plots
dust_plot     <- plot_variable_across_cities(
  city_data,
  variable = "DUSMASS25",
  var_label = "Dust Surface Mass concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 7.5)

organic_carbon_plot <- plot_variable_across_cities(
  city_data,
  variable = "OCSMASS",
  var_label = "Organic Carbon Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 30)

black_carbon_plot <- plot_variable_across_cities(
  city_data,
  variable = "BCSMASS",
  var_label = "Black Carbon Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 10)

sea_salt_plot <- plot_variable_across_cities(
  city_data,
  variable = "SSSMASS25",
  var_label = "ea Salt Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 15)

sulfate_plot <- plot_variable_across_cities(
  city_data,
  variable = "SO4SMASS",
  var_label = "SO4 Surface Mass Concentration (µg/m³)",
  max_y_limit = NULL,
  max_x_limit = 15)

pm_25_plot    <- plot_variable_across_cities(
  city_data,
  variable = "pm25_estimate",
  var_label = "PM 2.5 (µg/m³)",
  max_y_limit = 0.2,
  max_x_limit = 90)

# ============================================================================================
# III: Save data - we need to apply a function to save the lists of plots
# ============================================================================================

# Save the Dust Surface Mass concentration plot
ggsave(
  filename = here::here("results", "figures", "dust_plot.pdf"),
  plot = dust_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the Organic Carbon Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "organic_carbon_plot.pdf"),
  plot = organic_carbon_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the Black Carbon Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "black_carbon_plot.pdf"),
  plot = black_carbon_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the Sea Salt Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "sea_salt_plot.pdf"),
  plot = sea_salt_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the SO4 Surface Mass Concentration plot
ggsave(
  filename = here::here("results", "figures", "sulfate_plot.pdf"),
  plot = sulfate_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)

# Save the PM 2.5 plot
ggsave(
  filename = here::here("results", "figures", "pm_25_plot.pdf"),
  plot = pm_25_plot,
  device = cairo_pdf,
  width = 16, height = 9, dpi = 300
)
