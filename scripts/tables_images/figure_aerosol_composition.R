# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Show what each city's particulate matter is actually made of.
#
#' @Description: Two cuts of the MERRA-2 aerosol panels. Section II overlays the four
# cities on one axis per species, which is how the composition differences between them
# become visible; section III gives each city its own multi-panel PDF across all species.
# Both read the PM2.5 panels from process_merra2_panels.R. The species and their x-axis
# limits sit in one specification table, because the limits are chosen per species and
# were previously repeated across six near-identical blocks.
#
#' @Summary:
#   I.   Import data: the four cities' MERRA-2 PM2.5 panels.
#   II.  One cross-city distribution per aerosol species.
#   III. One multi-species distribution set per city.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
dir_pm25       <- here::here("data", "processed", "merra2_pm25")
outdir_joint   <- here::here("results", "figures", "joint_plots")
outdir_bycity  <- here::here("results", "figures", "cities_aerosols")

fig_width  <- 16
fig_height <- 9
fig_dpi    <- 300

bogota        <- read.csv(file.path(dir_pm25, "bogota_pm25.csv"))
ciudad_mexico <- read.csv(file.path(dir_pm25, "ciudad_mexico_pm25.csv"))
santiago      <- read.csv(file.path(dir_pm25, "santiago_pm25.csv"))
sao_paulo     <- read.csv(file.path(dir_pm25, "sao_paulo_pm25.csv"))

city_data <- list("Bogota"           = bogota,
                  "Ciudad de México" = ciudad_mexico,
                  "Santiago"         = santiago,
                  "São Paulo"        = sao_paulo)

# ============================================================================================
# II: Cross-city distribution per aerosol species
# ============================================================================================
dir.create(outdir_joint, recursive = TRUE, showWarnings = FALSE)

# One row per species. The x limits are per-species judgement calls that trim the long
# right tail so the bulk of the distribution stays readable; y is free except for PM2.5.
species_specs <- data.frame(
  variable  = c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS",
                "pm25_estimate"),
  file_stem = c("dust_plot", "organic_carbon_plot", "black_carbon_plot",
                "sea_salt_plot", "sulfate_plot", "pm_25_plot"),
  max_x     = c(7.5, 30, 10, 15, 15, 90),
  max_y     = c(NA, NA, NA, NA, NA, 0.2),
  var_label = c("Dust Surface Mass concentration (µg/m³)",
                "Organic Carbon Surface Mass Concentration (µg/m³)",
                "Black Carbon Surface Mass Concentration (µg/m³)",
                "Sea Salt Surface Mass Concentration (µg/m³)",
                "SO4 Surface Mass Concentration (µg/m³)",
                "PM 2.5 (µg/m³)"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(species_specs))) {
  p <- plot_variable_across_cities(
    city_data,
    variable    = species_specs$variable[i],
    var_label   = species_specs$var_label[i],
    max_y_limit = if (is.na(species_specs$max_y[i])) NULL else species_specs$max_y[i],
    max_x_limit = species_specs$max_x[i])

  ggplot2::ggsave(
    filename = file.path(outdir_joint, paste0(species_specs$file_stem[i], ".pdf")),
    plot     = p, device = cairo_pdf,
    width    = fig_width, height = fig_height, dpi = fig_dpi)
}

# ============================================================================================
# III: Per-city distribution sets
# ============================================================================================
dir.create(outdir_bycity, recursive = TRUE, showWarnings = FALSE)

# plot_city_distributions() returns a list of plots; save_plot_list_to_pdf() writes one
# multi-page PDF per city. File names are ASCII, so they do not track the display labels.
city_distributions <- list(
  Bogota        = plot_city_distributions(bogota, city_name = "Bogotá"),
  Ciudad_Mexico = plot_city_distributions(ciudad_mexico,
                                          city_name = "Ciudad de México"),
  Santiago      = plot_city_distributions(santiago, city_name = "Santiago"),
  Sao_Paulo     = plot_city_distributions(sao_paulo, city_name = "São Paulo"))

for (city_name in names(city_distributions)) {
  save_plot_list_to_pdf(city_distributions[[city_name]], city_name, outdir_bycity)
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
