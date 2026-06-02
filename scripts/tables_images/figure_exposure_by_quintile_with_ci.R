# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Plot PM exposure differences by education quintile with regression-based 95% CIs.
#
# @Description: This script opens the regression estimates produced by
# compute_exposure_regressions.R and creates one figure for each city, pollutant, and
# exposure outcome. The figures show normalized exposure differences relative to the
# highest education quintile, with 95% confidence intervals.
#
# @Summary: This program performs the following steps:
#   I.   Import regression estimates from processed data
#   II.  Define city labels and plotting combinations
#   III. Create and export one CI plot per city-outcome-pollutant combination
#
# @Date: May 2026
# @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_regression <- here::here("data", "processed", "idw_regressions")
outdir_figs    <- here::here("results", "figures", "exposure_by_quintiles", "ci")

# Define regression estimates path
ci_estimates_pq <- here::here(dir_regression, "exposure_ci_estimates_2023.parquet")

# Stop early if the regression artifact has not been created yet
if (!file.exists(ci_estimates_pq)) {
  stop("Regression estimates not found: ", ci_estimates_pq)
}

# Open regression estimates created by compute_exposure_regressions.R
ci_estimates <- data.table::as.data.table(arrow::read_parquet(ci_estimates_pq))

# Ensure output folder exists
dir.create(outdir_figs, recursive = TRUE, showWarnings = FALSE)

# ============================================================================================
# II: Process data
# ============================================================================================
# Define city labels used in the regression table and final figures
city_specs <- data.table::data.table(
  city       = c("Bogota", "Mexico City", "Santiago", "Sao Paulo"),
  city_label = c("Bogotá", "Mexico City", "Santiago", "São Paulo"),
  city_file  = c("bogota", "mexico_city", "santiago", "sao_paulo")
)

# Keep only city names available in the regression artifact
city_specs <- city_specs[city %in% unique(ci_estimates$city)]

# Define available city-outcome-pollutant plotting combinations
plot_grid <- unique(
  ci_estimates[
    !is.na(city) & !is.na(outcome) & !is.na(pollutant),
    .(city, outcome, pollutant)
  ]
)

# Add final city labels and file-safe city names
plot_grid <- merge(plot_grid, city_specs, by = "city", all.x = TRUE)

# Remove combinations without city metadata
plot_grid <- plot_grid[!is.na(city_label) & !is.na(city_file)]

# ============================================================================================
# III: Create and save plots
# ============================================================================================
for (i in seq_len(nrow(plot_grid))) {
  # Select current plotting parameters
  current_city      <- plot_grid$city[i]
  current_label     <- plot_grid$city_label[i]
  current_city_file <- plot_grid$city_file[i]
  current_outcome   <- plot_grid$outcome[i]
  current_pollutant <- plot_grid$pollutant[i]
  
  # Subset the regression estimates for the current city
  ci_city <- ci_estimates[city == current_city]
  
  if (nrow(ci_city) == 0L) {
    message("[", current_label, "] No estimates available — skipping.")
    next
  }
  
  # Create the confidence-interval plot
  p <- plot_exposure_by_quintile_with_ci(
    ci_table   = ci_city,
    outcome    = current_outcome,
    pollutant  = current_pollutant,
    city_label = current_label
  )
  
  # Define output file path
  out_file <- file.path(
    outdir_figs,
    sprintf(
      "%s_%s_%s_ci.pdf",
      current_city_file,
      current_outcome,
      current_pollutant
    )
  )
  
  # Save plot as high-resolution PDF
  ggplot2::ggsave(
    filename = out_file,
    plot     = p,
    device   = cairo_pdf,
    width    = 6,
    height   = 4.5,
    dpi      = 300,
    bg       = "white"
  )
}

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")