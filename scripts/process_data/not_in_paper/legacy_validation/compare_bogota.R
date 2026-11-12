# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Run all validation comparisons for Bogotá and render a single city-level report.
#
#' @Description: This script compares the new automated pipeline against the legacy dataset.
#   Artefacts are written to: data/validation/bogota/{data_type}/
#
#' @Details: This script is the single entry point for comparing every data layer
# of the Bogotá pipeline against the coauthor's legacy dataset. Comparison functions are called
# in pipeline order (raw → interim → processed → results). It also maps education quintiles
# and 2023 PM at the legacy stations on the 2005 area, at 3 and 5 km. All artefacts are
# written to:
#     data/validation/bogota/
# A single self-contained HTML report is rendered from bogota_report.qmd.
#
#' @Summary:
#   I. Setup.
#   II. Process and Compare Data.
#   III. Save the 2005 maps and render the city report.
#
#' @Date: April 2026
#' @Author: Marcos Paulo
# ============================================================================================

# Load all utility functions and city configs
# ============================================================================================
# I: Setup
# ============================================================================================
source(here::here("src", "general_utilities","config_utils_validation_old_version.R"))
source(here::here("src", "general_utilities","config_utils_process_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))
source(here::here("src", "city_specific", "registry.R"))
load_city_modules()

# Register Tex Gyre Pagella and set the paper ggplot theme for the maps
set_paper_theme()

# Root folder for all Bogotá comparison artefacts
city_id   <- "bogota"
cfg       <- city_cfg(city_id)

# Attach the validation/comparison sublist to cfg
cfg$compare <- build_compare_cfg(city_id)

# Set the inputs of the 2005 maps (prepare_bogota_2005_inputs.R) and their folder
dir_geography_2005 <- here::here("data", "interim", "geospatial_data", "bogota")
file_metro_2005    <- here::here(dir_geography_2005, "bogota_area_metro_2005.gpkg")
file_stations_2005 <- here::here(dir_geography_2005,
                                 "bogota_2005_stations_buffer_metro.gpkg")
file_census_2005   <- here::here("data", "interim", "census", "bogota_extended_2005",
                                 "collapse_metro_area_extended.parquet")
dir_pollution      <- here::here("data", "interim", "monitoring_stations",
                                 "bogota_metro_dataset")
dir_maps           <- here::here(cfg$compare$city_dir, "maps")

# Read the 2005 area, its stations and the extended 2005 census
metro_2005    <- sf::st_read(file_metro_2005, quiet = TRUE)
stations_2005 <- sf::st_read(file_stations_2005, quiet = TRUE)
census_2005   <- arrow::read_parquet(file_census_2005)

# ============================================================================================
# II: Process and Compare Data
# ============================================================================================
# 1. Compare Raw Ground Stations
gs_results <- compare_ground_stations(
  cfg      = cfg,
  out_root = cfg$compare$city_dir)

# 2. Compare Metro area
metro_results <- compare_metro_area(
  cfg           = cfg,
  out_root      = cfg$compare$city_dir,
  station_audit = if (!is.null(gs_results)) gs_results$station_audit else NULL)

# 3, Compare Census data
census_results <- compare_census(
  cfg      = cfg,
  out_root = cfg$compare$city_dir)

# 4. Compare station-to-station and Station-to-locality distances
idw_results <- compare_idw(
  cfg           = cfg,
  out_root      = cfg$compare$city_dir,
  station_audit = if (!is.null(gs_results)) gs_results$station_audit else NULL
)

# 5. Compare Outlier-corrected pollution panel
outlier_results <- compare_outlier_procedure(
  cfg           = cfg,
  out_root      = cfg$compare$city_dir,
  station_audit = if (!is.null(gs_results)) gs_results$station_audit else NULL
)

# 6. Map education quintiles and 2023 PM at the legacy stations, at 3 and 5 km
# Keep the 19 stations with legacy pollution data, matched by name to step 1's audit
legacy_names         <- gs_results$station_audit$station[gs_results$station_audit$in_legacy]
station_keys_2005    <- legacy_station_key(stations_2005$station_name,
                                           cfg$compare$residual_map)
legacy_stations_2005 <- stations_2005[station_keys_2005 %in% legacy_names, ]

maps_2005 <- lapply(c("3" = 3, "5" = 5), function(buffer_km) {
  plot_inequality_pollution(metro_sf    = metro_2005,
                            stations_sf = legacy_stations_2005,
                            arrow_dir   = dir_pollution,
                            census_df   = census_2005,
                            join_sf_col = "GEO_ID",
                            join_df_col = "GEO_ID",
                            station_col = "station_name",
                            ed_col      = "escolaridad_avg",
                            pop_col     = "n",
                            pollutants  = c("pm25", "pm10"),
                            year_filter = 2023,
                            buffer_km   = buffer_km,
                            city_label  = "",
                            label_x_pct = 0.45,
                            label_y_pct = 0.99,
                            legend_pos  = c(0.95, 0.25))
})

# 10: Console summary
message("\n", strrep("=", 60))
message(sprintf("%s — Execution Summary", toupper(city_id)))
message(strrep("=", 60))

# Group the summary tables into a named list
summaries <- list(
  "Ground Stations"   = if (!is.null(gs_results)) gs_results$diff_summary else NULL,
  "Metro Area"        = if (!is.null(metro_results)) metro_results$summary else NULL,
  "Census" = if (!is.null(census_results)) census_results$collapsed_summary else NULL,
  "Distance Matrices" = if (!is.null(idw_results))
    idw_results$station_dist_summary else NULL,
  "Outlier Detection" = if (!is.null(outlier_results))
    outlier_results$step_summary else NULL
)

# Loop through and print cleanly
for (name in names(summaries)) {
  df <- summaries[[name]]
  if (!is.null(df)) {
    message(sprintf("\n--- %s ---", name))

    # Optional: Keep your specific row count logic for Ground Stations
    if (name == "Ground Stations") {
      message("Rows only in legacy : ", format(nrow(gs_results$only_legacy),
        big.mark = ","))
      message("Rows only in new    : ", format(nrow(gs_results$only_new), big.mark = ","))
    }

    # as.data.frame strips the noisy tibble headers/data types from the console output
    print(as.data.frame(df), row.names = FALSE)
  }
}

# =============================================================================================
# III: Save the 2005 maps and render the city report
# =============================================================================================
# Save the 3 and 5 km maps in data/validation/bogota/maps/
dir.create(dir_maps, recursive = TRUE, showWarnings = FALSE)
for (buffer_km in names(maps_2005)) {
  ggplot2::ggsave(
    filename = here::here(dir_maps, sprintf(
      "map_bogota_new_metro_old_stations_2005_%skm.pdf", buffer_km)),
    plot     = maps_2005[[buffer_km]],
    device   = cairo_pdf,
    width    = 12, height = 8, dpi = 300, bg = "white")
}

# Render the report from the comparison files above and save the HTML in the ignored
# data/validation/bogota/ folder. Quarto ships with RStudio.
report_file <- render_validation_report(qmd_path  = cfg$compare$qmd_path,
                                        html_dest = cfg$compare$html_dest,
                                        params    = list(city_id  = city_id,
                                                         out_root = cfg$compare$out_root))

cat("Script from the IDB project executed successfully!\n")
