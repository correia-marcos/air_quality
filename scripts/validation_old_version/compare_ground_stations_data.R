# ============================================================================
# IDB: Air monitoring
# ============================================================================
# @Goal  : Compare raw ground-station air quality data across all cities.
#
# @Description: Validates the new scraping/API pipeline against the legacy datasets from MP. 
# For each city we match records on station x year x month x day x hour, identify rows present 
# in only one source, and measure cell-level pollutant differences.
# Station names are normalised (with .std_name) before matching. A small residual_map inside
# each city cfg handles entries normalisation cannot resolve. All comparison parameters live 
# inside cfg$compare (see src/city_specific/bogota.R and equivalents). No separate
# comparison_config.R is needed.
#
# @Summary:
#   I.   Load libraries, utility functions, city configs via registry.
#   II.  Set run parameters.
#   III. Run compare_ground_stations() for each city.
#   IV.  Print console summary; render per-city Quarto report.
#
# @Date  : March 2026
# @Author: Marcos Paulo
# ============================================================================

# Load utility functions and city configs
source(here::here("src", "general_utilities", "config_utils_validation_old_version.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "bogota.R"))
source(here::here("src", "city_specific", "cdmx.R"))
source(here::here("src", "city_specific", "santiago.R"))
source(here::here("src", "city_specific", "sao_paulo.R"))

# ============================================================================
# I: Parameters
# ============================================================================
# Root folder where per-city comparison artefacts will be written
out_root <- here::here("results", "validation_rep_package")

# Cities to validate — comment out those whose cfg$compare is not yet set
cities_to_run <- c(
  "bogota"
  # "cdmx",
  # "santiago",
  # "sao_paulo"
)

# Per-pollutant tolerances (0 = exact match required)
tolerances <- c(pm10 = 0, pm25 = 0, ozone = 0, co = 0, no2 = 0)

# ============================================================================
# II: Run comparisons
# ============================================================================
results <- vector("list", length(cities_to_run))
names(results) <- cities_to_run

for (city_id in cities_to_run) {
  cfg <- city_cfg(city_id)   # retrieved from registry
  message("\n", strrep("=", 60))
  message("City: ", cfg$id)
  message(strrep("=", 60))
  
  results[[city_id]] <- tryCatch(
    compare_ground_stations(
      cfg      = cfg,
      out_root = out_root,
      tol      = tolerances,
      quiet    = FALSE
    ),
    error = function(e) {
      warning(
        "\n[", city_id, "] compare_ground_stations() failed:\n",
        conditionMessage(e)
      )
      NULL
    }
  )
}

# ============================================================================
# III: Console summary + Quarto reports
# ============================================================================
for (city_id in cities_to_run) {
  res <- results[[city_id]]
  if (is.null(res)) next
  
  cfg <- city_cfg(city_id)
  message("\n--- ", cfg$id, " ---")
  message(
    "  Rows only in legacy : ",
    format(nrow(res$only_legacy), big.mark = ",")
  )
  message(
    "  Rows only in new    : ",
    format(nrow(res$only_new), big.mark = ",")
  )
  print(res$diff_summary)
  
  html_name        <- paste0("ground_stations_report_", city_id, ".html")
  qmd_path         <- here::here(out_root, "bogota", "ground_stations_report.qmd")
  html_next_to_qmd <- file.path(dirname(qmd_path), html_name)
  report_dest      <- file.path(out_root, city_id, html_name)
  
  tryCatch({
    quarto::quarto_render(
      input          = qmd_path,
      output_file    = html_name,
      execute_params = list(
        city_id  = city_id,
        out_root = out_root
      )
    )
    # No copy needed — .qmd is already in the results folder,
    # so Quarto writes the HTML there directly.
    message("  Report: ", report_dest)
  },
  error = function(e) warning(
    "[", city_id, "] Report rendering failed:\n",
    conditionMessage(e)
  ))
}


# Print a success message for Docker container runs
cat("Script from the IDB project executed successfully!\n")