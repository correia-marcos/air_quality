# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Report PM10 and PM2.5 data availability by education quintile, for each city.
#
# @Description: Assigns every monitoring station the education quintile of its nearest
# census geographic unit, joins the cleaned hourly readings to that assignment, and
# computes the share of non-missing observations by pollutant, city and quintile. This
# asks whether monitoring coverage is itself unequal: a smaller share in the lower
# quintiles means the exposure estimates are least reliable exactly where the paper's
# question bites. Outputs go to results/tables/ as CSV, Parquet and LaTeX.
#
# @Summary:
#   I.   Import data: define the per-city inputs and analysis options.
#   II.  Process: compute the availability shares per city.
#   III. Save: write CSV, Parquet and the LaTeX table.
#
# @Date: June 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define the output folder
outdir_tables <- here::here("results", "tables", "missing_proportions")

# Analysis options. "available" reports non-missing shares, "missing" the complement.
analysis_year <- 2023L
pollutants    <- c("pm10", "pm25")
report        <- "available"

# One row per city. Santiago uses the 2024 commune-level census here, because this
# diagnostic needs the same geography as its distance matrix, not the 2017 zonas.
city_specs <- data.table::data.table(
  city       = c("Bogota", "Mexico City", "Santiago", "Sao Paulo"),
  city_order = 1:4,
  geo_id_col = c("GEO_ID", "CVE_MUN", "comuna", "code_weighting"),
  pollution_dir = here::here(
    "data", "processed", "monitoring_stations_outliers",
    c("bogota_metro_clean", "cdmx_metro_clean",
      "santiago_metro_clean", "sao_paulo_metro_clean")
  ),
  dist_pq = here::here(
    "data", "processed", "distances_matrices",
    c("bogota_2018", "cdmx_2020", "santiago_2024", "sao_paulo_2010"),
    "matrix_geo_station_distances.parquet"
  ),
  census_file = here::here(
    "data", "interim", "census",
    c("bogota_2018", "cdmx_extended_2020", "santiago_2024", "sao_paulo_2010"),
    c("census_2018_metro_individual.csv", "census_metro_individual_2020.csv",
      "census_santiago_individual_2024.csv", "census_sp_individual_2010.csv")
  )
)

# ============================================================================================
# II: Process data
# ============================================================================================
# Create the output folder before processing
dir.create(outdir_tables, recursive = TRUE, showWarnings = FALSE)

missing_by_quintile <- data.table::rbindlist(lapply(
  seq_len(nrow(city_specs)),
  function(i) {
    compute_missing_by_quintile(
      city          = city_specs$city[i],
      city_order    = city_specs$city_order[i],
      pollution_dir = city_specs$pollution_dir[i],
      dist_pq       = city_specs$dist_pq[i],
      census_file   = city_specs$census_file[i],
      geo_id_col    = city_specs$geo_id_col[i],
      pollutants    = pollutants,
      year          = analysis_year,
      report        = report)
  }
))

# ============================================================================================
# III: Save data
# ============================================================================================
stem <- paste0("missing_by_education_quintile_", analysis_year)

csv_path <- file.path(outdir_tables, paste0(stem, ".csv"))
pq_path  <- file.path(outdir_tables, paste0(stem, ".parquet"))
tex_path <- file.path(outdir_tables, paste0(stem, ".tex"))

data.table::fwrite(missing_by_quintile, csv_path)
arrow::write_parquet(missing_by_quintile, pq_path)
writeLines(latex_missing_by_quintile(missing_by_quintile), tex_path)

message("Wrote: ", csv_path)
message("Wrote: ", pq_path)
message("Wrote: ", tex_path)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
