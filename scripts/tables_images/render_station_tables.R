# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Render the LaTeX tables describing the monitoring network and what it measured.
#
#' @Description: Turns the station-count and WHO-exceedance Parquet files written by
# compute_descriptive_tables.R into .tex fragments under results/tables/. Nothing is
# calculated here: the counts describe the infrastructure that exists, and the
# exceedances compare annual concentrations against the WHO AQG 2021 targets.
#
#' @Summary:
#   I.   Import data: locate the process-stage Parquet files.
#   II.  Render: station counts by pollutant, then WHO exceedance factors.
#   III. Report where each .tex landed.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_counts    <- here::here("data", "processed", "station_counts")
dir_who       <- here::here("data", "processed", "who_exceedances")
outdir_tables <- here::here("results", "tables")

analysis_year <- 2023L

station_counts  <- arrow::read_parquet(
  file.path(dir_counts, paste0("stations_by_pollutant_", analysis_year, ".parquet")))
who_exceedances <- arrow::read_parquet(
  file.path(dir_who, "who_exceedances_all_cities.parquet"))

# ============================================================================================
# II: Render tables
# ============================================================================================
# Number of monitoring stations reporting each pollutant, by city.
tex_counts <- file.path(outdir_tables, "station_counts",
                        paste0("stations_by_pollutant_", analysis_year, ".tex"))
write_station_count_latex(station_counts = station_counts, out_file = tex_counts)

# Annual PM concentrations against the WHO AQG 2021 interim and long-term targets.
tex_who <- file.path(outdir_tables, "who_exceedances", "who_exceedances_all_cities.tex")
dir.create(dirname(tex_who), recursive = TRUE, showWarnings = FALSE)

table_who_exceedances(
  exceedances_dt   = who_exceedances,
  save_latex_table = TRUE,
  out_file         = tex_who,
  caption          = paste("Annual PM concentrations vs. WHO AQG 2021",
                           "(interim and long-term targets)."),
  label            = "tab:who_exceedances",
  overwrite_tex    = TRUE)

# ============================================================================================
# III: Report
# ============================================================================================
message("Wrote: ", tex_counts)
message("Wrote: ", tex_who)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
