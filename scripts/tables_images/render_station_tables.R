# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Render the LaTeX tables describing the monitoring network and what it measured.
#
#' @Description: Turns the station-count, WHO-exceedance and threshold-exceedance Parquet
# files written by compute_descriptive_tables.R into .tex fragments. Nothing is
# calculated here: the counts describe the infrastructure that exists, the exceedance
# factors compare annual concentrations against the WHO AQG 2021 targets, and the two
# threshold tables report days above IT1/IT2 and the hours per exceeding day. The three
# fragments the manuscript prints are written to results/tables/ as well as to the
# repo's own results/tables/.
#
#' @Summary:
#   I.   Import data: locate the process-stage Parquet files.
#   II.  Render: station counts, WHO exceedance factors, threshold exceedances.
#   III. Report where each .tex landed.
#
#' @Date: September 2026
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
dir_exceed    <- here::here("data", "processed", "threshold_exceedances")
outdir_tables <- here::here("results", "tables")
outdir_paper  <- here::here("results", "tables")

analysis_year <- 2023L

station_counts  <- arrow::read_parquet(
  file.path(dir_counts, paste0("stations_by_pollutant_", analysis_year, ".parquet")))
who_exceedances <- arrow::read_parquet(
  file.path(dir_who, "who_exceedances_all_cities.parquet"))
threshold_exceedances <- data.table::as.data.table(arrow::read_parquet(
  file.path(dir_exceed, paste0("days_and_hours_", analysis_year, ".parquet"))))

# ============================================================================================
# II: Render tables
# ============================================================================================
dir.create(outdir_paper, recursive = TRUE, showWarnings = FALSE)

# Number of monitoring stations reporting each pollutant, by city. The manuscript prints
# this one; export selection points to this single canonical file.
tex_counts <- file.path(outdir_tables,
                        paste0("stations_by_pollutant_", analysis_year, ".tex"))
write_station_count_latex(station_counts = station_counts, out_file = tex_counts)

# Days above IT1/IT2, and the mean hours per exceeding day. Two views of one artefact.
tab_days  <- latex_threshold_exceedance_table(threshold_exceedances, measure = "days")
tab_hours <- latex_threshold_exceedance_table(threshold_exceedances, measure = "hours")

dir.create(outdir_tables,
           recursive = TRUE, showWarnings = FALSE)

for (d in outdir_tables) {
  writeLines(tab_days,  file.path(d, "table_days_above_thresholds.tex"), useBytes = TRUE)
  writeLines(tab_hours, file.path(d, "table_avg_hours_above_thresholds.tex"),
             useBytes = TRUE)
}

# Annual PM concentrations against the WHO AQG 2021 interim and long-term targets.
tex_who <- file.path(outdir_tables, "who_exceedances_all_cities.tex")
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
message("Wrote the two threshold tables to: ", outdir_paper)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
