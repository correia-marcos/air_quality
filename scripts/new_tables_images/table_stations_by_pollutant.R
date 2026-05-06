# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Render the "stations-per-pollutant" LaTeX table.
#
# @Description: Reads the combined Parquet written by
# scripts/process_data/count_stations_by_pollutant.R and writes a booktabs .tex under
# results/tables/station_counts/.
#
# @Summary:
#   I.   Load Parquet
#   II.  Call table_stations_by_pollutant(save_latex_table = TRUE)
#   III. Done
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
pq <- here::here("data", "processed", "station_counts",
                 "stations_by_pollutant_2023.parquet")
out_tex <- here::here("results", "tables", "station_counts",
                      "stations_by_pollutant_2023.tex")

if (!file.exists(pq))
  stop("Run scripts/process_data/count_stations_by_pollutant.R first (missing: ", pq, ")")

long <- arrow::read_parquet(pq)

# ============================================================================================
# II and III: Render + save
# ============================================================================================
dir.create(dirname(out_tex), recursive = TRUE, showWarnings = FALSE)
table_stations_by_pollutant(
  stations_long    = long,
  save_latex_table = TRUE,
  out_file         = out_tex,
  caption          = "Number of monitoring stations reporting each pollutant, by city (2023).",
  label            = "tab:stations_by_pollutant",
  overwrite_tex    = TRUE
)

cat("Script from the IDB project executed successfully in the Docker container!\n")
