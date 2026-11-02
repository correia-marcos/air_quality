# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Render the share of reported readings removed by cleaning, by education quintile.
#
#' @Description: Read the table saved by compute_cleaning_removals_by_quintile.R and write
# it in the layout of the manuscript's availability table (pollutant and city rows, Q1 to
# Q5 columns), so the two can be read side by side.
#
#' @Summary:
#   I.   Import data: source functions and settings, set paths and read the table.
#   II.  Process data: format the LaTeX table.
#   III. Save outputs: write the LaTeX table.
#
#' @Date: November 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the LaTeX helpers and the shared settings
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "plot", "latex_tables.R"))
source(here::here("config", "analysis_settings.R"))

# Set the input table and the output file
file_removals <- here::here("data", "processed", "missing_proportions",
  sprintf("cleaning_removals_by_education_quintile_%d.parquet", analysis_year))
out_removals  <- here::here("results", "tables",
  sprintf("cleaning_removals_by_education_quintile_%d.tex", analysis_year))

# Read the shares of reported readings removed by cleaning
removals_by_quintile <- data.table::as.data.table(arrow::read_parquet(file_removals))

# ========================================================================================
# II: Process data
# ========================================================================================
# Format the shares as the pollutant-by-city, Q1-Q5 table
tex_removals <- latex_missing_by_quintile(dt = removals_by_quintile, digits = 4L)

# ========================================================================================
# III: Save outputs
# ========================================================================================
write_latex_table(tex_removals, out_removals)
