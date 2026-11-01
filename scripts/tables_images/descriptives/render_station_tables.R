# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Render monitoring counts and pollution exceedance tables.
#
#' @Description: Format four tables from the saved descriptive summaries.
# They report station counts, days above IT1/IT2, hours per exceeding day, and
# annual concentrations relative to WHO AQG 2021 targets. No estimates are rerun.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read saved summaries.
#   II.  Process data: format counts, threshold exceedances and WHO comparisons.
#   III. Save outputs: write the four LaTeX tables.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "plot", "latex_tables.R"))
source(here::here("config", "analysis_settings.R"))

dir_counts <- here::here("data", "processed", "station_counts")
dir_who    <- here::here("data", "processed", "who_exceedances")
dir_exceed <- here::here("data", "processed", "threshold_exceedances")
dir_tables <- here::here("results", "tables")

file_counts <- here::here(dir_counts,
                          sprintf("stations_by_pollutant_%d.parquet", analysis_year))
file_who    <- here::here(dir_who, "who_exceedances_all_cities.parquet")
file_exceed <- here::here(dir_exceed, sprintf("days_and_hours_%d.parquet", analysis_year))
out_counts  <- here::here(dir_tables,
                          sprintf("stations_by_pollutant_%d.tex", analysis_year))
out_days    <- here::here(dir_tables, "table_days_above_thresholds.tex")
out_hours   <- here::here(dir_tables, "table_avg_hours_above_thresholds.tex")
out_who     <- here::here(dir_tables, "who_exceedances_all_cities.tex")

station_counts       <- arrow::read_parquet(file_counts)
who_exceedances      <- arrow::read_parquet(file_who)
threshold_exceedances <- data.table::as.data.table(arrow::read_parquet(file_exceed))

# ========================================================================================
# II: Process data
# ========================================================================================
tex_counts <- latex_station_counts(station_counts = station_counts)

tex_days <- latex_threshold_exceedance_table(exceed_dt = threshold_exceedances,
                                            measure = "days")

tex_hours <- latex_threshold_exceedance_table(exceed_dt = threshold_exceedances,
                                             measure = "hours")

who_table <- table_who_exceedances(exceedances_dt = who_exceedances)
tex_who <- latex_who_exceedances(wide = who_table,
  caption = "Annual PM concentrations vs. WHO AQG 2021 (interim and long-term targets).",
  label = "tab:who_exceedances")

# ========================================================================================
# III: Save outputs
# ========================================================================================
write_latex_table(tex_counts, out_counts, use_bytes = TRUE)
write_latex_table(tex_days, out_days, use_bytes = TRUE)
write_latex_table(tex_hours, out_hours, use_bytes = TRUE)
write_latex_table(tex_who, out_who)
