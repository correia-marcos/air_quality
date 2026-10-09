# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Render the appendix exposure tables by socioeconomic group.
#
#' @Description: Format four tables from saved 3 km exposure summaries and regressions.
# Education uses quintiles; the income table accommodates CDMX quintiles and
# São Paulo deciles. Santiago 2024 remains outside these manuscript tables.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read saved summaries.
#   II.  Process data: format mean concentrations and hours above thresholds by group.
#   III. Save outputs: write the education and income tables in that order.
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

buffer_km <- individual_exposure_buffer_km
dir_reg    <- here::here("data", "processed", "idw_regressions")
dir_tables <- here::here("results", "tables")

file_summary_edu <- here::here(dir_reg,
  sprintf("exposure_group_summaries_education_%dkm_%d.parquet", buffer_km, analysis_year))
file_summary_inc <- here::here(dir_reg,
  sprintf("exposure_group_summaries_income_%dkm_%d.parquet", buffer_km, analysis_year))
file_ci_edu <- here::here(dir_reg,
  sprintf("exposure_ci_estimates_education_%dkm_%d.parquet", buffer_km, analysis_year))
file_ci_inc <- here::here(dir_reg,
  sprintf("exposure_ci_estimates_income_%dkm_%d.parquet", buffer_km, analysis_year))
out_means_edu <- here::here(dir_tables, "table_means_education_quintiles.tex")
out_means_inc <- here::here(dir_tables, "table_means_income_groups.tex")
out_hours_edu <- here::here(dir_tables, "table_hours_above_education_quintiles.tex")
out_hours_inc <- here::here(dir_tables, "table_hours_above_income_groups.tex")

summary_edu <- data.table::as.data.table(arrow::read_parquet(file_summary_edu))
summary_inc <- data.table::as.data.table(arrow::read_parquet(file_summary_inc))
ci_edu      <- data.table::as.data.table(arrow::read_parquet(file_ci_edu))
ci_inc      <- data.table::as.data.table(arrow::read_parquet(file_ci_inc))

# ========================================================================================
# II: Process data
# ========================================================================================
tex_means_edu <- latex_exposure_means_by_group(summary_dt = summary_edu, ci_dt = ci_edu,
  panel_cities = exposure_table_cities_edu, panel_labels = exposure_table_labels_edu,
  n_groups = 5L)

tex_means_inc <- latex_exposure_means_by_group(summary_dt = summary_inc, ci_dt = ci_inc,
  panel_cities = exposure_table_cities_inc, panel_labels = exposure_table_labels_inc,
  n_groups = 10L)

tex_hours_edu <- latex_exposure_hours_by_group(summary_dt = summary_edu,
  panel_cities = exposure_table_cities_edu, panel_labels = exposure_table_labels_edu)

tex_hours_inc <- latex_exposure_hours_by_group(summary_dt = summary_inc,
  panel_cities = exposure_table_cities_inc, panel_labels = exposure_table_labels_inc)

# ========================================================================================
# III: Save outputs
# ========================================================================================
write_latex_table(tex_means_edu, out_means_edu, use_bytes = TRUE)
write_latex_table(tex_means_inc, out_means_inc, use_bytes = TRUE)
write_latex_table(tex_hours_edu, out_hours_edu, use_bytes = TRUE)
write_latex_table(tex_hours_inc, out_hours_inc, use_bytes = TRUE)
