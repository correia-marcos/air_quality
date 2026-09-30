# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Render missingness by station, month, hour and education.
#
#' @Description: Use the original reported-hour panel for station, month and hour tables.
# These missing shares exclude removals by the outlier procedure. The education
# table reports available readings among stored station-hour rows in each quintile.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read saved summaries.
#   II.  Process data: format missingness tables and the education coverage table.
#   III. Save outputs: write each city/dimension table, then the education table.
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

dir_missing <- here::here("data", "processed", "missing_proportions")
dir_tables  <- here::here("results", "tables")
city_files  <- c("bogota", "cdmx", "santiago", "sao_paulo_metro")

# Keep each city and dimension in a named list.
stems <- unlist(lapply(missing_table_dimensions, function(dimension) {
  sprintf("%s_%s_missing_by_%s", city_files, missing_table_panel, dimension)
}))
files_missing <- here::here(dir_missing, paste0(stems, ".parquet"))
out_missing   <- here::here(dir_tables, paste0(stems, ".tex"))
file_quintile <- here::here(dir_missing,
  sprintf("missing_by_education_quintile_%d.parquet", analysis_year))
out_quintile <- here::here(dir_tables,
  sprintf("missing_by_education_quintile_%d.tex", analysis_year))

missing_tables      <- setNames(lapply(files_missing, arrow::read_parquet), stems)
missing_by_quintile <- arrow::read_parquet(file_quintile)

# ========================================================================================
# II: Process data
# ========================================================================================
dimension_tables <- list()
tex_missing      <- list()
for (dimension in missing_table_dimensions) {
  for (city in city_files) {
    name <- sprintf("%s_%s_missing_by_%s", city, missing_table_panel, dimension)
    dimension_tables[[name]] <- table_missing_by_dimension(
      missing_list = setNames(list(missing_tables[[name]]), dimension),
      dim = dimension, city_label = city)
    tex_missing[[name]] <- latex_missing_dimension(dt         = dimension_tables[[name]],
                                                  dim        = dimension,
                                                  city_label = city)
  }
}

tex_quintile <- latex_missing_by_quintile(dt = missing_by_quintile)

# ========================================================================================
# III: Save outputs
# ========================================================================================
for (i in seq_along(stems)) {
  write_latex_table(tex_missing[[stems[i]]], out_missing[i])
}
write_latex_table(tex_quintile, out_quintile)
