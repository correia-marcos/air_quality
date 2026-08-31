# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Render the LaTeX tables describing how much of the hourly record is observed.
#
#' @Description: Turns the missing-proportion Parquet files written by
# compute_descriptive_tables.R into .tex fragments under results/tables/. Nothing is
# calculated here. Two families: missing shares along one dimension at a time, one table
# per city and dimension, and the share of expected hours reported by education quintile.
#
#' @Summary:
#   I.   Import data: locate the process-stage Parquet files.
#   II.  Render: by-dimension missing shares, then availability by education quintile.
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
dir_missing        <- here::here("data", "processed", "missing_proportions")
outdir_missing_tex <- here::here("results", "tables", "missing_proportions")

analysis_year <- 2023L

# Which hourly panel the by-dimension tables describe. "raw" is structural missingness --
# hours the network never reported. "clean" additionally folds in what detect_outliers.R
# removed, which mixes two different phenomena in one number.
panel <- "raw"

missing_dims <- c("station", "month", "hour")

missing_by_quintile <- arrow::read_parquet(
  file.path(dir_missing,
            paste0("missing_by_education_quintile_", analysis_year, ".parquet")))

# ============================================================================================
# II: Render tables
# ============================================================================================
dir.create(outdir_missing_tex, recursive = TRUE, showWarnings = FALSE)

# One table per city and dimension. The dimension loop is a scalar knob over the three
# views of the same city panel; the cities themselves are written out.
for (dim in missing_dims) {
  render_missing_dimension_table(
    dir_missing = dir_missing,
    city_id     = "bogota",
    panel       = panel,
    dim         = dim,
    out_dir     = outdir_missing_tex)

  render_missing_dimension_table(
    dir_missing = dir_missing,
    city_id     = "cdmx",
    panel       = panel,
    dim         = dim,
    out_dir     = outdir_missing_tex)

  render_missing_dimension_table(
    dir_missing = dir_missing,
    city_id     = "santiago",
    panel       = panel,
    dim         = dim,
    out_dir     = outdir_missing_tex)

  render_missing_dimension_table(
    dir_missing = dir_missing,
    city_id     = "sao_paulo_metro",
    panel       = panel,
    dim         = dim,
    out_dir     = outdir_missing_tex)
}

# Share of expected hours actually reported, by education quintile.
tex_quintile <- file.path(outdir_missing_tex,
                          paste0("missing_by_education_quintile_", analysis_year, ".tex"))
writeLines(latex_missing_by_quintile(missing_by_quintile), tex_quintile)

# ============================================================================================
# III: Report
# ============================================================================================
message("Wrote: 4 cities x ", length(missing_dims), " by-dimension tables in ",
        outdir_missing_tex)
message("Wrote: ", tex_quintile)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
