# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Render the census coverage table the paper reports for the four metro areas.
#
#' @Description: Turns two process-stage Parquet files into the .tex fragments the paper
# \input's. The census summary is written twice under the same content: once as
# census_summary/census_summary_table.tex, the repo's descriptive name, and once as
# table_census_coverage.tex, the name the manuscript uses. The distance-band descriptives
# become table_descriptives_a (Bogota and Mexico City) and table_descriptives_b (Santiago
# and Sao Paulo). Nothing is calculated here.
#
#' @Summary:
#   I.   Import data: locate the process-stage Parquet files.
#   II.  Render: the census coverage tabular, then the two descriptive panels.
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
dir_census    <- here::here("data", "processed", "census_summary")
dir_bands     <- here::here("data", "processed", "distance_band_descriptives")
outdir_tables <- here::here("results", "tables")
outdir_paper  <- here::here("results", "tables")

dir.create(outdir_paper, recursive = TRUE, showWarnings = FALSE)

census_summary <- arrow::read_parquet(file.path(dir_census, "census_summary.parquet"))
distance_bands <- arrow::read_parquet(
  file.path(dir_bands, "distance_band_descriptives.parquet"))

# ============================================================================================
# II: Render tables
# ============================================================================================
tex_lines <- latex_census_summary(census_summary)

# Repo-facing name, alongside the Parquet it was built from.
tex_census <- file.path(outdir_tables, "census_summary_table.tex")
dir.create(dirname(tex_census), recursive = TRUE, showWarnings = FALSE)
writeLines(tex_lines, tex_census)

# Manuscript-facing name: the paper \input's tables/table_census_coverage.
tex_paper <- tex_census  # Export mapping supplies the manuscript name.

# Descriptive statistics by distance band, two cities per table as the paper prints them.
tex_desc_a <- file.path(outdir_paper, "table_descriptives_a.tex")
writeLines(latex_distance_band_table(distance_bands,
                                     c("Bogota", "Mexico City")), tex_desc_a)

tex_desc_b <- file.path(outdir_paper, "table_descriptives_b.tex")
writeLines(latex_distance_band_table(distance_bands,
                                     c("Santiago", "Sao Paulo")), tex_desc_b)

# ============================================================================================
# III: Report
# ============================================================================================
message("Wrote: ", tex_census)
message("Wrote: ", tex_paper)
message("Wrote: ", tex_desc_a)
message("Wrote: ", tex_desc_b)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
