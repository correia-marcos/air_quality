# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Render census coverage and distance-band descriptions.
#
#' @Description: Read the saved census and distance-band summaries for three LaTeX tables.
# The artifact manifest supplies the manuscript name for census_summary_table.tex.
# The two distance-band tables retain the existing city pairs and reported statistics.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read saved summaries.
#   II.  Process data: format the census table and the two distance-band panels.
#   III. Save outputs: write the three LaTeX tables.
#
#' @Date: September 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Source the shared formatting helpers and table functions
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "plot", "latex_tables.R"))

# Set input folders and the output folder
dir_census <- here::here("data", "processed", "census_summary")
dir_bands  <- here::here("data", "processed", "distance_band_descriptives")
dir_tables <- here::here("results", "tables")

# Set the census and distance-band input files and the LaTeX output paths
file_census <- here::here(dir_census, "census_summary.parquet")
file_bands  <- here::here(dir_bands, "distance_band_descriptives.parquet")
out_census  <- here::here(dir_tables, "census_summary_table.tex")
out_bands_a <- here::here(dir_tables, "table_descriptives_a.tex")
out_bands_b <- here::here(dir_tables, "table_descriptives_b.tex")

# Read the saved census and distance-band summaries
census_summary <- arrow::read_parquet(file_census)
distance_bands <- arrow::read_parquet(file_bands)

# ========================================================================================
# II: Process data
# ========================================================================================
# Format the tables as character vectors containing LaTeX lines
tex_census <- latex_census_summary(dt = census_summary)

tex_bands_a <- latex_distance_band_table(bands_dt     = distance_bands,
                                        panel_cities = c("Bogota", "Mexico City"))
tex_bands_b <- latex_distance_band_table(bands_dt     = distance_bands,
                                        panel_cities = c("Santiago", "Sao Paulo"))

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the census table, followed by the two distance-band tables
write_latex_table(tex_census, out_census)
write_latex_table(tex_bands_a, out_bands_a)
write_latex_table(tex_bands_b, out_bands_b)
