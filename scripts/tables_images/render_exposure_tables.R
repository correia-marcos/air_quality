# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Render the appendix tables that report exposure by socioeconomic group.
#
#' @Description: Turns the group summaries and regression estimates written by
# estimate_exposure.R into the four .tex fragments the appendix prints: mean and median
# concentration by group, and average hours above IT1/IT2 by group, each for education
# and for income. Nothing is estimated here; the same artefacts already drive the exposure
# figures, so table and figure cannot disagree. Only the paper's 3 km specification is
# rendered. Fragments are bare tabulars, so the manuscript keeps its own float, caption
# and label, and each is written to results/paper/tables/ (what the paper prints) and to
# results/tables/exposure_by_groups/ (the repo's own copy).
#
#' @Summary:
#   I.   Import data: the 3 km education and income artefacts.
#   II.  Render: two table shapes, education then income.
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
dir_reg      <- here::here("data", "processed", "idw_regressions")
outdir_paper <- here::here("results", "paper", "tables")
outdir_repo  <- here::here("results", "tables", "exposure_by_groups")

analysis_year <- 2023L
buffer_km     <- 3L

summary_edu <- data.table::as.data.table(arrow::read_parquet(here::here(
  dir_reg, sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
                   buffer_km, analysis_year))))
summary_inc <- data.table::as.data.table(arrow::read_parquet(here::here(
  dir_reg, sprintf("exposure_group_summaries_income_%dkm_%d.parquet",
                   buffer_km, analysis_year))))
ci_edu <- data.table::as.data.table(arrow::read_parquet(here::here(
  dir_reg, sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
                   buffer_km, analysis_year))))
ci_inc <- data.table::as.data.table(arrow::read_parquet(here::here(
  dir_reg, sprintf("exposure_ci_estimates_income_%dkm_%d.parquet",
                   buffer_km, analysis_year))))

# The manuscript's panel order. Santiago's 2024 commune run is a robustness vintage and
# is deliberately absent from these tables.
cities_edu <- c("Bogota", "CDMX", "Santiago", "Sao Paulo")
cities_inc <- c("CDMX", "Sao Paulo")

# Panel headings. The income ones name their own grouping, because the two panels are not
# cut the same way: 63 municipalities cannot identify ten income coefficients in CDMX.
labels_edu <- c(Bogota = "Bogota", CDMX = "Mexico City",
                Santiago = "Santiago", `Sao Paulo` = "Sao Paulo")
labels_inc <- c(CDMX = "Mexico City (income quintiles)",
                `Sao Paulo` = "Sao Paulo (income deciles)")

dir.create(outdir_paper, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_repo, recursive = TRUE, showWarnings = FALSE)

# Write one fragment to both trees, so the paper copy is never the only copy.
write_both <- function(lines, stem) {
  for (d in c(outdir_paper, outdir_repo)) {
    writeLines(lines, file.path(d, paste0(stem, ".tex")), useBytes = TRUE)
  }
  message("Wrote: ", stem, ".tex")
}

# ============================================================================================
# II: Render tables
# ============================================================================================
# Mean and median concentration by group. Education is five quintiles everywhere; the
# income table spans ten columns because Sao Paulo is estimated in deciles while CDMX,
# with too few clusters for ten coefficients, is estimated in quintiles.
tab_means_edu <- latex_exposure_means_by_group(
  summary_dt = summary_edu, ci_dt = ci_edu,
  panel_cities = cities_edu, panel_labels = labels_edu, n_groups = 5L)

tab_means_inc <- latex_exposure_means_by_group(
  summary_dt = summary_inc, ci_dt = ci_inc,
  panel_cities = cities_inc, panel_labels = labels_inc, n_groups = 10L)

# Average hours above IT1 and IT2 by group; groups run down the rows.
tab_hours_edu <- latex_exposure_hours_by_group(summary_edu, cities_edu, labels_edu)
tab_hours_inc <- latex_exposure_hours_by_group(summary_inc, cities_inc, labels_inc)

write_both(tab_means_edu, "table_means_education_quintiles")
write_both(tab_means_inc, "table_means_income_groups")
write_both(tab_hours_edu, "table_hours_above_education_quintiles")
write_both(tab_hours_inc, "table_hours_above_income_groups")

# ============================================================================================
# III: Report
# ============================================================================================
message("Exposure tables written to: ", outdir_paper)
message("Repo copies written to: ", outdir_repo)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
