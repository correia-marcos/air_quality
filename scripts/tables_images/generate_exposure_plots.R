# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Generate all exposure figures by socioeconomic group: regression gaps with
#   confidence intervals, and population-weighted concentration levels.
#
#' @Description: Reads the tidy artifacts written by estimate_exposure.R (the CI
#   estimates and the raw group summaries, for education and for income) and draws
#   two figure families: (1) regression gaps vs the base group with 95% CIs, per
#   city/outcome/pollutant; (2) dual-axis PM10/PM2.5 mean concentration by group,
#   per city. Plotting consumes the already-computed tables, so figures are always
#   consistent with the regressions and summary tables. estimate_exposure.R writes
#   one artifact set per buffer radius: 3 km is the paper's specification and 5 km
#   the robustness check. Both sets are read and stacked into one table per family;
#   the buffer_km column tells the rows apart and stamps every output file name.
#   Figures are built into named lists of plots first and only written in the last
#   step, so they can be inspected in RStudio before saving. Income figures are
#   produced only for the cities whose artifacts contain income (CDMX and Sao Paulo).
#
#' @Summary:
#   I.   Import data: read the CI and summary artifacts for both buffers and stack
#   them into one table per family.
#   II.  Build figures: draw every figure into a named list of plots; nothing is
#   written yet.
#   III. Save figures: write each list of plots to its output folder.
#
#' @Date: August 2026
#' @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_reg      <- here::here("data", "processed", "idw_regressions")
dir_reg_imp  <- here::here("data", "processed", "idw_regressions_imputed")
outdir_ci    <- here::here("results", "figures", "exposure_by_group", "ci")
outdir_lvl   <- here::here("results", "figures", "exposure_by_group", "levels")
outdir_paper <- here::here("results", "paper", "figures")

analysis_year <- 2023L

# Artifact paths written by estimate_exposure.R: one set per buffer radius (3 km is
# the paper's specification, 5 km the robustness check) and per analysis year.
ci_edu_3km_pq <- here::here(dir_reg, "exposure_ci_estimates_education_3km_2023.parquet")
ci_edu_5km_pq <- here::here(dir_reg, "exposure_ci_estimates_education_5km_2023.parquet")
ci_inc_3km_pq <- here::here(dir_reg, "exposure_ci_estimates_income_3km_2023.parquet")
ci_inc_5km_pq <- here::here(dir_reg, "exposure_ci_estimates_income_5km_2023.parquet")
summary_edu_3km_pq <- here::here(dir_reg,
                                 "exposure_group_summaries_education_3km_2023.parquet")
summary_edu_5km_pq <- here::here(dir_reg,
                                 "exposure_group_summaries_education_5km_2023.parquet")
summary_inc_3km_pq <- here::here(dir_reg,
                                 "exposure_group_summaries_income_3km_2023.parquet")
summary_inc_5km_pq <- here::here(dir_reg,
                                 "exposure_group_summaries_income_5km_2023.parquet")

# Stop early if the required education artifacts are missing
for (pq in c(ci_edu_3km_pq, ci_edu_5km_pq, summary_edu_3km_pq, summary_edu_5km_pq)) {
  if (!file.exists(pq)) stop("Required artifact not found: ", pq)
}

# Read education artifacts eagerly for RStudio inspection
ci_edu_3km      <- data.table::as.data.table(arrow::read_parquet(ci_edu_3km_pq))
ci_edu_5km      <- data.table::as.data.table(arrow::read_parquet(ci_edu_5km_pq))
summary_edu_3km <- data.table::as.data.table(arrow::read_parquet(summary_edu_3km_pq))
summary_edu_5km <- data.table::as.data.table(arrow::read_parquet(summary_edu_5km_pq))

# Stack both buffers into one table per family; buffer_km tells the rows apart
ci_education      <- rbind(ci_edu_3km, ci_edu_5km)
summary_education <- rbind(summary_edu_3km, summary_edu_5km)

# The regressions also cover the annual means, which only render_exposure_tables.R
# reports. Drop those rows so the CI figure family stays the two exceedance outcomes.
ci_education <- ci_education[outcome != "avg"]

# Income artifacts cover only CDMX and Sao Paulo, so they are optional
has_income <- all(file.exists(ci_inc_3km_pq), file.exists(ci_inc_5km_pq),
                  file.exists(summary_inc_3km_pq), file.exists(summary_inc_5km_pq))
if (has_income) {
  ci_inc_3km      <- data.table::as.data.table(arrow::read_parquet(ci_inc_3km_pq))
  ci_inc_5km      <- data.table::as.data.table(arrow::read_parquet(ci_inc_5km_pq))
  summary_inc_3km <- data.table::as.data.table(arrow::read_parquet(summary_inc_3km_pq))
  summary_inc_5km <- data.table::as.data.table(arrow::read_parquet(summary_inc_5km_pq))

  ci_income      <- rbind(ci_inc_3km, ci_inc_5km)[outcome != "avg"]
  summary_income <- rbind(summary_inc_3km, summary_inc_5km)
}

# City display labels and file-safe names (city matches the regression artifact)
city_labels <- c(Bogota = "Bogotá", CDMX = "Mexico City",
                 Santiago = "Santiago", `Sao Paulo` = "São Paulo",
                 `Santiago (comuna, 2024)` = "Santiago (commune, 2024 census)")
city_files  <- c(Bogota = "bogota", CDMX = "mexico_city",
                 Santiago = "santiago", `Sao Paulo` = "sao_paulo",
                 `Santiago (comuna, 2024)` = "santiago_comuna_2024")

# The manuscript's own spelling, used for the second copy of the 3 km figures.
paper_files <- c(bogota = "bogota", mexico_city = "mexico",
                 santiago = "santiago", sao_paulo = "saopaulo")

# ============================================================================================
# II: Build figures
# ============================================================================================
# Education figures, always produced. The name of each list element is the output
# file name of that figure.
plots_ci_edu  <- build_exposure_ci_figures(ci_education, "education",
                                           city_labels, city_files)
plots_lvl_edu <- build_exposure_level_figures(summary_education, "education",
                                              city_labels, city_files)

# Income figures, only for the cities whose census carries income
if (has_income) {
  plots_ci_inc  <- build_exposure_ci_figures(ci_income, "income",
                                             city_labels, city_files)
  plots_lvl_inc <- build_exposure_level_figures(summary_income, "income",
                                                city_labels, city_files)
}

# ============================================================================================
# III: Save figures
# ============================================================================================
# Create output folders
dir.create(outdir_ci, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_lvl, recursive = TRUE, showWarnings = FALSE)

# Education figures
save_exposure_figures(plots_ci_edu, outdir_ci)
save_exposure_figures(plots_lvl_edu, outdir_lvl)

# Income figures
if (has_income) {
  save_exposure_figures(plots_ci_inc, outdir_ci)
  save_exposure_figures(plots_lvl_inc, outdir_lvl)
}

cat("Saved exposure CI figures to:", outdir_ci, "\n")
cat("Saved exposure level figures to:", outdir_lvl, "\n")

# ============================================================================================
# IV: Save the manuscript's copy of the 3 km figures
# ============================================================================================
# Same plot objects, written a second time under the names the .tex cites; see
# paper_exposure_filename() for which runs the manuscript prints.
paper_plots <- c(plots_ci_edu, plots_lvl_edu)

if (has_income) {
  paper_plots <- c(paper_plots, plots_ci_inc, plots_lvl_inc)
}

n_paper <- 0L

for (fname in names(paper_plots)) {
  paper_path <- paper_exposure_filename(fname, paper_files, analysis_year)

  if (is.na(paper_path)) next

  out_file <- file.path(outdir_paper, paper_path)
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  save_plot_pdf(paper_plots[[fname]], out_file)
  n_paper <- n_paper + 1L
}

cat("Saved", n_paper, "manuscript-named figures under:", outdir_paper, "\n")

# ============================================================================================
# V: The imputed robustness specification
# ============================================================================================
# Same figures, estimated on the panels that combine observed and imputed readings. Only
# education at 3 km is produced, which is all the manuscript reports for this variant.
ci_imp_pq  <- here::here(dir_reg_imp, "exposure_ci_estimates_education_3km_2023.parquet")
sum_imp_pq <- here::here(dir_reg_imp,
                         "exposure_group_summaries_education_3km_2023.parquet")

ci_imputed      <- data.table::as.data.table(arrow::read_parquet(ci_imp_pq))[
  outcome != "avg"]
summary_imputed <- data.table::as.data.table(arrow::read_parquet(sum_imp_pq))

plots_ci_imp  <- build_exposure_ci_figures(ci_imputed, "education",
                                           city_labels, city_files)
plots_lvl_imp <- build_exposure_level_figures(summary_imputed, "education",
                                              city_labels, city_files)

imputed_plots <- c(plots_ci_imp, plots_lvl_imp)
n_imputed <- 0L

for (fname in names(imputed_plots)) {
  paper_path <- paper_exposure_filename(fname, paper_files, analysis_year,
                                        imputed = TRUE)

  if (is.na(paper_path)) next

  out_file <- file.path(outdir_paper, paper_path)
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  save_plot_pdf(imputed_plots[[fname]], out_file)
  n_imputed <- n_imputed + 1L
}

cat("Saved", n_imputed, "imputed-specification figures under:", outdir_paper, "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")
