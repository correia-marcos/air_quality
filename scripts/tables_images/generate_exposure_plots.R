# =====================================================================================
# IDB: Air monitoring
# =====================================================================================
# @Goal: Generate all exposure figures by socioeconomic group: regression gaps with
#   confidence intervals, and population-weighted concentration levels.
#
# @Description:
#   Reads the tidy artifacts written by compute_exposure_regressions.R (the CI
#   estimates and the raw group summaries, for education and for income) and draws
#   two figure families: (1) regression gaps vs the base group with 95% CIs, per
#   city/outcome/pollutant; (2) dual-axis PM10/PM2.5 mean concentration by group,
#   per city. Plotting consumes the already-computed tables, so figures are always
#   consistent with the regressions and summary tables. Income figures are produced
#   only for the cities whose artifacts contain income (CDMX and Sao Paulo).
#
# @Summary:
#   I.   Import data: read the CI and summary artifacts eagerly into memory.
#   II.  Process: draw CI plots and level plots over the available combinations.
#   III. Save: write one PDF per figure.
#
# @Date: June 2026
# @Author: Marcos
# =====================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# Register Tex Gyre Pagella and set the paper ggplot theme for this script.
set_paper_theme()

# =====================================================================================
# I: Import data
# =====================================================================================
# Define input and output folders
dir_reg    <- here::here("data", "processed", "idw_regressions")
outdir_ci  <- here::here("results", "figures", "exposure_by_group", "ci")
outdir_lvl <- here::here("results", "figures", "exposure_by_group", "levels")

# Reader for the artifacts of compute_exposure_regressions.R. Income files are optional,
# so a missing one returns NULL and its figures are skipped below.
read_artifact <- function(name, required) {
  path <- here::here(dir_reg, paste0(name, "_2023.parquet"))
  if (!file.exists(path)) {
    if (required) stop("Artifact not found: ", path)
    return(NULL)
  }
  data.table::as.data.table(arrow::read_parquet(path))
}

ci_education      <- read_artifact("exposure_ci_estimates_education", TRUE)
summary_education <- read_artifact("exposure_group_summaries_education", TRUE)
ci_income         <- read_artifact("exposure_ci_estimates_income", FALSE)
summary_income    <- read_artifact("exposure_group_summaries_income", FALSE)

# City display labels and file-safe names (city matches the regression artifact)
city_labels <- c(Bogota = "Bogot\u00e1", CDMX = "Mexico City",
                 Santiago = "Santiago", `Sao Paulo` = "S\u00e3o Paulo",
                 `Santiago (comuna, 2024)` = "Santiago (commune, 2024 census)")
city_files  <- c(Bogota = "bogota", CDMX = "mexico_city",
                 Santiago = "santiago", `Sao Paulo` = "sao_paulo",
                 `Santiago (comuna, 2024)` = "santiago_comuna_2024")

# Create output folders
dir.create(outdir_ci, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir_lvl, recursive = TRUE, showWarnings = FALSE)

# =====================================================================================
# II and III: Build and save figures
# =====================================================================================
# Helper: save a ggplot as a high-resolution PDF (skips quietly if plot is NULL).
save_pdf <- function(plot_obj, path) {
  if (is.null(plot_obj)) {
    return(invisible(NULL))
  }
  ggplot2::ggsave(filename = path, plot = plot_obj, device = cairo_pdf,
                  width = 6, height = 4.5, dpi = 300, bg = "white")
}

# Helper: x-axis label for a grouping. Keys on both socioeconomic variable and the
# grouping, since CDMX income runs on quintiles and SP income on deciles.
group_axis_label <- function(socio_var, group_type) {
  var_label <- if (identical(socio_var, "income")) "Income" else "Education"
  paste(var_label, group_type)
}

# Draw one CI figure per city x outcome. Exceedance-hour outcomes overlay PM2.5 and PM10.
plot_ci_set <- function(ci_dt, tag) {
  combos <- unique(ci_dt[!is.na(city) & !is.na(outcome),
                         .(city, outcome, group_type, socioeconomic_var)])

  for (j in seq_len(nrow(combos))) {
    city_j <- combos$city[j]
    out_j  <- combos$outcome[j]
    poll_j <- intersect(c("pm25", "pm10"),
                        ci_dt[city == city_j & outcome == out_j, unique(pollutant)])

    if (length(poll_j) == 0L) next

    p <- plot_group_ci(
      ci_table    = ci_dt[city == city_j],
      outcome     = out_j,
      pollutant   = poll_j,
      group_label = group_axis_label(combos$socioeconomic_var[j],
                                     combos$group_type[j]),
      city_label  = city_labels[[city_j]])

    poll_tag <- if (length(poll_j) > 1L) "pm25_pm10" else poll_j

    save_pdf(p, file.path(outdir_ci, sprintf("%s_%s_%s_%s_ci.pdf",
                                             city_files[[city_j]], tag, out_j, poll_tag)))
  }
}

# Draw one dual-axis PM10/PM2.5 level figure per city.
plot_level_set <- function(sum_dt, tag) {
  for (city_j in unique(sum_dt[!is.na(city), city])) {
    sub <- sum_dt[city == city_j]

    p <- plot_group_levels(
      summary_table = sub,
      group_label   = group_axis_label(sub$socioeconomic_var[1], sub$group_type[1]),
      city_label    = city_labels[[city_j]],
      year_label    = as.character(sub$year[1]))

    save_pdf(p, file.path(outdir_lvl, sprintf("%s_%s_levels.pdf",
                                              city_files[[city_j]], tag)))
  }
}

# Education always; income only when its artifacts exist
plot_ci_set(ci_education, "education")
plot_level_set(summary_education, "education")

if (!is.null(ci_income)) {
  plot_ci_set(ci_income, "income")
  plot_level_set(summary_income, "income")
}

cat("Saved exposure CI figures to:", outdir_ci, "\n")
cat("Saved exposure level figures to:", outdir_lvl, "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")
