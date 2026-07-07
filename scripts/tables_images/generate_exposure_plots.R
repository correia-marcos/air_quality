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

# =====================================================================================
# I: Import data
# =====================================================================================
# Define input and output folders
dir_reg     <- here::here("data", "processed", "idw_regressions")
outdir_ci   <- here::here("results", "figures", "exposure_by_group", "ci")
outdir_lvl  <- here::here("results", "figures", "exposure_by_group", "levels")

# Tidy artifact paths produced by compute_exposure_regressions.R
ci_edu_pq      <- here::here(dir_reg, "exposure_ci_estimates_education_2023.parquet")
ci_inc_pq      <- here::here(dir_reg, "exposure_ci_estimates_income_2023.parquet")
summary_edu_pq <- here::here(dir_reg,
                             "exposure_group_summaries_education_2023.parquet")
summary_inc_pq <- here::here(dir_reg,
                             "exposure_group_summaries_income_2023.parquet")

# Stop early if the required education artifacts are missing
if (!file.exists(ci_edu_pq)) {
  stop("CI estimates not found: ", ci_edu_pq)
}
if (!file.exists(summary_edu_pq)) {
  stop("Group summaries not found: ", summary_edu_pq)
}

# Read education artifacts eagerly for RStudio inspection
ci_education      <- data.table::as.data.table(arrow::read_parquet(ci_edu_pq))
summary_education <- data.table::as.data.table(arrow::read_parquet(summary_edu_pq))

# Read income artifacts if present (CDMX and Sao Paulo only)
ci_income      <- if (file.exists(ci_inc_pq)) {
  data.table::as.data.table(arrow::read_parquet(ci_inc_pq))
} else {
  NULL
}
summary_income <- if (file.exists(summary_inc_pq)) {
  data.table::as.data.table(arrow::read_parquet(summary_inc_pq))
} else {
  NULL
}

# City display labels and file-safe names (city matches the regression artifact)
city_labels <- c(Bogota = "Bogot\u00e1", CDMX = "Mexico City",
                 Santiago = "Santiago", `Sao Paulo` = "S\u00e3o Paulo")
city_files  <- c(Bogota = "bogota", CDMX = "mexico_city",
                 Santiago = "santiago", `Sao Paulo` = "sao_paulo")

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

# Helper: x-axis label for a grouping (education quintile vs income decile).
group_axis_label <- function(group_type) {
  if (identical(group_type, "decile")) "Income decile" else "Education quintile"
}

# Loop over the two groupings: education always, income only if present.
ci_sets <- list(
  education = list(ci = ci_education, summary = summary_education, tag = "education"),
  income    = list(ci = ci_income,    summary = summary_income,    tag = "income")
)

for (set in ci_sets) {
  # Skip income when its artifacts are absent.
  if (is.null(set$ci)) {
    next
  }
  
  # 1. Confidence-interval plots: one per city x outcome.
  #    Exceedance-hour figures overlay PM2.5 and PM10 in the same plot.
  # --------------------------------------------------------------------------------------
  ci_dt  <- set$ci
  combos <- unique(ci_dt[
    !is.na(city) & !is.na(outcome),
    .(city, outcome, group_type)
  ])
  
  for (j in seq_len(nrow(combos))) {
    city_j <- combos$city[j]
    out_j  <- combos$outcome[j]
    
    # For hours above WHO targets, plot both pollutants together.
    # For all other outcomes, use all pollutants available in the artifact.
    poll_j <- ci_dt[city == city_j & outcome == out_j, unique(pollutant)]
    poll_j <- intersect(c("pm25", "pm10"), poll_j)
    
    if (length(poll_j) == 0L) {
      next
    }
    
    p_ci <- plot_group_ci(
      ci_table    = ci_dt[city == city_j],
      outcome     = out_j,
      pollutant   = poll_j,
      group_label = group_axis_label(combos$group_type[j]),
      city_label  = city_labels[[city_j]]
    )
    
    poll_tag <- if (length(poll_j) > 1L) "pm25_pm10" else poll_j
    
    out_file <- file.path(outdir_ci, sprintf(
      "%s_%s_%s_%s_ci.pdf",
      city_files[[city_j]], set$tag, out_j, poll_tag
    ))
    
    save_pdf(p_ci, out_file)
  }
  
  # 2. Level plots: one dual-axis PM10/PM2.5 figure per city
  # --------------------------------------------------------------------------------------
  sum_dt     <- set$summary
  cities_lvl <- unique(sum_dt[!is.na(city), city])
  
  for (city_j in cities_lvl) {
    sub <- sum_dt[city == city_j]
    
    p_lvl <- plot_group_levels(
      summary_table = sub,
      group_label   = group_axis_label(sub$group_type[1]),
      city_label    = city_labels[[city_j]],
      year_label    = as.character(sub$year[1])
    )
    
    out_file <- file.path(outdir_lvl, sprintf(
      "%s_%s_levels.pdf", city_files[[city_j]], set$tag
    ))
    
    save_pdf(p_lvl, out_file)
  }
}

cat("Saved exposure CI figures to:", outdir_ci, "\n")
cat("Saved exposure level figures to:", outdir_lvl, "\n")
cat("Script from the IDB project executed successfully in the Docker container!\n")
