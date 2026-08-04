# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Render every LaTeX table the paper and appendix need, from the Parquet files the
#   process stage already computed.
#
# @Description: One script for the whole table layer, so a referee finds them all in one
# place and no statistic is computed twice. Every input comes from data/processed/ and
# every output is a .tex under results/tables/; nothing here calculates anything. Run
# scripts/process_data/compute_descriptive_tables.R and compute_exposure_regressions.R
# first.
#
# @Summary:
#   I.   Import data: locate the process-stage Parquet files.
#   II.  Render: station counts, WHO exceedances, missing shares, census summary.
#   III. Report where each .tex landed.
#
# @Date: August 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Define input and output folders
dir_counts  <- here::here("data", "processed", "station_counts")
dir_who     <- here::here("data", "processed", "who_exceedances")
dir_missing <- here::here("data", "processed", "missing_proportions")
dir_census  <- here::here("data", "processed", "census_summary")

outdir_tables <- here::here("results", "tables")

analysis_year <- 2023L

# Which hourly panel the by-dimension missing tables describe. "raw" is structural
# missingness — hours the network never reported. "clean" additionally folds in what
# detect_outliers.R removed, which mixes two different phenomena in one number.
panel <- "raw"

missing_cities <- c("bogota", "cdmx", "santiago", "sao_paulo_metro")
missing_dims   <- c("station", "month", "hour")

station_counts      <- arrow::read_parquet(
  file.path(dir_counts, paste0("stations_by_pollutant_", analysis_year, ".parquet")))
who_exceedances     <- arrow::read_parquet(
  file.path(dir_who, "who_exceedances_all_cities.parquet"))
missing_by_quintile <- arrow::read_parquet(
  file.path(dir_missing,
            paste0("missing_by_education_quintile_", analysis_year, ".parquet")))
census_summary      <- arrow::read_parquet(
  file.path(dir_census, "census_summary.parquet"))

# ============================================================================================
# II: Render tables
# ============================================================================================
# Number of monitoring stations reporting each pollutant, by city.
tex_counts <- file.path(outdir_tables, "station_counts",
                        paste0("stations_by_pollutant_", analysis_year, ".tex"))
write_station_count_latex(station_counts = station_counts, out_file = tex_counts)

# Annual PM concentrations against the WHO AQG 2021 interim and long-term targets.
tex_who <- file.path(outdir_tables, "who_exceedances", "who_exceedances_all_cities.tex")
dir.create(dirname(tex_who), recursive = TRUE, showWarnings = FALSE)
table_who_exceedances(
  exceedances_dt   = who_exceedances,
  save_latex_table = TRUE,
  out_file         = tex_who,
  caption          = paste("Annual PM concentrations vs. WHO AQG 2021",
                           "(interim and long-term targets)."),
  label            = "tab:who_exceedances",
  overwrite_tex    = TRUE)

# Missing-observation shares by station, month and hour, one table per city and dimension.
outdir_missing_tex <- file.path(outdir_tables, "missing_proportions")
dir.create(outdir_missing_tex, recursive = TRUE, showWarnings = FALSE)

for (city in missing_cities) {
  missing_list <- lapply(missing_dims, function(d) {
    arrow::read_parquet(
      file.path(dir_missing, sprintf("%s_%s_missing_by_%s.parquet", city, panel, d)))
  })
  names(missing_list) <- missing_dims

  for (d in missing_dims) {
    table_missing_by_dimension(
      missing_list     = missing_list,
      dim              = d,
      city_label       = city,
      save_latex_table = TRUE,
      out_file         = file.path(outdir_missing_tex,
                                   sprintf("%s_%s_missing_by_%s.tex", city, panel, d)),
      overwrite_tex    = TRUE)
  }
}

# Share of expected hours actually reported, by education quintile.
tex_quintile <- file.path(outdir_missing_tex,
                          paste0("missing_by_education_quintile_", analysis_year, ".tex"))
writeLines(latex_missing_by_quintile(missing_by_quintile), tex_quintile)

# Population and geographic-unit counts behind the exposure estimates.
tex_census <- file.path(outdir_tables, "census_summary", "census_summary_table.tex")
dir.create(dirname(tex_census), recursive = TRUE, showWarnings = FALSE)
writeLines(latex_census_summary(census_summary), tex_census)

# ============================================================================================
# III: Report
# ============================================================================================
message("Wrote: ", tex_counts)
message("Wrote: ", tex_who)
message("Wrote: ", length(missing_cities) * length(missing_dims),
        " by-dimension tables in ", outdir_missing_tex)
message("Wrote: ", tex_quintile)
message("Wrote: ", tex_census)

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
