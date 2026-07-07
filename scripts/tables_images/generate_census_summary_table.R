# ======================================================================================
# IDB: Air monitoring
# ======================================================================================
# @Goal: Generate census summary table for the paper/slides.
#
# @Description:
#   This script reads the collapsed census datasets used in the IDW exposure
#   pipeline and produces a LaTeX table with total population, census year,
#   geographic level, number of geographic units, and average population per
#   geographic unit.
#
# @Summary:
#   I.   Import data: Define census paths and city metadata.
#   II.  Process: Compute population totals and geographic-unit counts.
#   III. Save: Export CSV, Parquet, and LaTeX table files.
#
# @Assumptions:
#   - Population is the sum of the collapsed census weight column.
#   - Number of census geographic units is the count of unique geographic IDs.
#   - Bogota's updated geographic level is census tract.
#
# @Date: June 2026
# @Author: Marcos Paulo
# ======================================================================================

# Get all libraries and functions used across the processing pipeline.
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# ======================================================================================
# I: Import data
# ======================================================================================
# Define input and output folders.
dir_census <- here::here("data", "interim", "census")
outdir_tab <- here::here("results", "tables", "census_summary")

# Create output folder if needed.
dir.create(outdir_tab, recursive = TRUE, showWarnings = FALSE)

# Define collapsed census paths. These are the same files used by IDW exposure.
geo_bogota_csv <- here::here(
  dir_census,
  "bogota_2018",
  "census_2018_metro_collapsed.csv"
)

geo_cdmx_csv <- here::here(
  dir_census,
  "cdmx_extended_2020",
  "collapse_metro_area_2020.csv"
)

geo_santiago_csv <- here::here(
  dir_census,
  "santiago_2024",
  "census_santiago_collapsed_2024.csv"
)

geo_sp_csv <- here::here(
  dir_census,
  "sao_paulo_2010",
  "census_sp_collapsed_2010.csv"
)

# City-level metadata. Keep one row per city to make assumptions visible.
city_specs <- data.table::data.table(
  city = c("Bogota", "Mexico City", "Gran Santiago", "Sao Paulo"),
  city_latex = c("Bogot\\'a", "Mexico City", "Gran Santiago", "S\\~ao Paulo"),
  census_year = c(2018L, 2020L, 2024L, 2010L),
  census_level = c(
    "Census tract",
    "Municipality",
    "Census tract",
    "Weighting area"
  ),
  census_path = c(
    geo_bogota_csv,
    geo_cdmx_csv,
    geo_santiago_csv,
    geo_sp_csv
  ),
  geo_id_col = c("GEO_ID", "CVE_MUN", "CUT", "code_weighting"),
  pop_col = c("weight", "weight", "weight", "weight")
)

# ======================================================================================
# II: Process data
# ======================================================================================
# Helper to format large integers for LaTeX tables.
.format_int_latex <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Helper to escape characters that can break LaTeX in ordinary text cells.
.escape_latex <- function(x) {
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

# Compute summary statistics for one city.
.compute_city_census_summary <- function(spec_row) {
  if (!file.exists(spec_row$census_path)) {
    stop("Census file not found: ", spec_row$census_path)
  }

  dt <- data.table::fread(spec_row$census_path)

  needed_cols <- c(spec_row$geo_id_col, spec_row$pop_col)
  missing_cols <- setdiff(needed_cols, names(dt))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing column(s) in ", spec_row$city, ": ",
      paste(missing_cols, collapse = ", ")
    )
  }

  dt <- dt[
    !is.na(get(spec_row$geo_id_col)) &
      !is.na(get(spec_row$pop_col)) &
      get(spec_row$pop_col) > 0
  ]

  total_population <- sum(dt[[spec_row$pop_col]], na.rm = TRUE)
  n_geo_units <- data.table::uniqueN(dt[[spec_row$geo_id_col]])

  data.table::data.table(
    city = spec_row$city,
    city_latex = spec_row$city_latex,
    year = spec_row$census_year,
    total_population = total_population,
    census_geographic_level = spec_row$census_level,
    n_census_geographic_units = n_geo_units,
    average_population_per_unit = total_population / n_geo_units
  )
}

# Apply city-level summary function and preserve the table order in city_specs.
summary_dt <- data.table::rbindlist(
  lapply(seq_len(nrow(city_specs)), function(i) {
    .compute_city_census_summary(city_specs[i])
  })
)

# Build display columns used in the LaTeX table.
table_dt <- data.table::copy(summary_dt)

table_dt[, total_population_fmt := .format_int_latex(total_population)]
table_dt[, n_units_fmt := .format_int_latex(n_census_geographic_units)]
table_dt[, avg_pop_fmt := .format_int_latex(average_population_per_unit)]

table_dt[, census_level_latex := .escape_latex(census_geographic_level)]

# ======================================================================================
# III: Save data and LaTeX table
# ======================================================================================
# Save machine-readable outputs for checking and replication.
csv_out <- file.path(outdir_tab, "census_summary_table.csv")
pq_out <- file.path(outdir_tab, "census_summary_table.parquet")
tex_out <- file.path(outdir_tab, "census_summary_table.tex")

data.table::fwrite(summary_dt, csv_out)
arrow::write_parquet(summary_dt, pq_out)

# Build LaTeX rows. The table body is intentionally plain for easy manual edits.
latex_rows <- vapply(seq_len(nrow(table_dt)), function(i) {
  paste0(
    table_dt$city_latex[i], " & ",
    table_dt$year[i], " & ",
    table_dt$total_population_fmt[i], " & ",
    table_dt$census_level_latex[i], " & ",
    table_dt$n_units_fmt[i], " & ",
    table_dt$avg_pop_fmt[i], " \\\\"
  )
}, character(1))

latex_table <- c(
  "\\begin{tabular}{lccccc}",
  "    \\toprule",
  "    \\toprule",
  paste0(
    "    \\multicolumn{1}{c}{\\textbf{City}} & ",
    "\\multicolumn{1}{c}{\\textbf{Year}} & ",
    "\\multicolumn{1}{c}{\\textbf{Total}} & ",
    "\\multicolumn{1}{c}{\\textbf{Census}} & ",
    "\\multicolumn{1}{c}{\\textbf{Number of census}} & ",
    "\\multicolumn{1}{c}{\\textbf{Average population per}} \\\\"
  ),
  paste0(
    "    & & ",
    "\\multicolumn{1}{c}{\\textbf{population}} & ",
    "\\multicolumn{1}{c}{\\textbf{geographic level}} & ",
    "\\multicolumn{1}{c}{\\textbf{geographic units}} & ",
    "\\multicolumn{1}{c}{\\textbf{census geographic unit}} \\\\"
  ),
  "    \\midrule",
  paste0("    ", latex_rows),
  "    \\bottomrule",
  "    \\bottomrule",
  "\\end{tabular}"
)

writeLines(latex_table, tex_out)

message("[census summary] Wrote: ", csv_out)
message("[census summary] Wrote: ", pq_out)
message("[census summary] Wrote: ", tex_out)

cat("Census summary table generated successfully.\n")
