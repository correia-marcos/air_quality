# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Render the "city × year × pollutant exceedance factor" LaTeX table.
#
# @Description: Reads the combined Parquet written by
# scripts/process_data/compute_who_exceedances.R and produces both a wide data.table
# (for sanity checks) and a booktabs .tex file under results/tables/who_exceedances/.
#
# @Summary:
#   I.   Load Parquet
#   II.  Call table_who_exceedances(save_latex_table = TRUE)
#   III. Done
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
pq   <- here::here("data", "processed", "who_exceedances",
                   "who_exceedances_all_cities.parquet")
out_tex <- here::here("results", "tables", "who_exceedances",
                      "who_exceedances_all_cities.tex")

if (!file.exists(pq))
  stop("Run scripts/process_data/compute_who_exceedances.R first (missing: ", pq, ")")

dt <- arrow::read_parquet(pq)

# ============================================================================================
# II and III: Render + save
# ============================================================================================
dir.create(dirname(out_tex), recursive = TRUE, showWarnings = FALSE)
table_who_exceedances(
  exceedances_dt    = dt,
  save_latex_table  = TRUE,
  out_file          = out_tex,
  caption           = "Annual PM concentrations vs. WHO AQG 2021 (interim and long-term targets).",
  label             = "tab:who_exceedances",
  overwrite_tex     = TRUE
)

cat("Script from the IDB project executed successfully in the Docker container!\n")
