# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Render LaTeX tables of missing-observation shares by station / month / hour.
#
# @Description: Uses the per-dimension Parquets written by
# scripts/process_data/compute_missing_proportions.R. For each city, writes three
# tables (one per dimension) under results/tables/missing_proportions/.
#
# @Summary:
#   I.   Load the Parquet files into a list keyed by dimension
#   II.  Loop cities × dims and call table_missing_by_dimension()
#   III. Done
#
# @Date: April 2026
# @Author: Marcos
# ============================================================================================

source(here::here("src", "general_utilities", "config_utils_plot_tables.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
dir_missing <- here::here("data", "processed", "missing_proportions")
outdir_tex  <- here::here("results", "tables", "missing_proportions")
dir.create(outdir_tex, recursive = TRUE, showWarnings = FALSE)

cities <- c("bogota", "cdmx", "santiago", "sao_paulo")
dims   <- c("station", "month", "hour")

# ============================================================================================
# II and III: Render + save
# ============================================================================================
for (city in cities) {
  missing_list <- list()
  for (d in dims) {
    pq <- file.path(dir_missing, sprintf("%s_missing_by_%s.parquet", city, d))
    if (!file.exists(pq)) {
      message("[", city, "] Missing file: ", pq, " — skipping dim ", d)
      next
    }
    missing_list[[d]] <- arrow::read_parquet(pq)
  }
  if (length(missing_list) == 0L) next

  for (d in names(missing_list)) {
    out_file <- file.path(outdir_tex,
                          sprintf("%s_missing_by_%s.tex", city, d))
    table_missing_by_dimension(
      missing_list     = missing_list,
      dim              = d,
      city_label       = city,
      save_latex_table = TRUE,
      out_file         = out_file,
      overwrite_tex    = TRUE
    )
  }
}

cat("Script from the IDB project executed successfully in the Docker container!\n")
