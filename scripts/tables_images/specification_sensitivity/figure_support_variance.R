# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Table and figure of how far a coarser support moves the gap, by variance lost.
#
#' @Description: Reads the specification panel (build_specification_panel.R) and the
# paired cluster-bootstrap differences of the 3 km and 20 km robustness runs. Tabulates,
# for quintiles at exponent 1, each support's size, the share of fine-exposure variance it
# retains, its gap relative to the finest support and the available intervals. Summarises
# and plots, over every Design A specification, the change of the gap against the
# variance the support averages away. Writes results/tables/specification_sensitivity/
# review/support_variance_*.csv and results/figures/specification_sensitivity/review/
# support_variance.pdf. Estimates nothing.
#
#' @Summary:
#   I.   Import data: functions, settings and the saved panel and differences.
#   II.  Process data: the support table, its summary and the plot.
#   III. Save outputs: the tables, then the figure.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions, the grid figures and the paper theme
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("src", "general_utilities", "plot", "specification_sensitivity",
                  "design_a_grid.R"))
set_paper_theme(base_size = 10)

# Settings: city labels and the metric of each outcome
city_labels <- c(bogota_2018 = "Bogota", santiago_2017 = "Santiago",
                 sao_paulo_2010 = "Sao Paulo")
metrics     <- c("annual mean", "hours >= IT1", "hours >= IT2")

# Set the folders
dir_panel   <- here::here("data", "processed", "resolution_sensitivity",
                          "specification_panel")
dir_robust  <- here::here("results", "tables", "specification_sensitivity",
                          "robustness_designs")
dir_tables  <- here::here("results", "tables", "specification_sensitivity", "review")
dir_figures <- here::here("results", "figures", "specification_sensitivity", "review")

# Read the specification panel and the paired differences from the finest support
panel       <- as.data.table(arrow::read_parquet(file.path(dir_panel,
                                                           "specification_panel.parquet")))
differences <- rbind(
  fread(file.path(dir_robust, "resolution_multicity_differences.csv"))[, buffer_km := 3],
  fread(file.path(dir_robust, "resolution_multicity_20km_differences.csv"))[
    , buffer_km := 20])

# ========================================================================================
# II: Process data
# ========================================================================================
# 1. Support table: quintiles, exponent 1, 3 and 20 km; size, variance retained, gap
#    relative to the finest support, level HC1 intervals and paired bootstrap intervals
#    of the difference from the finest support where the clusters allow them
support_table <- panel[grouping == "edu_quintile" & distance_power == 1 &
                         buffer_km %in% c(3, 20),
  .(city_id, outcome, buffer_km, level, level_label, level_rank, n_clusters,
    pw_unit_area_km2, pw_unit_population, share_variance_retained, gap,
    gap_finest = gap_ref_fine, gap_relative = gap / gap_ref_fine,
    change_sd = delta_resolution / sd0, gap_lower = lower, gap_upper = upper,
    inference_status)]
support_table <- merge(support_table,
  differences[design == "A", .(city_id, level, outcome, buffer_km,
                               difference_lower = lower, difference_upper = upper,
                               difference_interval = interval_type)],
  by = c("city_id", "level", "outcome", "buffer_km"), all.x = TRUE)
setorder(support_table, city_id, outcome, buffer_km, level_rank)

# 2. Every coarser Design A row (all groupings, buffers and exponents): the variance a
#    support averages away and the change of the gap, in SD of fine exposure
support_points <- panel[level_rank > 1, .(city = factor(city_labels[city_id],
                                                        levels = city_labels),
  level, metric = factor(fcase(grepl("^avg", outcome), metrics[1],
                               grepl("it1$", outcome), metrics[2],
                               grepl("it2$", outcome), metrics[3]), levels = metrics),
  variance_lost = 1 - share_variance_retained, change_sd = delta_resolution / sd0)]

# Summarise by city and support: rows, variance lost, |change| and their rank correlation
support_summary <- support_points[, .(rows = .N,
  median_variance_lost = median(variance_lost), max_variance_lost = max(variance_lost),
  median_abs_change_sd = median(abs(change_sd)), max_abs_change_sd = max(abs(change_sd))),
  by = .(city, level)]
support_summary <- merge(support_summary, support_points[, .(
  spearman_city = cor(abs(change_sd), variance_lost, method = "spearman")), by = city],
  by = "city")

# 3. Plot the change of the gap against the variance lost
plot_support <- plot_support_variance(x = support_points,
  title   = "Design A: change of the gap at coarser supports against the variance lost",
  caption = paste("Every coarser support of the Design A grid: four groupings, all",
    "buffers, exponents 1 and 2. Change: gap at the support minus the finest gap, in",
    "\nstandard deviations of fine exposure. Variance lost: 1 minus the share of the",
    "fine-exposure variance that the support means retain."))

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the tables, then the figure
fwrite(support_table, file.path(dir_tables, "support_variance_table.csv"))
fwrite(support_summary, file.path(dir_tables, "support_variance_summary.csv"))
ggsave(file.path(dir_figures, "support_variance.pdf"), plot_support, width = 10,
       height = 5)
