# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Figures of the Design A buffer x exponent grid and its variance decomposition.
#
#' @Description: Reads the Design A grid tables (estimate_design_a_grid.R) and the
# variance shares (estimate_specification_variance_decomposition.R). Plots the
# lowest-minus-highest education gap against the IDW buffer by exponent and by support,
# the exponent difference, the samples and station weights behind each buffer, the
# fixed-sample decomposition of the change from 3 km, and the variance shares of each
# choice. Quintiles only; other groupings are in the tables. Writes PDFs to
# results/figures/diagnostics/specification_sensitivity/design_a/. Estimates nothing.
#
#' @Summary:
#   I.   Import data: functions, settings and the saved grid tables.
#   II.  Process data: plotting tables and named plot objects.
#   III. Save outputs: the figures in the same order.
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

# Settings: cities, pollutants and the labels of choices and outcomes
cities          <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
city_labels     <- c(bogota_2018 = "Bogota", santiago_2017 = "Santiago",
                     sao_paulo_2010 = "Sao Paulo")
pollutant_names <- c(pm10 = "PM10", pm25 = "PM2.5")
choice_labels   <- c(level = "Geographic support", grouping = "Education grouping",
                     buffer_km = "IDW buffer", distance_power = "IDW exponent")
outcome_labels  <- c(avg_pm10 = "PM10 mean", hrs_d_pm10_it1 = "PM10 h >= IT1",
                     hrs_d_pm10_it2 = "PM10 h >= IT2", avg_pm25 = "PM2.5 mean",
                     hrs_d_pm25_it1 = "PM2.5 h >= IT1", hrs_d_pm25_it2 = "PM2.5 h >= IT2")

# Set the folders
dir_inputs  <- here::here("data", "interim", "resolution_sensitivity")
dir_grid    <- here::here("data", "processed", "resolution_sensitivity", "design_a_grid")
dir_panel   <- here::here("data", "processed", "resolution_sensitivity",
                          "specification_panel")
dir_figures <- here::here("results", "figures", "diagnostics", "specification_sensitivity",
                          "design_a")

# Read the grid tables, the variance shares and the support ladders
read_grid <- function(name) as.data.table(arrow::read_parquet(file.path(dir_grid,
  paste0("design_a_grid_", name, ".parquet"))))
contrasts        <- read_grid("contrasts")
samples          <- read_grid("samples")
station_weights  <- read_grid("station_weights")
decomposition    <- read_grid("decomposition")
variance_indices <- as.data.table(arrow::read_parquet(file.path(dir_panel,
  "variance_decomposition_indices.parquet")))
ladders <- rbindlist(lapply(cities, function(city) {
  fread(file.path(dir_inputs, city, "levels.csv"))[, city_id := city]
}))

# Set the output files
pdf_finest        <- file.path(dir_figures, paste0("design_a_grid_finest_", names(
  pollutant_names), ".pdf"))
pdf_supports      <- file.path(dir_figures, paste0("design_a_grid_supports_", names(
  pollutant_names), ".pdf"))
pdf_power         <- file.path(dir_figures, paste0("design_a_grid_power_", names(
  pollutant_names), ".pdf"))
pdf_samples       <- file.path(dir_figures, "design_a_grid_samples.pdf")
pdf_decomposition <- file.path(dir_figures, paste0("design_a_grid_decomposition_", names(
  pollutant_names), ".pdf"))
pdf_variance      <- file.path(dir_figures, "variance_decomposition.pdf")
names(pdf_finest) <- names(pdf_supports) <- names(pdf_power) <- names(pollutant_names)
names(pdf_decomposition) <- names(pollutant_names)

# ========================================================================================
# II: Process data
# ========================================================================================
# 1. Quintile gaps with panel labels; supports numbered from the finest (1)
quintile_gaps <- design_a_panel_labels(contrasts[grouping == "edu_quintile"], city_labels)
quintile_gaps <- merge(quintile_gaps, ladders[, .(city_id, level, fine_to_coarse)],
                       by = c("city_id", "level"))
quintile_gaps[, `:=`(
  pollutant = fifelse(grepl("pm25", outcome), "pm25", "pm10"),
  exponent  = factor(paste("Exponent", distance_power)),
  support   = factor(fifelse(fine_to_coarse == 1, "1 (finest)",
                             as.character(fine_to_coarse))))]
support_caption <- paste("Supports, finest (1) to coarsest:",
  "Bogota fine unit, seccion, sector, localidad/municipio, municipio;",
  "Santiago zona, distrito, comuna; Sao Paulo area de ponderacao, municipio.",
  "\nDesign A: the people of each buffer are fixed across supports; samples grow with",
  "the buffer. Dashed line: 3 km, the paper's buffer.")

plots_finest <- lapply(names(pollutant_names), function(poll) {
  plot_design_a_gap_by_buffer(
    x         = quintile_gaps[pollutant == poll & fine_to_coarse == 1],
    series    = "exponent",
    title     = paste0(pollutant_names[[poll]],
                       ": Design A gap at the finest support, by IDW buffer and exponent"),
    caption   = paste("Finest support, education quintiles. Each buffer has its own",
                      "sample, which does not depend on the exponent."))
})
plots_supports <- lapply(names(pollutant_names), function(poll) {
  plot_design_a_gap_by_buffer(
    x       = quintile_gaps[pollutant == poll & distance_power == 1],
    series  = "support",
    title   = paste0(pollutant_names[[poll]], ": Design A gap by IDW buffer and support"),
    legend  = "Support",
    caption = support_caption)
})

# 2. Exponent effect: gap at exponent 2 minus exponent 1, same people
power_effect <- dcast(quintile_gaps[pollutant %in% names(pollutant_names)],
  city_id + outcome + panel + pollutant + support + buffer_km ~ distance_power,
  value.var = "gap")
power_effect[, gap := `2` - `1`]
power_effect <- power_effect[!is.na(gap)]  # buffers run at both exponents only
plots_power <- lapply(names(pollutant_names), function(poll) {
  plot_design_a_gap_by_buffer(
    x         = power_effect[pollutant == poll],
    series    = "support",
    title     = paste0(pollutant_names[[poll]],
                       ": change in the Design A gap from exponent 1 to exponent 2"),
    gap_label = "Exponent 2 minus exponent 1",
    legend    = "Support",
    caption   = paste0("Same people at each buffer. The exponent matters only where two ",
                       "or more stations are within the buffer; exponent 2 was run at ",
                       "1, 2, 3, 5, 10 and 20 km.\n", support_caption))
})

# 3. Samples and station weights behind each buffer (exponent 1)
sample_measures <- rbind(
  samples[distance_power == 1, .(city_id, pollutant, buffer_km,
    measure = "Share of city adults", value = sample_share_city)],
  samples[distance_power == 1, .(city_id, pollutant, buffer_km,
    measure = "Q1 share of sample", value = share_q1)],
  samples[distance_power == 1, .(city_id, pollutant, buffer_km,
    measure = "Q5 share of sample", value = share_q5)],
  station_weights[distance_power == 1, .(city_id, pollutant, buffer_km,
    measure = "Eligible stations", value = mean_eligible_stations)],
  station_weights[distance_power == 1, .(city_id, pollutant, buffer_km,
    measure = "Effective stations", value = mean_static_neff)])
sample_measures[, `:=`(city = factor(city_labels[city_id], levels = city_labels),
  measure = factor(measure, levels = unique(measure)),
  pollutant = factor(pollutant_names[pollutant], levels = pollutant_names))]
plot_samples <- plot_design_a_samples(
  x     = sample_measures,
  title = "Design A samples and station weights by IDW buffer")

# 4. Fixed-sample decomposition of the change from 3 km (finest support, exponent 1)
parts <- decomposition[grouping == "edu_quintile" & level == "fine" & distance_power == 1]
parts <- melt(parts[, .(city_id, outcome, buffer_km, `Total change` = total_change,
  `Exposure change, same people` = exposure_change,
  `Sample change` = sample_expansion - sample_contraction)],
  id.vars = c("city_id", "outcome", "buffer_km"), variable.name = "component")
parts <- design_a_panel_labels(parts, city_labels)
parts[, pollutant := fifelse(grepl("pm25", outcome), "pm25", "pm10")]
plots_decomposition <- lapply(names(pollutant_names), function(poll) {
  plot_design_a_decomposition(
    x     = parts[pollutant == poll],
    title = paste0(pollutant_names[[poll]],
                   ": change in the Design A gap from 3 km, exposure versus sample"))
})

# 5. Variance shares of each choice over the full grid
shares <- melt(variance_indices[range == "all_buffers",
                                .(city_id, outcome, factor, first_order, total)],
               id.vars = c("city_id", "outcome", "factor"),
               variable.name = "share_type", value.name = "share")
shares[, `:=`(city = factor(city_labels[city_id], levels = city_labels),
  outcome_label = factor(outcome_labels[outcome], levels = outcome_labels),
  choice = factor(choice_labels[factor], levels = choice_labels),
  share_type = fifelse(share_type == "first_order", "First-order", "Total"))]
plot_variance <- plot_variance_shares(
  x     = shares,
  title = "Share of the variance of the Design A gap explained by each choice")

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the figures
dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
for (i in seq_along(pollutant_names)) {
  ggsave(pdf_finest[[i]], plots_finest[[i]], width = 10, height = 8)
}
for (i in seq_along(pollutant_names)) {
  ggsave(pdf_supports[[i]], plots_supports[[i]], width = 10, height = 8)
}
for (i in seq_along(pollutant_names)) {
  ggsave(pdf_power[[i]], plots_power[[i]], width = 10, height = 8)
}
ggsave(pdf_samples, plot_samples, width = 10, height = 9)
for (i in seq_along(pollutant_names)) {
  ggsave(pdf_decomposition[[i]], plots_decomposition[[i]], width = 10, height = 8)
}
ggsave(pdf_variance, plot_variance, width = 9, height = 10)
