# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Attribute the variation of Design A gaps to each analytical choice.
#
#' @Description: Reads the specification panel (build_specification_panel.R). For each
# city and outcome, the Design A gaps form a complete grid over geographic support,
# education grouping, IDW buffer and IDW exponent. A saturated factorial ANOVA splits
# their variance exactly into main effects and interactions (a functional-ANOVA, or
# Sobol, decomposition with each chosen value weighted equally). Reports first-order and
# total shares per choice, for the full grid and for buffers of 3 km or more. Writes
# data/processed/resolution_sensitivity/specification_panel/variance_decomposition_*
# and results/tables/specification_sensitivity/panel/variance_decomposition_*.csv.
#
#' @Summary:
#   I.   Import data: functions, the panel and the choices decomposed.
#   II.  Process data: complete grids and their decompositions by city and outcome.
#   III. Save outputs: terms, then shares per choice.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions
source(here::here("src", "general_utilities", "config_utils_resolution.R"))

# Settings: the choices decomposed, and the two buffer ranges reported
choices <- c("level", "grouping", "buffer_km", "distance_power")
ranges  <- list(all_buffers = c(0, Inf), buffers_3km_or_more = c(3, Inf))

# Set the folders
dir_out    <- here::here("data", "processed", "resolution_sensitivity",
                         "specification_panel")
dir_tables <- here::here("results", "tables", "specification_sensitivity", "panel")

# Read the specification panel
panel <- as.data.table(arrow::read_parquet(file.path(dir_out,
                                                     "specification_panel.parquet")))

# ========================================================================================
# II: Process data
# ========================================================================================
# Keep complete grids: a buffer without both exponents, or where any gap of the city
# and outcome is undefined (an empty endpoint group), is dropped and recorded
grids <- list()
for (range in names(ranges)) for (city in unique(panel$city_id)) {
  for (oc in unique(panel$outcome)) {
    x <- panel[city_id == city & outcome == oc & buffer_km >= ranges[[range]][1] &
                 buffer_km <= ranges[[range]][2]]
    one_exponent <- x[, uniqueN(distance_power), by = buffer_km][
      V1 < uniqueN(x$distance_power), buffer_km]
    incomplete <- sort(unique(c(one_exponent, x[!is.finite(gap), buffer_km])))
    grids[[paste(range, city, oc)]] <- list(range = range, city_id = city, outcome = oc,
      data = x[!buffer_km %in% incomplete], dropped_buffers = incomplete)
  }
}

# Decompose each grid's gaps into main effects and interactions of the choices
decompositions <- lapply(grids, function(g) {
  specification_variance_decomposition(x = g$data, factors = choices, response = "gap")
})

# Collect the terms and the per-choice shares with their grid identifiers
variance_terms <- rbindlist(lapply(names(grids), function(n) {
  g <- grids[[n]]
  data.table(range = g$range, city_id = g$city_id, outcome = g$outcome,
             decompositions[[n]]$terms)
}))
variance_indices <- rbindlist(lapply(names(grids), function(n) {
  g <- grids[[n]]
  data.table(range = g$range, city_id = g$city_id, outcome = g$outcome,
             decompositions[[n]]$indices, grid_rows = nrow(g$data),
             dropped_buffers = paste(g$dropped_buffers, collapse = " "),
             residual = decompositions[[n]]$residual)
}))
print(dcast(variance_indices[range == "all_buffers"], city_id + outcome ~ factor,
            value.var = "first_order"))

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the terms, then the shares per choice
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
resolution_write(variance_terms, file.path(dir_out, "variance_decomposition_terms"))
fwrite(variance_terms, file.path(dir_tables, "variance_decomposition_terms.csv"))
resolution_write(variance_indices, file.path(dir_out, "variance_decomposition_indices"))
fwrite(variance_indices, file.path(dir_tables, "variance_decomposition_indices.csv"))
