# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Tables and figures for the coauthor review of the resolution analysis.
#
#' @Description: Reads the frozen resolution inputs, the saved 3 km and 20 km multicity
# products, and the Mexico City distance matrix. Builds the IT1/IT2 reading table, the
# Bogota urban/rural class breakdown, Santiago's quintile composition by sample and by
# comuna, monitoring diagnostics by support (kept apart from exposure estimation), and
# the 3 km versus 20 km Design B comparison for every education grouping, and the
# headline gaps of the four education definitions side by side. Writes
# results/tables/resolution_review_*.csv and
# results/figures/diagnostics/resolution_review_*.pdf. It estimates nothing new.
#
#' @Summary:
#   I.   Import data: functions, settings, paths and the saved inputs.
#   II.  Process data: review tables and figures as named objects.
#   III. Save outputs: tables, then figures, in the same order.
#
#' @Date: September 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the resolution functions, the review summaries and their figures.
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("src", "general_utilities", "plot", "resolution_review.R"))
set_paper_theme(base_size = 10)

# Settings: the three ladder cities, the station-count radii and the pollutants.
cities      <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
city_labels <- c(bogota_2018 = "Bogota", santiago_2017 = "Santiago",
                 sao_paulo_2010 = "Sao Paulo", cdmx_2020 = "Mexico City")
radii_km    <- c(1, 3, 5, 10, 20)
pollutants  <- c("pm10", "pm25")

# Support labels ordered coarse to fine; each city uses a consistent subset.
support_order <- c("Municipio", "Localidad/municipio", "Comuna", "Sector",
                   "Distrito censal", "Seccion", "Fine census unit", "Zona censal",
                   "Area de ponderacao")

# Set the folders
dir_inputs    <- here::here("data", "interim", "resolution_sensitivity")
dir_processed <- here::here("data", "processed", "resolution_sensitivity")
dir_tables    <- here::here("results", "tables")
dir_figures   <- here::here("results", "figures", "diagnostics")

# Read the saved multicity products for both buffers
contrasts_3km    <- fread(file.path(dir_tables, "resolution_multicity_contrasts.csv"))
contrasts_20km   <- fread(file.path(dir_tables, "resolution_multicity_20km_contrasts.csv"))
profiles_3km     <- fread(file.path(dir_tables, "resolution_multicity_profiles.csv"))
profiles_20km    <- fread(file.path(dir_tables, "resolution_multicity_20km_profiles.csv"))
composition_3km  <- fread(file.path(dir_tables, "resolution_multicity_composition.csv"))
composition_20km <- fread(file.path(dir_tables,
                                    "resolution_multicity_20km_composition.csv"))
matrices_3km     <- fread(file.path(dir_tables, "resolution_multicity_matrices.csv"))
matrices_20km    <- fread(file.path(dir_tables, "resolution_multicity_20km_matrices.csv"))

# Alternative education groupings: table name, headline contrast and title words
alt_groupings <- c(quintile_split   = "Q1 - Q5 (split ties)",
                   education_level  = "None - Graduate",
                   education_group3 = "Below secondary - Bachelor's+")
alt_titles    <- c(quintile_split   = "quintiles with split ties",
                   education_level  = "six education levels",
                   education_group3 = "three education groups")
alt_names     <- setNames(names(alt_groupings), names(alt_groupings))

# Read their saved contrasts and profiles for both buffers
read_alt <- function(g, what) fread(file.path(dir_tables,
  paste0("resolution_multicity_", g, "_", what, ".csv")))
alt_contrasts_3km  <- lapply(alt_names, read_alt, what = "contrasts")
alt_contrasts_20km <- lapply(alt_names, read_alt, what = "20km_contrasts")
alt_profiles_3km   <- lapply(alt_names, read_alt, what = "profiles")
alt_profiles_20km  <- lapply(alt_names, read_alt, what = "20km_profiles")

# Read the frozen per-city inputs: crosswalks, adults, quintile cells and support labels
keys       <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "crosswalk.parquet"))))
population <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "adult_population.parquet"))))
cells      <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "individual_quintile_cells.parquet"))))
levels_dt  <- rbindlist(lapply(cities, function(city) {
  fread(file.path(dir_inputs, city, "levels.csv"))[, city_id := city]
}))

# Read each support's full distance matrix and its 3 km observed hours
distances <- lapply(setNames(cities, cities), function(city) {
  lvs <- levels_dt[city_id == city, level]
  lapply(setNames(lvs, lvs), function(lv) as.data.table(arrow::read_parquet(
    file.path(dir_processed, city, "B", lv, "matrix_geo_station_distances.parquet"))))
})
observed_hours <- lapply(setNames(cities, cities), function(city) {
  lvs <- levels_dt[city_id == city, level]
  lapply(setNames(lvs, lvs), function(lv) lapply(setNames(pollutants, pollutants),
    function(poll) as.data.table(arrow::read_parquet(file.path(dir_processed, city, "B",
      lv, paste0(poll, "_matrix_units.parquet")), col_select = c("geo_id",
                                                                 "observed_hours")))))
})

# Read the frozen 2023 station panels named in each input manifest
panels <- lapply(setNames(cities, cities), function(city) {
  manifest <- fread(file.path(dir_inputs, city, "input_manifest.csv"))
  as.data.table(arrow::read_parquet(here::here(manifest[input == "pollution", path])))
})

# Santiago: census education, comuna names and the 3 km comuna assignments
census_santiago    <- as.data.table(arrow::read_parquet(
  file.path(dir_inputs, "santiago_2017", "census_education.parquet")))
zonas_santiago     <- sf::st_drop_geometry(sf::st_read(here::here("data", "interim",
  "geospatial_data", "santiago", "gran_santiago_zonas_2017.gpkg"), quiet = TRUE))
assignment_comunas <- as.data.table(arrow::read_parquet(file.path(dir_processed,
  "santiago_2017", "assignments", "comuna_avg_pm10.parquet")))

# Individual adults behind the frozen quintiles, for the schooling-tie table
groups <- lapply(setNames(cities, cities), function(city) {
  manifest <- fread(file.path(dir_inputs, city, "input_manifest.csv"))
  as.data.table(arrow::read_parquet(here::here(manifest[input == "groups", path]),
    col_select = c("geo_id", "educ_years", "person_weight", "adult", "edu_quintile")))
})

# Mexico City: one support (municipality), read for the monitoring and tie tables
distances_cdmx <- as.data.table(arrow::read_parquet(here::here("data", "processed",
  "distances_matrices", "cdmx_2020", "matrix_geo_station_distances.parquet")))
groups_cdmx    <- as.data.table(arrow::read_parquet(here::here("data", "processed",
  "idw_estimates", "cdmx_2020", "cdmx_2020_indiv_groups.parquet")))
panel_cdmx     <- as.data.table(arrow::read_parquet(here::here("data", "processed",
  "monitoring_stations_outliers", "cdmx_metro_clean", "year=2023", "data.parquet")))

# Set the output files
csv_it_reading      <- file.path(dir_tables, "resolution_review_it_reading.csv")
csv_bogota_classes  <- file.path(dir_tables, "resolution_review_bogota_classes.csv")
csv_quintile_ties   <- file.path(dir_tables, "resolution_review_quintile_ties.csv")
csv_santiago_shares <- file.path(dir_tables, "resolution_review_santiago_samples.csv")
csv_santiago_units  <- file.path(dir_tables, "resolution_review_santiago_comunas.csv")
csv_monitoring      <- file.path(dir_tables, "resolution_review_monitoring.csv")
csv_buffer_gaps     <- file.path(dir_tables, "resolution_review_buffer_gaps.csv")
csv_buffer_coverage <- file.path(dir_tables, "resolution_review_buffer_coverage.csv")
csv_buffer_parts    <- file.path(dir_tables, "resolution_review_buffer_decomposition.csv")
csv_alt_gaps        <- file.path(dir_tables,
  paste0("resolution_review_", alt_names, "_buffer_gaps.csv"))
names(csv_alt_gaps) <- alt_names
csv_definitions     <- file.path(dir_tables, "resolution_review_definition_comparison.csv")
pdf_santiago_units  <- file.path(dir_figures, "resolution_review_santiago_comunas.pdf")
pdf_santiago_shares <- file.path(dir_figures, "resolution_review_santiago_samples.pdf")
pdf_monitoring      <- file.path(dir_figures, "resolution_review_monitoring.pdf")
pdf_buffer_pm10     <- file.path(dir_figures, "resolution_review_buffer_pm10.pdf")
pdf_buffer_pm25     <- file.path(dir_figures, "resolution_review_buffer_pm25.pdf")
pdf_alt_buffers     <- character()
for (g in alt_names) for (poll in pollutants) {
  pdf_alt_buffers[paste(g, poll, sep = "_")] <- file.path(dir_figures,
    paste0("resolution_review_", g, "_buffer_", poll, ".pdf"))
}
pdf_definitions     <- character()
for (b in c("3km", "20km")) for (poll in pollutants) {
  pdf_definitions[paste(b, poll, sep = "_")] <- file.path(dir_figures,
    paste0("resolution_review_definitions_", b, "_", poll, ".pdf"))
}

# ========================================================================================
# II: Process data
# ========================================================================================
# 1. IT reading: Q1 and Q5 means behind each fine-support gap and normalized gap.
it_reading <- contrasts_3km[design == "A" & level == "fine",
  .(city_id, outcome, q1, q5, gap, normalized_pct, q1_over_q5 = q1 / q5,
    population, n_clusters)]

# 2. Bogota: which DANE classes the "seccion" and "sector" prefixes actually aggregate.
bogota_classes <- resolution_bogota_class_breakdown(
  keys       = keys$bogota_2018,
  population = population$bogota_2018)

# 3. Quintile cuts inside tied schooling values, split by geo_id order (four cities).
groups$cdmx_2020 <- groups_cdmx
quintile_ties <- rbindlist(lapply(names(groups), function(city) {
  resolution_tie_splits(groups[[city]])[, city_id := city]
}))

# 4. Santiago: individual-quintile shares of every estimation sample, both buffers.
santiago_shares <- rbind(
  resolution_sample_shares(profiles_3km[city_id == "santiago_2017"],
                           cells$santiago_2017, "avg_pm10")[, buffer := "3 km"],
  resolution_sample_shares(profiles_20km[city_id == "santiago_2017"],
                           cells$santiago_2017, "avg_pm10")[, buffer := "20 km"])
# Keep one row per distinct sample; A and the zona samples are the same people.
samples_kept <- data.table(
  design = c("All adults", "A", "B_native", "B_native", "B_common", "C", "C", "C"),
  level  = c("all", "fine", "distrito", "comuna", "comuna", "fine", "distrito", "comuna"))
santiago_shares <- santiago_shares[samples_kept, on = c("design", "level")]
santiago_shares[, buffer := factor(buffer, levels = c("3 km", "20 km"))]
santiago_shares[, sample_label := fcase(
  design == "All adults", "All education-reporting adults",
  design == "A", "Zona sample (= Design A at every support)",
  design == "B_native", paste("Native B:", level),
  design == "B_common", paste("Common B:", level),
  design == "C", paste("C area quintiles:", sub("fine", "zona", level)))]
santiago_shares <- santiago_shares[order(buffer)]

# 5. Santiago: quintile composition of each comuna and which designs reach it.
comuna_names <- unique(as.data.table(zonas_santiago)[, .(parent_id = as.character(CUT),
  unit_label = tools::toTitleCase(tolower(d_COMUNA)))])
santiago_comunas <- resolution_unit_composition(
  cells        = cells$santiago_2017,
  keys         = keys$santiago_2017,
  level        = "comuna",
  census       = census_santiago,
  a_fine_ids   = assignment_comunas[!is.na(A), geo_id],
  b_parent_ids = assignment_comunas[!is.na(B), unique(parent_B)])
santiago_comunas <- merge(santiago_comunas, comuna_names, by = "parent_id")

# 6. Monitoring by support: nearest active station and counts within each radius.
monitoring <- rbindlist(lapply(cities, function(city) {
  rbindlist(lapply(levels_dt[city_id == city, level], function(lv) {
    rbindlist(lapply(pollutants, function(poll) {
      active <- panels[[city]][is.finite(get(poll)), unique(normalize_station(station))]
      d <- copy(distances[[city]][[lv]])[, station_id := normalize_station(station_id)]
      units <- resolution_monitoring_units(d, active, radii_km)
      units <- merge(units, observed_hours[[city]][[lv]][[poll]], by = "geo_id",
                     all.x = TRUE)
      weights <- resolution_level_weights(population[[city]], cells[[city]],
                                          keys[[city]], lv)
      resolution_monitoring_summary(units, weights, radii_km)[
        , `:=`(city_id = city, level = lv, pollutant = poll)]
    }))
  }))
}))

# Mexico City enters at its only support; adults are aged 25 or older.
population_cdmx <- groups_cdmx[adult == 1, .(pop = sum(person_weight)), by = geo_id]
cells_cdmx <- groups_cdmx[adult == 1 & !is.na(edu_quintile),
  .(person_weight = sum(person_weight)), by = .(geo_id, edu_quintile)]
keys_cdmx <- data.table(geo_id = population_cdmx$geo_id, level = "municipio",
                        parent_id = population_cdmx$geo_id)
monitoring_cdmx <- rbindlist(lapply(pollutants, function(poll) {
  active <- panel_cdmx[is.finite(get(poll)), unique(normalize_station(station))]
  d <- copy(distances_cdmx)[, station_id := normalize_station(station_id)]
  units <- resolution_monitoring_units(d, active, radii_km)
  weights <- resolution_level_weights(population_cdmx, cells_cdmx, keys_cdmx,
                                      "municipio")
  resolution_monitoring_summary(units, weights, radii_km)[
    , `:=`(city_id = "cdmx_2020", level = "municipio", pollutant = poll)]
}))
monitoring <- rbind(monitoring, monitoring_cdmx)
monitoring <- merge(monitoring, rbind(levels_dt[, .(city_id, level, label)],
  data.table(city_id = "cdmx_2020", level = "municipio", label = "Municipio")),
  by = c("city_id", "level"))

# 7. Buffer robustness: gaps, coverage and the native-change decomposition.
buffer_cities <- intersect(unique(contrasts_20km$city_id), cities)
buffer_gaps <- resolution_buffer_comparison(contrasts_3km[city_id %in% buffer_cities],
                                            contrasts_20km)
coverage_cols <- c("city_id", "level", "pollutant", "contributing_stations",
                   "covered_units", "covered_adults", "adult_coverage_share",
                   "mean_eligible_stations", "median_nearest_active_km",
                   "mean_hourly_neff")
buffer_coverage <- merge(matrices_3km[city_id %in% buffer_cities, ..coverage_cols],
  matrices_20km[, ..coverage_cols], by = c("city_id", "level", "pollutant"),
  suffixes = c("_3km", "_20km"))
part_cols <- c("city_id", "level", "outcome", "native_change", "native_selection",
               "procedure_common", "baseline_selection", "population_native",
               "population_common")
buffer_parts <- merge(composition_3km[city_id %in% buffer_cities, ..part_cols],
  composition_20km[, ..part_cols], by = c("city_id", "level", "outcome"),
  suffixes = c("_3km", "_20km"))

# The same gap comparison for each alternative grouping; coverage does not depend on it.
alt_gaps <- lapply(alt_names, function(g) {
  resolution_buffer_comparison(alt_contrasts_3km[[g]], alt_contrasts_20km[[g]],
                               means = c("low_mean", "high_mean"))
})

# 8. Headline gaps of the four education definitions, with their endpoint shares.
definitions <- c(quintile         = "Quintiles, ties by geo order",
                 quintile_split   = "Quintiles, split ties",
                 education_level  = "Six levels: none - graduate",
                 education_group3 = "Three groups: below secondary - bachelor's+")
group_cols  <- c(quintile = "edu_quintile", quintile_split = "edu_quintile_split",
                 education_level = "edu_level", education_group3 = "edu_group3")
contrasts_by_buffer <- list(
  `3 km`  = c(list(quintile = contrasts_3km), alt_contrasts_3km),
  `20 km` = c(list(quintile = contrasts_20km), alt_contrasts_20km))
profiles_by_buffer  <- list(
  `3 km`  = c(list(quintile = profiles_3km), alt_profiles_3km),
  `20 km` = c(list(quintile = profiles_20km), alt_profiles_20km))
definition_comparison <- rbindlist(lapply(names(contrasts_by_buffer), function(b) {
  rbindlist(lapply(names(definitions), function(d) {
    resolution_headline_summary(contrasts_by_buffer[[b]][[d]], profiles_by_buffer[[b]][[d]],
                                group_cols[[d]])[, `:=`(buffer = b,
                                                        definition = definitions[[d]])]
  }))
}))
definition_comparison[, definition := factor(definition, levels = definitions)]

# 9. Figures: Santiago composition, monitoring, buffers and education definitions.
plot_santiago_units <- plot_resolution_unit_composition(
  composition = santiago_comunas,
  title       = "Santiago 2017: individual education quintiles within each comuna")

plot_santiago_shares <- plot_resolution_sample_shares(
  shares = santiago_shares,
  title  = "Santiago 2017: education composition of each estimation sample")

monitoring_plot_data <- monitoring[pollutant == "pm10"]
monitoring_plot_data[, `:=`(city_label = city_labels[city_id],
  level_label = factor(label, levels = rev(support_order)))]
plot_monitoring <- plot_resolution_monitoring(
  summary = monitoring_plot_data,
  title   = "Distance to the nearest active PM10 station, by support and quintile")

buffer_plot_data <- resolution_buffer_plot_data(
  base          = contrasts_3km,
  alternative   = contrasts_20km,
  levels        = levels_dt,
  city_labels   = city_labels,
  support_order = support_order)
plot_buffer_pm10 <- plot_resolution_buffer_gaps(
  comparison = buffer_plot_data[pollutant == "pm10"],
  title      = "PM10: native Design B gap at 3 km and 20 km")
plot_buffer_pm25 <- plot_resolution_buffer_gaps(
  comparison = buffer_plot_data[pollutant == "pm25"],
  title      = "PM2.5: native Design B gap at 3 km and 20 km")

# The same buffer figures for each alternative grouping.
pollutant_names <- c(pm10 = "PM10", pm25 = "PM2.5")
alt_plot_data <- lapply(alt_names, function(g) resolution_buffer_plot_data(
  base = alt_contrasts_3km[[g]], alternative = alt_contrasts_20km[[g]],
  levels = levels_dt, city_labels = city_labels, support_order = support_order))
alt_buffer_plots <- list()
for (g in alt_names) for (poll in pollutants) {
  alt_buffer_plots[[paste(g, poll, sep = "_")]] <- plot_resolution_buffer_gaps(
    comparison = alt_plot_data[[g]][pollutant == poll],
    title      = paste0(pollutant_names[[poll]],
      ": native Design B gap at 3 km and 20 km, ", alt_titles[[g]]),
    gap_label  = alt_groupings[[g]])
}

# Design A headline gap by support under the four definitions, per buffer.
definition_plot_data <- resolution_label_supports(definition_comparison[design == "A"],
  levels_dt, city_labels, support_order)
definition_plots <- list()
for (b in c("3 km", "20 km")) for (poll in pollutants) {
  definition_plots[[paste(sub(" ", "", b), poll, sep = "_")]] <-
    plot_resolution_grouping_comparison(
      comparison = definition_plot_data[buffer == b & pollutant == poll],
      title      = paste0(pollutant_names[[poll]], ": Design A headline gap by ",
                          "education definition, ", b, " buffer"))
}

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the tables
fwrite(it_reading, csv_it_reading)
fwrite(bogota_classes, csv_bogota_classes)
fwrite(quintile_ties, csv_quintile_ties)
fwrite(santiago_shares, csv_santiago_shares)
fwrite(santiago_comunas, csv_santiago_units)
fwrite(monitoring, csv_monitoring)
fwrite(buffer_gaps, csv_buffer_gaps)
fwrite(buffer_coverage, csv_buffer_coverage)
fwrite(buffer_parts, csv_buffer_parts)
for (g in alt_names) fwrite(alt_gaps[[g]], csv_alt_gaps[[g]])
fwrite(definition_comparison, csv_definitions)

# Save the figures
ggsave(pdf_santiago_units, plot_santiago_units, width = 8, height = 9.5)
ggsave(pdf_santiago_shares, plot_santiago_shares, width = 8, height = 6.5)
ggsave(pdf_monitoring, plot_monitoring, width = 8, height = 8)
ggsave(pdf_buffer_pm10, plot_buffer_pm10, width = 10, height = 8)
ggsave(pdf_buffer_pm25, plot_buffer_pm25, width = 10, height = 8)
for (name in names(alt_buffer_plots)) {
  ggsave(pdf_alt_buffers[[name]], alt_buffer_plots[[name]], width = 10, height = 8)
}
for (name in names(definition_plots)) {
  ggsave(pdf_definitions[[name]], definition_plots[[name]], width = 10, height = 8.5)
}
