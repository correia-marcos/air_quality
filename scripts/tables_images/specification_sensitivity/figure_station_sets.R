# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Figures and comparison table of the station sets against the default stations.
#
#' @Description: Reads the station table (estimate_idw_station_sets.R), the station-set
# Design A tables (estimate_design_a_station_sets.R), the default Design A grid
# contrasts and the frozen geography. Maps the stations of Bogotá, Santiago and São Paulo
# by what they report in 2023 over the mean education quintile of residents; draws the
# 3 km against 20 km Design A gap by support for each set and grouping (quintiles and
# quintiles with split ties); and draws the finest-support quintile gap by buffer for the
# default stations and each set. Writes the comparison table to
# results/tables/specification_sensitivity/station_sets/ and the PDFs to
# results/figures/specification_sensitivity/station_sets/. Estimates nothing.
#
#' @Summary:
#   I.   Import data: functions, settings, saved tables, stations and geography.
#   II.  Process data: station categories, maps, plotting tables and plot objects.
#   III. Save outputs: the comparison table, then the figures.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions, the review and grid figures, the station map and the paper theme
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))
for (file in c("resolution_review.R", "design_a_grid.R", "station_sets.R")) {
  source(here::here("src", "general_utilities", "plot", "specification_sensitivity", file))
}
set_paper_theme(base_size = 10)

# Settings: cities, labels, the units mapped, the station sets and their labels
cities          <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
city_labels     <- c(bogota_2018 = "Bogota", santiago_2017 = "Santiago",
                     sao_paulo_2010 = "Sao Paulo")
table_labels    <- c(bogota_2018 = "Bogotá", santiago_2017 = "Santiago",
                     sao_paulo_2010 = "São Paulo")
map_levels      <- c(bogota_2018 = "sector", santiago_2017 = "fine",
                     sao_paulo_2010 = "fine")
pollutants      <- c("pm10", "pm25")
pollutant_names <- c(pm10 = "PM10", pm25 = "PM2.5")
sets            <- names(resolution_station_sets)
set_labels      <- c(default = "All reporting stations (default)",
                     both_pollutants = "Stations reporting both pollutants",
                     three_months = "Records of three months or more")
groupings       <- c(edu_quintile = "Q1 - Q5", edu_quintile_split = "Q1 - Q5 (split ties)")
grouping_files  <- c(edu_quintile = "", edu_quintile_split = "quintile_split_")
grouping_titles <- c(edu_quintile = "", edu_quintile_split = ", quintiles with split ties")
support_order   <- c("Municipio", "Localidad/municipio", "Comuna", "Sector",
                     "Distrito censal", "Seccion", "Fine census unit", "Zona censal",
                     "Area de ponderacao")

# Set the folders
dir_inputs  <- here::here("data", "interim", "resolution_sensitivity")
dir_frozen  <- here::here("data", "processed", "resolution_sensitivity")
dir_sets    <- here::here(dir_frozen, "station_sets", "design_a")
dir_review  <- here::here("results", "tables", "specification_sensitivity", "review")
dir_tables  <- here::here("results", "tables", "specification_sensitivity", "station_sets")
dir_figures <- here::here("results", "figures", "specification_sensitivity",
                          "station_sets")

# Read the station table and the station-set Design A tables
station_reporting <- fread(file.path(dir_review, "station_reporting_2023.csv"))
read_sets <- function(name) as.data.table(arrow::read_parquet(file.path(dir_sets,
  paste0("station_sets_", name, ".parquet"))))
set_contrasts     <- read_sets("contrasts")
set_samples       <- read_sets("samples")
set_decomposition <- read_sets("decomposition_vs_default")

# Read the default Design A contrasts at the set buffers (quintiles, exponent 1)
default_contrasts <- as.data.table(arrow::read_parquet(file.path(dir_frozen,
  "design_a_grid", "design_a_grid_contrasts.parquet")))[grouping == "edu_quintile" &
  distance_power == 1 & buffer_km %in% resolution_station_set_buffers_km]

# Read the support ladders, the crosswalks and the frozen quintile cells
levels_dt <- rbindlist(lapply(cities, function(city) {
  fread(file.path(dir_inputs, city, "levels.csv"))[, city_id := city]
}))
keys  <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "crosswalk.parquet"))))
cells <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "individual_quintile_cells.parquet"))))

# Read the mapped units (Bogotá sectors; Santiago and São Paulo fine units) and the
# frozen station catalogues
units <- lapply(setNames(cities, cities), function(city) {
  sf::st_read(file.path(dir_inputs, city, paste0(map_levels[[city]], "_polygons.gpkg")),
              quiet = TRUE)
})
catalogues <- lapply(setNames(cities, cities), function(city) {
  manifest <- fread(file.path(dir_inputs, city, "input_manifest.csv"))
  sf::st_read(here::here(manifest[input == "stations", path]), quiet = TRUE)
})

# ========================================================================================
# II: Process data
# ========================================================================================
# 1. Station categories: what each station reports in 2023 and whether a record it has
#    covers fewer than three months
categories <- dcast(station_reporting, city + station_id ~ pollutant,
                    value.var = c("valid_hours", "months"), fill = 0L)
categories[, category := factor(fcase(
  valid_hours_pm10 > 0 & valid_hours_pm25 > 0, "Both",
  valid_hours_pm10 > 0, "PM10 only",
  valid_hours_pm25 > 0, "PM2.5 only",
  default = "Neither"), levels = c("Both", "PM10 only", "PM2.5 only", "Neither"))]
categories[, record := factor(fifelse(
  (valid_hours_pm10 > 0 & months_pm10 < 3) | (valid_hours_pm25 > 0 & months_pm25 < 3),
  "Part-year (under three months)", "Three months or more"),
  levels = c("Three months or more", "Part-year (under three months)"))]

# 2. Map layers: the mean education quintile of each mapped unit's adults, and the
#    stations of the panel placed by the catalogue (Bogotá sectors are simplified)
map_units <- lapply(setNames(cities, cities), function(city) {
  k <- keys[[city]][level == map_levels[[city]], .(geo_id, parent_id)]
  means <- merge(cells[[city]], k, by = "geo_id")[
    , .(mean_quintile = sum(edu_quintile * person_weight) / sum(person_weight)),
    by = parent_id]
  u <- merge(units[[city]], means, by.x = "geo_id", by.y = "parent_id", all.x = TRUE)
  if (city == "bogota_2018") u <- sf::st_simplify(u, dTolerance = 30)
  u
})
map_stations <- lapply(setNames(cities, cities), function(city) {
  s <- catalogues[[city]]
  s$station_id <- normalize_station(s$station_name)
  label <- table_labels[[city]]
  merge(s, categories[city == label], by = "station_id")
})
maps <- lapply(setNames(cities, cities), function(city) {
  plot_station_set_map(units    = map_units[[city]],
                       stations = map_stations[[city]],
                       title    = paste0(city_labels[[city]],
                                         ": monitoring stations by what they report, 2023"),
                       caption  = paste("Stations of the cleaned 2023 panel placed by",
                         "the frozen station catalogue. A part-year record covers fewer",
                         "than three months of a pollutant.\nShading: mean education",
                         "quintile of the education-reporting adults of each",
                         if (city == "bogota_2018") "sector." else "fine unit."))
})

# 3. The 3 km against 20 km Design A gap by support for each set and grouping (localidad
#    branch off the line, as in the review); each panel notes the set's samples
buffer_plots <- list()
for (set in sets) for (g in names(groupings)) for (poll in pollutants) {
  x <- set_contrasts[station_set == set & grouping == g & buffer_km %in% c(3, 20) &
                       !(city_id == "bogota_2018" & level == "localidad")]
  x <- resolution_label_supports(x[, design := "A"], levels_dt, city_labels,
                                 support_order)
  x[, buffer := factor(paste(buffer_km, "km"), levels = c("3 km", "20 km"))]
  notes <- set_samples[station_set == set & buffer_km %in% c(3, 20) & pollutant == poll][
    order(buffer_km), .(label = paste0(buffer_km, " km: N=",
      format(round(sample_population / 1e6, 2), nsmall = 2), "m (",
      round(100 * share_of_default), "% of default)", collapse = "\n")), by = city_id]
  notes <- notes[rep(seq_len(.N), each = 3)]
  notes[, panel := paste0(city_labels[city_id], ": ",
                          c("annual mean", "hours >= IT1", "hours >= IT2"))]
  buffer_plots[[paste0(set, "_", grouping_files[[g]], poll)]] <-
    plot_resolution_buffer_gaps(
      comparison = x[pollutant == poll],
      title      = paste0(pollutant_names[[poll]], ": Design A gap at 3 km and 20 km, ",
                          tolower(set_labels[[set]]), grouping_titles[[g]]),
      gap_label  = groupings[[g]],
      design     = "A",
      notes      = notes) +
    ggplot2::labs(caption = paste("Design A: fine exposure averaged over each support;",
      "people fixed across supports within a buffer. Point labels (G): support units",
      "with sampled adults.\nThe change from the default stations is split into",
      "exposure and sample parts in station_sets_decomposition_vs_default.csv."))
}

# 4. Finest-support gap by buffer: the default stations and each set
finest <- rbind(
  default_contrasts[level == "fine", .(series = set_labels[["default"]], city_id,
                                       buffer_km, outcome, gap)],
  set_contrasts[level == "fine" & grouping == "edu_quintile",
                .(series = set_labels[station_set], city_id, buffer_km, outcome, gap)])
finest[, series := factor(series, levels = set_labels)]
finest <- design_a_panel_labels(finest, city_labels)
finest_plots <- lapply(setNames(pollutants, pollutants), function(poll) {
  plot_design_a_gap_by_buffer(
    x       = finest[grepl(poll, outcome)],
    series  = "series",
    title   = paste0(pollutant_names[[poll]],
                     ": Design A gap at the finest support, by station set"),
    caption = paste("Quintiles, exponent 1. Each set has its own sample at each buffer;",
                    "the decomposition table splits the change from the default."))
})

# 5. Comparison table: finest and coarsest supports at 3 and 20 km, default against each
#    set, with the exposure and sample parts of the change
coarsest <- levels_dt[, .(level = level[which.max(fine_to_coarse)]), by = city_id]
station_sets_comparison <- set_decomposition[buffer_km %in% c(3, 20) & (level == "fine" |
  paste(city_id, level) %in% coarsest[, paste(city_id, level)]),
  .(station_set, grouping, city_id, buffer_km, level, outcome, gap_default, gap_set,
    total_change, exposure_change_same_people = exposure_change,
    people_lost_change = -sample_contraction, common_population)]
setorder(station_sets_comparison, station_set, grouping, city_id, outcome, buffer_km, level)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the comparison table, then the figures
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
fwrite(station_sets_comparison, file.path(dir_tables, "station_sets_comparison.csv"))

for (city in cities) {
  ggsave(file.path(dir_figures, paste0("station_sets_map_", city, ".pdf")), maps[[city]],
         width = 9, height = 8)
}
for (name in names(buffer_plots)) {
  ggsave(file.path(dir_figures, paste0("station_sets_", name, "_buffer_design_a.pdf")),
         buffer_plots[[name]], width = 10, height = 8)
}
for (poll in pollutants) {
  ggsave(file.path(dir_figures, paste0("station_sets_finest_", poll, ".pdf")),
         finest_plots[[poll]], width = 10, height = 8)
}
