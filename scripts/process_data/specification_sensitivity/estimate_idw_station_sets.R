# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate fine-unit IDW exposure for each declared station set.
#
#' @Description: Specification sensitivity, inputs of the station-set comparison. Counts
# the hours and months in which each station reports PM10 and PM2.5 in 2023, before and
# after cleaning, for the four cities. For Bogotá 2018, Santiago 2017 and São Paulo 2010,
# keeps the stations of each set in resolution_station_sets, blanks the other readings of
# the frozen 2023 panel and reruns aggregate_idw_exposure() at exponent 1 on
# resolution_station_set_buffers_km. Writes the restricted panels, the exposures and a
# manifest to data/processed/resolution_sensitivity/station_sets/<set>/, checks the
# exposures against the default grid wherever a set keeps a pollutant's whole network,
# and writes the station tables to results/tables/specification_sensitivity/review/.
#
#' @Summary:
#   I.  Import data: functions, settings, frozen inputs, station panels and folders.
#   II. Process and save data: station reporting, sets, restricted panels, exposures
#       and the comparison with the default grid.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and the station-set settings
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

# Settings: the three ladder cities (Bogotá, the slowest, runs last) and the four cities
# of the station table, by panel name. DuckDB memory, threads and spill folder are
# computing resources, as in the grid recipe.
cities           <- c("santiago_2017", "sao_paulo_2010", "bogota_2018")
city_labels      <- c(santiago_2017 = "Santiago", sao_paulo_2010 = "São Paulo",
                      bogota_2018 = "Bogotá")
table_cities     <- c(bogota = "Bogotá", cdmx = "Mexico City", santiago = "Santiago",
                      sao_paulo = "São Paulo")
pollutants       <- c("pm10", "pm25")
duckdb_mem_gb    <- 16
duckdb_threads   <- 2L
dir_duckdb_spill <- file.path(dirname(tempdir()), "idw_grid_duckdb_spill")

# Set the folders
dir_inputs <- here::here("data", "interim", "resolution_sensitivity")
dir_frozen <- here::here("data", "processed", "resolution_sensitivity")
dir_grid   <- here::here(dir_frozen, "idw_grid")
dir_sets   <- here::here(dir_frozen, "station_sets")
dir_raw    <- here::here("data", "interim", "monitoring_stations")
dir_clean  <- here::here("data", "processed", "monitoring_stations_outliers")
dir_review <- here::here("results", "tables", "specification_sensitivity", "review")

# Read each city's frozen input manifest; stop if any input changed after freezing
manifests <- lapply(setNames(cities, cities), function(city) {
  fread(file.path(dir_inputs, city, "input_manifest.csv"))
})
for (city in cities) {
  stopifnot(identical(unname(tools::md5sum(here::here(manifests[[city]]$path))),
                      manifests[[city]]$md5))
}

# The station-set folder belongs to one input snapshot and one set of rules: files from
# them are kept on a rerun (an interrupted run resumes); anything else stops the run.
# One row per city and set, in plain fields that read back unchanged.
manifest_md5  <- unname(tools::md5sum(file.path(dir_inputs, cities, "input_manifest.csv")))
snapshot      <- rbindlist(lapply(names(resolution_station_sets), function(set) {
  rule <- resolution_station_sets[[set]]
  data.table(city_id = cities, input_manifest_md5 = manifest_md5, station_set = set,
             require = if (length(rule$require)) paste(rule$require, collapse = " ")
                       else "none",
             min_months = rule$min_months)
}))
snapshot_file <- file.path(dir_sets, "snapshot.csv")
if (file.exists(snapshot_file) &&
    !isTRUE(all.equal(fread(snapshot_file), snapshot, check.attributes = FALSE))) {
  stop("station_sets/ was built from another snapshot or rule; move it aside to rebuild.")
}
dir.create(dir_sets, recursive = TRUE, showWarnings = FALSE)
fwrite(snapshot, snapshot_file)

# Set the inputs: the frozen fine matrices and the frozen year=2023 panels
fine_matrices <- setNames(file.path(dir_frozen, cities, "B", "fine",
                                    "matrix_geo_station_distances.parquet"), cities)
panel_files   <- vapply(manifests, function(m) {
  here::here(m[input == "pollution", path])
}, character(1))

# Read the frozen panels and their schema, which the restricted copies keep
panels        <- lapply(panel_files, function(f) as.data.table(arrow::read_parquet(f)))
panel_schemas <- lapply(panel_files, function(f) arrow::open_dataset(f)$schema)

# Read the frozen individual quintile cells; interpolation never uses the groups
cells <- lapply(setNames(cities, cities), function(city) {
  x <- as.data.table(arrow::read_parquet(file.path(dir_inputs, city,
                                                   "individual_quintile_cells.parquet")))
  x[, adult := 1L]
})

# Read the 2023 station readings of the four cities before and after outlier cleaning
readings_raw   <- lapply(setNames(names(table_cities), names(table_cities)), function(c) {
  as.data.table(arrow::read_parquet(file.path(dir_raw, paste0(c, "_metro_dataset"),
    "year=2023", "data_0.parquet"),
    col_select = dplyr::all_of(c("station", "datetime", pollutants))))
})
readings_clean <- lapply(setNames(names(table_cities), names(table_cities)), function(c) {
  as.data.table(arrow::read_parquet(file.path(dir_clean, paste0(c, "_metro_clean"),
    "year=2023", "data.parquet"),
    col_select = dplyr::all_of(c("station", "datetime", pollutants))))
})

# ========================================================================================
# II: Process and save data
# ========================================================================================
# 1. Hours and months of each station and pollutant, before and after cleaning
reporting_raw   <- lapply(readings_raw, station_reporting, pollutants = pollutants)
reporting_clean <- lapply(readings_clean, station_reporting, pollutants = pollutants)

# One row per city, station and pollutant: raw hours beside the cleaned description
station_reporting_table <- rbindlist(lapply(names(table_cities), function(c) {
  raw <- reporting_raw[[c]][, .(station_id = normalize_station(station), pollutant,
                                raw_valid_hours = valid_hours)]
  clean <- copy(reporting_clean[[c]])[, station_id := normalize_station(station)]
  merge(raw, clean, by = c("station_id", "pollutant"), all = TRUE)[
    , city := table_cities[[c]]]
}))
setcolorder(station_reporting_table, c("city", "station_id", "station", "pollutant"))

# Count the stations reporting each pollutant, both, only one, or neither (cleaned panel)
reports <- dcast(station_reporting_table, city + station_id ~ pollutant,
                 value.var = "valid_hours", fill = 0L)
stations_by_pollutant_sets <- reports[, .(stations = .N, pm10 = sum(pm10 > 0),
  pm25 = sum(pm25 > 0), both = sum(pm10 > 0 & pm25 > 0),
  pm10_only = sum(pm10 > 0 & pm25 == 0), pm25_only = sum(pm25 > 0 & pm10 == 0),
  neither = sum(pm10 == 0 & pm25 == 0)), by = city]

# 2. Members of each station set in each city, from the frozen panel's reporting
reporting_frozen <- lapply(panels, station_reporting, pollutants = pollutants)
members <- lapply(resolution_station_sets, function(rule) {
  lapply(reporting_frozen, station_set_members, require = rule$require,
         min_months = rule$min_months)
})

# Add the stations each set keeps in each network to the station table
for (set in names(resolution_station_sets)) {
  networks <- rbindlist(lapply(cities, function(city) {
    members[[set]][[city]][reporting_frozen[[city]], on = c("station", "pollutant")][
      valid_hours > 0, .(city = city_labels[[city]], n = sum(in_set)), by = pollutant]
  }))
  networks <- dcast(networks, city ~ pollutant, value.var = "n")
  setnames(networks, pollutants, paste0(set, "_", pollutants))
  stations_by_pollutant_sets <- merge(stations_by_pollutant_sets, networks, by = "city",
                                      all.x = TRUE, sort = FALSE)
}

# 3. Restricted panels: the frozen panel with the readings outside each set blanked.
#    Each call writes one Parquet file in the frozen year=2023 layout.
restricted_dirs <- list()
for (set in names(resolution_station_sets)) for (city in cities) {
  restricted <- restrict_station_panel(panel   = panels[[city]],
                                       members = members[[set]][[city]])
  out_dir <- file.path(dir_sets, set, "panels", city, "year=2023")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(arrow::arrow_table(restricted, schema = panel_schemas[[city]]),
                       file.path(out_dir, "data.parquet"))
  restricted_dirs[[set]][[city]] <- out_dir
}

# 4. Interpolate each set, city and buffer at exponent 1, largest buffer first; a file
#    already written from this snapshot is kept. Each call writes one file.
set_runs <- list()
for (set in names(resolution_station_sets)) for (city in cities) {
  for (b in sort(resolution_station_set_buffers_km, decreasing = TRUE)) {
    out_dir <- file.path(dir_sets, set, "idw_grid", city)
    path    <- file.path(out_dir, paste0(city, "_", b, "km_idw_exposure.parquet"))
    seconds <- NA_real_
    if (!file.exists(path)) {
      started <- Sys.time()

      aggregate_idw_exposure(arrow_dir = restricted_dirs[[set]][[city]],
          geo_sta_pq            = fine_matrices[[city]],
          census_col            = cells[[city]],
          group_var             = "edu_quintile",
          quintile_level        = "individual",
          precomputed_group_col = "edu_quintile",
          buffer_km             = b,
          distance_power        = 1,
          target_years          = analysis_year,
          mem_gb                = duckdb_mem_gb,
          n_threads             = duckdb_threads,
          duckdb_temp_dir       = dir_duckdb_spill,
          out_dir               = out_dir,
          out_name              = paste0(city, "_", b, "km"),
          indiv_name            = city,
          overwrite             = TRUE,
          quiet                 = TRUE)

      seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      message(set, ", ", city, ": ", b, " km, ", round(seconds), " s")
    }
    set_runs[[length(set_runs) + 1L]] <- data.table(station_set = set, city_id = city,
      buffer_km = b, distance_power = 1,
      path = sub(paste0(here::here(), "/"), "", path, fixed = TRUE),
      md5 = unname(tools::md5sum(path)), seconds = seconds)
  }
}

# Save the set manifest: one row per exposure file
set_manifest <- rbindlist(set_runs)
resolution_write(set_manifest, file.path(dir_sets, "set_manifest"))

# 5. Where a set keeps every station that reports a pollutant, its exposure for that
#    pollutant must equal the default grid file at the same buffer
grid_manifest <- fread(file.path(dir_grid, "grid_manifest.csv"))
default_anchors <- rbindlist(lapply(seq_len(nrow(set_manifest)), function(i) {
  r <- set_manifest[i]
  m <- members[[r$station_set]][[r$city_id]][reporting_frozen[[r$city_id]],
                                             on = c("station", "pollutant")]
  kept_all <- m[valid_hours > 0, .(unchanged = all(in_set)), by = pollutant][
    unchanged == TRUE, pollutant]
  if (!length(kept_all)) return(NULL)
  outcomes <- c(paste0("avg_", kept_all), paste0("hrs_d_", kept_all, "_it1"),
                paste0("hrs_d_", kept_all, "_it2"), paste0("total_hrs_", kept_all))
  set_exposure <- as.data.table(arrow::read_parquet(here::here(r$path)))
  default      <- as.data.table(arrow::read_parquet(here::here(grid_manifest[
    city_id == r$city_id & buffer_km == r$buffer_km & distance_power == 1, path])))
  idw_grid_anchor(set_exposure[year == analysis_year], default[year == analysis_year],
                  outcomes)[, `:=`(station_set = r$station_set, city_id = r$city_id,
                                   buffer_km = r$buffer_km)]
}))
resolution_write(default_anchors, file.path(dir_sets, "default_anchors"))
print(default_anchors)
stopifnot(all(default_anchors$passed))

# Save the station tables of the four cities for the review
dir.create(dir_review, recursive = TRUE, showWarnings = FALSE)
fwrite(stations_by_pollutant_sets,
       file.path(dir_review, "stations_by_pollutant_sets_2023.csv"))
fwrite(station_reporting_table, file.path(dir_review, "station_reporting_2023.csv"))
