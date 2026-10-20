# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate fine-unit IDW exposure over the buffer x exponent grid.
#
#' @Description: Specification sensitivity, inputs of the primary Design A. For Bogotá
# 2018, Santiago 2017 and São Paulo 2010, reruns aggregate_idw_exposure() on the frozen
# fine distance matrix and 2023 station panel at every buffer in
# resolution_grid_buffers_km at exponent 1 and resolution_grid_power2_buffers_km at
# exponent 2. The
# manuscript keeps 3/5 km and exponent 1; these files serve the comparison only. Writes
# one exposure file per city, buffer and exponent to
# data/processed/resolution_sensitivity/idw_grid/power_<p>/<city>/, a grid manifest and
# the comparison of the exponent-1 files with the manuscript files at 3, 5 and 20 km.
#
#' @Summary:
#   I.  Import data: functions, grid settings, frozen inputs and output folders.
#   II. Process and save data: interpolate each specification, then check the anchors.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and the grid settings
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

# Settings: the three ladder cities (Bogotá, the slowest, runs last) and the manuscript
# buffers used as anchors. DuckDB memory, threads and spill folder are computing
# resources; they never change estimates. Every call reuses the one spill folder on the
# internal disk, so DuckDB overwrites its temporary files instead of accumulating them.
cities            <- c("santiago_2017", "sao_paulo_2010", "bogota_2018")
anchor_buffers_km <- c(3, 5, 20)
duckdb_mem_gb     <- 16
duckdb_threads    <- 2L
dir_duckdb_spill  <- file.path(dirname(tempdir()), "idw_grid_duckdb_spill")

# Set the folders
dir_inputs <- here::here("data", "interim", "resolution_sensitivity")
dir_frozen <- here::here("data", "processed", "resolution_sensitivity")
dir_grid   <- here::here("data", "processed", "resolution_sensitivity", "idw_grid")
dir_idw    <- here::here("data", "processed", "idw_estimates")

# Read each city's frozen input manifest; stop if any input changed after freezing
manifests <- lapply(setNames(cities, cities), function(city) {
  fread(file.path(dir_inputs, city, "input_manifest.csv"))
})
for (city in cities) {
  stopifnot(identical(unname(tools::md5sum(here::here(manifests[[city]]$path))),
                      manifests[[city]]$md5))
}

# A grid folder belongs to one input snapshot: files from this snapshot are kept on a
# rerun (an interrupted run resumes); files from another snapshot stop the run.
snapshot      <- data.table(city_id = cities, input_manifest_md5 = unname(tools::md5sum(
  file.path(dir_inputs, cities, "input_manifest.csv"))))
snapshot_file <- file.path(dir_grid, "snapshot.csv")
if (file.exists(snapshot_file) &&
    !isTRUE(all.equal(fread(snapshot_file), snapshot, check.attributes = FALSE))) {
  stop("idw_grid/ was built from another input snapshot; move it aside to rebuild.")
}
dir.create(dir_grid, recursive = TRUE, showWarnings = FALSE)
fwrite(snapshot, snapshot_file)

# Set the inputs: the frozen fine matrix and the frozen year=2023 panel partition
fine_matrices <- setNames(file.path(dir_frozen, cities, "B", "fine",
                                    "matrix_geo_station_distances.parquet"), cities)
panel_dirs    <- vapply(manifests, function(m) {
  dirname(here::here(m[input == "pollution", path]))
}, character(1))

# Read the frozen individual quintile cells; interpolation never uses the groups
cells <- lapply(setNames(cities, cities), function(city) {
  x <- as.data.table(arrow::read_parquet(file.path(dir_inputs, city,
                                                   "individual_quintile_cells.parquet")))
  x[, adult := 1L]
})

# ========================================================================================
# II: Process and save data
# ========================================================================================
# Count the eligible pairs (0 < d <= buffer) of each city and buffer
eligible_pairs <- rbindlist(lapply(cities, function(city) {
  d <- arrow::read_parquet(fine_matrices[[city]], col_select = "distance_km")$distance_km
  data.table(city_id = city, buffer_km = resolution_grid_buffers_km,
             eligible_pairs = vapply(resolution_grid_buffers_km,
                                     function(b) sum(d > 0 & d <= b), numeric(1)))
}))
print(eligible_pairs)

# Interpolate each city, exponent and buffer, largest buffer first; a buffer without
# eligible pairs is skipped, and a file already written from this snapshot is kept.
# Each call writes one file.
grid_runs <- list()
for (city in cities) for (p in resolution_grid_distance_powers) {
  buffers_p <- if (p == 1) resolution_grid_buffers_km else resolution_grid_power2_buffers_km
  for (b in sort(buffers_p, decreasing = TRUE)) {
    if (eligible_pairs[city_id == city & buffer_km == b, eligible_pairs] == 0) next
    out_dir <- file.path(dir_grid, paste0("power_", p), city)
    path    <- file.path(out_dir, paste0(city, "_", b, "km_idw_exposure.parquet"))
    if (file.exists(path)) {
      grid_runs[[length(grid_runs) + 1L]] <- data.table(city_id = city, buffer_km = b,
        distance_power = p, path = sub(paste0(here::here(), "/"), "", path, fixed = TRUE),
        md5 = unname(tools::md5sum(path)), seconds = NA_real_)
      next
    }
    started <- Sys.time()

    aggregate_idw_exposure(arrow_dir = panel_dirs[[city]],
        geo_sta_pq            = fine_matrices[[city]],
        census_col            = cells[[city]],
        group_var             = "edu_quintile",
        quintile_level        = "individual",
        precomputed_group_col = "edu_quintile",
        buffer_km             = b,
        distance_power        = p,
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
    message(city, ": ", b, " km, exponent ", p, ", ", round(seconds), " s")
    grid_runs[[length(grid_runs) + 1L]] <- data.table(city_id = city, buffer_km = b,
      distance_power = p, path = sub(paste0(here::here(), "/"), "", path, fixed = TRUE),
      md5 = unname(tools::md5sum(path)), seconds = seconds)
  }
}

# Save the grid manifest: one row per exposure file, with its eligible pairs
grid_manifest <- merge(rbindlist(grid_runs), eligible_pairs, by = c("city_id", "buffer_km"))
resolution_write(grid_manifest, file.path(dir_grid, "grid_manifest"))

# Compare the exponent-1 files with the manuscript files at the same buffer
anchor_outcomes <- c("avg_pm10", "avg_pm25", "hrs_d_pm10_it1", "hrs_d_pm10_it2",
                     "hrs_d_pm25_it1", "hrs_d_pm25_it2", "total_hrs_pm10", "total_hrs_pm25")
manuscript_anchors <- rbindlist(lapply(cities, function(city) {
  rbindlist(lapply(anchor_buffers_km, function(b) {
    grid <- as.data.table(arrow::read_parquet(here::here(
      grid_manifest[city_id == city & buffer_km == b & distance_power == 1, path])))
    manuscript <- as.data.table(arrow::read_parquet(file.path(dir_idw, city,
      paste0(city, "_", b, "km_idw_exposure.parquet"))))
    if (city == "bogota_2018") {
      manuscript[, geo_id := stringi::stri_pad_right(geo_id, 22L, pad = "0")]
    }
    idw_grid_anchor(grid[year == analysis_year], manuscript[year == analysis_year],
                    anchor_outcomes)[, `:=`(city_id = city, buffer_km = b)]
  }))
}))
resolution_write(manuscript_anchors, file.path(dir_grid, "manuscript_anchors"))
print(manuscript_anchors)
stopifnot(all(manuscript_anchors$passed))
