# Readings by education quintile: both grouping methods on a hand-computed example.
# Five census units, one person each, so unit and person quintiles both equal the unit's
# rank. Station A reports 3 of 4 hours (one reported reading removed by cleaning), B
# reports 2 of 4 (none removed), C never measures the pollutant.

missing_quintile_fixture <- function(root) {
  hours <- as.POSIXct("2023-06-01 12:00:00", tz = "UTC") + 3600 * 0:3
  panel <- data.frame(
    station       = rep(c("A", "B", "C"), each = 4),
    datetime      = rep(hours, 3),
    pm10          = c(10, NA, 30, 40, NA, NA, 5, 5, NA, NA, NA, NA),
    pm10_original = c(10, 20, 30, 40, NA, NA, 5, 5, NA, NA, NA, NA))
  dir.create(file.path(root, "panel", "year=2023"), recursive = TRUE)
  arrow::write_parquet(panel, file.path(root, "panel", "year=2023", "data.parquet"))
  # Diagnostics beside the partitions must not be read as readings.
  dir.create(file.path(root, "panel", "_audit"))
  arrow::write_parquet(data.frame(note = "diagnostic"),
                       file.path(root, "panel", "_audit", "diagnostics.parquet"))
  distances <- data.frame(
    station     = rep(c("A", "B", "C"), each = 5),
    geo_id      = rep(paste0("g", 1:5), 3),
    distance_km = c(1, 2, 4, 6, 6,   6, 2, 1, 2, 6,   6, 6, 6, 6, 0.5))
  arrow::write_parquet(distances, file.path(root, "distances.parquet"))
  census <- data.frame(geo_id = paste0("g", 1:5), educ_years = 1:5, person_weight = 1)
  arrow::write_parquet(census, file.path(root, "census.parquet"))
  list(panel = file.path(root, "panel"), dist = file.path(root, "distances.parquet"),
       census = file.path(root, "census.parquet"))
}

quintile_functions <- function() {
  env <- new.env(parent = globalenv())
  for (path in c("src/general_utilities/base_utils.R",
                 "src/general_utilities/process/geo_ids.R",
                 "src/general_utilities/process/idw_exposure.R",
                 "src/general_utilities/process/diagnostics.R")) {
    sys.source(here::here(path), env)
  }
  env
}

test_that("both grouping methods give the hand-computed shares", {
  env <- quintile_functions()
  root <- tempfile("missing-quintile-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  files <- missing_quintile_fixture(root)
  run <- function(fun, method, ...) suppressMessages(fun(
    city = "Fixture", city_order = 1L, pollution_dir = files$panel, dist_pq = files$dist,
    census_file = files$census, geo_id_col = "geo_id", pollutants = "pm10", year = 2023L,
    method = method, buffer_km = 3, distance_power = 1, ...))

  counts <- env$station_reading_counts(files$panel, "pm10", 2023L)
  data.table::setorder(counts, station)
  expect_equal(counts$n_available, c(3, 2, 0))
  expect_equal(counts$n_reported, c(4, 2, 0))
  expect_equal(counts$n_removed, c(1, 0, 0))

  # Nearest unit: A -> g1 (Q1), B -> g3 (Q3); C (nearest g5) never reports, so Q5 drops.
  nearest <- run(env$compute_missing_by_quintile, "nearest_unit", report = "available")
  expect_equal(nearest$quintile, c(1, 3))
  expect_equal(nearest$value, c(3 / 4, 2 / 4))
  expect_equal(nearest$total_obs, c(4, 4))

  # IDW within 3 km, weights 1/d, reporting stations only: g1 = A; g2 = A and B at 2 km;
  # g3 and g4 = B; g5 has no reporting station within 3 km.
  idw <- run(env$compute_missing_by_quintile, "idw", report = "available")
  expect_equal(idw$quintile, 1:4)
  expect_equal(idw$value, c(0.75, (0.75 * 0.5 + 0.5 * 0.5) / 1, 0.5, 0.5))
  expect_equal(idw$total_obs, c(1, 1, 1, 1))

  # Removed over reported: A 1/4, B 0/2; C reports nothing, so Q5 drops out.
  removed <- run(env$compute_cleaning_removals_by_quintile, "nearest_unit")
  expect_equal(removed$quintile, c(1, 3))
  expect_equal(removed$value, c(0.25, 0))
  expect_identical(unique(removed$statistic), "removed_by_cleaning")
  removed_idw <- run(env$compute_cleaning_removals_by_quintile, "idw")
  expect_equal(removed_idw$value, c(0.25, 0.125, 0, 0))
})
