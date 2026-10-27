# Synthetic contracts for the station sets of the specification-sensitivity analysis.

# Five stations, one reading per listed month (stored clock, UTC):
#   s_both  PM10 and PM2.5 in January-March      s_pm10  PM10 only, January-March
#   s_pm25  PM2.5 only, January-March            s_dec   PM10 January-March, PM2.5 December
#   s_none  rows without any reading
station_sets_toy <- function() {
  hour <- function(m) as.POSIXct(sprintf("2023-%02d-15 12:00:00", m), tz = "UTC")
  months <- c(1, 2, 3)
  data.table::rbindlist(list(
    data.table::data.table(station = "s_both", datetime = hour(months), pm10 = 10,
                           pm25 = 5),
    data.table::data.table(station = "s_pm10", datetime = hour(months), pm10 = 20,
                           pm25 = NA_real_),
    data.table::data.table(station = "s_pm25", datetime = hour(months), pm10 = NA_real_,
                           pm25 = 7),
    data.table::data.table(station = "s_dec", datetime = hour(c(months, 12)),
                           pm10 = c(30, 30, 30, NA), pm25 = c(NA, NA, NA, 9)),
    data.table::data.table(station = "s_none", datetime = hour(months), pm10 = NA_real_,
                           pm25 = NA_real_)))
}

test_that("station reporting counts valid hours and months for every station", {
  r <- station_reporting(station_sets_toy())
  data.table::setkey(r, station, pollutant)
  expect_equal(nrow(r), 10L)
  expect_equal(r[.("s_both", "pm25")]$valid_hours, 3L)
  expect_equal(r[.("s_dec", "pm25")]$months, 1L)
  expect_equal(r[.("s_dec", "pm10")]$months, 3L)
  expect_equal(r[.("s_pm10", "pm25")]$valid_hours, 0L)
  expect_true(is.na(r[.("s_none", "pm10")]$first_hour))
  expect_equal(r[.("s_dec", "pm25")]$first_hour,
               as.POSIXct("2023-12-15 12:00:00", tz = "UTC"))
})

test_that("the both-pollutant rule and the month rule keep the intended networks", {
  r <- station_reporting(station_sets_toy())
  both <- station_set_members(r, require = c("pm10", "pm25"), min_months = 1L)
  expect_setequal(both[pollutant == "pm10" & in_set, station], c("s_both", "s_dec"))
  expect_setequal(both[pollutant == "pm25" & in_set, station], c("s_both", "s_dec"))

  months <- station_set_members(r, require = character(), min_months = 3L)
  expect_setequal(months[pollutant == "pm10" & in_set, station],
                  c("s_both", "s_pm10", "s_dec"))
  expect_setequal(months[pollutant == "pm25" & in_set, station], c("s_both", "s_pm25"))
  expect_error(station_set_members(r, require = "no2"))
})

test_that("restricting the panel blanks only the readings outside the set", {
  panel <- station_sets_toy()
  members <- station_set_members(station_reporting(panel), c("pm10", "pm25"), 1L)
  out <- restrict_station_panel(panel, members)
  expect_equal(nrow(out), nrow(panel))
  expect_identical(out$station, panel$station)
  expect_true(all(is.na(out[station %in% c("s_pm10", "s_pm25"), c(pm10, pm25)])))
  kept <- c("s_both", "s_dec")
  expect_identical(out[station %in% kept], panel[station %in% kept])
  expect_true(any(is.finite(panel[station == "s_pm10", pm10])))  # input untouched
})

# Blanking s3's PM10 must equal removing s3 from the distance matrix. In the toy of
# helper-fixtures.R, g2 then averages s1 and s2 (both at 1 km): h0 = (60 + 120) / 2 = 90,
# h1 = 40, so the mean is 65; g1 never reached s3 (4 km) and keeps (80 + 40) / 2 = 60.
test_that("IDW on a blanked panel equals IDW without the station", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("duckdb", minimum_version = "0.9.2")

  root <- tempfile("station_sets_toy_")
  dir.create(root)
  fx <- make_toy_fixture(root)
  panel <- data.table::as.data.table(arrow::read_parquet(file.path(fx$arrow, "year=2023",
                                                                   "part-0.parquet")))
  members <- data.table::data.table(station = c("s1", "s2", "s3"), pollutant = "pm10",
                                    in_set = c(TRUE, TRUE, FALSE))
  blanked_dir <- file.path(root, "blanked", "year=2023")
  dir.create(blanked_dir, recursive = TRUE)
  arrow::write_parquet(restrict_station_panel(panel, members),
                       file.path(blanked_dir, "part-0.parquet"))
  dropped_pq <- file.path(root, "dist_without_s3.parquet")
  arrow::write_parquet(data.table::as.data.table(arrow::read_parquet(fx$dist_pq))[
    station_id != "s3"], dropped_pq)

  run <- function(arrow_dir, dist_pq, name) {
    out <- suppressMessages(aggregate_idw_exposure(arrow_dir = arrow_dir,
      geo_sta_pq = dist_pq, census_col = fx$census, pop_col = "pop_total",
      group_var = "education_mean", n_groups = 2L, group_name = "edu_half",
      quintile_level = "geo", buffer_km = 3, distance_power = 1, target_years = 2023,
      pollutants = "pm10", mem_gb = 1, n_threads = 1, out_dir = file.path(root, name),
      out_name = name, chunk_by_month = FALSE, quiet = TRUE, return_data = TRUE))
    res <- data.table::as.data.table(out$exposure_yearly)
    data.table::setkey(res, geo_id)
    res[, .(geo_id, avg_pm10, total_hrs_pm10 = as.numeric(total_hrs_pm10),
            hrs_d_pm10_it2 = as.numeric(hrs_d_pm10_it2))]
  }
  blanked <- run(file.path(root, "blanked"), fx$dist_pq, "blanked")
  # Without s3 in the matrix, the estimator warns that s3's readings are dropped: the point
  dropped <- suppressWarnings(run(fx$arrow, dropped_pq, "dropped"))

  expect_equal(blanked, dropped)
  expect_equal(blanked["g2"]$avg_pm10, 65)
  expect_equal(blanked["g1"]$avg_pm10, 60)
})
