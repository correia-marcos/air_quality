# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Check the worked outlier example in doc/reference/outlier_cleaning.md.
#
#' @Description: Runs detect_pollution_outliers() with the manuscript settings on two
# stations and 100 hours. Station A reads 20 except 300 at hour 50:
#   gate      q99 = 20 + 0.01 * 280 = 22.8; hour 50 is tested
#   temporal  sd of first differences = sqrt(2 * 280^2 / 98) = 40; 280 > 80 fails
#   steady B  A - B: mean 2.8, sd 28; 280 > 58.8 fails -> reason 3, removed
#   tracking  A - B alternates -5/+5: sd ~ 5.03; -5 inside -> retained
# The screen codes 20, 2000, 2500, -3 and NA as 0, 0, 1, 2 and 3.
#
#' @Date: November 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ========================================================================================

test_that("the worked outlier example reproduces the hand-computed decisions", {
  settings <- new.env()
  source(here::here("config", "analysis_settings.R"), local = settings)
  root <- tempfile("outlier-example-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  hours <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC") + 0:99 * 3600
  a <- rep(20, 100); a[50] <- 300
  arrow::write_parquet(data.frame(station_from = c("A", "B"), station_to = c("B", "A"),
                                  distance_km = 1), file.path(root, "dist.parquet"))
  run <- function(b, tag) {
    panel <- data.frame(station = rep(c("A", "B"), each = 100), datetime = rep(hours, 2),
                        pm25 = c(a, b), year = 2023L)
    arrow::write_dataset(panel, file.path(root, tag), partitioning = "year")
    path <- detect_pollution_outliers(file.path(root, tag),
      file.path(root, "dist.parquet"), root, tag, pollutants = "pm25",
      upper_bounds         = settings$pollution_upper_bounds,
      pct_flag             = settings$outlier_quantile,
      n_sd                 = settings$outlier_sd_band,
      on_missing_temporal  = settings$outlier_missing_temporal,
      on_missing_neighbor  = settings$outlier_missing_neighbor,
      neighbor_eligibility = settings$outlier_neighbor_eligibility, quiet = TRUE)
    out <- data.table::as.data.table(dplyr::collect(arrow::open_dataset(path)))
    data.table::setorder(out, station, datetime)
    out
  }

  # The hand-computed gate and bands of sections 3(a)-(c).
  b_tracking <- a + 5 * (-1)^(1:100)
  expect_equal(unname(stats::quantile(a, 0.99)), 22.8)
  expect_equal(sd(diff(a)), 40)
  expect_equal(c(mean(a - 20), sd(a - 20)), c(2.8, 28))
  expect_equal(sd(a - b_tracking), sqrt(100 * 25 / 99))
  expect_equal(b_tracking[49:51], c(15, 305, 15))

  # Steady B: A's peak fails both checks and is removed with reason 3.
  steady <- run(rep(20, 100), "steady")
  peak <- steady[station == "A" & datetime == hours[50]]
  expect_equal(peak$pm25_original, 300)
  expect_identical(peak$pm25_outlier_reason, 3L)
  expect_true(is.na(peak$pm25))
  expect_true(all(steady[!(station == "A" & datetime == hours[50]),
                         pm25_outlier_reason] == 0L))

  # Tracking B: the neighbour confirms the peak at both stations.
  tracking <- run(b_tracking, "tracking")
  peaks <- tracking[datetime == hours[50]]
  expect_identical(peaks$pm25_outlier_reason, c(0L, 0L))
  expect_equal(peaks$pm25, c(300, 305))
  expect_true(all(tracking$pm25_outlier_reason == 0L))

  # Section 3(d): the screen codes and the analytical values they leave.
  screened <- screen_pollution_quality(data.frame(pm25 = c(20, 2000, 2500, -3, NA)),
                                       "pm25", settings$pollution_upper_bounds)
  expect_identical(screened$pm25_screen_reason, c(0L, 0L, 1L, 2L, 3L))
  expect_equal(screened$pm25, c(20, 2000, NA, NA, NA))
})

test_that("the declared outlier settings equal the function defaults", {
  settings <- new.env()
  source(here::here("config", "analysis_settings.R"), local = settings)
  defaults <- formals(detect_pollution_outliers)
  expect_identical(settings$outlier_quantile, eval(defaults$pct_flag))
  expect_identical(settings$outlier_sd_band, eval(defaults$n_sd))
  expect_identical(settings$outlier_missing_temporal, eval(defaults$on_missing_temporal))
  expect_identical(settings$outlier_missing_neighbor, eval(defaults$on_missing_neighbor))
  expect_identical(settings$outlier_neighbor_eligibility,
                   eval(defaults$neighbor_eligibility))
})
