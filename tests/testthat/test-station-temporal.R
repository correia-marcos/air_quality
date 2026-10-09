# Exercise the extracted preparation on hand-computed inputs with duplicate MERRA hours,
# unmatched station hours, all-missing measurements and an out-of-metro São Paulo station.
test_that("temporal preparation preserves timestamp support and city station means", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("lubridate")
  env <- new.env(parent = asNamespace("dplyr"))
  env$hour <- lubridate::hour
  source(here::here("src", "general_utilities", "process", "merra2.R"),
         local = env)
  panel <- data.frame(Date = rep("2023-01-01", 5), Hour = c(0, 0, 1, 2, 3))
  for (field in c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")) {
    panel[[field]] <- rep(1e-9, 5)
  }
  stations <- data.frame(
    datetime = as.POSIXct(paste("2023-01-01", c("00:00:00", "00:00:00",
      "01:00:00", "02:00:00", "04:00:00", "00:00:00")), tz = "UTC"),
    pm25 = c(10, 30, NA, 40, 80, 100), station_code = c(1, 2, 1, 1, 1, 9))
  original_panel <- panel
  original_stations <- stations
  pm25 <- env$convert_and_add_pm25(panel)
  expected_pm25 <- 4 + 132.14 / 96.06
  for (city in c("bogota", "ciudad_mexico", "santiago", "sao_paulo")) {
    input <- stations
    if (city == "sao_paulo") input <- dplyr::filter(input, station_code %in% c(1, 2))
    time_column <- if (city == "santiago") "date2_hour" else "datetime"
    pm_column <- if (city == "santiago") "pm25_validated" else "pm25"
    if (city == "santiago") {
      input$date2_hour <- input$datetime
      input$pm25_validated <- input$pm25
    }
    joined <- env$combine_station_merra2_pm25(station_df = input,
      station_datetime_col = time_column, station_pm25_col = pm_column, merra2_df = pm25)
    mean_hour_zero <- if (city == "sao_paulo") 20 else 140 / 3
    expect_identical(joined$Hour, panel$Hour)
    expect_equal(as.character(joined$Date), panel$Date)
    expect_equal(joined$pm25_merra2, rep(expected_pm25, 5))
    expect_equal(joined$pm25_stations,
                 c(mean_hour_zero, mean_hour_zero, NaN, 40, NA))
  }
  expect_identical(panel, original_panel)
  expect_identical(stations, original_stations)
})

test_that("city aerosol extraction returns the hand-computed four-cell means", {
  root <- tempfile("aerosol-fixture-", tmpdir = here::here("tests/_cache"))
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- write_temporal_fixture(root, exact_values = TRUE)
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/merra2.R"), e)
  before <- list.files(root, recursive = TRUE)
  hashes <- tools::md5sum(file.path(root, before))
  result <- e$process_merra2_region_hourly(shapefile = fixture$geography,
    nc_files = fixture$nc_file, region_name = "toy", extraction_fun = "mean",
    parallel = FALSE)
  expect_equal(result$Hour, 0:23)
  expect_equal(result$Date, rep(as.Date("2023-01-01"), 24))
  fields <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  for (i in seq_along(fields)) {
    expect_equal(result[[fields[i]]], (2.5 + i + 0:23) * 2^-30, tolerance = 0)
  }
  expect_identical(list.files(root, recursive = TRUE), before)
  expect_identical(tools::md5sum(file.path(root, before)), hashes)
})

test_that("optional satellite joins use current hourly station summaries", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/merra2.R"), e)
  hourly <- data.frame(Date = as.Date("2023-01-01"), Hour = 0:3,
    pm25_stations = c(20, NA, 40, 80), n_reporting = c(2L, 0L, 1L, 1L))
  satellite <- data.frame(Date = rep("2023-01-01", 4), Hour = c(0, 0, 1, 4),
    pm25_estimate = c(5, 6, 7, 8))
  result <- e$join_station_hourly_merra2(hourly, satellite)
  expect_equal(result$pm25_stations, c(20, 20, NA, NA))
  expect_equal(result$pm25_merra2, satellite$pm25_estimate)
  expect_equal(result$n_reporting, c(2L, 2L, 0L, NA))
  expect_error(e$join_station_hourly_merra2(rbind(hourly, hourly[1, ]), satellite),
    "unique")
})

test_that("optional extraction precision is explicit for decimal raster values", {
  root <- tempfile("aerosol-precision-", tmpdir = here::here("tests/_cache"))
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- write_temporal_fixture(root)
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/merra2.R"), e)
  result <- e$process_merra2_region_hourly(fixture$geography, fixture$nc_file,
    region_name = "toy precision", extraction_fun = "mean", parallel = FALSE)
  # Installed extraction returns means on the float32 grid, despite the FLT8S fixture.
  # Check that boundary and its half-ULP rounding error against independent double means.
  values <- terra::values(terra::rast(fixture$nc_file))
  expected <- colMeans(values)
  fields <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  for (i in seq_along(fields)) {
    actual <- result[[fields[i]]]
    means <- expected[(i - 1L) * 24 + 1:24]
    float32 <- readBin(writeBin(actual, raw(), size = 4),
      what = "double", n = length(actual), size = 4)
    expect_identical(actual, float32)
    half_ulp <- 2^(floor(log2(abs(means))) - 24)
    double_rounding <- 4 * .Machine$double.eps * abs(means)
    expect_true(all(abs(actual - means) <= half_ulp + double_rounding))
  }
})
