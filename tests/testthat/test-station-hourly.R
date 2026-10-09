test_that("observed city-hours preserve availability, missing hours and leap years", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/station_temporal.R"), e)
  origin <- as.POSIXct("2024-01-01", tz = "UTC")
  readings <- data.frame(station = c("a", "b", "a", "a", "b", "a", "a"),
    datetime = origin + c(0, 0, 3600, 10800, 10800, 14400, -3600),
    pm25 = c(10, 30, 50, NA, NA, Inf, 999),
    year = c(rep(2024L, 6), 2023L))
  before <- readings
  result <- e$summarize_city_hourly_pm25(readings, 2024L)
  expect_equal(nrow(result), 8784L)
  expect_equal(result$pm25_stations[1:5], c(20, 50, NA, NA, NA))
  expect_identical(result$n_reporting[1:5], c(2L, 1L, 0L, 0L, 0L))
  expect_identical(result$Hour[1:25], c(0:23, 0L))
  expect_identical(diff(as.numeric(result$datetime)), rep(3600, 8783))
  expect_identical(readings, before)
  expect_error(e$summarize_city_hourly_pm25(rbind(readings, readings[1, ]), 2024L),
    "Duplicate station/timestamp")
  shifted <- readings; shifted$datetime[1] <- shifted$datetime[1] + 1800
  expect_error(e$summarize_city_hourly_pm25(shifted, 2024L), "whole hours")
  invalid <- readings; invalid$year[7] <- 2024L
  expect_error(e$summarize_city_hourly_pm25(invalid, 2024L), "selected year")

  path <- tempfile(fileext = ".parquet")
  output <- tempfile(fileext = ".parquet")
  on.exit(unlink(c(path, output)), add = TRUE)
  arrow::write_parquet(readings, path)
  streamed <- e$summarize_city_hourly_pm25(arrow::open_dataset(path), 2024L)
  expect_equal(streamed, result, tolerance = 0)
  expect_identical(e$write_station_hourly(streamed, output), output)
  expect_equal(as.data.frame(arrow::read_parquet(output)), result, tolerance = 0)
})

test_that("episodes include equality and end consistently at gaps and the final row", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/plot/timeseries_hourly.R"), e)
  input <- data.frame(Date = as.Date("2023-01-01"), Hour = 0:7,
    pm25_stations = c(50, 70, NA, 80, 40, 50, 51, 60))
  episodes <- e$compute_time_spans_above_target(input, "toy", target = "IT2")
  expect_identical(episodes$Hour, c(0L, 3L, 5L))
  expect_identical(episodes$time_span_above_target, c(2L, 1L, 3L))
  expect_identical(episodes$city, rep("toy", 3))
  expect_identical(e$compute_time_spans_above_target(input[c(1, 4), ], "toy",
    target = "IT2")$time_span_above_target, c(1L, 1L))
  expect_equal(nrow(e$compute_time_spans_above_target(input[5, ], "toy")), 0L)
  expect_error(e$compute_time_spans_above_target(rbind(input, input[1, ]), "toy"),
    "one valid observation")
})

test_that("manuscript temporal dependencies exclude optional satellite and old panels", {
  graph <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  expect_false(any(grepl("merra2|aerosol|balanced_|cities_shapefiles", graph$command)))
  expect_false(any(grepl("merra2|aerosol|temporal_series", graph$name)))
  expect_match(graph$command[graph$name == "temporal"], "prepare_station_hourly")
  artifacts <- read.csv(here::here("config/paper_artifacts.csv"))
  temporal <- artifacts[grepl("figure_station_temporal.R$", artifacts$producer_script), ]
  expect_equal(nrow(temporal), 5L)
})

test_that("hourly targets agree with direct calls and recreate only deleted checkpoints", {
  root <- tempfile("station-hourly-targets-", tmpdir = here::here("tests/_cache"))
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "observed.parquet")
  output <- file.path(root, "hourly.parquet")
  script <- file.path(root, "pipeline.R")
  store <- file.path(root, "store")
  readings <- data.frame(station = c("a", "b"),
    datetime = rep(as.POSIXct("2023-01-01", tz = "UTC"), 2),
    pm25 = c(10, 30), year = 2023L)
  arrow::write_parquet(readings, input)
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/station_temporal.R"), e)
  expected <- e$summarize_city_hourly_pm25(readings, 2023L)
  quote_text <- function(x) encodeString(x, quote = '"')
  graph <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  summary <- graph$command[graph$name == "bogota_station_hourly_summary"]
  writer <- graph$command[graph$name == "bogota_station_hourly_file"]
  destination <- str2lang(writer)
  destination$file <- output
  writeLines(c(paste0("source(", quote_text(here::here(
    "src/general_utilities/process/station_temporal.R")), ")"),
    "analysis_year <- 2023L", "list(",
    paste0("targets::tar_target(bogota_outliers, ", quote_text(input),
      ", format = 'file'),"),
    paste0("targets::tar_target(bogota_station_hourly_summary, ", summary, "),"),
    paste0("targets::tar_target(bogota_station_hourly_file, ",
      paste(deparse(destination), collapse = "\n"), ", format = 'file'))")), script)
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    x <- targets::tar_meta(fields = c("name", "time"), store = store)
    x[order(x$name), ]
  }
  make()
  expect_equal(targets::tar_read_raw("bogota_station_hourly_summary", store = store),
    expected, tolerance = 0)
  before <- metadata()
  make()
  expect_identical(metadata(), before)
  unlink(output)
  make()
  expect_true(file.exists(output))
  after <- metadata()
  expect_identical(after[after$name == "bogota_station_hourly_summary", ],
    before[before$name == "bogota_station_hourly_summary", ])
  readings$pm25[1] <- 70
  arrow::write_parquet(readings, input)
  make()
  expect_equal(targets::tar_read_raw("bogota_station_hourly_summary", store = store)$
    pm25_stations[1], 50)
})
