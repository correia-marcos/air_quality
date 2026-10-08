test_that("particulate bounds retain equality and preserve original concentrations", {
  panel <- data.table::data.table(station = "s", datetime = as.POSIXct(
    "2023-01-01", tz = "UTC") + 0:6 * 3600,
    pm25 = c(0, -1, NA, 500, 501, 79999, Inf),
    pm10 = c(0, -1, NA, 1000, 1001, 999, Inf))
  result <- screen_pollution_quality(panel, upper_bounds = c(pm25 = 500, pm10 = 1000))
  expect_identical(result$pm25_original, panel$pm25)
  expect_equal(result$pm25, c(0, NA, NA, 500, NA, NA, NA))
  expect_equal(result$pm10, c(0, NA, NA, 1000, NA, 999, NA))
  expect_identical(result$pm25_screen_reason, c(0L, 2L, 3L, 0L, 1L, 1L, 3L))
  expect_identical(result$pm25_source_validation, rep("unknown", nrow(panel)))
  expect_identical(panel$pm25[6], 79999)
  expect_equal(screen_pollution_quality(panel, upper_bounds = NULL)$pm25[6], 79999)
  expect_equal(screen_pollution_quality(panel,
    upper_bounds = c(pm25 = Inf, pm10 = 900))$pm25[6], 79999)
  expect_true(is.na(screen_pollution_quality(panel,
    upper_bounds = c(pm25 = 499))$pm25[4]))
  expect_error(screen_pollution_quality(panel, upper_bounds = c(pm25 = -1)), "bounds")
})

test_that("approved broad defaults agree across settings and both functions", {
  settings <- new.env(parent = globalenv())
  sys.source(here::here("config", "analysis_settings.R"), envir = settings)
  bounds <- c(pm25 = 2000, pm10 = 6000)
  expect_identical(settings$pollution_upper_bounds, bounds)
  expect_identical(eval(formals(screen_pollution_quality)$upper_bounds), bounds)
  expect_identical(eval(formals(detect_pollution_outliers)$upper_bounds), bounds)
  panel <- data.frame(pm25 = c(915, 2000, 2001, 79999),
                      pm10 = c(5337.6, 6000, 6001, 79999))
  result <- screen_pollution_quality(panel)
  expect_equal(result$pm25, c(915, 2000, NA, NA))
  expect_equal(result$pm10, c(5337.6, 6000, NA, NA))
  expect_identical(result$pm25_original, panel$pm25)
  expect_identical(result$pm10_original, panel$pm10)
})

test_that("screening separates negative and nonfinite input from source validation", {
  panel <- data.frame(pm25 = c(-1, -Inf, NaN, Inf, 0, 79999),
    pm10 = c(10, 10, 10, 10, 10, 10),
    pm25_source_status = c("validated", "raw_unvalidated", NA, "mixed", "unknown",
                          "validated"))
  result <- screen_pollution_quality(panel)
  expect_identical(result$pm25_original, panel$pm25)
  expect_identical(result$pm25_screen_reason, c(2L, 3L, 3L, 3L, 0L, 1L))
  expect_identical(result$pm10_screen_reason, rep(0L, nrow(panel)))
  expect_identical(result$pm25_source_validation,
    c("validated", "raw_unvalidated", "unknown", "mixed", "unknown", "validated"))
  expect_false("pm25_source_status" %in% names(result))
  expect_error(screen_pollution_quality(panel, review_decisions = data.frame()),
               "unused argument")
})

test_that("held readings and boundary donors do not enter statistical benchmarks", {
  root <- tempfile("quality-cleaner-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "input")
  panel <- data.table::CJ(station = c("Calpúl", "peer"), datetime = as.POSIXct(
    "2022-12-31 00:00:00", tz = "UTC") + 0:47 * 3600)
  panel[, pm25 := ifelse(station == "Calpúl", 79999, 20)]
  panel[, pm25_source_status := ifelse(station == "Calpúl", "raw_unvalidated", "mixed")]
  panel[, pm25_source_ids := ifelse(station == "Calpúl", "raw-id", "raw-id;valid-id")]
  panel[, year := as.integer(format(datetime, "%Y", tz = "UTC"))]
  table <- arrow::Table$create(panel)$SetColumn(1L,
    arrow::field("datetime", arrow::timestamp("us")),
    arrow::ChunkedArray$create(panel$datetime, type = arrow::timestamp("us")))
  arrow::write_dataset(table, input, partitioning = "year")
  dist <- file.path(root, "dist.parquet")
  arrow::write_parquet(data.frame(station_from = c("Calpúl", "peer"),
    station_to = c("peer", "Calpúl"), distance_km = 1), dist)
  run <- function(name, bounds) detect_pollution_outliers(input, dist, root, name,
    upper_bounds = bounds, pollutants = "pm25", quiet = TRUE)
  old_tz <- Sys.getenv("TZ", unset = NA_character_)
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz), add = TRUE)
  outputs <- lapply(c("UTC", "America/Sao_Paulo", "America/Mexico_City"), function(tz) {
    Sys.setenv(TZ = tz)
    path <- run(gsub("/", "_", tz), c(pm25 = 500))
    expect_true(arrow::open_dataset(path)$schema$GetFieldByName("datetime")$type$Equals(
      arrow::open_dataset(input)$schema$GetFieldByName("datetime")$type))
    result <- data.table::as.data.table(dplyr::collect(arrow::open_dataset(path)))
    data.table::setorder(result, station, datetime)
    result
  })
  expect_equal(outputs[[1]], outputs[[2]])
  expect_equal(outputs[[1]], outputs[[3]])
  held <- outputs[[1]][station == normalize_station("Calpúl")]
  expect_true(all(is.na(held$pm25)))
  expect_true(all(held$pm25_original == 79999))
  expect_true(all(held$pm25_screen_reason == 1L))
  expect_true(all(is.na(held$pm25_outlier_reason)))
  expect_setequal(grep("^pm25", names(held), value = TRUE),
    c("pm25_original", "pm25_source_validation", "pm25_screen_reason",
      "pm25_outlier_reason", "pm25"))
  disabled <- data.table::as.data.table(dplyr::collect(arrow::open_dataset(
    run("disabled", NULL))))
  expect_true(all(disabled[station == normalize_station("Calpúl"), pm25] == 79999))
  expected <- screen_pollution_quality(panel, pollutants = "pm25")
  masked <- file.path(root, "masked")
  arrow::write_dataset(expected[, .(station, datetime, pm25, year)], masked,
                       partitioning = "year")
  masked_path <- detect_pollution_outliers(masked, dist, root, "masked_clean",
    upper_bounds = NULL, pollutants = "pm25", quiet = TRUE)
  reference <- data.table::as.data.table(dplyr::collect(arrow::open_dataset(masked_path)))
  data.table::setorder(reference, station, datetime)
  expect_equal(as.numeric(outputs[[1]]$datetime), as.numeric(reference$datetime))
  columns <- c("station", "pm25", "pm25_outlier_reason")
  expect_equal(outputs[[1]][, ..columns], reference[, ..columns])
  summary <- summarize_pollution_quality(run("summary", c(pm25 = 500)), "pm25")
  expect_equal(sum(summary$hours), nrow(panel))
  expect_equal(sum(summary[screen_reason == 1L, hours]), 48)
  sentinel <- file.path(root, "protected_clean"); dir.create(sentinel)
  writeLines("keep", file.path(sentinel, "sentinel"))
  expect_error(detect_pollution_outliers(input, dist, root, "protected",
    upper_bounds = c(pm25 = -1)), "bounds")
  expect_true(file.exists(file.path(sentinel, "sentinel")))
  expect_error(detect_pollution_outliers(input, dist, root, "protected",
    upper_bounds = c(pm25 = -1), overwrite = FALSE), "bounds")
  path <- run("audit", c(pm25 = 500))
  records <- collect_pollution_screening_records(path, "pm25")
  expect_equal(nrow(records), 48)
  expect_true(all(records$value == 79999))
  expect_true(all(records$station == "Calpúl"))
  expect_true(all(records$source_ids == "raw-id"))
  links <- dplyr::collect(arrow::open_dataset(
    file.path(path, "_audit", "source_contributions")))
  expect_equal(nrow(links), nrow(panel))
  expect_true(all(links$source_ids[links$station == "PEER"] == "raw-id;valid-id"))
  expect_true(all(links$source_validation[links$station == "PEER"] == "mixed"))
  expect_equal(nrow(dplyr::collect(arrow::open_dataset(path))), nrow(panel))
  counts <- arrow::read_parquet(file.path(path, "_audit",
    "station_month_diagnostics.parquet"))
  expect_equal(nrow(counts), 4L)
  expect_equal(counts$n_missing_temporal_sd, rep(0L, 4))
  expect_equal(data.table::fread(file.path(path, "_audit", "input_partitions.csv")),
               pollution_input_identities(input))

})

test_that("timestamp serialization preserves integer-backed source clocks", {
  root <- tempfile(fileext = ".parquet")
  on.exit(unlink(root), add = TRUE)
  clock <- structure(c(1672531200L, 1672534800L), class = c("POSIXct", "POSIXt"),
                     tzone = "UTC")
  materialized <- as.POSIXct(as.numeric(clock), origin = "1970-01-01", tz = "UTC")
  arrow::write_parquet(data.frame(datetime = materialized), root)
  restored <- arrow::read_parquet(root)$datetime
  expect_equal(as.numeric(restored), as.numeric(clock))
  expect_equal(format(restored, tz = "UTC"), c("2023-01-01 00:00:00",
                                               "2023-01-01 01:00:00"))
  audit <- tempfile("source-audit-")
  on.exit(unlink(audit, recursive = TRUE), add = TRUE)
  panel <- data.table::data.table(station = "A", station_original = "A",
    datetime = clock, year = 2023L, input_id = "fixture", pm25 = 10,
    pm25_source_validation = "mixed", pm25_source_ids = "raw;validated")
  for (label in c("missing_temporal", "zero_temporal", "missing_spatial", "zero_spatial")) {
    panel[, (paste0("pm25_n_", label, "_sd")) := 0L]
  }
  write_pollution_audit_partition(panel, audit, "pm25")
  links <- arrow::read_parquet(file.path(audit, "source_contributions", "year=2023",
                                        "data.parquet"))
  expect_equal(as.numeric(links$datetime), as.numeric(clock))
  expect_identical(links$source_ids, rep("raw;validated", 2))
})
