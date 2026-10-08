test_that("particulate bounds retain equality and preserve original concentrations", {
  panel <- data.table::data.table(station = "s", datetime = as.POSIXct(
    "2023-01-01", tz = "UTC") + 0:6 * 3600,
    pm25 = c(0, -1, NA, 500, 501, 79999, Inf),
    pm10 = c(0, -1, NA, 1000, 1001, 999, Inf))
  result <- screen_pollution_quality(panel)
  expect_identical(result$pm25_input, panel$pm25)
  expect_equal(result$pm25, c(0, NA, NA, 500, NA, NA, NA))
  expect_equal(result$pm10, c(0, NA, NA, 1000, NA, 999, NA))
  expect_identical(result$pm25_qa_status,
    c("not_flagged", "excluded", "missing", "not_flagged", "pending_review",
      "pending_review", "excluded"))
  expect_identical(panel$pm25[6], 79999)
  expect_equal(screen_pollution_quality(panel, upper_bounds = NULL)$pm25[6], 79999)
  expect_equal(screen_pollution_quality(panel,
    upper_bounds = c(pm25 = Inf, pm10 = 900))$pm25[6], 79999)
  expect_true(is.na(screen_pollution_quality(panel,
    upper_bounds = c(pm25 = 499))$pm25[4]))
  expect_error(screen_pollution_quality(panel, upper_bounds = c(pm25 = -1)), "bounds")
})

test_that("independent logical gates cannot bypass a concentration hold", {
  panel <- data.table::data.table(station = "s", datetime = as.POSIXct(
    "2023-01-01", tz = "UTC") + 0:2 * 3600, pm25 = c(79999, 10, 20),
    pm10 = c(40, 50, 60), gate25 = c(TRUE, NA, FALSE), gate10 = c(FALSE, TRUE, NA))
  mapping <- c(pm25 = "gate25", pm10 = "gate10")
  result <- screen_pollution_quality(panel, eligibility_cols = mapping)
  expect_identical(result$pm25_qa_eligible, c(FALSE, FALSE, FALSE))
  expect_identical(result$pm10_qa_eligible, c(FALSE, TRUE, FALSE))
  expect_equal(result$pm10, c(NA, 50, NA))
  expect_error(screen_pollution_quality(panel,
    eligibility_cols = c(pm25 = "absent")), "must exist")
  panel$gate25 <- c(1, 0, NA)
  expect_error(screen_pollution_quality(panel, eligibility_cols = mapping), "logical")
})

test_that("documented decisions are bound to exact original inputs", {
  panel <- data.table::data.table(station = "Calpúl", datetime = as.POSIXct(
    "2023-01-01", tz = "UTC") + 0:1 * 3600, pm25 = c(79999, 30), input_id = "hash")
  reviews <- data.frame(station = panel$station, datetime = panel$datetime,
    pollutant = "pm25", input_id = "hash", value = panel$pm25,
    decision = c("retain", "exclude"), evidence = "fixture operating record",
    reviewer = "fixture reviewer", review_date = "2026-10-06")
  result <- screen_pollution_quality(panel, review_decisions = reviews)
  expect_equal(result$pm25, c(79999, NA))
  expect_equal(result$pm25_qa_status, c("reviewed_retained", "excluded"))
  changed <- reviews; changed$input_id[1] <- "stale"
  expect_error(screen_pollution_quality(panel, review_decisions = changed), "identity")
  changed <- reviews; changed$value[1] <- 80000
  expect_error(screen_pollution_quality(panel, review_decisions = changed), "value")
  changed <- reviews; changed$evidence[1] <- ""
  expect_error(screen_pollution_quality(panel, review_decisions = changed), "complete")
  expect_error(screen_pollution_quality(panel,
    review_decisions = rbind(reviews, reviews[1, ])), "conflicting")
  panel$gate <- FALSE
  expect_false(screen_pollution_quality(panel, eligibility_cols = c(pm25 = "gate"),
    review_decisions = reviews)$pm25_qa_eligible[1])
})

test_that("held readings and boundary donors do not enter statistical benchmarks", {
  root <- tempfile("quality-cleaner-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "input")
  panel <- data.table::CJ(station = c("Calpúl", "peer"), datetime = as.POSIXct(
    "2022-12-31 00:00:00", tz = "UTC") + 0:47 * 3600)
  panel[, pm25 := ifelse(station == "Calpúl", 79999, 20)]
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
  expect_true(all(held$pm25_input == 79999))
  expect_true(all(!held$pm25_use & held$pm25_outlier_reason == 0L))
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
  columns <- c("station", "pm25", "pm25_outlier", "pm25_outlier_reason")
  expect_equal(outputs[[1]][, ..columns], reference[, ..columns])
  summary <- summarize_pollution_quality(run("summary", c(pm25 = 500)), "pm25")
  expect_equal(sum(summary$hours), nrow(panel))
  expect_equal(sum(summary[qa_status == "pending_review", hours]), 48)
  sentinel <- file.path(root, "protected_clean"); dir.create(sentinel)
  writeLines("keep", file.path(sentinel, "sentinel"))
  expect_error(detect_pollution_outliers(input, dist, root, "protected",
    eligibility_cols = c(pm25 = "absent")), "must exist")
  expect_true(file.exists(file.path(sentinel, "sentinel")))
  expect_error(detect_pollution_outliers(input, dist, root, "protected",
    eligibility_cols = c(pm25 = "absent"), overwrite = FALSE), "must exist")
  identity <- pollution_input_identities(input)[year == 2023L, input_id]
  review <- data.frame(station = "Calpúl", datetime = as.POSIXct("2023-01-01", tz = "UTC"),
    pollutant = "pm25", input_id = identity, value = 79999, decision = "retain",
    evidence = "fixture original label and partition", reviewer = "fixture reviewer",
    review_date = "2026-10-06")
  retained_path <- detect_pollution_outliers(input, dist, root, "retained",
    pollutants = "pm25", review_decisions = review, quiet = TRUE)
  retained <- data.table::as.data.table(dplyr::collect(arrow::open_dataset(retained_path)))
  expect_equal(retained[pm25_qa_status == "reviewed_retained", pm25], 79999)
  review$input_id <- "stale"
  expect_error(detect_pollution_outliers(input, dist, root, "protected",
    pollutants = "pm25", review_decisions = review, quiet = TRUE), "identity")
  expect_true(file.exists(file.path(sentinel, "sentinel")))
  records <- collect_pollution_review_records(retained_path, "pm25")
  expect_equal(nrow(records), 48)
  expect_true(all(records$value == 79999))
  expect_true(all(records$station == "Calpúl"))
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
})
