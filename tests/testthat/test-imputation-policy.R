# Read every result before removing the fixture's isolated output directory.
run_imputation_policy_fixture <- function(panel, pollutants = "pm10", id_col = "station") {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  root <- tempfile("imputation-policy-", tmpdir = cache)
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "input")
  arrow::write_dataset(panel, input, partitioning = "year")
  result <- impute_missing_hourly_ols(arrow_dir = input, out_dir = root,
    out_name = "toy", pollutants = pollutants, id_col = id_col,
    diag_year = 2023L, quiet = TRUE)
  actual <- as.data.frame(arrow::open_dataset(result$out_path))
  predictions <- arrow::read_parquet(result$diag_path)
  key <- function(x) paste(x[[id_col]], as.numeric(x$datetime))
  actual <- actual[match(key(panel), key(actual)), ]
  list(panel = actual, predictions = predictions, fits = result$per_station,
       counts = result$per_poll)
}

test_that("imputation starts separately for each station, pollutant and year", {
  panel <- expand.grid(hour = 0:8759, station = c("A", "B", "C", "EMPTY"),
                       year = 2022:2023, stringsAsFactors = FALSE)
  panel$datetime <- as.POSIXct(paste0(panel$year, "-01-01"), tz = "UTC") +
    panel$hour * 3600
  signal <- 20 + sin(panel$hour / 4) + cos(panel$hour / 11)
  panel$pm10 <- ifelse(panel$station == "B", signal, 10 + 2 * signal)
  panel$pm25 <- ifelse(panel$station == "B", signal, 4 + 1.5 * signal)
  december_10 <- 343L * 24L
  december_20 <- 353L * 24L
  december_23 <- 356L * 24L
  a <- panel$station == "A"
  c_station <- panel$station == "C"
  for (poll in c("pm10", "pm25")) {
    start <- if (poll == "pm10") december_10 else december_20
    panel[[poll]][a & panel$year == 2023 & panel$hour < start] <- NA_real_
    panel[[poll]][a & panel$year == 2022 & panel$hour < 8736L] <- NA_real_
    panel[[poll]][c_station & panel$hour < december_23] <- NA_real_
    panel[[poll]][panel$station == "EMPTY"] <- NA_real_
    panel[[poll]][a & panel$hour == 8759L] <- NA_real_
    panel[[poll]][a & panel$year == 2023 & panel$hour == start + 100L] <- NA_real_
  }
  panel$pm10[a & panel$hour == 1L] <- Inf
  panel$pm25[a & panel$hour == 2L] <- NaN
  # Reverse the rows: the first observation is determined by time, not input order.
  panel <- panel[nrow(panel):1, ]
  result <- run_imputation_policy_fixture(panel, c("pm10", "pm25"))
  expect_equal(nrow(result$fits), 16L)
  expect_equal(result$fits[station_id == "A" & year == 2022, status],
               rep("insufficient_observations", 2))
  expect_true(all(result$fits[station_id %in% c("B", "C"), status] ==
                    "complete_window"))
  expect_false(any(result$fits[station_id %in% c("B", "C", "EMPTY"), fitted]))
  expect_true(all(result$fits[station_id == "EMPTY", status] == "no_observations"))
  for (poll in c("pm10", "pm25")) {
    start <- if (poll == "pm10") december_10 else december_20
    row <- result$fits[station_id == "A" & year == 2023 & pollutant == poll]
    expect_equal(row$first_observed,
                 as.POSIXct("2023-01-01", tz = "UTC") + start * 3600)
    expect_equal(row$n_window_gaps, 2L)
    expect_equal(row$n_imputed, 2L)
    observed <- is.finite(panel[[poll]])
    expect_equal(result$panel[[poll]][observed], panel[[poll]][observed], tolerance = 0)
    before <- panel$station == "A" & panel$year == 2023 & panel$hour < start
    expect_true(all(is.na(result$panel[[poll]][before])))
    gaps <- panel$station == "A" & panel$year == 2023 &
      panel$hour %in% c(start + 100L, 8759L)
    x <- 20 + sin(panel$hour[gaps] / 4) + cos(panel$hour[gaps] / 11)
    expected <- if (poll == "pm10") 10 + 2 * x else 4 + 1.5 * x
    expect_equal(result$panel[[poll]][gaps], expected, tolerance = 1e-8)
    expect_equal(result$counts$n_imputed[result$counts$pollutant == poll], 2L)
  }
  expect_true(all(is.na(result$predictions$predicted[
    result$predictions$station_id %in% c("B", "C", "EMPTY") ])))
})

test_that("custom station identifiers align panels, diagnostics and both pollutants", {
  panel <- expand.grid(hour = 0:503, site = c(" Cañada El Hato ", "São Miguel"),
                       stringsAsFactors = FALSE)
  panel$datetime <- as.POSIXct("2023-01-01", tz = "UTC") + panel$hour * 3600
  panel$year <- 2023L
  signal <- 20 + sin(panel$hour / 4) + cos(panel$hour / 11)
  a <- panel$site == " Cañada El Hato "
  panel$pm10 <- ifelse(a, 10 + 2 * signal, signal)
  panel$pm25 <- ifelse(a, signal, 4 + 1.5 * signal)
  panel$pm10[a & panel$hour %in% c(100, 120)] <- NA_real_
  panel$pm25[!a & panel$hour %in% c(200, 220)] <- NA_real_
  panel <- panel[nrow(panel):1, ]

  result <- run_imputation_policy_fixture(panel, c("pm10", "pm25"), id_col = "site")
  expect_false("station" %in% names(result$panel))
  expect_identical(result$panel$site, panel$site)
  expect_setequal(unique(result$predictions$station_id),
                  c("CANADA EL HATO", "SAO MIGUEL"))
  expect_setequal(unique(result$fits$station_id), c("CANADA_EL_HATO", "SAO_MIGUEL"))

  # Match diagnostics using the shared station spelling, not the model's column names.
  expected_ids <- ifelse(panel$site == "São Miguel", "SAO MIGUEL", "CANADA EL HATO")
  for (poll in c("pm10", "pm25")) {
    observed <- is.finite(panel[[poll]])
    expect_equal(result$panel[[poll]][observed], panel[[poll]][observed], tolerance = 0)
    x <- 20 + sin(panel$hour[!observed] / 4) + cos(panel$hour[!observed] / 11)
    expected <- if (poll == "pm10") 10 + 2 * x else 4 + 1.5 * x
    expect_equal(result$panel[[poll]][!observed], expected, tolerance = 1e-8)
    diag <- result$predictions[result$predictions$pollutant == poll, ]
    idx <- match(paste(expected_ids, as.numeric(panel$datetime)),
                 paste(diag$station_id, as.numeric(diag$datetime)))
    expect_false(anyNA(idx))
    expect_equal(diag$observed[idx], panel[[poll]], tolerance = 0)
    expect_equal(diag$predicted[idx[!observed]], expected, tolerance = 1e-8)
  }
})

test_that("station fitting order does not feed imputed readings into other models", {
  panel <- expand.grid(hour = 0:743, station = c("A", "B", "C"),
                       stringsAsFactors = FALSE)
  panel$datetime <- as.POSIXct("2023-01-01", tz = "UTC") + panel$hour * 3600
  panel$year <- 2023L
  x <- sin(panel$hour / 4) + cos(panel$hour / 11)
  z <- sin(panel$hour / 7) + cos(panel$hour / 17)
  panel$pm10 <- ifelse(panel$station == "A", 30 + 5 * x + 3 * z,
                       ifelse(panel$station == "B", 20 + 2 * x - z, 25 + x + 4 * z))
  panel$pm10 <- panel$pm10 + sin(panel$hour / 3)
  panel$pm10[panel$station == "A" & panel$hour %in% c(200, 300)] <- NA_real_
  panel$pm10[panel$station == "B" & panel$hour %in% c(200, 400)] <- NA_real_
  before <- run_imputation_policy_fixture(panel)
  expect_equal(before$fits[station_id %in% c("A", "B"), n_imputed], c(2L, 2L))

  # Rename the stations to reverse fitting order; also reverse the input rows.
  panel$station <- c(A = "Z", B = "Y", C = "X")[panel$station]
  panel <- panel[nrow(panel):1, ]
  after <- run_imputation_policy_fixture(panel)
  expect_equal(after$panel$pm10[nrow(panel):1], before$panel$pm10, tolerance = 1e-8)
  expect_equal(after$counts, before$counts)
  expect_equal(after$fits$n_imputed[3:1], before$fits$n_imputed)
})

test_that("constant calendar factors still restrict the prediction hours", {
  panel <- expand.grid(hour = 0:8759, station = c("A", "B"))
  panel$datetime <- as.POSIXct("2023-01-01", tz = "UTC") + panel$hour * 3600
  panel$year <- 2023L
  signal <- 20 + sin(panel$hour / 4)
  panel$pm10 <- ifelse(panel$station == "A", 2 * signal + 10, signal)
  # A reports only at noon on Mondays: 52 possible readings, 50 used for fitting.
  monday_noon <- format(panel$datetime, "%u %H", tz = "UTC") == "1 12"
  a <- panel$station == "A"
  monday_hours <- panel$hour[a & monday_noon]
  gaps <- monday_hours[c(10, 40)]
  panel$pm10[a & (!monday_noon | panel$hour %in% gaps)] <- NA_real_
  result <- run_imputation_policy_fixture(panel)
  expect_equal(result$fits[station_id == "A", n_imputed], 2L)
  expect_true(all(is.na(result$panel$pm10[a & !monday_noon])))
  expect_equal(result$panel$pm10[a & panel$hour %in% gaps],
               2 * signal[a & panel$hour %in% gaps] + 10, tolerance = 1e-8)

  # With observations only in April-June, trailing July-December stays unsupported.
  daytime <- format(panel$datetime, "%H", tz = "UTC") == "12"
  months <- as.integer(format(panel$datetime, "%m", tz = "UTC"))
  panel$pm10 <- ifelse(a, 2 * signal + 10, signal)
  gap <- panel$datetime == as.POSIXct("2023-05-15 12:00:00", tz = "UTC")
  panel$pm10[a & (!daytime | !months %in% 4:6 | gap)] <- NA_real_
  result <- run_imputation_policy_fixture(panel)
  expect_equal(result$fits[station_id == "A", n_imputed], 1L)
  expect_true(all(is.na(result$panel$pm10[a & months > 6])))
  expect_equal(result$panel$pm10[a & gap], 2 * signal[a & gap] + 10,
               tolerance = 1e-8)

  panel$pm10[a & gap] <- 2 * signal[a & gap] + 10
  result <- run_imputation_policy_fixture(panel)
  expect_equal(result$fits[station_id == "A", status], "unsupported_calendar")
  expect_false(any(result$fits$fitted))
})

test_that("unobserved weekday-hour combinations are not filled", {
  # The former ten-day fixture has no observed counterpart for either missing cell.
  panel <- expand.grid(hour = 0:239, station = c("A", "B"))
  panel$datetime <- as.POSIXct("2023-01-01", tz = "UTC") + panel$hour * 3600
  panel$year <- 2023L
  panel$pm10 <- 20 + sin(panel$hour / 4) + (panel$station == "B") * 10
  gaps <- panel$station == "A" & panel$hour %in% c(100, 120)
  panel$pm10[gaps] <- NA_real_
  result <- run_imputation_policy_fixture(panel)
  expect_true(all(is.na(result$panel$pm10[gaps])))
  expect_equal(result$fits[station_id == "A", n_supported_gaps], 2L)
  expect_equal(result$fits[station_id == "A", status], "no_estimable_gaps")
})

test_that("unsupported predictor combinations stay missing", {
  panel <- expand.grid(hour = 0:743, station = c("A", "B", "C"))
  panel$datetime <- as.POSIXct("2023-12-01", tz = "UTC") + panel$hour * 3600
  panel$year <- 2023L
  signal <- 20 + sin(panel$hour / 4)
  panel$pm10 <- ifelse(panel$station == "A", 2 * signal + 10, signal)
  panel$pm10[panel$station == "A" & panel$hour %in% c(100, 200)] <- NA_real_
  # B and C agree throughout training; their disagreement at hour 100 is unsupported.
  panel$pm10[panel$station == "C" & panel$hour == 100] <- 100
  result <- run_imputation_policy_fixture(panel)
  expect_true(is.na(result$panel$pm10[panel$station == "A" & panel$hour == 100]))
  expect_equal(result$panel$pm10[panel$station == "A" & panel$hour == 200],
               2 * (20 + sin(200 / 4)) + 10, tolerance = 1e-8)
  expect_equal(result$fits[station_id == "A", status], "partially_imputed")
  expect_equal(result$fits[station_id == "A", n_imputed], 1L)
})

test_that("saturated models and series without usable neighbours are skipped", {
  panel <- expand.grid(hour = 0:743, station = c("A", "B"))
  panel$datetime <- as.POSIXct("2023-12-01", tz = "UTC") + panel$hour * 3600
  panel$year <- 2023L
  panel$pm10 <- 20 + sin(panel$hour / 4) + (panel$station == "A") * 10
  panel$pm10[panel$station == "A" & panel$hour >= 50] <- NA_real_
  result <- run_imputation_policy_fixture(panel)
  expect_equal(result$fits[station_id == "A", status], "no_residual_df")
  expect_equal(result$fits[station_id == "A", residual_df], 0L)
  expect_true(all(is.na(result$panel$pm10[panel$station == "A" & panel$hour >= 50])))

  panel$pm10 <- 20 + sin(panel$hour / 4)
  panel$pm10[panel$station == "A" & panel$hour == 200] <- NA_real_
  panel$pm10[panel$station == "B"] <- 20
  result <- run_imputation_policy_fixture(panel)
  expect_equal(result$fits[station_id == "A", status], "no_varying_neighbors")
  expect_false(any(result$fits$fitted))
})


test_that("imputation preserves original screening and identifies predictions", {
  panel <- expand.grid(hour = 0:503, station = c("A", "B"))
  panel$datetime <- as.POSIXct("2023-01-01", tz = "UTC") + panel$hour * 3600
  panel$year <- 2023L
  signal <- 20 + sin(panel$hour / 4) + cos(panel$hour / 11)
  panel$pm25 <- ifelse(panel$station == "A", 10 + 2 * signal, signal)
  gap <- panel$station == "A" & panel$hour == 100
  panel$pm25[gap] <- 79999
  panel$pm25_source_status <- "raw_unvalidated"
  screened <- screen_pollution_quality(panel, "pm25")
  screened[, pm25_outlier_reason := ifelse(pm25_screen_reason == 0L, 0L, NA_integer_)]
  result <- run_imputation_policy_fixture(as.data.frame(screened), "pm25")
  expected <- 10 + 2 * signal[gap]
  expect_equal(result$panel$pm25[gap], expected, tolerance = 1e-8)
  expect_identical(result$panel$pm25_imputed_from[gap], "OLS_imputed")
  expect_equal(result$panel$pm25_original[gap], 79999)
  expect_identical(result$panel$pm25_screen_reason[gap], 1L)
  expect_true(is.na(result$panel$pm25_outlier_reason[gap]))
  expect_identical(result$panel$pm25_source_validation[gap], "raw_unvalidated")
  observed <- !gap
  expect_equal(result$panel$pm25[observed], panel$pm25[observed], tolerance = 0)
  expect_true(all(is.na(result$panel$pm25_imputed_from[observed])))
})
