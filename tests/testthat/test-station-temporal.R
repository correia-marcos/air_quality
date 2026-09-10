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
  for (city in c("bogota", "ciudad_mexico", "santiago", "sao_paulo")) {
    env[[paste0(city, "_panel")]] <- panel
    env[[paste0(city, "_stations")]] <- stations
  }
  env$santiago_stations$date2_hour <- stations$datetime
  env$santiago_stations$pm25_validated <- stations$pm25
  env$stations_in_sp_metro <- data.frame(sttn_cd = c(1, 2))
  output <- tempfile("temporal-fixture-")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  env$outdir_pm25 <- file.path(output, "pm25")
  env$outdir_m2_stations <- file.path(output, "joined")
  dir.create(env$outdir_pm25, recursive = TRUE)

  expressions <- parse(here::here("scripts", "process_data",
                                  "prepare_station_temporal.R"))
  first_conversion <- which(vapply(expressions, function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("bogota_pm25"))
  }, logical(1)))[1]
  for (expression in expressions[first_conversion:length(expressions)]) {
    eval(expression, env)
  }

  expected_pm25 <- 4 + 132.14 / 96.06
  for (city in c("bogota", "ciudad_mexico", "santiago", "sao_paulo")) {
    joined <- env[[paste0(city, "_pollution")]]
    mean_hour_zero <- if (city == "sao_paulo") 20 else 140 / 3
    expect_identical(joined$Hour, panel$Hour)
    expect_equal(as.character(joined$Date), panel$Date)
    expect_equal(joined$pm25_merra2, rep(expected_pm25, 5))
    expect_equal(joined$pm25_stations,
                 c(mean_hour_zero, mean_hour_zero, NaN, 40, NA))
    saved <- read.csv(file.path(env$outdir_m2_stations,
                                paste0(city, "_pm25_stations_merra2.csv")))
    expect_equal(saved$Hour, joined$Hour)
    expect_equal(saved$pm25_stations, replace(joined$pm25_stations,
                                             is.nan(joined$pm25_stations), NA))
  }
})
