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
  fixture <- write_temporal_fixture(root)
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
    expect_equal(result[[fields[i]]] * 1e9, 2.5 + i + 0:23, tolerance = 1e-10)
  }
  expect_identical(list.files(root, recursive = TRUE), before)
  expect_identical(tools::md5sum(file.path(root, before)), hashes)
})

test_that("temporal recipes and targets agree, cache computations and track source files", {
  root <- tempfile("temporal-targets-", tmpdir = here::here("tests/_cache"))
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- write_temporal_fixture(root)
  script <- file.path(root, "pipeline.R")
  store <- file.path(root, "store")
  redirect <- function(x, route) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(source))) x$local <- TRUE
    if (identical(x[[1]], quote(here::here)) && length(x) > 2L &&
        identical(x[[2]], "data")) {
      x[[2]] <- if (identical(x[[3]], "raw")) root else file.path(root, route)
    }
    for (i in seq_along(x)[-1]) if (is.call(x[[i]])) x[[i]] <- redirect(x[[i]], route)
    x
  }
  e <- new.env(parent = globalenv())
  for (recipe in c("generate_panel_air_quality", "prepare_station_temporal")) {
    for (expr in parse(here::here("scripts/process_data", paste0(recipe, ".R")))) {
      eval(redirect(expr, "manual"), e)
      e$merra2_parallel <- FALSE
    }
  }
  expect_equal(e$series_bogota$pm25_stations[1], 140 / 3)
  expect_equal(e$series_sp$pm25_stations[1], 20)
  expect_equal(nrow(e$series_bogota), 24L)
  expect_true(is.nan(e$series_bogota$pm25_stations[2]))
  expect_true(all(is.na(e$series_bogota$pm25_stations[4:24])))

  graph <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  edges <- targets::tar_network(script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))$edges
  needed <- c("generate_panel_air_quality", "prepare_station_temporal")
  repeat {
    more <- union(needed, intersect(edges$from[edges$to %in% needed], graph$name))
    if (setequal(more, needed)) break
    needed <- more
  }
  quote_text <- function(x) encodeString(x, quote = '"')
  declarations <- vapply(needed, function(name) {
    row <- graph[graph$name == name, ]
    command <- redirect(str2lang(row$command), "targets")
    paste0("targets::tar_target_raw(", quote_text(name), ", quote(",
      paste(deparse(command), collapse = "\n"), "), format = ", quote_text(row$format), ")")
  }, character(1))
  write_pipeline <- function(extraction = "mean") {
    writeLines(c(paste0("source(", quote_text(here::here(
      "src/general_utilities/process/merra2.R")), ")"),
      paste0("source(", quote_text(here::here("config/analysis_settings.R")), ")"),
      "merra2_parallel <- FALSE", paste0("merra2_extraction_fun <- ", quote_text(extraction)),
      "list(", paste(declarations, collapse = ",\n"), ")"), script)
  }
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  read <- function(name) targets::tar_read_raw(name, store = store)
  metadata <- function() {
    x <- targets::tar_meta(fields = c("name", "time"), store = store)
    x[order(x$name), ]
  }
  write_pipeline()
  make()
  files <- c(read("generate_panel_air_quality"), read("prepare_station_temporal"))
  expect_length(files, 12L)
  for (file in files) {
    manual <- sub("/targets/", "/manual/", file, fixed = TRUE)
    expect_identical(readLines(file), readLines(manual))
  }
  before <- metadata()
  make()
  expect_identical(metadata(), before)
  unlink(c(read("bogota_aerosol_file"), read("bogota_temporal_series_file")))
  make()
  expect_true(all(file.exists(files)))
  after <- metadata()
  computed <- c("bogota_aerosol_panel", "bogota_temporal_series")
  expect_identical(after[after$name %in% computed, ], before[before$name %in% computed, ])

  # The temporal sample follows the supplied RDS file, and other cities stay cached.
  path <- read("bogota_temporal_station_input")
  data <- readRDS(path); data$pm25[1] <- 70; saveRDS(data, path)
  before <- metadata()
  make()
  expect_equal(read("bogota_temporal_series")$pm25_stations[1], 200 / 3)
  after <- metadata()
  expect_identical(after[startsWith(after$name, "cdmx_"), ],
                   before[startsWith(before$name, "cdmx_"), ])

  # An added day enters every city panel; removing it restores the original support.
  added <- file.path(dirname(fixture$nc_file), "toy.20230102.nc4")
  expect_true(file.copy(fixture$nc_file, added))
  make()
  expect_equal(nrow(read("bogota_aerosol_panel")), 48L)
  expect_equal(nrow(read("cdmx_temporal_series")), 48L)
  unlink(added)
  make()
  expect_equal(nrow(read("bogota_aerosol_panel")), 24L)
  # A .prj change is tracked even when its equivalent CRS leaves the result unchanged.
  prj <- list.files(read("bogota_temporal_geography_inputs"), pattern = "[.]prj$",
                    full.names = TRUE)
  before <- metadata()
  writeLines(c(readLines(prj, warn = FALSE), ""), prj)
  make()
  after <- metadata()
  expect_false(identical(after[after$name == "bogota_temporal_geography", ],
                        before[before$name == "bogota_temporal_geography", ]))
  mean <- read("bogota_aerosol_panel")$DUSMASS25
  write_pipeline(extraction = "sum")
  make()
  expect_equal(read("bogota_aerosol_panel")$DUSMASS25, mean * 4, tolerance = 1e-10)
})
