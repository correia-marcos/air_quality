test_that("missingness describes stored rows and saves all dimensions separately", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/diagnostics.R"), e)
  root <- tempfile("missingness-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  panel <- file.path(root, "panel"); dir.create(panel)
  readings <- data.frame(station = c("A", "A", "B", "B"),
    datetime = as.POSIXct(c("2023-06-15 12:00:00", "2023-06-15 14:00:00",
      "2023-06-15 12:00:00", "2022-06-15 12:00:00"), tz = "UTC"),
    year = c(2023L, 2023L, 2023L, 2022L),
    pm10 = c(10, NA, 40, 90), pm25 = c(NA, 5, 8, 10))
  arrow::write_parquet(readings, file.path(panel, "readings.parquet"))
  before <- list.files(root, recursive = TRUE)
  tables <- e$compute_missing_proportions(panel, pollutants = c("pm10", "pm25"),
    dims = c("station", "month", "hour", "day_of_week"), year_filter = 2023L,
    quiet = TRUE)
  expect_identical(list.files(root, recursive = TRUE), before)
  expect_equal(tables$station[station == "A", pm10_missing_pct], 50)
  expect_equal(tables$station[station == "A", total_hrs], 2)
  expect_equal(tables$month$pm10_missing_pct, 100 / 3)
  expect_equal(sum(tables$hour$total_hrs), 3)
  original <- lapply(tables, data.table::copy)
  files <- e$write_missing_proportions(tables, file.path(root, "separate"),
                                       "toy_raw", quiet = TRUE)
  expect_identical(tables, original)
  expect_length(files, 4L)
  for (dimension in names(files)) {
    expect_equal(as.data.frame(arrow::read_parquet(files[[dimension]])),
                 as.data.frame(tables[[dimension]]))
  }
  combined <- e$compute_missing_proportions(panel, pollutants = c("pm10", "pm25"),
    dims = names(tables), year_filter = 2023L, out_dir = file.path(root, "combined"),
    out_name = "toy_raw", quiet = TRUE)
  expect_equal(combined, tables)
  for (file in files) {
    expect_equal(as.data.frame(arrow::read_parquet(file)),
      as.data.frame(arrow::read_parquet(file.path(root, "combined", basename(file)))))
  }
  expect_error(suppressWarnings(e$write_missing_proportions(tables, files[1], "toy")))
})

test_that("descriptive targets track city inputs and regenerate individual checkpoints", {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  root <- tempfile("descriptives-", tmpdir = cache); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "pipeline.R")
  store <- file.path(root, "store")
  output <- file.path(root, "output")
  cities <- c("bogota", "cdmx", "santiago", "sao_paulo")
  contexts <- c("bogota_2018", "cdmx_2020", "santiago_2017", "sao_paulo_2010")
  micro <- c("census_2018_metro_individual.parquet", "census_metro_individual_2020.parquet",
             "census_individual_2017.parquet", "census_sp_individual_2010.parquet")
  readings <- data.frame(station = c("A", "A", "B", "B"),
    datetime = as.POSIXct(c("2023-06-15 12:00:00", "2023-06-15 14:00:00",
      "2023-06-15 12:00:00", "2022-06-15 12:00:00"), tz = "UTC"),
    year = c(2023L, 2023L, 2023L, 2022L),
    pm10 = c(10, NA, 40, 90), pm25 = c(NA, 5, 8, 10))
  census <- data.frame(geo_id = c("a", "b"), person_weight = c(2, 1),
                       educ_years = c(5, 15))
  distances <- data.frame(geo_id = c("a", "b"), station = c("A", "B"),
                           distance_km = c(1, 2))
  quote_text <- function(x) encodeString(x, quote = '"')
  file_target <- function(name, path) {
    paste0("targets::tar_target_raw(", quote_text(name), ", quote(", quote_text(path),
           "), format = 'file')")
  }
  declarations <- character()
  for (i in seq_along(cities)) {
    dir <- file.path(root, "alternate-inputs", cities[i])
    raw <- file.path(dir, paste0(cities[i], "_metro_dataset"))
    clean <- file.path(dir, paste0(cities[i], "_metro_clean"))
    dir.create(raw, recursive = TRUE); dir.create(clean)
    arrow::write_parquet(readings, file.path(raw, "readings.parquet"))
    arrow::write_parquet(readings, file.path(clean, "readings.parquet"))
    arrow::write_parquet(census, file.path(dir, micro[i]))
    arrow::write_parquet(distances, file.path(dir, "matrix_geo_station_distances.parquet"))
    declarations <- c(declarations,
      file_target(paste0(cities[i], "_pollution_parquet"), raw),
      file_target(paste0(cities[i], "_outliers"), clean),
      file_target(paste0(cities[i], "_census"), file.path(dir, micro[i])),
      file_target(paste0(contexts[i], "_distances"),
                  file.path(dir, "matrix_geo_station_distances.parquet")))
  }
  manifest <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  city_summaries <- paste0("^(bogota|cdmx|santiago|sao_paulo)_(missing_(raw|clean)",
    "(_files)?|station_counts|who_exceedances|threshold_exceedances|",
    "quintile_availability|census_summary)$")
  combined <- c("station_counts_summary", "who_exceedance_summary",
    "threshold_exceedance_summary", "quintile_availability_summary", "census_summary",
    "station_counts_files", "who_exceedance_file", "threshold_exceedance_files",
    "quintile_availability_files", "census_summary_files", "missing_dimension_files",
    "compute_descriptive_tables")
  stages <- manifest$name[grepl(city_summaries, manifest$name) |
                            manifest$name %in% combined]
  redirect <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(here::here)) && length(x) >= 4L &&
        identical(x[[2]], "data") && identical(x[[3]], "processed")) {
      return(as.call(c(list(quote(file.path), output), as.list(x)[-(1:3)])))
    }
    for (i in seq_along(x)[-1]) if (is.call(x[[i]])) x[[i]] <- redirect(x[[i]])
    x
  }
  for (stage in stages) {
    row <- manifest[manifest$name == stage, ]
    command <- redirect(str2lang(row$command))
    declarations <- c(declarations, paste0("targets::tar_target_raw(", quote_text(stage),
      ", quote(", paste(deparse(command), collapse = "\n"), "), format = ",
      quote_text(row$format), ", packages = 'data.table')"))
  }
  modules <- here::here("src/general_utilities", c("base_utils.R", "process/geo_ids.R",
    "process/diagnostics.R", "process/station_socio.R", "process/exposure_regressions.R"))
  write_pipeline <- function(year = 2023L, alter_counts = FALSE) {
    writeLines(c(paste0("source(", quote_text(modules), ")"),
      paste0("source(", quote_text(here::here("config/analysis_settings.R")), ")"),
      paste0("analysis_year <- ", year, "L"),
      if (alter_counts) paste0("body(count_stations_reporting) <- ",
        "substitute({ value <- ORIGINAL; value$pm10 <- value$pm10 + 1L; value }, ",
        "list(ORIGINAL = body(count_stations_reporting)))"),
      "list(", paste(declarations, collapse = ",\n"), ")"), script)
  }
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    x <- targets::tar_meta(fields = c("name", "time"), store = store)
    x[order(x$name), ]
  }
  read <- function(name) targets::tar_read_raw(name, store = store)
  write_pipeline()
  make()
  files <- read("compute_descriptive_tables")
  expect_length(files, 41L)
  expect_false(anyDuplicated(files) > 0L)
  expect_true(all(file.exists(files)))
  expect_equal(read("station_counts_summary")$pm10, rep(2L, 4))
  who <- read("bogota_who_exceedances")
  expect_equal(who$city_avg[who$year == 2023L & who$pollutant == "pm10"], 25)
  expect_setequal(who$year, c(2022L, 2023L))
  expect_equal(read("bogota_census_summary")$total_population, 3)
  before <- metadata()
  make()
  expect_identical(metadata(), before)

  # Each writer restores either a missing dimension or the CSV copy without recomputing.
  for (name in c("bogota_raw_missing_by_hour.parquet", "census_summary.csv")) {
    unlink(files[basename(files) == name])
    make()
    expect_true(all(file.exists(files)))
    after <- metadata()
    computation <- stages[!grepl("files$|_file$|^compute_", stages)]
    expect_identical(after[after$name %in% computation, ],
                     before[before$name %in% computation, ])
  }

  # Adding or removing a source file updates its city, including directory membership.
  extra <- readings[1, ]; extra$station <- "C"
  path <- file.path(root, "alternate-inputs", "bogota", "bogota_metro_dataset",
                    "extra.parquet")
  arrow::write_parquet(extra, path)
  make()
  expect_equal(read("bogota_station_counts")$pm10, 3L)
  after <- metadata()
  other_city <- grep("^cdmx_", before$name, value = TRUE)
  expect_identical(after[after$name %in% other_city, ],
                   before[before$name %in% other_city, ])
  unlink(path)
  make()
  expect_equal(read("bogota_station_counts")$pm10, 2L)

  before <- metadata()
  write_pipeline(year = 2022L)
  make()
  expect_equal(read("bogota_station_counts")$pm10, 1L)
  after <- metadata()
  unaffected <- c("bogota_who_exceedances", "bogota_census_summary")
  expect_identical(after[after$name %in% unaffected, ],
                   before[before$name %in% unaffected, ])
  write_pipeline(year = 2022L, alter_counts = TRUE)
  make()
  expect_equal(read("bogota_station_counts")$pm10, 2L)
})
