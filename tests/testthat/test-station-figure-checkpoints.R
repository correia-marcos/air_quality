test_that("station joins retain active stations and distinguish unmatched context", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/station_socio.R"), e)
  pollution <- data.table::data.table(station_id = c("A", "B", "C", "D"),
    avg_pm10 = c(10, 20, 30, 40))
  context <- data.table::data.table(station_id = c("A", "B", "C", "unused"),
    education_mean = c(8, NA, 10, 12), income_mean = c(NA, 100, 120, 130),
    n_geo_context = c(2L, 1L, 0L, 1L))
  before_pollution <- data.table::copy(pollution)
  before_context <- data.table::copy(context)
  joined <- e$join_station_scatter_inputs(pollution, context,
    socio_vars = c("education_mean", "income_mean"), year_filter = 2023L, quiet = TRUE)
  expect_identical(joined$station_id, pollution$station_id)
  expect_identical(joined$matched_socio_context, c(1L, 1L, 0L, 0L))
  expect_equal(joined$avg_pm10, c(10, 20, 30, 40))
  expect_identical(joined$year, rep(2023L, 4))
  expect_equal(pollution, before_pollution)
  expect_equal(context, before_context)
  education_only <- e$join_station_scatter_inputs(pollution, context,
    socio_vars = "education_mean", year_filter = 2023L, quiet = TRUE)
  expect_identical(education_only$matched_socio_context, c(1L, 0L, 0L, 0L))
})

test_that("exposure densities consume explicit weights without changing their inputs", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/base_utils.R"), e)
  sys.source(here::here("src/general_utilities/process/geo_ids.R"), e)
  sys.source(here::here("src/general_utilities/plot/exposure_figures.R"), e)
  root <- tempfile("quintile-weights-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "alternate-groups.parquet")
  groups <- data.frame(geo_id = c("a", "a", "b", "b", "c", "c", "d"),
    edu_quintile = c(1L, 1L, 1L, 2L, 2L, NA, 1L),
    person_weight = c(2, 3, 4, 0, NA, 8, 1))
  arrow::write_parquet(groups, path)
  weights <- e$compute_exposure_quintile_weights(path)
  expect_equal(as.data.frame(weights), data.frame(geo_id = c("a", "b", "d"),
    edu_quintile = rep(1L, 3), quintile_population = c(5, 4, 1)))
  exposure <- data.table::data.table(geo_id = c("a", "b", "d", "a"),
    year = c(2023L, 2023L, 2023L, 2022L), avg_pm25 = c(10, 20, 100, 999))
  before_exposure <- data.table::copy(exposure)
  before_weights <- data.table::copy(weights)
  # Nine of ten weighted people have exposure <=20, so the 90th percentile is 20.
  plot <- e$plot_exposure_density_by_quintile(exposure, weights, city_id = "toy",
    buffer_km = 3, pollutant = "pm25", year_filter = 2023L, x_trim_q = 0.9)
  expect_equal(plot$data$avg_pm25, c(10, 20))
  expect_equal(plot$data$quintile_population, c(5, 4))
  expect_equal(exposure, before_exposure)
  expect_equal(weights, before_weights)
  expect_identical(list.files(root), basename(path))
})

test_that("station figure targets reuse plots and repair files from alternate inputs", {
  root <- tempfile("station-figures-", tmpdir = here::here("tests/_cache"))
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "pipeline.R")
  store <- file.path(root, "store")
  inputs <- file.path(root, "inputs"); dir.create(inputs)
  outputs <- file.path(root, "outputs")
  pollution <- data.frame(station_id = LETTERS[1:4], avg_pm10 = c(10, 20, 30, 40),
    avg_pm25 = c(5, 15, 10, 20), hrs_d_pm10_it1 = 1:4, hrs_d_pm10_it2 = 2:5,
    hrs_d_pm25_it1 = 2:5, hrs_d_pm25_it2 = 3:6)
  context <- data.frame(station_id = LETTERS[1:4], education_mean = c(8, 10, 12, 9),
    n_geo_context = 1L)
  for (city in c("bogota", "santiago")) {
    arrow::write_parquet(pollution, file.path(inputs, paste0(city, ".parquet")))
  }
  arrow::write_parquet(context, file.path(inputs, "context.parquet"))
  graph <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  quote_text <- function(x) encodeString(x, quote = '"')
  redirect <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(here::here)) && identical(x[[2]], "results")) {
      x[[2]] <- outputs
    }
    if (identical(x[[1]], quote(here::here)) && length(x) >= 4L &&
        identical(as.list(x)[2:4], list("data", "processed", "station_socio_exposure"))) {
      x <- as.call(c(list(quote(file.path), outputs), as.list(x)[-(1:4)]))
    }
    for (i in seq_along(x)[-1]) if (is.call(x[[i]])) x[[i]] <- redirect(x[[i]])
    x
  }
  declarations <- character()
  for (city in c("bogota", "santiago")) {
    declarations <- c(declarations,
      paste0("targets::tar_target_raw('", city, "_input', quote(",
        quote_text(file.path(inputs, paste0(city, ".parquet"))), "), format = 'file')"),
      paste0("targets::tar_target_raw('", city, "_station_pollution_summary', ",
        "quote(data.table::as.data.table(arrow::read_parquet(", city, "_input))))"),
      paste0("targets::tar_target_raw('", city, "_station_context', ",
        "quote(data.table::as.data.table(arrow::read_parquet(context_file))))"))
    for (suffix in c("station_socio", "station_socio_file", "station_plot_data",
                     "station_scatter_plots", "station_scatter_files")) {
      name <- paste(city, suffix, sep = "_")
      row <- graph[graph$name == name, ]
      command <- redirect(str2lang(row$command))
      declarations <- c(declarations, paste0("targets::tar_target_raw(", quote_text(name),
        ", quote(", paste(deparse(command), collapse = "\n"), "), format = ",
        quote_text(row$format), ", packages = 'data.table')"))
    }
  }
  declarations <- c(declarations,
    paste0("targets::tar_target(context_file, ",
      quote_text(file.path(inputs, "context.parquet")), ", format = 'file')"),
    paste0("targets::tar_target(paper_font, ",
      quote_text(here::here("fonts/texgyrepagella-regular.otf")), ", format = 'file')"))
  modules <- here::here("src/general_utilities", c("base_utils.R", "theme_paper.R",
    "process/station_socio.R", "plot/station_monitoring.R", "plot/exposure_figures.R"))
  write_pipeline <- function(year = 2023L, alter_function = FALSE) {
    writeLines(c(paste0("source(", quote_text(modules), ")"),
      paste0("source(", quote_text(here::here("config/analysis_settings.R")), ")"),
      paste0("analysis_year <- ", year, "L"),
      if (alter_function) paste0("body(join_station_scatter_inputs) <- as.call(list(",
        "as.name('{'), quote(invisible(NULL)), body(join_station_scatter_inputs)))"),
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
  files <- read("bogota_station_scatter_files")
  expect_length(files, 6L)
  expect_true(all(file.exists(files)))
  expect_equal(read("bogota_station_socio")$avg_pm10, pollution$avg_pm10)
  expect_s3_class(read("bogota_station_scatter_plots")$avg_pm10, "ggplot")
  before <- metadata()
  make()
  expect_identical(metadata(), before)
  unlink(files[1])
  make()
  expect_true(all(file.exists(files)))
  after <- metadata()
  expect_identical(after[after$name == "bogota_station_scatter_plots", ],
                   before[before$name == "bogota_station_scatter_plots", ])
  unchanged <- after[startsWith(after$name, "santiago_"), ]
  pollution$avg_pm10[1] <- 12
  arrow::write_parquet(pollution, file.path(inputs, "bogota.parquet"))
  make()
  after <- metadata()
  expect_equal(read("bogota_station_socio")$avg_pm10[1], 12)
  expect_identical(after[startsWith(after$name, "santiago_"), ], unchanged)
  write_pipeline(year = 2024L)
  make()
  expect_identical(read("bogota_station_socio")$year, rep(2024L, 4))
  before <- metadata()
  write_pipeline(year = 2024L, alter_function = TRUE)
  make()
  after <- metadata()
  expect_false(identical(after[after$name == "bogota_station_socio", ],
                        before[before$name == "bogota_station_socio", ]))
})
