test_that("LaTeX objects preserve source values and save separately", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/base_utils.R"), e)
  sys.source(here::here("src/general_utilities/plot/latex_tables.R"), e)
  counts <- data.frame(city = "Toy", pm10 = 3L, pm25 = 4L)
  tex <- e$latex_station_counts(counts)
  expect_true(any(grepl("Toy &  3 &  4", tex, fixed = TRUE)))

  missing <- data.table::data.table(hour = 0:1, pm10_missing_pct = c(12.345, 0))
  original <- data.table::copy(missing)
  table <- e$table_missing_by_dimension(list(hour = missing), "hour", "Toy")
  tex_missing <- e$latex_missing_dimension(table, "hour", "Toy")
  expect_equal(missing, original)
  expect_true(any(grepl("12.3", tex_missing, fixed = TRUE)))
  expect_true(any(grepl("tab:missing_hour_toy", tex_missing, fixed = TRUE)))

  root <- tempfile("latex-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "new-folder", "counts.tex")
  expect_identical(e$write_latex_table(tex, path), path)
  expect_identical(readLines(path), tex)
  expect_error(suppressWarnings(e$write_latex_table(tex, file.path(path, "invalid.tex"))))
})

test_that("table targets cache LaTeX and regenerate missing files from explicit inputs", {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  root <- tempfile("rendering-", tmpdir = cache); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  inputs <- file.path(root, "alternate-inputs"); dir.create(inputs)
  for (family in c("station_counts", "who_exceedances", "threshold_exceedances")) {
    dir.create(file.path(inputs, family))
  }
  outputs <- file.path(root, "outputs")
  script <- file.path(root, "pipeline.R")
  store <- file.path(root, "store")
  counts <- data.frame(city = "Toy", pm10 = 3L, pm25 = 4L)
  count_file <- file.path(inputs, "station_counts", "stations_by_pollutant_2023.parquet")
  arrow::write_parquet(counts, count_file)
  who <- data.frame(city = "Toy", year = 2023L, pollutant = c("pm10", "pm25"),
                     city_avg = c(30, 10), who_aqg = c(15, 5), exceedance_factor = 2)
  arrow::write_parquet(who, file.path(inputs, "who_exceedances",
                                    "who_exceedances_all_cities.parquet"))
  thresholds <- expand.grid(city = c("Bogota", "Santiago", "Mexico City", "Sao Paulo"),
    series = c("city_hour", "station_hour"), pollutant = c("pm10", "pm25"),
    threshold = c("it1", "it2"), stringsAsFactors = FALSE)
  thresholds$days_ge1 <- 3L
  thresholds$days_ge2 <- 2L
  thresholds$mean_hours <- 1.5
  arrow::write_parquet(thresholds, file.path(inputs, "threshold_exceedances",
                                           "days_and_hours_2023.parquet"))

  manifest <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  redirect <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(here::here)) && identical(x[[2]], "results")) {
      x[[1]] <- quote(file.path)
      x[[2]] <- outputs
    }
    for (i in seq_along(x)[-1]) if (is.call(x[[i]])) x[[i]] <- redirect(x[[i]])
    x
  }
  quote_text <- function(x) encodeString(x, quote = '"')
  stages <- c("station_table_data", "station_table_tex", "render_station_tables")
  declarations <- character()
  families <- c(station_counts_files = "station_counts",
    who_exceedance_file = "who_exceedances", threshold_exceedance_files =
      "threshold_exceedances")
  for (name in names(families)) {
    declarations <- c(declarations, paste0("targets::tar_target_raw(", quote_text(name),
      ", quote(list.files(", quote_text(file.path(inputs, families[[name]])),
      ", full.names = TRUE)), format = 'file')"))
  }
  for (stage in stages) {
    row <- manifest[manifest$name == stage, ]
    command <- redirect(str2lang(row$command))
    declarations <- c(declarations, paste0("targets::tar_target_raw(", quote_text(stage),
      ", quote(", paste(deparse(command), collapse = "\n"), "), format = ",
      quote_text(row$format), ", packages = 'data.table')"))
  }
  writeLines(c(
    paste0("source(", quote_text(here::here("src/general_utilities/base_utils.R")), ")"),
    paste0("source(", quote_text(here::here("src/general_utilities/plot/latex_tables.R")),
           ")"),
    "analysis_year <- 2023L", "list(", paste(declarations, collapse = ",\n"), ")"), script)
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    x <- targets::tar_meta(fields = c("name", "time"), store = store)
    x[order(x$name), ]
  }
  make()
  files <- targets::tar_read_raw("render_station_tables", store = store)
  expect_length(files, 4L)
  expect_true(all(file.exists(files)))
  tex <- targets::tar_read_raw("station_table_tex", store = store)
  expect_true(any(grepl("Toy &  3 &  4", tex$counts, fixed = TRUE)))
  before <- metadata()
  make()
  expect_identical(metadata(), before)
  unlink(files[endsWith(files, "table_avg_hours_above_thresholds.tex")])
  make()
  expect_true(all(file.exists(files)))
  after <- metadata()
  expect_identical(after[after$name == "station_table_tex", ],
                   before[before$name == "station_table_tex", ])

  counts$pm10 <- 7L
  arrow::write_parquet(counts, count_file)
  make()
  new_counts <- readLines(files[endsWith(files, "stations_by_pollutant_2023.tex")])
  expect_true(any(grepl("Toy &  7 &  4", new_counts, fixed = TRUE)))
})

test_that("exposure saving preserves observed and imputed filename selection", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/plot/exposure_figures.R"), e)
  calls <- list()
  e$save_plot_pdf <- function(plot_obj, path, ...) {
    calls[[path]] <<- plot_obj
    path
  }
  root <- tempfile("exposure-names-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  plots <- list(bogota_education_3km_levels.pdf = 1,
                bogota_education_5km_levels.pdf = 2,
                bogota_education_3km_hrs_d_it1_pm25_pm10_ci.pdf = 3)
  files <- e$save_exposure_plot_family(plots, root, c(bogota = "bogota"), 2023)
  expect_setequal(basename(files), c("plot_quintiles_bogota_pm10_pm25_mean_2023_3km.pdf",
    "bogota_education_5km_levels.pdf", "plot_hours_above_IT1_bogota_2023_3km_reg1.pdf"))
  expect_length(calls, 3L)
  files <- e$save_exposure_plot_family(plots, root, c(bogota = "bogota"), 2023,
                                       imputed = TRUE)
  expect_setequal(basename(files), c("plot_quintiles_bogota_all_mean_2023_3km_imp.pdf",
                                    "plot_hours_above_IT1_bogota_2023_3km_imp.pdf"))
  expect_true(all(basename(dirname(files)) == "imputation"))
})

test_that("manuscript export rejects existing files absent from current target outputs", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/city_specific/processing.R"), e)
  sys.source(here::here("src/general_utilities/reproducibility.R"), e)
  root <- tempfile("rendered-selection-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  current <- file.path(root, "current.tex")
  stale <- file.path(root, "stale.tex")
  writeLines("current table", current)
  writeLines("old table", stale)
  e$artifact_manifest <- function(path) data.frame(source_path = c(current, stale))
  e$export_paper_artifacts <- function(...) stop("Exporter reached")

  expect_error(e$prepare_paper_export(current, "fixture"), "did not report.*stale.tex")
  expect_identical(readLines(stale), "old table")
  expect_error(e$prepare_paper_export(c(current, stale), "fixture"), "Exporter reached")
})
