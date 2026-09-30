test_that("imputation diagnostics expose the station ratios without writing files", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/plot/imputation_diagnostics.R"), e)
  predictions <- data.frame(station_id = rep(c("A", "B", "C"), each = 4),
    pollutant = "pm10", datetime = rep(as.POSIXct("2023-01-01", tz = "UTC") +
      (0:3) * 3600, 3), observed = c(10, 20, NA, NA, 20, 20, NA, NA, 10, 10, 10, 10),
    predicted = c(10, 20, 30, 30, 20, 20, 10, 10, 10, 10, 10, 10))
  predictions$was_missing <- is.na(predictions$observed)
  stations <- data.frame(station_id = c("A", "B", "C"), education_mean = c(NA, 8, 12))
  original <- predictions
  ratios <- e$summarize_imputation_ratios(predictions, stations, "pm10")
  expect_identical(ratios$station_id, c("B", "A"))
  expect_equal(ratios$ratio, c(0.5, 2))
  expect_equal(ratios$n_missing, c(2, 2))
  expect_equal(ratios$education_rank, 1:2)
  expect_identical(predictions, original)
  expect_s3_class(e$plot_imputation_series(predictions, "pm10", "Toy"), "ggplot")
  plot <- e$plot_imputation_ratio_by_station(ratios, "pm10", "Toy")
  expect_equal(plot$data, ratios)
  expect_false("out_file" %in% names(formals(e$plot_imputation_series)))
  expect_false("out_file" %in% names(formals(e$plot_imputation_ratio_by_station)))
  expect_error(e$summarize_imputation_ratios(predictions, stations, "pm25"),
               "No station")
})

test_that("imputation targets track partitions and sidecars separately for each city", {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("imputation-targets-", tmpdir = cache)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  script <- file.path(work, "pipeline.R")
  store <- file.path(work, "store")
  output <- file.path(work, "imputed_ols")
  # Three weeks retain observed counterparts for both missing weekday-hour cells.
  panel <- expand.grid(hour = 0:503, station = c("A", "B"))
  panel$station_code <- panel$station
  panel$datetime <- as.POSIXct("2023-01-01", tz = "UTC") + panel$hour * 3600
  panel$pm10 <- 20 + sin(panel$hour / 4) + (panel$station == "B") * 10
  panel$pm10[panel$station == "A" & panel$hour %in% c(100, 120)] <- NA_real_
  panel$pm25 <- panel$pm10 / 2
  panel$year <- 2023L
  input <- file.path(work, "input")
  arrow::write_dataset(panel, input, partitioning = "year")

  manifest <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  quote_text <- function(x) encodeString(x, quote = '"')
  redirect <- function(command) {
    if (!is.call(command)) return(command)
    if (identical(command[[1]], quote(here::here))) return(output)
    for (i in seq_along(command)[-1]) {
      if (is.call(command[[i]])) command[[i]] <- redirect(command[[i]])
    }
    command
  }
  declarations <- paste0("targets::tar_target(", c("bogota_outliers", "cdmx_outliers"),
                          ", ", quote_text(input), ", format = 'file')")
  for (name in c("bogota_imputation", "cdmx_imputation")) {
    row <- manifest[manifest$name == name, ]
    command <- redirect(str2lang(row$command))
    declarations <- c(declarations, paste0("targets::tar_target_raw(", quote_text(name),
      ", quote(", paste(deparse(command), collapse = "\n"), "), format = 'file')"))
  }
  writeLines(c(
    paste0("source(", quote_text(here::here("src/general_utilities/process/imputation.R")),
           ")"),
    "imputation_year <- 2023L", "imputation_pollutants <- c('pm10', 'pm25')",
    "list(", paste(declarations, collapse = ",\n"), ")"), script)
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  outdated <- function() targets::tar_outdated(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    x <- targets::tar_meta(fields = c("name", "time"), store = store)
    x[order(x$name), ]
  }
  make()
  expect_length(outdated(), 0L)
  before <- metadata()
  make()
  expect_identical(metadata(), before)
  files <- targets::tar_read_raw("bogota_imputation", store = store)
  expect_length(files, 3L)
  expect_true(all(file.exists(files)))
  count_file <- files[endsWith(files, "_counts.parquet")]
  expect_equal(arrow::read_parquet(count_file)$n_imputed, c(2L, 2L))

  # Losing either sidecar or a year partition must rerun its owner, leaving CDMX cached.
  panel_dir <- files[basename(files) == "bogota_imputed"]
  deletions <- c(count_file, files[endsWith(files, "_predictions.parquet")],
                list.files(panel_dir, pattern = "[.]parquet$", recursive = TRUE,
                           full.names = TRUE)[1])
  cdmx_before <- before[before$name == "cdmx_imputation", ]
  for (file in deletions) {
    unlink(file)
    expect_true("bogota_imputation" %in% outdated())
    expect_false("cdmx_imputation" %in% outdated())
    make()
    expect_true(file.exists(file))
    after <- metadata()
    expect_identical(after[after$name == "cdmx_imputation", ], cdmx_before)
  }
  settings <- readLines(script)
  writeLines(sub("imputation_pollutants <- .*", "imputation_pollutants <- 'pm10'",
                 settings), script)
  expect_true(all(c("bogota_imputation", "cdmx_imputation") %in% outdated()))

  # Imputed estimates consume their own file family and preserve public stage selections.
  deps <- targets::tar_deps_raw(str2lang(manifest$command[
    manifest$name == "bogota_2018_imputed_estimates"]))
  expect_true(all(c("bogota_2018_imputed_idw", "bogota_2018_distances",
                    "imputation_year", "imputed_exposure_buffer_km") %in% deps))
  expect_false(any(grepl("cdmx|santiago|sao_paulo", deps)))
})

test_that("a missing diagnostic PDF reruns saving without rebuilding its plots", {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("imputation-figures-", tmpdir = cache)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  old_theme <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(old_theme), add = TRUE)
  on.exit(showtext::showtext_auto(FALSE), add = TRUE)
  script <- file.path(work, "pipeline.R")
  store <- file.path(work, "store")
  output <- file.path(work, "figures")
  manifest <- targets::tar_manifest(fields = c("name", "command"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  writer <- str2lang(manifest$command[manifest$name == "bogota_imputation_figures"])
  redirect <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(here::here)) && identical(x[[2]], "results")) {
      return(output)
    }
    for (i in seq_along(x)[-1]) if (is.call(x[[i]])) x[[i]] <- redirect(x[[i]])
    x
  }
  quote_text <- function(x) encodeString(x, quote = '"')
  writeLines(c(
    paste0("source(", quote_text(here::here("src/general_utilities/theme_paper.R")), ")"),
    "imputation_pollutants <- c('pm10', 'pm25')",
    "list(",
    paste0("targets::tar_target(paper_font, ",
      quote_text(here::here("fonts/texgyrepagella-regular.otf")), ", format = 'file'),"),
    "targets::tar_target(bogota_imputation_plots, {",
    "  p <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:3), ggplot2::aes(x, y)) +",
    "    ggplot2::geom_point()",
    "  list(series = list(pm10 = p, pm25 = p), ratios = list(pm10 = p, pm25 = p))",
    "}),",
    paste0("targets::tar_target_raw('bogota_imputation_figures', quote(",
           paste(deparse(redirect(writer)), collapse = "\n"), "), format = 'file'))")),
    script)
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  plot_time <- function() targets::tar_meta(names = "bogota_imputation_plots",
                                            fields = c("name", "time"), store = store)
  make()
  files <- targets::tar_read_raw("bogota_imputation_figures", store = store)
  expect_setequal(basename(files), c("model2_bogota.pdf", "model2_bogota_scatter.pdf",
    "model2_bogota_pm25.pdf", "model2_bogota_scatter_pm25.pdf"))
  expect_true(all(file.info(files)$size > 0))
  before <- plot_time()
  unlink(files[1])
  make()
  expect_true(file.exists(files[1]))
  expect_identical(plot_time(), before)
})
