# Exercise the production writers without rebuilding scientific estimates or census inputs.
test_that("exposure checkpoints regenerate files without recomputing tables", {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("exposure-targets-", tmpdir = cache)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  script <- file.path(work, "pipeline.R")
  store <- file.path(work, "store")
  output <- file.path(work, "outputs")
  input <- file.path(work, "estimates.rds")
  estimates <- data.table::data.table(city = "toy", city_id = "toy", year = 2023L,
    buffer_km = 3L, socioeconomic_var = "education", group_type = "quintile", estimate = 1)
  saveRDS(estimates, input)

  manifest <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  quote_text <- function(x) encodeString(x, quote = '"')
  declarations <- c(
    paste0("targets::tar_target(input, ", quote_text(input), ", format = 'file')"),
    paste0("targets::tar_target(exposure_group_tables, list('3' = list(ci = ",
           "readRDS(input)), '5' = list(ci = readRDS(input))))"),
    "targets::tar_target(exposure_individual_table, readRDS(input))")
  writers <- c("exposure_group_files", "exposure_individual_files", "estimate_exposure")
  redirect <- function(command) {
    if (!is.call(command)) return(command)
    if (identical(command[[1]], quote(here::here))) return(output)
    for (i in seq_along(command)[-1]) {
      if (is.call(command[[i]])) command[[i]] <- redirect(command[[i]])
    }
    command
  }
  for (name in writers) {
    row <- manifest[manifest$name == name, ]
    command <- redirect(str2lang(row$command))
    declarations <- c(declarations, paste0("targets::tar_target_raw(",
      quote_text(name), ", quote(", paste(deparse(command), collapse = "\n"),
      "), format = 'file')"))
  }
  writeLines(c(
    paste0("source(", quote_text(here::here(
      "src/general_utilities/process/exposure_regressions.R")), ")"),
    paste0("source(", quote_text(here::here("config/analysis_settings.R")), ")"),
    "list(", paste(declarations, collapse = ",\n"), ")"), script)
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  outdated <- function() targets::tar_outdated(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    x <- targets::tar_meta(fields = c("name", "time"), store = store)
    x[x$name %in% c("exposure_group_tables", "exposure_individual_table"), ]
  }
  make()
  expect_length(outdated(), 0L)
  files <- targets::tar_read_raw("estimate_exposure", store = store)
  expect_length(files, 6L)
  expect_true(all(file.exists(files)))
  before <- metadata()
  make()
  expect_identical(metadata(), before)

  # Both formats are owned: deleting either one rebuilds its writer, not its computation.
  for (extension in c("parquet", "csv")) {
    missing <- files[endsWith(files, paste0(".", extension))][1]
    unlink(missing)
    expect_true("exposure_group_files" %in% outdated())
    make()
    expect_true(file.exists(missing))
    expect_identical(metadata(), before)
  }
  estimates[, estimate := 2]
  saveRDS(estimates, input)
  expect_true(all(c("exposure_group_tables", "exposure_individual_table") %in% outdated()))
  make()
  values <- arrow::read_parquet(files[endsWith(files, ".parquet")][1])
  expect_equal(values$estimate, 2)

  # Microdata remain file-backed; estimates depend on their own city's files.
  population <- manifest[grepl("_(education|income)_population$", manifest$name), ]
  expect_equal(nrow(population), 7L)
  expect_true(all(population$format == "file"))
  deps <- targets::tar_deps_raw(str2lang(manifest$command[
    manifest$name == "bogota_2018_education_estimates"]))
  expect_true(all(c("bogota_2018_education_population", "bogota_2018_distances",
                    "bogota_2018_exposure_inputs", "analysis_year") %in% deps))
  expect_false(any(grepl("cdmx|santiago|sao_paulo", deps)))
})
