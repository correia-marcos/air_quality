# Isolated engine checks use tiny text fixtures, never project datasets.
#' @return Number of cache and dependency assertions exercised with targets.
check_targets_engine <- function() {
  stopifnot(requireNamespace("targets", quietly = TRUE))
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("targets-engine-", tmpdir = cache)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  script <- file.path(work, "pipeline.R")
  store <- file.path(work, "store")
  partitions <- file.path(work, "source", "year=2023")
  dir.create(partitions, recursive = TRUE)
  writeLines("2", file.path(partitions, "part.txt"))
  source_b <- file.path(work, "source-b.txt")
  writeLines("7", source_b)
  output <- file.path(work, "output-a.txt")
  count <- 0L
  check <- function(condition) {
    stopifnot(isTRUE(condition))
    count <<- count + 1L
  }
  write_pipeline <- function(multiplier = 1L, offset = 0L) {
    writeLines(c(
      paste0("fixture_root <- ", encodeString(work, quote = '"')),
      paste0("config_a <- ", multiplier),
      paste0("transform_a <- function(x) x + ", offset),
      "list(",
      "targets::tar_target(source_a, file.path(fixture_root, 'source'), format='file'),",
      "targets::tar_target(source_b, file.path(fixture_root, 'source-b.txt'), format='file'),",
      "targets::tar_target(data_a, {",
      "  parts <- list.files(source_a, recursive=TRUE, full.names=TRUE)",
      "  values <- vapply(parts, function(p) as.numeric(readLines(p)), numeric(1))",
      "  transform_a(sum(values) * config_a)",
      "}),",
      "targets::tar_target(result_a, {",
      "  path <- file.path(fixture_root, 'output-a.txt')",
      "  writeLines(as.character(data_a), path)",
      "  c(dataset=path)",
      "}, format='file'),",
      "targets::tar_target(result_b, as.numeric(readLines(source_b)))",
      ")"), script)
  }
  all_targets <- c("source_a", "source_b", "data_a", "result_a", "result_b")
  make <- function(selection = all_targets) targets::tar_make(
    names = tidyselect::all_of(selection), script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  outdated <- function(selection = all_targets) targets::tar_outdated(
    names = tidyselect::all_of(selection), script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  write_pipeline()
  make("result_a")
  check(length(outdated("result_a")) == 0L)
  check(setequal(outdated(), c("source_b", "result_b")))
  make()
  check(identical(readLines(output), "2"))
  check(length(outdated()) == 0L)
  cached <- targets::tar_read_raw("result_a", store = store)
  e <- new.env(parent = globalenv())
  sys.source(here::here("src", "city_specific", "processing.R"), e)
  check(identical(e$processing_file_roles(cached, c(dataset = output)),
                  c(dataset = output)))
  contract <- c(dataset = output, other = source_b)
  check(identical(e$processing_file_roles(rev(unname(contract)), contract), contract))
  failure <- tryCatch(e$processing_file_roles(output, contract), error = conditionMessage)
  check(grepl("declared processing contract", failure))
  metadata <- function() {
    result <- targets::tar_meta(fields = c(name, time), store = store)
    result <- result[result$name %in% all_targets, ]
    result[order(result$name), ]
  }
  before <- metadata()
  make()
  check(identical(before, metadata()))

  writeLines("3", file.path(partitions, "part.txt"))
  check(setequal(outdated(), c("source_a", "data_a", "result_a")))
  make()
  check(identical(readLines(output), "3"))
  extra <- file.path(partitions, "extra.txt")
  writeLines("4", extra)
  check(setequal(outdated(), c("source_a", "data_a", "result_a")))
  make()
  check(identical(readLines(output), "7"))
  unlink(extra)
  check(setequal(outdated(), c("source_a", "data_a", "result_a")))
  make()
  check(identical(readLines(output), "3"))

  unlink(output)
  check(identical(outdated(), "result_a"))
  make()
  check(file.exists(output))
  write_pipeline(multiplier = 2L)
  check(setequal(outdated(), c("data_a", "result_a")))
  make()
  check(identical(readLines(output), "6"))
  write_pipeline(multiplier = 2L, offset = 1L)
  check(setequal(outdated(), c("data_a", "result_a")))
  make()
  check(identical(readLines(output), "7"))
  check(length(outdated()) == 0L)

  # A file-backed checkpoint can stay identical when its files change underneath it.
  # Its writer must also declare the files consumed during rendering.
  path_script <- file.path(work, "file-backed.R")
  path_store <- file.path(work, "file-backed-store")
  writeLines(c(
    paste0("fixture_root <- ", encodeString(work, quote = '"')),
    "read_fixture <- function(inputs) list(path = inputs)",
    "write_fixture <- function(data, inputs) {",
    "  stopifnot(identical(data$path, inputs))",
    "  output <- file.path(fixture_root, 'rendered.txt')",
    "  writeLines(readLines(data$path), output)",
    "  output",
    "}",
    "list(",
    "targets::tar_target(source, file.path(fixture_root, 'source-b.txt'), format='file'),",
    "targets::tar_target(selection, source, format='file'),",
    "targets::tar_target(data, read_fixture(selection)),",
    "targets::tar_target(rendered, write_fixture(data, selection), format='file'))"
  ), path_script)
  render <- function() targets::tar_make(script = path_script, store = path_store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  render()
  paths_before <- targets::tar_read_raw("data", store = path_store)
  writeLines("11", source_b)
  render()
  check(identical(paths_before, targets::tar_read_raw("data", store = path_store)))
  check(identical(readLines(file.path(work, "rendered.txt")), "11"))

  network <- targets::tar_network(script = here::here("_targets.R"),
    callr_function = NULL, envir = new.env(parent = globalenv()))
  parents <- function(target) network$edges$from[network$edges$to == target]
  check(all(c("bogota_config", "bogota_metro_2005_sf", "write_geopackage")
    %in% parents("bogota_geography")))
  check(all(c("bogota_geography_inputs", "bogota_prepare_metro_area")
    %in% parents("bogota_metro_2005_sf")))
  check(!"cdmx_config" %in% parents("bogota_geography"))
  check("bogota_2018_distances" %in% parents("bogota_outliers"))
  check("figure_station_temporal_data" %in% parents("figure_station_temporal"))
  check("prepare_station_temporal" %in% parents("figure_station_temporal_data"))
  check("exposure_plot_data" %in% parents("generate_exposure_plots"))
  check("station_table_data" %in% parents("render_station_tables"))
  check("idw" %in% parents("figure_quintile_kernel_distributions"))
  check("estimate_exposure_imputed" %in% parents("generate_exposure_plots"))
  count
}

if (sys.nframe() == 0L) {
  count <- check_targets_engine()
  cat(count, "targets engine assertions passed\n")
}
