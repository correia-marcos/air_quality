# Execute the actual distance commands from _targets.R on isolated two-city fixtures.
test_that("distance targets cache computations and own regenerable Parquet checkpoints", {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("distance-targets-", tmpdir = cache)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  script <- file.path(work, "pipeline.R")
  store <- file.path(work, "store")
  manifest <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))

  stations <- sf::st_as_sf(data.frame(station_name = c("a", "b"), station = c("a", "b"),
    x = c(0, .02), y = 0), coords = c("x", "y"), crs = 4326)
  polygon <- sf::st_polygon(list(matrix(c(0, 0, .01, 0, .01, .01, 0, .01, 0, 0),
                                        ncol = 2, byrow = TRUE)))
  geography <- sf::st_sf(GEO_ID = "0001", CVE_MUN = "0001",
                         geometry = sf::st_sfc(polygon, crs = 4326))
  input_paths <- c(
    bogota_stations_filter = file.path(work, "bogota_2018_stations_buffer_metro.gpkg"),
    bogota_geography = file.path(work, "bogota_area_metro_census_tracts_2018.gpkg"),
    cdmx_stations_filter = file.path(work, "cdmx_stations_buffer_metro.gpkg"),
    cdmx_geography = file.path(work, "cdmx_area_metro_municipalities_2024.gpkg"))
  for (name in names(input_paths)) {
    object <- if (grepl("stations", name)) stations else geography
    sf::st_write(object, input_paths[[name]], quiet = TRUE)
  }
  context <- c("bogota_2018", "cdmx_2020")
  nodes <- c("bogota_distance_stations", "cdmx_distance_stations",
             paste0(context, "_distance_geography"),
             paste0(context, "_distance_matrices"), paste0(context, "_distances"))
  expected <- compute_distance_matrices(stations, "station_name", geography, "GEO_ID",
                                        quiet = TRUE)

  write_pipeline <- function(metric = "aeqd", changed_function = FALSE,
                             blocked_output = NULL) {
    quote_text <- function(x) encodeString(x, quote = '"')
    declarations <- vapply(names(input_paths), function(name) {
      paste0("targets::tar_target_raw(", quote_text(name), ", quote(",
             quote_text(input_paths[[name]]), "), format = 'file')")
    }, character(1))
    for (name in nodes) {
      row <- manifest[manifest$name == name, ]
      stopifnot(nrow(row) == 1L)
      command <- str2lang(row$command)
      if (endsWith(name, "_distances")) {
        command[["out_dir"]] <- file.path(work, "outputs", name)
        if (name == "bogota_2018_distances" && !is.null(blocked_output)) {
          command[["out_dir"]] <- blocked_output
        }
      }
      declarations <- c(declarations, paste0("targets::tar_target_raw(",
        quote_text(name), ", quote(", paste(deparse(command), collapse = "\n"),
        "), format = ", quote_text(row$format), ")"))
    }
    definitions <- c(
      paste0("source(", quote_text(here::here(
        "src/general_utilities/base_utils.R")), ")"),
      paste0("source(", quote_text(here::here(
        "src/general_utilities/process/distances.R")), ")"),
      paste0("source(", quote_text(here::here("config/analysis_settings.R")), ")"),
      paste0("distance_metric <- ", quote_text(metric)))
    if (changed_function) {
      definitions <- c(definitions,
        "body(compute_distance_matrices) <- as.call(list(as.name('{'),",
        "  quote(invisible(NULL)), body(compute_distance_matrices)))")
    }
    writeLines(c(definitions, "list(", paste(declarations, collapse = ",\n"), ")"),
               script)
  }
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  outdated <- function() targets::tar_outdated(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    data <- targets::tar_meta(fields = c("name", "time"), store = store)
    data[data$name %in% c(nodes, names(input_paths)), ][order(
      data$name[data$name %in% c(nodes, names(input_paths))]), ]
  }

  write_pipeline()
  make()
  expect_length(outdated(), 0L)
  for (id in context) {
    actual <- targets::tar_read_raw(paste0(id, "_distance_matrices"), store = store)
    expect_equal(actual, expected)
    files <- targets::tar_read_raw(paste0(id, "_distances"), store = store)
    expect_length(files, 2L)
    expect_true(all(file.exists(files)))
  }
  before <- metadata()
  make()
  expect_identical(metadata(), before)

  files <- targets::tar_read_raw("bogota_2018_distances", store = store)
  for (file in files) {
    before <- metadata()
    unlink(file)
    expect_identical(outdated(), "bogota_2018_distances")
    make()
    expect_true(file.exists(file))
    after <- metadata()
    expect_identical(after[after$name != "bogota_2018_distances", ],
                     before[before$name != "bogota_2018_distances", ])
  }

  changed <- geography
  changed$GEO_ID <- "0002"
  sf::st_write(changed, input_paths[["bogota_geography"]],
               delete_dsn = TRUE, quiet = TRUE)
  expect_setequal(outdated(), c("bogota_geography", "bogota_2018_distance_geography",
                                "bogota_2018_distance_matrices", "bogota_2018_distances"))
  before <- metadata()
  make()
  after <- metadata()
  expect_identical(after[startsWith(after$name, "cdmx"), ],
                   before[startsWith(before$name, "cdmx"), ])
  actual <- targets::tar_read_raw("bogota_2018_distance_matrices", store = store)
  expect_identical(actual$geo_station_matrix$geo_id, c("0002", "0002"))

  write_pipeline(metric = "geosphere")
  expect_setequal(outdated(), c(paste0(context, "_distance_matrices"),
                                paste0(context, "_distances")))
  make()
  write_pipeline(metric = "geosphere", changed_function = TRUE)
  expect_setequal(outdated(), c(paste0(context, "_distance_matrices"),
                                paste0(context, "_distances")))
  make()
  expect_length(outdated(), 0L)

  # A failed writer must not be reported as a successful checkpoint.
  blocked_output <- file.path(work, "blocked-output")
  writeLines("blocks directory creation", blocked_output)
  write_pipeline(metric = "geosphere", changed_function = TRUE,
                 blocked_output = blocked_output)
  expect_error(suppressWarnings(make()), "Cannot|Failed|directory|file|File")
  expect_true("bogota_2018_distances" %in% outdated())
})
