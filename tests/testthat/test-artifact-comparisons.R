test_that("registered CSV and TeX comparisons detect analytical and textual changes", {
  source(here::here("src/general_utilities/reproducibility.R"), local = TRUE)
  root <- tempfile("artifact-comparisons-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  a <- file.path(root, "actual.csv"); b <- file.path(root, "baseline.csv")
  table <- data.frame(id = c("a", "b"), value = c(10, 20))
  write.csv(table, a, row.names = FALSE)
  write.csv(table[2:1, ], b, row.names = FALSE)
  expect_true(compare_registered_artifact(a, b, "id", atol = 0, rtol = 0))
  table$value[1] <- 11; write.csv(table, b, row.names = FALSE)
  expect_error(compare_registered_artifact(a, b, "id"), "differences")
  a <- file.path(root, "actual.tex"); b <- file.path(root, "baseline.tex")
  writeLines("known table", a); writeLines("known table", b)
  expect_true(compare_registered_artifact(a, b, character()))
  writeLines("changed table", b)
  expect_error(compare_registered_artifact(a, b, character()), "TeX content")
})

test_that("geographic baselines detect geometry changes with unchanged attributes", {
  source(here::here("src/general_utilities/reproducibility.R"), local = TRUE)
  root <- tempfile("geographic-comparisons-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  a <- file.path(root, "actual.gpkg"); b <- file.path(root, "baseline.gpkg")
  spatial <- sf::st_as_sf(data.frame(id = c("a", "b"), x = c(0, 1), y = c(0, 1)),
    coords = c("x", "y"), crs = 4326)
  sf::st_write(spatial, a, quiet = TRUE)
  sf::st_write(spatial[2:1, ], b, quiet = TRUE)
  expect_true(compare_registered_artifact(a, b, "id"))
  sf::st_geometry(spatial)[[1]] <- sf::st_point(c(0.1, 0))
  sf::st_write(spatial, b, delete_dsn = TRUE, quiet = TRUE)
  expect_error(compare_registered_artifact(a, b, "id"), "differences")
})

test_that("registered station provenance enters the generated-product comparison scope", {
  source(here::here("src", "general_utilities", "verification_cli.R"), local = TRUE)
  root <- tempfile("verification-products-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  manifest <- "data/interim/monitoring_stations/cdmx_metro_source_manifest.csv"
  coverage <- "data/interim/monitoring_stations/cdmx_metro_source_coverage.csv"
  analytical <- c(manifest, coverage, "data/processed/station_hourly/cdmx.parquet",
    "data/processed/clean/_audit/input_partitions.csv",
    "data/interim/geospatial_data/cdmx/metro.gpkg", "results/tables/coefficients.tex")
  excluded <- c("data/downloads/cdmx/original.csv", "data/_legacy/baseline.csv",
    "results/figures/temporal/ridge.pdf", "data/interim/census_extracted/persons.csv")
  for (relative in c(analytical, excluded)) {
    path <- file.path(root, relative)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("fixture", path)
  }
  expect_setequal(verification_product_paths(root), analytical)
  registry <- read.csv(here::here("config", "verification_comparisons.csv"))
  expect_true(all(c(manifest, coverage) %in% registry$actual_path))
})

test_that("verification keeps a failed subprocess status and its diagnostic output", {
  source(here::here("src", "general_utilities", "verification_cli.R"), local = TRUE)
  log <- tempfile("failed-verification-", fileext = ".log")
  on.exit(unlink(log), add = TRUE)
  command <- "cat('deliberate fixture failure\\n'); quit(status = 3L)"
  result <- suppressWarnings(verification_command(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(command)), log))
  expect_equal(result$exit_status, 3L)
  expect_match(readLines(log), "deliberate fixture failure")
})
