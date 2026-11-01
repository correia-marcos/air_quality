test_that("registered CSV and TeX comparisons detect analytical and textual changes", {
  source(here::here("src/general_utilities/reproducibility.R"), local = TRUE)
  root <- tempfile("artifact-comparisons-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  a <- file.path(root, "actual.csv"); b <- file.path(root, "baseline.csv")
  table <- data.frame(id = c("a", "b"), value = c(10.5, 20.25))
  write.csv(table, a, row.names = FALSE)
  write.csv(table[2:1, ], b, row.names = FALSE)
  expect_equal(compare_registered_artifact(a, b, "id")$max_abs_diff, 0)
  table$value[1] <- 11.5; write.csv(table, b, row.names = FALSE)
  expect_error(compare_registered_artifact(a, b, "id", 1000L), "differences")
  a <- file.path(root, "actual.tex"); b <- file.path(root, "baseline.tex")
  writeLines("known table", a); writeLines("known table", b)
  expect_equal(nrow(compare_registered_artifact(a, b, character())), 0)
  writeLines("changed table", b)
  expect_error(compare_registered_artifact(a, b, character()), "TeX content")
})

test_that("the rounding bound follows the declared steps and scale", {
  source(here::here("src/general_utilities/reproducibility.R"), local = TRUE)
  bound <- 2 * 1000 * 2^-53 / (1 - 1000 * 2^-53)
  # Signed contrasts: differences near zero are judged on the column scale (100).
  baseline <- data.frame(id = 1:3, gap = c(1e-15, -2e-15, 100), count = c(1L, 2L, 3L))
  rounding <- baseline
  rounding$gap <- baseline$gap + c(3e-14, -3e-14, 0)
  differences <- compare_numerical_tables(rounding, baseline, "id", 1000L, "column")
  expect_equal(differences$max_abs_diff, 3e-14)
  expect_equal(differences$max_scaled_diff, 3e-16)
  expect_equal(differences$bound, bound)
  expect_error(compare_numerical_tables(rounding, baseline, "id", 1000L, "value"), "gap")
  expect_error(compare_numerical_tables(rounding, baseline, "id"), "gap \\(scaled")
  # Nonnegative sums: each difference is judged on its own value.
  means <- data.frame(id = 1:2, avg = c(0.5, 80))
  drift <- means
  drift$avg <- means$avg * (1 + c(1e-13, -1e-13))
  expect_lt(compare_numerical_tables(drift, means, "id", 1000L)$max_scaled_diff, bound)
  drift$avg[1] <- 0.5 + 1e-12
  expect_error(compare_numerical_tables(drift, means, "id", 1000L), "avg")
  changed <- baseline
  changed$count[1] <- 2L
  expect_error(compare_numerical_tables(changed, baseline, "id", 1000L, "column"), "count")
})

test_that("registered rounding steps are counted, explained and scaled", {
  registry <- read.csv(here::here("config", "verification_comparisons.csv"),
                       stringsAsFactors = FALSE)
  expect_true(all(registry$rounding_steps >= 0 &
                  registry$rounding_steps == round(registry$rounding_steps)))
  expect_true(all(nzchar(registry$rounding_steps_basis[registry$rounding_steps > 0])))
  expect_true(all(registry$bound_scale %in% c("value", "column")))
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
  expect_equal(nrow(compare_registered_artifact(a, b, "id")), 0)
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
