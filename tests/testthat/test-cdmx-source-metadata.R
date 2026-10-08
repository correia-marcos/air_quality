test_that("CDMX memory and DuckDB readers agree on wide CSVs and actual contributors", {
  source(here::here("src", "city_specific", "registry.R"), local = TRUE)
  source(here::here("src", "city_specific", "cdmx.R"), local = TRUE)
  root <- tempfile("cdmx-reader-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  primary <- file.path(root, "hidalgo__network__pm2_5__2023.csv")
  writeLines(c("Fecha,Hora,Parametro,Unidad,AAA : Alpha,BBB : Beta,AAC : Alpha",
    "01/01/2023,01:00 - 02:00,PM2.5,ug/m3,10,30,99000",
    "01/01/2023,01:00 - 02:00,PM2.5,ug/m3,20,-5,- - - -",
    "01/01/2023,02:00 - 03:00,PM2.5,ug/m3,- - - -,invalid,- - - -"), primary)
  fallback <- file.path(root, "hidalgo__network__pm10__2021.csv")
  writeLines(c("Fecha,Hora,Unidad,AAA: Alpha",
    "2021-01-01,00,ug/m3,40"), fallback)
  secondary <- file.path(root, "hidalgo__network__aaa__alpha__pm25__2023.xls")
  row <- function(values) paste0("<row>", paste0("<cell><data>", values,
    "</data></cell>", collapse = ""), "</row>")
  writeLines(c("<workbook><worksheet><table>",
    row(c("Fecha", "Hora", "Parametro", "Unidad", "Valor")), row(rep("", 5)),
    row(c("2023-01-01", "01:00", "PM2.5", "ug/m3", "50")),
    row(c("2023-01-01", "02:00", "PM2.5", "ug/m3", "-5")),
    "</table></worksheet></workbook>"), secondary)
  lookup <- data.frame(lookup_key = c("hidalgo__alpha", "hidalgo__beta", "hidalgo__alias"),
    code = c("AAA", "BBB", "AAC"), station_name_official = c("Alpha", "Beta", "alias"))
  engines <- lapply(c("memory", "duckdb"), function(engine) {
    common <- list(csvs = c(primary, fallback), new_source_files = secondary,
      station_lookup = lookup, years = c(2021L, 2023L), tz = "America/Mexico_City",
      out_dir = root, out_name = engine, cleanup = FALSE, verbose = FALSE,
      include_source_metadata = TRUE)
    dataset <- if (engine == "memory") do.call(.cdmx_merge_memory_engine,
      c(common, list(write_parquet = TRUE, write_rds = FALSE, write_csv = FALSE))) else
      do.call(.cdmx_merge_duckdb_engine, c(common, list(run_parallel = FALSE)))
    panel <- data.table::as.data.table(dplyr::collect(dataset))
    data.table::setorder(panel, year, station, datetime)
    panel
  })
  columns <- c("station", "station_code", "datetime", "year", "pm25", "pm10",
               "pm25_source_status", "pm25_source_ids", "pm10_source_status",
               "pm10_source_ids", "no2", "so2", "co", "ozone")
  expect_equal(engines[[1]][, ..columns], engines[[2]][, ..columns])
  panel <- engines[[2]]
  expect_identical(sort(unique(panel$year)), c(2021L, 2023L))
  expect_equal(panel[year == 2021, unique(station)], "Alpha")
  expect_equal(nrow(panel), 3 * 8760)
  first <- panel[station == "Alpha" & year == 2023 &
    datetime == as.POSIXct("2023-01-01 01:00:00", tz = "UTC")]
  expect_equal(first$pm25, (10 + 20 + 50) / 3)
  expect_equal(first$pm25_source_status, "mixed")
  expect_length(strsplit(first$pm25_source_ids, "|", fixed = TRUE)[[1]], 2)
  expect_equal(panel[station == "Beta" & pm25 == 30, pm25_source_status], "validated")
  expect_true(all(panel[is.na(pm25), pm25_source_status] == "unknown"))
  for (engine in c("memory", "duckdb")) {
    manifest <- data.table::fread(file.path(root, paste0(engine, "_source_manifest.csv")))
    expect_equal(nrow(manifest), 3)
    expect_true(all(nchar(manifest$source_sha256) == 64))
    expect_true(all(is.na(manifest$retrieved_at)))
    coverage <- data.table::fread(file.path(root, paste0(engine, "_source_coverage.csv")))
    expect_equal(sum(coverage$n_selected), 5)
    expect_equal(sum(coverage$n_negative), 2)
  }
  plain <- .cdmx_merge_duckdb_engine(c(primary, fallback), secondary, lookup,
    years = c(2021L, 2023L), tz = "UTC", out_dir = root, out_name = "plain",
    cleanup = FALSE, run_parallel = FALSE, verbose = FALSE)
  plain <- data.table::as.data.table(dplyr::collect(plain))
  data.table::setorder(plain, year, station, datetime)
  expect_equal(panel[, names(plain), with = FALSE], plain)
  expect_true(all(file.exists(c(primary, secondary, fallback))))
})

test_that("generated Python bytecode does not change the scientific code inventory", {
  source(here::here("src", "general_utilities", "verification_cli.R"), local = TRUE)
  root <- tempfile("bytecode-inventory-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "src", "__pycache__"), recursive = TRUE)
  writeLines("code", file.path(root, "src", "kept.R"))
  writeLines("cache", file.path(root, "src", "__pycache__", "guard.cpython-314.pyc"))
  writeLines("cache", file.path(root, "src", "module.pyc"))
  inventory <- verification_code_inventory(root)
  expect_true("src/kept.R" %in% inventory$path)
  expect_false(any(grepl("__pycache__|[.]pyc$", inventory$path)))
})

test_that("identical archives retain distinct file identities in provenance", {
  source(here::here("src", "city_specific", "registry.R"), local = TRUE)
  source(here::here("src", "city_specific", "cdmx.R"), local = TRUE)
  root <- tempfile("source-identities-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  paths <- file.path(root, c("original.csv", "duplicate.csv"))
  writeLines("identical archived bytes", paths[1])
  file.copy(paths[1], paths[2])
  files <- .cdmx_source_files(paths, character())
  expect_identical(files$source_sha256[1], files$source_sha256[2])
  expect_false(files$source_id[1] == files$source_id[2])
})
