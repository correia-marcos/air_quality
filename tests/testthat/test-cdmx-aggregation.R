test_that("CDMX contributor aggregation completes within a bounded memory budget", {
  source(here::here("src", "city_specific", "registry.R"), local = TRUE)
  source(here::here("src", "city_specific", "cdmx.R"), local = TRUE)
  root <- tempfile("cdmx-aggregation-")
  dir.create(root)
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = file.path(root, "records.duckdb"),
                                    shared_home = FALSE))
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(root, recursive = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, "SET threads=1")
  DBI::dbExecute(con, "SET memory_limit='128MB'")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE long_mapped AS SELECT ",
    "TIMESTAMP '2000-01-01' + (i // 80000) * INTERVAL 1 YEAR + ",
    "(i % 8000) * INTERVAL 1 HOUR AS datetime, ",
    "'station_' || CAST((i // 8000) % 10 AS VARCHAR) AS station, ",
    "CAST((i // 8000) % 10 AS VARCHAR) AS station_code, ",
    "CAST(2000 + i // 80000 AS INTEGER) AS year, 'pm25' AS var, ",
    "CAST(i % 100 AS DOUBLE) AS value, REPEAT('a',64) || '-1' AS source_id, ",
    "'validated' AS source_status FROM range(1280000) t(i)"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO long_mapped VALUES ",
    "('2000-01-01', 'station_0', '0', 2000, 'pm25', 30, ",
    "REPEAT('b',64) || '-2', 'unvalidated'), ",
    "('2000-01-01', 'station_0', '0', 2000, 'pm25', -9, 'negative', 'unvalidated'), ",
    "('2000-01-01 01:00:00', 'station_0', '0', 2000, 'pm25', 10, ",
    "REPEAT('a',64) || '-1', 'validated'), ",
    "('2000-01-01', 'negative_only', 'N', 2000, 'pm25', -5, 'negative', 'unvalidated'), ",
    "('2000-01-01', 'missing_only', 'M', 2000, 'pm25', NULL, 'missing', 'validated')"))

  .cdmx_aggregate_pollution(con, years = 2015:2000, include_source_metadata = TRUE)
  totals <- DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS records, SUM(value) AS total, ",
    "COUNT(DISTINCT year) AS years FROM aggr"))
  expect_equal(as.numeric(totals$records), 1280002)
  expect_equal(as.numeric(totals$years), 16)
  # Each original hundred sums to 4,950; only the means at hours zero and one change.
  expect_equal(totals$total, 12800 * 4950 + 15 + 4.5)
  first <- DBI::dbGetQuery(con, paste0(
    "SELECT value, source_status, source_ids FROM aggr ",
    "WHERE station='station_0' AND datetime=TIMESTAMP '2000-01-01'"))
  expect_equal(first$value, 15)
  expect_identical(first$source_status, "mixed")
  ids <- paste0(c(strrep("a", 64), strrep("b", 64)), c("-1", "-2"))
  expect_identical(first$source_ids, paste(ids, collapse = "|"))
  same_source <- DBI::dbGetQuery(con, paste0(
    "SELECT value, source_status, source_ids FROM aggr WHERE station='station_0' ",
    "AND datetime=TIMESTAMP '2000-01-01 01:00:00'"))
  expect_equal(same_source$value, 5.5)
  expect_identical(same_source$source_status, "validated")
  expect_identical(same_source$source_ids, ids[1])
  unavailable <- DBI::dbGetQuery(con, paste0(
    "SELECT value, source_status, source_ids FROM aggr ",
    "WHERE station IN ('negative_only','missing_only')"))
  expect_equal(nrow(unavailable), 2L)
  expect_true(all(is.na(unavailable)))

  DBI::dbExecute(con, "DROP TABLE aggr")
  .cdmx_aggregate_pollution(con, years = 2000L, include_source_metadata = FALSE)
  plain <- DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS records, COUNT(source_ids) AS ids, ",
    "COUNT(source_status) AS statuses ",
    "FROM aggr"))
  expect_equal(as.numeric(plain$records), 80002)
  expect_equal(as.numeric(plain$ids), 0)
  expect_equal(as.numeric(plain$statuses), 0)
})

test_that("the public CDMX reader retains cross-year station codes and hourly means", {
  source(here::here("src", "city_specific", "registry.R"), local = TRUE)
  source(here::here("src", "city_specific", "cdmx.R"), local = TRUE)
  root <- tempfile("cdmx-public-")
  inputs <- file.path(root, "sources")
  dir.create(inputs, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  files <- file.path(inputs, c("hidalgo__network__pm2_5__2021.csv",
    "hidalgo__network__pm2_5__2023.csv", "hidalgo__alias__pm2_5__2023.csv"))
  writeLines(c("Fecha,Hora,Parametro,AAA : Alpha",
    "2021-01-01,00,PM2.5,40", "2021-01-01,01,PM2.5,50"), files[1])
  writeLines(c("Fecha,Hora,Parametro,AAA : Alpha",
    "2023-01-01,00,PM2.5,10", "2023-01-01,00,PM2.5,20",
    "2023-01-01,00,PM2.5,-5"), files[2])
  writeLines(c("Fecha,Hora,Parametro,AAC : Alpha",
    "2023-01-01,00,PM2.5,100", "2023-01-01,01,PM2.5,200"), files[3])
  before <- tools::md5sum(files)
  stations <- sf::st_as_sf(data.frame(entity = "HGO", station = c("Alpha", "alias"),
    code = c("AAA", "AAC"), x = 0, y = 0), coords = c("x", "y"), crs = 4326)
  dataset <- cdmx_merge_pollution_data(primary_data_dir = inputs, stations_sf = stations,
    years = c(2021L, 2023L), out_dir = file.path(root, "derived"), out_name = "cdmx",
    cleanup = FALSE, verbose = FALSE, include_source_metadata = TRUE)
  panel <- data.table::as.data.table(dplyr::collect(dataset))
  expect_equal(nrow(panel), 2 * 8760)
  expect_identical(unique(panel$station_code), "AAA")
  hour <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  expect_equal(panel[datetime == hour, pm25], 15)
  expect_identical(panel[datetime == hour, pm25_source_status], "validated")
  expect_true(is.na(panel[datetime == hour + 3600, pm25]))
  expect_equal(sort(panel[year == 2021L & !is.na(pm25), pm25]), c(40, 50))
  expect_identical(tools::md5sum(files), before)
})
