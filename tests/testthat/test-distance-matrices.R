# A 3-4-5 km triangle checks distances independently of file-writing behavior.
test_that("distance computation returns normalized tables without saving checkpoints", {
  crs <- aeqd_crs(0, 0)
  stations <- sf::st_as_sf(
    data.frame(station = c('  "á"  ', "b", "c"), x = c(0, 3000, 0),
               y = c(0, 0, 4000)), coords = c("x", "y"), crs = crs)
  result <- compute_distance_matrices(
    stations_sf = stations, station_id_col = "station",
    evaluation_crs = crs, return_points = TRUE, quiet = TRUE)
  expect_equal(result$station_matrix$distance_km,
               c(0, 3, 4, 3, 0, 5, 4, 5, 0), tolerance = 1e-8)
  expect_identical(result$station_matrix$station_from, rep(c("A", "B", "C"), 3))
  expect_identical(result$station_matrix$station_to, rep(c("A", "B", "C"), each = 3))
  expect_null(result$geo_station_matrix)
  expect_null(result$representative_points)
  expect_equal(result$evaluation_crs, sf::st_crs(crs))
  expect_false(any(c("out_dir", "out_name", "overwrite") %in%
                     names(formals(compute_distance_matrices))))
  testthat::local_mocked_bindings(
    write_parquet = function(...) stop("Unexpected checkpoint write"), .package = "arrow")
  expect_no_error(compute_distance_matrices(stations, "station", quiet = TRUE))
})

test_that("distance writers round-trip both tables and preflight existing destinations", {
  root <- tempfile("distance-writer-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  stations <- sf::st_as_sf(data.frame(station = c("a", "b"), x = c(0, .02), y = 0),
                          coords = c("x", "y"), crs = 4326)
  polygon <- sf::st_polygon(list(matrix(c(0, 0, .01, 0, .01, .01, 0, .01, 0, 0),
                                        ncol = 2, byrow = TRUE)))
  geography <- sf::st_sf(geo_id = "0001", geometry = sf::st_sfc(polygon, crs = 4326))
  result <- compute_distance_matrices(stations, "station", geography, "geo_id",
                                       return_points = TRUE, quiet = TRUE)
  expect_false(dir.exists(root))
  original <- data.table::copy(result)
  paths <- write_distance_matrices(result, root)
  expect_named(paths, c("stations", "geography"))
  expect_identical(basename(unname(paths)), c("matrix_station_distances.parquet",
                                            "matrix_geo_station_distances.parquet"))
  expect_equal(data.table::as.data.table(arrow::read_parquet(paths[["stations"]])),
               result$station_matrix)
  expect_equal(data.table::as.data.table(arrow::read_parquet(paths[["geography"]])),
               result$geo_station_matrix)
  expect_identical(result$geo_station_matrix$geo_id, c("0001", "0001"))
  expect_equal(result, original)

  # One existing destination must prevent partial writes to the other one.
  before <- tools::md5sum(paths[["geography"]])
  unlink(paths[["stations"]])
  expect_error(write_distance_matrices(result, root, overwrite = FALSE),
               "overwrite = FALSE")
  expect_false(file.exists(paths[["stations"]]))
  expect_identical(tools::md5sum(paths[["geography"]]), before)
  expect_identical(write_distance_matrices(result, root), paths)
  expect_true(all(file.exists(paths)))

  station_only <- compute_distance_matrices(stations, "station", quiet = TRUE)
  station_paths <- write_distance_matrices(station_only, file.path(root, "stations"))
  expect_named(station_paths, "stations")
  expect_length(list.files(file.path(root, "stations")), 1L)
  expect_error(write_distance_matrices(list(), file.path(root, "invalid")))
  expect_false(dir.exists(file.path(root, "invalid")))
})

test_that("legacy distance metrics retain their equatorial spherical definitions", {
  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  stations <- sf::st_as_sf(data.frame(station = c("a", "b"), lon = c(0, 1), lat = 0),
                          coords = c("lon", "lat"), crs = 4326)
  geosphere <- compute_distance_matrices(stations, "station",
    distance_metric = "geosphere", quiet = TRUE)
  haversine <- compute_distance_matrices(stations, "station",
    distance_metric = "haversine", quiet = TRUE)
  expect_equal(geosphere$station_matrix$distance_km[2],
               6378137 * pi / 180 / 1000, tolerance = 1e-10)
  expect_equal(haversine$station_matrix$distance_km[2], 111.1951, tolerance = 1e-6)
  expect_identical(geosphere$station_matrix$station_from,
                   haversine$station_matrix$station_from)
})
