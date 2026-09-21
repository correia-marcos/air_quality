# Synthetic contracts for the optional three-city methodological analysis.
test_that("A preserves weights, annual counts, and the weighted variance identity", {
  fine <- data.table::data.table(geo_id = letters[1:4], y = c(0, 100, 20, 60),
                                 pop = c(9, 1, 2, 2))
  keys <- data.table::data.table(geo_id = letters[1:4], parent_id = c("x", "x", "y", "y"))
  a <- resolution_aggregate(fine, keys)
  expect_equal(a$y, c(10, 10, 40, 40))
  expect_equal(a$pop, fine$pop)
  expect_equal(resolution_variance(a$y0, a$pop),
    resolution_variance(a$y, a$pop) + sum(a$pop * (a$y0 - a$y)^2) / sum(a$pop))
  expect_true(is.na(resolution_variance(numeric(), numeric())))
  expect_error(resolution_aggregate(rbind(fine, fine[1]), keys), "unique")
  # Averaging two annual exceedance indicators is not thresholding a mean exposure.
  annual <- resolution_aggregate(data.table::data.table(geo_id = c("a", "b"),
    y = c(0, 1), pop = 1), keys[1:2])
  expect_equal(annual$y, c(.5, .5))
  expect_false(annual$y[1] == as.numeric(mean(c(0, 200)) >= 150))
})

test_that("crosswalk auditing detects non-nested branches without merging them", {
  k <- data.table::data.table(geo_id = rep(letters[1:4], 2),
    level = rep(c("sector", "locality"), each = 4),
    parent_id = c("x", "x", "y", "y", "a", "b", "b", NA))
  expect_equal(resolution_validate_keys(k, list(c("sector", "locality")))$
                 crossing_children, 1L)
  expect_error(resolution_validate_keys(rbind(k, k[1])), "one nonmissing")
  expect_error(resolution_validate_keys(transform(k, parent_id = "")), "Empty")
})

test_that("classification weights education correctly and leaves empty quintiles", {
  census <- data.table::data.table(geo_id = letters[1:4], pop_total = c(80, 10, 5, 5),
    pop_educ_known = c(8, 10, 0, 5), education_mean = c(2, 10, NA, 15))
  k <- data.table::data.table(geo_id = letters[1:4], level = "fine",
    parent_id = letters[1:4])
  class <- resolution_classify(census, k)
  expect_true(is.na(class[geo_id == "c", area_quintile]))
  expect_lt(data.table::uniqueN(class$area_quintile, na.rm = TRUE), 5)
  cells <- data.table::data.table(geo_id = c("a", "b"), edu_quintile = c(1L, 5L),
                                  person_weight = c(3, 1))
  assignment <- data.table::data.table(geo_id = c("a", "b"), parent_id = "p", y = c(10,
    0))
  e <- resolution_estimate(cells, assignment, TRUE)
  expect_equal(e$contrast$gap, 10)
  expect_true(is.na(e$contrast$normalized_pct))
  expect_equal(e$profile$population, c(3, 0, 0, 0, 1))
  expect_equal(e$contrast$inference_status, "insufficient_cluster_support")
  empty <- resolution_estimate(cells[1], assignment)
  expect_true(is.na(empty$contrast$gap))
  expect_equal(empty$contrast$inference_status, "missing_endpoint_group")
})

test_that("parent bootstrap duplicates clusters once, with genuinely paired endpoints", {
  cells <- data.table::data.table(geo_id = letters[1:4], edu_quintile = c(1, 5, 1, 5),
                                  person_weight = c(2, 1, 1, 3))
  pair <- data.table::data.table(geo_id = letters[1:4], parent_id = c("x", "x", "y", "y"),
                                 y0 = c(8, 2, 10, 4), y1 = c(6, 6, 5.5, 5.5))
  draws <- matrix(c(1, 1, 1, 2, 2, 2), 2)
  boot <- resolution_bootstrap(cells, pair, draws = draws)
  expect_equal(boot$replicates$baseline[c(1, 3)], c(6, 6))
  expect_equal(boot$replicates$comparison[c(1, 3)], c(0, 0))
  expect_equal(boot$replicates$delta[c(1, 3)], c(-6, -6))
  expect_equal(boot$replicates$baseline[2], 26 / 3 - 14 / 4)
  expect_equal(boot$replicates$comparison[2], 17.5 / 3 - 22.5 / 4)
  expect_identical(resolution_bootstrap(cells, pair, 10)$replicates,
                   resolution_bootstrap(cells, pair, 10)$replicates)
  expect_error(resolution_bootstrap(cells, pair, draws = matrix(0, 2, 2)), "Invalid")
})

test_that("absolute gaps and HC1 match the existing saturated regression", {
  cells <- data.table::CJ(geo_id = sprintf("g%03d", 1:80), edu_quintile = 1:5)
  cells[, person_weight := 1 + ((seq_len(.N) * 7L) %% 13L)]
  a <- data.table::data.table(geo_id = sprintf("g%03d", 1:80),
    parent_id = sprintf("g%03d", 1:80), y = (1:80)^.5)
  exp <- a[, .(geo_id, year = 2023L, avg_pm10 = y)]
  reg <- compute_exposure_regressions(exp, cells, normalized = FALSE, quiet = TRUE,
                                      outcome_pattern = "^avg_pm10$")
  e <- resolution_estimate(cells, a, TRUE)$contrast
  expect_equal(e$gap, reg[group == 1, estimate], tolerance = 1e-10)
  expect_equal(e$se, reg[group == 1, std_error], tolerance = 1e-10)
  # Scale aggregation can reverse or increase a quintile contrast; no monotone-gap test.
  fine <- data.table::data.table(geo_id = letters[1:4], y = c(0, 10, 8, 2), pop = 1)
  k <- data.table::data.table(geo_id = letters[1:4], parent_id = c("x", "y", "x", "y"))
  c <- data.table::data.table(geo_id = letters[1:4], edu_quintile = c(1, 5, 5, 1),
                             person_weight = 1)
  coarse <- resolution_aggregate(fine, k)
  g0 <- resolution_estimate(c, fine[, .(geo_id, parent_id = geo_id, y)])$contrast$gap
  g1 <- resolution_estimate(c, coarse)$contrast$gap
  expect_equal(g0, -8)
  expect_equal(g1, 0)
  expect_lt(resolution_variance(coarse$y, coarse$pop), resolution_variance(fine$y,
    fine$pop))
})

test_that("matrix diagnostics renormalize hourly, enforce the boundary, and reconcile", {
  root <- tempfile(); dir.create(root)
  fx <- make_toy_fixture(root)
  d <- data.table::as.data.table(arrow::read_parquet(fx$dist_pq))
  p <- data.table::as.data.table(arrow::read_parquet(file.path(fx$arrow,
                                                "year=2023", "part-0.parquet")))
  p[, station := normalize_station(station)]
  m <- resolution_matrix_diagnostics(d, p, "pm10")
  expect_equal(m$units[geo_id == "g1", avg_reconstructed], 60)
  expect_equal(m$units[geo_id == "g2", avg_reconstructed], (132 + 70 / 1.5) / 2)
  expect_equal(m$units[geo_id == "g1", static_neff], 1.8)
  expect_equal(m$units[geo_id == "g1", hourly_neff_mean], 1.4)
  expect_equal(m$units[geo_id == "g3", eligible_stations], 0)
  expect_equal(m$units[geo_id == "g3", zero_weight_hours], 3)
  expect_equal(m$units[geo_id == "g1", static_row_sum], 1)
  expect_equal(m$units[geo_id == "g1", hourly_row_sum_error], 0, tolerance = 1e-14)
  expect_equal(m$units[geo_id == "g2", it2_reconstructed], 1)
  expect_true(all(m$units[eligible_stations > 0, it1_lower <= it1_reconstructed &
                           it1_upper >= it1_reconstructed]))
  d[geo_id == "g1" & station_id == "s3", distance_km := 3]
  expect_equal(resolution_matrix_diagnostics(d, p, "pm10")$units[
    geo_id == "g1", eligible_stations], 3)
  expect_error(resolution_matrix_diagnostics(rbind(d, d[1]), p, "pm10"), "Duplicate")
  resolution_write(m$units, file.path(root, "metrics"))
  expect_equal(data.table::as.data.table(arrow::read_parquet(file.path(root,
    "metrics.parquet"))), m$units)
})

test_that("IDW preserves supplied groups including schooling ties and validates them", {
  root <- tempfile(); dir.create(root); fx <- make_toy_fixture(root)
  census <- data.table::data.table(geo_id = c("g1", "g2", "g3"), adult = 1L,
    person_weight = c(100, 200, 50), education = 10, frozen = c(5L, 1L, NA_integer_))
  out <- aggregate_idw_exposure(fx$arrow, fx$dist_pq, census, group_var = "education",
    quintile_level = "individual", precomputed_group_col = "frozen", target_years = 2023L,
    pollutants = "pm10", mem_gb = 1, n_threads = 1, out_dir = file.path(root, "out"),
    out_name = "frozen", quiet = TRUE, return_data = TRUE, chunk_by_month = FALSE)
  expect_equal(out$individual_quintiles$edu_quintile, census$frozen)
  census$frozen[1] <- 6L
  expect_error(aggregate_idw_exposure(fx$arrow, fx$dist_pq, census,
    group_var = "education", precomputed_group_col = "frozen", out_name = "bad"),
    "Precomputed groups")
})

test_that("fixed evaluation CRS and returned points preserve default distances", {
  square <- function(x) sf::st_polygon(list(matrix(c(x, 0, x + .01, 0, x + .01, .01,
    x, .01, x, 0), ncol = 2, byrow = TRUE)))
  geo <- sf::st_sf(geo_id = c("a", "b"), geometry = sf::st_sfc(square(0), square(.01),
                                                                            crs = 4326))
  stations <- sf::st_as_sf(data.frame(station = "s", x = .005, y = .005),
                          coords = c("x", "y"), crs = 4326)
  root <- tempfile(); dir.create(root)
  old <- compute_distance_matrices(stations, "station", geo, "geo_id", root,
                                    "default", quiet = TRUE, return_points = TRUE)
  fixed <- compute_distance_matrices(stations, "station", geo, "geo_id", root,
    "fixed", quiet = TRUE, evaluation_crs = old$evaluation_crs, return_points = TRUE)
  expect_equal(old$geo_station_matrix, fixed$geo_station_matrix)
  expect_equal(nrow(fixed$representative_points), 2)
  cached <- compute_distance_matrices(stations, "station", geo, "geo_id", root,
    "fixed", quiet = TRUE, overwrite = FALSE)
  expect_named(cached, c("station_matrix", "geo_station_matrix"))
  expect_error(compute_distance_matrices(stations, "station", geo, "geo_id", root,
    "fixed", quiet = TRUE, overwrite = FALSE, return_points = TRUE), "overwrite")
  expect_error(compute_distance_matrices(stations, "station", geo, "geo_id", root,
    "bad", evaluation_crs = 4326), "projected metre")
  union <- resolution_dissolve(geo, data.table::data.table(geo_id = c("a", "b"),
                                                          parent_id = "p"))
  expect_equal(nrow(union), 1)
  expect_true(all(sf::st_is_valid(union)))
  expect_equal(union$geo_id, "p")
})

test_that("extracted Bogota reference functions retain the historical joint design", {
  cells <- data.table::CJ(geo_id = sprintf("g%03d", 1:60), edu_quintile = 1:5)
  cells[, person_weight := 1 + seq_len(.N) %% 7]
  fine <- data.table::data.table(geo_id = sprintf("g%03d", 1:60),
                                 year = 2023L, avg_pm10 = 1:60)
  pop <- cells[, .(pop = sum(person_weight)), by = geo_id]
  keys <- fine[, .(geo_id, fine = geo_id)]
  reference <- resolution_reference_run("avg_pm10", "fine", cells, "A", fine,
    pop, keys, 2023L, "toy", 3L)
  a <- resolution_aggregate(fine[, .(geo_id, y = avg_pm10, pop = pop$pop)],
    keys[, .(geo_id, parent_id = fine)])
  direct <- resolution_estimate(cells, a)$contrast
  expect_equal(reference[group == 1, estimate], direct$normalized_pct / 100)
  drawn <- merge(fine[, .(geo_id, y = avg_pm10)], pop, by = "geo_id")
  drawn[, `:=`(.draw = seq_len(.N), fine = geo_id)]
  dc <- merge(cells, drawn[, .(geo_id, .draw)], by = "geo_id")
  shortcuts <- resolution_reference_gaps(drawn, dc,
    data.table::data.table(level = "fine"))
  expect_equal(shortcuts[group == 1, gap], direct$normalized_pct / 100)
})

test_that("a nested ladder can have non-monotonic gaps while variance declines", {
  fine <- data.table::data.table(geo_id = letters[1:4], y = c(0, 10, 8, 2), pop = 1)
  cells <- data.table::data.table(geo_id = letters[1:4], edu_quintile = c(1, 1, 5, 5),
                                  person_weight = 1)
  mid <- resolution_aggregate(fine, data.table::data.table(geo_id = letters[1:4],
    parent_id = c("x", "x", "x", "y")))
  coarse <- resolution_aggregate(fine, data.table::data.table(geo_id = letters[1:4],
    parent_id = "all"))
  a0 <- fine[, .(geo_id, parent_id = geo_id, y)]
  gaps <- vapply(list(a0, mid, coarse), function(x) {
    resolution_estimate(cells, x)$contrast$gap
  }, numeric(1))
  expect_equal(gaps, c(0, 2, 0))
  v <- vapply(list(fine, mid, coarse), function(x) {
    resolution_variance(x$y, x$pop)
  }, numeric(1))
  expect_true(all(diff(v) < 0))
})
