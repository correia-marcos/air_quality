# Synthetic contracts for the resolution review summaries and the buffer option.
test_that("another buffer widens eligibility, keeps d > 0, and never reuses 3 km outputs", {
  root <- tempfile(); dir.create(root)
  fx <- make_toy_fixture(root)
  d <- data.table::as.data.table(arrow::read_parquet(fx$dist_pq))
  p <- data.table::as.data.table(arrow::read_parquet(file.path(fx$arrow,
                                                "year=2023", "part-0.parquet")))
  p[, station := normalize_station(station)]
  three <- resolution_matrix_diagnostics(d, p, "pm10", buffer_km = 3)
  five <- resolution_matrix_diagnostics(d, p, "pm10", buffer_km = 5)
  expect_equal(three$units[geo_id == "g1", eligible_stations], 2)
  expect_equal(five$units[geo_id == "g1", eligible_stations], 3)
  # The station at zero distance stays excluded whatever the buffer.
  expect_equal(five$units[geo_id == "g3", eligible_stations], 0)
  expect_equal(resolution_buffer_root(3),
               here::here("data", "processed", "resolution_sensitivity"))
  expect_equal(resolution_buffer_root(20), here::here("data", "processed",
                                         "resolution_sensitivity", "buffer_20km"))
})

test_that("weighted quantiles follow the weighted-median convention", {
  expect_equal(resolution_weighted_quantile(c(4, 1, 3, 2), rep(1, 4), c(.1, .5, .9)),
               c(1, 2, 4))
  expect_equal(resolution_weighted_quantile(c(1, 10), c(1, 3), .5), 10)
  expect_true(is.na(resolution_weighted_quantile(numeric(), numeric(), .5)))
})

test_that("unit composition keeps individual quintiles and design coverage apart", {
  keys <- data.table::data.table(geo_id = c("a", "b", "c"), level = "comuna",
                                 parent_id = c("x", "x", "y"))
  cells <- data.table::data.table(geo_id = c("a", "a", "b", "c"),
    edu_quintile = c(1L, 5L, 5L, 1L), person_weight = c(2, 1, 1, 4))
  census <- data.table::data.table(geo_id = c("a", "b", "c"),
    pop_educ_known = c(3, 1, 4), education_mean = c(8, 16, 6))
  x <- resolution_unit_composition(cells, keys, "comuna", census,
                                   a_fine_ids = "a", b_parent_ids = "y")
  expect_equal(nrow(x), 10)
  expect_equal(x[parent_id == "x" & edu_quintile == 5, share], .5)
  expect_equal(x[parent_id == "x", unique(a_share)], .75)
  expect_equal(x[parent_id == "x", unique(education_mean)], 10)
  expect_equal(x[parent_id == "y", unique(b_covered)], TRUE)
  expect_equal(x[, sum(population)], sum(cells$person_weight))
  population <- data.table::data.table(geo_id = c("a", "b", "c"), pop = c(4, 1, 5))
  w <- resolution_level_weights(population, cells, keys, "comuna")
  expect_equal(w[group == "All adults" & geo_id == "x", weight], 5)
  expect_equal(w[group == "Q5" & geo_id == "x", weight], 2)
})

test_that("sample shares start from every reporting adult and sum to one", {
  cells <- data.table::data.table(geo_id = "a", edu_quintile = 1:5, person_weight = 2)
  profiles <- data.table::data.table(design = "A", level = "fine", outcome = "avg_pm10",
    edu_quintile = 1:5, population = c(4, 1, 1, 1, 3))
  s <- resolution_sample_shares(profiles, cells, "avg_pm10")
  expect_equal(s[design == "All adults", Q1], .2)
  expect_equal(s[design == "A", Q5], .3)
  expect_equal(s[design == "A", population], 10)
  expect_equal(rowSums(s[, paste0("Q", 1:5), with = FALSE]), c(1, 1))
})

test_that("monitoring counts include co-located stations but IDW coverage does not", {
  d <- data.table::data.table(geo_id = c("u", "u", "v", "w"),
    station_id = c("s1", "s2", "s1", "s3"), distance_km = c(0, 2.5, 4, 1))
  units <- resolution_monitoring_units(d, active_ids = c("s1", "s2"),
                                       radii_km = c(1, 3, 5, 10, 20))
  expect_equal(units[geo_id == "u", n_within_1km], 1)
  expect_equal(units[geo_id == "u", zero_distance_pairs], 1)
  expect_true(units[geo_id == "u", covered_idw])
  expect_false(units[geo_id == "v", covered_idw])
  expect_false("w" %in% units$geo_id)
  weights <- data.table::data.table(geo_id = c("u", "v"), group = "All adults",
                                    weight = c(1, 3))
  s <- resolution_monitoring_summary(units, weights, radii_km = c(1, 3, 5, 10, 20))
  expect_equal(s$share_within_3km, .25)
  expect_equal(s$share_within_5km, 1)
  expect_equal(s$nearest_median_km, 4)
  expect_equal(s$mean_stations_3km, .5)
  expect_true(is.na(s$observed_hour_share))
})

test_that("buffer comparison aligns designs and reports the gap change", {
  base <- data.table::data.table(city_id = "c", design = "B_native", level = "fine",
    outcome = "avg_pm10", gap = 2, normalized_pct = 10, q1 = 22, q5 = 20,
    population = 10, n_units = 3, n_clusters = 3)
  alternative <- data.table::copy(base)[, `:=`(gap = 1.5, population = 20)]
  x <- resolution_buffer_comparison(base, alternative)
  expect_equal(x$gap_change, -.5)
  expect_equal(x$population_20km, 20)
  # Education-level contrasts carry low_mean/high_mean instead of q1/q5.
  data.table::setnames(base, c("q1", "q5"), c("low_mean", "high_mean"))
  data.table::setnames(alternative, c("q1", "q5"), c("low_mean", "high_mean"))
  x <- resolution_buffer_comparison(base, alternative, means = c("low_mean", "high_mean"))
  expect_equal(x$high_mean_20km, 20)
  expect_equal(x$gap_change, -.5)
})

test_that("headline summary reports the shares of the lowest and highest groups", {
  contrasts <- data.table::data.table(city_id = "c", design = "A", level = "fine",
    outcome = "avg_pm10", gap = 2, normalized_pct = 10, population = 10)
  profiles <- data.table::data.table(city_id = "c", design = "A", level = "fine",
    outcome = "avg_pm10", edu_group3 = 1:3, share = c(.5, .3, .2))
  x <- resolution_headline_summary(contrasts, profiles, "edu_group3")
  expect_equal(c(x$low_share, x$high_share, x$gap), c(.5, .2, 2))
  expect_true("edu_group3" %in% names(profiles))
})

test_that("buffer plot data keeps native B of cities run at both buffers", {
  base <- data.table::data.table(city_id = c("a", "a", "b"),
    design = c("B_native", "A", "B_native"), level = "fine",
    outcome = c("avg_pm10", "avg_pm10", "hrs_d_pm25_it2"), gap = 1:3, population = 1)
  alternative <- base[city_id == "a"]
  levels <- data.table::data.table(city_id = c("a", "b"), level = "fine",
                                   label = "Fine census unit")
  x <- resolution_buffer_plot_data(base, alternative, levels,
    city_labels = c(a = "City A", b = "City B"), support_order = "Fine census unit")
  expect_equal(nrow(x), 2L)
  expect_equal(levels(x$buffer), c("3 km", "20 km"))
  expect_equal(unique(x$panel), "City A: annual mean")
  expect_equal(unique(x$pollutant), "pm10")
})

test_that("tie splits report quintile cuts inside one schooling value by geo_id", {
  groups <- data.table::data.table(geo_id = c("a", "b", "c", "d", "e"),
    educ_years = c(5, 8, 9, 11, 12), person_weight = 1, adult = 1L)
  assign_socio_group(groups, "educ_years", "person_weight", 5L, "edu_quintile")
  ties <- resolution_tie_splits(groups)
  expect_equal(nrow(ties), 0)
  groups <- data.table::data.table(geo_id = c("a", "b", "c", "d"), educ_years = 8,
    person_weight = 1, adult = 1L, edu_quintile = c(1L, 1L, 2L, 2L))
  ties <- resolution_tie_splits(groups)
  expect_equal(ties$share_of_value, c(.5, .5))
  expect_equal(ties$first_geo_id, c("a", "c"))
  expect_equal(ties$share_of_all_adults, c(.5, .5))
})
