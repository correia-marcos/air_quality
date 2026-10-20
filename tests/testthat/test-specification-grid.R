# Synthetic contracts for the Design A grid, its decomposition and the specification panel.

# Three fine units, two parents: f1 (pop 10, y 10) and f2 (pop 30, y 20) in P1, f3 (pop 20,
# y 40) in P2. Cells: f1 group 1 (5); f2 group 2 (10); f3 groups 1 and 2 (4 each).
design_a_toy <- function() {
  list(exposure = data.table::data.table(geo_id = c("f1", "f2", "f3"),
                                         y = c(10, 20, 40)),
       population = data.table::data.table(geo_id = c("f1", "f2", "f3"),
                                           pop = c(10, 30, 20)),
       cells = data.table::data.table(geo_id = c("f1", "f2", "f3", "f3"),
                                      edu_quintile = c(1L, 2L, 1L, 2L),
                                      person_weight = c(5, 10, 4, 4)),
       keys = data.table::data.table(geo_id = c("f1", "f2", "f3"),
                                     parent_id = c("P1", "P1", "P2")))
}

test_that("Design A averages over covered units and reproduces the hand values", {
  toy <- design_a_toy()
  e <- design_a_estimate(toy$exposure, c("f1", "f2", "f3"), toy$population, toy$cells,
                         toy$keys, "y", groups = 1:2)
  # P1 mean (10*10 + 20*30) / 40 = 17.5; group 1 (5*17.5 + 4*40) / 9; group 2 /14.
  expect_equal(e$contrast$low_mean, 247.5 / 9)
  expect_equal(e$contrast$high_mean, 335 / 14)
  expect_equal(e$contrast$gap, 247.5 / 9 - 335 / 14)
  # Variances 125 (fine) and 112.5 (supports), so 90% retained and within = 12.5.
  expect_equal(e$contrast$share_variance_retained, .9)
  expect_equal(e$contrast$variance_identity_error, 0)
  expect_equal(e$contrast$sd0, sqrt(125))

  # Restricting the sample to f1 and f3 re-averages P1 over f1 alone.
  r <- design_a_estimate(toy$exposure, c("f1", "f3"), toy$population, toy$cells,
                         toy$keys, "y", groups = 1:2)
  expect_equal(r$contrast$gap, (5 * 10 + 4 * 40) / 9 - 40)
  expect_setequal(r$assignment$geo_id, c("f1", "f3"))
})

test_that("the Design A sample needs finite exposure, adults and a full parent chain", {
  toy <- design_a_toy()
  toy$exposure[geo_id == "f2", y := NA]
  expect_equal(design_a_sample(toy$exposure, toy$population, c("f1", "f2", "f3"), "y"),
               c("f1", "f3"))
  expect_equal(design_a_sample(toy$exposure, toy$population, c("f1", "f2"), "y"), "f1")
  toy$population[geo_id == "f1", pop := 0]
  expect_equal(design_a_sample(toy$exposure, toy$population, c("f1", "f2", "f3"), "y"),
               "f3")
})

test_that("the fixed-sample decomposition adds up with one sample term zero", {
  # b < 3: the common sample is the specification's; b > 3: it is the reference's.
  smaller <- design_a_decomposition(1.8, 2.1, 1.8, 1.6)
  expect_equal(smaller$sample_expansion, 0)
  expect_equal(smaller$exposure_change, .2)
  expect_equal(smaller$sample_contraction, .5)
  larger <- design_a_decomposition(1.66, 2.11, 1.62, 2.11)
  expect_equal(larger$sample_contraction, 0)
  expect_equal(larger$exposure_change + larger$sample_expansion, larger$total_change)
  same <- design_a_decomposition(2.1, 2.1, 2.1, 2.1)
  expect_equal(unlist(same[, .(total_change, exposure_change, sample_expansion)]),
               c(total_change = 0, exposure_change = 0, sample_expansion = 0))
  expect_true(all(abs(rbind(smaller, larger, same)$identity_error) < 1e-12))
})

test_that("station weights: eligibility nests in the buffer and ignores the exponent", {
  d <- data.table::data.table(geo_id = c("g1", "g2", "g2", "g3", "g3", "g4", "g5"),
    station_id = c("s1", "s1", "s2", "s1", "s2", "s1", "s3"),
    distance_km = c(1, 1, 1, 1, 2, 5, 1))
  w1 <- idw_station_weights(d, active = c("s1", "s2"), buffer_km = 3, distance_power = 1)
  w2 <- idw_station_weights(d, active = c("s1", "s2"), buffer_km = 3, distance_power = 2)
  data.table::setkey(w1, geo_id); data.table::setkey(w2, geo_id)
  expect_equal(w1$geo_id, c("g1", "g2", "g3"))  # g4 beyond 3 km; s3 never reported
  expect_identical(w1$geo_id, w2$geo_id)
  expect_equal(w1["g1"]$static_neff, 1)
  expect_equal(w1["g2"]$static_neff, 2)
  expect_equal(w1["g3"]$static_neff, 1.5^2 / 1.25)
  expect_equal(w2["g3"]$static_neff, 1.25^2 / (1 + 1 / 16))
  expect_lt(w2["g3"]$static_neff, w1["g3"]$static_neff)
  near <- idw_station_weights(d, c("s1", "s2"), 1, 1)$geo_id
  far  <- idw_station_weights(d, c("s1", "s2"), 5, 1)$geo_id
  expect_true(all(near %in% w1$geo_id) && all(w1$geo_id %in% far))
})

test_that("the grid anchor flags value and coverage differences", {
  a <- data.table::data.table(geo_id = c("u1", "u2"), avg_pm10 = c(10, 20))
  expect_true(all(idw_grid_anchor(a, data.table::copy(a), "avg_pm10")$passed))
  b <- data.table::copy(a)[geo_id == "u2", avg_pm10 := 20.1]
  expect_false(idw_grid_anchor(a, b, "avg_pm10")$passed)
  expect_false(idw_grid_anchor(a, a[1], "avg_pm10")$passed)
})

test_that("the variance decomposition is exact on a complete grid", {
  additive <- data.table::CJ(a = 1:3, b = 1:2)
  additive[, y := c(1, 2, 3)[a] + c(0, 10)[b]]
  v <- specification_variance_decomposition(additive, c("a", "b"), "y")
  expect_equal(sum(v$indices$first_order), 1)
  expect_equal(v$indices$total, v$indices$first_order)
  expect_lt(abs(v$residual), 1e-10)

  interaction <- data.table::CJ(a = c(-1, 1), b = c(-1, 1))
  interaction[, y := a * b]
  v <- specification_variance_decomposition(interaction, c("a", "b"), "y")
  expect_equal(v$indices$first_order, c(0, 0))
  expect_equal(v$indices$total, c(1, 1))
  expect_error(specification_variance_decomposition(additive[-1], c("a", "b"), "y"),
               "incomplete")
})

test_that("baselines: one paper row per city and outcome, zero self-deltas", {
  panel <- data.table::CJ(city_id = "c", outcome = "avg_pm10",
                          grouping = c("edu_quintile", "edu_group3"),
                          buffer_km = c(3, 20), distance_power = 1, level_rank = 1:2)
  panel[, `:=`(gap = seq_len(.N), sd0 = 2)]
  x <- specification_add_baselines(panel)
  expect_equal(x[n_choices_vs_paper == 0, .N], 1L)
  expect_equal(x[n_choices_vs_paper == 0, delta_vs_paper], 0)
  expect_true(all(x[level_rank == 1, delta_resolution] == 0))
  expect_equal(x$gap_paper_sd, x$gap / 2)
  expect_equal(max(x$n_choices_vs_paper), 3)
})

test_that("the grid is comparison-only: the manuscript keeps exponent 1", {
  settings <- new.env()
  sys.source(here::here("config", "analysis_settings.R"), settings)
  expect_equal(settings$idw_distance_power, 1)
  expect_true(3 %in% settings$resolution_grid_buffers_km)
  files <- c(list.files(here::here("config", "targets"), full.names = TRUE),
             here::here("_targets.R"),
             here::here("scripts", "process_data", "estimate_idw.R"))
  used <- vapply(files, function(f) any(grepl("resolution_grid_", readLines(f))),
                 logical(1))
  expect_false(any(used))
})
