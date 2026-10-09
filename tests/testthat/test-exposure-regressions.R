# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Golden-value tests for compute_exposure_regressions() (pure: data.table only,
#   plus sandwich for the clustered-interval test).
#
#' @Description: Checks the estimator against hand-computed weighted group means on a
#   4-geo-unit x 2-group toy: the saturated-model identity (the group coefficient is the
#   difference of lambda-weighted group means), the zero-pinned base row, releveling to a
#   non-default base group, exact invariance of the t-statistic to the normalization, the
#   G <= k guard (NA standard errors with a warning), and the t(G-1) critical value.
#   All fixtures are small enough to verify by counting on one hand.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

# Toy: geo units g1..g4 with exposure y = 1, 2, 4, 0.5 (constant within a unit).
# Group-1 populations 10, 30, 20, 40 and group-2 populations 20, 10, 10, 60, so
# both group totals are 100 and the lambda-weighted group means are
#   q1: (10*1 + 30*2 + 20*4 + 40*0.5)/100 = 1.7
#   q2: (20*1 + 10*2 + 10*4 + 60*0.5)/100 = 1.1
make_reg_toy <- function() {
  exposure <- data.table::data.table(
    geo_id = paste0("g", 1:4), year = 2023L,
    hrs_d_pm10_it1 = c(1, 2, 4, 0.5)
  )
  individual <- data.table::data.table(
    geo_id        = rep(paste0("g", 1:4), each = 2),
    edu_quintile  = rep(1:2, times = 4),
    person_weight = c(10, 20, 30, 10, 20, 10, 40, 60)
  )
  list(exposure = exposure, individual = individual)
}

test_that("group coefficient equals the difference of lambda-weighted means", {
  toy <- make_reg_toy()

  raw <- compute_exposure_regressions(
    toy$exposure, toy$individual, group_values = 1:2, base_group = 2L,
    pollutants = "pm10", year_filter = 2023, normalized = FALSE,
    se_type = "classic", quiet = TRUE
  )

  expect_equal(raw$estimate[raw$group == 1], 1.7 - 1.1, tolerance = 1e-12)

  # Normalization divides estimate and SE by the base mean (1.1), so the
  # t-statistic is exactly invariant to it.
  norm <- compute_exposure_regressions(
    toy$exposure, toy$individual, group_values = 1:2, base_group = 2L,
    pollutants = "pm10", year_filter = 2023, normalized = TRUE,
    se_type = "classic", quiet = TRUE
  )

  expect_equal(norm$estimate[norm$group == 1], 1.7 / 1.1 - 1, tolerance = 1e-12)
  t_raw  <- raw$estimate[1] / raw$std_error[1]
  t_norm <- norm$estimate[1] / norm$std_error[1]
  expect_equal(t_norm, t_raw, tolerance = 1e-10)
})

test_that("base group row is pinned at zero, and releveling moves it", {
  toy <- make_reg_toy()

  fit2 <- compute_exposure_regressions(
    toy$exposure, toy$individual, group_values = 1:2, base_group = 2L,
    pollutants = "pm10", year_filter = 2023, normalized = TRUE,
    se_type = "classic", quiet = TRUE
  )
  base_row <- fit2[group == 2]
  expect_equal(unlist(base_row[, .(estimate, std_error, ci_low, ci_high)]),
               c(estimate = 0, std_error = 0, ci_low = 0, ci_high = 0))

  fit1 <- compute_exposure_regressions(
    toy$exposure, toy$individual, group_values = 1:2, base_group = 1L,
    pollutants = "pm10", year_filter = 2023, normalized = TRUE,
    se_type = "classic", quiet = TRUE
  )
  expect_equal(fit1$estimate[fit1$group == 1], 0)
  expect_equal(fit1$estimate[fit1$group == 2], 1.1 / 1.7 - 1, tolerance = 1e-12)
})

test_that("G <= k clusters refuses clustered SEs with a warning", {
  # 5 geo units x 5 groups: G = 5 clusters for k = 5 coefficients.
  exposure <- data.table::data.table(
    geo_id = paste0("g", 1:5), year = 2023L, hrs_d_pm10_it1 = 1:5 + 0.5
  )
  individual <- data.table::data.table(
    geo_id        = rep(paste0("g", 1:5), each = 5),
    edu_quintile  = rep(1:5, times = 5),
    person_weight = 10 * 1:25
  )

  expect_warning(
    res <- compute_exposure_regressions(
      exposure, individual, group_values = 1:5, base_group = 5L,
      pollutants = "pm10", year_filter = 2023, normalized = TRUE,
      se_type = "cluster_geo", quiet = TRUE
    ),
    "not identified"
  )

  # Coefficients are still fit; only the clustered SEs and intervals are NA.
  expect_false(any(is.na(res$estimate)))
  expect_true(all(is.na(res$std_error[res$group != 5])))
  expect_equal(unique(res$n_clusters), 5L)
  expect_equal(unique(res$n_coef), 5L)
})

test_that("clustered intervals use the t(G-1) critical value", {
  skip_if_not_installed("sandwich")

  # 6 geo units x 5 groups: G = 6 > k = 5, so the sandwich is computed and the
  # critical value must be qt(0.975, 5).
  exposure <- data.table::data.table(
    geo_id = paste0("g", 1:6), year = 2023L,
    hrs_d_pm10_it1 = 1:6 + 0.25
  )
  individual <- data.table::data.table(
    geo_id        = rep(paste0("g", 1:6), each = 5),
    edu_quintile  = rep(1:5, times = 6),
    person_weight = 5 + (1:30) %% 7 * 11
  )

  res <- compute_exposure_regressions(
    exposure, individual, group_values = 1:5, base_group = 5L,
    pollutants = "pm10", year_filter = 2023, normalized = TRUE,
    se_type = "cluster_geo", quiet = TRUE
  )

  crit <- stats::qt(0.975, 5)
  non_base <- res[group != 5]
  expect_equal(non_base$ci_high - non_base$estimate,
               crit * non_base$std_error, tolerance = 1e-10)
  expect_equal(non_base$estimate - non_base$ci_low,
               crit * non_base$std_error, tolerance = 1e-10)
})

# Six bands as the city modules build them: Bogota 2018 codes secondary complete as 11,
# Sao Paulo credits partial college as 12; both are the secondary-complete band.
make_level_toy <- function() {
  bands <- c("no_education", "high_school_incomplete", "high_school_complete",
             "college_incomplete", "college_complete", "graduate_educ")
  dt <- data.table::data.table(educ_years = c(0, 5, 11, 12, 14, 17, 19, NA))
  band <- c(1L, 2L, 3L, 3L, 4L, 5L, 6L, NA)
  for (k in seq_along(bands)) {
    data.table::set(dt, j = bands[k], value = as.numeric(band == k))
  }
  list(dt = dt, bands = bands)
}

test_that("education levels follow the city bands and leave missing schooling NA", {
  toy <- make_level_toy()
  assign_education_level(toy$dt, toy$bands)

  expect_identical(toy$dt$edu_level, c(1L, 2L, 3L, 3L, 4L, 5L, 6L, NA))
})

test_that("a reporting row outside exactly one band stops level assignment", {
  toy <- make_level_toy()
  data.table::set(toy$dt, i = 3L, j = "college_complete", value = 1)
  expect_error(assign_education_level(toy$dt, toy$bands), "exactly one")

  toy <- make_level_toy()
  data.table::set(toy$dt, i = 3L, j = "high_school_complete", value = 0)
  expect_error(assign_education_level(toy$dt, toy$bands), "exactly one")
})

test_that("split quintile cells share tied values in proportion, whatever the geo order", {
  # Values 5, 5, 5, 9 with unit weights and two groups: value 5 spans (0, .75], so
  # each adult with 5 puts 2/3 in group 1 and 1/3 in group 2; value 9 is all group 2.
  dt <- data.table::data.table(geo_id = c("d", "a", "c", "b"), educ_years = c(5, 5, 5, 9),
                               person_weight = 1)
  cells <- split_socio_group_cells(dt, "educ_years", "person_weight", 2L, "g")
  data.table::setkey(cells, geo_id, g)
  expect_equal(cells[geo_id %in% c("a", "c", "d") & g == 1L, person_weight], rep(2 / 3, 3))
  expect_equal(cells[geo_id %in% c("a", "c", "d") & g == 2L, person_weight], rep(1 / 3, 3))
  expect_equal(cells[geo_id == "b", g], 2L)
  expect_equal(cells[, sum(person_weight), keyby = g]$V1, c(2, 2))
  # A value ending exactly at a cut is not split.
  even <- split_socio_group_cells(data.table::data.table(geo_id = c("a", "b"),
    educ_years = c(1, 2), person_weight = 1), "educ_years", "person_weight", 2L, "g")
  expect_equal(nrow(even), 2L)
})
