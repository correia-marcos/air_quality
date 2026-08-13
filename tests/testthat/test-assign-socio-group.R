# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Golden-value tests for assign_socio_group() (pure: needs data.table only).
#
#' @Description: Checks the spec's equal-population rule (k-1)/n < F <= k/n against
#   hand-computed fixtures: unweighted and weighted cuts, deterministic tie-breaking
#   by (value, geo_id, row order), invariance to input row order, and NA handling
#   (rows with missing value or weight get NA group and leave the totals untouched).
#   All fixtures are small enough to verify by counting on one hand.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

test_that("unweighted values 1..10 cut into exact fifths", {
  dt <- data.table::data.table(
    geo_id = paste0("u", 1:10), val = 1:10, w = 1
  )
  assign_socio_group(dt, "val", "w", 5L, "grp")

  # F = 0.1, 0.2, ..., 1.0: two values per fifth, group 1 = lowest values.
  expect_equal(dt$grp[order(dt$val)], rep(1:5, each = 2))
})

test_that("expansion weights shift the boundary", {
  # F = 2/4, 3/4, 4/4: the 0.5 edge falls after the first row only.
  dt <- data.table::data.table(
    geo_id = c("a", "b", "c"), val = c(1, 2, 3), w = c(2, 1, 1)
  )
  assign_socio_group(dt, "val", "w", 2L, "grp")

  expect_equal(dt$grp[order(dt$val)], c(1L, 2L, 2L))
})

test_that("ties are split by geo_id order, not by input order", {
  # Six units tied on val = 5 (F = 1/8..6/8): the 0.5 edge cuts inside the tie
  # block, after geo units a, b, c, d in sorted order.
  dt <- data.table::data.table(
    geo_id = c("f", "a", "e", "b", "d", "c", "g", "h"),
    val    = c( 5,   5,   5,   5,   5,   5,   9,   9),
    w      = 1
  )
  assign_socio_group(dt, "val", "w", 2L, "grp")

  grp <- dt$grp[match(letters[1:8], dt$geo_id)]
  expect_equal(grp, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
})

test_that("assignment is invariant to input row order", {
  base <- data.table::data.table(
    geo_id = c("f", "a", "e", "b", "d", "c", "g", "h"),
    val    = c( 5,   5,   5,   5,   5,   5,   9,   9),
    w      = 1
  )
  set.seed(20260808)
  shuf <- data.table::copy(base)[sample(.N)]

  assign_socio_group(base, "val", "w", 2L, "grp")
  assign_socio_group(shuf, "val", "w", 2L, "grp")

  expect_equal(shuf$grp[match(base$geo_id, shuf$geo_id)], base$grp)
})

test_that("NA value or weight yields NA group and leaves totals untouched", {
  dt <- data.table::data.table(
    geo_id = c(paste0("u", 1:10), "x", "y"),
    val    = c(1:10, NA, 99),
    w      = c(rep(1, 10), 1, NA)
  )
  assign_socio_group(dt, "val", "w", 5L, "grp")

  # The ten valid rows behave exactly as the unweighted fixture above; the
  # NA-value row (x) and the NA-weight row (y) stay unassigned.
  expect_equal(dt$grp[order(dt$val)][1:10], rep(1:5, each = 2))
  expect_true(is.na(dt$grp[dt$geo_id == "x"]))
  expect_true(is.na(dt$grp[dt$geo_id == "y"]))
})
