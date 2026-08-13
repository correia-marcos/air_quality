# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Golden-value regression test for aggregate_idw_exposure() at geo level.
#
#' @Description: Runs the real estimator (DuckDB SQL and all) on the toy fixture from
#   helper-fixtures.R and compares every output column to hand-computed values:
#
#     hourly idw       g1: h0 = (60*1 + 120/2) / 1.5     = 80
#                      g1: h1 = 40 / 1                   = 40   (s2 missing: re-norm)
#                      g2: h0 = (60 + 120 + 300/2) / 2.5 = 132
#                      g2: h1 = (40 + 60/2) / 1.5        = 46.666...
#                      h2: all stations missing -> no geo-hour rows
#     annual avg_pm10  g1 = (80 + 40) / 2 = 60;  g2 = (132 + 46.666...) / 2 = 89.333...
#     total_hrs_pm10   2 for both units (h2 contributes nothing)
#     hrs_d_pm10_it*   WHO thresholds 150/100/75/50 counted with >=:
#                      g1: 0, 0, 1, 1   (80 crosses 75 and 50; 40 none)
#                      g2: 0, 1, 1, 1   (132 crosses 100/75/50; 46.67 none)
#     d = 0            g3's only pair is excluded (with a diagnostic), so g3 is
#                      absent from the exposure output.
#
#   First run needs network: DuckDB downloads its ICU extension. Any pipeline run in
#   the project container has already cached it.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

test_that("aggregate_idw_exposure reproduces the hand-computed toy values", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("duckdb", minimum_version = "0.9.2")

  root <- tempfile("idw_toy_")
  dir.create(root)
  fx <- make_toy_fixture(root)

  # The spec footnote requires d = 0 pairs to be reported, not silently dropped.
  expect_message(
    out <- aggregate_idw_exposure(
      arrow_dir      = fx$arrow,
      geo_sta_pq     = fx$dist_pq,
      census_col     = fx$census,
      geo_id_col     = "geo_id",
      pop_col        = "n",
      group_var      = "education_mean",
      n_groups       = 2L,
      group_name     = "edu_half",
      quintile_level = "geo",
      buffer_km      = 3,
      distance_power = 1,
      target_years   = 2023,
      pollutants     = "pm10",
      mem_gb         = 1,
      n_threads      = 1,
      out_dir        = file.path(root, "out"),
      out_name       = "toy",
      chunk_by_month = FALSE,
      quiet          = FALSE,
      return_data    = TRUE
    ),
    "excluded"
  )

  res <- data.table::as.data.table(out$exposure_yearly)
  data.table::setkey(res, geo_id)

  # g3's only station pair had d = 0, so no exposure rows survive for it.
  expect_setequal(res$geo_id, c("g1", "g2"))

  expect_equal(res["g1"]$avg_pm10, 60)
  expect_equal(res["g2"]$avg_pm10, (132 + 70 / 1.5) / 2)

  # Count columns arrive as BIGINT; as.numeric() keeps this bit64-proof.
  expect_equal(as.numeric(res["g1"]$total_hrs_pm10), 2)
  expect_equal(as.numeric(res["g2"]$total_hrs_pm10), 2)

  expect_equal(as.numeric(res["g1"]$hrs_d_pm10_it1), 0)
  expect_equal(as.numeric(res["g1"]$hrs_d_pm10_it2), 0)
  expect_equal(as.numeric(res["g1"]$hrs_d_pm10_it3), 1)
  expect_equal(as.numeric(res["g1"]$hrs_d_pm10_it4), 1)
  expect_equal(as.numeric(res["g2"]$hrs_d_pm10_it1), 0)
  expect_equal(as.numeric(res["g2"]$hrs_d_pm10_it2), 1)
  expect_equal(as.numeric(res["g2"]$hrs_d_pm10_it3), 1)
  expect_equal(as.numeric(res["g2"]$hrs_d_pm10_it4), 1)

  # The Parquet checkpoint carries the same rows (referees open it directly).
  on_disk <- data.table::as.data.table(arrow::read_parquet(out$exposure_path))
  expect_setequal(on_disk$geo_id, c("g1", "g2"))
})
