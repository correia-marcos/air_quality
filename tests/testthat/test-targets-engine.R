test_that("targets tracks file changes and independent branches", {
  source(here::here("tests", "check-targets-engine.R"), local = TRUE)
  expect_gt(check_targets_engine(), 0L)
})
