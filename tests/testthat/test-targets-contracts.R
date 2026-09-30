test_that("city preparation and manuscript dependency contracts are portable", {
  checks <- new.env(parent = globalenv())
  sys.source(here::here("tests", "check-targets-contracts.R"), checks)
  expect_gt(checks$check_targets_contracts(), 80L)
})
