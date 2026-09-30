test_that("entry points are documented and inspection is independent of target state", {
  source(here::here("tests", "check-reader-workflows.R"), local = TRUE)
  expect_gt(check_reader_workflows(), 0L)
})
