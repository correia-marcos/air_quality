# A separate testthat context demonstrates failure and release skip exit semantics.
test_that("runner signals assertion failures and incomplete release checks", {
  d <- tempfile(); dir.create(d)
  writeLines('testthat::test_that("deliberate failure", testthat::expect_true(FALSE))',
             file.path(d, "test-deliberate.R"))
  capture.output(r <- run_project_tests(d, "development"))
  expect_equal(r$status, 1L)
  expect_equal(unname(r$counts["failed"]), 1)
  writeLines('testthat::test_that("missing fixture", testthat::skip("no input"))',
             file.path(d, "test-deliberate.R"))
  capture.output(r <- run_project_tests(d, "release"))
  expect_equal(r$status, 1L)
  expect_equal(unname(r$counts["skipped"]), 1)
  writeLines('testthat::test_that("missing input", stop("Required source missing"))',
             file.path(d, "test-deliberate.R"))
  capture.output(r <- run_project_tests(d, "release"))
  expect_equal(r$status, 1L)
  expect_equal(unname(r$counts["errors"]), 1)
})

test_that("a failing runner process exits nonzero rather than printing success", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixtures <- file.path(root, "tests"); dir.create(fixtures)
  writeLines('testthat::test_that("deliberate", testthat::expect_equal(1, 2))',
    file.path(fixtures, "test-deliberate.R"))
  script <- file.path(root, "runner.R")
  writeLines(c(sprintf("source(%s)", dQuote(here::here("src/general_utilities/test_runner.R"), q = FALSE)),
    sprintf("r <- run_project_tests(%s, 'development')", dQuote(fixtures, q = FALSE)),
    "quit(status = r$status)"), script)
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
                    stdout = file.path(root, "log"), stderr = file.path(root, "log"))
  expect_equal(status, 1L)
  expect_true(any(grepl("Failure", readLines(file.path(root, "log")))))
})
