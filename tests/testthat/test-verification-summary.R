test_that("failed verification prints the stage and log instead of staying silent", {
  source(here::here("src", "general_utilities", "verification_cli.R"), local = TRUE)
  root <- tempfile("verification-summary-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  report <- list(reproduction_verified = FALSE,
    stages = list(docker = list(exit_status = 1L, log = "docker.log")),
    findings = "Full reproduction incomplete.")
  output <- capture.output(verification_print_summary(report, root, 1L))
  expect_true(any(grepl("Verification exit status: 1", output, fixed = TRUE)))
  expect_true(any(grepl("Recorded reproduction status: incomplete", output, fixed = TRUE)))
  expect_true(any(grepl("process/docker", output, fixed = TRUE)))
  expect_true(any(grepl(file.path(root, "docker.log"), output, fixed = TRUE)))
})

test_that("container rebuild failures are visible even when the image build passed", {
  source(here::here("src", "general_utilities", "verification_cli.R"), local = TRUE)
  root <- tempfile("verification-child-")
  dir.create(file.path(root, "report"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  child <- list(reproduction_verified = FALSE,
    stages = list(rebuild = list(exit_status = 1L, log = "rebuild.log")),
    findings = "No reviewed baseline registered.")
  jsonlite::write_json(child, file.path(root, "report", "report.json"), auto_unbox = TRUE)
  report <- list(reproduction_verified = FALSE,
    stages = list(build = list(exit_status = 0L, log = "build.log"),
                  container = list(exit_status = 1L, log = "container.log")))
  output <- capture.output(verification_print_summary(report, root, 1L))
  expect_true(any(grepl("container/rebuild", output, fixed = TRUE)))
  expect_true(any(grepl(file.path(root, "report", "rebuild.log"), output, fixed = TRUE)))
  expect_true(any(grepl("No reviewed baseline registered.", output, fixed = TRUE)))
  expect_false(any(grepl("Failed stage: process/build", output, fixed = TRUE)))
})

test_that("successful local checks do not imply scientific reproduction", {
  source(here::here("src", "general_utilities", "verification_cli.R"), local = TRUE)
  root <- tempfile("verification-local-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  report <- list(reproduction_verified = FALSE,
    stages = list(tests = list(exit_status = 0L, log = "tests.log")))
  output <- capture.output(verification_print_summary(report, root, 0L))
  expect_true(any(grepl("Verification exit status: 0", output, fixed = TRUE)))
  expect_true(any(grepl("Recorded reproduction status: incomplete", output, fixed = TRUE)))
  expect_false(any(grepl("Failed stage:", output, fixed = TRUE)))
})
