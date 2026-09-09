test_that("release includes every selected artifact and producer", {
  if (!identical(getOption("airmonitoring.test_mode"), "release")) {
    skip("Full artifact inventory is a release check.")
  }
  m <- artifact_manifest(here::here("config", "paper_artifacts.csv"))
  expect_true(all(file.exists(here::here(m$source_path))))
  expect_true(all(file.exists(here::here(m$producer_script))))
  expect_false(any(dir.exists(here::here(m$source_path))))
})
