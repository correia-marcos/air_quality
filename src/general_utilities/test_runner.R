# --------------------------------------------------------------------------------------------
# Function: run_project_tests
#' @param test_dir Directory containing testthat files.
#' @param mode development, synthetic, or release.
#' @return Named counts and an exit status; release treats every skip as incomplete.
#' @details Synthetic mode excludes only declared full-data checks. Assertion failures
#   always produce failure status, independently of the human-readable reporter.
# --------------------------------------------------------------------------------------------
run_project_tests <- function(test_dir, mode = "development") {
  if (!mode %in% c("development", "synthetic", "release")) stop("Unknown test mode.")
  old <- options(airmonitoring.test_mode = mode)
  on.exit(options(old), add = TRUE)
  result <- testthat::test_dir(test_dir, reporter = "summary",
                              stop_on_failure = FALSE, stop_on_warning = FALSE,
                              filter = if (mode == "synthetic") {
                                paste0("^(", paste(setdiff(sub("^test-(.*)\\.R$", "\\1",
                                  list.files(test_dir, pattern = "^test-.*\\.R$")),
                                  c("canonical-schema", "results-freshness",
                                    "release-inputs")), collapse = "|"), ")$")
                              } else NULL)
  frame <- as.data.frame(result)
  count <- function(name) if (name %in% names(frame)) sum(frame[[name]]) else 0L
  counts <- c(passed = count("passed"), failed = count("failed"),
              errors = count("error"), skipped = count("skipped"),
              warnings = count("warning"))
  failed <- counts["failed"] + counts["errors"] > 0L ||
    (mode != "development" && counts["skipped"] > 0L)
  list(mode = mode, counts = counts, status = as.integer(failed))
}
