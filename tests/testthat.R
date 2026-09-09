# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Run the project's golden-value regression tests.
#
#' @Description: Sources the minimal src/ dependencies and runs tests/testthat/ with
#   testthat. The suite checks the grouping logic, the full IDW estimator against
#   values computed by hand from a 3-station x 2-geo-unit toy (see
#   tests/testthat/helper-fixtures.R), and the exposure-regression estimator against
#   hand-computed weighted group means. Run inside the Docker container, after
#   renv::restore(), with:  Rscript tests/testthat.R
#
#' @Summary:
#   I.   Setup: load testthat, source the src/ functions under test.
#   II.  Run: execute every test-*.R file under tests/testthat/.
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# Functions under test. geo_ids.R and base_utils.R provide the helpers that
# idw_exposure.R calls (safe_chr, normalize_station, reconcile_geo_ids).
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "idw_exposure.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))

for (file in c("distances.R", "outliers.R", "imputation.R")) {
  source(here::here("src", "general_utilities", "process", file))
}
source(here::here("src", "general_utilities", "test_runner.R"))
source(here::here("src", "general_utilities", "reproducibility.R"))
args <- commandArgs(trailingOnly = TRUE)
mode_arg <- grep("^--mode=", args, value = TRUE)
if (length(mode_arg) > 1L || any(!grepl("^--mode=", args))) {
  stop("Use --mode=development, --mode=synthetic, or --mode=release.")
}
mode <- if (length(mode_arg)) sub("^--mode=", "", mode_arg) else "development"
result <- run_project_tests(here::here("tests", "testthat"), mode)
cat("\nTest mode:", result$mode, "\n")
print(result$counts)
quit(status = result$status)
