# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Run the project's golden-value regression tests.
#
# @Description: Sources the minimal src/ dependencies and runs tests/testthat/ with
#   testthat. The suite checks the grouping logic and the full IDW estimator against
#   values computed by hand from a 3-station x 2-geo-unit toy (see
#   tests/testthat/helper-fixtures.R). Run inside the Docker container, after
#   renv::restore(), with:  Rscript tests/testthat.R
#
# @Summary:
#   I.   Setup: load testthat, source the src/ functions under test.
#   II.  Run: execute every test-*.R file under tests/testthat/.
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# Functions under test. geo_ids.R and base_utils.R provide the helpers that
# idw_exposure.R calls (safe_chr, normalize_station, reconcile_geo_ids).
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "idw_exposure.R"))

testthat::test_dir(
  here::here("tests", "testthat"),
  reporter = "summary",
  stop_on_failure = FALSE
)
