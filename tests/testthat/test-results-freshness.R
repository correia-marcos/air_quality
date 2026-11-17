# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Guard that no tracked result is older than the code and data it was built from.
#
#' @Description: In August 2026 the regression inputs were regenerated while the figure
# layer kept June files, and nothing said so. The targets store records what each result
# was built from, so a stale result is an outdated target. This test asks the store
# which manuscript targets would rerun and fails while any would. Without a store (a
# fresh clone, synthetic checks) it skips; release checks require one.
#
#' @Summary:
#   I.  Locate the targets store.
#   II. List the manuscript targets that would rerun.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ========================================================================================

test_that("manuscript outputs are current in the targets store", {
  store <- Sys.getenv("AIR_TARGETS_STORE", here::here("_targets"))
  if (!dir.exists(store)) {
    if (identical(getOption("airmonitoring.test_mode"), "release")) {
      fail(paste("No targets store at", store))
    }
    skip("No targets store: build it with Rscript scripts/run_pipeline.R")
  }
  outdated <- targets::tar_outdated(
    names = tidyselect::all_of(c("paper_export", "robustness", "verification_records")),
    script = here::here("_targets.R"), store = store, callr_function = NULL)
  expect_equal(length(outdated), 0L,
    info = paste("Stale targets:", paste(outdated, collapse = ", ")))
})
