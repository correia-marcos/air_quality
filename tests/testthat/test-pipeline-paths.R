# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Guard that every script path named in run_pipeline.R and the Makefile exists.
#
#' @Description: run_pipeline.R holds the run order and the Makefile holds the stage
# dependencies, but each spells every basename as its own literal string. A `git mv`
# updates neither, so a rename desynchronises both silently and the break only surfaces at
# run time. This test turns that into a failing test instead: it extracts the script paths
# from both files and checks them against disk, while the complete workflow inventory check accounts for all entry points.
#
#' @Summary:
#   I.   Extract the script paths each file claims to run.
#   II.  Every claimed path exists on disk.
#   III. Transitional schedulers execute the same manuscript selection.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

# Pull "scripts/<dir>/<file>.R" out of each here::here("scripts", <dir>, <file>) call.
# Commented-out calls count too: a disabled source() still asserts that the path is real.
pipeline_script_paths <- function(path) {
  lines <- readLines(path, warn = FALSE)
  pattern <- 'here::here\\(\\s*"scripts"\\s*,\\s*"[^"]+"\\s*,\\s*"[^"]+\\.R"\\s*\\)'
  calls   <- unlist(regmatches(lines, gregexpr(pattern, lines)))
  segments <- regmatches(calls, gregexpr('"[^"]+"', calls))

  vapply(segments,
         function(s) do.call(file.path, as.list(gsub('"', "", s, fixed = TRUE))),
         character(1))
}

# The Makefile writes the same paths literally, as prerequisites and as recipe commands.
makefile_script_paths <- function(path) {
  lines <- readLines(path, warn = FALSE)
  unique(unlist(regmatches(lines, gregexpr("(?:scripts|tools/reproduction)/[^ \t\\\\]+\\.R", lines))))
}

root          <- here::here()
pipeline_refs <- pipeline_script_paths(file.path(root, "scripts", "run_pipeline.R"))
makefile_refs <- makefile_script_paths(file.path(root, "Makefile"))

test_that("every script path in run_pipeline.R exists on disk", {
  expect_gt(length(pipeline_refs), 0)
  missing <- pipeline_refs[!file.exists(file.path(root, pipeline_refs))]
  expect_equal(missing, character(0))
})

test_that("every script path in the Makefile exists on disk", {
  expect_gt(length(makefile_refs), 0)
  missing <- makefile_refs[!file.exists(file.path(root, makefile_refs))]
  expect_equal(missing, character(0))
})

# Complete entry-point coverage lives in test-reader-workflows.R and the inventory.

# Compare executable commands, so commented optional paths cannot mask disagreement.
test_that("default orchestrators execute the same manuscript stages", {
  skip_if(Sys.which("make") == "", "make is unavailable")
  lines <- readLines(file.path(root, "scripts", "run_pipeline.R"))
  active <- tempfile(fileext = ".R")
  on.exit(unlink(active), add = TRUE)
  writeLines(lines[!grepl("^\\s*#", lines)], active)
  r_stages <- pipeline_script_paths(active)
  dry_run <- system2("make", c("-C", shQuote(root), "-n", "-B", "all"),
                     stdout = TRUE)
  make_stages <- sub("^Rscript ", "", dry_run[grepl("^Rscript ", dry_run)])
  expect_setequal(r_stages, make_stages)
  expect_false(anyDuplicated(make_stages) > 0L)
  expect_true(match("scripts/process_data/prepare_station_hourly.R", make_stages) <
                match("scripts/tables_images/figure_station_temporal.R", make_stages))
  expect_false(any(grepl(paste0("generate_panel_air_quality|prepare_station_temporal|",
                                 "process_merra2_panels|figure_merra2_vs_stations|",
                                 "figure_aerosol_composition"), make_stages)))
})
