# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Guard that every script path named in run_pipeline.R and the Makefile exists.
#
#' @Description: run_pipeline.R holds the run order and the Makefile holds the stage
# dependencies, but each spells every script basename as its own literal string. A `git mv`
# updates neither, so a rename desynchronises both silently and the break only surfaces at
# run time. This test turns that into a failing test instead: it extracts the script paths
# from both files and checks them against disk, then checks the reverse direction so a new
# script cannot be added without a conscious decision about wiring it in.
#
#' @Summary:
#   I.   Extract the script paths each file claims to run.
#   II.  Every claimed path exists on disk.
#   III. Every script on disk is either wired in or listed as a known exception.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

# Pull "scripts/<dir>/<file>.R" out of each here::here("scripts", "<dir>", "<file>.R") call.
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
  unique(unlist(regmatches(lines, gregexpr("scripts/[^ \t\\\\]+\\.R", lines))))
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

# Scripts that run outside the default pipeline on purpose. Adding a script here is the
# conscious opt-out; leaving it out is what makes the reverse check fail.
known_unwired <- c(
  "scripts/process_data/generate_inegi_lab_inputs.R",
  "scripts/process_data/impute_missing_hourly.R",
  "scripts/tables_images/bogota_fig_pollution_quintiles_geo_id.R",
  "scripts/tables_images/figure_missing_heatmap.R",
  "scripts/tables_images/figure_pollution_stations_by_hour.R",
  "scripts/tables_images/figure_stations_on_metro_area.R"
)

test_that("every process_data and tables_images script is wired in or opted out", {
  on_disk <- list.files(file.path(root, c("scripts/process_data", "scripts/tables_images")),
                        pattern = "\\.R$", full.names = TRUE)
  on_disk <- sub(paste0("^", root, "/"), "", on_disk)

  unaccounted <- setdiff(on_disk, c(pipeline_refs, makefile_refs, known_unwired))
  expect_equal(unaccounted, character(0))
})
