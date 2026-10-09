# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Execute or inspect the candidate manuscript dependency graph.
#' @Description: Uses an explicit cache and never installs packages or acquires inputs.
# The script runner remains the default until migration acceptance is complete.
#' @Summary:
#   I. Settings and arguments.
#   II. Resolve target selection.
#   III. Execute and record metadata.
#' @Date: September 2026
#' @Author: Project contributors
# ============================================================================================

# ============================================================================================
# I: Settings and arguments
# ============================================================================================
args <- commandArgs(trailingOnly = TRUE)
inspect <- "--outdated" %in% args
args <- setdiff(args, "--outdated")
if (length(args) > 1L) stop("Use run_targets.R [stage] [--outdated]")
if (!requireNamespace("targets", quietly = TRUE)) {
  stop("The targets dependency is unavailable. Complete the reviewed renv migration ",
       "before running this candidate graph; no packages were installed.")
}

# ============================================================================================
# II: Resolve target selection
# ============================================================================================
script <- here::here("_targets.R")
manifest <- targets::tar_manifest(fields = "name", script = script)
selection <- if (length(args)) args else "paper_export"
if (selection == "all") selection <- "paper_export"
if (!selection %in% manifest$name) stop("Unknown stage: ", selection)
store <- Sys.getenv("AIR_TARGETS_STORE", here::here("_targets"))
# Embed the selection as a literal: the child R process cannot see this caller's variables.

# ============================================================================================
# III: Execute and record metadata
# ============================================================================================
if (inspect) {
  print(eval(bquote(targets::tar_outdated(names = tidyselect::all_of(.(selection)),
                                         script = .(script), store = .(store)))))
} else {
  old <- Sys.getenv("AIR_VERIFY_STRICT", unset = NA_character_)
  Sys.setenv(AIR_VERIFY_STRICT = "1")
  tryCatch({
    eval(bquote(targets::tar_make(names = tidyselect::all_of(.(selection)),
                                  script = .(script), store = .(store))))
  }, finally = {
    if (is.na(old)) {
      Sys.unsetenv("AIR_VERIFY_STRICT")
    } else {
      Sys.setenv(AIR_VERIFY_STRICT = old)
    }
    run <- Sys.getenv("AIR_VERIFY_RUN")
    if (nzchar(run)) {
      tryCatch({
        metadata <- targets::tar_meta(store = store)
        saveRDS(metadata, here::here(run, "targets-metadata.rds"))
      }, error = function(e) warning("Could not save targets metadata: ",
                                     conditionMessage(e)))
      writeLines(c(paste("store:", store), paste("selection:", selection)),
                 here::here(run, "targets-run.txt"))
    }
  })
}
