# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Build every manuscript result through the targets graph.
#
#' @Description: Runs the graph declared in _targets.R (modules in config/targets/): the
# paper export, the reported robustness checks and the verification records. targets
# reruns only what changed since the last build. Downloads and analyses that are not in
# the paper are separate commands (doc/HOW_TO_RUN.md). For one stage, use
# scripts/run_targets.R.
#
#' @Summary:
#   I.  Inspect: list the manuscript targets that will rerun.
#   II. Execute: build them.
#
#' @Date: November 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Inspect
# ========================================================================================
# List the manuscript targets whose code, settings or inputs changed since the last build
outdated <- targets::tar_outdated(names = c(paper_export, robustness, verification_records))
print(outdated)

# ========================================================================================
# II: Execute
# ========================================================================================
# Build the paper export, the robustness checks and the verification records; targets
# skips everything that is still current
targets::tar_make(names = c(paper_export, robustness, verification_records))
