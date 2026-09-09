# ============================================================================================
# IDB: Air monitoring
#' @Goal: Export exactly the manifest-selected manuscript artifacts.
#' @Description: Validate all paths, then copy and verify SHA-256 checksums.
#' @Summary: I. Arguments. II. Validate/export. III. Report.
#' @Date: September 2026
#' @Author: Marcos Paulo (implementation assisted by Codex)
# ============================================================================================
source(here::here("src", "general_utilities", "reproducibility.R"))
args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args) {
  cat("Rscript scripts/export/export_paper.R --destination DIR [--dry-run] [--overwrite]\n")
  quit(status = 0L)
}
position <- match("--destination", args)
if (is.na(position) || position == length(args)) stop("Supply --destination DIR.")
destination <- args[position + 1L]
remaining <- args[-c(position, position + 1L)]
if (any(!remaining %in% c("--dry-run", "--overwrite"))) stop("Unknown export argument.")
manifest <- artifact_manifest(here::here("config", "paper_artifacts.csv"))
plan <- export_paper_artifacts(manifest, here::here(), destination,
                              dry_run = "--dry-run" %in% args,
                              overwrite = "--overwrite" %in% args)
print(plan[, c("source_path", "paper_path", "sha256")], row.names = FALSE)
cat(if ("--dry-run" %in% args) "Validated" else "Exported", nrow(plan), "artifacts.\n")
