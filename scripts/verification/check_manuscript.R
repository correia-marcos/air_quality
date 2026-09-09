# ============================================================================================
# IDB: Air monitoring
#' @Goal: Check literal manuscript references against the export manifest.
#' @Description: Report omitted artifacts, unused mappings, and incomplete TeX coverage.
#' @Summary: I. Read. II. Compare. III. Report.
#' @Date: September 2026
#' @Author: Marcos Paulo (implementation assisted by Codex)
# ============================================================================================
source(here::here("src", "general_utilities", "reproducibility.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply the main TeX file path.")
refs <- manuscript_references(args[1])
manifest <- artifact_manifest(here::here("config", "paper_artifacts.csv"))
missing <- setdiff(refs$artifacts, manifest$paper_path)
unused <- setdiff(manifest$paper_path, refs$artifacts)
cat("Unmapped references:\n", paste(missing, collapse = "\n"), "\n")
cat("Unused manifest destinations:\n", paste(unused, collapse = "\n"), "\n")
cat("Coverage issues:\n", paste(refs$issues, collapse = "\n"), "\n")
quit(status = as.integer(length(missing) + length(unused) + length(refs$issues) > 0L))
