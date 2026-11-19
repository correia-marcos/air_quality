# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare an explicitly unreviewed baseline candidate from an isolated run.
#' @Description: Copy the run's registered products and record their hashes, the run's
# code revision and code inventory. This snapshot does not establish provenance,
# numerical acceptance or human approval. An existing baseline is never overwritten.
# Baseline paths are relative to its root.
#' @Summary:
#   I. Read the comparison registry, the run's report and the destination.
#   II. Copy the registered products and the run's code inventory.
#   III. Write the review record with every approval flag FALSE.
#' @Date: November 2026
#' @Author: Project contributors
# ========================================================================================

# ========================================================================================
# I: Read inputs
# ========================================================================================
source(here::here("src", "general_utilities", "verification_cli.R"))
args <- commandArgs(trailingOnly = TRUE)
usage <- "Use --source <isolated run directory> --destination <new baseline directory>."
flag_value <- function(flag) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) stop(usage)
  args[i + 1L]
}
if (length(args) != 4L) stop(usage)
run <- here::here(flag_value("--source"))
destination <- here::here(flag_value("--destination"))
registry <- read.csv(here::here("config", "verification_comparisons.csv"))
if (!nrow(registry)) stop("Register comparisons before preparing a candidate.")
# The container's own report: its revision and code identity, and its findings.
report_path <- file.path(run, "report", "report.json")
if (!file.exists(report_path)) stop("No container report in ", run)
report <- jsonlite::read_json(report_path)
if (any(grepl("^Uncompared analytical products", unlist(report$findings)))) {
  stop("The run generated unregistered products; register them first (", report_path, ").")
}
if (dir.exists(destination) && length(list.files(destination, all.files = TRUE,
  no.. = TRUE))) stop("Choose an empty baseline destination; existing files are protected.")
# The verifier mounts <run>/processed, interim and results as data/processed,
# data/interim and results.
registry$source_path <- file.path(run, sub("^data/", "", registry$actual_path))
missing <- registry$actual_path[!file.exists(registry$source_path)]
if (length(missing)) stop("The run lacks registered products: ", paste(missing,
  collapse = "; "))
if (anyDuplicated(registry$baseline_path) ||
  any(grepl("(^/|(^|/)\\.\\.(/|$))", registry$baseline_path))) {
  stop("Baseline destinations must be unique relative paths inside the new directory.")
}

# ========================================================================================
# II: Prepare the candidate
# ========================================================================================
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
baseline_hashes <- list()
for (i in seq_len(nrow(registry))) {
  path <- file.path(destination, registry$baseline_path[i])
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(registry$source_path[i], path)) {
    stop("Cannot copy ", registry$source_path[i])
  }
  baseline_hashes[[registry$baseline_path[i]]] <- verification_sha(path)
  if (!identical(verification_sha(registry$source_path[i]), verification_sha(path))) {
    stop("Candidate copy checksum mismatch.")
  }
}
code_file <- file.path(destination, "code-inventory.csv")
if (!file.copy(file.path(run, "report", "code-inventory.csv"), code_file) ||
  !identical(verification_sha(code_file), report$code_sha256)) {
  stop("The run's code inventory is missing or does not match its report.")
}
review <- list(human_reviewed = FALSE, reviewer = "", baseline_revision = "",
  candidate_revision = report$revision, candidate_code_sha256 = report$code_sha256,
  source_run = basename(run), image = report$image,
  baseline_sha256 = baseline_hashes, rendering_reviewed = FALSE,
  independent_reproduction = "not performed",
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  note = paste("Unreviewed derived snapshot.",
    "Review input identity, methods and coverage separately."))

# ========================================================================================
# III: Save the review record
# ========================================================================================
write.csv(registry[setdiff(names(registry), "source_path")],
  file.path(destination, "comparison-registry.csv"), row.names = FALSE)
jsonlite::write_json(review, file.path(destination, "baseline-review.json"),
  pretty = TRUE, auto_unbox = TRUE)
cat("Prepared", nrow(registry), "unreviewed comparison files at", destination, "\n")
