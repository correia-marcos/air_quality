# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Prepare an explicitly unreviewed baseline candidate from registered products.
#' @Description: Copy derived files and record their hashes and candidate code identity.
# This snapshot does not establish provenance, numerical acceptance or human approval.
# An existing baseline is never overwritten. Baseline paths are relative to its root.
#' @Summary:
#   I. Read the comparison registry and destination.
#   II. Copy the registered products and inventory the candidate.
#   III. Write the review record with every approval flag FALSE.
#' @Date: October 2026
#' @Author: Project contributors
# ========================================================================================

# ========================================================================================
# I: Read inputs
# ========================================================================================
source(here::here("src", "general_utilities", "verification_cli.R"))
source(here::here("src", "general_utilities", "reproducibility.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || args[1] != "--destination") {
  stop("Use --destination <new-local-baseline-directory>.")
}
destination <- here::here(args[2])
registry <- read.csv(here::here("config", "verification_comparisons.csv"))
if (!nrow(registry)) stop("Register comparisons before preparing a candidate.")
if (dir.exists(destination) && length(list.files(destination, all.files = TRUE,
  no.. = TRUE))) stop("Choose an empty baseline destination; existing files are protected.")
if (any(!file.exists(registry$actual_path))) stop("Generate all registered products first.")
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
  if (!file.copy(registry$actual_path[i], path)) {
    stop("Cannot copy ", registry$actual_path[i])
  }
  baseline_hashes[[registry$baseline_path[i]]] <- verification_sha(path)
  if (!identical(verification_sha(registry$actual_path[i]), verification_sha(path))) {
    stop("Candidate copy checksum mismatch.")
  }
}
code <- verification_code_inventory(here::here())
code_file <- file.path(destination, "code-inventory.csv")
write.csv(code, code_file, row.names = FALSE)
provenance <- verification_revision(here::here())
review <- list(human_reviewed = FALSE, reviewer = "", baseline_revision = "",
  candidate_revision = provenance$revision,
  candidate_code_sha256 = verification_sha(code_file),
  baseline_sha256 = baseline_hashes, rendering_reviewed = FALSE, plot_data_reviewed = FALSE,
  independent_reproduction = "not performed",
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  note = paste("Unreviewed derived snapshot.",
    "Review input identity, methods and coverage separately."))

# ========================================================================================
# III: Save the review record
# ========================================================================================
write.csv(registry, file.path(destination, "comparison-registry.csv"), row.names = FALSE)
jsonlite::write_json(review, file.path(destination, "baseline-review.json"),
  pretty = TRUE, auto_unbox = TRUE)
cat("Prepared", nrow(registry), "unreviewed comparison files at", destination, "\n")
