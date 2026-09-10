# Reproduce the analysis

Inputs: a clean checkout, declared data access, restored dependencies, a fresh output workspace and a revision-matched comparison baseline when available. Read ../../HOW_TO_RUN.md and ../evidence.md first.

Permitted actions: inspect and diagnose, restore the declared environment when authorized, run synthetic checks, then use `Rscript scripts/verification/verify.R --full` for an isolated batch attempt. Acquisition is a separate explicit operation. Do not enable downloads merely because an input is missing. Credentials are only relevant to applicable acquisition stages; do not request or read their contents.

Required evidence: input/code/output inventories, environment and image identity, executed stages and durations, nonzero failures, skips, schemas, manuscript export plan, numerical comparisons and rendering review. Preserve historical outputs as historical references if their producing revision is unknown.

Completion: report precisely what ran and whether full reproduction succeeded. A missing source,
failed source-based geography preparation, unsupported manuscript reference, absent baseline or
unavailable client is an explicit finding. Never claim full reproduction from a successful Docker
build or a collection of existing figures. Follow the clean-room protocol; independent
reproduction is recorded only after a colleague performs it.
