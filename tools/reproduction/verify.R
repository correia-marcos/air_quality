# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Verify an isolated candidate and record evidence.
#' @Description: Preserves sources and reports failures without claiming reproduction.
#' @Summary:
#   I. Settings and arguments.
#   II. Record provenance.
#   III. Execute isolated verification.
#   IV. Report acceptance status.
#' @Date: September 2026
#' @Author: Project contributors
# ============================================================================================
# ============================================================================================
# I: Settings and arguments
# ============================================================================================
args <- commandArgs(trailingOnly = TRUE)
full <- "--full" %in% args
rebuild <- "--rebuild" %in% args
engine <- if ("--targets" %in% args) "targets" else Sys.getenv("AIR_PIPELINE", "scripts")
if (!engine %in% c("scripts", "targets")) stop("Unknown pipeline engine")
Sys.setenv(AIR_PIPELINE = engine)
if (any(!args %in% c("--full", "--rebuild", "--release", "--targets"))) {
  stop("Unknown verification argument")
}
root <- normalizePath(getwd())
run <- Sys.getenv("AIR_VERIFY_RUN", file.path(root, "data/verification",
  format(Sys.time(), "%Y%m%dT%H%M%S")))
dir.create(run, recursive = TRUE, showWarnings = FALSE)
run <- normalizePath(run)
Sys.setenv(AIR_VERIFY_STRICT = "1", OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
Sys.setenv(AIR_RUN_ID = basename(run))
set.seed(20230901)
source(here::here("src", "general_utilities", "verification_cli.R"))
source(here::here("src", "general_utilities", "reproducibility.R"))

# ============================================================================================
# II: Record provenance
# ============================================================================================
provenance <- verification_revision(root)
report <- list(started_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  revision = provenance$revision, git_available = provenance$git_available,
  changes = provenance$changes, R = R.version.string,
  renv = if (requireNamespace("renv",
    quietly = TRUE)) as.character(packageVersion("renv")) else NA,
  platform = Sys.info(), image = Sys.getenv("AIR_IMAGE_ID", "unrecorded"),
  spatial = if (requireNamespace("sf", quietly = TRUE)) sf::sf_extSoftVersion() else NULL,
  seed = 20230901, threads = Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS")),
  tolerance = list(atol = 1e-10, rtol = 1e-8), stages = list(), findings = character(),
  pipeline = engine,
  targets_version = if (requireNamespace("targets", quietly = TRUE))
    as.character(packageVersion("targets")) else NA_character_,
  reproduction_verified = FALSE, independent_reproduction = "not performed")
if (provenance$git_available) {
  writeLines(provenance$patch, file.path(run, "working-tree.patch"))
}
code <- verification_code_inventory(root)
write.csv(code, file.path(run, "code-inventory.csv"), row.names = FALSE)
report$code_sha256 <- verification_sha(file.path(run, "code-inventory.csv"))
Sys.setenv(AIR_CODE_REVISION = report$revision)
verification_save_report(report, run)

# ============================================================================================
# III: Execute isolated verification
# ============================================================================================
if (full) {
  # Sources are mounted read-only by compose; outputs start in this new run directory.
  if (any(dir.exists(file.path(run, c("interim", "processed", "results", "targets"))))) {
    stop("Use a fresh run directory")
  }
  for (d in c("interim", "processed", "results", "report", "targets")) {
    dir.create(file.path(run, d))
  }
  baseline_root <- Sys.getenv("AIR_BASELINE_ROOT", file.path(root,
    "data/verification/baseline"))
  dir.create(baseline_root, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(AIR_BASELINE_ROOT = normalizePath(baseline_root))
  Sys.setenv(AIR_SOURCE_ROOT = root, AIR_VERIFY_RUN = run,
    AIR_CODE_REVISION = report$revision)
  report$stages$docker <- verification_command("docker", c("version"), file.path(run,
    "docker.log"))
  if (report$stages$docker$exit_status == 0L) {
    image <- Sys.getenv("AIR_VERIFY_IMAGE", "air-monitoring-verification:local")
    Sys.setenv(AIR_VERIFY_IMAGE = image)
    report$stages$build <- verification_command("docker", c("build", "-t",
      shQuote(image), "."), file.path(run, "build.log"))
    if (report$stages$build$exit_status == 0L) {
      report$image <- paste(verification_capture("docker", c("image", "inspect",
        "--format", "'{{.Id}}'", shQuote(image))), collapse = "")
      Sys.setenv(AIR_IMAGE_ID = report$image)
      report$stages$container <- verification_command("docker", c("compose", "-f",
        "docker-compose.verify.yml",
        "run", "--rm", "verify"), file.path(run, "container.log"))
      child <- file.path(run, "report/report.json")
      if (file.exists(child)) {
        child_report <- jsonlite::read_json(child)
        report$reproduction_verified <- isTRUE(child_report$reproduction_verified)
      }
    }
  }
  report$findings <- c(report$findings, if (!report$reproduction_verified)
    "Full reproduction incomplete; inspect stage logs and container report.")
  verification_save_report(report,
    run); quit(status = if (report$reproduction_verified) 0L else 1L)
}
inputs <- verification_inventory(c("data/raw", "data/downloads", "data/_legacy"), root)
write.csv(inputs, file.path(run, "inputs.csv"), row.names = FALSE)
report$input_provenance <- list(source_map = "config/input_sources.csv",
  note = paste("Provider/access descriptions in doc/HOW_TO_RUN.md.",
    "Unclassified files and legacy producing revisions require review;",
    "checksums do not establish origin."))
if (rebuild) {
  # Refuse native rebuilding: checking a mount option is stronger than an environment flag.
  mounts <- if (file.exists("/proc/mounts")) read.table("/proc/mounts",
    stringsAsFactors = FALSE) else NULL
  protected <- file.path(root, c("data/raw", "data/downloads", "data/_legacy"))
  ro <- !is.null(mounts) && all(vapply(protected, function(p)
    any(mounts$V2 == p & grepl("(^|,)ro(,|$)", mounts$V4)), logical(1)))
  if (!ro) report$findings <- c(report$findings,
    "Rebuild refused: source directories are not read-only mounts.")
  else {
    requirements <- preparation_input_status("config/input_sources.csv", root)
    write.csv(requirements, file.path(run, "preparation-inputs.csv"), row.names = FALSE)
    missing <- requirements$root[!requirements$present]
    report$stages$preparation_inputs <- list(
      exit_status = as.integer(length(missing) > 0L),
      missing = missing, inventory = "preparation-inputs.csv")
    if (length(missing)) {
      message("Missing preparation sources; see ", file.path(run,
        "preparation-inputs.csv"))
      report$findings <- c(report$findings,
        paste("Acquire missing preparation sources before rebuilding:",
              paste(missing, collapse = "; ")))
    } else {
      if (engine == "targets") {
        store <- Sys.getenv("AIR_TARGETS_STORE", here::here("_targets"))
        if (dir.exists(store) && length(list.files(store, all.files = TRUE,
          no.. = TRUE))) {
          stop("Verification requires a fresh targets store")
        }
        report$stages$rebuild <- verification_command("Rscript",
          c("scripts/run_targets.R", "all"),
          file.path(run, "rebuild.log"))
      } else {
        report$stages$rebuild <- verification_command("make",
          c("-B", shQuote("RUN=Rscript tools/reproduction/run_stage.R"), "all"),
          file.path(run, "rebuild.log"))
      }
    }
  }
}
mode <- if ("--release" %in% args || rebuild) "release" else "development"
rscript <- file.path(R.home("bin"), "Rscript")
report$stages$tests <- verification_command(rscript, c("--vanilla", "tests/testthat.R",
  paste0("--mode=", mode)), file.path(run, "tests.log"))
source(here::here("src", "general_utilities", "reproducibility.R"))
manifest <- artifact_manifest("config/paper_artifacts.csv")
report$stages$export <- tryCatch({
  p <- export_paper_artifacts(manifest, root, file.path(run, "paper"), dry_run = FALSE)
  list(exit_status = 0L)
}, error = function(e) list(exit_status = 1L, error = conditionMessage(e)))
write.csv(verification_inventory(c("results", "data/processed"), root), file.path(run,
  "outputs.csv"), row.names = FALSE)
write.csv(manifest, file.path(run, "required-artifacts.csv"), row.names = FALSE)
after <- verification_inventory(c("data/raw", "data/downloads", "data/_legacy"), root)
if (!identical(inputs, after)) report$findings <- c(report$findings,
  "Source inventory changed during verification.")
# Baselines remain local. Register a revision and per-file hashes only after provenance review.
comparison_file <- "config/verification_comparisons.csv"
comparisons <- read.csv(comparison_file, stringsAsFactors = FALSE)
baseline_root <- Sys.getenv("AIR_BASELINE_ROOT", "data/verification/baseline")
review_path <- Sys.getenv("AIR_BASELINE_REVIEW",
  file.path(baseline_root, "baseline-review.json"))
report$comparisons <- list()
if (!nrow(comparisons) || !file.exists(review_path)) {
  report$findings <- c(report$findings,
    "No reviewed revision-matched comparison baseline registered.")
} else {
  review <- jsonlite::read_json(review_path, simplifyVector = TRUE)
  valid_review <- isTRUE(review$human_reviewed) &&
    nzchar(if (is.null(review$reviewer)) "" else review$reviewer) &&
    nzchar(if (is.null(review$baseline_revision)) "" else review$baseline_revision) &&
    identical(review$candidate_revision, report$revision) &&
    identical(review$candidate_code_sha256, report$code_sha256)
  if (!valid_review) report$findings <- c(report$findings,
    "Baseline review lacks reviewer or matching candidate revision.")
  generated <- verification_product_paths(root)
  omitted <- setdiff(generated, comparisons$actual_path)
  if (length(omitted)) report$findings <- c(report$findings,
    paste("Uncompared analytical products:", paste(omitted, collapse = "; ")))
  for (i in seq_len(nrow(comparisons))) {
    x <- comparisons[i, ]
    report$comparisons[[x$comparison_id]] <- tryCatch({
      if (!x$actual_path %in% generated) {
        stop("Actual path is not a generated analytical table, TeX or geography product.")
      }
      if (!is.finite(x$atol) || !is.finite(x$rtol) || x$atol < 0 || x$rtol < 0) {
        stop("Invalid tolerance")
      }
      if ((x$atol > 1e-10 || x$rtol > 1e-8) && !nzchar(x$justification)) {
        stop("Tolerance exception needs justification")
      }
      baseline_path <- file.path(baseline_root, x$baseline_path)
      expected_hash <- review$baseline_sha256[[x$baseline_path]]
      if (is.null(expected_hash) || !identical(verification_sha(baseline_path),
        expected_hash)) stop("Baseline checksum not reviewed or differs")
      compare_registered_artifact(x$actual_path, baseline_path,
        strsplit(x$keys, ";", fixed = TRUE)[[1]], x$atol, x$rtol)
      list(exit_status = 0L, atol = x$atol, rtol = x$rtol,
        justification = x$justification)
    }, error = function(e) list(exit_status = 1L, error = conditionMessage(e)))
  }
  if (!isTRUE(review$rendering_reviewed) || !isTRUE(review$plot_data_reviewed))
    report$findings <- c(report$findings,
      "Rendering and underlying plot-data review are incomplete.")
}
if (mode == "release" && (!rebuild || is.null(report$stages$rebuild)))
  report$findings <- c(report$findings,
    "Release requires a fresh rebuild under read-only source mounts.")

# ============================================================================================
# IV: Report acceptance status
# ============================================================================================
report$reproduction_verified <- mode == "release" && !length(report$findings) &&
  all(vapply(c(report$stages, report$comparisons), function(s) s$exit_status == 0L,
    logical(1)))
report$completed_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
verification_save_report(report, run)
failed <- any(vapply(report$stages, function(s) s$exit_status != 0L, logical(1))) ||
  (mode == "release" && !report$reproduction_verified)
quit(status = as.integer(failed))
