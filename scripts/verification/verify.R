# Batch verification. Run from the repository root; see doc/HOW_TO_RUN.md.
args <- commandArgs(trailingOnly = TRUE)
full <- "--full" %in% args
rebuild <- "--rebuild" %in% args
if (any(!args %in% c("--full", "--rebuild", "--release"))) stop("Unknown verification argument")
root <- normalizePath(getwd())
run <- Sys.getenv("AIR_VERIFY_RUN", file.path(root, "data/verification", format(Sys.time(), "%Y%m%dT%H%M%S")))
dir.create(run, recursive = TRUE, showWarnings = FALSE)
run <- normalizePath(run)
Sys.setenv(AIR_VERIFY_STRICT = "1", OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
Sys.setenv(AIR_RUN_ID = basename(run))
set.seed(20230901)
sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
command <- function(cmd, args, log) {
  started <- Sys.time()
  status <- tryCatch(system2(cmd, args, stdout = log, stderr = log), error = function(e) {
    writeLines(conditionMessage(e), log); 127L
  })
  list(command = cmd, args = args, exit_status = status,
       seconds = as.numeric(difftime(Sys.time(), started, units = "secs")), log = basename(log))
}
capture <- function(cmd, args) tryCatch(system2(cmd, args, stdout = TRUE, stderr = TRUE),
                                      error = function(e) conditionMessage(e))
inventory <- function(paths) {
  files <- sort(unique(unlist(lapply(paths, list.files, recursive = TRUE, full.names = TRUE,
                                    all.files = TRUE, no.. = TRUE))))
  files <- files[!dir.exists(files)]
  data.frame(path = sub(paste0(root, "/"), "", files, fixed = TRUE),
             bytes = file.info(files)$size, sha256 = vapply(files, sha, character(1)))
}
report <- list(started_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  revision = Sys.getenv("AIR_CODE_REVISION", paste(capture("git", c("rev-parse", "HEAD")), collapse = "\n")),
  changes = capture("git", c("status", "--porcelain")), R = R.version.string,
  renv = if (requireNamespace("renv", quietly = TRUE)) as.character(packageVersion("renv")) else NA,
  platform = Sys.info(), image = Sys.getenv("AIR_IMAGE_ID", "unrecorded"),
  spatial = if (requireNamespace("sf", quietly = TRUE)) sf::sf_extSoftVersion() else NULL,
  seed = 20230901, threads = Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS")),
  tolerance = list(atol = 1e-10, rtol = 1e-8), stages = list(), findings = character(),
  reproduction_verified = FALSE, independent_reproduction = "not performed")
save_report <- function() jsonlite::write_json(report, file.path(run, "report.json"),
                                              pretty = TRUE, auto_unbox = TRUE, null = "null")
writeLines(capture("git", c("diff", "--binary", "HEAD")), file.path(run, "working-tree.patch"))
code <- inventory(c("src", "scripts", "config", "tests", "doc/ai"))
root_code <- c("Dockerfile", "Makefile", "renv.lock", "DESCRIPTION", ".Rprofile", "docker-compose.verify.yml")
code <- rbind(code, data.frame(path = root_code, bytes = file.info(root_code)$size,
                               sha256 = vapply(root_code, sha, character(1))))
write.csv(code, file.path(run, "code-inventory.csv"), row.names = FALSE)
report$code_sha256 <- sha(file.path(run, "code-inventory.csv"))
Sys.setenv(AIR_CODE_REVISION = report$revision)
save_report()
if (full) {
  # Sources are mounted read-only by compose; outputs start in this new run directory.
  if (any(dir.exists(file.path(run, c("interim", "processed", "results"))))) stop("Use a fresh run directory")
  for (d in c("interim", "processed", "results", "report")) dir.create(file.path(run, d))
  baseline_root <- Sys.getenv("AIR_BASELINE_ROOT", file.path(root, "data/verification/baseline"))
  dir.create(baseline_root, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(AIR_BASELINE_ROOT = normalizePath(baseline_root))
  Sys.setenv(AIR_SOURCE_ROOT = root, AIR_VERIFY_RUN = run, AIR_CODE_REVISION = report$revision)
  report$stages$docker <- command("docker", c("version"), file.path(run, "docker.log"))
  if (report$stages$docker$exit_status == 0L) {
    image <- Sys.getenv("AIR_VERIFY_IMAGE", "air-monitoring-verification:local")
    Sys.setenv(AIR_VERIFY_IMAGE = image)
    report$stages$build <- command("docker", c("build", "-t", shQuote(image), "."), file.path(run, "build.log"))
    if (report$stages$build$exit_status == 0L) {
      report$image <- paste(capture("docker", c("image", "inspect", "--format", "'{{.Id}}'", shQuote(image))), collapse = "")
      Sys.setenv(AIR_IMAGE_ID = report$image)
      report$stages$container <- command("docker", c("compose", "-f", "docker-compose.verify.yml",
        "run", "--rm", "verify"), file.path(run, "container.log"))
      child <- file.path(run, "report/report.json")
      if (file.exists(child)) report$reproduction_verified <- isTRUE(jsonlite::read_json(child)$reproduction_verified)
    }
  }
  report$findings <- c(report$findings, if (!report$reproduction_verified)
    "Full reproduction incomplete; inspect stage logs and container report.")
  save_report(); quit(status = if (report$reproduction_verified) 0L else 1L)
}
inputs <- inventory(c("data/raw", "data/downloads", "data/_legacy"))
write.csv(inputs, file.path(run, "inputs.csv"), row.names = FALSE)
report$input_provenance <- list(source_map = "config/input_sources.csv",
  note = "Provider/access descriptions in doc/HOW_TO_RUN.md. Unclassified files and legacy producing revisions require review; checksums do not establish origin.")
if (rebuild) {
  # Refuse native rebuilding: checking a mount option is stronger than an environment flag.
  mounts <- if (file.exists("/proc/mounts")) read.table("/proc/mounts", stringsAsFactors = FALSE) else NULL
  protected <- file.path(root, c("data/raw", "data/downloads", "data/_legacy"))
  ro <- !is.null(mounts) && all(vapply(protected, function(p)
    any(mounts$V2 == p & grepl("(^|,)ro(,|$)", mounts$V4)), logical(1)))
  if (!ro) report$findings <- c(report$findings, "Rebuild refused: source directories are not read-only mounts.")
  else report$stages$rebuild <- command("make", c("-B", shQuote("RUN=Rscript scripts/verification/run_stage.R"), "all", "merra2"), file.path(run, "rebuild.log"))
}
mode <- if ("--release" %in% args || rebuild) "release" else "development"
rscript <- file.path(R.home("bin"), "Rscript")
report$stages$tests <- command(rscript, c("--vanilla", "tests/testthat.R", paste0("--mode=", mode)), file.path(run, "tests.log"))
source("src/general_utilities/reproducibility.R")
manifest <- artifact_manifest("config/paper_artifacts.csv")
report$stages$export <- tryCatch({
  p <- export_paper_artifacts(manifest, root, file.path(run, "paper"), dry_run = FALSE)
  list(exit_status = 0L)
}, error = function(e) list(exit_status = 1L, error = conditionMessage(e)))
write.csv(inventory(c("results", "data/processed")), file.path(run, "outputs.csv"), row.names = FALSE)
write.csv(manifest, file.path(run, "required-artifacts.csv"), row.names = FALSE)
after <- inventory(c("data/raw", "data/downloads", "data/_legacy"))
if (!identical(inputs, after)) report$findings <- c(report$findings, "Source inventory changed during verification.")
# Baselines remain local. Register a revision and per-file hashes only after provenance review.
comparison_file <- "config/verification_comparisons.csv"
comparisons <- read.csv(comparison_file, stringsAsFactors = FALSE)
review_path <- Sys.getenv("AIR_BASELINE_REVIEW", "data/verification/baseline-review.json")
report$comparisons <- list()
if (!nrow(comparisons) || !file.exists(review_path)) {
  report$findings <- c(report$findings, "No reviewed revision-matched comparison baseline registered.")
} else {
  review <- jsonlite::read_json(review_path, simplifyVector = TRUE)
  valid_review <- isTRUE(review$human_reviewed) && nzchar(if (is.null(review$reviewer)) "" else review$reviewer) &&
    nzchar(if (is.null(review$baseline_revision)) "" else review$baseline_revision) && identical(review$candidate_revision, report$revision) &&
    identical(review$candidate_code_sha256, report$code_sha256)
  if (!valid_review) report$findings <- c(report$findings, "Baseline review lacks reviewer or matching candidate revision.")
  generated <- list.files("data/processed", pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  omitted <- setdiff(generated, comparisons$actual_path)
  if (length(omitted)) report$findings <- c(report$findings, paste("Uncompared processed products:", paste(omitted, collapse = "; ")))
  for (i in seq_len(nrow(comparisons))) {
    x <- comparisons[i, ]
    report$comparisons[[x$comparison_id]] <- tryCatch({
      if (!x$actual_path %in% generated) stop("Actual path is not a generated processed Parquet.")
      if (!is.finite(x$atol) || !is.finite(x$rtol) || x$atol < 0 || x$rtol < 0) stop("Invalid tolerance")
      if ((x$atol > 1e-10 || x$rtol > 1e-8) && !nzchar(x$justification)) stop("Tolerance exception needs justification")
      expected_hash <- review$baseline_sha256[[x$baseline_path]]
      if (is.null(expected_hash) || !identical(sha(x$baseline_path), expected_hash)) stop("Baseline checksum not reviewed or differs")
      compare_numerical_tables(as.data.frame(arrow::read_parquet(x$actual_path)),
        as.data.frame(arrow::read_parquet(x$baseline_path)), strsplit(x$keys, ";", fixed = TRUE)[[1]], x$atol, x$rtol)
      list(exit_status = 0L, atol = x$atol, rtol = x$rtol, justification = x$justification)
    }, error = function(e) list(exit_status = 1L, error = conditionMessage(e)))
  }
  if (!isTRUE(review$rendering_reviewed) || !isTRUE(review$plot_data_reviewed))
    report$findings <- c(report$findings, "Rendering and underlying plot-data review are incomplete.")
}
if (mode == "release" && (!rebuild || is.null(report$stages$rebuild)))
  report$findings <- c(report$findings, "Release requires a fresh rebuild under read-only source mounts.")
report$reproduction_verified <- mode == "release" && !length(report$findings) &&
  all(vapply(c(report$stages, report$comparisons), function(s) s$exit_status == 0L, logical(1)))
report$completed_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
save_report()
failed <- any(vapply(report$stages, function(s) s$exit_status != 0L, logical(1))) ||
  (mode == "release" && !report$reproduction_verified)
quit(status = as.integer(failed))
