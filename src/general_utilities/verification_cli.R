# Hash a file without reading its contents into a report.
#' @param p Input filename.
#' @return SHA-256 string.
verification_sha <- function(p) digest::digest(file = p, algo = "sha256",
  serialize = FALSE)
# Execute an external verification command and record its exit status.
#' @param cmd,args Command and argument vector.
#' @param log Output log filename.
#' @return Command, elapsed seconds, log basename, exit status and available memory events.
#' @details Linux cgroup counters describe the container, not an individual R process.
verification_command <- function(cmd, args, log) {
  started <- Sys.time()
  memory_events <- function() {
    path <- "/sys/fs/cgroup/memory.events"
    if (file.exists(path)) readLines(path, warn = FALSE) else NULL
  }
  memory_before <- memory_events()
  status <- tryCatch(system2(cmd, args, stdout = log, stderr = log), error = function(e) {
    writeLines(conditionMessage(e), log); 127L
  })
  result <- list(command = cmd, args = args, exit_status = status,
       seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
         log = basename(log))
  if (!is.null(memory_before)) {
    result$memory_events_before <- memory_before
    result$memory_events_after <- memory_events()
  }
  result
}
# Capture a diagnostic command's output or error message.
#' @param cmd,args Command and argument vector.
#' @return Character diagnostic output.
verification_capture <- function(cmd, args) tryCatch(system2(cmd, args, stdout = TRUE,
  stderr = TRUE),
                                      error = function(e) conditionMessage(e))
# Inventory files beneath declared roots without modifying them.
#' @param paths Directories to inventory.
#' @param root Repository root used to express relative filenames.
#' @return Data frame of paths, bytes and hashes.
verification_inventory <- function(paths, root) {
  files <- sort(unique(unlist(lapply(paths, list.files, recursive = TRUE,
    full.names = TRUE,
                                    all.files = TRUE, no.. = TRUE))))
  files <- files[!dir.exists(files)]
  data.frame(path = sub(paste0(root, "/"), "", files, fixed = TRUE),
             bytes = file.info(files)$size, sha256 = vapply(files, verification_sha,
               character(1)))
}

# Inventory the candidate code used by both verification and baseline review.
#' @param root Repository root.
#' @return Stable path/size/hash table; excludes transient and image-excluded local files.
verification_code_inventory <- function(root) {
  code <- verification_inventory(file.path(root, c("src", "scripts", "tools/reproduction",
    "config", "tests", "doc/ai")), root)
  code <- code[!grepl("^tests/(harness|_cache|_out)/", code$path), ]
  code <- code[!grepl("(^|/)__pycache__(/|$)|[.]py[cod]$", code$path), ]
  code <- code[basename(code$path) != ".DS_Store" &
    code$path != "scripts/export/paper.local.sh", ]
  root_code <- c("Dockerfile", "Makefile", "renv.lock", "DESCRIPTION", ".Rprofile",
    "_targets.R", ".dockerignore", "docker-compose.yml", "docker-compose.release.yml",
    "docker-compose.verify.yml", "doc/planning/remaining-work.md")
  root_code <- root_code[file.exists(file.path(root, root_code))]
  paths <- file.path(root, root_code)
  rbind(code, data.frame(path = root_code, bytes = file.info(paths)$size,
    sha256 = vapply(paths, verification_sha, character(1))))
}

# List generated tables and geography eligible for registered baseline comparisons.
#' @param root Repository or isolated rebuild root.
#' @return Sorted relative paths, including station source manifests and coverage tables.
#' @details Original inputs and rendered PDFs are excluded. Rendering requires review.
verification_product_paths <- function(root) {
  root <- normalizePath(root, mustWork = TRUE)
  patterns <- c("data/processed" = "[.](parquet|csv)$",
    "data/interim/geospatial_data" = "[.]gpkg$",
    "data/interim/monitoring_stations" = "[.]csv$",
    "results/tables" = "[.](csv|tex)$")
  files <- unlist(lapply(names(patterns), function(directory) {
    list.files(file.path(root, directory), pattern = patterns[[directory]],
      recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE)
  sort(unique(substring(files, nchar(root) + 2L)))
}

# Persist the current verification report.
#' @param report Named verification results.
#' @param run Verification output directory.
#' @return Writer result; writes report.json in run.
verification_save_report <- function(report, run) {
  jsonlite::write_json(report, here::here(run, "report.json"), pretty = TRUE,
                       auto_unbox = TRUE, null = "null")
}
