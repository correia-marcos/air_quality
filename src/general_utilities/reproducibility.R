# --------------------------------------------------------------------------------------------
# Function: artifact_manifest
#' @param path CSV manifest.
#' @return Validated source/destination/producer mapping.
#' @details The manifest selects manuscript artifacts; producers retain their own logic.
# --------------------------------------------------------------------------------------------
artifact_manifest <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  validate_artifact_manifest(x)
}

validate_artifact_manifest <- function(x) {
  required <- c("artifact_id", "source_path", "paper_path", "producer_script")
  if (!identical(names(x), required) || !nrow(x) || anyNA(x) || any(x == "")) {
    stop("Manifest requires nonempty artifact_id, source_path, paper_path, producer_script.")
  }
  if (anyDuplicated(x$artifact_id) || anyDuplicated(tolower(x$paper_path))) {
    stop("Duplicate artifact ID or manuscript destination.")
  }
  for (col in required[-1]) {
    bad <- grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", x[[col]])
    bad <- bad | grepl("(^|/)\\.(/|$)|//|/$", x[[col]])
    if (any(bad)) stop("Unsafe relative path in ", col)
  }
  if (any(!grepl("^results/(figures|tables)/", x$source_path)) ||
      any(!grepl("^(figures|tables)/", x$paper_path)) ||
      any(!grepl("^scripts/", x$producer_script))) stop("Invalid manifest path scope.")
  destinations <- tolower(x$paper_path)
  for (path in destinations) {
    if (any(startsWith(destinations, paste0(path, "/")))) stop("Destination ancestor collision.")
  }
  x
}

# --------------------------------------------------------------------------------------------
# Function: contained_path
#' @param root Permitted directory.
#' @param relative Relative path within root.
#' @return Absolute path, after checking existing symlink ancestors.
#' @details Checking the nearest existing ancestor also covers new destination folders.
# --------------------------------------------------------------------------------------------
contained_path <- function(root, relative) {
  root <- normalizePath(root, mustWork = TRUE)
  candidate <- file.path(root, relative)
  parent <- candidate
  while (!file.exists(parent) && !dir.exists(parent)) parent <- dirname(parent)
  resolved <- normalizePath(parent, mustWork = TRUE)
  if (!(resolved == root || startsWith(resolved, paste0(root, "/")))) {
    stop("Path escapes root through a symlink: ", relative)
  }
  candidate
}

# --------------------------------------------------------------------------------------------
# Function: export_paper_artifacts
#' @param manifest Validated manifest data frame.
#' @param root Project root.
#' @param destination Existing export root (created only after successful preflight).
#' @param dry_run Validate and return the plan without writing.
#' @param overwrite Allow replacement of mapped files; unrelated files remain untouched.
#' @return Copy plan with source SHA-256 checksums, invisibly.
#' @details All sources and destinations are checked before copying. Files are staged and
#   checksummed before delivery. This function never edits a manuscript or deletes files.
# --------------------------------------------------------------------------------------------
export_paper_artifacts <- function(manifest, root, destination,
                                   dry_run = FALSE, overwrite = FALSE) {
  manifest <- validate_artifact_manifest(manifest)
  root <- normalizePath(root, mustWork = TRUE)
  destination <- path.expand(destination)
  if (!nzchar(destination)) stop("A destination is required.")
  if (!grepl("^/", destination)) destination <- file.path(getwd(), destination)
  manifest$source <- vapply(manifest$source_path, function(p) {
    contained_path(root, p)
  }, character(1))
  missing <- !file.exists(manifest$source) | dir.exists(manifest$source)
  if (any(missing)) stop("Missing artifact(s): ",
                         paste(manifest$source_path[missing], collapse = ", "))
  if (any(!file.exists(file.path(root, manifest$producer_script)))) {
    stop("Missing producer script.")
  }
  # Validate against the nearest existing destination ancestor without creating it.
  ancestor <- destination
  while (!dir.exists(ancestor)) {
    if (file.exists(ancestor)) stop("Destination ancestor is a file: ", ancestor)
    ancestor <- dirname(ancestor)
  }
  if (dir.exists(destination)) {
    dest <- vapply(manifest$paper_path, function(p) {
      contained_path(destination, p)
    }, character(1))
  } else dest <- file.path(destination, manifest$paper_path)
  for (p in dest) {
    parent <- dirname(p)
    while (!dir.exists(parent)) {
      if (file.exists(parent)) stop("Destination ancestor is a file: ", parent)
      parent <- dirname(parent)
    }
  }
  if (any(dir.exists(dest))) stop("A destination is a directory.")
  if (!overwrite && any(file.exists(dest))) stop("Destination exists; use --overwrite.")
  if (any(normalizePath(dest, mustWork = FALSE) ==
          normalizePath(manifest$source, mustWork = TRUE))) {
    stop("Export destination must differ from the canonical source.")
  }
  manifest$destination <- dest
  manifest$sha256 <- vapply(manifest$source, digest::digest, character(1),
                            algo = "sha256", file = TRUE)
  if (dry_run) return(invisible(manifest))
  staging <- tempfile("paper_export_")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  for (i in seq_len(nrow(manifest))) {
    stage <- file.path(staging, as.character(i))
    if (!file.copy(manifest$source[i], stage) ||
        digest::digest(stage, algo = "sha256", file = TRUE) != manifest$sha256[i]) {
      stop("Staging checksum failed: ", manifest$source_path[i])
    }
  }
  for (i in seq_len(nrow(manifest))) {
    dir.create(dirname(dest[i]), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(staging, as.character(i)), dest[i], overwrite = overwrite) ||
        digest::digest(dest[i], algo = "sha256", file = TRUE) != manifest$sha256[i]) {
      stop("Export/checksum failed: ", dest[i])
    }
  }
  invisible(manifest)
}

# --------------------------------------------------------------------------------------------
# Function: manuscript_references
#' @param path Main TeX file.
#' @return Lists of literal artifact paths, source files, and incomplete-coverage issues.
#' @details Supports ordinary literal includes. Macro-built paths and missing includes
#   remain explicit issues; this is not a full TeX interpreter.
# --------------------------------------------------------------------------------------------
manuscript_references <- function(path) {
  visited <- character(); refs <- character(); issues <- character()
  read_tex <- function(p) {
    if (!file.exists(p)) {
      issues <<- c(issues, paste("Missing TeX include:", p)); return(invisible(NULL))
    }
    p <- normalizePath(p)
    if (p %in% visited) return(invisible(NULL))
    visited <<- c(visited, p)
    lines <- readLines(p, warn = FALSE)
    lines <- vapply(lines, function(line) {
      # An even number of preceding backslashes leaves % as a comment delimiter.
      chars <- strsplit(line, "", fixed = TRUE)[[1]]
      for (i in which(chars == "%")) {
        n <- 0L; j <- i - 1L
        while (j > 0L && chars[j] == "\\") { n <- n + 1L; j <- j - 1L }
        if (n %% 2L == 0L) return(substr(line, 1L, i - 1L))
      }
      line
    }, character(1))
    txt <- paste(lines, collapse = "\n")
    if (grepl("\\\\(graphicspath|includeonly|newcommand|def)\\b", txt)) {
      issues <<- c(issues, paste("TeX macros/path directives need review:", p))
    }
    pat <- "\\\\(includegraphics|input|include)\\s*(?:\\[[^]]*\\])?\\s*\\{[^}]*\\}"
    calls <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
    all_commands <- regmatches(txt, gregexpr("\\\\(includegraphics|input|include)\\b", txt,
                                              perl = TRUE))[[1]]
    if (length(all_commands) != length(calls)) {
      issues <<- c(issues, paste("Unsupported unbraced/starred TeX reference:", p))
    }
    for (call in calls) {
      value <- sub("^[^{]*\\{(.*)\\}$", "\\1", call)
      if (grepl("[\\\\#$\\{]", value)) {
        issues <<- c(issues, paste("Dynamic TeX reference:", value)); next
      }
      if (startsWith(call, "\\includegraphics") || startsWith(value, "tables/")) {
        if (!grepl("\\.[A-Za-z0-9]+$", value)) {
          value <- paste0(value, if (startsWith(value, "tables/")) ".tex" else ".pdf")
        }
        refs <<- c(refs, value)
      } else {
        child <- if (grepl("\\.tex$", value)) value else paste0(value, ".tex")
        read_tex(file.path(dirname(p), child))
      }
    }
  }
  read_tex(path)
  list(artifacts = unique(refs), files = visited, issues = unique(issues))
}

# Compare tables using declared unique identifiers and independently chosen tolerances.
# Counts, schemas, missingness, integers and identifiers are exact; doubles use atol/rtol.
compare_numerical_tables <- function(actual, expected, keys, atol = 1e-10, rtol = 1e-8) {
  if (!identical(names(actual), names(expected)) || nrow(actual) != nrow(expected))
    stop("Schema names or row count differ.")
  if (!length(keys) || !all(keys %in% names(actual))) stop("Declare valid comparison keys.")
  if (!identical(lapply(actual, class), lapply(expected, class))) stop("Column classes differ.")
  if (anyDuplicated(actual[keys]) || anyDuplicated(expected[keys])) stop("Comparison keys are not unique.")
  a <- actual[do.call(order, actual[keys]), , drop = FALSE]
  b <- expected[do.call(order, expected[keys]), , drop = FALSE]
  rownames(a) <- rownames(b) <- NULL
  failures <- character()
  for (col in names(a)) {
    x <- a[[col]]; y <- b[[col]]
    if (!identical(is.na(x), is.na(y))) { failures <- c(failures, col); next }
    if (is.double(x) && !is.object(x) && !col %in% keys) {
      ok <- (is.na(x) & is.na(y)) | (x == y) |
        (is.finite(x) & is.finite(y) & abs(x - y) <= atol + rtol * abs(y))
      if (!all(ok)) failures <- c(failures, col)
    } else if (!identical(x, y)) failures <- c(failures, col)
  }
  if (length(failures)) stop("Numerical/structural differences: ", paste(failures, collapse = ", "))
  invisible(TRUE)
}

# --------------------------------------------------------------------------------------------
# Function: require_local_sources
#' @param paths Required source files.
#' @param acquisition Instruction for acquiring the missing inputs.
#' @return The paths, invisibly; missing sources stop before processing or downloading.
# --------------------------------------------------------------------------------------------
require_local_sources <- function(paths, acquisition) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing preserved source inputs: ", paste(missing, collapse = "; "),
         ". Acquire them first: ", acquisition, call. = FALSE)
  }
  invisible(paths)
}

# --------------------------------------------------------------------------------------------
# Function: record_source_acquisition
#' @param path Newly acquired source file; existing inputs are never relabelled as new.
#' @param provider Provider or package call used to acquire the file.
#' @param version Requested source year/version, including package version when relevant.
#' @return Sidecar path, invisibly.
#' @details Records file identity and acquisition time, not scientific approval.
# --------------------------------------------------------------------------------------------
record_source_acquisition <- function(path, provider, version) {
  metadata <- list(provider = provider, requested_version = as.character(version),
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE))
  sidecar <- paste0(path, ".source.json")
  jsonlite::write_json(metadata, sidecar, pretty = TRUE, auto_unbox = TRUE)
  invisible(sidecar)
}

# --------------------------------------------------------------------------------------------
# Function: preparation_input_status
#' @param path Source inventory, with offline_preparation rows for required local files.
#' @param root Project directory.
#' @return Required preparation inputs with presence flags and acquisition instructions.
#' @details This preflight covers geography and package-managed census prerequisites;
#   it does not certify the availability or provenance of every analytical input.
# --------------------------------------------------------------------------------------------
preparation_input_status <- function(path, root) {
  sources <- utils::read.csv(path, stringsAsFactors = FALSE)
  sources <- sources[sources$role == "offline_preparation", , drop = FALSE]
  sources$present <- file.exists(file.path(root, sources$root))
  sources
}

# --------------------------------------------------------------------------------------------
# Function: verification_revision
#' @param root Project directory; .git may be a directory or a worktree metadata file.
#' @param revision Explicit revision supplied by the host, when available.
#' @return Revision, Git availability, working-tree changes and patch.
#' @details An image has a code inventory but no Git metadata. Missing Git information
#   is recorded explicitly, without invoking Git or fabricating a clean-tree claim.
# --------------------------------------------------------------------------------------------
verification_revision <- function(root, revision = Sys.getenv("AIR_CODE_REVISION")) {
  has_git <- file.exists(file.path(root, ".git")) && nzchar(Sys.which("git"))
  git <- function(args) {
    result <- system2("git", c("-C", shQuote(root), args), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(result, "status"))) stop("Cannot collect Git provenance.")
    result
  }
  if (!nzchar(revision)) {
    revision <- if (has_git) paste(git(c("rev-parse", "HEAD")), collapse = "\n") else {
      "unrecorded"
    }
  }
  list(revision = revision, git_available = has_git,
    changes = if (has_git) git(c("status", "--porcelain")) else NULL,
    patch = if (has_git) git(c("diff", "--binary", "HEAD")) else NULL)
}
