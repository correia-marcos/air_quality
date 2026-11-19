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

# ----------------------------------------------------------------------------------------
# Function: compare_numerical_tables
#' @param actual,expected Data frames with the same columns; expected is the baseline.
#' @param keys Unique row identifiers; compared exactly.
#' @param rounding_steps Integer m: rounding steps on the longest chain from the declared
#   inputs to any column, counted as in doc/HOW_TO_RUN.md. 0 requires equality.
#' @param bound_scale "value" scales each difference by its baseline value; "column" by
#   the column's largest baseline magnitude.
#' @return Data frame, invisibly: one row per compared double column with its largest
#   absolute difference, its largest scaled difference and the bound 2 gamma_m. Stops on
#   any schema, class, key, missingness or non-double difference, or a bound exceeded.
#' @details A double passes when |actual - expected| <= 2 gamma_m S, with
# gamma_m = m u / (1 - m u) and u = 2^-53. A sum of k terms, added in any order, lies
# within gamma_(k-1) of its exact value relative to the sum of the magnitudes, and relative
# errors along a chain add (Higham 2002, Accuracy and Stability of Numerical Algorithms,
# sec. 3.1 and ch. 4); two computations that differ only in rounding (summation order,
# threads, accumulator width) therefore differ by at most twice that. For chains of
# nonnegative terms the sum of magnitudes is the value itself, so S = |baseline value|
# and the bound is proven. Signed or model-based outputs, where values near zero come from
# cancellation, use S = the column's largest baseline magnitude; their count covers only
# their sums, so the bound is a strict screen. Integers, dates, identifiers, counts and
# missingness are exact.
# ----------------------------------------------------------------------------------------
compare_numerical_tables <- function(actual, expected, keys, rounding_steps = 0L,
                                     bound_scale = c("value", "column")) {
  bound_scale <- match.arg(bound_scale)
  if (!identical(names(actual), names(expected)) || nrow(actual) != nrow(expected))
    stop("Schema names or row count differ.")
  if (!length(keys) || !all(keys %in% names(actual))) stop("Declare valid comparison keys.")
  if (!identical(lapply(actual, class), lapply(expected, class))) stop("Column classes differ.")
  if (anyDuplicated(actual[keys]) || anyDuplicated(expected[keys])) stop("Comparison keys are not unique.")
  a <- actual[do.call(order, actual[keys]), , drop = FALSE]
  b <- expected[do.call(order, expected[keys]), , drop = FALSE]
  rownames(a) <- rownames(b) <- NULL
  u <- 2^-53
  bound <- 2 * rounding_steps * u / (1 - rounding_steps * u)
  differences <- data.frame(column = character(), max_abs_diff = numeric(),
                            max_scaled_diff = numeric(), bound = numeric())
  failures <- character()
  for (col in names(a)) {
    x <- a[[col]]; y <- b[[col]]
    if (!identical(is.na(x), is.na(y))) { failures <- c(failures, col); next }
    if (is.double(x) && !is.object(x) && !col %in% keys) {
      finite <- is.finite(x) & is.finite(y)
      special <- (is.na(x) & is.na(y)) | (x == y)
      gap <- abs(x[finite] - y[finite])
      scale <- if (bound_scale == "value") abs(y[finite]) else max(abs(y[finite]), 0)
      # A zero scale admits no difference.
      scaled <- ifelse(gap == 0, 0, gap / scale)
      worst <- max(scaled, 0)
      differences[nrow(differences) + 1L, ] <- list(col, max(gap, 0), worst, bound)
      if (worst > bound || !all(special[!finite])) {
        failures <- c(failures,
                      sprintf("%s (scaled difference %.3g, bound %.3g)", col, worst, bound))
      }
    } else if (!identical(x, y)) failures <- c(failures, col)
  }
  if (length(failures)) stop("Numerical/structural differences: ", paste(failures, collapse = ", "))
  invisible(differences)
}

# ----------------------------------------------------------------------------------------
# Function: compare_registered_artifact
#' @param actual_path,baseline_path Generated product and reviewed baseline filenames.
#' @param keys Unique table identifiers. Empty for TeX, compared as literal lines, and for
#   tables without a unique key, compared as sorted multisets (`rounding_steps` 0).
#' @param rounding_steps,bound_scale Declared rounding steps and scale, as in
#   compare_numerical_tables().
#' @return The observed differences from compare_numerical_tables(), invisibly (no rows
#   for TeX or keyless tables), or a comparison error. Does not modify either file.
#' @details CSV and Parquet compare keyed tables; TeX compares literal lines. GeoPackages
# compare attributes and exact geometry/CRS. A table without a unique key, such as the
# person-level census records where two people can share every recorded attribute, is
# sorted on all its columns and must be identical, duplicates included. Figures are
# compared through the data they draw (write_plot_layers()) and a separate pixel review;
# byte equality is not a rendering criterion.
# ----------------------------------------------------------------------------------------
compare_registered_artifact <- function(actual_path, baseline_path, keys,
                                        rounding_steps = 0L, bound_scale = "value") {
  none <- data.frame(column = character(), max_abs_diff = numeric(),
                     max_scaled_diff = numeric(), bound = numeric())
  extension <- tolower(tools::file_ext(actual_path))
  if (!identical(extension, tolower(tools::file_ext(baseline_path)))) {
    stop("Comparison file formats differ.")
  }
  if (extension == "tex") {
    if (!identical(readLines(actual_path, warn = FALSE),
      readLines(baseline_path, warn = FALSE))) stop("TeX content differs.")
    return(invisible(none))
  }
  read_table <- function(path) {
    if (extension == "parquet") return(as.data.frame(arrow::read_parquet(path)))
    if (extension == "csv") return(utils::read.csv(path, stringsAsFactors = FALSE))
    if (extension == "gpkg") {
      spatial <- sf::st_read(path, quiet = TRUE)
      table <- sf::st_drop_geometry(spatial)
      table$.geometry <- vapply(sf::st_as_binary(sf::st_geometry(spatial)),
        digest::digest, character(1), algo = "sha256", serialize = FALSE)
      attr(table, "crs") <- sf::st_crs(spatial)
      return(table)
    }
    stop("Unsupported numerical comparison format: ", extension)
  }
  actual <- read_table(actual_path)
  baseline <- read_table(baseline_path)
  if (!identical(attr(actual, "crs"), attr(baseline, "crs"))) stop("CRS differs.")
  if (length(keys)) {
    return(compare_numerical_tables(actual, baseline, keys, rounding_steps, bound_scale))
  }
  # See @details: a keyless table compares as a sorted multiset.
  if (rounding_steps != 0) stop("A table without keys compares exactly; declare 0 steps.")
  if (!identical(names(actual), names(baseline)) || nrow(actual) != nrow(baseline) ||
      !identical(lapply(actual, class), lapply(baseline, class))) {
    stop("Schema names, classes or row count differ.")
  }
  data.table::setDT(actual)
  data.table::setDT(baseline)
  data.table::setorderv(actual, names(actual), na.last = TRUE)
  data.table::setorderv(baseline, names(baseline), na.last = TRUE)
  differ <- names(actual)[!mapply(identical, actual, baseline)]
  if (length(differ)) {
    stop("Keyless table content differs: ", paste(differ, collapse = ", "))
  }
  invisible(none)
}

# ----------------------------------------------------------------------------------------
# Function: write_plot_layers
#' @param plots One ggplot, or a list of them nested to any depth, as a plot target
#   returns it.
#' @param file  Parquet file to write.
#' @return The file path, invisibly. Writes one row per drawn row of every layer of every
#   plot: `plot` (its path in the list, such as `ci_edu/<name>.pdf`; empty when `plots`
#   is a single ggplot), `layer`, `geom` and `row`, then the layer's built columns.
#' @details
#   ggplot2::ggplot_build() computes the numbers a figure draws: stat output such as
#   densities and fitted lines, positions and mapped aesthetics. Since ggplot2 4.0 default
#   sizes and colours come from the active theme, so call this after set_paper_theme(),
#   as the figure writers do before ggsave(). Built columns are made comparable as
#   Parquet: PANEL becomes an integer, discrete positions plain doubles, other factors
#   text, and a geometry column its SHA-256 per feature, the rule
#   compare_registered_artifact() applies to GeoPackages, plus each feature's bounding
#   box (`geometry_xmin` ... `geometry_ymax`; ggplot's own `xmin` ... `ymax` hold the
#   layer's), which shows how far a feature moved when its hash differs. Colours stay
#   as hex text and compare exactly. A colour from a
#   continuous scale (the ridge fills) is an 8-bit sample of its value, so a rerun changes
#   it only if a value lies within about 1e-11 of a rounding boundary of a channel.
#   Within a layer, rows are sorted on every column, exact ones first, and numbered, so
#   `plot;layer;row` is a unique key that does not depend on the input's row order.
#   Layers are stacked with data.table::rbindlist(fill = TRUE), which gives a column that
#   changes type between layers its widest type. A cowplot grid holds its panels as
#   images, so only their positions are recorded.
# ----------------------------------------------------------------------------------------
write_plot_layers <- function(plots, file) {
  flatten <- function(x, path) {
    if (inherits(x, "ggplot")) return(stats::setNames(list(x), path))
    if (is.null(x)) return(list())
    if (!is.list(x) || is.data.frame(x)) stop("Not a ggplot or a list of them: ", path)
    labels <- if (is.null(names(x))) as.character(seq_along(x)) else names(x)
    paths <- if (nzchar(path)) paste(path, labels, sep = "/") else labels
    do.call(c, lapply(seq_along(x), function(i) flatten(x[[i]], paths[i])))
  }
  plot_list <- flatten(plots, "")

  layers <- list()
  for (i in seq_along(plot_list)) {
    built <- ggplot2::ggplot_build(plot_list[[i]])
    for (j in seq_along(built$data)) {
      layer <- as.data.frame(built$data[[j]])
      for (column in names(layer)) {
        value <- layer[[column]]
        if (inherits(value, "sfc")) {
          box <- vapply(value, function(feature) as.numeric(sf::st_bbox(feature)),
                        numeric(4))
          sides <- paste0(column, c("_xmin", "_ymin", "_xmax", "_ymax"))
          for (k in 1:4) layer[[sides[k]]] <- box[k, ]
          layer[[paste0(column, "_sha256")]] <- vapply(sf::st_as_binary(value),
            digest::digest, character(1), algo = "sha256", serialize = FALSE)
          layer[[column]] <- NULL
        } else if (column == "PANEL") {
          layer[[column]] <- as.integer(value)
        } else if (inherits(value, "mapped_discrete")) {
          layer[[column]] <- as.numeric(unclass(value))
        } else if (is.factor(value)) {
          layer[[column]] <- as.character(value)
        } else if (is.list(value) || is.object(value)) {
          stop("Cannot record layer column ", column, " of ", names(plot_list)[i])
        }
      }

      # Sort on the exact columns first, then the doubles; see @details.
      layer <- data.table::as.data.table(layer)
      exact <- names(layer)[!vapply(layer, is.double, logical(1))]
      data.table::setorderv(layer, c(exact, setdiff(names(layer), exact)), na.last = TRUE)
      layers[[length(layers) + 1L]] <- cbind(
        data.table::data.table(plot = names(plot_list)[i], layer = j,
          geom = class(plot_list[[i]]$layers[[j]]$geom)[1], row = seq_len(nrow(layer))),
        layer)
    }
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data.table::rbindlist(layers, use.names = TRUE, fill = TRUE), file)
  invisible(file)
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
# Function: preserve_local_source
#' @param source Existing unfiltered provider file, including a package-managed cache.
#' @param destination Declared preserved source location.
#' @param provider Provider/package identity.
#' @param version Known year/release; unknown provenance must be stated explicitly.
#' @param overwrite Explicit permission to replace an existing preserved copy.
#' @return Destination invisibly, after verifying the original and copied hashes.
#' @details A preservation date does not establish the original retrieval date.
# --------------------------------------------------------------------------------------------
preserve_local_source <- function(source, destination, provider, version, overwrite = FALSE) {
  if (file.exists(destination) && !overwrite) return(invisible(destination))
  require_local_sources(source, "supply the existing unfiltered provider file")
  if (identical(normalizePath(source), normalizePath(destination, mustWork = FALSE))) {
    stop("Preserved destination must differ from the original source.")
  }
  hash <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
  original_hash <- hash(source)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile("source-copy-", tmpdir = dirname(destination))
  on.exit(unlink(staged), add = TRUE)
  if (!file.copy(source, staged) || !identical(hash(staged), original_hash) ||
      !identical(hash(source), original_hash)) stop("Source copy checksum mismatch.")
  if (!file.rename(staged, destination)) stop("Cannot preserve local source.")
  metadata <- list(provider = provider, requested_version = version,
    retrieved_utc = NULL, preserved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    original_path = normalizePath(source), sha256 = original_hash,
    note = "Original provider retrieval date is unknown; local preservation only.")
  jsonlite::write_json(metadata, paste0(destination, ".source.json"),
    pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(destination)
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

#' @param artifacts Upstream figure and table outputs.
#' @param manifest_file Canonical export mapping tracked as a file target.
#' @return Exported manuscript files and integrity manifest under data/processed/paper.
#' @details Required files must be reported by current targets, even if old copies exist.
prepare_paper_export <- function(artifacts, manifest_file) {
  processing_files(artifacts, "manuscript artifacts")
  manifest <- artifact_manifest(manifest_file)
  required <- here::here(manifest$source_path)
  missing <- !normalizePath(required, mustWork = FALSE) %in%
    normalizePath(artifacts, mustWork = TRUE)
  if (any(missing)) {
    stop("Current targets did not report manuscript artifacts: ",
         paste(manifest$source_path[missing], collapse = ", "))
  }
  destination <- here::here("data", "processed", "paper")
  exported <- export_paper_artifacts(manifest, here::here(), destination,
                                     dry_run = FALSE, overwrite = TRUE)
  inventory <- here::here(destination, "export-manifest.csv")
  utils::write.csv(exported, inventory, row.names = FALSE)
  processing_files(c(exported$destination, inventory))
}
