# Portable coverage and reader-interface checks; no analytical datasets required.
#' @return Number of assertions checked against the live inventory and script tree.
check_reader_workflows <- function() {
  root <- here::here()
  count <- 0L
  check <- function(value, message) {
    if (!isTRUE(value)) stop(message, call. = FALSE)
    count <<- count + 1L
  }
  text <- readLines(file.path(root, "doc/planning/remaining-work.md"))
  begin <- match("<!-- workflow-inventory:start -->", text)
  end <- match("<!-- workflow-inventory:end -->", text)
  check(!is.na(begin) && !is.na(end) && begin < end, "Inventory markers missing")
  rows <- grep("^\\| \\[scripts/", text[seq.int(begin, end)], value = TRUE)
  paths <- sub("^\\| \\[([^]]+)\\].*$", "\\1", rows)
  files <- list.files(file.path(root, "scripts"), pattern = "[.](R|qmd)$",
                      recursive = TRUE)
  files <- paste0("scripts/", files)
  check(!anyDuplicated(paths), "An entry point has multiple inventory rows")
  check(setequal(paths, files), paste("Inventory mismatch:",
    paste(setdiff(files, paths), collapse = ", "),
    paste(setdiff(paths, files), collapse = ", ")))

  e <- new.env(parent = globalenv())
  sys.source(file.path(root, "src/pipeline/load.R"), e)
  e$load_manuscript_functions(envir = e)
  manifest <- targets::tar_manifest(fields = "name", script = here::here("_targets.R"),
    callr_function = NULL, envir = new.env(parent = globalenv()))
  launcher <- readLines(file.path(root, "scripts/run_pipeline.R"))
  active <- launcher[!grepl("^\\s*#", launcher)]
  check(!any(grepl('source\\(.*"download_data"', active)),
        "The default manuscript runner must not start acquisition")
  workflows <- c("manuscript", "optional analysis", "acquisition", "legacy validation",
                 "operational utility", "retirement candidate")
  for (i in seq_along(rows)) {
    fields <- trimws(strsplit(rows[i], "|", fixed = TRUE)[[1]])[-1]
    check(any(startsWith(fields[2], paste0(workflows, ";"))),
          paste("Invalid workflow:", paths[i]))
    check(grepl("\\*\\*(maintained|unverified|blocked)\\*\\*", fields[2]),
          paste("Missing readiness:", paths[i]))
    check(grepl(paths[i], fields[4], fixed = TRUE),
          paste("Direct command does not identify the entry point:", paths[i]))
    check(grepl(paste0("(../../", paths[i], ")"), fields[1], fixed = TRUE),
          paste("Broken inventory link:", paths[i]))
    check(all(nzchar(fields[3:6])), paste("Incomplete contract:", paths[i]))
    if (startsWith(fields[3], "Targets:")) {
      refs <- regmatches(fields[3], gregexpr("`[^`]+`", fields[3]))[[1]]
      refs <- gsub("`", "", refs, fixed = TRUE)
      check(length(refs) > 0L && all(refs %in% manifest$name),
            paste("Unknown target mapping:", paths[i]))
    } else {
      check(grepl("Outside|Transitional", fields[3]),
            paste("Missing reason for separation:", paths[i]))
    }
    if (endsWith(paths[i], ".R")) {
      lines <- readLines(file.path(root, paths[i]), warn = FALSE)
      headings <- grep("^# [IVX]+:", lines, value = TRUE)
      labels <- sub("^# ([IVX]+):.*", "\\1", headings)
      # Streaming functions also save; their recipes need no empty third section.
      check(length(labels) %in% 2:4 &&
            identical(labels, c("I", "II", "III", "IV")[seq_along(labels)]),
            paste("Sections must be ordered and meaningful:", paths[i]))
      check(all(vapply(c("Goal", "Description", "Summary", "Date", "Author"),
        function(tag) any(startsWith(lines, paste0("#' @", tag, ":"))), logical(1))),
        paste("Incomplete script header:", paths[i]))
      expression <- parse(text = lines)
      if (grepl("process_data|tables_images", paths[i])) {
        names <- all.names(expression)
        check(!any(c("tar_read", "tar_load", "tar_read_raw") %in% names),
              paste("Reader script requires hidden target state:", paths[i]))
        check(length(expression) > 3L, paste("Opaque analytical wrapper:", paths[i]))
      }
    }
  }

  # Supplied files govern reading, including outside the canonical data tree.
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("reader-contracts-", tmpdir = cache)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  series <- file.path(work, "merra2_stations_pm25")
  dir.create(series)
  cities <- c("bogota", "santiago", "ciudad_mexico", "sao_paulo")
  for (i in seq_along(cities)) {
    write.csv(data.frame(fixture = i), file.path(series,
      paste0(cities[i], "_pm25_stations_merra2.csv")), row.names = FALSE)
  }
  explicit <- list.files(series, full.names = TRUE)
  objects <- e$figure_station_temporal_inputs(explicit)
  check(identical(vapply(objects$city_specs, function(x) x$df$fixture, integer(1)),
                  seq_along(cities)), "Reader ignored explicitly supplied sources")
  fail <- function(expr) inherits(try(force(expr), silent = TRUE), "try-error")
  check(fail(e$pipeline_input_root(work, "census")), "Missing input family accepted")
  check(fail(e$pipeline_input_root(c(file.path(work, "a/census"),
    file.path(work, "b/census")), "census")), "Ambiguous input roots accepted")

  # A cached rendering input must still work after all its output directories disappear.
  e$set_paper_theme <- function(...) invisible(NULL)
  e$latex_census_summary <- function(x) paste("population", sum(x$population))
  e$latex_distance_band_table <- function(x, cities) paste(cities, collapse = ";")
  e$pipeline_stage_outputs <- function(stage, written) {
    stopifnot(all(file.exists(written)))
    written
  }
  output <- file.path(work, "tables")
  inputs <- list(written = character(), outdir_tables = output, outdir_paper = output,
                 census_summary = data.frame(population = c(10L, 20L)),
                 distance_bands = data.frame(distance = 1:2))
  dir.create(output)
  manual <- do.call(e$render_census_tables_tables, inputs)
  check(identical(manual$tex_lines, "population 30"), "Table object not inspectable")
  before <- lapply(manual$written, readLines)
  unlink(manual$written)
  check(!any(file.exists(manual$written)), "Fixture output removal failed")
  inputs$outdir_tables <- inputs$outdir_paper <- file.path(work, "fresh-output")
  check(!dir.exists(inputs$outdir_tables), "Expected a fresh output directory")
  automated <- e$write_render_census_tables(list(inputs = inputs))
  check(identical(before, lapply(automated, readLines)),
        "Cached input rendering differs from the manual operations")
  count
}

if (sys.nframe() == 0L) {
  cat(check_reader_workflows(), "reader workflow assertions passed\n")
}
