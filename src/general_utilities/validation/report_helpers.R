# Attach the existing report dependencies without installing packages.
#' @return NULL invisibly; changes only the package search path.
load_validation_report_packages <- function() {
  packages <- c("here", "arrow", "dplyr", "ggplot2", "knitr", "kableExtra",
                "scales", "lubridate", "sf", "leaflet")
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Restore the report dependency before rendering: ", package)
    }
    library(package, character.only = TRUE)
  }
  invisible(NULL)
}

# Read one optional comparison artifact for an interactive report chunk.
#' @param city_dir Declared directory containing the city's comparison products.
#' @param subdir Comparison family within city_dir.
#' @param name Artifact basename without the Parquet extension.
#' @return A table when present, otherwise NULL (report status remains pending).
read_validation_parquet <- function(city_dir, subdir, name) {
  path <- file.path(city_dir, subdir, paste0(name, ".parquet"))
  if (file.exists(path)) arrow::read_parquet(path) else NULL
}

# Format completion status without changing the comparison data.
#' @param ok Whether the comparison artifact is present.
#' @param label_ok,label_pending Labels for the two states.
#' @return HTML span used by the Quarto status table; writes nothing.
validation_status_badge <- function(ok, label_ok = "Complete",
                                    label_pending = "Pending") {
  color <- if (ok) "#2e7d32" else "#9e9e9e"
  label <- if (ok) label_ok else label_pending
  sprintf(paste0('<span style="background:%s;color:white;padding:2px 8px;',
                 'border-radius:4px;font-size:.85em">%s</span>'), color, label)
}

# Render a validation report with the Quarto program.
#' @param qmd_path  The report's .qmd file.
#' @param html_dest Where the rendered HTML is saved; its folder must exist.
#' @param params    Named list of report parameters, passed as `-P name:value`.
#' @return html_dest, invisibly. Moves the self-contained HTML there; the report's
#   figures folder (`<report>_files/`, ignored by git) remains beside the .qmd.
#' @details Calls the Quarto program directly. The quarto R package is not a project
#   dependency and only wraps the same program. RStudio ships Quarto and puts it on PATH;
#   a terminal needs Quarto installed. QUARTO_R makes the report's R chunks run with this
#   session's R, and so with the project library, not whichever Rscript is first on PATH.
#   No --output or --output-dir: Quarto 1.10 places --output in the working directory
#   while the support files stay beside the .qmd, so the resources are not embedded and
#   the page loses its theme. Rendered beside the .qmd, the HTML embeds them; it is then
#   copied to html_dest, checked, and removed from beside the .qmd.
render_validation_report <- function(qmd_path, html_dest, params) {
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto not found: render from RStudio or install Quarto.")
  param_args <- shQuote(paste0(names(params), ":", unlist(params)))
  args       <- c("render", shQuote(qmd_path), as.vector(rbind("-P", param_args)))
  r_binary   <- shQuote(file.path(R.home("bin"), "Rscript"))
  status     <- system2(quarto, args, env = paste0("QUARTO_R=", r_binary))
  if (status != 0L) stop("quarto render stopped with status ", status, ".")

  # Move the rendered HTML only after an identical copy exists at html_dest.
  html_beside_qmd <- sub("[.]qmd$", ".html", qmd_path)
  if (!file.copy(html_beside_qmd, html_dest, overwrite = TRUE) ||
      tools::md5sum(html_beside_qmd) != tools::md5sum(html_dest)) {
    stop("Validation report copy failed; the render stays at ", html_beside_qmd)
  }
  file.remove(html_beside_qmd)
  invisible(html_dest)
}
