# ----------------------------------------------------------------------------------------
# Function: write_geopackage
#
#' @param x An sf object; identifiers and geometry are written without transformation.
#' @param path Destination GeoPackage, containing one layer named after the file.
#' @param overwrite Replace an existing GeoPackage; FALSE fails before writing.
#' @return The path of the written file, invisibly.
#' @details Creates the destination directory and writes the complete object.
# ----------------------------------------------------------------------------------------
write_geopackage <- function(x, path, overwrite = TRUE) {
  if (file.exists(path) && !overwrite) stop("Output already exists: ", path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(x, path, delete_dsn = overwrite, quiet = TRUE)
  invisible(path)
}
