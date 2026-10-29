# ==========================================================================================
# IDB: Air monitoring — declared variants of the A/B/C robustness designs
# ==========================================================================================
#' @Goal: Turn one declared specification into its validated folders and file prefix.
#' @Description: The variants are declared in config/analysis_settings.R
#   (resolution_specifications). This function checks that a combination is valid and
#   derives where its products are written, so no recipe types a folder or a prefix.
#' @Summary:
#   1. resolution_specification
#' @Date: October 2026
#' @Author: Marcos Paulo
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# Function: resolution_specification
#
#' @param id             string; the specification's id, e.g. "education_group3_20km".
#' @param specifications named list; declared variants, each list(buffer_km, grouping),
#   in run order (resolution_specifications).
#' @param cities         character; city ids of the run (resolution_cities).
#' @param root           string; project root that receives the outputs. The canonical
#   root by default; tools/explore/ passes data/exploration/<run-id>.
#' @param input_root     string; project root of the frozen and prepared inputs.
#
#' @return  list: id, buffer_km, grouping, cities; the folders frozen_root (frozen city
#   inputs), prepared_root (3 km quintile root with the B matrices), processed_root (this
#   run), exposure_root (quintile run at the same buffer), table_dir and figure_dir;
#   prefix (file prefix of the combined tables and figures); and reference_anchor
#   (whether this run checks the archived Bogota reference).
#
#' @details
#   A grouping is "edu_quintile", "edu_quintile_split", "edu_level" or "edu_group3".
#   Groupings other than quintiles reuse the quintile run's B exposure at the same
#   buffer (interpolation never sees groups), so that quintile run must be declared
#   earlier. Folders and prefixes reproduce the existing layout: other groupings write
#   to their own folder (resolution_buffer_root()) and other buffers add <b>km_. Only the
#   3 km quintile run is anchored to the archived Bogota reference.
# ------------------------------------------------------------------------------------------
resolution_specification <- function(id, specifications, cities, root = here::here(),
                                     input_root = root) {
  groupings <- c("edu_quintile", "edu_quintile_split", "edu_level", "edu_group3")
  if (anyDuplicated(names(specifications))) stop("Specification ids must be unique.")
  if (!id %in% names(specifications)) stop("Undeclared specification: ", id)
  s <- specifications[[id]]
  if (!setequal(names(s), c("buffer_km", "grouping"))) {
    stop("Specification ", id, " must declare exactly buffer_km and grouping.")
  }
  if (!s$grouping %in% groupings) stop("Unknown grouping in ", id, ": ", s$grouping)
  if (!is.numeric(s$buffer_km) || length(s$buffer_km) != 1L || !(s$buffer_km > 0)) {
    stop("Specification ", id, " needs one positive buffer_km.")
  }
  if (s$grouping != "edu_quintile") {
    earlier <- specifications[seq_len(match(id, names(specifications)) - 1L)]
    quintile_first <- any(vapply(earlier, function(x) {
      identical(x$grouping, "edu_quintile") && isTRUE(x$buffer_km == s$buffer_km)
    }, logical(1)))
    if (!quintile_first) {
      stop("Specification ", id, " reuses the quintile B exposure at ", s$buffer_km,
           " km; declare that quintile run first.")
    }
  }
  folder <- basename(resolution_buffer_root(3, s$grouping))
  prefix <- paste0("resolution_multicity_",
    if (s$grouping != "edu_quintile") paste0(folder, "_"),
    if (s$buffer_km != 3) paste0(s$buffer_km, "km_"))
  list(id = id, buffer_km = s$buffer_km, grouping = s$grouping, cities = cities,
       frozen_root = file.path(input_root, "data", "interim", "resolution_sensitivity"),
       prepared_root = resolution_buffer_root(3, "edu_quintile", input_root),
       processed_root = resolution_buffer_root(s$buffer_km, s$grouping, root),
       exposure_root = resolution_buffer_root(s$buffer_km, "edu_quintile", root),
       table_dir = file.path(root, "results", "tables", "specification_sensitivity",
                             "robustness_designs"),
       figure_dir = file.path(root, "results", "figures", "specification_sensitivity",
                              "robustness_designs"),
       prefix = prefix,
       reference_anchor = s$buffer_km == 3 && s$grouping == "edu_quintile")
}
