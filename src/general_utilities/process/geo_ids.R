# ============================================================================================
# IDB: Air monitoring — geographic identifier repair
# ============================================================================================
# @Goal: Functions for geographic identifier repair.
#
# @Description: Reconciles the geographic keys the census and the spatial layers ship, which differ in
#   zero padding and width between providers.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
# @Summary:
#   1. repair_bogota_geo_ids
#   2. canonical_geo_id
#   3. reconcile_geo_ids
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: repair_bogota_geo_ids
#
# @Arg geo_ids          : character vector; spatial geographic IDs to repair.
# @Arg census_ids       : character vector; valid census geographic IDs.
# @Arg id_width         : integer; target Bogotá MGN ID width. Default 22.
# @Arg max_zero_suffix  : integer; maximum trailing digits to replace by zero.
# @Arg allow_broad_ids  : logical; allow broader zero-suffix repairs?
#
# @Output : data.table with original ID, repaired ID, method, and diagnostics.
#
# @Details:
#   Repairs Bogotá geographic IDs for spatial-to-census joins. The function
#   first preserves exact matches, then tries right-padding short IDs, and
#   then tries hierarchical trailing-zero repairs. By default, only local
#   repairs are allowed. Broad repairs should be used only for diagnostics.
#
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
repair_bogota_geo_ids <- function(
    geo_ids,
    census_ids,
    id_width        = 22L,
    max_zero_suffix = 2L,
    allow_broad_ids = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  
  # 1. Prepare inputs
  # -----------------------------------------------------------------------
  geo_ids <- trimws(as.character(geo_ids))
  census_ids <- trimws(as.character(census_ids))
  
  census_ids <- unique(census_ids[!is.na(census_ids) & census_ids != ""])
  
  if (length(geo_ids) == 0L) {
    return(data.table::data.table(
      geo_id_original = character(),
      geo_id_repaired = character(),
      repair_method = character(),
      zero_suffix_n = integer(),
      matched_repaired = logical(),
      changed_id = logical(),
      broad_repair = logical()
    ))
  }
  
  census_env <- new.env(hash = TRUE, parent = emptyenv())
  
  for (id in census_ids) {
    assign(id, TRUE, envir = census_env)
  }
  
  # 2. Inner helpers
  # -----------------------------------------------------------------------
  .in_census <- function(x) {
    !is.na(x) && exists(x, envir = census_env, inherits = FALSE)
  }
  
  .right_pad <- function(x) {
    if (is.na(x)) {
      return(NA_character_)
    }
    
    if (nchar(x) >= id_width) {
      return(substr(x, 1L, id_width))
    }
    
    paste0(x, strrep("0", id_width - nchar(x)))
  }
  
  .repair_one <- function(id) {
    
    if (is.na(id) || id == "") {
      return(list(id = NA_character_, method = "missing", suffix = NA_integer_))
    }
    
    if (.in_census(id)) {
      return(list(id = id, method = "exact", suffix = 0L))
    }
    
    id_pad <- .right_pad(id)
    
    if (!identical(id, id_pad) && .in_census(id_pad)) {
      return(list(id = id_pad, method = "right_pad", suffix = 0L))
    }
    
    max_suffix <- if (isTRUE(allow_broad_ids)) {
      id_width - 1L
    } else {
      as.integer(max_zero_suffix)
    }
    
    if (is.na(max_suffix) || max_suffix < 1L) {
      return(list(id = NA_character_, method = "unmatched", suffix = NA_integer_))
    }
    
    for (k in seq_len(max_suffix)) {
      
      prefix_len <- id_width - k
      
      candidate <- paste0(
        substr(id_pad, 1L, prefix_len),
        strrep("0", k)
      )
      
      if (.in_census(candidate)) {
        return(list(
          id = candidate,
          method = paste0("zero_suffix_", k),
          suffix = k
        ))
      }
    }
    
    list(id = NA_character_, method = "unmatched", suffix = NA_integer_)
  }
  
  # 3. Apply repair
  # -----------------------------------------------------------------------
  unique_geo <- unique(geo_ids)
  repairs <- lapply(unique_geo, .repair_one)
  
  out <- data.table::data.table(
    geo_id_original = unique_geo,
    geo_id_repaired = vapply(repairs, `[[`, character(1), "id"),
    repair_method = vapply(repairs, `[[`, character(1), "method"),
    zero_suffix_n = vapply(repairs, `[[`, integer(1), "suffix")
  )
  
  out[, matched_repaired := !is.na(geo_id_repaired)]
  out[, changed_id := geo_id_original != geo_id_repaired]
  out[, broad_repair := !is.na(zero_suffix_n) & zero_suffix_n > max_zero_suffix]
  
  return(out[])
}


# --------------------------------------------------------------------------------------------
# Function: canonical_geo_id
#
# @Arg x        : vector; raw geographic IDs (character, integer, numeric, int64).
# @Arg width    : integer or NULL; if given, left-pad with zeros to this width.
# @Arg state    : string or NULL; if given, prefix this state code (then pad).
#
# @Output : character vector of canonical geo IDs.
#
# @Purpose:
#   Single source of truth for geo-id formatting so that the distance matrix,
#   the IDW census merge, and the station-socio census merge all agree. Numeric
#   IDs are printed without scientific notation; an optional fixed width fixes
#   leading-zero loss (e.g. Mexico "9007" -> "09007").
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
canonical_geo_id <- function(x, width = NULL, state = NULL) {
  
  # Print numerics/int64 without scientific notation or decimals.
  if (inherits(x, "integer64")) {
    x <- as.character(x)
  } else if (is.numeric(x)) {
    x <- ifelse(is.na(x), NA_character_, sprintf("%.0f", x))
  } else {
    x <- trimws(as.character(x))
  }
  
  # Optionally prefix a fixed state code before padding.
  if (!is.null(state)) {
    x <- ifelse(is.na(x), NA_character_, paste0(state, x))
  }
  
  # Optionally left-pad with zeros to a fixed width (base-R only).
  if (!is.null(width)) {
    need <- pmax(0L, width - nchar(x))
    x <- ifelse(is.na(x), NA_character_, paste0(strrep("0", need), x))
  }
  
  return(x)
}


# --------------------------------------------------------------------------------------------
# Function: reconcile_geo_ids
#
# @Arg geo_ids    : character; geographic IDs from the spatial/exposure side.
# @Arg census_ids : character; geographic IDs from the census side.
# @Arg label      : string; used in the diagnostic message.
# @Arg quiet      : logical; suppress the message. Default FALSE.
#
# @Output : character vector the same length as geo_ids, with repairable IDs rewritten.
# @Details:
#   Spatial and census layers spell the same geographic code in different ways. Two
#   defects occur in this project and they are opposites, so neither rule can be
#   applied blindly:
#     - left-pad  : a fixed-width numeric code lost its leading zero when read as a
#                   number. CDMX alcaldias are "09002" spatially and 9002 in the census.
#     - right-pad : a hierarchical code was stored at its natural depth on one side and
#                   zero-filled to full width on the other. Bogota rural sectors are
#                   "11001300706" spatially and "1100130070600000000000" in the census.
#   Rather than guess which applies, every candidate is checked against the census IDs
#   and accepted only if it lands on one. A geo ID whose two candidates both match is
#   ambiguous and is left alone. This makes the census the arbiter of every repair, so
#   the function can never invent a match that the census does not already contain.
#
# @Written_on : 02/03/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
reconcile_geo_ids <- function(geo_ids, census_ids, label = "", quiet = FALSE) {

  geo_ids    <- as.character(geo_ids)
  census_ids <- unique(as.character(census_ids))

  # Only IDs that fail to match need repairing at all.
  todo <- unique(geo_ids[!geo_ids %in% census_ids])

  if (length(todo) == 0L) {
    return(geo_ids)
  }

  # Candidate widths are the widths the census actually uses.
  widths <- sort(unique(nchar(census_ids)))
  fixed  <- character(0)
  repl   <- character(0)
  n_left <- 0L
  n_right <- 0L

  for (id in todo) {
    hits <- character(0)
    rule <- character(0)

    # The census may spell the code wider than the spatial layer does.
    for (w in widths[widths > nchar(id)]) {
      pad <- strrep("0", w - nchar(id))

      # Leading-zero loss versus hierarchical zero-fill.
      cand_l <- paste0(pad, id)
      cand_r <- paste0(id, pad)

      if (cand_l %in% census_ids) { hits <- c(hits, cand_l); rule <- c(rule, "left") }
      if (cand_r %in% census_ids) { hits <- c(hits, cand_r); rule <- c(rule, "right") }
    }

    # Or narrower, when the census read the code as a number and dropped its
    # leading zero. This is the CDMX alcaldia case: "09002" against 9002.
    cand_s <- sub("^0+", "", id)

    if (nzchar(cand_s) && cand_s != id && cand_s %in% census_ids) {
      hits <- c(hits, cand_s)
      rule <- c(rule, "strip")
    }

    # Accept only an unambiguous single match.
    if (length(unique(hits)) == 1L) {
      fixed <- c(fixed, id)
      repl  <- c(repl, hits[[1]])

      if (rule[[1]] == "right") n_right <- n_right + 1L else n_left <- n_left + 1L
    }
  }

  if (length(fixed) == 0L) {
    return(geo_ids)
  }

  # Rewrite only the verified IDs; everything else is untouched.
  out <- geo_ids
  idx <- match(geo_ids, fixed)
  out[!is.na(idx)] <- repl[idx[!is.na(idx)]]

  if (!quiet) {
    message(
      label, " geo ID reconciliation: repaired ", length(fixed), " of ",
      length(todo), " unmatched ID(s) (", n_left, " leading-zero, ",
      n_right, " hierarchical zero-fill)."
    )
  }

  out
}
