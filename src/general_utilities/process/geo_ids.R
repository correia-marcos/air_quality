# ============================================================================================
# IDB: Air monitoring — geographic identifier repair
# ============================================================================================
#' @Goal: Functions for geographic identifier repair.
#
#' @Description: Reconciles the geographic keys the census and the spatial layers ship,
#   which differ in zero padding and width between providers, and renames each provider's
#   columns to the project's canonical schema.
#   Sourced by config_utils_process_data.R; never sourced directly by a script.
#
#' @Summary:
#   1. repair_bogota_geo_ids
#   2. canonical_geo_id
#   3. reconcile_geo_ids
#   4. apply_canonical_names
#   5. write_canonical_parquet
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: repair_bogota_geo_ids
#
#' @param geo_ids         character vector; spatial geographic IDs to repair.
#' @param census_ids      character vector; valid census geographic IDs.
#' @param id_width        integer; target Bogotá MGN ID width. Default 22.
#' @param max_zero_suffix integer; maximum trailing digits to replace by zero.
#' @param allow_broad_ids logical; allow broader zero-suffix repairs?
#
#' @return  data.table with original ID, repaired ID, method, and diagnostics.
#
#' @details
#   Repairs Bogotá geographic IDs for spatial-to-census joins. The function
#   first preserves exact matches, then tries right-padding short IDs, and
#   then tries hierarchical trailing-zero repairs. By default, only local
#   repairs are allowed. Broad repairs should be used only for diagnostics.
#
#' @Written_by : Marcos Paulo
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
#' @param x       vector; raw geographic IDs (character, integer, numeric, int64).
#' @param width   integer or NULL; if given, left-pad with zeros to this width.
#' @param state   string or NULL; if given, prefix this state code (then pad).
#
#' @return  character vector of canonical geo IDs.
#
#' @Purpose:
#   Single source of truth for geo-id formatting so that the distance matrix,
#   the IDW census merge, and the station-socio census merge all agree. Numeric
#   IDs are printed without scientific notation; an optional fixed width fixes
#   leading-zero loss (e.g. Mexico "9007" -> "09007").
#
#' @Written_on : June 2026
#' @Written_by : Marcos Paulo
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
#' @param geo_ids   character; geographic IDs from the spatial/exposure side.
#' @param census_ids character; geographic IDs from the census side.
#' @param label     string; used in the diagnostic message.
#' @param quiet     logical; suppress the message. Default FALSE.
#
#' @return  character vector the same length as geo_ids, with repairable IDs rewritten.
#' @details
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
#' @Written_on : 02/03/2026
#' @Written_by : Marcos Paulo
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


# The canonical vocabulary of doc/data_dictionary.md, in machine-readable form. A column
# already carrying one of these names is canonical wherever it came from, so it is never
# treated as provider-native. `count_*` and `share_*` are matched by pattern below.
.CANONICAL_COLS <- c(
  # identifiers and weights
  "geo_id", "geo_level", "comuna_id", "station_id", "station_name",
  "person_weight", "pop_total", "n_records",
  # census variables
  "educ_years", "income", "income_raw", "education_mean", "income_mean",
  "age", "adult", "women", "employed", "indigena", "hh_head", "hh_head_women",
  "no_education", "high_school_incomplete", "high_school_complete",
  "college_incomplete", "college_complete", "graduate_educ",
  # panels and distances
  "datetime", "year", "month", "day", "hour",
  "pm10", "pm25", "ozone", "no2", "co", "so2", "distance_km"
)

# --------------------------------------------------------------------------------------------
# Function: apply_canonical_names
#
#' @param dt        data.table or data.frame; one processed census or station table.
#' @param map       named character vector from cfg$schema; names are the provider's
#                   columns, values are the canonical names.
#' @param geo_level string or NULL; unit type to stamp on every row, e.g. "municipio".
#' @param raw_cols  character vector from cfg$schema$raw; provider-native columns to keep
#                   under a "raw_" prefix. Default NULL prefixes nothing.
#' @param geo_id_width integer or NULL from cfg$schema$geo_id_width; zero-pad geo_id to
#                   this many characters. Default NULL leaves the width alone.
#' @param quiet     logical; suppress the mapping message. Default FALSE.
#
#' @return  data.table with canonical names, geo_id as character, and geo_level added.
#
#' @details
#   The one place a provider's vocabulary meets the project's. Everything downstream reads
#   the canonical names in doc/data_dictionary.md, so this is the only step that has to
#   know that CDMX calls the municipality key CVE_MUN and Sao Paulo calls its unit code
#   code_weighting.
#
#   Why the raw list is explicit. Only the columns a city names in cfg$schema$raw take the
#   "raw_" prefix. Prefixing everything the map did not claim would be wrong: a processed
#   census also carries derived project columns that are neither provider-native nor part
#   of the shared schema (Sao Paulo's `white` and `formal_emp`, Santiago's `indigena`),
#   and calling those raw would misdescribe them. Keeping the provider's own variables
#   makes the derivation of each canonical variable checkable against its source; the
#   prefix is what makes that boundary visible in the file itself.
#
#   geo_id goes through canonical_geo_id() so it is always character: a numeric key loses
#   its leading zero (CDMX 9002 for "09002") and a 13-digit one renders in scientific
#   notation.
#
#   The mapping is printed on every run, so a script's console output records the
#   provenance of the table it just wrote.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
apply_canonical_names <- function(dt, map, geo_level = NULL, raw_cols = NULL,
                                  geo_id_width = NULL, quiet = FALSE) {

  dt <- data.table::as.data.table(dt)

  # A promised provider column that is absent means the mapping and the data disagree.
  missing <- setdiff(names(map), names(dt))
  if (length(missing))
    stop("apply_canonical_names(): column(s) not found: ",
         paste(missing, collapse = ", "))

  data.table::setnames(dt, names(map), unname(map))

  # Only the columns the city declares as provider-native get the raw_ prefix; derived
  # project columns are left alone. See @details: why the list is explicit.
  passthrough <- as.character(intersect(raw_cols, names(dt)))
  passthrough <- passthrough[!startsWith(passthrough, "raw_")]
  if (length(passthrough))
    data.table::setnames(dt, passthrough, paste0("raw_", passthrough))

  # Character identifiers only: see @details on leading zeros.
  if ("geo_id" %in% names(dt))
    dt[, geo_id := canonical_geo_id(geo_id, width = geo_id_width)]
  if ("comuna_id" %in% names(dt)) dt[, comuna_id := canonical_geo_id(comuna_id)]

  if (!is.null(geo_level)) dt[, geo_level := geo_level]

  if (!quiet) {
    message("  canonical names: ",
            paste(paste0(names(map), " -> ", unname(map)), collapse = ", "))
    if (length(passthrough))
      message("  kept as raw_*  : ", paste(passthrough, collapse = ", "))
  }

  dt[]
}


# --------------------------------------------------------------------------------------------
# Function: write_canonical_parquet
#
#' @param dt   data frame; the canonical table to write.
#' @param path string; full destination path, ending in .parquet.
#' @param meta named list; provenance stamped into the file's key-value metadata.
#
#' @return  the path, invisibly. Writes the Parquet file.
#
#' @details
#   Writes a processed census table and stamps its provenance into the Parquet
#   file-level key-value metadata, so a reader who opens the file alone can still
#   tell which city, vintage and native identifier produced it. Canonical column
#   names are uniform by design, which is exactly what makes a file ambiguous once
#   it is separated from the script that wrote it; the metadata is where the
#   provider-specific facts now live. Read it back with
#   arrow::read_parquet(path, as_data_frame = FALSE)$metadata -- reading the file as
#   a data frame is unaffected.
#
#   Every value is coerced to character because Parquet key-value metadata is a
#   string map. Use doc/data_dictionary.md for the meaning of each key.
#
#' @Written_on : August 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
write_canonical_parquet <- function(dt, path, meta) {

  # Provenance rides along as file-level key-value metadata, not as columns.
  tbl <- arrow::as_arrow_table(dt)
  tbl$metadata <- c(tbl$metadata, lapply(meta, as.character))

  arrow::write_parquet(tbl, path)
  invisible(path)
}
