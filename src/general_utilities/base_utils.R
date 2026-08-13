# ============================================================================================
# IDB: Air monitoring — shared leaf helpers
# ============================================================================================
#' @Goal: One definition of every helper more than one stage needs.
#
#' @Description: Sourced by config_utils_process_data.R, config_utils_plot_tables.R and
#   config_utils_validation_old_version.R. Loads no packages and has no side effects, so
#   it is
#   safe to source more than once; scripts never source it directly. Absorbs the former
#   geo_utils.R. Every function here previously existed in two or more copies; keeping one
#   copy
#   is what stops them drifting apart the way .safe_chr did.
#
#' @Summary:
#   I.   Projections  — aeqd_crs, utm_epsg, aeqd_for
#   II.  Identifiers  — normalize_station, safe_chr
#   III. Formatting   — to_iso, latex_escape, format_int_latex
#   IV.  Disk         — write_pq, save_raw_data_tidy_formatted
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# --------------------------------------------------------------------------------------------
# Function: aeqd_crs
#
#' @param lon0        numeric; WGS84 longitude of the projection origin.
#' @param lat0        numeric; WGS84 latitude of the projection origin.
#
#' @return     character; proj4 string to hand to `crs =`.
#
#' @Purpose   : Puts a layer on a metre grid centred on the study area, so a 20 km ring is
#              20 000 ground metres whatever CRS the provider shipped. A provider's own
#              projected CRS carries its own scale factor and does not give that:
#              EPSG:6372
#              is 0.99712 at Mexico City's latitude, which stretched a "20 000 m" ring to
#              20 058 m on the ground.
#
#' @details    Exact along rays from the origin, so the origin only has to be near the
# area.
#              Metro extent only — error grows with distance off-axis.
#
#' @Written_on: July 2026
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
aeqd_crs <- function(lon0, lat0) {
  sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs", lat0, lon0)
}


# --------------------------------------------------------------------------------------------
# Function: utm_epsg
#
#' @param x        sf or sfc object, in any CRS.
#
#' @return     integer; EPSG code of the UTM zone holding the layer's bounding-box
# midpoint.
#
#' @Purpose   : Gives one metric CRS for a metro-scale layer, used for area, distance and
# any
#              geometry repair that must run planar (GEOS) rather than spherical (s2).
#
#' @details    326xx north of the equator, 327xx south. The zone formula is defined on
#              longitude, so the bounding box is taken in lon/lat first.
#
#' @Written_on: July 2026
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
utm_epsg <- function(x) {
  bb   <- sf::st_bbox(sf::st_transform(x, 4326))
  lon  <- as.numeric((bb[["xmin"]] + bb[["xmax"]]) / 2)
  lat  <- as.numeric((bb[["ymin"]] + bb[["ymax"]]) / 2)
  zone <- floor((lon + 180) / 6) + 1

  if (lat >= 0) 32600 + zone else 32700 + zone
}


# --------------------------------------------------------------------------------------------
# Function: aeqd_for
#
#' @param x        sf or sfc object, in any CRS.
#
#' @return     character; proj4 string of an AEQD grid centred on the layer.
#
#' @Purpose   : Picks the AEQD origin from the bounding-box midpoint instead of the
# centroid.
#              A centroid needs st_union() first, and on a lon/lat layer that runs through
#              s2, whose rebuild snaps vertices to a ~1.1 cm grid — enough to collapse the
#              sub-centimetre edges some providers ship into degenerate (duplicate)
#              vertices.
#              A bounding box touches no vertices, so the layer never reaches s2.
#
#' @Written_on: July 2026
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
aeqd_for <- function(x) {
  bb <- sf::st_bbox(sf::st_transform(x, 4326))

  aeqd_crs(lon0 = (bb[["xmin"]] + bb[["xmax"]]) / 2,
           lat0 = (bb[["ymin"]] + bb[["ymax"]]) / 2)
}


# --------------------------------------------------------------------------------------------
# Function: normalize_station
#
#' @param x        character; station names as a provider shipped them.
#
#' @return     character; upper-cased, trimmed, accent-stripped, quote-free names.
#
#' @Purpose   : Gives every stage one spelling per station, so hourly readings, station
#              catalogues and distance matrices join on the same key. Providers ship the
#              same
#              station as "Nezahualcóyotl", "NEZAHUALCOYOTL" and ' "Nezahualcoyotl" '.
#
#' @details    Case, accents and quotes only. It deliberately does NOT repair genuine
#              misspellings — those belong in a city's station_nme_map, where each pair is
#              visible and reviewable. Stations whose names differ by more than accents
#              will
#              still fail to join, which aggregate_idw_exposure() reports rather than
#              hides.
#
#' @Written_by: Marcos Paulo
#' @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
normalize_station <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  gsub('"', "", x)
}


# --------------------------------------------------------------------------------------------
# Function: normalize_key
#
#' @param x        character; station names or filenames to match on.
#
#' @return     character; upper-cased, accent-stripped, alphanumeric-only keys.
#
#' @Purpose  : Matches a station to the file that carries its readings when the two differ
# in
#              punctuation, spacing or case — "Cerro Navia", "CERRO_NAVIA" and "cerro-
#              navia"
#              all reduce to "CERRONAVIA".
#
#' @details    Stricter than normalize_station(): it removes every non-alphanumeric
# character,
#              which is right for filename matching and wrong for a display name. Santiago
#              needs an extra step and therefore keeps its own santiago_normalize_key() in
#              santiago.R: SINCA prefixes its filenames with the region, so "METROPOLITANA
#              DE
#              SANTIAGO" must come off before the station name can be isolated.
#
#' @Written_by: Marcos Paulo
#' @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
normalize_key <- function(x) {
  x <- toupper(x)
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  gsub("[^A-Z0-9]", "", x)
}


# --------------------------------------------------------------------------------------------
# Function: safe_chr
#
#' @param x        geographic identifiers, of any type.
#
#' @return     character; the identifiers without scientific notation or lost digits.
#
#' @Purpose   : Geographic keys are zero-padded codes that arrive as integer64, integer,
#              double or character depending on the reader. as.character() on a double
#              gives
#              "1e+05" for 100000, which then joins to nothing.
#
#' @details    Doubles go through sprintf("%.0f"), which prints all digits. Above 2^53 a
#              double cannot hold an exact integer, so those warn rather than fail
#              silently —
#              the fix there is to read the column as character or integer64 upstream.
#
#' @Written_by: Marcos Paulo
#' @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
safe_chr <- function(x) {
  if (inherits(x, "integer64")) {
    return(as.character(x))
  }

  if (is.character(x)) {
    return(trimws(x))
  }

  if (is.integer(x)) {
    return(as.character(x))
  }

  if (is.numeric(x)) {
    is_bad_large <- !is.na(x) & abs(x) > 2^53

    if (any(is_bad_large)) {
      warning(
        "Large numeric geo IDs may have lost precision before conversion. ",
        "Prefer reading them as character or integer64."
      )
    }

    return(ifelse(is.na(x), NA_character_, sprintf("%.0f", x)))
  }

  as.character(x)
}


# --------------------------------------------------------------------------------------------
# Function: to_iso
#
#' @param x        POSIXct/POSIXlt timestamps.
#
#' @return     character; "YYYY-MM-DD HH:MM:SS" in UTC.
#
#' @Purpose   : One timestamp spelling across the four cities' hourly panels, so a merged
#              panel does not carry three formats and three implicit time zones.
#
#' @Written_by: Marcos Paulo
#' @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
to_iso <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}


# --------------------------------------------------------------------------------------------
# Function: latex_escape
#
#' @param x        character; text destined for a LaTeX table cell.
#
#' @return     character; the same text with LaTeX's special characters escaped.
#
#' @Purpose   : Stops a city or station name containing &, %, _ or $ from breaking the
#              generated .tex at compile time.
#
#' @details    Backslash is escaped first, otherwise it would re-escape the backslashes
# the
#              later substitutions insert.
#
#' @Written_by: Marcos Paulo
#' @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&",  "\\\\&",  x, fixed = TRUE)
  x <- gsub("%",  "\\\\%",  x, fixed = TRUE)
  x <- gsub("\\$", "\\\\$",  x)
  x <- gsub("#",  "\\\\#",  x)
  x <- gsub("_",  "\\\\_",  x)
  x <- gsub("\\{", "\\\\{",  x)
  x <- gsub("\\}", "\\\\}",  x)
  x <- gsub("~",  "\\\\textasciitilde{}",  x, fixed = TRUE)
  gsub("\\^", "\\\\textasciicircum{}", x)
}


# --------------------------------------------------------------------------------------------
# Function: format_int_latex
#
#' @param x numeric; values to render as whole numbers in a LaTeX cell.
#
#' @return  character; rounded, thousands-separated, no scientific notation.
#
#' @details
#   scientific = FALSE matters: population totals run to eight digits, and format() would
#   otherwise print "1.2e+07" into the table.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
format_int_latex <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# --------------------------------------------------------------------------------------------
# Function: write_pq
#
#' @param df          data frame to write.
#' @param dir         string; destination folder.
#' @param name        string; file stem, without extension.
#
#' @return     invisible NULL. Writes <dir>/<name>.parquet.
#
#' @Purpose   : One Parquet writer for the comparison outputs, so compression and the
# tibble
#              conversion are decided once rather than in each caller.
#
#' @details    `dir` is an argument because the four call sites this replaced closed over
# two
#              different folder variables (cmp_dir and out_dir).
#
#' @Written_by: Marcos Paulo
#' @Updated_on: August 2026
# --------------------------------------------------------------------------------------------
write_pq <- function(df, dir, name) {
  arrow::write_parquet(
    dplyr::as_tibble(df),
    file.path(dir, paste0(name, ".parquet")),
    compression = "zstd"
  )

  invisible(NULL)
}


# --------------------------------------------------------------------------------------------
# Function: save_raw_data_tidy_formatted
#
#' @param data                 data.frame or tibble to write.
#' @param out_dir              string; directory to write outputs (created if missing).
#' @param out_name             string|NULL; base filename without extension. If NULL,
#                              inferred from available columns ('city' and 'year').
#' @param write_rds            logical; write .rds (default TRUE).
#' @param write_parquet        logical; write .parquet via {arrow} (default TRUE).
#' @param write_csv_gz         logical; write .csv.gz (default FALSE).
#' @param rds_compress         string; RDS compress method (default "xz").
#' @param parquet_comp         string; Parquet compression codec (default "zstd").
#' @param quiet                logical; suppress messages (default FALSE).
#
#' @return     (invisible) Named list containing the written file paths.
#' @Purpose   : Materializes a dataframe to standard formats with consistent naming.
#' @Written_on: 27/08/2025
#' @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
save_raw_data_tidy_formatted <- function(
    data,
    out_dir,
    out_name        = NULL,
    write_rds       = TRUE,
    write_parquet   = TRUE,
    write_csv_gz    = FALSE,
    rds_compress    = "xz",
    parquet_comp    = "zstd",
    quiet           = FALSE
) {
  # 1. Validate inputs
  stopifnot(is.data.frame(data))
  
  # 2. Ensure output directory exists
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 3. Dynamic Filename Inference (if out_name is not provided)
  infer_stub <- function(df) {
    has_city <- "city" %in% names(df)
    has_year <- "year" %in% names(df)
    
    # Scenario A: Dataset contains both city and year
    if (has_city && has_year) {
      city_val <- as.character(df$city[which(!is.na(df$city))[1]])
      ymin <- suppressWarnings(min(df$year, na.rm = TRUE))
      ymax <- suppressWarnings(max(df$year, na.rm = TRUE))
      
      if (is.finite(ymin) && is.finite(ymax) && nzchar(city_val)) {
        # Strip spaces from city name for clean file naming
        clean_city <- gsub("\\s+", "", city_val)
        return(sprintf("%s_%d_%d", clean_city, ymin, ymax))
      }
    }
    
    # Scenario B: Dataset contains only year
    if (has_year) {
      ymin <- suppressWarnings(min(df$year, na.rm = TRUE))
      ymax <- suppressWarnings(max(df$year, na.rm = TRUE))
      
      if (is.finite(ymin) && is.finite(ymax)) {
        return(sprintf("dataset_%d_%d", ymin, ymax))
      }
    }
    
    # Scenario C: Default fallback
    return("dataset")
  }
  
  if (is.null(out_name) || !nzchar(out_name)) {
    out_name <- infer_stub(data)
  }
  
  # Initialize tracking list for generated paths
  paths <- list(rds = NA_character_, parquet = NA_character_, csv = NA_character_)
  
  # 4. Write RDS Artifact
  if (isTRUE(write_rds)) {
    rds_path <- file.path(out_dir, paste0(out_name, ".rds"))
    saveRDS(data, rds_path, compress = rds_compress)
    
    paths$rds <- normalizePath(rds_path, winslash = "/", mustWork = FALSE)
    if (!quiet) message("[save] Wrote RDS: ", paths$rds)
  }
  
  # 5. Write Parquet Artifact
  if (isTRUE(write_parquet)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for Parquet output. Please install it.")
    }
    
    pq_path <- file.path(out_dir, paste0(out_name, ".parquet"))
    arrow::write_parquet(data, pq_path, compression = parquet_comp)
    
    paths$parquet <- normalizePath(pq_path, winslash = "/", mustWork = FALSE)
    if (!quiet) message("[save] Wrote Parquet: ", paths$parquet)
  }
  
  # 6. Write Compressed CSV Artifact
  if (isTRUE(write_csv_gz)) {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("Package 'readr' is required for CSV output. Please install it.")
    }
    
    csv_path <- file.path(out_dir, paste0(out_name, ".csv.gz"))
    con <- gzfile(csv_path, open = "wt")
    
    # Ensure the connection closes even if write_csv fails
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    
    readr::write_csv(data, con)
    
    paths$csv <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)
    if (!quiet) message("[save] Wrote CSV.GZ: ", paths$csv)
  }
  
  # Return the absolute paths invisibly for downstream use
  invisible(paths)
}


# --------------------------------------------------------------------------------------------
# Function: find_col
#
#' @param dt        data.table to search.
#' @param candidates character; acceptable column names, most preferred first.
#' @param file_label string; used in the error message so a failure names its file.
#
#' @return  string; the first candidate present in dt.
#
#' @details
#   Resolves the column-name differences between providers (station vs station_id vs
#   codigo_estacion). Flagged in doc/deletion_candidates.md: a fallback list is a
#   guardrail
#   standing in for a test, and naming the column explicitly per city would be stricter.
#
#' @Written_by : Marcos Paulo
#' @Updated_on : August 2026
# --------------------------------------------------------------------------------------------
find_col <- function(dt, candidates, file_label) {
  hit <- candidates[candidates %in% names(dt)]
  
  if (length(hit) == 0L) {
    stop(
      "None of these columns were found in ", file_label, ": ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns are: ",
      paste(names(dt), collapse = ", ")
    )
  }
  
  hit[1L]
}
