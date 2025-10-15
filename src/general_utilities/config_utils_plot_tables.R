# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
# 
# @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "plot_table" scripts.
# 
# @Date: Nov 2024
# @Author: Marcos Paulo
# ============================================================================================

# List of required packages
packages <- c(
  "arrow",
  "cowplot",
  "dplyr",
  "ggmap",
  "ggplot2",
  "ggspatial",
  "ggridges",
  "haven",
  "here",
  "htmltools",
  "kableExtra",
  "leaflet",
  "lubridate",
  "rlang",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "showtext",
  "terra",
  "tidyr",
  "viridisLite",
  "zoo")

# Define the default source library for packages installation - may have problems otherwise
options(repos=c(CRAN="https://cran.rstudio.com/"))

# Install (if needed) and load packages
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    renv::install(pkg, configure.args = c("--with-gdal",
                                          "--with-proj",
                                          "--with-geos",
                                          "--with-data-copy=true")
                  )
  }
  library(pkg, character.only = TRUE)
}

# Clear objects on environment
rm(packages, pkg)

# Try installing rnaturalearthhires in different cran
options(repos = c(CRAN = "https://ropensci.r-universe.dev"))
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  renv::install("rnaturalearthhires")
  library(rnaturalearthhires)
}

# Set a global theme with Palatino as the base font
font_add("Palatino", regular = here::here("fonts", "texgyrepagella-regular.otf"))
showtext_auto()
theme_set(theme_minimal(base_family = "Palatino", base_size = 14))

# ############################################################################################
# Helper - Functions
# ############################################################################################

# Pick a Stadia tile (if key present) or use a quiet fallback (CartoDB)
.stadia_tile <- function(style = "stamen_terrain_background",
                         envkey = "STADIA_MAPS_KEY") {
  key <- Sys.getenv(envkey, unset = "")
  if (nzchar(key)) {
    # Stadia hosts Stamen styles; many are .jpg tiles
    url <- sprintf(
      "https://tiles.stadiamaps.com/tiles/%s/{z}/{x}/{y}.jpg?api_key=%s",
      style, key
    )
    list(
      url = url,
      attribution = paste0(
        '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a> ',
        '&copy; <a href="https://stamen.com/">Stamen</a> ',
        '&copy; <a href="https://www.openstreetmap.org/copyright">',
        'OpenStreetMap</a> contributors'
      )
    )
  } else {
    list(url = NULL, attribution = NULL)
  }
}

# ############################################################################################
# Main Functions
# ############################################################################################

# --------------------------------------------------------------------------------------------
# Function: table_state_metro_distances
# @Arg : national_states_sf — sf MULTIPOLYGON of country states (any CRS)
# @Arg : metro_area_sf      — sf (MULTI)POLYGON for the metro area (any CRS)
# @Arg : state_name_col     — column in `national_states_sf` with state names
#                             (default "name"; falls back to common variants)
# @Arg : caption            — LaTeX caption (default auto: country/neutral text)
# @Arg : save_latex_table   — write LaTeX to file? (default FALSE)
# @Arg : out_file           — path to .tex file if saving
# @Arg : overwrite_tex      — overwrite existing .tex? (default FALSE)
# @Arg : quiet              — suppress info messages (default FALSE)
# @Output : data.frame with columns: state_name, distance_km, Potential_source
#           (If save_latex_table = TRUE, also writes a .tex file.)
# @Purpose: Min distance (km) from each state to the metro area (0 for overlaps).
#           Adds an indicator (≤ 20 km) as Potential_source (1/0).
# @Notes  : Distances computed in a local UTM for accuracy; then converted to km.
#           Uses st_make_valid() as a guard for tricky polygons.
# @Written_on: 28/09/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
table_state_metro_distances <- function(
    national_states_sf,
    metro_area_sf,
    state_name_col   = "name",
    caption          = NULL,
    save_latex_table = FALSE,
    out_file         = NULL,
    overwrite_tex    = FALSE,
    quiet            = FALSE
) {
  # ---- 0) deps + input validation --------------------------------------------
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required. install.packages('sf')")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required. install.packages('dplyr')")
  
  stopifnot(inherits(national_states_sf, "sf"),
            inherits(metro_area_sf, "sf"))
  if (nrow(national_states_sf) == 0)
    stop("`national_states_sf` has zero rows.")
  if (nrow(metro_area_sf) == 0)
    stop("`metro_area_sf` has zero rows.")
  
  # ---- 1) pick state-name column (with fallbacks) ----------------------------
  nm_col <- state_name_col
  if (!nm_col %in% names(national_states_sf)) {
    fallbacks <- c("name", "name_es", "name_en", "NAME", "STATE_NAME")
    avail <- intersect(fallbacks, names(national_states_sf))
    if (length(avail) > 0) {
      nm_col <- avail[1]
      if (!quiet) message("Using state name column: '", nm_col, "' (fallback).")
    } else {
      stop("Column '", state_name_col, "' not found. Available: ",
           paste(names(national_states_sf), collapse = ", "))
    }
  }
  
  # ---- 2) make geometries valid and project to local UTM ---------------------
  # local UTM from metro bbox center
  utm_for <- function(sfobj) {
    bb  <- sf::st_bbox(sf::st_transform(sfobj, 4326))
    lon <- as.numeric((bb["xmin"] + bb["xmax"]) / 2)
    lat <- as.numeric((bb["ymin"] + bb["ymax"]) / 2)
    zone <- floor((lon + 180) / 6) + 1
    if (lat >= 0) 32600 + zone else 32700 + zone
  }
  
  states_ok <- sf::st_make_valid(national_states_sf)
  metro_ok  <- sf::st_make_valid(metro_area_sf)
  
  crs_utm   <- utm_for(metro_ok)
  states_utm <- sf::st_transform(states_ok, crs_utm)
  metro_utm  <- sf::st_transform(metro_ok,  crs_utm)
  
  # Treat metro as one geometry
  metro_union <- sf::st_union(metro_utm)
  
  # ---- 3) min distance (km) per state; 0 if intersects ----------------------
  # st_distance returns an n×1 matrix (units in meters)
  dist_m  <- as.numeric(sf::st_distance(states_utm, metro_union))
  dist_km <- dist_m / 1000
  
  # Set overlaps to 0 (robust vs float equality)
  overlaps <- sf::st_intersects(states_utm, metro_union, sparse = FALSE)[, 1]
  dist_km[overlaps] <- 0
  
  # Build result df
  result_df <- dplyr::tibble(
    state_name       = as.character(states_utm[[nm_col]]),
    distance_km      = dist_km,
    Potential_source = as.integer(dist_km <= 20)
  ) |>
    dplyr::arrange(distance_km) %>% 
    dplyr::filter(!is.na(state_name))
  
  # ---- 4) Optional LaTeX export (pretty-printed, booktabs) -------------------
  # ---- 4) Optional LaTeX export (pretty-printed, booktabs) -------------------
  if (isTRUE(save_latex_table)) {
    if (is.null(out_file))
      stop("Provide `out_file` when `save_latex_table = TRUE`.")
    
    # (a) helper: escape LaTeX special chars in text cells
    latex_escape <- function(x) {
      x <- gsub("\\\\", "\\\\textbackslash{}", x)   # backslash
      x <- gsub("&",  "\\\\&",  x, fixed = TRUE)
      x <- gsub("%",  "\\\\%",  x, fixed = TRUE)
      x <- gsub("\\$", "\\\\$",  x)
      x <- gsub("#",  "\\\\#",  x)
      x <- gsub("_",  "\\\\_",  x)
      x <- gsub("\\{", "\\\\{",  x)
      x <- gsub("\\}", "\\\\}",  x)
      x <- gsub("~",  "\\\\textasciitilde{}",  x, fixed = TRUE)
      x <- gsub("\\^", "\\\\textasciicircum{}", x)
      x
    }
    
    # (b) caption (auto if missing)
    if (is.null(caption)) {
      caption <- paste(
        "Administrative states and distance to metropolitan area",
        "(distance in km; Potential source = 1 if $\\leq 20$ km)"
      )
    }
    
    # (c) format table data
    fmt_km <- function(v) format(round(v, 2), big.mark = ",", trim = TRUE)
    df_tbl <- result_df |>
      dplyr::mutate(
        State              = latex_escape(as.character(state_name)),
        `Distance (km)`    = fmt_km(distance_km),
        `Potential source` = ifelse(Potential_source == 1, "1", "0")
      ) |>
      dplyr::select(State, `Distance (km)`, `Potential source`)
    
    # (d) build LaTeX as a vector of lines (pretty-printed)
    # NOTE: requires \usepackage{booktabs} in your preamble
    lines <- c(
      "\\begin{table}[htbp]",
      "  \\centering",
      paste0("  \\caption{", latex_escape(caption), "}"),
      "  \\begin{tabular}{lrr}",
      "    \\midrule",
      "    \\midrule",
      "    \\multicolumn{1}{c}{\\textbf{State}} &",
      "    \\multicolumn{1}{c}{\\textbf{Distance to metro area}} &",
      "    \\multicolumn{1}{c}{\\textbf{Potential pollution source?}} \\\\",
      "    \\multicolumn{1}{c}{} &",
      "    \\multicolumn{1}{c}{\\textbf{(km)}} &",
      "    \\multicolumn{1}{c}{\\textbf{($\\leq 20$ km)}} \\\\",
      "    \\midrule"
    )
    
    # (e) append one line per data row (indented, readable)
    if (nrow(df_tbl) > 0) {
      row_lines <- apply(df_tbl, 1, function(r)
        paste0("    ", r[1], " & ", r[2], " & ", r[3], " \\\\"))
      lines <- c(lines, row_lines)
    }
    
    # (f) close the environment
    lines <- c(
      lines,
      "    \\bottomrule",
      "    \\bottomrule",
      "  \\end{tabular}",
      "  \\label{table_state_metro_distances}",
      "\\end{table}"
    )
    
    # (g) write to file (preserves indentation / one row per line)
    if (file.exists(out_file) && !overwrite_tex) {
      stop("File exists and `overwrite_tex = FALSE`: ", out_file)
    }
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, out_file)
    
    if (!quiet) message("LaTeX table saved → ", normalizePath(out_file))
  }
  
  return(result_df)
}


# --------------------------------------------------------------------------------------------
# Function: plot_metro_area_national_context
# @Arg : national_states_sf — sf MULTIPOLYGON of country states (any CRS)
# @Arg : metro_area_sf      — sf (MULTI)POLYGON for the metro area (any CRS)
# @Arg : which_states       — chr vec; state names to highlight (must match `state_name_col`)
# @Arg : state_name_col     — column in `national_states_sf` with state names (default "name")
# @Arg : map_mode           — 'ggmap' (tiles) | 'sf' (no tiles). If 'ggmap' but the
#                            Stadia Maps key is missing, it will fall back to 'sf'.
# @Arg : basemap_zoom       — numeric zoom for ggmap::get_stadiamap (default 5)
# @Arg : basemap_type       — one of possible options on the ggmap. ('stamen_terrain',
# 'stamen_toner', 'stamen_toner_lite'...)
# @Arg : city_name          — character; used in title (e.g., "Mexico City")
# @Arg : states_border_col  — color for all state borders (default "grey20")
# @Arg : states_border_lwd  — linewidth for borders (default 0.4)
# @Arg : highlight_fill     — fill for highlighted states (default "#F59E0B")
# @Arg : highlight_alpha    — alpha for highlighted states (default 0.20)
# @Arg : highlight_border   — border color for highlighted states (default "#B45309")
# @Arg : metro_fill         — fill for metro polygon (default "#1D4ED8")
# @Arg : metro_alpha        — alpha for metro polygon (default 0.30)
# @Arg : metro_border       — border color for metro polygon (default "#1E3A8A")
# @Arg : add_graticule      — logical; add light graticule lines (default TRUE)
# @Arg : stadiamaps_envkey  — env var name with Stadia key (default "STADIA_MAPS_KEY")
# @Output : ggplot object
# @Purpose: Country map with optional raster tiles, borders, highlighted states,
#           and metro overlay. Legend shows what colors mean and metro area (km²).
# @Notes  : Requires packages: sf, ggplot2. For 'ggmap' mode: ggmap + stadiamaps key.
# @Written_on: 28/09/2025
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_metro_area_national_context <- function(
    national_states_sf,
    metro_area_sf,
    which_states       = NULL,
    state_name_col     = "name",
    map_mode           = c("ggmap", "sf"),
    basemap_zoom       = 5,
    basemap_type       = "stamen_terrain_background",
    city_name          = "the city",
    states_border_col  = "grey20",
    states_border_lwd  = 0.4,
    highlight_fill     = "#F59E0B",
    highlight_alpha    = 0.20,
    highlight_border   = "#B45309",
    metro_fill         = "#1D4ED8",
    metro_alpha        = 0.30,
    metro_border       = "#1E3A8A",
    add_graticule      = TRUE,
    stadiamaps_envkey  = "STADIA_MAPS_API_KEY"
) {
  # ---- 0) deps + theme -------------------------------------------------------
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. install.packages('sf')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. install.packages('ggplot2')")
  }
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 14)
  )
  
  map_mode <- match.arg(map_mode)
  
  # ---- 1) prepare data (CRS → 4326 for tiles; compute metro area) -----------
  states84 <- sf::st_transform(national_states_sf, 4326)
  metro84  <- sf::st_transform(metro_area_sf, 4326)
  
  # pick a UTM CRS based on bbox midpoint (no centroid needed)
  utm_for <- function(sfobj) {
    bb  <- sf::st_bbox(sf::st_transform(sfobj, 4326))
    lon <- as.numeric((bb["xmin"] + bb["xmax"]) / 2)
    lat <- as.numeric((bb["ymin"] + bb["ymax"]) / 2)
    zone <- floor((lon + 180) / 6) + 1
    if (lat >= 0) 32600 + zone else 32700 + zone
  }

  crs_area <- utm_for(metro84)
  metro_u  <- sf::st_make_valid(sf::st_union(metro84))
  metro_u  <- sf::st_transform(metro_u, crs_area)
  area_km2 <- as.numeric(sf::st_area(metro_u)) / 1e6
  area_lab <- round(area_km2, 2)
  
  # ---- 2) choose state-name col, build highlight sf --------------------------
  nm_col <- state_name_col
  if (!nm_col %in% names(states84)) {
    nm_col <- if ("name_es" %in% names(states84)) "name_es" else NULL
  }
  if (is.null(nm_col)) {
    stop("Could not find a state name column. Set `state_name_col`.")
  }
  has_hl   <- !is.null(which_states) && length(which_states) > 0
  states_h <- if (has_hl) {
    states84[states84[[nm_col]] %in% which_states, , drop = FALSE]
  } else {
    states84[0, , drop = FALSE]
  }
  
  # ---- 3) legend labels + levels --------------------------------------------
  lbl_states <- "Downloaded stations data"
  lbl_metro  <- sprintf("Metro area (%s km²)", format(area_lab, big.mark = ","))
  
  # Build the levels present in the plot (states label only if there are any)
  legend_levels <- c(if (nrow(states_h) > 0) lbl_states, lbl_metro)
  
  # Map the fill colors to those *labels* (names must match factor levels)
  fill_values <- stats::setNames(
    c(if (nrow(states_h) > 0) highlight_fill, metro_fill),
    legend_levels
  )
  
  # ---- 4) get raster tiles if requested --------------------------------------
  basemap <- NULL
  if (map_mode == "ggmap") {
    if (!requireNamespace("ggmap", quietly = TRUE)) {
      warning("ggmap not available; falling back to 'sf' mode.")
      map_mode <- "sf"
    } else {
      key <- Sys.getenv(stadiamaps_envkey, unset = "")
      if (!nzchar(key)) {
        warning("No Stadia key in env var '", stadiamaps_envkey,
                "'. Falling back to 'sf' mode.")
        map_mode <- "sf"
      } else {
        ggmap::register_stadiamaps(key)  # safe to call multiple times
        bb  <- sf::st_bbox(states84)
        pad <- 0.4
        bbox <- c(
          left   = as.numeric(bb["xmin"]) - pad,
          bottom = as.numeric(bb["ymin"]) - pad,
          right  = as.numeric(bb["xmax"]) + pad,
          top    = as.numeric(bb["ymax"]) + pad
        )
        basemap <- try(
          ggmap::get_stadiamap(
            bbox = bbox, zoom = basemap_zoom, maptype = basemap_type, crop = TRUE
          ),
          silent = TRUE
        )
        if (inherits(basemap, "try-error")) {
          warning("Stadia request failed; using 'sf' mode.")
          map_mode <- "sf"
          basemap  <- NULL
        }
      }
    }
  }
  
  # ---- 5) draw plot ----------------------------------------------------------
  if (map_mode == "ggmap" && !is.null(basemap)) {
    p <- ggmap::ggmap(basemap)
    
    if (nrow(states_h) > 0) {
      states_h$..layer <- factor(lbl_states, levels = legend_levels)
      p <- p + ggplot2::geom_sf(
        data = states_h, ggplot2::aes(fill = ..layer), inherit.aes = FALSE,
        alpha = highlight_alpha, color = highlight_border,
        linewidth = states_border_lwd
      )
    }
    
    p <- p + ggplot2::geom_sf(
      data = states84, inherit.aes = FALSE,
      fill = NA, color = states_border_col, linewidth = states_border_lwd
    )
    
    metro84$..layer <- factor(lbl_metro, levels = legend_levels)
    p <- p + ggplot2::geom_sf(
      data = metro84, ggplot2::aes(fill = ..layer), inherit.aes = FALSE,
      alpha = metro_alpha, color = metro_border, linewidth = 0.8
    )
    
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = states84, inherit.aes = FALSE,
        fill = "grey98", color = states_border_col,
        linewidth = states_border_lwd
      )
    
    if (nrow(states_h) > 0) {
      states_h$..layer <- factor(lbl_states, levels = legend_levels)
      p <- p + ggplot2::geom_sf(
        data = states_h, ggplot2::aes(fill = ..layer), inherit.aes = FALSE,
        alpha = highlight_alpha, color = highlight_border,
        linewidth = states_border_lwd
      )
    }
    
    metro84$..layer <- factor(lbl_metro, levels = legend_levels)
    p <- p + ggplot2::geom_sf(
      data = metro84, ggplot2::aes(fill = ..layer), inherit.aes = FALSE,
      alpha = metro_alpha, color = metro_border, linewidth = 0.8
    )
  }
  
  # ---- 6) manual legend + styling -------------------------------------------
  p <- p +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = fill_values,
      breaks = legend_levels,
      guide  = ggplot2::guide_legend(override.aes = list(alpha = 0.6))
    ) +
    ggplot2::labs(
      title = sprintf("Metropolitan area of %s — national context", city_name),
      subtitle = if (map_mode == "ggmap")
        "Basemap: Stadia Maps (Stamen styles) via ggmap"
      else
        NULL
    )
  
  if (add_graticule) {
    p <- p + ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey80", linewidth = 0.2)
    )
  } else {
    p <- p + ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )
  }
  
  p <- p + ggplot2::theme(
    axis.title      = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title      = ggplot2::element_text(face = "bold")
  )
  
  # Print and return the plot
  print(p)
  invisible(p)
}


# ============================================================================================
# Function: plot_metro_area_interactive
# @Arg : metro_area_sf   — sf (MULTI)POLYGON of the metro area (any CRS)
# @Arg : stations_sf     — sf POINTS of stations; must include columns:
#                          • code (station_code), • station (name),
#                          • entity (state),      • altitude_m (meters)
# @Arg : pollution_ds    — OPTIONAL Arrow Dataset (or dplyr tbl) with columns:
#                          station_code, year, pm10, `pm2.5`  (default NULL)
# @Arg : legacy_df       — OPTIONAL tibble with columns station, year
#                          (used only for color_scheme = "legacy2023")
# @Arg : filter_type     — one of:
#                          "none"               : no filter
#                          "has_pm_any"         : any non-NA in pm10 OR pm2.5 (any year)
#                          "has_pm_in_year"     : non-NA in selected pollutant & year
#                          "has_both_in_year"   : non-NA in BOTH pm10 & pm2.5 for year
# @Arg : filter_year     — integer year used by *_in_year filters (default 2023)
# @Arg : pollutant       — "none", "pm25", or "pm10" (used only by has_pm_in_year)
# @Arg : color_scheme    — "entity" or "legacy2023"
#                          • entity     : color by stations_sf$entity
#                          • legacy2023 : color by presence in legacy_df (year==2023)
# @Arg : buffer_km       — numeric; radius for outside-station buffers (default 20)
# @Arg : city_name       — character; used in the map title/control
# @Arg : stadiamaps_key  — character; your Stadia Maps API key. If empty, function
#                          falls back to CartoDB Positron tiles (no key needed).
#                          A convenient pattern is to pass:
#                          Sys.getenv("STADIA_MAPS_API_KEY", unset = "")
# @Arg : tileset         — Stadia tileset id (e.g. "stamen_terrain_background",
#                          "stamen_toner", "stamen_watercolor").
# @Output : leaflet htmlwidget (interactive map)
# @Purpose : Interactive metro map with basemap, metro polygon, station points,
#            optional 20-km buffers for stations outside the metro polygon,
#            optional filters from a parquet/Arrow dataset, and info-rich tooltips.
#            The corner box shows the city + number of stations; legend labels
#            include per-category counts.
# @Written_on: 30/09/2025
# @Written_by: Marcos Paulo
# ============================================================================================
plot_metro_area_interactive <- function(
    metro_area_sf,
    stations_sf,
    pollution_ds   = NULL,
    legacy_df      = NULL,
    filter_type    = c("none", "has_pm_any", "has_pm_in_year", "has_both_in_year"),
    filter_year    = 2023,
    pollutant      = c("none", "pm25", "pm10"),
    color_scheme   = c("entity", "legacy2023"),
    buffer_km      = 20,
    city_name      = "the city",
    stadiamaps_key = Sys.getenv("STADIA_MAPS_API_KEY", unset = ""),
    tileset        = "stamen_terrain_background"
) {
  # ---- 0) Dependencies & argument checks ------------------------------------
  stopifnot(inherits(metro_area_sf, "sf"), inherits(stations_sf, "sf"))
  for (pkg in c("leaflet", "sf", "dplyr", "rlang", "htmltools", "RColorBrewer"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' is required. Please install it.")
  
  filter_type  <- match.arg(filter_type)
  pollutant    <- match.arg(pollutant)
  color_scheme <- match.arg(color_scheme)
  
  # If no Stadia key, we *warn* and later fall back to CartoDB Positron tiles
  if (!nzchar(stadiamaps_key)) {
    # Try alternate env var name before warning (handy if you stored a different key)
    stadiamaps_key <- Sys.getenv("STADIA_MAPS_KEY", unset = "")
    if (!nzchar(stadiamaps_key)) {
      message("ℹ No Stadia key supplied; using CartoDB Positron fallback basemap.")
    }
  }
  
  # ---- 1) Make sure station columns exist & work in WGS84 for leaflet --------
  needed_cols <- c("code", "station", "entity", "altitude_m")
  miss <- setdiff(needed_cols, names(stations_sf))
  if (length(miss)) stop("stations_sf is missing: ", paste(miss, collapse = ", "))
  
  metro_84    <- sf::st_transform(metro_area_sf, 4326)
  stations_84 <- sf::st_transform(stations_sf, 4326)
  
  # ---- 2) (Optional) build a station-code filter from the Arrow dataset ------
  # This block only runs if a filter was requested. It returns character vector
  # 'codes_keep' with station codes that pass the filter; stations outside this
  # set are dropped from the plot.
  codes_keep <- NULL
  if (filter_type != "none") {
    if (is.null(pollution_ds))
      stop("filter_type='", filter_type, "' requires 'pollution_ds'.")
    
    # Arrow/dplyr quirk: backtick the pm2.5 column name via parse_expr
    pm25_expr <- rlang::parse_expr("`pm2.5`")
    pm10_expr <- rlang::sym("pm10")
    
    if (!all(c("station_code", "year") %in% names(pollution_ds)))
      stop("pollution_ds must contain 'station_code' and 'year'.")
    
    if (filter_type == "has_pm_any") {
      codes_keep <- pollution_ds |>
        dplyr::select(station_code, !!pm10_expr, !!pm25_expr) |>
        dplyr::filter(!is.na(!!pm10_expr) | !is.na(!!pm25_expr)) |>
        dplyr::distinct(station_code) |>
        dplyr::collect() |>
        dplyr::pull(station_code)
      
    } else if (filter_type == "has_pm_in_year") {
      if (pollutant == "none")
        stop("For 'has_pm_in_year' set pollutant = 'pm25' or 'pm10'.")
      col_expr <- if (pollutant == "pm25") pm25_expr else pm10_expr
      
      codes_keep <- pollution_ds |>
        dplyr::select(station_code, year, !!col_expr) |>
        dplyr::filter(year == !!filter_year, !is.na(!!col_expr)) |>
        dplyr::distinct(station_code) |>
        dplyr::collect() |>
        dplyr::pull(station_code)
      
    } else if (filter_type == "has_both_in_year") {
      codes_keep <- pollution_ds |>
        dplyr::select(station_code, year, !!pm10_expr, !!pm25_expr) |>
        dplyr::filter(year == !!filter_year) |>
        dplyr::group_by(station_code) |>
        dplyr::summarise(
          has10 = any(!is.na(!!pm10_expr)),
          has25 = any(!is.na(!!pm25_expr)),
          .groups = "drop"
        ) |>
        dplyr::filter(has10 & has25) |>
        dplyr::collect() |>
        dplyr::pull(station_code)
    }
  }
  
  if (!is.null(codes_keep)) {
    stations_84 <- stations_84 |>
      dplyr::filter(.data$code %in% codes_keep)
  }
  
  # ---- 3) Compute 20-km buffers for stations outside the metro polygon -------
  # We need metric units to buffer distances accurately. Choose a local UTM zone
  # from the *bbox center* (no st_point_on_surface on lon/lat → avoids warnings).
  utm_for <- function(sfobj_wgs84) {
    bb  <- sf::st_bbox(sfobj_wgs84)     # xmin, ymin, xmax, ymax (lon/lat)
    lon <- (bb["xmin"] + bb["xmax"]) / 2
    lat <- (bb["ymin"] + bb["ymax"]) / 2
    zone <- floor((lon + 180) / 6) + 1
    if (is.na(zone)) zone <- 14         # safe default near central MX
    if (lat >= 0) 32600 + zone else 32700 + zone
  }
  epsg_loc   <- utm_for(metro_84)
  metro_m    <- sf::st_transform(metro_84, epsg_loc)
  stations_m <- sf::st_transform(stations_84, epsg_loc)
  
  # Identify stations within the metro polygon; buffer those outside by buffer_km
  inside_lgl <- sf::st_within(
    stations_m, sf::st_union(metro_m), sparse = FALSE
  )[, 1]
  outside_m  <- stations_m[!inside_lgl, , drop = FALSE]
  buffers_m  <- if (nrow(outside_m)) sf::st_buffer(outside_m, buffer_km * 1000) else outside_m
  buffers_84 <- sf::st_transform(buffers_m, 4326)
  
  # ---- 4) Color scheme + counts to display in the legend ---------------------
  # We build:
  #  • 'category' vector for each station (entity or presence/absence),
  #  • 'counts' of each category,
  #  • a color palette over unique categories,
  #  • and final *labels with counts* for the legend.
  if (color_scheme == "entity") {
    category      <- stations_84$entity
    cats          <- sort(unique(category))
    category      <- factor(category, levels = cats)
    counts        <- as.integer(tabulate(factor(category, levels = cats)))
    # Color palette: Dark2 with fallback recycle
    palette_cols  <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(8, "Dark2")
    )(length(cats))
    pal <- leaflet::colorFactor(palette = palette_cols, domain = cats,
                                na.color = "black")
    legend_title  <- "Entity (state)"
    legend_labels <- sprintf("%s (%d)", cats, counts)
    
  } else {
    # legacy2023: need a set of station *codes* present in 2023 in legacy_df
    if (is.null(legacy_df))
      stop("color_scheme='legacy2023' requires 'legacy_df' (station, year).")
    if (!all(c("station", "year") %in% names(legacy_df)))
      stop("legacy_df must have columns 'station' and 'year'.")
    
    # Find the stations in the legacy dataframe for a given filter
    if (filter_type != "none") {
      if (filter_type == "has_pm_any") {
        present_legacy <- legacy_df |>
          dplyr::filter(!is.na(pm10) | !is.na(pm25)) |>
          dplyr::distinct(station) |>
          dplyr::collect() |>
          dplyr::pull(station)
      } else if (filter_type == "has_pm_in_year") {
        col_expr <- if (pollutant == "pm25") "pm25" else "pm10"
        present_legacy <- legacy_df |>
          dplyr::filter(year == filter_year, !is.na(.data[[col_expr]])) |>
          dplyr::distinct(station) |>
          dplyr::pull(station)
      } else if (filter_type == "has_both_in_year") {
        present_legacy <- legacy_df |>
          dplyr::filter(year == filter_year, !is.na(pm10), !is.na(pm25)) |>
          dplyr::distinct(station) |>
          dplyr::pull(station)
      }} else {
        present_legacy <- legacy_df |>
          dplyr::distinct(station) |>
          dplyr::pull(station)
      }
    
    # Create a list of stations that exists in the legacy dataframe
    new_in_legacy  <- stations_84 %>% 
      dplyr::filter(entity == "CDMX") %>% # Only CDMX stations in the legacy dataframe
      dplyr::filter(code %in% present_legacy) %>%
      dplyr::pull(station)
    
    is_in  <- stations_84$station %in% new_in_legacy
    cats   <- c("Present in replication", "Not in replication")
    category <- ifelse(is_in, cats[1], cats[2])
    counts   <- as.integer(tabulate(factor(category, levels = cats)))
    category <- factor(category, levels = cats)
    
    legend_title  <- "Legacy presence"
    legend_labels <- c(sprintf("%s (%d)", cats[1], counts[1]),
                       sprintf("%s (%d)", cats[2], counts[2]))
    
    # names correspond to domain values
    pal_vec <- setNames(c("#9CA3AF", "#1D4ED8"), legend_labels)
    
    # Make color palette
    pal <- leaflet::colorFactor(
      palette = pal_vec,
      domain  = cats,
      na.color = "#9CA3AF"
    )

  }
  
  # ---- 5) Labels & popups (enforce 0/NA altitude → NA in display) ------------
  alt_disp <- stations_84$altitude_m
  alt_disp[is.na(alt_disp) | alt_disp == 0] <- NA_real_  # display rule
  
  fmt_num <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 0,
                                                        big.mark = ","))
  stations_84$.label <- sprintf(
    "%s (%s)<br/>Entity: %s<br/>Altitude: %s m",
    htmltools::htmlEscape(stations_84$station),
    htmltools::htmlEscape(stations_84$code),
    htmltools::htmlEscape(stations_84$entity),
    fmt_num(alt_disp)
  )
  stations_84$.popup <- htmltools::HTML(stations_84$.label)
  
  # ---- 6) Basemap URL (Stadia if key available; else CartoDB Positron) -------
  if (nzchar(stadiamaps_key)) {
    tile_url <- sprintf(
      "https://tiles.stadiamaps.com/tiles/%s/{z}/{x}/{y}.png?api_key=%s",
      tileset, stadiamaps_key
    )
    tile_attr <- paste0(
      '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a>, ',
      '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a>, ',
      'Map tiles & styles by Stamen/Stadia'
    )
    base_group <- "Stadia"
  } else {
    # Nice, light fallback that needs no key
    tile_url <- "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png"
    tile_attr <- paste0(
      '&copy; <a href="https://carto.com/attributions">CARTO</a> | ',
      '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a>'
    )
    base_group <- "CartoDB Positron"
  }
  
  # ---- 7) Fit view to metro area bbox ----------------------------------------
  bbox84 <- sf::st_bbox(metro_84)
  view   <- unname(c(bbox84["ymin"], bbox84["xmin"], bbox84["ymax"], bbox84["xmax"]))
  
  # ---- 8) Build leaflet map (no 'if' mid-pipe; mutate object instead) --------
  m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 3))
  m <- m |>
    leaflet::addTiles(urlTemplate = tile_url,
                      attribution = tile_attr,
                      group = base_group) |>
    leaflet::fitBounds(lng1 = view[2], lat1 = view[1],
                       lng2 = view[4], lat2 = view[3])
  
  # Metro area polygon
  m <- m |>
    leaflet::addPolygons(
      data = metro_84,
      color = "#1E3A8A", weight = 1, fillColor = "#1D4ED8",
      fillOpacity = 0.25, group = "Metro area",
      highlightOptions = leaflet::highlightOptions(
        weight = 2, color = "#0F172A", bringToFront = TRUE
      )
    )
  
  # Optional 20-km buffers (only if there are stations outside the metro)
  if (nrow(buffers_84) > 0) {
    m <- m |>
      leaflet::addPolygons(
        data = buffers_84, color = "#7C3AED", weight = 1,
        fillColor = "#7C3AED", fillOpacity = 0.15,
        group = "20 km buffers",
        label = "20 km buffer (outside metro)"
      )
  }
  
  # Stations as circle markers
  m <- m |>
    leaflet::addCircleMarkers(
      data = stations_84,
      radius = 6,
      stroke = TRUE, color = "#111827", weight = 1,
      fillColor = pal(category), fillOpacity = 0.9,
      label = lapply(stations_84$.label, htmltools::HTML),
      popup = stations_84$.popup,
      group = "Stations"
    )
  
  # ---- 9) Legend with counts (labels show “… (n)”) ---------------------------
  # We pass both 'pal' and fixed 'labels' so the legend shows counts. We also
  # ensure the legend uses the same category order we computed above.
  m <- m |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = pal(cats),
      labels   = legend_labels,
      values   = category,
      na.label = "Not Available",
      opacity  = 0.9,
      title    = legend_title
    )
  
  # ---- 10) Layers control -----------------------------------------------------
  overlay_groups <- c("Metro area", "Stations")
  if (nrow(buffers_84) > 0) overlay_groups <- c(overlay_groups, "20 km buffers")
  
  m <- m |>
    leaflet::addLayersControl(
      baseGroups    = c(base_group),
      overlayGroups = overlay_groups,
      options       = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  # ---- 11) Corner box: city name + TOTAL station count -----------------------
  total_stations <- nrow(stations_84)
  box_html <- sprintf(
    '<div style="background:rgba(255,255,255,.85);padding:.45em .65em;
                border-radius:6px; line-height:1.15;">
       <b>%s</b><br/>
       <span style="font-size:90%%;">
         Number of stations shown: <b>%s</b><br/>
         Hover for details • Click for popup
       </span>
     </div>',
    htmltools::htmlEscape(sprintf("Metro area of %s", city_name)),
    formatC(total_stations, format = "d", big.mark = ",")
  )
  
  m <- m |>
    leaflet::addControl(html = box_html, position = "topleft")
  
  # ---- 12) Return htmlwidget --------------------------------------------------
  m
}


# --------------------------------------------------------------------------------------------
# Function: plot_merra2_grid_city
# @Arg       : shapefile is an 'sf' object representing the city boundary
# @Arg       : nc_file is a string containing the path to a single .nc4 file
#              from the MERRA-2 dataset
# @Arg       : city_name is a string with the name of the city
# @Output    : A ggplot object representing the map of the city boundary and 
#              MERRA-2 grid cells
# @Purpose   : Creates a spatial plot showing the city's boundary and the overlayed
#              MERRA-2 grid cells from the specified nc_file. This visualization helps 
#              in understanding the spatial extent of the MERRA-2 data relative to 
#              the city's area.
# @Written_on: 10/12/2024
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_merra2_grid_city <- function(shapefile, nc_file, city_name) {
  
  # Load the MERRA-2 data
  nc_data <- rast(nc_file)
  
  # Crop the raster to the extent of the shapefile plus a buffer (optional)
  # buffer_distance <- 0.0001  # Degrees, adjust as needed
  # shapefile_buffered <- st_buffer(shapefile, dist = buffer_distance)
  
  # Transform shapefile to match MERRA-2 CRS if necessary
  # crs(nc_data) <- "EPSG:4326"
  shapefile_proj <- st_transform(shapefile, crs = crs(nc_data))
  
  # Crop MERRA-2 data to the city's extent for visualization
  nc_data_cropped <- terra::crop(nc_data, vect(shapefile_proj), snap = "out")
  nc_data_masked  <- terra::mask(nc_data_cropped, vect(shapefile_proj))
  
  # Convert raster to polygons to represent grid cells
  grid_cells    <- as.polygons(nc_data_cropped, dissolve = FALSE, values = FALSE)
  grid_cells_sf <- st_as_sf(grid_cells)
  
  # Calculate centroids for grids
  grid_centroids <- st_centroid(grid_cells_sf)
  
  # Create a ggplot
  p <- ggplot() +
    geom_sf(data = grid_cells_sf, fill = NA, color = "navy", size = 0.3) +
    geom_sf(data = shapefile_proj, fill = "tan4", color = "grey10", size = 0.001) +
    geom_sf(data = grid_centroids, shape = 3, fill = "navy", color = "navy", size = 1.5) +
    labs(
      title = paste("MERRA-2 Grid Cells over", city_name),
      x = "Longitude", 
      y = "Latitude"
    ) +
    coord_sf() +
    scale_x_continuous(name = "Longitude", breaks = seq(-180, 180, by = 0.5)) +
    scale_y_continuous(name = "Latitude", breaks = seq(-90, 90, by = 0.5)) +
    theme_set(theme_minimal(base_family = "Palatino", base_size = 14)) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "gray80", size = 14, face = "bold"),
      axis.text = element_text(color = "gray80", size = 12),
      axis.ticks = element_line(color = "gray80"),
      axis.line = element_line(color = "gray80", linewidth = 0.03),
      plot.title = element_text(face = "bold", hjust = 0.5, color = "black", size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "black"),
      panel.border = element_blank(),
      legend.position = "none",
    )
  
  print(p)
  
  # Return the plot object
  return(p)
}


# --------------------------------------------------------------------------------------------
# Function: plot_variable_across_cities
# @Arg       : df_list is a named list of dataframes, where each dataframe contains
#              aerosol concentration and PM 2.5 data.
# @Arg       : variable is a string specifying the variable to plot (e.g., "DUSMASS25",
#              "OCSMASS", "pm25_estimate").
# @Arg       : var_label is a string specifying the label for the x-axis and plot title.
#              Defaults to the variable name if not provided.
# @Arg.      : max_x_limit is a number representing the right limit for the x axis
# @Output    : A single density plot comparing the variable across cities.
# @Purpose   : Generate a density plot for a specific aerosol or PM 2.5 concentration
#              across multiple cities, including WHO PM 2.5 guidelines if applicable.
# @Written_on: 13/12/2024
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_variable_across_cities <- function(df_list,
                                        variable,
                                        var_label = NULL,
                                        max_y_limit = NULL,
                                        max_x_limit = NULL) {
  
  # Set a default label if none is provided
  if (is.null(var_label)) {
    var_label <- variable
  }
  
  # Ensure all dataframes in the list contain the specified variable
  if (!all(sapply(df_list, function(df) variable %in% names(df)))) {
    stop(paste("All dataframes must contain the variable:", variable))
  }
  
  # Combine dataframes into one with a city identifier
  combined_df <- do.call(rbind, lapply(names(df_list), function(city) {
    df <- df_list[[city]]
    df$City <- city  # Add city name as a new column
    return(df)
  }))
  
  # Ensure the variable is numeric
  combined_df[[variable]] <- as.numeric(combined_df[[variable]])
  
  # Create the density plot
  p <- ggplot(combined_df, aes(x = .data[[variable]], color = City, fill = City)) +
    geom_density(alpha = 0.3, linewidth = 1) +
    labs(
      title = paste("Density Plot of", var_label),
      x = var_label,
      y = "Density"
    ) +
    theme_minimal(base_family = "Palatino", base_size = 14) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  # Apply custom x-axis limits if provided
  if (!is.null(max_x_limit)) {
    p <- p + scale_x_continuous(limits = c(NA, max_x_limit))  # Keep default min, set max
  }
  
  # Add WHO lines if the variable is PM 2.5
  if (variable == "pm25_estimate") {
    # Apply custom y-axis limit for the segments
    if (is.null(max_y_limit)){
      # Calculate max_y for the plot to position the lines and labels
      dens <- density(combined_df[[variable]], na.rm = TRUE)
      max_y <- max(dens$y)
    } else {
      max_y <- max_y_limit
    }

    p <- p +
      geom_segment(x = 25,
                   xend = 25,
                   y = 0,
                   yend = max_y,
                   linetype = "dashed",
                   color = "orange",
                   linewidth = 0.5) +
      geom_segment(x = 35,
                   xend = 35,
                   y = 0,
                   yend = max_y,
                   linetype = "dashed",
                   color = "darkred",
                   linewidth = 0.5) +
      annotate("text",
               x = 25.5,
               y = max_y - 0.01,
               label = "IT2",
               color = "orange",
               size = 4,
               hjust = 0) +
      annotate("text",
               x = 35.5,
               y = max_y - 0.01,
               label = "IT1",
               color = "darkred",
               size = 4,
               hjust = 0)
  }
  
  print(p)
  invisible(p)
}


# --------------------------------------------------------------------------------------------
# Function: plot_latin_america_map
# @Arg       : latin_america - An 'sf' object representing Latin America map.
# @Arg       : regions       - A list of 'sf' objects for metropolitan areas 
#                              (e.g., Bogota, ciudad_mexico, etc.).
# @Arg       : region_names  - A vector of city names corresponding to 'regions'.
# @Arg       : outline       - Logical; if TRUE, regions will be outlined.
# @Output    : A high-quality map with scale bar, compass, and customized aesthetics.
# @Purpose   : Produce a publication-ready map highlighting metropolitan regions 
#              over Latin America with optional outlines.
# @Written_on: 15/12/2024
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_latin_america_map <- function(latin_america, regions, region_names, outline = TRUE) {
  # Check input validity
  if (length(regions) != length(region_names)) {
    stop("The length of 'regions' and 'region_names' must match.")
  }
  
  # Predefined colors for each region
  region_colors <- c("Bogotá" = "#4C72B0", 
                     "Ciudad de México" = "#DD8452", 
                     "Santiago" = "aquamarine4", 
                     "São Paulo" = "darkred")
  
  # Ensure all regions have the same CRS and add an identifier column
  regions <- lapply(1:length(regions), function(i) {
    region <- regions[[i]]
    region <- st_transform(region, crs = 4326) # Ensure consistent CRS
    region_union <- st_union(region)  # Aggregate into one polygon
    
    # Simplify the geometry to remove internal details
    region_simplified <- st_simplify(region_union, dTolerance = 0.01, preserveTopology = TRUE)
    
    # Convert to sf object with simplified geometry
    region_sf <- st_sf(region_name = region_names[i], geometry = region_simplified)
    
    return(region_sf)
  })
  
  # Combine all regions into a single sf object
  combined_regions <- do.call(rbind, regions)  # Preserve attributes, including 'region_name'
  
  # Determine bounding box for the regions to "zoom" the map
  bbox <- st_bbox(combined_regions)
  xlim <- c(bbox["xmin"] - 2, bbox["xmax"] + 2)  # Add small buffer for aesthetics
  ylim <- c(bbox["ymin"] - 2, bbox["ymax"] + 2)
  
  # Base map: Latin America
  base_map <- ggplot() +
    # Plot Latin America background
    geom_sf(data = st_transform(latin_america, crs = 4326), 
            fill = "gray85", color = "white", size = 0.2) +
    
    # Plot the metropolitan regions
    geom_sf(data = combined_regions, aes(fill = region_name), lwd = 0, alpha = 0.7) +
    
    # Zoom the map to focus on the regions
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    # Custom fill colors and legend title
    scale_fill_manual(
      values = region_colors, 
      name = "Metropolitan Regions") +
    
    # Add map labels and aesthetics
    labs(
      title = "Metropolitan Regions in Latin America",
      x = "Longitude", y = "Latitude", fill = "Regions"
    ) +
    theme_set(theme_minimal(base_family = "Palatino", base_size = 14)) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
  
  # Add a scale bar and north arrow
  base_map <- base_map +
    # annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           style = north_arrow_fancy_orienteering())
  
  # Print the map
  print(base_map)
  
  # Return the map
  return(base_map)
}


# --------------------------------------------------------------------------------------------
# Function: plot_city_distributions
# @Arg         : df is a data frame containing columns "Date", "Hour", 
#                "DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS" and "pm25_estimate".
# @Arg         : city_name is a string representing the city's name.
# @Output      : A list of five ggplot objects, each representing the distribution 
#                of a selected variable for the given city.
# @Purpose     : This function creates five distribution plots (histograms) for 
#                selected variables from the given data frame. The chosen variables are:
#                - DUSMASS25, OCSMASS, BCSMASS, SSSMASS25, and pm25_estimate.
#                For pm25_estimate, two vertical lines are added to indicate WHO guidelines:
#                Interim Target 1 (IT1): 35 µg/m³
#                Interim Target 2 (IT2): 25 µg/m³
# @Written_on  : 13/12/2024
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_city_distributions <- function(df, city_name) {
  
  # Associate variable names with descriptions
  variable_descriptions <- list(
    DUSMASS25 = "Dust Surface Mass Concentration (PM 2.5) in µg/m³",
    OCSMASS = "Organic Carbon Surface Mass Concentration in µg/m³",
    BCSMASS = "Black Carbon Surface Mass Concentration in µg/m³",
    SSSMASS25 = "Sea Salt Surface Mass Concentration (PM 2.5) in µg/m³",
    SO4SMASS = "SO4 Surface Mass Concentration in µg/m³",
    pm25_estimate = "PM 2.5 (MERRA-2) in µg/m³")
  
  # Variables to plot (5 in total)
  vars_to_plot <- names(variable_descriptions)
  
  # Check if required columns exist
  if (!all(vars_to_plot %in% names(df))) {
    stop("The data frame must contain: 
         DUSMASS25, OCSMASS, BCSMASS, SSSMASS25, and pm25_estimate.")
  }
  
  # Certify all columns have the required class
  df[vars_to_plot] <- sapply(df[, vars_to_plot], as.numeric)
  
  # Initialize a list to store plots
  plot_list <- list()
  
  for (var in vars_to_plot) {
    p <- ggplot(df, aes(x = .data[[var]])) +
      geom_density(fill = "chocolate4", color = "black", alpha = 0.5, linewidth = 0.8) +
      labs(
        title = paste(city_name, "-", variable_descriptions[[var]]),
        x = variable_descriptions[[var]],
        y = "Density") +
      theme_minimal(base_family = "Palatino", base_size = 14) +
      theme(
        axis.title = element_text(color = "black", face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5))
    
    # If the variable is pm25_estimate, add vertical lines for IT1 and IT2
    if (var == "pm25_estimate") {
      # Calculate density to find the highest point
      dens <- density(df[[var]], na.rm = TRUE)
      max_y <- max(dens$y) 
      max_x <- dens$x[which.max(dens$y)]
      
      # Add vertical lines and labels for WHO limits
      p <- p +
        geom_segment(x = 25, xend = 25, y = 0, yend = max_y, 
                     color = "orange", linetype = "dashed", linewidth = 0.5) +
        geom_segment(x = 35, xend = 35, y = 0, yend = max_y, 
                     color = "darkred", linetype = "dashed", linewidth = 0.5) +
        annotate("text", x = 26.1, y = (max_y - 0.01), label = "IT2", vjust = -1, 
                 color = "orange", size = 3) +
        annotate("text", x = 36.1, y = (max_y - 0.01), label = "IT1", vjust = -1, 
                 color = "darkred", size = 3)
      # +
      # labs(
        #   subtitle = "WHO Interim Targets: IT1 = 35 µg/m³, IT2 = 25 µg/m³")
    }
    
    plot_list[[var]] <- p
  }
  
  print(plot_list)
  
  return(plot_list)
}


# --------------------------------------------------------------------------------------------
# Function: save_plot_list_to_pdf
# @Arg       : plot_list is a list of ggplot objects for a single city.
# @Arg       : city_name is a string specifying the name of the city.
# @Arg       : output_dir is a string specifying the directory to save the PDFs.
# @Output    : Saves a single PDF for the provided plot list.
# @Purpose   : Save each list of plots into a separate PDF efficiently.
# @Written_on: 13/12/2024
# @Written_by: Marcos Paulo
# --------------------------------------------------------------------------------------------
save_plot_list_to_pdf <- function(plot_list, city_name, output_dir) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Define the output PDF file path
  output_file <- file.path(output_dir, paste0(city_name, "_aerosols.pdf"))
  
  # Open the PDF device
  pdf(file = output_file, width = 16, height = 9)
  
  # Save each plot in the list to the PDF
  for (plot_name in names(plot_list)) {
    print(plot_list[[plot_name]])
  }
  
  # Close the PDF device
  dev.off()
  
  # Print confirmation
  cat("Saved:", output_file, "\n")
}


# --------------------------------------------------------------------------------------------
# Function: plot_pm25_timeseries_smooth
# @Arg         : df is a data frame with columns "Date", "Hour", and PM 2.5 data.
# @Arg         : region_name is a string representing the region or city name.
# @Arg         : apply_rolling is a logical indicating whether to apply a rolling window.
# @Arg         : window_hours is an integer specifying size (in hours) of the rolling window.
# @Arg         : corr_method is a string specifying the correlation method ("pearson" default).
# @Arg         : color_merra2 is a string specifying the color for the MERRA-2 series.
# @Arg         : color_stations is a string specifying the color for the ground station series.
# @Output      : A ggplot object showing the PM2.5 time series (raw or optionally smoothed),
#                with an annotation showing the correlation (based on raw data).
# @Purpose     : To visualize PM2.5 from MERRA-2 and ground stations, optionally applying a 
#                rolling average to reduce noise, and annotate the correlation of the raw data.
# @Written_on  : 14/02/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_pm25_timeseries_smooth <- function(df, 
                                        region_name, 
                                        apply_rolling  = TRUE, 
                                        window_hours   = 24,
                                        corr_method    = "pearson",
                                        color_merra2   = "darkred",
                                        color_stations = "darkblue") {

  # Ensure a proper Datetime column
  df <- df %>%
    mutate(
      Datetime = as.POSIXct(paste(Date, sprintf("%02d:00:00", Hour)),
                            format = "%Y-%m-%d %H:%M:%S")
    ) %>%
    arrange(Datetime)
  
  # Compute correlation on raw data
  corr_value <- cor(df$pm25_merra2, df$pm25_stations,
                    use = "pairwise.complete.obs", method = corr_method)
  
  # If rolling is applied, compute rolling means
  if (apply_rolling) {
    df <- df %>%
      mutate(
        pm25_merra2_smooth   = rollmean(pm25_merra2, k = window_hours, fill = NA,
                                        align = "right"),
        pm25_stations_smooth = rollmean(pm25_stations, k = window_hours, fill = NA,
                                        align = "right")
      )
  }
  
  if (apply_rolling) {
    # Compute rolling means
    df <- df %>%
      mutate(
        pm25_merra2_smooth   = rollmean(pm25_merra2,   k = window_hours,
                                        fill = NA, align = "right"),
        pm25_stations_smooth = rollmean(pm25_stations, k = window_hours,
                                        fill = NA, align = "right")
      )
    
    # Define legend labels for smoothed data
    legend_names <- c(
      paste0("MERRA-2 (", window_hours, "hr MA)"),
      paste0("Stations (", window_hours, "hr MA)")
    )
    
    # Build plot with smoothed lines using setNames() for color vector
    p <- ggplot(df, aes(x = Datetime)) +
      geom_line(aes(y = pm25_merra2_smooth, color = legend_names[1]),
                linewidth = 0.7, na.rm = TRUE) +
      geom_line(aes(y = pm25_stations_smooth, color = legend_names[2]),
                linewidth = 0.7, na.rm = TRUE) +
      scale_color_manual(values = setNames(c(color_merra2, color_stations), legend_names)) +
      labs(
        title = paste("PM2.5 Time Series in", region_name, "(Rolling", window_hours, "hrs)"),
        x     = "Datetime",
        y     = "PM2.5 (µg/m³)",
        color = "Legend"
      ) +
      theme_set(theme_minimal(base_family = "Palatino", base_size = 14))
    
  } else {
    # Build plot with raw lines
    p <- ggplot(df, aes(x = Datetime)) +
      geom_line(aes(y = pm25_merra2, color = "MERRA-2"), linewidth = 0.2, na.rm = TRUE) +
      geom_line(aes(y = pm25_stations, color = "Stations"), linewidth = 0.2, na.rm = TRUE) +
      scale_color_manual(values = c("MERRA-2" = color_merra2, "Stations" = color_stations)) +
      labs(
        title = paste("PM2.5 Time Series in", region_name, "(Raw)"),
        x     = "Datetime",
        y     = "PM2.5 (µg/m³)",
        color = "Legend"
      ) +
      theme_set(theme_minimal(base_family = "Palatino", base_size = 14))
  }
  
  # Determine annotation placement using raw data limits
  max_datetime <- max(df$Datetime, na.rm = TRUE)
  y_max <- if (apply_rolling) {
    max(c(df$pm25_merra2_smooth, df$pm25_stations_smooth), na.rm = TRUE)
  } else {
    max(c(df$pm25_merra2, df$pm25_stations), na.rm = TRUE)
  }
  
  # Annotate the correlation at the top-right corner
  p <- p + annotate(
    "text",
    x = max_datetime - 2000,
    y = y_max + 2,
    label = paste0("Correlation (", corr_method, "): ", round(corr_value, 2)),
    hjust = 1, vjust = 1, size = 3, color = "black"
  )
  
  print(p)
  return(p)
}


# --------------------------------------------------------------------------------------------
# Function: plot_hourly_avg_pollution
# @Arg         : df is a data frame with columns "Date", "Hour", "pm25_merra2" 
#                and "pm25_stations".
# @Arg         : region_name is a string representing the region or city name.
# @Arg         : plot_ci is a logical indicating whether to add error bars (standard error).
# @Arg         : bar_width is a numeric value for the width of the bars (default is 0.7).
# @Arg         : color_merra2_main is a string specifying the main color for MERRA-2 bars.
# @Arg         : color_stations_main is a string specifying the main color for stations bars.
# @Arg         : color_merra2_error is a string specifying the color for MERRA-2 error bars.
# @Arg         : color_stations_error is a string specifying the color for station error bars.
# @Output      : A ggplot object showing the average hourly PM2.5 (from MERRA-2 and stations),
#                with optional error bars in matching/darker tones, and with dashed vertical 
#                lines indicating WHO Interim Targets: IT2 (25 µg/m³, orange) and IT1 (35 µg/m³,
#                dark red). This is the IT1 and IT2 values for annual averages.
# @Purpose     : To visualize and compare the hourly persistence of PM2.5 pollution, 
#                facilitating an understanding of differences among cities/hours.
#                The IT dashed lines help highlight when pollutant concentrations
#                exceed WHO targets.
# @Written_on  : 28/02/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_hourly_avg_pollution <- function(df, 
                                      region_name, 
                                      plot_ci             = FALSE, 
                                      bar_width           = 0.7,
                                      color_merra2_main   = "#cc9900",
                                      color_stations_main = "#009999",
                                      color_merra2_error  = "#440154FF",
                                      color_stations_error= "#440154FF") {
  # ---
  # Summarize data by Hour - group by Hour and then compute:
  # - Mean and standard deviation (for both MERRA-2 and station data)
  # - Count of non-missing observations (n)
  # - Standard error (SE) as sd/sqrt(n)
  # ---
  summary_df <- df %>%
    group_by(Hour) %>%
    summarise(
      mean_MERRA2   = mean(pm25_merra2, na.rm = TRUE),
      sd_MERRA2     = sd(pm25_merra2, na.rm = TRUE),
      n_MERRA2      = sum(!is.na(pm25_merra2)),
      mean_Stations = mean(pm25_stations, na.rm = TRUE),
      sd_Stations   = sd(pm25_stations, na.rm = TRUE),
      n_Stations    = sum(!is.na(pm25_stations))
    ) %>%
    mutate(
      se_MERRA2   = sd_MERRA2 / sqrt(n_MERRA2),
      se_Stations = sd_Stations / sqrt(n_Stations)
    ) %>%
    ungroup() %>%
    # Reshape data from wide to long format for plotting
    pivot_longer(
      cols      = starts_with("mean"),
      names_to  = "series",
      values_to = "mean_value",
      names_prefix = "mean_"
    ) %>%
    # Map the corresponding standard error for each series
    mutate(
      se = ifelse(series == "MERRA2", se_MERRA2, se_Stations)
    )
  
  # ---
  # Build the Plot
  # ---
  # We'll define color mapping for bar fills:
  fill_values <- c(
    "MERRA2"   = color_merra2_main,
    "Stations" = color_stations_main
  )
  # For error bars, we need a color scale as well:
  error_colors <- c(
    "MERRA2"   = color_merra2_error,
    "Stations" = color_stations_error
  )
  
  # Create a horizontal bar plot
  p <- ggplot(summary_df, aes(x = mean_value, y = as.factor(Hour), fill = series)) +
    geom_bar(stat = "identity",
             position = position_dodge(width = bar_width),
             width    = bar_width,
             color    = "black") +
    scale_fill_manual(values = fill_values) +
    labs(
      title = paste("Average Hourly PM2.5 in", region_name, "for 2023"),
      x     = "Average PM2.5 (µg/m³)",
      y     = "Hour of Day",
      fill  = "Data Source"
    ) +
    theme_set(theme_minimal(base_family = "Palatino", base_size = 14))
  
  # If error bars (CI) are requested, add them in a matching/darker color
  if (plot_ci) {
    p <- p + geom_errorbar(
      aes(xmin = mean_value - se, xmax = mean_value + se, color = series),
      position = position_dodge(width = bar_width),
      width    = 0.2
    ) +
      scale_color_manual(values = error_colors) +
      guides(color = "none")  # Hide separate legend for error bars
  }
  
  # Add Interim Target Lines (IT2 and IT1)
  p <- p +
    geom_vline(xintercept = 25, color = "orange", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = 35, color = "darkred", linetype = "dashed", linewidth = 0.5) +
    annotate("text", x = 26, y = 23, label = "IT2", vjust = -0.5,
             color = "orange", size = 3) +
    annotate("text", x = 36, y = 23, label = "IT1", vjust = -0.5,
             color = "darkred", size = 3)
  
  print(p)
  return(p)
}


# --------------------------------------------------------------------------------------------
# Function: plot_hourly_ridgeline_pollution
# @Arg         : df is a data frame with columns "Date", "Hour", and PM2.5 data
#                (either "pm25_merra2" or "pm25_stations").
# @Arg         : region_name is a string representing the region or city name.
# @Arg         : pollution_var is a string specifying which column of PM2.5 data to visualize.
# @Output      : A ggplot object showing the distribution of PM2.5 across 24 hours in ridgeline 
#                form, with dashed vertical lines indicating WHO Interim Targets: IT2 (50 µg/m³, 
#                orange) and IT1 (75 µg/m³, dark red). IT1 and IT2 are the 24 hours average.
# @Purpose     : To visualize the distribution (rather than just the mean) of pollutants by 
#                hour of day, helping to spot patterns in how pollution accumulates over time.
# @Written_on  : 28/02/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_hourly_ridgeline_pollution <- function(df, 
                                            region_name,
                                            pollution_var = "pm25_merra2") {

  # Filter out rows with missing values in the chosen pollution column
  df <- df %>%
    filter(!is.na(.data[[pollution_var]]))
  
  # Compute maximum hour value (as numeric) to position annotations above the top ridge
  max_hour <- max(as.numeric(as.character(unique(df$Hour))))
  
  # Build ridgeline plot
  p <- ggplot(df, aes(x = !!rlang::sym(pollution_var), y = as.factor(Hour))) +
    geom_density_ridges_gradient(
      aes(fill = after_stat(x)),
      scale = 8.5,
      rel_min_height = 0.000001, 
      color = "black",
      alpha = 0.3
    ) +
    scale_fill_viridis_c(
      option = "viridis",                # or "magma", "inferno", "viridis", etc.
      name   = "PM2.5 (µg/m³)",         # legend title
      # breaks = c(0, 25, 50, 75, 100),   # where the ticks will appear
      # labels = c("0", "25", "50", "75", "100"),
      guide  = guide_colorbar(
        barheight   = unit(5, "cm"),    # increase the height for a smoother gradient
        barwidth    = unit(0.8, "cm"),  # narrower or wider as you prefer
        frame.colour = "black",         # add a frame around the color bar
        ticks.colour = "black")         # color of the tick marks
      ) +
    labs(
      title = paste("Hourly PM2.5 Distribution in", region_name, "using", pollution_var),
      x     = "PM2.5 (µg/m³)",
      y     = "Hour of Day"
    ) +
    theme_set(theme_minimal(base_family = "Palatino", base_size = 14))
  
  # Add dashed vertical lines for Interim Targets and annotations
  p <- p +
    geom_vline(xintercept = 50, color = "orange", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = 75, color = "darkred", linetype = "dashed", linewidth = 0.5) +
    annotate("text", x = 53.5, y = max_hour + 2.5, label = "IT2", vjust = -0.5, 
             color = "orange", size = 3) +
    annotate("text", x = 78.5, y = max_hour + 2.5, label = "IT1", vjust = -.5, 
             color = "darkred", size = 3)
  
  print(p)
  return(p)
}


# --------------------------------------------------------------------------------------------
# Function: compute_time_spans_above_target
# @Arg         : df is a data frame with columns "Date", "Hour", and at least one
#                PM2.5 column (e.g., "pm25_stations" or "pm25_merra2").
# @Arg         : city_name is a string identifying the city/region (e.g., "Bogotá").
# @Arg         : target is a string, either "IT1" or "IT2". 
#                "IT1" => threshold = 75 µg/m³
#                "IT2" => threshold = 50 µg/m³
# @Arg         : pollution_var is a string specifying which PM2.5 column to check.
# @Output      : A data frame listing each episode above the chosen threshold, 
#                with columns: Date, Hour, time_span_above_target, city.
# @Purpose     : To identify consecutive hours where PM2.5 is >= the chosen WHO interim target
#                (IT1 or IT2), facilitating further analysis and plotting.
# @Written_on  : 10/03/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_time_spans_above_target <- function(df, 
                                            city_name, 
                                            target = c("IT1", "IT2"), 
                                            pollution_var = "pm25_stations") {
  
  # 1) Determine threshold based on the chosen WHO Interim Target
  threshold <- if (target == "IT1") {
    75
  } else if (target == "IT2") {
    50
  } else {
    stop("Invalid target. Use 'IT1' or 'IT2'.")
  }
  
  # 2) Create a datetime column and ensure data is in chronological order
  df <- df %>%
    mutate(
      Datetime = as.POSIXct(
        paste(Date, sprintf("%02d:00:00", Hour)), 
        format = "%Y-%m-%d %H:%M:%S", 
        tz = "UTC"
      )
    ) %>%
    arrange(Datetime)
  
  # 3) Iterate through rows to find consecutive hours above threshold
  result_list <- list()
  
  is_above <- FALSE
  start_idx <- NA
  count_hrs <- 0
  
  # Main loop of the function - must important part of calculation
  for (i in seq_len(nrow(df))) {
    current_value <- as.numeric(df[[pollution_var]][i])
    
    # Check if current_value is NA and handle it
    if (is.na(current_value)) {
      if (is_above) {
        # End the current above-target episode if one is ongoing
        result_list[[length(result_list) + 1]] <- data.frame(
          Date                    = df$Date[start_idx],
          Hour                    = df$Hour[start_idx],
          time_span_above_target = count_hrs,
          stringsAsFactors        = FALSE
        )
        is_above <- FALSE
        start_idx <- NA
        count_hrs <- 0
      }
      # Skip this iteration since there's no valid value
      next
    } else if (current_value >= threshold) {
      # If current value is above the threshold
      if (!is_above) {
        is_above <- TRUE
        start_idx <- i
        count_hrs <- 1
      } else {
        count_hrs <- count_hrs + 1
      }
    } else {
      # current_value < threshold
      if (is_above) {
        # End the current above-target episode
        result_list[[length(result_list) + 1]] <- data.frame(
          Date                    = df$Date[start_idx],
          Hour                    = df$Hour[start_idx],
          time_span_above_target = count_hrs,
          stringsAsFactors        = FALSE
        )
        is_above <- FALSE
        start_idx <- NA
        count_hrs <- 0
      }
    }
  }
  
  # 4) Check if the last row ended while still above threshold
  if (is_above) {
    result_list[[length(result_list) + 1]] <- data.frame(
      Date                    = df$Date[start_idx],
      Hour                    = df$Hour[start_idx],
      time_span_above_target = count_hrs,
      city                    = city_name,
      stringsAsFactors        = FALSE
    )
  }
  
  # 5) Combine all episodes into a single data frame
  if (length(result_list) > 0) {
    final_df <- do.call(rbind, result_list)
  } else {
    # If no episodes found, return empty data frame with same structure
    final_df <- data.frame(
      Date                    = character(),
      Hour                    = integer(),
      time_span_above_target = integer(),
      city                    = character(),
      stringsAsFactors        = FALSE
    )
  }
  
  return(final_df)
}


# --------------------------------------------------------------------------------------------
# Function: plot_time_spans_ridgeline
# @Arg         : list_of_dfs is a named list of data frames (e.g., 
#                list("Bogotá" = bogota_pm25, "Santiago" = santiago_pm25, ...)).
# @Arg         : target is a string, either "IT1" or "IT2".
# @Arg         : pollution_var is a string specifying which PM2.5 column to check.
# @Output      : A ggplot object showing the distribution of consecutive hours 
#                above the chosen WHO Interim Target, faceted by city on the y-axis.
# @Purpose     : To reveal how often and for how long each city experiences 
#                pollution levels above a WHO interim target, using a ridgeline plot.
# @Written_on  : 06/03/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_time_spans_ridgeline <- function(list_of_dfs, 
                                      target = c("IT1", "IT2"),
                                      pollution_var = "pm25_stations") {

  # 1) Combine time-span episodes for each city
  #    list_of_dfs should be named: e.g., list("Bogotá" = bogota_pm25, ...)
  all_episodes <- list()
  
  for (city_name in names(list_of_dfs)) {
    city_df <- list_of_dfs[[city_name]]
    
    # Compute episodes for this city
    city_episodes <- compute_time_spans_above_target(
      df            = city_df,
      city_name     = city_name,
      target        = target,
      pollution_var = pollution_var
    )
    
    all_episodes[[city_name]] <- city_episodes
  }
  
  # 2) Bind all city episodes into a single data frame
  final_episodes <- bind_rows(all_episodes, .id = "city")
  
  # 3) Build a ridgeline plot: x = time_span_above_target, y = city
  p <- ggplot(final_episodes, aes(x = time_span_above_target, y = city)) +
    geom_density_ridges_gradient(
      aes(fill = after_stat(x)),
      scale          = 1.5,
      rel_min_height = 0.01,
      color          = "black",
      alpha          = 0.8
    ) +
    scale_fill_viridis_c(
      option = "viridis",
      name   = "Consecutive Hours\nAbove Target",
      guide  = guide_colorbar(
        barheight   = unit(5, "cm"),
        barwidth    = unit(0.8, "cm"),
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    labs(
      title = paste("Distribution of Consecutive Hours Above", target),
      x     = "Consecutive Hours Above Target",
      y     = "City"
    ) +
    theme_minimal(base_family = "Palatino", base_size = 14)
  
  
  return(p)
}

# ------------------------------------------------------------------------------------------------
# Function: summarize_hourly_by_station
# @Arg         : df           a data frame with columns for station code, datetime, and value.
# @Arg         : station_col  name of the station code column (default "station_code").
# @Arg         : datetime_col name of the datetime column (default "date2_hour").
# @Arg         : value_col    name of the pollutant column to average (default "pm25_validated").
# @Arg         : filter_type  one of "none", "gt_it1", or "gt_it2" for threshold filtering.
# @Arg         : it1, it2     numeric thresholds (defaults reflect WHO annual PM2.5 IT1/IT2).
# @Arg         : tz           timezone for parsing if needed (kept for signature parity).
# @Arg         : station_lookup data.frame with columns Station (char) and StationName (char).
# @Output      : A data frame with columns Station, Hour, mean_value, n and an attribute
#                "station_levels" giving a fixed station order across hours.
# @Purpose     : Build hourly means per station, optionally filtering by WHO interim targets,
#                and attach a stable station ordering for consistent stacked plots.
# @Written_on  : 12/08/2025
# @Written_by  : Marcos Paulo
# ------------------------------------------------------------------------------------------------
summarize_hourly_by_station <- function(df,
                                        station_col    = "station_code",
                                        datetime_col   = "date2_hour",
                                        value_col      = "pm25_validated",
                                        filter_type    = c("none", "gt_it1", "gt_it2"),
                                        it1            = 35,
                                        it2            = 25,
                                        tz             = "UTC",
                                        station_lookup = SANTIAGO_STATION_LOOKUP) {
  filter_type <- match.arg(filter_type)
  
  # 1) Parse time + prepare core fields
  df2 <- df %>%
    mutate(
      .dt     = as.POSIXct(.data[[datetime_col]]),   # keep your original choice (no tz)
      Date    = as.Date(.dt),
      Hour    = lubridate::hour(.dt),
      .val    = as.numeric(.data[[value_col]]),
      Station = as.character(.data[[station_col]])
    )
  
  # 2) Apply IT filters
  df2 <- switch(
    filter_type,
    "none"   = df2,
    "gt_it1" = dplyr::filter(df2, .val > it1),
    "gt_it2" = dplyr::filter(df2, .val > it2)
  )
  
  # 3) Join station names; fall back to "Station <code>" when missing
  df2 <- df2 %>%
    dplyr::left_join(
      station_lookup %>% dplyr::mutate(Station = as.character(Station)),
      by = "Station"
    ) %>%
    dplyr::mutate(
      Station = dplyr::coalesce(StationName, paste0("Station ", Station))
    )
  
  # 4) Compute hourly means per (station, hour)
  hourly <- df2 %>%
    dplyr::group_by(Station, Hour) %>%
    dplyr::summarise(
      mean_value = mean(.val, na.rm = TRUE),
      n          = sum(!is.na(.val)),
      .groups    = "drop"
    )
  
  # 5) Fix a single station order across hours (descending overall mean)
  station_levels <- hourly %>%
    dplyr::group_by(Station) %>%
    dplyr::summarise(overall_mean = mean(mean_value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(overall_mean)) %>%
    dplyr::pull(Station)
  
  hourly$Station <- factor(hourly$Station, levels = station_levels)
  attr(hourly, "station_levels") <- station_levels
  hourly
}

# ------------------------------------------------------------------------------------------------
# Function: plot_hourly_stacked_stations
# @Arg         : hourly_df      output of summarize_hourly_by_station().
# @Arg         : region_name    string for the plot title (e.g., "Santiago").
# @Arg         : pollutant_label label to show (e.g., "PM2.5" or "PM10").
# @Arg         : filter_label   subtitle (e.g., "All values", "Values > IT1 (35 µg/m³)").
# @Arg         : normalize      if TRUE, 100% stacks by hour (shares); else stacks absolute means.
# @Arg         : show_it_lines  if TRUE and not normalized, add IT2/IT1 vertical lines.
# @Arg         : year           integer shown in title (default 2012L for signature parity).
# @Arg         : it1, it2       numeric WHO lines if show_it_lines = TRUE.
# @Arg         : base_family    font family for theme.
# @Output      : A ggplot object (horizontal stacked bars; one bar per hour; segment = station).
# @Purpose     : Visualize composition and level (or share) of hourly averages by station with a
#                fixed station order across all hours to ease comparisons.
# @Written_on  : 12/08/2025
# @Written_by  : Marcos Paulo
# ------------------------------------------------------------------------------------------------
plot_hourly_stacked_stations <- function(hourly_df,
                                         region_name     = "Santiago",
                                         pollutant_label = "PM2.5",
                                         filter_label    = "All values",
                                         normalize       = FALSE,
                                         show_it_lines   = FALSE,
                                         year            = 2012L,
                                         it1             = 35,
                                         it2             = 25,
                                         base_family     = "Palatino") {
  
  dfp <- hourly_df
  
  # 1) Optionally convert to shares within hour (100% stacks)
  if (normalize) {
    dfp <- dfp %>%
      dplyr::group_by(Hour) %>%
      dplyr::mutate(
        total_hour = sum(mean_value, na.rm = TRUE),
        share      = dplyr::if_else(total_hour > 0, mean_value / total_hour, NA_real_)
      ) %>%
      dplyr::ungroup()
  }
  
  # 2) Choose palette length from the fixed station order
  n_stations <- length(attr(dfp, "station_levels") %||% levels(dfp$Station))
  pal        <- viridisLite::viridis(n_stations, option = "D", direction = 1)
  
  # 3) Build stacked horizontal bars (order = factor levels, fixed across hours)
  p <- ggplot(
    dfp,
    aes(
      x   = if (normalize) share else mean_value,
      y   = factor(Hour),
      fill = Station
    )
  ) +
    geom_bar(stat = "identity", width = 0.7, color = "black") +
    scale_fill_manual(values = pal, drop = FALSE) +
    labs(
      title    = paste0("Hourly ", pollutant_label, " by Station — ", region_name, " (", year, ")"),
      subtitle = filter_label,
      x        = if (normalize) "Share of hourly mean (100% stacked)"
      else paste0("Average ", pollutant_label, " (µg/m³)"),
      y        = "Hour of Day",
      fill     = "Station"
    ) +
    theme_minimal(base_family = base_family, base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position    = "right"
    )
  
  # 4) Optional WHO lines (only meaningful on absolute scale)
  if (show_it_lines && !normalize) {
    p <- p +
      geom_vline(xintercept = it2, color = "orange",  linetype = "dashed", linewidth = 0.5) +
      geom_vline(xintercept = it1, color = "darkred", linetype = "dashed", linewidth = 0.5) +
      annotate("text", x = it2 + 1, y = max(as.numeric(factor(dfp$Hour))), label = "IT2",
               vjust = -0.4, color = "orange",  size = 3) +
      annotate("text", x = it1 + 1, y = max(as.numeric(factor(dfp$Hour))), label = "IT1",
               vjust = -0.4, color = "darkred", size = 3)
  }
  
  return(p)
}

# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")
