# ============================================================================================
# IDB: Air monitoring — maps
# ============================================================================================
#' @Goal: Functions for maps.
#
#' @Description: Static and interactive maps: national context, metro areas by education quintile, MERRA-2
#   grids, and the Latin America locator.
#   Sourced by config_utils_plot_tables.R; never sourced directly by a script.
#
#' @Summary:
#   1. .stadia_tile
#   2. plot_metro_area_national_context
#   3. plot_metro_area_interactive
#   4. plot_merra2_grid_city
#   5. plot_variable_across_cities
#   6. plot_latin_america_map
#   7. plot_inequality_pollution
#
#' @Date: August 2026
#' @Author: Marcos Paulo
# ============================================================================================

# The theme is NOT set here: figure scripts call set_paper_theme() themselves, so a script
# that draws nothing does not silently inherit a global font and theme.

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


# --------------------------------------------------------------------------------------------
# Function: plot_metro_area_national_context
#' @param national_states_sf  sf MULTIPOLYGON of country states (any CRS)
#' @param metro_area_sf       sf (MULTI)POLYGON for the metro area (any CRS)
#' @param which_states        chr vec; state names to highlight (must match `state_name_col`)
#' @param state_name_col      column in `national_states_sf` with state names (default "name")
#' @param map_mode            'ggmap' (tiles) | 'sf' (no tiles). If 'ggmap' but the
#                            Stadia Maps key is missing, it will fall back to 'sf'.
#' @param basemap_zoom        numeric zoom for ggmap::get_stadiamap (default 5)
#' @param basemap_type        one of possible options on the ggmap. ('stamen_terrain',
# 'stamen_toner', 'stamen_toner_lite'...)
#' @param city_name           character; used in title (e.g., "Mexico City")
#' @param states_border_col   color for all state borders (default "grey20")
#' @param states_border_lwd   linewidth for borders (default 0.4)
#' @param highlight_fill      fill for highlighted states (default "#F59E0B")
#' @param highlight_alpha     alpha for highlighted states (default 0.20)
#' @param highlight_border    border color for highlighted states (default "#B45309")
#' @param metro_fill          fill for metro polygon (default "#1D4ED8")
#' @param metro_alpha         alpha for metro polygon (default 0.30)
#' @param metro_border        border color for metro polygon (default "#1E3A8A")
#' @param add_graticule       logical; add light graticule lines (default TRUE)
#' @param stadiamaps_envkey   env var name with Stadia key (default "STADIA_MAPS_KEY")
#' @return  ggplot object
#' @Purpose: Country map with optional raster tiles, borders, highlighted states,
#           and metro overlay. Legend shows what colors mean and metro area (km²).
#' @Notes  : Requires packages: sf, ggplot2. For 'ggmap' mode: ggmap + stadiamaps key.
#' @Written_on: 28/09/2025
#' @Written_by: Marcos Paulo
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
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "Palatino", base_size = 14)
  )
  
  map_mode <- match.arg(map_mode)
  
  # ---- 1) prepare data (CRS → 4326 for tiles; compute metro area) -----------
  states84 <- sf::st_transform(national_states_sf, 4326)
  metro84  <- sf::st_transform(metro_area_sf, 4326)
  
  # pick a UTM CRS based on bbox midpoint (no centroid needed)
  crs_area <- utm_epsg(metro84)
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
#' @param metro_area_sf    sf (MULTI)POLYGON of the metro area (any CRS)
#' @param stations_sf      sf POINTS of stations; must include columns:
#                          • code (station_code), • station (name),
#                          • entity (state),      • altitude_m (meters)
#' @param pollution_ds     OPTIONAL Arrow Dataset (or dplyr tbl) with columns:
#                          station_code, year, pm10, `pm2.5`  (default NULL)
#' @param legacy_df        OPTIONAL tibble with columns station, year
#                          (used only for color_scheme = "legacy2023")
#' @param filter_type      one of:
#                          "none"               : no filter
#                          "has_pm_any"         : any non-NA in pm10 OR pm2.5 (any year)
#                          "has_pm_in_year"     : non-NA in selected pollutant & year
#                          "has_both_in_year"   : non-NA in BOTH pm10 & pm2.5 for year
#' @param filter_year      integer year used by *_in_year filters (default 2023)
#' @param pollutant        "none", "pm25", or "pm10" (used only by has_pm_in_year)
#' @param color_scheme     "entity" or "legacy2023"
#                          • entity     : color by stations_sf$entity
#                          • legacy2023 : color by presence in legacy_df (year==2023)
#' @param buffer_km        numeric; radius for outside-station buffers (default 20)
#' @param city_name        character; used in the map title/control
#' @param stadiamaps_key   character; your Stadia Maps API key. If empty, function
#                          falls back to CartoDB Positron tiles (no key needed).
#                          A convenient pattern is to pass:
#                          Sys.getenv("STADIA_MAPS_API_KEY", unset = "")
#' @param tileset          Stadia tileset id (e.g. "stamen_terrain_background",
#                          "stamen_toner", "stamen_watercolor").
#' @return  leaflet htmlwidget (interactive map)
#' @Purpose : Interactive metro map with basemap, metro polygon, station points,
#            optional 20-km buffers for stations outside the metro polygon,
#            optional filters from a parquet/Arrow dataset, and info-rich tooltips.
#            The corner box shows the city + number of stations; legend labels
#            include per-category counts.
#' @Written_on: 30/09/2025
#' @Written_by: Marcos Paulo
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
  epsg_loc   <- utm_epsg(metro_84)
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
  # Builds a category per station, its counts, a palette over the unique categories,
  # and the legend labels carrying those counts.
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
#' @param      shapefile is an 'sf' object representing the city boundary
#' @param      nc_file is a string containing the path to a single .nc4 file
#              from the MERRA-2 dataset
#' @param      city_name is a string with the name of the city
#' @return     A ggplot object representing the map of the city boundary and
#              MERRA-2 grid cells
#' @Purpose   : Creates a spatial plot showing the city's boundary and the overlayed
#              MERRA-2 grid cells from the specified nc_file. This visualization helps 
#              in understanding the spatial extent of the MERRA-2 data relative to 
#              the city's area.
#' @Written_on: 10/12/2024
#' @Written_by: Marcos Paulo
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
#' @param      df_list is a named list of dataframes, where each dataframe contains
#              aerosol concentration and PM 2.5 data.
#' @param      variable is a string specifying the variable to plot (e.g., "DUSMASS25",
#              "OCSMASS", "pm25_estimate").
#' @param      var_label is a string specifying the label for the x-axis and plot title.
#              Defaults to the variable name if not provided.
#' @param      max_x_limit is a number representing the right limit for the x axis
#' @return     A single density plot comparing the variable across cities.
#' @Purpose   : Generate a density plot for a specific aerosol or PM 2.5 concentration
#              across multiple cities, including WHO PM 2.5 guidelines if applicable.
#' @Written_on: 13/12/2024
#' @Written_by: Marcos Paulo
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
#' @param latin_america        An 'sf' object representing Latin America map.
#' @param regions              A list of 'sf' objects for metropolitan areas
#                              (e.g., Bogota, ciudad_mexico, etc.).
#' @param region_names         A vector of city names corresponding to 'regions'.
#' @param outline              Logical; if TRUE, regions will be outlined.
#' @return     A high-quality map with scale bar, compass, and customized aesthetics.
#' @Purpose   : Produce a publication-ready map highlighting metropolitan regions 
#              over Latin America with optional outlines.
#' @Written_on: 15/12/2024
#' @Written_by: Marcos Paulo
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


# ---------------------------------------------------------------------------------------------
# Function: plot_inequality_pollution
#
#' @param metro_sf   sf object; The metropolitan area boundaries (e.g. tracts).
#' @param stations_sf sf object; The ground monitoring stations.
#' @param arrow_dir  string; Path to the partitioned parquet dataset folder.
#' @param census_df  data.frame; The collapsed census statistics.
#' @param join_sf_col string; Column name in metro_sf to join on.
#' @param join_df_col string; Column name in census_df to join on.
#' @param station_col string; Column in stations_sf with the station name/code.
#' @param ed_col     string; Column for education/sorting (e.g. "escolaridad").
#' @param pop_col    string; Column for population weights (e.g. "n").
#' @param year_filter numeric; Year to check for active stations.
#' @param buffer_km  numeric; Buffer size around stations in kilometers.
#' @param city_label string; Text label to place in the map.
#' @param pollutants vector; Pollutants to check (e.g., c("pm25", "pm10")).
#' @param label_x_pct numeric; X position of annotations (0 to 1, default 0.98).
#' @param label_y_pct numeric; Y position of annotations (0 to 1, default 0.98).
#' @param legend_pos vector; Relative X/Y pos of legend (default top-right).
#
#' @return           A ggplot object.
#' @Purpose         : Visualize inequality based on education levels and active 
#                    air monitoring station buffers. Uses a Sequential Cascading
#                    Join to seamlessly fill un-surveyed blocks with the median 
#                    data of their geographic parent sections.
#' @Written_on      : 10/01/2026 (Updated 04/03/2026)
#' @Written_by      : Marcos Paulo
# ---------------------------------------------------------------------------------------------
plot_inequality_pollution <- function(
    metro_sf,
    stations_sf,
    arrow_dir,
    census_df,
    join_sf_col,
    join_df_col,
    station_col = "station_name",
    ed_col,
    pop_col,
    year_filter = 2023,
    buffer_km = 5,
    city_label = "Metro Area",
    pollutants = c("pm25", "pm10"),
    label_x_pct = 0.98,               # <-- New control for X annotation
    label_y_pct = 0.98,               # <-- New control for Y annotation
    legend_pos = c(0.85, 0.85)        # <-- New control for legend position
) {
  
  pkgs <- c("dplyr", "sf", "ggplot2", "viridis", "arrow", "data.table", 
            "stringr", "stringi")
  # 0. Helper function: Normalize strings for joining
  
  # 1. Query Arrow for Active Stations
  # ---------------------------------------------------------------------------
  arrow_ds <- arrow::open_dataset(arrow_dir)
  
  active_stations <- arrow_ds |>
    dplyr::filter(year == year_filter) |>
    dplyr::select(station, dplyr::all_of(pollutants)) |>
    dplyr::collect() |>
    dplyr::filter(
      rowSums(!is.na(dplyr::across(dplyr::all_of(pollutants)))) > 0
    ) |>
    dplyr::distinct(station) |>
    dplyr::pull(station)
  
  norm_active <- normalize_key(active_stations)
  stations_sf$norm_name <- normalize_key(stations_sf[[station_col]])
  
  stations_subset <- stations_sf |> 
    dplyr::filter(norm_name %in% norm_active)
  
  if (nrow(stations_subset) == 0) {
    warning("No active stations found for the given year and pollutants.")
  }
  
  # 2. Compute Population-Weighted Quintiles
  # ---------------------------------------------------------------------------
  data.table::setDT(census_df)
  
  census_clean <- census_df[!is.na(get(ed_col)) & !is.na(get(pop_col))]
  data.table::setorderv(census_clean, cols = ed_col)
  
  census_clean[, cum_pop := cumsum(get(pop_col))]
  census_clean[, pct_pop := cum_pop / sum(get(pop_col), na.rm = TRUE)]
  census_clean[, quintiles := data.table::fcase(
    pct_pop <= 0.2, "1",
    pct_pop <= 0.4, "2",
    pct_pop <= 0.6, "3",
    pct_pop <= 0.8, "4",
    default = "5"
  )]
  
  # 3. Spatial Joins & Sequential Cascading Fallback
  # ---------------------------------------------------------------------------
  census_clean <- as.data.frame(census_clean)
  
  census_clean$k_base <- as.character(census_clean[[join_df_col]])
  sf_keys <- as.character(metro_sf[[join_sf_col]])
  
  # A. Prepare Exact Matches
  census_exact <- census_clean %>%
    dplyr::select(k_exact = k_base, q_exact = quintiles) %>%
    dplyr::distinct(k_exact, .keep_all = TRUE)
  
  # B. Build Median Data for Parent Geographic Levels
  build_tree <- function(len, col_name) {
    census_clean %>%
      dplyr::filter(nchar(k_base) >= len) %>%
      dplyr::mutate(key = stringr::str_sub(k_base, 1, len)) %>%
      dplyr::group_by(key) %>%
      dplyr::summarise(
        !!col_name := as.character(
          round(median(as.numeric(quintiles), na.rm = TRUE))
        ),
        .groups = "drop"
      )
  }
  
  tree20 <- build_tree(20, "q20") # Seccion Urbana
  tree17 <- build_tree(17, "q17") # Sector Urbano
  tree14 <- build_tree(14, "q14") # Centro Poblado
  tree11 <- build_tree(11, "q11") # Seccion Rural
  tree9  <- build_tree(9, "q9")   # Sector Rural
  
  # C. Perform Sequential Cascading Joins
  shp_merged <- metro_sf %>%
    dplyr::mutate(
      k_exact = sf_keys,
      k20 = stringr::str_sub(k_exact, 1, 20),
      k17 = stringr::str_sub(k_exact, 1, 17),
      k14 = stringr::str_sub(k_exact, 1, 14),
      k11 = stringr::str_sub(k_exact, 1, 11),
      k9  = stringr::str_sub(k_exact, 1, 9)
    ) %>%
    dplyr::left_join(census_exact, by = "k_exact") %>%
    dplyr::left_join(tree20, by = c("k20" = "key")) %>%
    dplyr::left_join(tree17, by = c("k17" = "key")) %>%
    dplyr::left_join(tree14, by = c("k14" = "key")) %>%
    dplyr::left_join(tree11, by = c("k11" = "key")) %>%
    dplyr::left_join(tree9,  by = c("k9" = "key")) %>%
    dplyr::mutate(
      final_join_key = dplyr::coalesce(
        ifelse(!is.na(q_exact), k_exact, NA_character_),
        ifelse(!is.na(q20), k20, NA_character_),
        ifelse(!is.na(q17), k17, NA_character_),
        ifelse(!is.na(q14), k14, NA_character_),
        ifelse(!is.na(q11), k11, NA_character_),
        ifelse(!is.na(q9),  k9, NA_character_)
      ),
      quintiles = dplyr::coalesce(q_exact, q20, q17, q14, q11, q9)
    )
  
  # Explicitly align the stations' CRS
  stations_subset <- sf::st_transform(stations_subset, sf::st_crs(shp_merged))
  stations_buffer <- sf::st_buffer(stations_subset, dist = buffer_km * 1000)
  
  # 4. Calculate Dropped Population
  # ---------------------------------------------------------------------------
  used_keys <- unique(shp_merged$final_join_key[!is.na(shp_merged$quintiles)])
  
  census_eval <- census_clean %>%
    dplyr::mutate(
      is_mapped = (
        k_base %in% used_keys | 
          stringr::str_sub(k_base, 1, 20) %in% used_keys |
          stringr::str_sub(k_base, 1, 17) %in% used_keys |
          stringr::str_sub(k_base, 1, 14) %in% used_keys |
          stringr::str_sub(k_base, 1, 11) %in% used_keys |
          stringr::str_sub(k_base, 1, 9) %in% used_keys
      )
    )
  
  tot_pop <- sum(census_eval[[pop_col]], na.rm = TRUE)
  drp_pop <- sum(census_eval[[pop_col]][!census_eval$is_mapped], na.rm = TRUE)
  pct_drp <- round(100 * (drp_pop / tot_pop), 2)
  
  drop_label <- paste0("Dropped Pop: ", pct_drp, "%")
  
  # 5. Extract Coordinates for Dynamic Labels
  # ---------------------------------------------------------------------------
  bbox <- sf::st_bbox(shp_merged)
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  
  # Dynamic placement based on user percentages (default is top-right)
  label_x <- bbox["xmin"] + (x_range * label_x_pct)
  label_y <- bbox["ymin"] + (y_range * label_y_pct)
  drop_y  <- label_y - (y_range * 0.03) # Shift dropped text exactly 3% below
  
  # 6. Build ggplot
  # ---------------------------------------------------------------------------
  geom_color <- if (nrow(shp_merged) > 1000) NA else "grey50"
  
  p <- ggplot() +
    geom_sf(
      data = shp_merged, aes(fill = quintiles), 
      color = geom_color, linewidth = 0.25
    ) +
    scale_fill_viridis_d(option="mako", direction=-1, na.value="grey90") +
    geom_sf(data = stations_subset, color = "red", size = 1) +
    geom_sf(
      data = stations_buffer, fill = NA, color = "red", 
      linewidth = 0.5, alpha = 0.3
    ) +
    annotate(
      "text", x = label_x, y = label_y, label = city_label, 
      hjust = 1, vjust = 1, family = "Palatino", fontface = "bold", 
      size = 5, color = "grey20"
    ) +
    annotate(
      "text", x = label_x, y = drop_y, label = drop_label, 
      hjust = 1, vjust = 1, family = "Palatino", fontface = "italic", 
      size = 3.5, color = "grey40"
    ) +
    labs(fill = "Years of schooling\nquintiles") +
    theme_minimal(base_family = "Palatino", base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      
      # Floating Legend Overhaul
      legend.position   = legend_pos,
      legend.title      = element_text(size = 9, face = "bold"),
      legend.text       = element_text(size = 9),
      legend.key.size   = unit(0.5, "cm"),
      legend.background = element_rect(fill = alpha("white", 0.7), color = NA)
    )
  
  print(p)
  return(p)
}
