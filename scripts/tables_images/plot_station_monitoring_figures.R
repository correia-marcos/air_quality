# ======================================================================================
# IDB: Air monitoring
# ======================================================================================
# @Goal:
#   Generate station-monitoring figures for the paper.
#
# @Description:
#   This script uses processed distance matrices and station-level socioeconomic
#   exposure data to create:
#   1. Station coverage and distance plots by education.
#   2. Station-level average pollution versus education plots.
#   3. Station-level hours above WHO thresholds versus education plots.
#
# @Date: June 2026
# @Author: Marcos
# ======================================================================================


# ======================================================================================
# I. Setup
# ======================================================================================

# Source project functions, if needed for paths/configs.
source(here::here("src", "general_utilities", "config_utils_process_data.R"))

# Required packages for the plotting script.
req_pkgs <- c("arrow", "data.table", "ggplot2", "scales", "stringi", "here")

# Check package availability before running.
for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("Package missing: ", p)
  }
}

# Optional package for cleaner station labels.
has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)

# Define input folders.
dir_distances <- here::here("data", "processed", "distances_matrices")
dir_station   <- here::here("data", "processed", "station_socio_exposure")
dir_census    <- here::here("data", "interim", "census")

# Define output folder.
outdir_fig <- here::here("results", "figures", "station_monitoring")
dir.create(outdir_fig, recursive = TRUE, showWarnings = FALSE)

# Define common visual style.
plot_font_family <- "Palatino"
color_nearest_distance <- "darkblue"
color_station_count <- "darkred"
plot_pollutant_colors <- c("PM2.5" = "darkred", "PM10" = "black")
plot_width <- 8.5
plot_height <- 5.8
plot_dpi <- 300



# Define distance matrix paths.
dist_bogota <- here::here(
  dir_distances, "bogota_2018", "matrix_geo_station_distances.parquet"
)
dist_cdmx <- here::here(
  dir_distances, "cdmx_2020", "matrix_geo_station_distances.parquet"
)
dist_santiago <- here::here(
  dir_distances, "santiago_2024", "matrix_geo_station_distances.parquet"
)
dist_sp <- here::here(
  dir_distances, "sao_paulo_2010", "matrix_geo_station_distances.parquet"
)

# Define station-socioeconomic paths.
station_bogota_pq <- here::here(
  dir_station, "bogota_2018", "bogota_2018_2023_3km_station_socio.parquet"
)
station_cdmx_pq <- here::here(
  dir_station, "cdmx_2020", "cdmx_2020_2023_station_socio.parquet"
)
station_santiago_pq <- here::here(
  dir_station, "santiago_2024", "santiago_2024_2023_station_socio.parquet"
)
station_sp_pq <- here::here(
  dir_station, "sao_paulo_2010", "sao_paulo_2010_2023_station_socio.parquet"
)

# Define collapsed census paths.
census_bogota_csv <- here::here(
  dir_census, "bogota_2018", "census_2018_metro_collapsed.csv"
)
census_cdmx_csv <- here::here(
  dir_census, "cdmx_extended_2020", "collapse_metro_area_2020.csv"
)
census_santiago_csv <- here::here(
  dir_census, "santiago_2024", "census_santiago_collapsed_2024.csv"
)
census_sp_csv <- here::here(
  dir_census, "sao_paulo_2010", "census_sp_collapsed_2010.csv"
)


# ======================================================================================
# II. Helper functions
# ======================================================================================

# --------------------------------------------------------------------------------------
# Function: normalize_station_id
#
# @Arg x : character vector; station names or IDs.
#
# @Output : normalized station IDs.
# --------------------------------------------------------------------------------------
normalize_station_id <- function(x) {
  
  # Normalize station identifiers as in the processing pipeline.
  x <- toupper(trimws(as.character(x)))
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  
  # Remove double quotes that may appear in station names.
  gsub('"', "", x)
}


# --------------------------------------------------------------------------------------
# Function: make_station_acronyms
#
# @Arg station_ids : character vector; normalized station identifiers.
#
# @Output : character vector with unique three-letter acronyms.
# --------------------------------------------------------------------------------------
make_station_acronyms <- function(station_ids) {
  
  # Clean station names to letters and spaces only.
  clean <- toupper(gsub("[^A-Z ]", " ", normalize_station_id(station_ids)))
  clean <- trimws(gsub("\\s+", " ", clean))
  
  # Build a base acronym from initials.
  pieces <- strsplit(clean, " ")
  
  base <- vapply(pieces, function(z) {
    z <- z[nzchar(z)]
    
    if (length(z) >= 3L) {
      return(substr(paste0(substr(z[1:3], 1L, 1L), collapse = ""), 1L, 3L))
    }
    
    if (length(z) == 2L) {
      return(substr(paste0(substr(z[1], 1L, 2L), substr(z[2], 1L, 1L)), 1L, 3L))
    }
    
    if (length(z) == 1L) {
      return(substr(paste0(z[1], "XXX"), 1L, 3L))
    }
    
    "STA"
  }, character(1))
  
  # Make duplicated acronyms unique while keeping labels short.
  out <- base
  dup_base <- duplicated(out) | duplicated(out, fromLast = TRUE)
  
  if (any(dup_base)) {
    dup_groups <- split(seq_along(out)[dup_base], out[dup_base])
    
    for (idx in dup_groups) {
      for (j in seq_along(idx)) {
        out[idx[j]] <- paste0(substr(out[idx[j]], 1L, 2L), j)
      }
    }
  }
  
  out
}


# --------------------------------------------------------------------------------------
# Function: safe_read_parquet
#
# @Arg path : string; path to Parquet file.
#
# @Output : data.table.
# --------------------------------------------------------------------------------------
safe_read_parquet <- function(path) {
  
  # Stop early if an expected processed file is missing.
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  data.table::as.data.table(arrow::read_parquet(path))
}


# --------------------------------------------------------------------------------------
# Function: safe_read_csv
#
# @Arg path : string; path to CSV file.
#
# @Output : data.table.
# --------------------------------------------------------------------------------------
safe_read_csv <- function(path) {
  
  # Stop early if an expected processed file is missing.
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  data.table::fread(path)
}


# --------------------------------------------------------------------------------------
# Function: rescale_station_education
#
# @Arg station_dt  : data.table; station-level socioeconomic exposure data.
# @Arg city_label  : string; city name used in messages.
# @Arg x_col       : string; education column to check and possibly rescale.
#
# @Output : data.table with the same columns, after a plausible-scale correction.
# --------------------------------------------------------------------------------------
rescale_station_education <- function(
    station_dt,
    city_label,
    x_col = "education_mean"
) {
  
  # Work on a copy so the caller's object is not modified by reference.
  dt <- data.table::copy(data.table::as.data.table(station_dt))
  
  if (!x_col %in% names(dt)) {
    return(dt)
  }
  
  # Correct likely scale errors such as Sao Paulo values stored x1000.
  max_x <- max(as.numeric(dt[[x_col]]), na.rm = TRUE)
  
  if (is.finite(max_x) && max_x > 100) {
    message(
      "[station plots] Rescaling ", x_col,
      " by 1000 for ", city_label,
      " because values exceed plausible years of schooling."
    )
    
    dt[, (x_col) := as.numeric(get(x_col)) / 1000]
  }
  
  dt[]
}


# --------------------------------------------------------------------------------------
# Function: get_active_station_ids
#
# @Arg station_dt : data.table; station-socioeconomic processed data.
# @Arg pollutant  : string; "pm10" or "pm25".
#
# @Output : character vector with active station IDs for the pollutant.
# --------------------------------------------------------------------------------------
get_active_station_ids <- function(station_dt, pollutant = c("pm10", "pm25")) {
  
  # Match requested pollutant.
  pollutant <- match.arg(pollutant)
  
  # Define observation-count column.
  obs_col <- paste0("n_obs_", pollutant)
  
  if (!obs_col %in% names(station_dt)) {
    stop("Column not found in station data: ", obs_col)
  }
  
  # Keep stations with at least one valid observation.
  out <- station_dt[
    !is.na(get(obs_col)) & get(obs_col) > 0,
    normalize_station_id(station_id)
  ]
  
  unique(out)
}


# --------------------------------------------------------------------------------------
# Function: build_station_distance_trend_data
#
# @Arg dist_pq      : string; geo-to-station distance matrix Parquet.
# @Arg census_dt    : data.table; collapsed census data by geographic unit.
# @Arg active_ids   : character vector; active station IDs for one pollutant.
# @Arg geo_id_col   : string; geographic unit ID column in census_dt.
# @Arg edu_col      : string; average education column in census_dt.
# @Arg radius_km    : numeric; radius used to count nearby stations.
#
# @Output : data.table with geographic-unit trend inputs.
# --------------------------------------------------------------------------------------
build_station_distance_trend_data <- function(
    dist_pq,
    census_dt,
    active_ids,
    geo_id_col,
    edu_col,
    radius_km = 3
) {
  
  # Read distance matrix and standardize names.
  dist_dt <- safe_read_parquet(dist_pq)
  
  # Validate distance matrix schema.
  req_cols <- c("geo_id", "station_id", "distance_km")
  missing_cols <- setdiff(req_cols, names(dist_dt))
  
  if (length(missing_cols) > 0L) {
    stop("Distance matrix missing: ", paste(missing_cols, collapse = ", "))
  }
  
  # Normalize join keys.
  dist_dt[, geo_id := as.character(geo_id)]
  dist_dt[, station_id := normalize_station_id(station_id)]
  
  # Filter to stations that reported the pollutant in 2023.
  dist_dt <- dist_dt[station_id %in% active_ids]
  
  if (nrow(dist_dt) == 0L) {
    stop("No active stations from station data matched the distance matrix.")
  }
  
  # Compute distance to nearest station and count stations within radius.
  coverage_dt <- dist_dt[
    ,
    .(
      distance_nearest_km = min(distance_km, na.rm = TRUE),
      n_stations_radius = sum(distance_km <= radius_km, na.rm = TRUE)
    ),
    by = geo_id
  ]
  
  # Prepare census data.
  census_use <- data.table::copy(census_dt)
  data.table::setnames(census_use, geo_id_col, "geo_id")
  census_use[, geo_id := as.character(geo_id)]
  
  # Keep only the education variable needed for ranking.
  census_use <- census_use[, .(geo_id, education_mean = get(edu_col))]
  
  # Merge coverage measures with census education.
  out <- merge(coverage_dt, census_use, by = "geo_id", all.x = TRUE)
  
  # Keep units with valid education and valid nearest-distance information.
  out <- out[
    !is.na(education_mean) & !is.na(distance_nearest_km)
  ]
  
  # Rank geographic units from lowest to highest education.
  data.table::setorder(out, education_mean)
  out[, education_rank := seq_len(.N)]
  out[, education_percentile := education_rank / .N]
  
  out[]
}


# --------------------------------------------------------------------------------------
# Function: plot_station_distance_trend
#
# @Arg dt                     : data.table; station-distance trend inputs.
# @Arg city_label             : string; city label.
# @Arg pollutant              : string; pollutant label.
# @Arg radius_km              : numeric; radius used for nearby-station count.
# @Arg out_file               : string; output image path.
# @Arg distance_color         : string; color for nearest-station distance.
# @Arg station_count_color    : string; color for number of nearby stations.
# @Arg font_family            : string; base font family.
# @Arg adaptive_y_axis        : logical; zoom y-axis to fitted curves. Default TRUE.
#
# @Output : ggplot object, saved to disk.
# --------------------------------------------------------------------------------------
plot_station_distance_trend <- function(
    dt,
    city_label,
    pollutant,
    radius_km,
    out_file,
    distance_color = color_nearest_distance,
    station_count_color = color_station_count,
    font_family = plot_font_family,
    adaptive_y_axis = TRUE
) {
  
  # Keep a local plotting copy.
  plot_dt <- data.table::copy(dt)
  
  # Define rescaling so distance can be displayed on the secondary axis.
  max_count <- max(plot_dt$n_stations_radius, na.rm = TRUE)
  max_dist <- max(plot_dt$distance_nearest_km, na.rm = TRUE)
  
  scale_fac <- ifelse(max_dist > 0, max_count / max_dist, 1)
  
  if (!is.finite(scale_fac) || scale_fac <= 0) {
    scale_fac <- 1
  }
  
  plot_dt[, distance_scaled := distance_nearest_km * scale_fac]
  
  # Use fitted values to define an informative y-axis range when requested.
  y_limits <- NULL
  
  if (isTRUE(adaptive_y_axis) && nrow(plot_dt) >= 4L) {
    fit_count <- stats::lm(
      n_stations_radius ~ stats::poly(education_percentile, 3),
      data = plot_dt
    )
    
    fit_dist <- stats::lm(
      distance_scaled ~ stats::poly(education_percentile, 3),
      data = plot_dt
    )
    
    pred_x <- data.frame(education_percentile = seq(0, 1, length.out = 200))
    pred_count <- stats::predict(fit_count, newdata = pred_x)
    pred_dist <- stats::predict(fit_dist, newdata = pred_x)
    
    y_values <- c(
      pred_count,
      pred_dist
    )
    
    y_limits <- range(y_values[is.finite(y_values)], na.rm = TRUE)
    y_pad <- diff(y_limits) * 0.052
    
    if (is.finite(y_pad) && y_pad > 0) {
      y_limits <- c(y_limits[1] - y_pad, y_limits[2] + y_pad)
    }
  }
  
  # Build plot with cubic polynomial trends.
  p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = education_percentile)) +
    ggplot2::geom_smooth(
      ggplot2::aes(y = n_stations_radius, color = "Stations within radius"),
      formula = y ~ poly(x, 3),
      method = "lm",
      se = FALSE,
      linewidth = 0.9
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(y = distance_scaled, color = "Nearest-station distance"),
      formula = y ~ poly(x, 3),
      method = "lm",
      se = FALSE,
      linewidth = 0.9
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Stations within radius" = station_count_color,
        "Nearest-station distance" = distance_color
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.03, 0.03))
    ) +
    ggplot2::scale_y_continuous(
      name = paste0("Number of stations within ", radius_km, " km"),
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_fac,
        name = "Distance to nearest station (km)"
      )
    ) +
    ggplot2::coord_cartesian(ylim = y_limits, clip = "off") +
    ggplot2::labs(
      title = city_label,
      subtitle = paste0("Stations reporting ", pollutant, " in 2023"),
      x = "Geographic units ranked by average years of schooling",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_family = font_family, base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(10, 22, 12, 10)
    )
  
  ggplot2::ggsave(
    filename = out_file,
    plot = p,
    width = plot_width,
    height = plot_height,
    dpi = plot_dpi,
    limitsize = FALSE,
    bg = "white"
  )
  
  return(p)
}


# --------------------------------------------------------------------------------------
# Function: prepare_station_scatter_data
#
# @Arg station_dt : data.table; station-socioeconomic data.
# @Arg y_cols     : character vector; outcome columns to plot.
# @Arg y_labels   : named character vector; labels for outcome columns.
#
# @Output : data.table in long format.
# --------------------------------------------------------------------------------------
prepare_station_scatter_data <- function(
    station_dt,
    y_cols,
    y_labels
) {
  
  # Ensure station data is copied before transformation.
  dt <- data.table::copy(station_dt)
  
  # Require education_mean for all station-level scatterplots.
  if (!"education_mean" %in% names(dt)) {
    stop("Column `education_mean` not found in station-socio data.")
  }
  
  # Use station_id as the stable label source.
  dt[, station_id := normalize_station_id(station_id)]
  dt[, station_acr := make_station_acronyms(station_id)]
  
  # Keep only columns needed for plotting.
  keep_cols <- unique(c("station_id", "station_acr", "education_mean", y_cols))
  dt <- dt[, ..keep_cols]
  
  # Reshape to long format.
  long <- data.table::melt(
    dt,
    id.vars = c("station_id", "station_acr", "education_mean"),
    measure.vars = y_cols,
    variable.name = "outcome",
    value.name = "value"
  )
  
  # Attach clean labels.
  long[, outcome_label := y_labels[as.character(outcome)]]
  
  # Drop missing and non-finite values.
  long <- long[!is.na(education_mean) & !is.na(value) & is.finite(value)]
  
  long[]
}


# --------------------------------------------------------------------------------------
# Function: plot_dual_pollutant_station_scatter
#
# @Arg station_dt          : data.table; station-level socioeconomic exposure data.
# @Arg y_pm10              : string; PM10 outcome column.
# @Arg y_pm25              : string; PM2.5 outcome column.
# @Arg city_label          : string; city name used in title.
# @Arg x_col               : string; socioeconomic variable on the x-axis.
# @Arg x_label             : string; x-axis label.
# @Arg y_left              : string; left y-axis label for PM10.
# @Arg y_right             : string; right y-axis label for PM2.5.
# @Arg title               : string or NULL; plot title.
# @Arg out_file            : string or NULL; if provided, saves the figure.
# @Arg pollutant_colors    : named vector with colors for PM10 and PM2.5.
# @Arg font_family         : string; base font family.
# @Arg adaptive_y_axis     : logical; zoom y-axis to observed data. Default TRUE.
#
# @Output : ggplot object.
#
# @Purpose:
#   Plots station-level PM10 and PM2.5 outcomes against education in the census
#   tract where the station is located. PM2.5 is rescaled to the PM10 axis.
# --------------------------------------------------------------------------------------
plot_dual_pollutant_station_scatter <- function(
    station_dt,
    y_pm10,
    y_pm25,
    city_label,
    x_col = "education_mean",
    x_label = paste(
      "Average education in the census tract",
      "where the station is located"
    ),
    y_left = "PM10",
    y_right = "PM2.5",
    title = NULL,
    out_file = NULL,
    width = plot_width,
    height = plot_height,
    dpi = plot_dpi,
    point_size = 1.8,
    alpha = 0.85,
    add_smooth = TRUE,
    pollutant_colors = plot_pollutant_colors,
    font_family = plot_font_family,
    adaptive_y_axis = TRUE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  pkgs <- c("data.table", "ggplot2")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package missing: ", p)
    }
  }
  
  # 1. Validate and prepare inputs
  # -----------------------------------------------------------------------
  dt <- data.table::copy(data.table::as.data.table(station_dt))
  
  req_cols <- c(x_col, y_pm10, y_pm25)
  miss_cols <- setdiff(req_cols, names(dt))
  
  if (length(miss_cols) > 0L) {
    stop("Missing column(s): ", paste(miss_cols, collapse = ", "))
  }
  
  # Use the already-validated station education variable on the x-axis.
  dt[, x_plot := as.numeric(get(x_col))]
  
  dt <- dt[!is.na(x_plot) & is.finite(x_plot)]
  
  if (nrow(dt) == 0L) {
    stop("No valid observations after filtering missing x-axis values.")
  }
  
  # 2. Reshape to long format
  # -----------------------------------------------------------------------
  keep_cols <- unique(c("x_plot", y_pm10, y_pm25))
  plot_dt <- dt[, ..keep_cols]
  
  long <- data.table::melt(
    plot_dt,
    id.vars = "x_plot",
    measure.vars = c(y_pm10, y_pm25),
    variable.name = "outcome",
    value.name = "value"
  )
  
  long <- long[!is.na(value) & is.finite(value)]
  
  if (nrow(long) == 0L) {
    stop("No valid observations after reshaping outcome columns.")
  }
  
  # 3. Create pollutant labels and rescale PM2.5
  # -----------------------------------------------------------------------
  long[
    ,
    pollutant := data.table::fcase(
      outcome == y_pm10, "PM10",
      outcome == y_pm25, "PM2.5",
      default = NA_character_
    )
  ]
  
  long <- long[!is.na(pollutant)]
  long[, value_plot := as.numeric(value)]
  
  max_pm10 <- long[pollutant == "PM10", max(value, na.rm = TRUE)]
  max_pm25 <- long[pollutant == "PM2.5", max(value, na.rm = TRUE)]
  
  scale_fac <- if (
    is.finite(max_pm10) && is.finite(max_pm25) && max_pm25 > 0
  ) {
    max_pm10 / max_pm25
  } else {
    1
  }
  
  long[pollutant == "PM2.5", value_plot := as.numeric(value) * scale_fac]
  
  # Define a data-driven range that includes both observed points and fits.
  y_limits <- NULL
  
  if (isTRUE(adaptive_y_axis)) {
    y_values <- long$value_plot
    
    if (isTRUE(add_smooth)) {
      for (pol_i in unique(long$pollutant)) {
        fit_dt <- long[pollutant == pol_i]
        
        if (nrow(fit_dt) >= 2L) {
          fit_i <- stats::lm(value_plot ~ x_plot, data = fit_dt)
          x_grid <- data.frame(
            x_plot = seq(
              min(fit_dt$x_plot, na.rm = TRUE),
              max(fit_dt$x_plot, na.rm = TRUE),
              length.out = 200
            )
          )
          
          y_values <- c(y_values, stats::predict(fit_i, newdata = x_grid))
        }
      }
    }
    
    y_limits <- range(y_values[is.finite(y_values)], na.rm = TRUE)
    y_pad <- diff(y_limits) * 0.12
    
    if (is.finite(y_pad) && y_pad > 0) {
      y_limits <- c(y_limits[1] - y_pad, y_limits[2] + y_pad)
    }
  }
  
  if (is.null(title)) {
    title <- paste0(city_label, ": Station outcomes and education")
  }
  
  # 4. Build plot
  # -----------------------------------------------------------------------
  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = x_plot,
      y = value_plot,
      color = pollutant,
      linetype = pollutant,
      shape = pollutant
    )
  ) +
    ggplot2::geom_point(size = point_size, alpha = alpha) +
    ggplot2::scale_color_manual(values = pollutant_colors) +
    ggplot2::scale_linetype_manual(
      values = c("PM10" = "solid", "PM2.5" = "dashed")
    ) +
    ggplot2::scale_shape_manual(values = c("PM10" = 16, "PM2.5" = 17)) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.04, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      name = y_left,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_fac,
        name = y_right
      )
    ) +
    ggplot2::coord_cartesian(ylim = y_limits, clip = "off") +
    ggplot2::labs(
      title = title,
      x = x_label,
      color = NULL,
      shape = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_family = font_family, base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 11, margin = ggplot2::margin(t = 8)),
      axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 8)),
      plot.margin = ggplot2::margin(10, 26, 14, 12)
    )
  
  # 5. Add fitted lines if requested
  # -----------------------------------------------------------------------
  if (isTRUE(add_smooth)) {
    p <- p +
      ggplot2::geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linewidth = 0.8,
        show.legend = FALSE
      )
  }
  
  if (!is.null(out_file)) {
    ggplot2::ggsave(
      filename = out_file,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      limitsize = FALSE,
      bg = "white"
    )
  }
  
  return(p)
}


# --------------------------------------------------------------------------------------
# Function: save_city_monitoring_figures
#
# @Arg city_label  : string; city name for plot titles.
# @Arg city_id     : string; city identifier for filenames.
# @Arg dist_pq     : string; distance matrix path.
# @Arg census_dt   : data.table; collapsed census data.
# @Arg station_dt  : data.table; station-socioeconomic data.
# @Arg geo_id_col  : string; geographic ID column in census_dt.
# @Arg edu_col     : string; average education column in census_dt.
# @Arg radius_km   : numeric; radius used in coverage figure.
# @Arg outdir_fig  : string; output folder.
#
# @Output : invisible list with ggplot objects.
# --------------------------------------------------------------------------------------
save_city_monitoring_figures <- function(
    city_label,
    city_id,
    dist_pq,
    census_dt,
    station_dt,
    geo_id_col,
    edu_col,
    radius_km = 3,
    outdir_fig,
    adaptive_y_axis = TRUE
) {
  
  # Store plot objects in a named list.
  plots <- list()
  
  # Apply station-level education scale correction once per city.
  station_dt <- rescale_station_education(
    station_dt = station_dt,
    city_label = city_label,
    x_col = "education_mean"
  )
  
  # 1. PM10 station coverage and nearest-distance plot.
  active_pm10 <- get_active_station_ids(station_dt, pollutant = "pm10")
  
  dt_pm10 <- build_station_distance_trend_data(
    dist_pq = dist_pq,
    census_dt = census_dt,
    active_ids = active_pm10,
    geo_id_col = geo_id_col,
    edu_col = edu_col,
    radius_km = radius_km
  )
  
  plots$distance_pm10 <- plot_station_distance_trend(
    dt = dt_pm10,
    city_label = city_label,
    pollutant = "PM10",
    radius_km = radius_km,
    out_file = file.path(
      outdir_fig,
      paste0(city_id, "_stations_distance_pm10_", radius_km, "km.png")
    ),
    adaptive_y_axis = adaptive_y_axis
  )
  
  # 2. PM2.5 station coverage and nearest-distance plot.
  active_pm25 <- get_active_station_ids(station_dt, pollutant = "pm25")
  
  dt_pm25 <- build_station_distance_trend_data(
    dist_pq = dist_pq,
    census_dt = census_dt,
    active_ids = active_pm25,
    geo_id_col = geo_id_col,
    edu_col = edu_col,
    radius_km = radius_km
  )
  
  plots$distance_pm25 <- plot_station_distance_trend(
    dt = dt_pm25,
    city_label = city_label,
    pollutant = "PM2.5",
    radius_km = radius_km,
    out_file = file.path(
      outdir_fig,
      paste0(city_id, "_stations_distance_pm25_", radius_km, "km.png")
    ),
    adaptive_y_axis = adaptive_y_axis
  )
  
  # 3. Average PM10 and PM2.5 concentration versus education.
  plots$avg_pollution <- plot_dual_pollutant_station_scatter(
    station_dt = station_dt,
    city_label = city_label,
    y_pm10 = "avg_pm10",
    y_pm25 = "avg_pm25",
    title = "Annual average concentration in 2023",
    y_left = "PM10 annual average",
    y_right = "PM2.5 annual average",
    out_file = file.path(
      outdir_fig,
      paste0(city_id, "_avg_pm10_pm25_vs_education.png")
    ),
    adaptive_y_axis = adaptive_y_axis
  )
  
  # 4. Hours above IT1 versus education.
  plots$hours_it1 <- plot_dual_pollutant_station_scatter(
    station_dt = station_dt,
    city_label = city_label,
    y_pm10 = "hrs_d_pm10_it1",
    y_pm25 = "hrs_d_pm25_it1",
    title = "Hours above WHO IT1 threshold in 2023",
    y_left = "PM10 hours above IT1",
    y_right = "PM2.5 hours above IT1",
    out_file = file.path(
      outdir_fig,
      paste0(city_id, "_hours_it1_pm10_pm25_vs_education.png")
    ),
    adaptive_y_axis = adaptive_y_axis
  )
  
  # 5. Hours above IT2 versus education.
  plots$hours_it2 <- plot_dual_pollutant_station_scatter(
    station_dt = station_dt,
    city_label = city_label,
    y_pm10 = "hrs_d_pm10_it2",
    y_pm25 = "hrs_d_pm25_it2",
    title = "Hours above WHO IT2 threshold in 2023",
    y_left = "PM10 hours above IT2",
    y_right = "PM2.5 hours above IT2",
    out_file = file.path(
      outdir_fig,
      paste0(city_id, "_hours_it2_pm10_pm25_vs_education.png")
    ),
    adaptive_y_axis = adaptive_y_axis
  )
  
  invisible(plots)
}


# ======================================================================================
# III. Read processed data
# ======================================================================================

# Read processed station-socioeconomic data.
station_bogota <- safe_read_parquet(station_bogota_pq)
station_cdmx <- safe_read_parquet(station_cdmx_pq)
station_santiago <- safe_read_parquet(station_santiago_pq)
station_sp <- safe_read_parquet(station_sp_pq)

# Read collapsed census data.
census_bogota <- safe_read_csv(census_bogota_csv)
census_cdmx <- safe_read_csv(census_cdmx_csv)
census_santiago <- safe_read_csv(census_santiago_csv)
census_sp <- safe_read_csv(census_sp_csv)


# ======================================================================================
# IV. Generate and save figures
# ======================================================================================

# 1. Bogotá.
plots_bogota <- save_city_monitoring_figures(
  city_label = "Bogota",
  city_id = "bogota_2018",
  dist_pq = dist_bogota,
  census_dt = census_bogota,
  station_dt = station_bogota,
  geo_id_col = "GEO_ID",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

# 2. Mexico City.
plots_cdmx <- save_city_monitoring_figures(
  city_label = "Mexico City",
  city_id = "cdmx_2020",
  dist_pq = dist_cdmx,
  census_dt = census_cdmx,
  station_dt = station_cdmx,
  geo_id_col = "CVE_MUN",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

# 3. Gran Santiago.
plots_santiago <- save_city_monitoring_figures(
  city_label = "Gran Santiago",
  city_id = "santiago_2024",
  dist_pq = dist_santiago,
  census_dt = census_santiago,
  station_dt = station_santiago,
  geo_id_col = "CUT",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

# 4. Sao Paulo.
plots_sp <- save_city_monitoring_figures(
  city_label = "Sao Paulo",
  city_id = "sao_paulo_2010",
  dist_pq = dist_sp,
  census_dt = census_sp,
  station_dt = station_sp,
  geo_id_col = "code_weighting",
  edu_col = "education_mean",
  radius_km = 3,
  outdir_fig = outdir_fig
)

cat("Station-monitoring figures saved to: ", outdir_fig, "\n")
