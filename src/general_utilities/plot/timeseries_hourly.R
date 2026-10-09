# Ground-station hourly profiles and consecutive pollution episodes.
# Sourced by manuscript and diagnostic recipes; no analysis runs on import.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggridges)
})

# --------------------------------------------------------------------------------------------
# Function: plot_hourly_ridgeline_pollution
#' @param        df is a data frame with columns "Date", "Hour", and PM2.5 data
#                (the manuscript uses "pm25_stations").
#' @param        region_name is a string representing the region or city name.
#' @param        pollution_var is a string specifying which column of PM2.5 data to visualize.
#' @return       A ggplot object showing the distribution of PM2.5 across 24 hours in ridgeline
#                form, with dashed vertical lines indicating WHO Interim Targets: IT2 (50 µg/m³, 
#                orange) and IT1 (75 µg/m³, dark red), using 24-hour reference values.
#                Returns without printing, saving or changing the global theme.
#' @Purpose    : To visualize the distribution (rather than just the mean) of pollutants by
#                hour of day, helping to spot patterns in how pollution accumulates over time.
#' @Written_on  : 28/02/2025
#' @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
plot_hourly_ridgeline_pollution <- function(df, 
                                            region_name,
                                            pollution_var = "pm25_stations") {

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
      name   = "PM2.5\n(µg/m³)",       # keep the unit visible in the PDF legend
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
    theme_minimal(base_family = "Palatino", base_size = 14)
  
  # Add dashed vertical lines for Interim Targets and annotations
  p <- p +
    geom_vline(xintercept = 50, color = "orange", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = 75, color = "darkred", linetype = "dashed", linewidth = 0.5) +
    annotate("text", x = 53.5, y = max_hour + 2.5, label = "IT2", vjust = -0.5, 
             color = "orange", size = 3) +
    annotate("text", x = 78.5, y = max_hour + 2.5, label = "IT1", vjust = -.5, 
             color = "darkred", size = 3)
  
  return(p)
}


# --------------------------------------------------------------------------------------------
# Function: compute_time_spans_above_target
#' @param        df is a data frame with columns "Date", "Hour", and at least one
#                PM2.5 column (the manuscript uses "pm25_stations").
#' @param        city_name is a string identifying the city/region (e.g., "Bogotá").
#' @param        target is a string, either "IT1" or "IT2".
#                "IT1" => threshold = 75 µg/m³
#                "IT2" => threshold = 50 µg/m³
#' @param        pollution_var is a string specifying which PM2.5 column to check.
#' @return       A data frame listing each episode above the chosen threshold,
#                with columns: Date, Hour, time_span_above_target, city.
#' @details Missing readings and timestamp gaps end episodes; equality is above target.
# All termination paths return the same columns, including episodes at the final row.
#' @Purpose    : To identify consecutive hours where PM2.5 is >= the chosen WHO interim target
#                (IT1 or IT2), facilitating further analysis and plotting.
#' @Written_on  : 10/03/2025
#' @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_time_spans_above_target <- function(df, city_name,
                                            target = c("IT1", "IT2"),
                                            pollution_var = "pm25_stations") {
  target <- match.arg(target)
  threshold <- if (target == "IT1") 75 else 50
  datetime <- as.POSIXct(paste(df$Date, sprintf("%02d:00:00", df$Hour)),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (anyNA(datetime) || anyDuplicated(datetime)) {
    stop("Episode inputs require one valid observation per city-hour.")
  }
  order <- order(datetime)
  df <- df[order, , drop = FALSE]
  datetime <- datetime[order]
  value <- as.numeric(df[[pollution_var]])
  above <- is.finite(value) & value >= threshold
  if (!any(above)) {
    return(data.frame(Date = as.Date(character()), Hour = integer(),
      time_span_above_target = integer(), city = character()))
  }
  previous_above <- c(FALSE, utils::head(above, -1L))
  adjacent <- c(FALSE, diff(as.numeric(datetime)) == 3600)
  episode <- cumsum(above & (!previous_above | !adjacent))
  runs <- split(which(above), episode[above])
  first <- vapply(runs, function(rows) rows[1L], integer(1))
  data.frame(Date = df$Date[first], Hour = as.integer(df$Hour[first]),
    time_span_above_target = as.integer(lengths(runs)), city = city_name,
    row.names = NULL)
}


# --------------------------------------------------------------------------------------------
# Function: plot_time_spans_ridgeline
#' @param        list_of_dfs is a named list of data frames (e.g.,
#                list("Bogotá" = bogota_pm25, "Santiago" = santiago_pm25, ...)).
#' @param        target is a string, either "IT1" or "IT2".
#' @param        pollution_var is a string specifying which PM2.5 column to check.
#' @return       A ggplot object showing the distribution of consecutive hours
#                above the chosen WHO Interim Target, faceted by city on the y-axis.
#' @Purpose     : To reveal how often and for how long each city experiences 
#                pollution levels above a WHO interim target, using a ridgeline plot.
#' @Written_on  : 06/03/2025
#' @Written_by  : Marcos Paulo
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
  
  plot_episode_spans_ridgeline(final_episodes, target)
}

# ----------------------------------------------------------------------------------------
# Function: plot_episode_spans_ridgeline
#' @param final_episodes Named episode table returned by compute_time_spans_above_target().
#' @param target Display label for the threshold used to compute the episodes.
#' @return Ridgeline plot; episode calculations remain inspectable separately.
# ----------------------------------------------------------------------------------------
plot_episode_spans_ridgeline <- function(final_episodes, target = "IT2") {
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


# ---------------------------------------------------------------------------------------------
# Function: summarize_hourly_by_station
#' @param        df           a data frame with columns for station code, datetime, and value.
#' @param        station_col  name of the station code column (default "station_code").
#' @param        datetime_col name of the datetime column (default "date2_hour").
#' @param        value_col    name of the pollutant column to average - default pm25_validated
#' @param        filter_type  one of "none", "gt_it1", or "gt_it2" for threshold filtering.
#' @param        it1, it2     numeric thresholds (defaults reflect WHO annual PM2.5 IT1/IT2).
#' @param        tz           timezone for parsing if needed (kept for signature parity).
#' @param        station_lookup data.frame with columns Station (char) and StationName (char).
#' @return       A data frame with columns Station, Hour, mean_value, n and an attribute
#                "station_levels" giving a fixed station order across hours.
#' @Purpose    : Build hourly means per station, optionally filtering by WHO interim targets,
#                and attach a stable station ordering for consistent stacked plots.
#' @Written_on  : 12/08/2025
#' @Written_by  : Marcos Paulo
# ---------------------------------------------------------------------------------------------
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


# ---------------------------------------------------------------------------------------------
# Function: plot_hourly_stacked_stations
#' @param        hourly_df      output of summarize_hourly_by_station().
#' @param        region_name    string for the plot title (e.g., "Santiago").
#' @param        pollutant_label label to show (e.g., "PM2.5" or "PM10").
#' @param        filter_label   subtitle (e.g., "All values", "Values > IT1 (35 µg/m³)").
#' @param        normalize      if TRUE, 100% stacks by hour (shares); else stacks abs means.
#' @param        show_it_lines  if TRUE and not normalized, add IT2/IT1 vertical lines.
#' @param        year           integer shown in title (default 2012L for signature parity).
#' @param        it1, it2       numeric WHO lines if show_it_lines = TRUE.
#' @param        base_family    font family for theme.
#' @return       A ggplot object (horizontal stacked bars; one bar per hour; segment = station).
#' @Purpose     : Visualize composition and level (or share) of hourly averages by station
# with a fixed station order across all hours to ease comparisons.
#' @Written_on  : 12/08/2025
#' @Written_by  : Marcos Paulo
# ---------------------------------------------------------------------------------------------
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
      title    = paste0("Hourly ", pollutant_label, " by Station — ", region_name,
                        " (", year, ")"),
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
