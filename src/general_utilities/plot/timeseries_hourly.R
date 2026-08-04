# ============================================================================================
# IDB: Air monitoring — time series and hourly profiles
# ============================================================================================
# @Goal: Functions for time series and hourly profiles.
#
# @Description: Concentration over time and over the hour of day, including the ridgeline profiles and the
#   time-above-target spans.
#   Sourced by config_utils_plot_tables.R; never sourced directly by a script.
#
# @Summary:
#   1. plot_city_distributions
#   2. save_plot_list_to_pdf
#   3. plot_pm25_timeseries_smooth
#   4. plot_hourly_avg_pollution
#   5. plot_hourly_ridgeline_pollution
#   6. compute_time_spans_above_target
#   7. plot_time_spans_ridgeline
#   8. summarize_hourly_by_station
#   9. plot_hourly_stacked_stations
#
# @Date: August 2026
# @Author: Marcos Paulo
# ============================================================================================

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


# ---------------------------------------------------------------------------------------------
# Function: summarize_hourly_by_station
# @Arg         : df           a data frame with columns for station code, datetime, and value.
# @Arg         : station_col  name of the station code column (default "station_code").
# @Arg         : datetime_col name of the datetime column (default "date2_hour").
# @Arg         : value_col    name of the pollutant column to average - default pm25_validated
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
# @Arg         : hourly_df      output of summarize_hourly_by_station().
# @Arg         : region_name    string for the plot title (e.g., "Santiago").
# @Arg         : pollutant_label label to show (e.g., "PM2.5" or "PM10").
# @Arg         : filter_label   subtitle (e.g., "All values", "Values > IT1 (35 µg/m³)").
# @Arg         : normalize      if TRUE, 100% stacks by hour (shares); else stacks abs means.
# @Arg         : show_it_lines  if TRUE and not normalized, add IT2/IT1 vertical lines.
# @Arg         : year           integer shown in title (default 2012L for signature parity).
# @Arg         : it1, it2       numeric WHO lines if show_it_lines = TRUE.
# @Arg         : base_family    font family for theme.
# @Output      : A ggplot object (horizontal stacked bars; one bar per hour; segment = station).
# @Purpose     : Visualize composition and level (or share) of hourly averages by station 
# with a fixed station order across all hours to ease comparisons.
# @Written_on  : 12/08/2025
# @Written_by  : Marcos Paulo
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
