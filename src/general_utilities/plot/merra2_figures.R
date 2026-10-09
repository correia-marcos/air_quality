# Optional MERRA-2 composition and station-comparison plots.
# Functions return plot objects; execution and destinations belong to optional recipes.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(zoo)
})

# --------------------------------------------------------------------------------------------
# Function: plot_city_distributions
#' @param        df is a data frame containing columns "Date", "Hour",
#                "DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS" and "pm25_estimate".
#' @param        city_name is a string representing the city's name.
#' @return       A list of five ggplot objects, each representing the distribution
#                of a selected variable for the given city.
#' @Purpose     : This function creates five distribution plots (histograms) for 
#                selected variables from the given data frame. The chosen variables are:
#                - DUSMASS25, OCSMASS, BCSMASS, SSSMASS25, and pm25_estimate.
#                For pm25_estimate, two vertical lines are added to indicate WHO guidelines:
#                Interim Target 1 (IT1): 35 µg/m³
#                Interim Target 2 (IT2): 25 µg/m³
#' @Written_on  : 13/12/2024
#' @Written_by  : Marcos Paulo
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
#' @param      plot_list is a list of ggplot objects for a single city.
#' @param      city_name is a string specifying the name of the city.
#' @param      output_dir is a string specifying the directory to save the PDFs.
#' @return     Saves a single PDF for the provided plot list.
#' @Purpose   : Save each list of plots into a separate PDF efficiently.
#' @Written_on: 13/12/2024
#' @Written_by: Marcos Paulo
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
#' @param        df is a data frame with columns "Date", "Hour", and PM 2.5 data.
#' @param        region_name is a string representing the region or city name.
#' @param        apply_rolling is a logical indicating whether to apply a rolling window.
#' @param        window_hours is an integer specifying size (in hours) of the rolling window.
#' @param        corr_method is a string specifying the correlation method ("pearson" default).
#' @param        color_merra2 is a string specifying the color for the MERRA-2 series.
#' @param        color_stations is a string specifying the color for the ground station series.
#' @return       A ggplot object showing the PM2.5 time series (raw or optionally smoothed),
#                with an annotation showing the correlation (based on raw data).
#' @Purpose    : To visualize PM2.5 from MERRA-2 and ground stations, optionally applying a
#                rolling average to reduce noise, and annotate the correlation of the raw data.
#' @Written_on  : 14/02/2025
#' @Written_by  : Marcos Paulo
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
#' @param        df is a data frame with columns "Date", "Hour", "pm25_merra2"
#                and "pm25_stations".
#' @param        region_name is a string representing the region or city name.
#' @param        plot_ci is a logical indicating whether to add error bars (standard error).
#' @param        bar_width is a numeric value for the width of the bars (default is 0.7).
#' @param        color_merra2_main is a string specifying the main color for MERRA-2 bars.
#' @param        color_stations_main is a string specifying the main color for stations bars.
#' @param        color_merra2_error is a string specifying the color for MERRA-2 error bars.
#' @param        color_stations_error is a string specifying the color for station error bars.
#' @return       A ggplot object showing the average hourly PM2.5 (from MERRA-2 and stations),
#                with optional error bars in matching/darker tones, and with dashed vertical 
#                lines indicating WHO Interim Targets: IT2 (25 µg/m³, orange) and IT1 (35 µg/m³,
#                dark red). These are the IT1 and IT2 values for annual averages.
#                Returns without printing, saving or changing the global theme.
#' @Purpose     : To visualize and compare the hourly persistence of PM2.5 pollution, 
#                facilitating an understanding of differences among cities/hours.
#                The IT dashed lines help highlight when pollutant concentrations
#                exceed WHO targets.
#' @Written_on  : 28/02/2025
#' @Written_by  : Marcos Paulo
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
    theme_minimal(base_family = "Palatino", base_size = 14)
  
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
  
  return(p)
}


