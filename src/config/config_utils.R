# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
#
# @Date: Nov 2024
# @author: Marcos

# Load libraries - groundhog increases code reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Loading required packages 
groundhog.library(
  pkg  = c("doParallel",
           "exactextractr",
           "foreach",
           "here",
           "rnaturalearth",
           "rnaturalearthdata",
           "sf",
           "terra", 
           "tidyr"),
  date = "2024-05-07")

here::i_am(".gitignore")

# ############################################################################################
# Functions
# ############################################################################################

# Function --------------------------------------------------------------------
# @Arg       : shapefile is an 'sf' object representing the city boundary
# @Arg       : nc_files is a vector of file paths to the .nc4 files
# @Arg       : city_name is a string with the name of the city
# @Output    : A data frame with Date, Hour, and the variables: DUSMASS25, OCSMASS, BCSMASS,
#              SSSMASS25, SO4SMASS
# @Purpose   : Processes MERRA-2 .nc4 files for a given city by extracting aerosol variables
#              over the city's area at hourly resolution
# @Written_on: 02/12/2024
# @Written_by: Marcos Paulo
process_merra2_city_hourly <- function(shapefile, nc_files, city_name) {

  # Record the start time
  start_time <- Sys.time()
  
  # Variables to extract
  vars_to_extract <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop through each .nc4 file
  for (nc_file in nc_files) {
    # Suppress warnings
    suppressWarnings({
      
      # Extract the date from the file name
      file_name <- basename(nc_file)
      
      # Use regex to extract the date (assuming format YYYYMMDD)
      date_str <- sub(".*\\.(\\d{8})\\.nc4$", "\\1", file_name)
      date <- as.Date(date_str, format = "%Y%m%d")
      
      # Open the .nc4 file using terra
      nc_data <- terra::rast(nc_file)
      
      # Ensure shapefile is in the correct coordinate reference system
      crs(nc_data) <- "EPSG:4326"
      shapefile_proj <- st_transform(shapefile, crs = crs(nc_data)) # WGS84
      
      # Crop and mask the raster to the shapefile extent
      nc_data_cropped <- terra::crop(nc_data, vect(shapefile), snap = "out")
      nc_data_masked  <- terra::mask(nc_data_cropped, vect(shapefile))
      
      # Identify layers corresponding to the variables of interest
      available_vars <- names(nc_data_masked)
      vars_present <- lapply(vars_to_extract, function(var) {
        grep(paste0("^", var, "_"), available_vars, value = TRUE)
      })
      names(vars_present) <- vars_to_extract
      
      # Check if all required variables are present
      if (any(sapply(vars_present, length) == 0)) {
        # Skip this file if variables are missing
        next
      }
      
      # Assume there are 24 hours (layers) per day
      for (hour in 1:24) {
        # Initialize a list for the layers of the current hour
        hourly_layers <- list()
        
        # Process each variable for the current hour
        for (var_name in vars_to_extract) {
          # Name of the layer corresponding to the variable and current hour
          layer_name <- paste0(var_name, "_", hour)
          
          # Check if the layer exists
          if (layer_name %in% available_vars) {
            # Extract the layer
            var_layer <- nc_data[[layer_name]]
            
            # Add to the list of layers
            hourly_layers[[var_name]] <- var_layer
          } else {
            # If the layer doesn't exist, skip this hour
            next
          }
        }
        
        # Check if all variables are available for this hour
        if (length(hourly_layers) != length(vars_to_extract)) {
          # Skip this hour if not all variables are available
          next
        }
        
        # Combine the layers of the variables into a raster stack
        combined_rast <- rast(hourly_layers)
        
        
        # Use exactextractr to compute weighted sums based on area overlap
        extraction <- exactextractr::exact_extract(
          combined_rast,
          shapefile,
          fun = "sum"
        )
        
        # Aggregate the results over all features
        aggregated_values <- colSums(extraction, na.rm = TRUE)
        
        # Remove 'sum.' prefix from the names
        names(aggregated_values) <- sub('^sum\\.', '', names(aggregated_values))
        
        # Create a data frame with the results for the current hour
        hourly_data <- data.frame(
          Date = date,
          Hour = hour - 1,  # Adjusting hour to start from 0
          DUSMASS25 = aggregated_values["DUSMASS25"],
          OCSMASS = aggregated_values["OCSMASS"],
          BCSMASS = aggregated_values["BCSMASS"],
          SSSMASS25 = aggregated_values["SSSMASS25"],
          SO4SMASS = aggregated_values["SO4SMASS"],
          stringsAsFactors = FALSE
        )
        
        # Add to the results list
        results_list[[length(results_list) + 1]] <- hourly_data
      }
    })
  }
  
  # Combine the results into a data frame
  results <- do.call(rbind, results_list)
  
  # Convert the Date column to Date type (if not already)
  results$Date <- as.Date(results$Date, origin = "1970-01-01")
  
  # Sort the results by date and hour
  results <- results[order(results$Date, results$Hour), ]
  
  # Record the end time
  end_time <- Sys.time()
  
  # Calculate the total processing time
  total_time <- end_time - start_time
  
  # Display the total processing time
  cat("Total processing time for", city_name, ":", total_time, "\n")
  
  # Return the final data frame and processing time
  return(list(data = results, processing_time = total_time))
}


# Function --------------------------------------------------------------------
# @Arg       : shapefile is an 'sf' object representing the city boundary
# @Arg       : nc_files is a vector of file paths to the .nc4 files
# @Arg       : city_name is a string with the name of the city
# @Arg       : num_cores is the number of CPU cores to use for parallel processing
# @Output    : A data frame with Date, Hour, and the variables: DUSMASS25, OCSMASS, 
#              BCSMASS, SSSMASS25, SO4SMASS
# @Purpose   : Processes MERRA-2 .nc4 files for a given city by extracting aerosol 
#              variables over the city's area at hourly resolution, utilizing parallel process
# @Written_on: 02/12/2024
# @Written_by: Marcos Paulo
process_merra2_city_hourly_parallel <- function(shapefile,
                                                nc_files, 
                                                city_name, 
                                                num_cores = NULL) {

  # Record the start time
  start_time <- Sys.time()
  
  # Set up parallel backend
  if (is.null(num_cores)) {
    num_cores <- parallel::detectCores() - 1  # Use one less than the total cores
  }
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Variables to extract
  vars_to_extract <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  
  # Parallel processing using foreach
  results_list <- foreach(nc_file = nc_files,
                          .packages = c("terra", "sf", "exactextractr")) %dopar% {
    suppressWarnings({
      
      # Extract the date from the file name
      file_name <- basename(nc_file)
      
      # Use regex to extract the date (assuming format YYYYMMDD)
      date_str <- sub(".*\\.(\\d{8})\\.nc4$", "\\1", file_name)
      date <- as.Date(date_str, format = "%Y%m%d")
      
      # Open the .nc4 file using terra
      nc_data <- terra::rast(nc_file)
      
      # Ensure shapefile is in the correct coordinate reference system
      crs(nc_data) <- "EPSG:4326"
      shapefile_proj <- st_transform(shapefile, crs = crs(nc_data)) # WGS84
      
      # Crop and mask the raster to the shapefile extent
      nc_data_cropped <- terra::crop(nc_data, vect(shapefile_proj), snap = "out")
      nc_data_masked  <- terra::mask(nc_data_cropped, vect(shapefile_proj))
      
      # Identify layers corresponding to the variables of interest
      available_vars <- names(nc_data_masked)
      vars_present <- lapply(vars_to_extract, function(var) {
        grep(paste0("^", var, "_"), available_vars, value = TRUE)
      })
      names(vars_present) <- vars_to_extract
      
      # Check if all required variables are present
      if (any(sapply(vars_present, length) == 0)) {
        # Skip this file if variables are missing
        return(NULL)
      }
      
      # Initialize a list to store hourly results
      hourly_results <- list()
      
      # Assume there are 24 hours (layers) per day
      for (hour in 1:24) {
        # Initialize a list for the layers of the current hour
        hourly_layers <- list()
        
        # Process each variable for the current hour
        for (var_name in vars_to_extract) {
          # Name of the layer corresponding to the variable and current hour
          layer_name <- paste0(var_name, "_", hour)
          
          # Check if the layer exists
          if (layer_name %in% available_vars) {
            # Extract the layer
            var_layer <- nc_data[[layer_name]]
            
            # Add to the list of layers
            hourly_layers[[var_name]] <- var_layer
          } else {
            # If the layer doesn't exist, skip this hour
            next
          }
        }
        
        # Check if all variables are available for this hour
        if (length(hourly_layers) != length(vars_to_extract)) {
          # Skip this hour if not all variables are available
          next
        }
        
        # Combine the layers of the variables into a raster stack
        combined_rast <- rast(hourly_layers)

        # Use exactextractr to compute weighted sums based on area overlap
        extraction <- exactextractr::exact_extract(
          combined_rast,
          shapefile,
          fun = "sum"
        )
        
        # Aggregate the results over all features
        aggregated_values <- colSums(extraction, na.rm = TRUE)
        
        # Remove 'sum.' prefix from the names
        names(aggregated_values) <- sub('^sum\\.', '', names(aggregated_values))
        
        # Create a data frame with the results for the current hour
        hourly_data <- data.frame(
          Date = date,
          Hour = hour - 1,  # Adjusting hour to start from 0
          DUSMASS25 = aggregated_values["DUSMASS25"],
          OCSMASS = aggregated_values["OCSMASS"],
          BCSMASS = aggregated_values["BCSMASS"],
          SSSMASS25 = aggregated_values["SSSMASS25"],
          SO4SMASS = aggregated_values["SO4SMASS"],
          stringsAsFactors = FALSE
        )
        
        # Add to the hourly results
        hourly_results[[length(hourly_results) + 1]] <- hourly_data
      }
      
      # Combine the hourly results into a daily data frame
      if (length(hourly_results) > 0) {
        daily_results <- do.call(rbind, hourly_results)
        return(daily_results)
      } else {
        return(NULL)
      }
    })
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Record the end time
  end_time <- Sys.time()
  
  # Calculate the total processing time
  total_time <- end_time - start_time
  
  # Remove NULL results (files that were skipped)
  results_list <- Filter(Negate(is.null), results_list)
  
  # Combine the results into a data frame
  results <- do.call(rbind, results_list)
  
  # Convert the Date column to Date type (if not already)
  results$Date <- as.Date(results$Date, origin = "1970-01-01")
  
  # Sort the results by date and hour
  results <- results[order(results$Date, results$Hour), ]
  
  # Display the total processing time
  cat("Total processing time for", city_name, ":", total_time, "\n")
  
  # Return the final data frame and processing time
  return(results)
}


# Function --------------------------------------------------------------------
# @Arg         : df is a data frame containing columns "DUSMASS25", "OCSMASS", "BCSMASS", 
#                "SSSMASS25", and "SO4SMASS" in kg m^-3.
# @Arg         : new_column_name is a string representing the name of the new PM2.5 column.
# @Output      : The original data frame with:
#                1) The given aerosol mass columns converted from kg m^-3 to µg m^-3.
#                2) An additional column for estimated PM2.5 (also in µg m^-3).
# @Purpose     : This function first converts the specified aerosol mass columns from kg m^-3
#                to µg m^-3. Then, it calculates the PM2.5 estimate based on the formula:
#                PM2.5 = DUSMASS25 + OCSMASS + BCSMASS + SSSMASS25 + (SO4SMASS * 132.14/96.06)
#                The final PM2.5 column will also be in µg m^-3.
# @Written_on  : 13/12/2024
# @Written_by  : Marcos Paulo
convert_and_add_pm25 <- function(df, new_column_name = "pm25_estimate") {
  # Check if required columns exist
  required_cols <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  if (!all(required_cols %in% names(df))) {
    stop("The data frame must contain the following columns: 
         DUSMASS25, OCSMASS, BCSMASS, SSSMASS25, SO4SMASS.")
  }
  
  # Conversion factor: 1 kg = 1e9 µg
  conversion_factor <- 1e9
  
  # Convert the required columns from kg m^-3 to µg m^-3
  for (col in required_cols) {
    df[[col]] <- df[[col]] * conversion_factor
  }
  
  # Calculate PM2.5 estimate (already in µg m^-3 after conversion)
  df[[new_column_name]] <- df$DUSMASS25 +
    df$OCSMASS +
    df$BCSMASS +
    df$SSSMASS25 +
    (df$SO4SMASS * 132.14 / 96.06)
  
  return(df)
}


# Function --------------------------------------------------------------------
# @Arg       : shapefile  is an 'sf' object representing the city boundary
# @Arg       : nc_file    is a string containing the path to a single .nc4 file
#              from the MERRA-2 dataset
# @Arg       : city_name  is a string with the name of the city
# @Output    : A ggplot object representing the map of the city boundary and 
#              MERRA-2 grid cells
# @Purpose   : Creates a spatial plot showing the city's boundary and the overlayed
#              MERRA-2 grid cells from the specified nc_file. This visualization helps 
#              in understanding the spatial extent of the MERRA-2 data relative to 
#              the city's area.
# @Written_on: 10/12/2024
# @Written_by: Marcos Paulo
plot_merra2_grid_city <- function(shapefile, nc_file, city_name) {
  
  # Load the MERRA-2 data
  nc_data <- rast(nc_file)
  
  # Crop the raster to the extent of the shapefile plus a buffer (optional)
  # buffer_distance <- 0.0001  # Degrees, adjust as needed
  # shapefile_buffered <- st_buffer(shapefile, dist = buffer_distance)
  
  # Transform shapefile to match MERRA-2 CRS if necessary
  crs(nc_data) <- "EPSG:4326"
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
    geom_sf(data = shapefile_proj, fill = "grey50", color = "grey10", size = 0.001) +
    geom_sf(data = grid_centroids, shape = 3, fill = "navy", color = "navy", size = 1.5) +
    labs(
      title = paste("MERRA-2 Grid Cells over", city_name),
      x = "Longitude", 
      y = "Latitude"
    ) +
    coord_sf() +
    scale_x_continuous(name = "Longitude", breaks = seq(-180, 180, by = 0.5)) +
    scale_y_continuous(name = "Latitude", breaks = seq(-90, 90, by = 0.5)) +
    theme_minimal(base_size = 14) +
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


# Function --------------------------------------------------------------------
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
# df <- bogota
# city_name <- "Bogotá"
# var <- vars_to_plot[1]
plot_city_distributions <- function(df, city_name) {
  
  # Associate variable names with descriptions
  variable_descriptions <- list(
    DUSMASS25 = "Dust Surface Mass Concentration - PM 2.5",
    OCSMASS = "Organic Carbon Surface Mass Concentration",
    BCSMASS = "Black Carbon Surface Mass Concentration",
    SSSMASS25 = "Sea Salt Surface Mass Concentration - PM 2.5",
    SO4SMASS = "SO4 Surface Mass Concentration",
    pm25_estimate = "PM 2.5 Estimation from Aerosols Data")
  
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
      geom_density(fill = "steelblue", color = "black", alpha = 0.5, linewidth = 0.8) +
      labs(
        title = paste(city_name, "-", variable_descriptions[[var]]),
        x = variable_descriptions[[var]],
        y = "Density") +
      theme_minimal(base_size = 14) +
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
                     color = "red", linetype = "dashed", linewidth = 0.5) +
        geom_segment(x = 35, xend = 35, y = 0, yend = max_y, 
                     color = "red", linetype = "dashed", linewidth = 0.5) +
        annotate("text", x = 26.1, y = (max_y - 0.01), label = "IT2", vjust = -1, 
                 color = "red", size = 3) +
        annotate("text", x = 36.1, y = (max_y - 0.01), label = "IT1", vjust = -1, 
                 color = "red", size = 3)
      # +
      # labs(
        #   subtitle = "WHO Interim Targets: IT1 = 35 µg/m³, IT2 = 25 µg/m³")
    }
    
    plot_list[[var]] <- p
  }
  
  print(plot_list)
  
  return(plot_list)
}
