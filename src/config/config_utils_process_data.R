# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
# 
# @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "process_data" scripts.
# 
# @Date: Nov 2024
# @author: Marcos Paulo
# ============================================================================================

# List of the necessary packages
packages <- c(
  "dplyr",
  "doParallel",
  "exactextractr",
  "foreach",
  "haven",
  "here",
  "memuse",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "terra",
  "tidyr")

# Define the default source library for packages installation - may have problems otherwise
options(repos=c(CRAN="https://cran.rstudio.com/"))

# Check if each package is installed; if not, install it and Then load them
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
rm(list = ls())

# ############################################################################################
# Functions
# ############################################################################################

# Function --------------------------------------------------------------------
# @Arg         : shapefile is an 'sf' object representing the city boundary (or any polygon
#                collection for which grid‐level values are desired).
# @Arg         : nc_files is a vector of file paths to the .nc4 files.
# @Arg         : city_name is a string with the name of the city.
# @Arg         : extraction_fun is either a character string (e.g., "mean") that will be 
#                passed to exact_extract via its 'fun' argument, or NULL. When not NULL, 
#                exact_extract returns an aggregated (single) value per hour. When NULL, 
#                exact_extract returns a list of data frames (one per feature in the input 
#                shapefile), and the function will add a column 'feature_index' to identify 
#                each grid cell.
# @Output      : A data frame with Date, Hour, and the aerosol variables. When extraction_fun 
#                is not NULL, a single row per hour is returned (aggregated over the shapefile).
#                When extraction_fun is NULL, the output includes one row per grid per hour,
#                including a 'feature_index' column.
# @Purpose     : Processes MERRA-2 .nc4 files for a given city by extracting aerosol variables
#                over the city's area at hourly resolution.
# @Written_on  : 02/12/2024
# @Written_by  : Marcos Paulo
process_merra2_city_hourly <- function(shapefile,
                                       nc_files,
                                       city_name,
                                       extraction_fun = "mean") {
  
  # Record the start time
  start_time <- Sys.time()
  
  # If an aggregation function is provided, union the shapefile so that it is a single feature.
  if (!is.null(extraction_fun)) {
    shapefile <- sf::st_union(shapefile)
  }
  
  # Variables to extract
  vars_to_extract <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop through each .nc4 file
  for (nc_file in nc_files) {
    suppressWarnings({
      
      # Extract the date from the file name
      file_name <- basename(nc_file)
      date_str <- sub(".*\\.(\\d{8})\\.nc4$", "\\1", file_name)
      date <- as.Date(date_str, format = "%Y%m%d")
      
      # Open the .nc4 file using terra
      nc_data <- terra::rast(nc_file)
      
      # Ensure shapefile is in the correct coordinate reference system
      terra::crs(nc_data) <- "EPSG:4326"
      shapefile_proj <- sf::st_transform(shapefile, crs = terra::crs(nc_data))
      
      # Crop and mask the raster to the shapefile extent
      nc_data_cropped <- terra::crop(nc_data, terra::vect(shapefile_proj), snap = "out")
      nc_data_masked  <- terra::mask(nc_data_cropped, terra::vect(shapefile_proj))
      
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
          if (layer_name %in% available_vars) {
            # Extract the layer
            var_layer <- nc_data[[layer_name]]
            hourly_layers[[var_name]] <- var_layer
          } else {
            # If a layer is missing, skip to the next hour
            next
          }
        }
        
        # Check if all variables are available for this hour
        if (length(hourly_layers) != length(vars_to_extract)) {
          next
        }
        
        # Combine the layers of the variables into a raster stack
        combined_rast <- terra::rast(hourly_layers)
        
        if (!is.null(extraction_fun)) {
          # When extraction_fun is provided (e.g., "mean")
          extraction <- exactextractr::exact_extract(combined_rast,
                                                     shapefile,
                                                     fun = extraction_fun)
          aggregated_values <- extraction
          # Remove the prefix (e.g., "mean.") from the names
          names(aggregated_values) <- sub(paste0('^', extraction_fun, '\\.'),
                                          '',
                                          names(aggregated_values))
          
          # Create a data frame with one row for the current hour
          hourly_data <- data.frame(
            Date = date,
            Hour = hour - 1,  # Adjust hour to start from 0
            DUSMASS25 = aggregated_values["DUSMASS25"],
            OCSMASS   = aggregated_values["OCSMASS"],
            BCSMASS   = aggregated_values["BCSMASS"],
            SSSMASS25 = aggregated_values["SSSMASS25"],
            SO4SMASS  = aggregated_values["SO4SMASS"],
            stringsAsFactors = FALSE
          )
        } else {
          # When extraction_fun is NULL: extract values for each grid cell
          extraction <- exactextractr::exact_extract(combined_rast,
                                                     shapefile,
                                                     include_cell = TRUE,
                                                     fun = NULL)
          if (is.list(extraction)) {
            extraction_list <- lapply(seq_along(extraction), function(i) {
              df_ex <- extraction[[i]]
              df_ex$feature_index <- i
              df_ex
            })
            extraction_df <- do.call(rbind, extraction_list)
          } else {
            extraction_df <- extraction
            extraction_df$feature_index <- 1
          }
          # Add Date and Hour information to each row
          extraction_df$Date <- date
          extraction_df$Hour <- hour - 1
          # Keep only desired columns: Date, Hour, feature_index, and aerosol variables
          cols_to_keep <- c("Date", "Hour", "feature_index", vars_to_extract)
          hourly_data <- extraction_df[,
                                       intersect(cols_to_keep, names(extraction_df)),
                                       drop = FALSE
                                       ]
        }
        
        # Add the hourly data (which may have one or multiple rows) to the results list
        results_list[[length(results_list) + 1]] <- hourly_data
      }
    })
  }
  
  # Combine the results into a data frame
  results <- do.call(rbind, results_list)
  results$Date <- as.Date(results$Date, origin = "1970-01-01")
  results <- results[order(results$Date, results$Hour), ]
  rownames(results) <- NULL
  
  # Record the end time and calculate total processing time
  end_time <- Sys.time()
  total_time <- end_time - start_time
  cat("Total processing time for", city_name, ":", total_time, " min.\n")
  
  # Return the final data frame
  return(results)
}


# Function --------------------------------------------------------------------
# @Arg         : shapefile is an 'sf' object representing the city boundary (or any polygon
#                collection for which grid‐level values are desired).
# @Arg         : nc_files is a vector of file paths to the .nc4 files.
# @Arg         : city_name is a string with the name of the city.
# @Arg         : num_cores is the number of CPU cores to use for parallel processing.
# @Arg         : extraction_fun is either a character string (e.g., "mean") that will be 
#                passed to exact_extract via its 'fun' argument, or NULL. When not NULL, 
#                exact_extract returns an aggregated (single) value per hour. When NULL, 
#                exact_extract returns a list of data frames (one per feature in the input 
#                shapefile), and the function will add a column 'feature_index' to identify 
#                each grid cell.
# @Output      : A data frame with Date, Hour, and the aerosol variables. When extraction_fun 
#                is not NULL, a single row per hour is returned (aggregated over the shapefile).
#                When extraction_fun is NULL, the output includes one row per grid per hour,
#                including a 'feature_index' column.
# @Purpose     : Processes MERRA-2 .nc4 files for a given area by extracting aerosol variables 
#                at hourly resolution. The extraction can be performed either in an aggregated 
#                manner (using an aggregation function such as "mean") or on a per-grid (per-
#                feature) basis.
# @Written_on  : 02/12/2024
# @Written_by  : Marcos Paulo
process_merra2_city_hourly_parallel <- function(shapefile,
                                                nc_files, 
                                                city_name, 
                                                num_cores = NULL,
                                                extraction_fun = "mean") {
  
  # Record the start time
  start_time <- Sys.time()
  
  # If an aggregation function is provided, union the shapefile so that it is a single feature.
  if (!is.null(extraction_fun)) {
    shapefile <- sf::st_union(shapefile)
  }
  
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
                          .packages = c("terra", "sf", "exactextractr")
                          ) %dopar% {
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
                              shapefile_proj <- st_transform(shapefile, crs = crs(nc_data))
                              
                              # Crop and mask the raster to the shapefile extent
                              nc_data_cropped <- terra::crop(nc_data,
                                                             vect(shapefile_proj), snap = "out")
                              nc_data_masked  <- terra::mask(nc_data_cropped,
                                                             vect(shapefile_proj))
                              
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
                                  # Layer corresponding to the variable and current hour
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
                                combined_rast <- terra::rast(hourly_layers)
                                
                                if (!is.null(extraction_fun)) {
                                  # Use exact_extract with the aggregation function when != NULL
                                  extraction <- exactextractr::exact_extract(
                                    combined_rast,
                                    shapefile,
                                    fun = extraction_fun)
                                  
                                  # The returned object (for a single polygon) is a named vector.
                                  aggregated_values <- extraction
                                  
                                  # Remove the prefix (e.g., "mean.") from the names
                                  names(aggregated_values) <- sub(
                                    paste0('^', extraction_fun, '\\.'),
                                    '',
                                    names(aggregated_values)
                                    )
                                  
                                  # Create a data frame with one row for the current hour
                                  hourly_data <- data.frame(
                                    Date = date,
                                    Hour = hour - 1,  # Adjust hour to start from 0
                                    DUSMASS25 = aggregated_values["DUSMASS25"],
                                    OCSMASS   = aggregated_values["OCSMASS"],
                                    BCSMASS   = aggregated_values["BCSMASS"],
                                    SSSMASS25 = aggregated_values["SSSMASS25"],
                                    SO4SMASS  = aggregated_values["SO4SMASS"],
                                    stringsAsFactors = FALSE
                                  )
                                } else {
                                  # When extraction_fun is NULL:
                                  # exact_extract returns a list of data frames (1 per feature).
                                  # Include_cell = TRUE so that each grid cell is identified.
                                  extraction <- exactextractr::exact_extract(
                                    combined_rast,
                                    shapefile,
                                    include_cell = TRUE,
                                    fun = NULL)
                                  
                                  # Ensure we have a list (one element per feature)
                                  if (is.list(extraction)) {
                                    extraction_list <- lapply(seq_along(extraction),
                                                              function(i) {
                                      df_ex <- extraction[[i]]
                                      df_ex$feature_index <- i
                                      df_ex
                                    })
                                    extraction_df <- do.call(rbind, extraction_list)
                                  } else {
                                    extraction_df <- extraction
                                    extraction_df$feature_index <- 1
                                  }
                                  # Add Date and Hour information to each row
                                  extraction_df$Date <- date
                                  extraction_df$Hour <- hour - 1
                                  # Keep only the desired columns:
                                  cols_to_keep <- c("Date",
                                                    "Hour", 
                                                    "feature_index",
                                                    vars_to_extract)
                                  hourly_data  <- extraction_df[,
                                                                intersect(cols_to_keep,
                                                                          names(extraction_df)),
                                                                drop = FALSE
                                                                ]
                                }
                                
                                # Add the hourly data (may have 1 or more rows) to results list
                                hourly_results[[length(hourly_results) + 1]] <- hourly_data
                              }
                              
                              # Join the hourly results into a daily df for the current file
                              if (length(hourly_results) > 0) {
                                daily_results <- do.call(rbind, hourly_results)
                                return(daily_results)
                              } else {
                                return(NULL)
                              }
                            })
                          }
  
  # Stop the cluster
  parallel::stopCluster(cl)
  
  # Record the end time and calculate total processing time
  end_time <- Sys.time()
  total_time <- end_time - start_time
  
  # Remove NULL results (files that were skipped)
  results_list <- Filter(Negate(is.null), results_list)
  
  # Combine the results into a single data frame
  results <- do.call(rbind, results_list)
  
  # Ensure the Date column is of Date type
  results$Date <- as.Date(results$Date, origin = "1970-01-01")
  
  # Sort the results by Date and Hour
  results <- results[order(results$Date, results$Hour), ]
  
  # Remove row names from the final data frame
  rownames(results) <- NULL
  
  # Display the total processing time
  cat("Total processing time for", city_name, ":", total_time, " min.\n")
  
  # Return the final data frame
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
#                to µg m^-2. Then, it calculates the PM2.5 estimate based on the formula:
#                PM2.5 = DUSMASS25 + OCSMASS + BCSMASS + SSSMASS25 + (SO4SMASS * 132.14/96.06)
#                The final PM2.5 column will also be in µg m^-3.
# @Written_on  : 13/12/2024
# @Written_by  : Marcos Paulo
convert_and_add_pm25 <- function(df, new_column_name = "pm25_estimate") {
  # Check if required columns exist
  required_cols <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  if (!all(required_cols %in% names(df))) {
    stop("The data frame must contain the following columns: 
         DUSMASS25, OCSMASS, BCSMASS, SSSMASS25, SO4SMASS.")}
  
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
# @Arg         : user_pm_data (data frame of hourly data for one country, containing 
#                columns "Date" (as a Date object) and "pm25_estimate" in µg/m^3).
# @Arg         : nasa_monthly_data (data frame of monthly data from NASA, containing:
#                - a "date" column in YYYY-MM format (e.g. "2023-01")
#                - a column named exactly as the country, e.g. "Brazil").
# @Arg         : country_name (string, e.g., "Brazil"), matching column in nasa_monthly_data.
# @Output      : A data frame with three columns: "country", "my_pm25", and "nasa_pm25".
# @Purpose     : This function computes monthly means from your hourly PM2.5, then merges 
#                with NASA's monthly PM2.5, producing a simple comparison table.
# @Written_on  : 01/02/2025
# @Written_by  : Marcos Paulo
compare_pm25_to_nasa <- function(user_pm_data,
                                 nasa_monthly_data, 
                                 country_name) {

  # 1) Aggregate your data (hourly) to monthly
  monthly_user_pm <- user_pm_data %>%
    # Extract YYYY-MM from the Date
    dplyr::mutate(year_month = format(Date, "%Y-%m")) %>%
    dplyr::group_by(year_month) %>%
    # Get the mean monthly value
    dplyr::summarize(
      my_pm25 = mean(pm25_estimate, na.rm = TRUE),
      .groups = "drop")
  
  # Prepare NASA data
  #    - The NASA dataset has a "date" column and a column as the country (e.g., "Brazil").
  #    - Rename that country column to "nasa_pm25" for clarity.
  #    - Also create a matching "year_month" for the join.
  monthly_nasa <- nasa_monthly_data %>%
    dplyr::rename(nasa_pm25 = !!country_name) %>%
    # Convert "YYYY-MM" to a Date, then extract the same format
    dplyr::mutate(year_month = format(as.Date(paste0(date, "-01")), "%Y-%m"))
  
  # Merge your monthly PM with NASA’s monthly PM by "year_month"
  merged_data <- dplyr::inner_join(monthly_user_pm, monthly_nasa, by = "year_month")
  
  # Add a 'country' column and select only the three requested columns
  merged_data$country <- country_name
  
  # Create final data
  final_data <- merged_data %>%
    dplyr::select(country, my_pm25, nasa_pm25)
  
  return(final_data)
}
