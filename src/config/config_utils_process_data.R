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
# @Arg         : shapefile is an 'sf' object representing the boundary of the region 
#                (or any polygon collection for which grid‐level values are desired).
# @Arg         : nc_files is a vector of file paths to the .nc4 files.
# @Arg         : region_name is a string with the name of the region (or country).
# @Arg         : num_cores is the number of CPU cores to use for parallel processing. 
#                If NULL, one less than the total available cores is used.
# @Arg         : extraction_fun is either a character string (e.g., "mean") that will be 
#                passed to exact_extract via its 'fun' argument, or NULL. When not NULL, 
#                exact_extract returns an aggregated (single) value per hour. When NULL, 
#                exact_extract returns a list of data frames (one per feature in the input 
#                shapefile) and the function will add a column 'feature_index' to identify 
#                each grid cell.
# @Arg         : parallel is a logical indicating whether to attempt parallel processing.
#                When TRUE, the function checks if available RAM is > 30GB. Otherwise,
#                processing is sequential.
# @Output      : A data frame with Date, Hour, and the aerosol variables. When extraction_fun 
#                is not NULL, a single row per hour is returned (aggregated over the shapefile).
#                When extraction_fun is NULL, the output includes one row per grid (feature) 
#                per hour, including a 'feature_index' column.
# @Purpose     : Processes MERRA-2 .nc4 files for a given region by extracting aerosol variables 
#                at hourly resolution. The extraction can be done in an aggregated manner (e.g.,
#                using "mean") or on a per-grid basis. The function automatically selects
#               parallel or sequential processing based on user input and available system RAM.
# @Written_on  : 02/12/2024
# @Written_by  : Marcos Paulo
process_merra2_region_hourly <- function(shapefile,
                                         nc_files, 
                                         region_name, 
                                         num_cores = NULL,
                                         extraction_fun = "mean",
                                         parallel = TRUE) {
  
  # Record the starting time
  start_time <- Sys.time()
  
  # If an aggregation function is provided, union the shapefile so it becomes a single polygon.
  # This ensures that the aggregation (e.g., "mean") returns one value per hour.
  if (!is.null(extraction_fun)) {
    shapefile <- sf::st_union(shapefile)
  }
  
  # Decide whether to run in parallel or sequentially.
  run_parallel <- FALSE
  if (parallel) {
    # Retrieve total RAM in bytes, then convert to GB
    ram_gb <- memuse::Sys.meminfo()[1]$totalram@size
    # Run in parallel only if system has more than 30GB RAM.
    run_parallel <- (ram_gb > 30)
    if (run_parallel) {
      cat("System has more than 30GB RAM -> using parallel processing.\n")
    } else {
      cat("System does not have enough RAM for parallel processing -> running sequentially.\n")
    }
  } else {
    cat("Parallel set to FALSE -> running sequentially.\n")
  }
  
  # Define the aerosol variable names to extract
  vars_to_extract <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  
  # Internal function to process a single netCDF file
  process_one_file <- function(nc_file) {
    suppressWarnings({
      # Extract the date from the file name using a regex pattern
      file_name <- basename(nc_file)
      date_str <- sub(".*\\.(\\d{8})\\.nc4$", "\\1", file_name)
      date <- as.Date(date_str, format = "%Y%m%d")
      
      # Load the netCDF file as a raster using the terra package
      nc_data <- terra::rast(nc_file)
      
      # Set the coordinate reference system (CRS) to WGS84 (EPSG:4326)
      terra::crs(nc_data) <- "EPSG:4326"
      
      # Reproject the shapefile to match the raster's CRS
      shapefile_proj <- sf::st_transform(shapefile, crs = terra::crs(nc_data))
      
      # Crop the raster to the extent of the shapefile and then mask it to the shapefile's shape
      nc_data_cropped <- terra::crop(nc_data, terra::vect(shapefile_proj), snap = "out")
      nc_data_masked <- terra::mask(nc_data_cropped, terra::vect(shapefile_proj))
      
      # Identify available layers and check for the required aerosol variables.
      available_vars <- names(nc_data_masked)
      vars_present <- lapply(vars_to_extract, function(var) {
        grep(paste0("^", var, "_"), available_vars, value = TRUE)
      })
      names(vars_present) <- vars_to_extract
      
      # If any required variable is missing, return NULL for this file.
      if (any(sapply(vars_present, length) == 0)) {
        return(NULL)
      }
      
      # Initialize a list to collect results for each hour (assumes 24 hours per day)
      hourly_results <- list()
      for (hour in 1:24) {
        # Initialize list for the layers corresponding to the current hour.
        hourly_layers <- list()
        for (var_name in vars_to_extract) {
          layer_name <- paste0(var_name, "_", hour)
          if (layer_name %in% available_vars) {
            var_layer <- nc_data[[layer_name]]
            hourly_layers[[var_name]] <- var_layer
          } else {
            # If one layer is missing, skip this hour.
            next
          }
        }
        # Check that all variables have been found for this hour.
        if (length(hourly_layers) != length(vars_to_extract)) {
          next
        }
        
        # Combine the selected layers into a single raster stack.
        combined_rast <- terra::rast(hourly_layers)
        
        # If an aggregation function is provided, perform aggregated extraction.
        if (!is.null(extraction_fun)) {
          extraction <- exactextractr::exact_extract(combined_rast,
                                                     shapefile,
                                                     fun = extraction_fun)
          aggregated_values <- extraction  # Should be a named vector
          # Remove any prefix from the names (e.g., "mean.")
          names(aggregated_values) <- sub(paste0('^', extraction_fun, '\\.'),
                                          '',
                                          names(aggregated_values))
          # Create a one-row data frame for the current hour.
          hourly_data <- data.frame(
            Date = date,
            Hour = hour - 1,  # Adjust so that hour 1 becomes 0, etc.
            DUSMASS25 = aggregated_values["DUSMASS25"],
            OCSMASS   = aggregated_values["OCSMASS"],
            BCSMASS   = aggregated_values["BCSMASS"],
            SSSMASS25 = aggregated_values["SSSMASS25"],
            SO4SMASS  = aggregated_values["SO4SMASS"],
            stringsAsFactors = FALSE
          )
        } else {
          # When extraction_fun is NULL, extract per grid cell values.
          extraction <- exactextractr::exact_extract(combined_rast,
                                                     shapefile,
                                                     include_cell = TRUE,
                                                     fun = NULL)
          if (is.list(extraction)) {
            # Add a feature index to identify each grid cell.
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
          # Add date and adjusted hour information.
          extraction_df$Date <- date
          extraction_df$Hour <- hour - 1
          cols_to_keep <- c("Date", "Hour", "feature_index", vars_to_extract)
          hourly_data <- extraction_df[,
                                       intersect(cols_to_keep, names(extraction_df)),
                                       drop = FALSE
                                       ]
        }
        # Append the hourly data to the results list.
        hourly_results[[length(hourly_results) + 1]] <- hourly_data
      }
      # Combine all hourly data frames into one data frame for the file.
      if (length(hourly_results) > 0) {
        daily_results <- do.call(rbind, hourly_results)
        return(daily_results)
      } else {
        return(NULL)
      }
    })
  }  # End process_one_file()
  
  # Process all files using either parallel or sequential methods.
  if (run_parallel) {
    # Determine number of cores if not provided.
    if (is.null(num_cores)) {
      num_cores <- parallel::detectCores() - 1
    }
    # Set up a parallel cluster.
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    results_list <- foreach::foreach(nc_file = nc_files,
                                     .packages = c("terra", "sf", "exactextractr")) %dopar% {
                                       process_one_file(nc_file)
                                     }
    parallel::stopCluster(cl)
    results_list <- Filter(Negate(is.null), results_list)
    results <- do.call(rbind, results_list)
  } else {
    results_list <- lapply(nc_files, process_one_file)
    results_list <- Filter(Negate(is.null), results_list)
    results <- do.call(rbind, results_list)
  }
  
  # Ensure that the Date column is of type Date and sort the results.
  results$Date <- as.Date(results$Date, origin = "1970-01-01")
  results <- results[order(results$Date, results$Hour), ]
  rownames(results) <- NULL
  
  # Calculate and print total processing time.
  end_time <- Sys.time()
  total_time <- end_time - start_time
  cat("Total processing time for", region_name, ":", total_time, " min.\n")
  
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
