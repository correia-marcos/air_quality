# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
# 
# @Description: This script sets up the necessary environment by checking and installing 
# required packages and defining utility functions for all "process_data" scripts.
# 
# @Date: Nov 2024
# @Author: Marcos Paulo
# ============================================================================================
# List of required packages
pkgs <- c(
  "arrow",
  "archive",
  "censobr",
  "DBI",
  "dplyr",
  "doParallel",
  "duckdb",
  "exactextractr",
  "foreach",
  "here",
  "janitor",
  "lubridate",
  "memuse",
  "readr",
  "rio",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "stringi",
  "terra",
  "tibble",
  "tidyr",
  "tools",
  "rlang",
  "XLConnect",
  "XML")

# Strict check: fail fast if something isn't in the project library
ensure_installed <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    message(
      "Missing packages: ", paste(miss, collapse = ", "),
      ". Run renv::restore() (or install locally with renv::install() then renv::snapshot())."
    )
    renv::install(miss)}
}

ensure_installed(pkgs)

# Attach (quiet)
invisible(lapply(pkgs, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# no repo tweaking, no renv::install() here
rm(pkgs, ensure_installed)

# ############################################################################################
# Functions
# ############################################################################################

# --------------------------------------------------------------------------------------------
# Function: process_merra2_region_hourly
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
# --------------------------------------------------------------------------------------------
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
  cat("Total processing time for", region_name, ":", total_time, " .\n")
  
  return(results)
}


# --------------------------------------------------------------------------------------------
# Function: convert_and_add_pm25
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
# --------------------------------------------------------------------------------------------
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


# --------------------------------------------------------------------------------------------
# Function: compare_pm25_to_nasa
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
# --------------------------------------------------------------------------------------------
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
      IDB_pm25 = mean(pm25_estimate, na.rm = TRUE),
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
    dplyr::select(country, year_month, IDB_pm25, nasa_pm25)
  
  return(final_data)
}


# --------------------------------------------------------------------------------------------
# Function: generate_region_comparison
# @Arg         : shapefile is an 'sf' object containing boundaries for all regions.
# @Arg         : filter_field is a string specifying the column name in the shapefile used 
#                to identify a region (e.g., "sov_a3" or "admin").
# @Arg         : filter_value is a string with the value in filter_field corresponding to the 
#                desired region (e.g., "BRA" for Brazil or "Argentina" for Argentina).
# @Arg         : region_name is a string with the full name of the region (or country) used 
#                for labeling in the output (e.g., "Brazil" or "Argentina").
# @Arg         : nc_files is a vector of file paths to the MERRA-2 .nc4 files.
# @Arg         : nasa_monthly_data is a dataframe with monthly PM2.5 values from NASA. It should
#                contain a "date" column (in "YYYY-MM" format) and a column named exactly as
#                region_name.
# @Arg         : num_cores is the number of CPU cores to use for parallel processing (default: 
#                NULL, which uses one less than the total available cores).
# @Arg         : extraction_fun is either a character string (e.g., "mean") for aggregated
#                extraction, or NULL to return grid-level values.
# @Arg         : parallel is a logical indicating whether to attempt parallel processing.
#                When TRUE, the function checks if available RAM is >30GB before running in
#                parallel.
# @Output      : A data frame with columns "country", "year_month", "IDB_pm25" (the region's 
#                monthly PM2.5 estimate from your MERRA-2 processing) and "nasa_pm25" (NASA's
#                monthly PM2.5 value).
# @Purpose     : This function automates the creation of a monthly PM2.5 comparison panel for a 
#                given region. It filters the shapefile, processes MERRA-2 netCDF files to
#                generate hourly data, converts the data to monthly averages, and then merges
#                the result with NASA's PM2.5 data.
# @Written_on  : 01/02/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
generate_region_comparison <- function(shapefile, 
                                       filter_field = "sov_a3", 
                                       filter_value, 
                                       region_name, 
                                       nc_files, 
                                       nasa_monthly_data,
                                       num_cores = NULL,
                                       extraction_fun = "mean",
                                       parallel = TRUE) {

  # Filter the shapefile for the desired region.
  region_shapefile <- shapefile %>% 
    dplyr::filter(!!sym(filter_field) == filter_value)
  
  # Process the netCDF files to generate the region's hourly panel using MERRA-2 data.
  region_results <- process_merra2_region_hourly(
    shapefile      = region_shapefile,
    nc_files       = nc_files,
    region_name    = region_name,
    num_cores      = num_cores,
    extraction_fun = extraction_fun,
    parallel       = parallel
  )
  
  # Convert the results to add a PM2.5 measurement column (in µg/m³).
  region_pm25    <- convert_and_add_pm25(region_results)
  
  # Compare your PM2.5 estimates with NASA's monthly PM2.5 data.
  comparison_panel <- compare_pm25_to_nasa(
    user_pm_data      = region_pm25,
    nasa_monthly_data = nasa_monthly_data,
    country_name      = region_name)
  
  return(comparison_panel)
}


# --------------------------------------------------------------------------------------------
# Function: combine_station_merra2_pm25
# @Arg         : station_df is a data frame containing ground station PM2.5 measurements
# @Arg         : station_datetime_col is a string with the name of the column in station_df 
#                that stores the date-time information (e.g., "datetime"). 
# @Arg         : station_pm25_col is a string with the name of the PM2.5 column in station_df
# @Arg         : merra2_df is a data frame representing MERRA-2 data, which must have columns:
#                "Date", "Hour", and "pm25_estimate".
# @Output      : A data frame with the following columns:
#                - "Date"          : The date (in YYYY-MM-DD format)
#                - "Hour"          : The hour of the day (0 to 23)
#                - "pm25_stations" : The averaged PM2.5 values from the ground station data
#                - "pm25_merra2"   : The PM2.5 estimates from the MERRA-2 data
# @Purpose     : This function merges hourly ground station PM2.5 data with MERRA-2 PM2.5 
#                estimates by matching on date and hour. The ground station data may contain 
#                multiple stations in the same city; the function computes an average PM2.5 for 
#                each hour across all stations.
# @Written_on  : 13/02/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
combine_station_merra2_pm25 <- function(station_df,
                                        station_datetime_col = "datetime",
                                        station_pm25_col     = "pm25",
                                        merra2_df) {
  # Convert the date-time column to POSIXct and extract Date & Hour
  station_processed <- station_df %>%
    mutate(
      temp_datetime = as.POSIXct(.data[[station_datetime_col]]),  # Convert to POSIXct
      Date          = as.Date(temp_datetime),                     # Extract the date
      Hour          = hour(temp_datetime)                         # Extract the hour
    ) %>%
    # Group by Date and Hour to average PM2.5 across all stations
    group_by(Date, Hour) %>%
    summarize(
      pm25_stations = mean(.data[[station_pm25_col]], na.rm = TRUE),
      .groups       = "drop"
    )
  
  # Prepare MERRA-2 data by renaming the PM2.5 column
  merra2_processed <- merra2_df %>%
    rename(pm25_merra2 = pm25_estimate) %>% 
    mutate(Date = as.Date(Date)) %>% 
    select(Date, Hour, pm25_merra2)
  
  # Join on Date and Hour to create the final merged data frame
  combined_data <- left_join(merra2_processed, station_processed, by = c("Date", "Hour"))
  
  return(combined_data)
}


# --------------------------------------------------------------------------------------------
# Function: aggregate_and_correlate
# @Arg         : df is a data frame that results from combining ground station and 
#                MERRA-2 data. It must contain at least the following columns:
#                - "Date"          : Date (in YYYY-MM-DD format)
#                - "pm25_merra2"   : PM2.5 estimates from MERRA-2 data
#                - "pm25_stations" : Averaged ground station PM2.5 values
# @Arg         : time_scale is a string indicating the aggregation level. It must be one of:
#                "hourly", "daily", or "monthly". 
#                - "hourly": Use the data as-is (assumes one row per hour).
#                - "daily": Average over each day.
#                - "monthly": Average over each month.
# @Output      : A numeric value representing the Pearson correlation between 
#                pm25_merra2 and pm25_stations for the specified time scale.
# @Purpose     : Aggregates the merged data to a desired time scale and computes 
#                the correlation between the MERRA-2 and ground station PM2.5 values.
# @Written_on  : 14/02/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
aggregate_and_correlate <- function(df, time_scale = "daily") {

  if (time_scale == "hourly") {
    # Use the raw data (assumes one row per hour)
    agg_df <- df
  } else if (time_scale == "daily") {
    # Group by Date and average the PM2.5 values
    agg_df <- df %>%
      group_by(Date) %>%
      summarize(
        pm25_merra2 = mean(pm25_merra2, na.rm = TRUE),
        pm25_stations = mean(pm25_stations, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (time_scale == "monthly") {
    # Create a year-month variable and group by it
    agg_df <- df %>%
      mutate(year_month = format(Date, "%Y-%m")) %>%
      group_by(year_month) %>%
      summarize(
        pm25_merra2 = mean(pm25_merra2, na.rm = TRUE),
        pm25_stations = mean(pm25_stations, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    stop("Invalid time scale. Choose 'hourly', 'daily', or 'monthly'.")
  }
  
  # Compute and return the Pearson correlation using pairwise complete observations
  cor_val <- cor(agg_df$pm25_merra2, agg_df$pm25_stations, use = "pairwise.complete.obs")
  return(cor_val)
}


# --------------------------------------------------------------------------------------------
# Function: compute_correlations_for_cities
# @Arg         : city_dfs is a named list of merged data frames (one per city) produced by
#                combine_station_merra2_pm25(). Each data frame must contain the columns:
#                "Date", "pm25_merra2", and "pm25_stations".
# @Arg         : timescales is a character vector specifying the aggregation levels to test.
#                Default is c("hourly", "daily", "monthly").
# @Output      : A data frame with columns "City", "Time_Scale", and "Correlation", containing
#                the Pearson correlation between pm25_merra2 and pm25_stations for each city and
#                each specified time scale.
# @Purpose     : This function computes the correlation between MERRA-2 and ground station PM2.5
#                measurements at different time scales for a collection of cities.
# @Written_on  : 14/02/2025
# @Written_by  : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_correlations_for_cities <- function(city_dfs,
                                            timescales = c("hourly", "daily", "monthly")) {
  
  # Create new empty data frame 
  results <- data.frame(City = character(), Time_Scale = character(), Correlation = numeric(), 
                        stringsAsFactors = FALSE)
  
  # Adding values using nested for loops
  for (city in names(city_dfs)) {
    df <- city_dfs[[city]]
    for (ts in timescales) {
      cor_val <- aggregate_and_correlate(df, time_scale = ts)
      results <- rbind(results, data.frame(City = city, Time_Scale = ts, Correlation = cor_val,
                                           stringsAsFactors = FALSE))
    }
  }
  return(results)
}


# --------------------------------------------------------------------------------------------
# Function: compute_distance_matrices
#
# @Arg stations_sf          : sf POINT object; monitoring stations.
# @Arg station_id_col       : string; column in stations_sf with station IDs.
# @Arg geo_sf               : sf POLYGON object or NULL; geographic units.
# @Arg geo_id_col           : string or NULL; unique ID column in geo_sf.
# @Arg out_dir              : string; output directory.
# @Arg out_name             : string; prefix, e.g. "bogota_2018".
# @Arg distance_metric      : string; "aeqd", "haversine", "euclidean",
#                             or "geosphere".
# @Arg representative_point : string; "point_on_surface" or "math_centroid".
# @Arg overwrite            : logical; skip if output exists. Default TRUE.
# @Arg quiet                : logical; suppress messages. Default FALSE.
#
# @Output : Named list of data.tables for station and geo distances.
# @Details:
#   Calculates station-to-station and geo-to-station distance matrices.
#   If representative_point = "point_on_surface", it uses an internal point
#   returned by st_point_on_surface() for every geographic unit.
#   If representative_point = "math_centroid", it uses st_centroid() with an
#   internal-point fallback for polygons whose centroid is outside.
#   The "geosphere" metric is included for legacy replication because the old
#   station-distance scripts used geosphere::distm(..., fun = distHaversine).
# @Written_on : 01/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_distance_matrices <- function(
    stations_sf,
    station_id_col,
    geo_sf               = NULL,
    geo_id_col           = NULL,
    out_dir,
    out_name,
    distance_metric      = c("aeqd", "haversine", "euclidean", "geosphere"),
    representative_point = c("point_on_surface", "math_centroid"),
    overwrite            = TRUE,
    quiet                = FALSE
) {
  
  # Match the requested distance and representative-point methods first
  dist_metric <- match.arg(distance_metric)
  representative_point <- match.arg(representative_point)
  
  # Check for required packages before running
  pkgs <- c("sf", "data.table", "arrow", "stringi")
  
  # geosphere is only needed for the legacy metric
  if (dist_metric == "geosphere") {
    pkgs <- c(pkgs, "geosphere")
  }
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required. Add to renv.")
    }
  }
  
  # Normalize station IDs: uppercase, strip accents and quotes
  .normalize <- function(x) {
    x <- toupper(trimws(x))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  # Build an AEQD proj4 string centred on a given WGS84 lon/lat
  .aeqd_proj <- function(lon0, lat0) {
    sprintf(
      "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
      lat0, lon0
    )
  }
  
  # Create one representative point per polygon
  .representative_points <- function(poly, method) {
    
    # Use guaranteed internal points for all polygons
    if (method == "point_on_surface") {
      return(suppressWarnings(sf::st_point_on_surface(poly)))
    }
    
    # Calculate mathematical centroids first
    cents <- suppressWarnings(
      sf::st_centroid(poly, of_largest_polygon = TRUE)
    )
    
    # Check whether centroid i intersects polygon i
    inside_mat <- suppressWarnings(
      sf::st_intersects(cents, poly, sparse = FALSE)
    )
    
    # Extract diagonal relation: point i versus polygon i
    is_inside <- as.logical(
      inside_mat[cbind(seq_len(nrow(poly)), seq_len(nrow(poly)))]
    )
    
    # Replace external centroids with guaranteed internal points
    if (any(!is_inside, na.rm = TRUE)) {
      bad_idx <- which(!is_inside)
      
      cents[bad_idx, ] <- suppressWarnings(
        sf::st_point_on_surface(poly[bad_idx, ])
      )
    }
    return(cents)
  }
  
  # Calculate geosphere Haversine distances from two sf POINT objects
  .geosphere_distance_km <- function(from_sf, to_sf) {
    # geosphere expects longitude-latitude coordinates in WGS84
    from_wgs <- sf::st_transform(from_sf, crs = 4326)
    to_wgs   <- sf::st_transform(to_sf,   crs = 4326)
    
    # Extract coordinates as lon-lat matrices
    from_xy <- sf::st_coordinates(from_wgs)[, c("X", "Y"), drop = FALSE]
    to_xy   <- sf::st_coordinates(to_wgs)[,   c("X", "Y"), drop = FALSE]
    
    # geosphere::distm returns meters
    dist_m <- geosphere::distm(
      x   = from_xy,
      y   = to_xy,
      fun = geosphere::distHaversine
    )
    # Convert meters to kilometers
    return(as.numeric(dist_m) / 1000)
  }
  # Validate stations input
  if (!inherits(stations_sf, "sf")) {
    stop("`stations_sf` must be an sf object.")
  }
  # Validate station column exists
  if (!station_id_col %in% names(stations_sf)) {
    stop("Column '", station_id_col, "' not found.")
  }
  # Validate geographic data if provided
  if (!is.null(geo_sf)) {
    if (!inherits(geo_sf, "sf")) {
      stop("`geo_sf` must be an sf object.")
    }
    if (is.null(geo_id_col)) {
      stop("`geo_id_col` is required.")
    }
    if (!geo_id_col %in% names(geo_sf)) {
      stop("Column '", geo_id_col, "' not found.")
    }
  }
  
  # Create directory if it does not exist
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    
    if (!quiet) {
      message("Created output directory: ", out_dir)
    }
  }
  
  # Define file paths for output Parquet files
  path_sta <- file.path(out_dir, paste0(out_name, "_station_distances.parquet"))
  path_geo <- file.path(out_dir, paste0(out_name, "_geo_station_distances.parquet"))
  
  # Check if files already exist to skip computation
  geo_ready <- is.null(geo_sf) || file.exists(path_geo)
  
  if (!overwrite && file.exists(path_sta) && geo_ready) {
    if (!quiet) {
      message("Files exist and overwrite = FALSE.")
    }
    
    return(invisible(list(
      station_matrix = data.table::as.data.table(
        arrow::read_parquet(path_sta)
      ),
      geo_station_matrix = if (!is.null(geo_sf)) {
        data.table::as.data.table(arrow::read_parquet(path_geo))
      } else {
        NULL
      }
    )))
  }
  
  if (!quiet) {
    message("[", out_name, "] Metric: ", dist_metric)
    message("[", out_name, "] Representative point: ", representative_point)
  }
  
  # Enforce standard WGS84 and keep only the ID column to save memory
  stations_wgs <- sf::st_transform(stations_sf, crs = 4326)
  stations_wgs <- stations_wgs[, station_id_col]
  
  # Save initial S2 setting to restore later
  s2_initial <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(s2_initial), add = TRUE)
  
  # Disable spherical geometry only for the simple Euclidean approximation
  if (dist_metric == "euclidean") {
    sf::sf_use_s2(FALSE)
  }
  
  # Handle projection based on selected metric
  if (dist_metric == "aeqd") {
    
    # Calculate center of all stations to center the AEQD
    cen <- sf::st_coordinates(
      sf::st_centroid(sf::st_union(stations_wgs))
    )
    # Build local equidistant projection
    proj_aeqd <- .aeqd_proj(lon0 = cen[1, "X"], lat0 = cen[1, "Y"])
    stations_eval <- sf::st_transform(stations_wgs, crs = proj_aeqd)
  } else {
    # Keep WGS84 for haversine, geosphere, and euclidean
    stations_eval <- stations_wgs
  }
  # Extract and normalize station IDs
  station_ids <- .normalize(as.character(stations_wgs[[station_id_col]]))
  n_sta <- length(station_ids)
  
  if (!quiet) {
    message("[", out_name, "] Station distances...")
  }
  
  # Calculate station-to-station distances
  if (dist_metric == "geosphere") {
    dist_sta_km <- .geosphere_distance_km(stations_wgs, stations_wgs)
  } else {
    dist_sta_raw <- as.numeric(
      sf::st_distance(stations_eval, stations_eval)
    )
    
    dist_sta_km <- if (dist_metric == "euclidean") {
      dist_sta_raw * 111.32
    } else {
      dist_sta_raw / 1000
    }
  }
  
  # Build data table with combinations
  station_dt <- data.table::data.table(
    station_from = rep(station_ids, times = n_sta),
    station_to   = rep(station_ids, each  = n_sta),
    distance_km  = dist_sta_km
  )

  # Save station matrix
  if (!quiet) {message("[", out_name, "] Writing: ", path_sta)}
  
  arrow::write_parquet(station_dt, path_sta)
  
  geo_station_dt <- NULL
  
  # Calculate geo-to-station matrix if geo_sf is provided
  if (!is.null(geo_sf)) {
    if (!quiet) {
      message("[", out_name, "] Geo distances...")
    }
    
    # Transform geo units to WGS84 and fix invalid polygons
    geo_wgs <- sf::st_transform(sf::st_make_valid(geo_sf), crs = 4326)
    
    # Extract representative points using the requested method
    geo_points <- .representative_points(
      poly   = geo_wgs,
      method = representative_point
    )
    
    # Apply AEQD projection if required
    if (dist_metric == "aeqd") {
      geo_eval <- sf::st_transform(geo_points, crs = proj_aeqd)
    } else {
      geo_eval <- geo_points
    }
    
    # Extract geographic unit IDs
    geo_ids <- as.character(geo_points[[geo_id_col]])
    
    # Calculate distances from representative points to stations
    if (dist_metric == "geosphere") {
      dist_geo_km <- .geosphere_distance_km(geo_points, stations_wgs)
    } else {
      dist_geo_raw <- as.numeric(
        sf::st_distance(geo_eval, stations_eval)
      )
      
      dist_geo_km <- if (dist_metric == "euclidean") {
        dist_geo_raw * 111.32
      } else {
        dist_geo_raw / 1000
      }
    }
    
    # Build data table for geo distances
    geo_station_dt <- data.table::data.table(
      geo_id      = rep(geo_ids,     times = n_sta),
      station_id  = rep(station_ids, each  = nrow(geo_sf)),
      distance_km = dist_geo_km
    )
    
    # Save geo matrix
    if (!quiet) {
      message("[", out_name, "] Writing: ", path_geo)
    }

    arrow::write_parquet(geo_station_dt, path_geo)
  }
  
  # Return both matrices invisibly
  invisible(list(
    station_matrix     = station_dt,
    geo_station_matrix = geo_station_dt
  ))
}


# --------------------------------------------------------------------------------------------
# Function: detect_pollution_outliers
#
# @Arg arrow_dir           : string; path to Arrow dataset of hourly data.
# @Arg station_dist_path   : string; station_distances.parquet path.
# @Arg out_dir             : string; output directory.
# @Arg out_name            : string; prefix, e.g. "bogota_2018".
# @Arg pollutants          : character; default c("pm10", "pm25").
# @Arg pct_flag            : numeric [0,1]; upper-tail quantile. Default 0.99.
# @Arg n_sd                : numeric; tolerance half-width in SD units. Default 2.
# @Arg on_missing_temporal : string; "finish" or "continue". Default "continue".
# @Arg on_missing_neighbor : string; "finish" or "second". Default "second".
# @Arg overwrite           : logical; skip if output exists. Default TRUE.
# @Arg quiet               : logical; suppress messages. Default FALSE.
#
# @Details:
#   Creates `{pollutant}_outlier_reason` columns:
#     0 = Valid or not flagged
#     1 = Flagged, no temporal benchmark, no feasible spatial rescue
#     2 = Flagged, failed temporal, no feasible spatial rescue
#     3 = Flagged, failed temporal, failed spatial
#     4 = Flagged, no temporal benchmark, failed spatial
#
#   Also creates diagnostic count columns:
#     `{pollutant}_n_missing_temporal_sd`
#     `{pollutant}_n_zero_temporal_sd`
#     `{pollutant}_n_missing_spatial_sd`
#     `{pollutant}_n_zero_spatial_sd`
#
# @Written_on : 02/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
detect_pollution_outliers <- function(
    arrow_dir,
    station_dist_path,
    out_dir,
    out_name,
    pollutants          = c("pm10", "pm25"),
    pct_flag            = 0.99,
    n_sd                = 2,
    on_missing_temporal = "continue",
    on_missing_neighbor = "second",
    overwrite           = TRUE,
    quiet               = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  # stringi is required to harmonize station identifiers across files.
  pkgs <- c("arrow", "data.table", "dplyr", "stringi")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required.")
    }
  }
  
  # Validate behavioral options early to avoid silent mistakes.
  on_missing_temporal <- match.arg(
    on_missing_temporal,
    c("finish", "continue")
  )
  
  on_missing_neighbor <- match.arg(
    on_missing_neighbor,
    c("finish", "second")
  )
  
  # Normalize station IDs in the same way as the distance-matrix code.
  # This avoids failed joins due to accents, quotes, case, or whitespace.
  .normalize_station <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  # 1. Output path + early exit
  # -----------------------------------------------------------------------
  out_path <- file.path(out_dir, paste0(out_name, "_clean"))
  
  # Skip computation only when explicitly requested.
  if (!overwrite && dir.exists(out_path)) {
    if (!quiet) {
      message("Output exists; overwrite=FALSE — skipping.")
    }
    
    return(invisible(out_path))
  }
  
  # Create output root if needed.
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # Replace previous output when overwrite = TRUE.
  if (dir.exists(out_path)) {
    unlink(out_path, recursive = TRUE)
  }
  
  dir.create(out_path)
  
  # 2. Load and validate station-distance table
  # -----------------------------------------------------------------------
  # This table is used only to define nearest monitoring stations.
  dist_dt <- data.table::as.data.table(
    arrow::read_parquet(station_dist_path)
  )
  
  # Require the schema produced by compute_distance_matrices().
  req_dist_cols <- c("station_from", "station_to", "distance_km")
  miss_dist_cols <- setdiff(req_dist_cols, names(dist_dt))
  
  if (length(miss_dist_cols) > 0L) {
    stop("Distance table is missing: ", paste(miss_dist_cols, collapse = ", "))
  }
  
  # Harmonize station names in the distance table.
  dist_dt[, station_from := .normalize_station(station_from)]
  dist_dt[, station_to   := .normalize_station(station_to)]
  
  if (!quiet) {
    message("Distance table loaded.")
  }
  
  # 3. Open Arrow dataset and collect available years
  # -----------------------------------------------------------------------
  arrow_ds <- arrow::open_dataset(arrow_dir)
  
  years <- arrow_ds |>
    dplyr::select(year) |>
    dplyr::distinct()   |>
    dplyr::collect()    |>
    dplyr::pull(year)   |>
    sort()
  
  # 4. Inner helper: flag one pollutant at a time
  # -----------------------------------------------------------------------
  .flag_pollutant <- function(dt, pol, dist_dt, pct_flag, n_sd,
                              miss_temp, miss_neigh) {
    
    # Skip pollutant if it is absent in this city/year dataset.
    if (!pol %in% names(dt)) {
      return(invisible(NULL))
    }
    
    # Main output columns for this pollutant.
    flag_col   <- paste0(pol, "_outlier")
    reason_col <- paste0(pol, "_outlier_reason")
    
    # Diagnostic columns repeated within station-month cells.
    miss_tsd_col <- paste0(pol, "_n_missing_temporal_sd")
    zero_tsd_col <- paste0(pol, "_n_zero_temporal_sd")
    miss_ssd_col <- paste0(pol, "_n_missing_spatial_sd")
    zero_ssd_col <- paste0(pol, "_n_zero_spatial_sd")
    
    # Initialize classification outputs.
    dt[, (flag_col)   := 0L]
    dt[, (reason_col) := 0L]
    
    # Initialize diagnostics to zero.
    dt[, (miss_tsd_col) := 0L]
    dt[, (zero_tsd_col) := 0L]
    dt[, (miss_ssd_col) := 0L]
    dt[, (zero_ssd_col) := 0L]
    
    # Nothing to classify if the pollutant is entirely missing.
    if (all(is.na(dt[[pol]]))) {
      return(invisible(NULL))
    }
    
    # -- (1) Dynamic nearest neighbors ----------------------------------
    # Eligible neighbors must have at least one non-missing observation
    # for the pollutant in the current year-level working data.
    has_data <- dt[!is.na(get(pol)), unique(station)]
    
    # Restrict distance table to stations available for this pollutant.
    near_dt <- dist_dt[
      distance_km > 0 &
        station_from %in% has_data &
        station_to %in% has_data
    ]
    
    # Rank possible neighbors by distance within origin station.
    data.table::setorder(near_dt, station_from, distance_km)
    near_dt[, rank := seq_len(.N), by = station_from]
    
    # Attach closest eligible neighbor.
    near_1 <- near_dt[
      rank == 1L,
      .(station = station_from, near1 = station_to)
    ]
    
    dt[near_1, .t_near1 := i.near1, on = "station"]
    
    # Attach second closest eligible neighbor only when requested.
    if (miss_neigh == "second") {
      near_2 <- near_dt[
        rank == 2L,
        .(station = station_from, near2 = station_to)
      ]
      
      dt[near_2, .t_near2 := i.near2, on = "station"]
    }
    
    # -- (2) Lag, lead, and temporal differences -------------------------
    # The panel is balanced before this helper runs, so adjacent rows
    # represent adjacent hours within each station.
    dt[, `:=`(
      .t_lag  = data.table::shift(get(pol), 1L, type = "lag"),
      .t_lead = data.table::shift(get(pol), 1L, type = "lead")
    ), by = station]
    
    # First difference used to define normal temporal volatility.
    dt[, .t_diff := get(pol) - .t_lag]
    
    # Lookup table for simultaneous neighbor values.
    tmp_lkp <- dt[, .(station, datetime, tmp_p = get(pol))]
    
    # Pull simultaneous value at closest neighbor.
    dt[
      tmp_lkp,
      .t_vn1 := i.tmp_p,
      on = .(.t_near1 = station, datetime)
    ]
    
    dt[, .t_diff_nb1 := get(pol) - .t_vn1]
    
    # Pull simultaneous value at second closest neighbor when requested.
    if (miss_neigh == "second") {
      dt[
        tmp_lkp,
        .t_vn2 := i.tmp_p,
        on = .(.t_near2 = station, datetime)
      ]
      
      dt[, .t_diff_nb2 := get(pol) - .t_vn2]
    }
    
    rm(tmp_lkp)
    
    # -- (3) Temporal benchmark construction -----------------------------
    # Type 1: both adjacent readings; Type 2: one adjacent reading;
    # Type 3: no adjacent reading.
    dt[, .t_bench := data.table::fcase(
      !is.na(.t_lag) & !is.na(.t_lead), (.t_lag + .t_lead) / 2,
      !is.na(.t_lag),  .t_lag,
      !is.na(.t_lead), .t_lead,
      default = NA_real_
    )]
    
    dt[, .t_btype := data.table::fcase(
      !is.na(.t_lag) & !is.na(.t_lead), 1L,
      !is.na(.t_lag) | !is.na(.t_lead), 2L,
      default = 3L
    )]
    
    # Difference between the observed value and the temporal benchmark.
    dt[, .t_diff_b := get(pol) - .t_bench]
    
    # -- (4) Flag station-month right-tail observations ------------------
    # Only values above this threshold can become outliers.
    dt[, .t_ym := format(datetime, "%Y-%m")]
    
    dt[, .t_p99 := as.numeric(
      stats::quantile(.SD[[1]], probs = pct_flag, na.rm = TRUE)
    ), by = .(station, .t_ym), .SDcols = pol]
    
    dt[, .t_flag := data.table::fifelse(
      !is.na(get(pol)) & !is.na(.t_p99) & get(pol) > .t_p99,
      1L,
      0L
    )]
    
    # -- (5) Station-month temporal and spatial statistics ---------------
    # Temporal stats are based on own-station first differences.
    # Spatial stats are based on station-neighbor simultaneous differences.
    if (miss_neigh == "second") {
      dt[, `:=`(
        .t_md   = mean(.t_diff,     na.rm = TRUE),
        .t_sd   = sd(.t_diff,       na.rm = TRUE),
        .t_mnb1 = mean(.t_diff_nb1, na.rm = TRUE),
        .t_snb1 = sd(.t_diff_nb1,   na.rm = TRUE),
        .t_mnb2 = mean(.t_diff_nb2, na.rm = TRUE),
        .t_snb2 = sd(.t_diff_nb2,   na.rm = TRUE)
      ), by = .(station, .t_ym)]
    } else {
      dt[, `:=`(
        .t_md   = mean(.t_diff,     na.rm = TRUE),
        .t_sd   = sd(.t_diff,       na.rm = TRUE),
        .t_mnb1 = mean(.t_diff_nb1, na.rm = TRUE),
        .t_snb1 = sd(.t_diff_nb1,   na.rm = TRUE)
      ), by = .(station, .t_ym)]
    }
    
    # -- (6) Diagnostic counts ------------------------------------------
    # sd() is NA when there are fewer than two non-missing differences.
    # sd() is zero when the station-month differences are constant.
    dt[, .t_missing_tsd := is.na(.t_sd)]
    dt[, .t_zero_tsd    := !is.na(.t_sd) & .t_sd == 0]
    
    dt[, .t_missing_ssd1 := is.na(.t_snb1)]
    dt[, .t_zero_ssd1    := !is.na(.t_snb1) & .t_snb1 == 0]
    
    # For the second-neighbor case, diagnose whether all spatial
    # alternatives are missing and whether any feasible one has zero SD.
    if (miss_neigh == "second") {
      dt[, .t_missing_ssd2 := is.na(.t_snb2)]
      dt[, .t_zero_ssd2    := !is.na(.t_snb2) & .t_snb2 == 0]
      
      dt[, .t_missing_ssd := .t_missing_ssd1 & .t_missing_ssd2]
      dt[, .t_zero_ssd    := .t_zero_ssd1 | .t_zero_ssd2]
    } else {
      dt[, .t_missing_ssd := .t_missing_ssd1]
      dt[, .t_zero_ssd    := .t_zero_ssd1]
    }
    
    # Store flagged-observation diagnostics as station-month counts.
    dt[, (miss_tsd_col) := sum(.t_flag == 1L & .t_missing_tsd),
       by = .(station, .t_ym)]
    
    dt[, (zero_tsd_col) := sum(.t_flag == 1L & .t_zero_tsd),
       by = .(station, .t_ym)]
    
    dt[, (miss_ssd_col) := sum(.t_flag == 1L & .t_missing_ssd),
       by = .(station, .t_ym)]
    
    dt[, (zero_ssd_col) := sum(.t_flag == 1L & .t_zero_ssd),
       by = .(station, .t_ym)]
    
    # -- (7) Temporal classification ------------------------------------
    # 1 = reasonable, 2 = unreasonable, 3 = no temporal benchmark.
    dt[, .t_cat := data.table::fcase(
      .t_btype == 3L, 3L,
      !is.na(.t_diff_b) &
        !is.na(.t_md) & !is.na(.t_sd) &
        .t_diff_b > (.t_md - n_sd * .t_sd) &
        .t_diff_b < (.t_md + n_sd * .t_sd), 1L,
      default = 2L
    )]
    
    # If temporal missingness is allowed to continue, Type 3 values
    # can still be rescued by the spatial check.
    cats_check <- if (miss_temp == "continue") c(2L, 3L) else 2L
    
    # -- (8) Spatial rescue using closest neighbor -----------------------
    # A spatial check is feasible only when both the current difference
    # and the station-month spatial benchmark exist.
    dt[, .t_spat1_feasible := !is.na(.t_diff_nb1) &
         !is.na(.t_mnb1) & !is.na(.t_snb1)]
    
    dt[, .t_spat1_pass := .t_spat1_feasible &
         .t_diff_nb1 > (.t_mnb1 - n_sd * .t_snb1) &
         .t_diff_nb1 < (.t_mnb1 + n_sd * .t_snb1)]
    
    # If closest-neighbor spatial check passes, keep the observation.
    dt[
      .t_cat %in% cats_check & .t_spat1_pass == TRUE,
      .t_cat := 1L
    ]
    
    # -- (9) Spatial rescue using second closest neighbor ----------------
    if (miss_neigh == "second") {
      
      # The second neighbor is used only when the first check is infeasible.
      dt[, .t_spat2_feasible := !is.na(.t_diff_nb2) &
           !is.na(.t_mnb2) & !is.na(.t_snb2)]
      
      dt[, .t_spat2_pass := .t_spat2_feasible &
           .t_diff_nb2 > (.t_mnb2 - n_sd * .t_snb2) &
           .t_diff_nb2 < (.t_mnb2 + n_sd * .t_snb2)]
      
      # Rescue observations when the first neighbor cannot be used
      # and the second neighbor validates the reading.
      dt[
        .t_cat %in% cats_check &
          .t_spat1_feasible == FALSE &
          .t_spat2_pass == TRUE,
        .t_cat := 1L
      ]
      
      # No spatial check exists if neither neighbor is feasible.
      dt[, .t_no_spat := .t_spat1_feasible == FALSE &
           .t_spat2_feasible == FALSE]
      
      # Spatial failure is assigned to the feasible check that was used.
      dt[, .t_failed_spat := .t_spat1_feasible == TRUE &
           .t_spat1_pass == FALSE]
      
      dt[
        .t_spat1_feasible == FALSE & .t_spat2_feasible == TRUE,
        .t_failed_spat := .t_spat2_pass == FALSE
      ]
    } else {
      
      # Without second-neighbor fallback, only the closest neighbor matters.
      dt[, .t_no_spat := .t_spat1_feasible == FALSE]
      
      dt[, .t_failed_spat := .t_spat1_feasible == TRUE &
           .t_spat1_pass == FALSE]
    }
    
    # -- (10) Assign diagnostic reason codes -----------------------------
    # Reason 1: flagged Type 3, no feasible spatial rescue.
    dt[
      .t_flag == 1L & .t_cat == 3L & .t_no_spat == TRUE,
      (reason_col) := 1L
    ]
    
    # Reason 2: flagged temporal failure, no feasible spatial rescue.
    dt[
      .t_flag == 1L & .t_cat == 2L & .t_no_spat == TRUE,
      (reason_col) := 2L
    ]
    
    # Reason 3: flagged temporal failure and spatial check failed.
    dt[
      .t_flag == 1L & .t_cat == 2L & .t_failed_spat == TRUE,
      (reason_col) := 3L
    ]
    
    # Reason 4: flagged Type 3 and spatial check failed.
    dt[
      .t_flag == 1L & .t_cat == 3L & .t_failed_spat == TRUE,
      (reason_col) := 4L
    ]
    
    # Legacy-style behavior: missing temporal benchmark is final.
    # This reproduces the older, more punitive rule when requested.
    if (miss_temp == "finish") {
      dt[.t_flag == 1L & .t_cat == 3L, (reason_col) := 1L]
    }
    
    # -- (11) Final masking ----------------------------------------------
    # Only observations with positive reason codes are removed.
    dt[get(reason_col) > 0L, (flag_col) := 1L]
    dt[get(flag_col) == 1L, (pol) := NA_real_]
    
    # -- (12) Cleanup temporary columns ----------------------------------
    # Keep final flags, reason codes, diagnostics, and cleaned pollutant.
    drop_cols <- c(
      ".t_lag", ".t_lead", ".t_diff", ".t_diff_nb1", ".t_bench",
      ".t_btype", ".t_diff_b", ".t_ym", ".t_p99", ".t_flag",
      ".t_md", ".t_sd", ".t_mnb1", ".t_snb1", ".t_cat",
      ".t_near1", ".t_vn1", ".t_missing_tsd", ".t_zero_tsd",
      ".t_missing_ssd1", ".t_zero_ssd1", ".t_missing_ssd",
      ".t_zero_ssd", ".t_spat1_feasible", ".t_spat1_pass",
      ".t_no_spat", ".t_failed_spat"
    )
    
    if (miss_neigh == "second") {
      drop_cols <- c(
        drop_cols, ".t_diff_nb2", ".t_mnb2", ".t_snb2",
        ".t_near2", ".t_vn2", ".t_missing_ssd2", ".t_zero_ssd2",
        ".t_spat2_feasible", ".t_spat2_pass"
      )
    }
    
    drop_cols <- intersect(drop_cols, names(dt))
    dt[, (drop_cols) := NULL]
    
    invisible(NULL)
  }
  
  # 5. Year loop
  # -----------------------------------------------------------------------
  for (yr in years) {
    if (!quiet) {
      message("  [", yr, "] Collecting ...")
    }
    
    # Collect one year at a time to limit memory use.
    dt_yr <- arrow_ds |>
      dplyr::filter(year == yr) |>
      dplyr::collect()          |>
      data.table::as.data.table()
    
    if (nrow(dt_yr) == 0L) {
      next
    }
    
    # Normalize station identifiers before balancing and joining.
    dt_yr[, station := .normalize_station(station)]
    
    all_sta  <- unique(dt_yr$station)
    yr_start <- min(dt_yr$datetime)
    yr_end   <- max(dt_yr$datetime)
    
    # Add one boundary hour before and after the year.
    # This avoids losing lag/lead information at year boundaries.
    prev_cutoff <- yr_start - 3600
    next_cutoff <- yr_end   + 3600
    prev_yr     <- as.integer(format(prev_cutoff, "%Y"))
    next_yr     <- as.integer(format(next_cutoff, "%Y"))
    
    bnd_prev <- arrow_ds |>
      dplyr::filter(year == prev_yr, datetime == prev_cutoff) |>
      dplyr::collect() |>
      data.table::as.data.table()
    
    if (nrow(bnd_prev) > 0L) {
      bnd_prev[, station := .normalize_station(station)]
      bnd_prev <- bnd_prev[station %in% all_sta]
    }
    
    bnd_next <- arrow_ds |>
      dplyr::filter(year == next_yr, datetime == next_cutoff) |>
      dplyr::collect() |>
      data.table::as.data.table()
    
    if (nrow(bnd_next) > 0L) {
      bnd_next[, station := .normalize_station(station)]
      bnd_next <- bnd_next[station %in% all_sta]
    }
    
    # Create a balanced station-hour grid for the year.
    all_hours <- seq(yr_start, yr_end, by = "hour")
    grid      <- data.table::CJ(station = all_sta, datetime = all_hours)
    
    # Keep all variables except keys and year; year is reassigned below.
    non_key <- setdiff(names(dt_yr), c("station", "datetime", "year"))
    
    dt_bal <- data.table::merge.data.table(
      grid,
      dt_yr[, c("station", "datetime", non_key), with = FALSE],
      by = c("station", "datetime"),
      all.x = TRUE
    )
    
    dt_bal[, year := yr]
    
    # Attach boundary rows for lag/lead computation.
    dt_bal <- data.table::rbindlist(
      list(dt_bal, bnd_prev, bnd_next),
      fill = TRUE,
      use.names = TRUE
    )
    
    # Ensure shift() uses the correct station-hour order.
    data.table::setorder(dt_bal, station, datetime)
    
    # Mark rows belonging to the target year before adding boundary rows.
    in_yr <- dt_bal$datetime >= yr_start & dt_bal$datetime <= yr_end
    
    # Apply the outlier procedure pollutant by pollutant.
    for (pol in pollutants) {
      .flag_pollutant(
        dt         = dt_bal,
        pol        = pol,
        dist_dt    = dist_dt,
        pct_flag   = pct_flag,
        n_sd       = n_sd,
        miss_temp  = on_missing_temporal,
        miss_neigh = on_missing_neighbor
      )
    }
    
    # Drop boundary rows before saving.
    dt_out <- dt_bal[in_yr]
    
    # Write partitioned output in the same year=YYYY structure.
    yr_dir <- file.path(out_path, paste0("year=", yr))
    dir.create(yr_dir, showWarnings = FALSE)
    
    arrow::write_parquet(
      dt_out,
      file.path(yr_dir, "data.parquet"),
      compression = "snappy"
    )
    
    # Explicit cleanup after each year helps with large city panels.
    rm(dt_yr, dt_bal, dt_out, grid, bnd_prev, bnd_next, in_yr)
    gc(verbose = FALSE)
  }
  
  invisible(out_path)
}


# --------------------------------------------------------------------------------------------
# Function: aggregate_idw_exposure
#
# @Arg arrow_dir           : string; path to partitioned Arrow/Parquet hourly data.
# @Arg geo_sta_pq          : string; path to geo-station distance Parquet file.
# @Arg census_col          : data.frame; census data used for quintile assignment.
# @Arg geo_id_col          : string; geographic ID column in census_col.
# @Arg pop_col             : string; population or expansion-weight column.
# @Arg edu_col             : string; education column used to define quintiles.
# @Arg quintile_level      : string; "geo" or "individual". Default "geo".
# @Arg indiv_adult_col     : string; adult filter column. Default "adult".
# @Arg buffer_km           : numeric; maximum geo-to-station distance. Default 3.
# @Arg distance_power      : numeric; IDW distance exponent. Default 2.
# @Arg target_years        : numeric vector or NULL; years to process.
# @Arg pollutants          : character vector; pollutant columns to aggregate.
# @Arg who_it              : named list; WHO interim target thresholds.
# @Arg mem_gb              : numeric; DuckDB memory ceiling in GB. Default 40.
# @Arg n_threads           : integer; DuckDB worker threads. Default 2.
# @Arg duckdb_temp_dir     : string or NULL; DuckDB spill directory.
# @Arg out_dir             : string; output directory.
# @Arg out_name            : string; output file prefix.
# @Arg overwrite           : logical; skip computation if outputs exist.
# @Arg quiet               : logical; suppress messages. Default FALSE.
# @Arg return_data         : logical; return data.tables in memory. Default FALSE.
# @Arg fail_on_query_error : logical; stop if a SQL query fails. Default TRUE.
# @Arg chunk_by_month      : logical; process each year-pollutant by month.
#
# @Output : Named list with saved file paths and, optionally, data.tables.
#
# @Details:
#   Aggregates hourly station pollution to geographic units using missingness-aware
#   inverse-distance weighting. For each geo-hour-pollutant cell, only stations
#   within buffer_km and with non-missing readings enter the numerator and
#   denominator. DuckDB is configured with conservative memory settings and a
#   spill directory. Monthly chunking reduces peak memory for large city-buffer
#   combinations while preserving annual averages and exceedance counts.
#
# @Written_on : 02/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
aggregate_idw_exposure <- function(
    arrow_dir,
    geo_sta_pq,
    census_col,
    geo_id_col          = "GEO_ID",
    pop_col             = "n",
    edu_col             = "escolaridad_avg",
    quintile_level      = c("geo", "individual"),
    indiv_adult_col     = "adult",
    buffer_km           = 3,
    distance_power      = 2,
    target_years        = NULL,
    pollutants          = c("pm10", "pm25"),
    who_it              = list(
      pm10 = c(it1 = 150, it2 = 100, it3 = 75,  it4 = 50),
      pm25 = c(it1 = 75,  it2 = 50,  it3 = 37.5, it4 = 25)
    ),
    mem_gb              = 40,
    n_threads           = 2L,
    duckdb_temp_dir     = NULL,
    out_dir             = "data/interim/exposure",
    out_name,
    overwrite           = TRUE,
    quiet               = FALSE,
    return_data         = FALSE,
    fail_on_query_error = TRUE,
    chunk_by_month      = TRUE
) {
  
  # 0. Dependencies and argument matching
  # -----------------------------------------------------------------------
  pkgs <- c("duckdb", "DBI", "arrow", "data.table", "dplyr", "stringi")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package missing: ", p)
    }
  }
  
  # Check DuckDB version for out-of-core stability.
  if (utils::packageVersion("duckdb") < "0.9.2") {
    stop("DuckDB >= 0.9.2 required.")
  }
  
  quintile_level <- match.arg(quintile_level)
  
  # Validate main inputs.
  if (!dir.exists(arrow_dir)) {
    stop("`arrow_dir` not found: ", arrow_dir)
  }
  
  if (!file.exists(geo_sta_pq)) {
    stop("`geo_sta_pq` not found: ", geo_sta_pq)
  }
  
  # Check census required columns.
  for (col in c(geo_id_col, pop_col, edu_col)) {
    if (!col %in% names(census_col)) {
      stop("Column '", col, "' missing.")
    }
  }
  
  # Individual mode requires an adult indicator.
  if (quintile_level == "individual" &&
      !indiv_adult_col %in% names(census_col)) {
    stop("Column '", indiv_adult_col, "' not found for individual mode.")
  }
  
  # 1. Output paths and early exit
  # -----------------------------------------------------------------------
  out_path   <- file.path(out_dir, paste0(out_name, "_idw_exposure.parquet"))
  indiv_path <- file.path(out_dir, paste0(out_name, "_indiv_quintiles.parquet"))
  
  # Skip computation if all relevant outputs already exist.
  if (!overwrite) {
    geo_done   <- file.exists(out_path)
    indiv_done <- quintile_level == "geo" || file.exists(indiv_path)
    
    if (geo_done && indiv_done) {
      if (!quiet) {
        message("Outputs exist; skipping.")
      }
      
      out <- list(exposure_path = out_path)
      
      if (isTRUE(return_data)) {
        out$exposure_yearly <- data.table::as.data.table(
          arrow::read_parquet(out_path)
        )
      }
      
      if (quintile_level == "individual") {
        out$individual_path <- indiv_path
        
        if (isTRUE(return_data)) {
          out$individual_quintiles <- data.table::as.data.table(
            arrow::read_parquet(indiv_path)
          )
        }
      }
      
      return(invisible(out))
    }
  }
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # 2. Helpers
  # -----------------------------------------------------------------------
  .dq_path <- function(p) {
    paste0("'", gsub("'", "''", gsub("\\\\", "/", p)), "'")
  }
  
  # Normalize station identifiers consistently with the distance step.
  .normalize_station <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  # Convert geographic IDs to strings without corrupting integer64 IDs.
  .safe_chr <- function(x) {
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
  
  # Query helper: fail by default to avoid incomplete output files.
  .run_query <- function(con, query, context) {
    tryCatch(
      data.table::as.data.table(DBI::dbGetQuery(con, query)),
      error = function(e) {
        msg <- paste0("Query failed for ", context, ": ", e$message)
        
        if (isTRUE(fail_on_query_error)) {
          stop(msg, call. = FALSE)
        }
        
        warning(msg)
        NULL
      }
    )
  }
  
  # 3. DuckDB disk-backed connection
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[", out_name, "] Starting DuckDB engine ...")
  }
  
  # Temporary database; DuckDB may use this file during execution.
  dbdir <- tempfile("idw_duck_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  
  # Track whether the function created its own DuckDB spill directory.
  delete_duckdb_temp <- is.null(duckdb_temp_dir)
  duckdb_temp_root   <- NULL
  
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    try(unlink(dbdir, recursive = TRUE, force = TRUE), silent = TRUE)
    
    if (isTRUE(delete_duckdb_temp) && exists("duckdb_temp_dir")) {
      try(unlink(duckdb_temp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
    }
    
    if (isTRUE(delete_duckdb_temp) && exists("duckdb_temp_root")) {
      if (!is.null(duckdb_temp_root) && dir.exists(duckdb_temp_root)) {
        remaining_files <- list.files(
          duckdb_temp_root,
          all.files = TRUE,
          no.. = TRUE
        )
        
        if (length(remaining_files) == 0L) {
          try(unlink(duckdb_temp_root, recursive = TRUE, force = TRUE),
              silent = TRUE)
        }
      }
    }
  }, add = TRUE)
  
  # Configure DuckDB with conservative memory settings.
  n_threads <- as.integer(max(1L, n_threads))
  mem_gb    <- as.integer(mem_gb)
  
  DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", n_threads))
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", mem_gb))
  
  # Disable insertion-order preservation to reduce memory pressure.
  DBI::dbExecute(con, "SET preserve_insertion_order = false;")
  
  # Allow DuckDB to spill intermediate results to disk.
  if (is.null(duckdb_temp_dir)) {
    duckdb_temp_root <- file.path(out_dir, "_duckdb_tmp")
    duckdb_temp_dir  <- file.path(duckdb_temp_root, out_name)
  }
  
  dir.create(duckdb_temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  DBI::dbExecute(
    con,
    paste0("SET temp_directory = ", .dq_path(duckdb_temp_dir), ";")
  )
  
  # 4. Load and normalize distance matrix
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[", out_name, "] Loading and normalizing distances ...")
  }
  
  dist_dt <- data.table::as.data.table(arrow::read_parquet(geo_sta_pq))
  
  # Require the schema produced by compute_distance_matrices().
  req_dist_cols <- c("geo_id", "station_id", "distance_km")
  miss_dist_cols <- setdiff(req_dist_cols, names(dist_dt))
  
  if (length(miss_dist_cols) > 0L) {
    stop("Distance table is missing: ", paste(miss_dist_cols, collapse = ", "))
  }
  
  # Normalize join keys before registering the table in DuckDB.
  dist_dt[, geo_id := .safe_chr(geo_id)]
  dist_dt[, station_id := .normalize_station(station_id)]
  
  # Diagnose zero-distance pairs before dropping them from IDW denominators.
  n_zero_dist <- dist_dt[!is.na(distance_km) & distance_km == 0, .N]
  
  if (n_zero_dist > 0L && !quiet) {
    message(
      "[", out_name, "] Diagnostic: ", n_zero_dist,
      " geo-station pair(s) have distance_km == 0 and will be excluded."
    )
  }
  
  # Keep positive distances inside the requested buffer.
  dist_dt <- dist_dt[
    !is.na(distance_km) & distance_km > 0 & distance_km <= buffer_km
  ]
  
  if (nrow(dist_dt) == 0L) {
    stop("No geo-station pairs within ", buffer_km, " km.")
  }
  
  # Pre-compute inverse-distance weights in R.
  dist_dt[, inv_d := 1 / (distance_km ^ distance_power)]
  dist_dt <- dist_dt[, .(geo_id, station_id, inv_d)]
  
  # Register normalized distance table in DuckDB.
  DBI::dbWriteTable(con, "dist_tbl", dist_dt, overwrite = TRUE)
  
  # Validate matrix dimensions after filtering.
  n_geo <- DBI::dbGetQuery(
    con, "SELECT COUNT(DISTINCT geo_id) AS n FROM dist_tbl;"
  )$n
  
  n_sta <- DBI::dbGetQuery(
    con, "SELECT COUNT(DISTINCT station_id) AS n FROM dist_tbl;"
  )$n
  
  if (n_geo == 0L || n_sta == 0L) {
    stop("Distance table is empty after filtering.")
  }
  
  if (!quiet) {
    message(
      "[", out_name, "] Distance table: ", n_geo,
      " geo unit(s), ", n_sta, " station(s)."
    )
  }
  
  # Release R-side distance object after registering it in DuckDB.
  rm(dist_dt)
  gc(verbose = FALSE)
  
  # 5. Pollution view and station crosswalk
  # -----------------------------------------------------------------------
  poll_glob <- paste0(gsub("\\\\", "/", arrow_dir), "/**/*.parquet")
  
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS\n",
    "SELECT * FROM read_parquet(",
    .dq_path(poll_glob), ", hive_partitioning = true);"
  ))
  
  # Read distinct raw station names only; this is small relative to the data.
  station_xwalk <- data.table::as.data.table(
    DBI::dbGetQuery(
      con,
      "SELECT DISTINCT CAST(station AS VARCHAR) AS station_raw FROM pollution;"
    )
  )
  
  if (!"station_raw" %in% names(station_xwalk)) {
    stop("Column `station` not found in pollution dataset.")
  }
  
  # Normalize raw pollution station names in R, not inside DuckDB SQL.
  station_xwalk[, station_id := .normalize_station(station_raw)]
  
  # Remove missing or empty station identifiers.
  station_xwalk <- station_xwalk[
    !is.na(station_raw) & !is.na(station_id) & station_id != ""
  ]
  
  DBI::dbWriteTable(con, "station_xwalk", station_xwalk, overwrite = TRUE)
  
  # Validate station overlap between pollution and distance matrix.
  station_overlap <- DBI::dbGetQuery(con, paste0(
    "SELECT\n",
    "  (SELECT COUNT(DISTINCT station_id) FROM station_xwalk) AS n_poll_sta,\n",
    "  (SELECT COUNT(DISTINCT station_id) FROM dist_tbl) AS n_dist_sta,\n",
    "  (SELECT COUNT(DISTINCT x.station_id)\n",
    "   FROM station_xwalk x\n",
    "   INNER JOIN dist_tbl d ON x.station_id = d.station_id) AS n_overlap;"
  ))
  
  if (!quiet) {
    message(
      "[", out_name, "] Station overlap: ",
      station_overlap$n_overlap, " of ", station_overlap$n_poll_sta,
      " pollution station(s) overlap distance matrix."
    )
  }
  
  if (station_overlap$n_overlap == 0L) {
    stop(
      "No station overlap between pollution data and distance matrix after ",
      "normalization. Check station names in `arrow_dir` and `geo_sta_pq`."
    )
  }
  
  rm(station_xwalk)
  gc(verbose = FALSE)
  
  # 6. Year list filtering
  # -----------------------------------------------------------------------
  avail_years <- sort(
    DBI::dbGetQuery(con, "SELECT DISTINCT year FROM pollution ORDER BY year;")$year
  )
  
  # Subset to requested target years if specified.
  years <- if (!is.null(target_years)) {
    intersect(avail_years, target_years)
  } else {
    avail_years
  }
  
  if (length(years) == 0L) {
    stop("No data found for requested target_years.")
  }
  
  if (!quiet) {
    message("[", out_name, "] Processing ", length(years), " year(s).")
  }
  
  # 7. Year x pollutant loop
  # -----------------------------------------------------------------------
  yearly_list <- vector("list", length(years))
  names(yearly_list) <- as.character(years)
  
  poll_fields <- DBI::dbListFields(con, "pollution")
  
  for (yr in years) {
    if (!quiet) {
      message("[", out_name, "] Year ", yr, " ...")
    }
    
    poll_results <- vector("list", length(pollutants))
    names(poll_results) <- pollutants
    
    for (poll in pollutants) {
      
      # Skip absent pollutants gracefully.
      if (!poll %in% poll_fields) {
        if (!quiet) {
          message("[", out_name, "] Pollutant absent, skipping: ", poll)
        }
        next
      }
      
      # Build WHO threshold columns for annual reconstruction.
      thr <- who_it[[poll]]
      who_cols <- character(0)
      who_frag <- ""
      
      if (!is.null(thr) && length(thr) > 0L) {
        who_cols <- paste0("hrs_d_", poll, "_", names(thr))
        
        who_frag <- paste(
          vapply(seq_along(thr), function(i) {
            sprintf(
              paste0(
                ",\n       SUM(CASE WHEN idw >= %s ",
                "THEN 1 ELSE 0 END) AS %s"
              ),
              thr[[i]],
              who_cols[[i]]
            )
          }, character(1)),
          collapse = ""
        )
      }
      
      # Use monthly chunks for memory-heavy city-buffer combinations.
      month_ids <- if (isTRUE(chunk_by_month)) seq_len(12L) else NA_integer_
      month_list <- vector("list", length(month_ids))
      
      for (i in seq_along(month_ids)) {
        mo <- month_ids[[i]]
        
        if (!quiet && isTRUE(chunk_by_month)) {
          message(
            "[", out_name, "] Year ", yr,
            ", ", poll, ", month ", sprintf("%02d", mo), " ..."
          )
        }
        
        # Add a month filter only in monthly mode.
        month_filter <- if (isTRUE(chunk_by_month)) {
          paste0("    AND EXTRACT(month FROM p.datetime) = ", mo, "\n")
        } else {
          ""
        }
        
        # Compute monthly or annual IDW summaries.
        query <- paste0(
          "WITH h AS (\n",
          "  SELECT x.station_id, p.datetime, p.", poll, " AS val\n",
          "  FROM pollution p\n",
          "  INNER JOIN station_xwalk x\n",
          "    ON CAST(p.station AS VARCHAR) = x.station_raw\n",
          "  WHERE p.year = ", yr, "\n",
          month_filter,
          "    AND p.", poll, " IS NOT NULL\n",
          "),\n",
          "hr_geo AS (\n",
          "  SELECT d.geo_id, h.datetime,\n",
          "         SUM(h.val * d.inv_d) / SUM(d.inv_d) AS idw\n",
          "  FROM h\n",
          "  INNER JOIN dist_tbl d ON h.station_id = d.station_id\n",
          "  GROUP BY d.geo_id, h.datetime\n",
          ")\n",
          "SELECT geo_id,\n",
          "       SUM(idw) AS sum_idw_", poll, ",\n",
          "       COUNT(*) AS total_hrs_", poll,
          who_frag, "\n",
          "FROM hr_geo\n",
          "GROUP BY geo_id;"
        )
        
        context <- if (isTRUE(chunk_by_month)) {
          paste0(poll, " in ", yr, ", month ", mo)
        } else {
          paste0(poll, " in ", yr)
        }
        
        res <- .run_query(con, query, context)
        
        if (!is.null(res) && nrow(res) > 0L) {
          month_list[[i]] <- res
        }
        
        rm(res)
        gc(verbose = FALSE)
      }
      
      # Combine monthly or annual chunks into one annual pollutant table.
      valid_chunks <- Filter(Negate(is.null), month_list)
      
      if (length(valid_chunks) == 0L) {
        next
      }
      
      chunk_dt <- data.table::rbindlist(valid_chunks, fill = TRUE)
      
      sum_col <- paste0("sum_idw_", poll)
      hrs_col <- paste0("total_hrs_", poll)
      avg_col <- paste0("avg_", poll)
      
      agg_cols <- c(sum_col, hrs_col, who_cols)
      agg_cols <- intersect(agg_cols, names(chunk_dt))
      
      annual_dt <- chunk_dt[
        ,
        lapply(.SD, sum, na.rm = TRUE),
        by = geo_id,
        .SDcols = agg_cols
      ]
      
      annual_dt[
        get(hrs_col) > 0,
        (avg_col) := get(sum_col) / get(hrs_col)
      ]
      
      annual_dt[, (sum_col) := NULL]
      poll_results[[poll]] <- annual_dt
      
      rm(month_list, valid_chunks, chunk_dt, annual_dt)
      gc(verbose = FALSE)
    }
    
    # Merge pollutant-specific exposure tables for the active year.
    valid <- Filter(Negate(is.null), poll_results)
    
    if (length(valid) == 0L) {
      next
    }
    
    yr_exp <- Reduce(
      function(a, b) merge(a, b, by = "geo_id", all = TRUE),
      valid
    )
    
    yr_exp[, year := yr]
    yearly_list[[as.character(yr)]] <- yr_exp
    
    rm(poll_results, valid, yr_exp)
    gc(verbose = FALSE)
  }
  
  # Stack yearly results.
  all_years <- data.table::rbindlist(
    Filter(Negate(is.null), yearly_list),
    fill = TRUE
  )
  
  if (nrow(all_years) == 0L) {
    stop("No exposure data produced.")
  }
  
  all_years[, geo_id := as.character(geo_id)]
  
  # 8. Census processing
  # -----------------------------------------------------------------------
  census_dt <- data.table::copy(data.table::as.data.table(census_col))
  data.table::setnames(census_dt, geo_id_col, "geo_id")
  census_dt[, geo_id := .safe_chr(geo_id)]
  
  if (quintile_level == "geo") {
    
    # Sort geo units by average education.
    data.table::setorderv(census_dt, edu_col)
    
    # Assign quintiles based on cumulative geographic population shares.
    census_dt[
      !is.na(get(edu_col)) & !is.na(get(pop_col)),
      `:=`(
        cum_pop = cumsum(get(pop_col)),
        tot_pop = sum(get(pop_col))
      )
    ]
    
    census_dt[
      !is.na(cum_pop),
      edu_quintile := data.table::fcase(
        cum_pop / tot_pop <= 0.2, 1L,
        cum_pop / tot_pop <= 0.4, 2L,
        cum_pop / tot_pop <= 0.6, 3L,
        cum_pop / tot_pop <= 0.8, 4L,
        default = 5L
      )
    ]
    
    census_dt[, c("cum_pop", "tot_pop") := NULL]
    
    result <- merge(all_years, census_dt, by = "geo_id", all.x = TRUE)
    arrow::write_parquet(result, out_path)
    
    out <- list(exposure_path = out_path)
    
    if (isTRUE(return_data)) {
      out$exposure_yearly <- result
    }
    
    return(invisible(out))
    
  } else {
    
    # Filter to adult individuals only.
    census_dt <- census_dt[get(indiv_adult_col) == 1]
    
    if (nrow(census_dt) == 0L) {
      stop("No adult rows after filtering.")
    }
    
    # Sort individuals by personal education.
    data.table::setorderv(census_dt, edu_col)
    
    # Assign quintiles based on city-wide expansion factors.
    census_dt[
      !is.na(get(edu_col)) & !is.na(get(pop_col)),
      `:=`(
        cum_pop = cumsum(get(pop_col)),
        tot_pop = sum(get(pop_col))
      )
    ]
    
    census_dt[
      !is.na(cum_pop),
      edu_quintile := data.table::fcase(
        cum_pop / tot_pop <= 0.2, 1L,
        cum_pop / tot_pop <= 0.4, 2L,
        cum_pop / tot_pop <= 0.6, 3L,
        cum_pop / tot_pop <= 0.8, 4L,
        default = 5L
      )
    ]
    
    census_dt[, c("cum_pop", "tot_pop") := NULL]
    
    # Save datasets independently to avoid a huge year-individual matrix.
    arrow::write_parquet(all_years, out_path)
    arrow::write_parquet(census_dt, indiv_path)
    
    out <- list(
      exposure_path   = out_path,
      individual_path = indiv_path
    )
    
    if (isTRUE(return_data)) {
      out$exposure_yearly <- all_years
      out$individual_quintiles <- census_dt
    }
    
    return(invisible(out))
  }
}


# --------------------------------------------------------------------------------------------
# Function: run_idw_city
#
# @Arg city_label     : string; city name used in progress messages.
# @Arg city_id        : string; city identifier used in output folders and file names.
# @Arg arrow_dir      : string; path to cleaned partitioned Arrow/Parquet data.
# @Arg distance_power : numeric; IDW distance exponent.
# @Arg geo_sta_pq     : string; path to geo-station distance Parquet file.
# @Arg geo_census     : data.frame; collapsed geographic-unit census data.
# @Arg micro_census   : data.frame; individual-level census microdata.
# @Arg geo_id_col     : string; geographic ID column in collapsed census data.
# @Arg geo_pop_col    : string; population column in collapsed census data.
# @Arg geo_edu_col    : string; education column in collapsed census data.
# @Arg micro_id_col   : string; geographic ID column in individual census data.
# @Arg micro_pop_col  : string; weight column in individual census data.
# @Arg micro_edu_col  : string; education column in individual census data.
# @Arg buffer_km      : numeric; maximum geo-to-station distance.
# @Arg outdir_exp     : string; root output directory for IDW estimates.
# @Arg mem_gb         : numeric; DuckDB memory ceiling in GB. Default 40.
# @Arg n_threads      : integer; DuckDB worker threads. Default 2.
# @Arg overwrite      : logical; overwrite existing outputs. Default TRUE.
# @Arg return_data    : logical; return data objects in memory. Default FALSE.
#
# @Output : Named list with two elements:
#   geo        : output path for geo-level exposure with geo quintiles.
#   individual : output paths for exposure and individual quintiles.
#
# @Details:
#   Computes the expensive IDW exposure table once using individual mode.
#   Then it builds the geo-level exposure output by merging the same exposure
#   table with collapsed census data and assigning geo-level education quintiles.
#
# @Written_on : April 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
run_idw_city <- function(
    city_label,
    city_id,
    arrow_dir,
    distance_power,
    geo_sta_pq,
    geo_census,
    micro_census,
    geo_id_col,
    geo_pop_col,
    geo_edu_col,
    micro_id_col,
    micro_pop_col,
    micro_edu_col,
    buffer_km,
    outdir_exp,
    mem_gb      = 40,
    n_threads   = 8L,
    overwrite   = TRUE,
    return_data = FALSE
) {
  
  message("\n--- Processing ", city_label, " | ", buffer_km, " km ---")
  
  # Define output folder and common output prefix.
  city_out_dir <- here::here(outdir_exp, city_id)
  dir.create(city_out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_base <- sprintf("%s_%dkm", city_id, buffer_km)
  
  # Compute IDW exposure once and save individual quintiles.
  exp_indiv <- aggregate_idw_exposure(
    arrow_dir      = arrow_dir,
    geo_sta_pq     = geo_sta_pq,
    census_col     = micro_census,
    geo_id_col     = micro_id_col,
    pop_col        = micro_pop_col,
    edu_col        = micro_edu_col,
    quintile_level = "individual",
    buffer_km      = buffer_km,
    distance_power = distance_power,
    mem_gb         = mem_gb,
    n_threads      = n_threads,
    out_dir        = city_out_dir,
    out_name       = out_base,
    overwrite      = overwrite,
    return_data    = FALSE
  )
  
  message(city_label, " exposure: ", exp_indiv$exposure_path)
  message(city_label, " individual quintiles: ", exp_indiv$individual_path)
  
  # Read the saved geo-level exposure table.
  exposure_dt <- data.table::as.data.table(
    arrow::read_parquet(exp_indiv$exposure_path)
  )
  
  # Prepare collapsed census data for geo-level quintiles.
  geo_dt <- data.table::copy(data.table::as.data.table(geo_census))
  
  data.table::setnames(geo_dt, geo_id_col, "geo_id")
  geo_dt[, geo_id := as.character(geo_id)]
  
  # Sort geographic units by education and assign population-weighted quintiles.
  data.table::setorderv(geo_dt, geo_edu_col)
  
  geo_dt[
    !is.na(get(geo_edu_col)) & !is.na(get(geo_pop_col)),
    `:=`(
      cum_pop = cumsum(get(geo_pop_col)),
      tot_pop = sum(get(geo_pop_col))
    )
  ]
  
  geo_dt[
    !is.na(cum_pop),
    edu_quintile := data.table::fcase(
      cum_pop / tot_pop <= 0.2, 1L,
      cum_pop / tot_pop <= 0.4, 2L,
      cum_pop / tot_pop <= 0.6, 3L,
      cum_pop / tot_pop <= 0.8, 4L,
      default = 5L
    )
  ]
  
  geo_dt[, c("cum_pop", "tot_pop") := NULL]
  
  # Merge exposure with collapsed census.
  geo_result <- merge(exposure_dt, geo_dt, by = "geo_id", all.x = TRUE)
  
  geo_path <- file.path(
    city_out_dir,
    paste0(out_base, "_geo_idw_exposure.parquet")
  )
  
  arrow::write_parquet(geo_result, geo_path)
  message(city_label, " geo exposure: ", geo_path)
  
  # Return paths only by default to avoid retaining large objects.
  if (!isTRUE(return_data)) {
    rm(exposure_dt, geo_dt, geo_result)
    gc(verbose = FALSE)
    
    return(invisible(list(
      geo = list(exposure_path = geo_path),
      individual = exp_indiv
    )))
  }
  
  invisible(list(
    geo = list(
      exposure_yearly = geo_result,
      exposure_path = geo_path
    ),
    individual = exp_indiv
  ))
}


# --------------------------------------------------------------------------------------------
# Function: impute_missing_hourly_ols
#
# @Arg arrow_dir   : string; Arrow dataset (hourly).
# @Arg out_dir     : string; output directory.
# @Arg out_name    : string; prefix for output folder.
# @Arg pollutants  : character; default c("pm10","pm25").
# @Arg id_col      : string; unique station column. Default "station_code".
# @Arg legacy_mode : logical; if TRUE, replicates the shifting-identity 
#                    compaction bug of the legacy pipeline. Default FALSE.
# @Arg overwrite   : logical; skip if output exists. Default TRUE.
# @Arg quiet       : logical; suppress messages. Default FALSE.
#
# @Details
#   LEGACY MODE (TRUE):
#     Replicates the Dropbox pipeline exactly. It collects non-NA readings from 
#     other stations and compacts them leftward into anonymous `other_X` columns.
#     This destroys spatial identity and changes missingness dummies into a simple 
#     count of offline stations. Fits a single pooled OLS.
#
#   UNBIASED MODE (FALSE):
#     Correctly implements the paper's intended Eq(1). Fits separate models per 
#     station, using explicitly named neighboring stations as predictors, 
#     preserving exact spatial correlation and distinct missingness states.
# @Output     : List with out_path = out_path, n_imputed = sum(pp_summary$n_imputed), 
#               per_poll = pp_summary, per_year = pp)
# @Written_on : 02/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
impute_missing_hourly_ols <- function(
    arrow_dir,
    out_dir,
    out_name,
    pollutants  = c("pm10", "pm25"),
    id_col      = "station",
    legacy_mode = FALSE,
    overwrite   = TRUE,
    quiet       = FALSE
) {
  pkgs <- c("arrow", "data.table", "stats", "lubridate", "dplyr", "stringi")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Missing: ", p)
  }
  
  out_path <- file.path(out_dir, out_name)
  if (!overwrite && dir.exists(out_path)) {
    if (!quiet) message("Output exists; skipping.")
    return(invisible(list(out_path = out_path, n_imputed = NA_integer_)))
  }
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  
  # Normalization Helper (Matches compute_distance_matrices logic)
  .normalize_st <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    x <- gsub("[^A-Z0-9_]", "_", x) # Replace spaces/special chars with underscore
    return(x)
  }
  
  # ── 1. Scan dataset for years ───────────────────────────────────────────────
  if (!quiet) message("[impute] Scanning dataset ...")
  ds <- arrow::open_dataset(arrow_dir)
  
  if (!id_col %in% names(ds)) stop("Column '", id_col, "' not found in data.")
  
  has_yr <- "year" %in% names(ds)
  if (has_yr) {
    unique_years <- ds |> dplyr::select(year) |> dplyr::distinct() |> 
      dplyr::collect() |> dplyr::pull() |> sort()
  } else {
    dts <- ds |> dplyr::select(datetime) |> dplyr::collect()
    unique_years <- sort(unique(lubridate::year(dts$datetime)))
  }
  
  pollutants <- intersect(pollutants, names(ds))
  if (length(pollutants) == 0L) stop("No requested pollutants found.")
  
  all_per_poll <- list()
  
  # ── 2. Year loop ────────────────────────────────────────────────────────────
  for (yr in unique_years) {
    if (!quiet) message("\n[impute] --- Year: ", yr, " ---")
    
    if (has_yr) {
      dt <- ds |> dplyr::filter(year == yr) |> dplyr::collect()
    } else {
      yr_s <- as.POSIXct(paste0(yr, "-01-01 00:00:00"), tz = "UTC")
      yr_e <- as.POSIXct(paste0(yr + 1, "-01-01 00:00:00"), tz = "UTC")
      dt <- ds |> dplyr::filter(datetime >= yr_s, datetime < yr_e) |> 
        dplyr::collect()
    }
    data.table::setDT(dt)
    
    # Sanitise: Inf/-Inf/NaN → NA
    for (p in pollutants) {
      if (p %in% names(dt)) {
        dt[!is.finite(get(p)) & !is.na(get(p)), (p) := NA_real_]
      }
    }
    
    # Temporal factors & Normalized Station ID
    if (!has_yr) dt[, year := yr]
    dt[, month := as.factor(lubridate::month(datetime))]
    dt[, hour := as.factor(lubridate::hour(datetime))]
    dt[, day_week := as.factor(lubridate::wday(datetime, week_start = 1))]
    dt[, station_code := as.factor(.normalize_st(get(id_col)))]
    
    # ── 3. Pollutant loop ─────────────────────────────────────────────────────
    for (poll in pollutants) {
      if (!quiet) message("         Fitting OLS for: ", poll)
      
      st_names <- sort(unique(as.character(dt$station_code)))
      n_st <- length(st_names)
      if (n_st < 2) {
        if (!quiet) message("         < 2 IDs. Skipping.")
        next
      }
      
      if (legacy_mode) {
        # ── LEGACY MODE ───────────────────────────────────────────────────────
        w_dt <- data.table::dcast(
          dt, datetime ~ station_code, value.var = poll,
          fun.aggregate = function(x) {
            v <- x[!is.na(x)]
            if (length(v) == 0L) NA_real_ else mean(v)
          }
        )
        dt_reg <- w_dt[dt, on = "datetime"]
        
        mat_all <- as.matrix(dt_reg[, ..st_names])
        row_idx <- match(as.character(dt_reg$station_code), st_names)
        
        mask <- matrix(TRUE, nrow = nrow(mat_all), ncol = ncol(mat_all))
        mask[cbind(seq_len(nrow(mat_all)), row_idx)] <- FALSE
        
        mat_other <- matrix(mat_all[mask], nrow = nrow(mat_all), ncol = n_st - 1)
        
        shift_na <- function(x) {
          v <- x[!is.na(x)]
          c(v, rep(NA_real_, length(x) - length(v)))
        }
        mat_shifted <- t(apply(mat_other, 1, shift_na))
        
        other_cols <- paste0("other_", seq_len(n_st - 1))
        dt_other <- data.table::as.data.table(mat_shifted)
        data.table::setnames(dt_other, other_cols)
        
        dt_reg <- cbind(dt_reg, dt_other)
        
        other_m_cols <- paste0(other_cols, "_m")
        for (col in other_cols) {
          m_col <- paste0(col, "_m")
          dt_reg[, (m_col) := as.integer(is.na(get(col)))]
          dt_reg[is.na(get(col)), (col) := 0]
        }
        
        f_str <- paste(
          poll, "~", paste(c(other_cols, other_m_cols), collapse = " + "),
          "+ station_code + month*day_week + hour*day_week + month*hour"
        )
        
        dt_reg[, prediction := NA_real_]
        
        n_miss_by_sta <- dt_reg[, .(n_miss = sum(is.na(get(poll)))), by = station_code]
        keep_sta <- n_miss_by_sta[n_miss < .N - 1, station_code]
        train_idx <- which(dt_reg$station_code %in% keep_sta)
        
        if (length(train_idx) > 50) {
          model <- tryCatch({
            stats::lm(as.formula(f_str), data = dt_reg[train_idx])
          }, warning = function(w) {
            suppressWarnings(stats::lm(as.formula(f_str), data = dt_reg[train_idx]))
          }, error = function(e) NULL)
          
          if (!is.null(model)) {
            valid <- rep(TRUE, nrow(dt_reg))
            for (fac in names(model$xlevels)) {
              valid <- valid & (as.character(dt_reg[[fac]]) %in% model$xlevels[[fac]])
            }
            if (any(valid)) {
              dt_reg[valid, prediction := suppressWarnings(
                stats::predict(model, newdata = dt_reg[valid])
              )]
            }
          }
        }
        dt[, prediction := dt_reg$prediction]
        
      } else {
        # ── UNBIASED MODE ─────────────────────────────────────────────────────
        w_dt <- data.table::dcast(
          dt, datetime ~ station_code, value.var = poll,
          fun.aggregate = function(x) {
            v <- x[!is.na(x)]
            if (length(v) == 0L) NA_real_ else mean(v)
          }
        )
        dt_reg <- w_dt[dt, on = "datetime"]
        
        for (col in st_names) {
          m_col <- paste0(col, "_m")
          dt_reg[, (m_col) := as.integer(is.na(get(col)))]
          dt_reg[is.na(get(col)), (col) := 0]
        }
        
        dt_reg[, prediction := NA_real_]
        
        for (st in st_names) {
          pred_cols <- setdiff(st_names, st)
          
          idx_fit <- which(dt_reg$station_code == st)
          if (length(idx_fit) < 50) next
          
          keep_p <- vapply(pred_cols, function(col) {
            v <- dt_reg[[col]][idx_fit]
            length(unique(v[!is.na(v)])) > 1L
          }, logical(1))
          
          pred_cols <- pred_cols[keep_p]
          if (length(pred_cols) == 0L) next
          pred_m <- paste0(pred_cols, "_m")
          
          # Dynamic Formula Builder (Prevents 'contrasts' error on sparse data)
          t_terms <- character()
          has_m <- length(unique(dt_reg$month[idx_fit])) > 1L
          has_d <- length(unique(dt_reg$day_week[idx_fit])) > 1L
          has_h <- length(unique(dt_reg$hour[idx_fit])) > 1L
          
          if (has_m) t_terms <- c(t_terms, "month")
          if (has_d) t_terms <- c(t_terms, "day_week")
          if (has_h) t_terms <- c(t_terms, "hour")
          
          # Only add interactions if both main factors exist for this station
          if (has_m && has_d) t_terms <- c(t_terms, "month:day_week")
          if (has_h && has_d) t_terms <- c(t_terms, "hour:day_week")
          if (has_m && has_h) t_terms <- c(t_terms, "month:hour")
          
          temp_str <- if (length(t_terms) > 0) paste(t_terms, collapse=" + ") else "1"
          
          f_str <- paste(
            poll, "~", paste(c(pred_cols, pred_m), collapse = " + "), "+", temp_str
          )
          
          if (sum(!is.na(dt_reg[[poll]][idx_fit])) < 50) next
          
          model <- tryCatch({
            stats::lm(as.formula(f_str), data = dt_reg[idx_fit])
          }, warning = function(w) {
            suppressWarnings(stats::lm(as.formula(f_str), data = dt_reg[idx_fit]))
          }, error = function(e) {
            if (!quiet) message("         [!] Error on ", st, ": ", e$message)
            NULL
          })
          
          if (!is.null(model)) {
            valid <- rep(TRUE, length(idx_fit))
            for (fac in names(model$xlevels)) {
              valid <- valid & (as.character(dt_reg[[fac]][idx_fit]) %in% 
                                  model$xlevels[[fac]])
            }
            if (any(valid)) {
              # Suppress the multicollinearity rank-deficient warnings
              dt_reg[idx_fit[valid], prediction := suppressWarnings(
                stats::predict(model, newdata = dt_reg[idx_fit[valid]])
              )]
            }
          }
        }
        
        dt[, prediction := dt_reg$prediction]
      }
      
      # ── 4. Apply predictions to gaps ────────────────────────────────────────
      is_miss <- is.na(dt[[poll]])
      n_imp <- sum(is_miss & !is.na(dt$prediction))
      dt[is_miss, (poll) := dt$prediction[is_miss]]
      
      t_col <- paste0(poll, "_imputed_from")
      if (!t_col %in% names(dt)) dt[, (t_col) := NA_character_]
      mode_lbl <- if (legacy_mode) "OLS_Legacy" else "OLS_Unbiased"
      dt[is_miss & !is.na(prediction), (t_col) := mode_lbl]
      
      all_per_poll[[length(all_per_poll) + 1]] <- data.table::data.table(
        year = yr, pollutant = poll, n_imputed = n_imp
      )
      if (!quiet) message("         Filled ", n_imp, " obs.")
    }
    
    # ── 5. Write year partition ───────────────────────────────────────────────
    drop <- c("month", "hour", "day_week", "station_code", "prediction")
    drop <- intersect(drop, names(dt))
    dt[, (drop) := NULL]
    
    arrow::write_dataset(
      dataset  = dt,
      path     = out_path,
      format   = "parquet",
      partitioning = "year",
      existing_data_behavior = "overwrite"
    )
  }
  
  # ── 6. Summary ──────────────────────────────────────────────────────────────
  pp <- data.table::rbindlist(all_per_poll)
  if (nrow(pp) > 0) {
    pp_summary <- pp[, .(n_imputed = sum(n_imputed)), by = pollutant]
  } else {
    pp_summary <- data.table::data.table(pollutant=character(), n_imputed=integer())
  }
  
  invisible(list(out_path = out_path, n_imputed = sum(pp_summary$n_imputed),
                 per_poll = pp_summary, per_year = pp))
}


# --------------------------------------------------------------------------------------------
# Function: summarize_stations_by_pollutant
#
# @Arg arrow_dir     : string; path to the Arrow pollution dataset.
# @Arg city_label    : string; city identifier.
# @Arg pollutants    : character; default c("pm10", "pm25", "o3", "no2", "co").
# @Arg year_filter   : integer|NULL; restrict to one year. Default NULL.
# @Arg min_valid_pct : numeric [0,1]; minimum share of expected hours per year.
# @Arg mem_gb        : numeric; DuckDB memory ceiling in GB. Default 4.
# @Arg quiet         : logical; suppress messages. Default FALSE.
#
# @Output : Named list ($wide and $long) of data.tables.
#
# @Details:
#   Implements Algebraic Balancing: Instead of physically imputing missing rows 
#   to fix implicit missingness, the SQL dynamically calculates the exact number 
#   of expected hours in the year (accounting for leap years) as the denominator.
#   This guarantees mathematically perfect coverage percentages at near-zero 
#   computational cost. Matches legacy behavior perfectly at min_valid_pct = 0.0.
#
# @Written_on : 17/04/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
summarize_stations_by_pollutant <- function(
    arrow_dir,
    city_label,
    pollutants    = c("pm10", "pm25", "o3", "no2", "co"),
    year_filter   = NULL,
    min_valid_pct = 0.0,
    mem_gb        = 32,
    quiet         = FALSE
) {
  # Check required packages before initiating database connections
  pkgs <- c("duckdb", "DBI", "data.table")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  # Validate inputs to prevent downstream SQL failures
  stopifnot(
    dir.exists(arrow_dir), nzchar(city_label),
    is.numeric(min_valid_pct), min_valid_pct >= 0, min_valid_pct <= 1
  )
  
  # Initialize DuckDB connection with strict memory limits
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), 
    add = TRUE
  )
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  
  # Create a virtual view over the partitioned Parquet files
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(", 
    glob_q, ", hive_partitioning = true);"
  ))
  
  # Extract available columns to dynamically filter requested pollutants
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present  <- tolower(col_info$name)
  pollutants <- intersect(tolower(pollutants), present)
  
  if (length(pollutants) == 0L) {
    stop("None of the requested pollutants are present in the dataset.")
  }
  
  # Build optional year filter for the SQL query
  yr_filter_sql <- if (is.null(year_filter)) {
    "" 
  } else {
    sprintf("WHERE EXTRACT(year FROM datetime) = %d", as.integer(year_filter))
  }
  
  res <- vector("list", length(pollutants))
  
  # Iterate over each valid pollutant to calculate balanced completeness
  for (i in seq_along(pollutants)) {
    poll <- pollutants[[i]]
    
    # ALGEBRAIC BALANCING LOGIC:
    # date_diff() calculates exact expected hours for that specific year.
    # This prevents implicit missingness from inflating valid_pct.
    q <- sprintf(
      "WITH per_sy AS (
         SELECT station,
                CAST(EXTRACT(year FROM datetime) AS INTEGER) AS yr,
                COUNT(%s) AS valid_n
         FROM pollution %s
         GROUP BY station, EXTRACT(year FROM datetime)
       )
       SELECT yr AS year,
              COUNT(DISTINCT station) AS n_stations,
              STRING_AGG(DISTINCT station, ';' ORDER BY station) AS stations
       FROM per_sy
       WHERE (valid_n * 1.0 / date_diff('hour', 
                                        make_date(yr, 1, 1), 
                                        make_date(yr + 1, 1, 1))) >= %f
       GROUP BY yr
       ORDER BY yr;",
      poll, yr_filter_sql, min_valid_pct
    )
    
    # Execute query and convert to data.table
    r <- data.table::as.data.table(DBI::dbGetQuery(con, q))
    
    if (nrow(r) == 0L) next
    
    # Append tracking metadata
    r[, `:=`(city = city_label, pollutant = poll)]
    res[[i]] <- r
  }
  
  # Combine results for all pollutants
  long <- data.table::rbindlist(Filter(Negate(is.null), res), fill = TRUE)
  
  # Handle empty returns gracefully
  if (nrow(long) == 0L) {
    if (!quiet) {
      message("[stations] ", city_label, ": no rows passed the filter.")
    }
    return(invisible(list(wide = data.table::data.table(), long = long)))
  }
  
  # Enforce standard column ordering for the long table
  data.table::setcolorder(
    long, 
    c("city", "year", "pollutant", "n_stations", "stations")
  )
  
  # Pivot to wide format for easy latex table generation
  wide <- data.table::dcast(
    long, city + year ~ pollutant, 
    value.var = "n_stations", fill = 0L
  )
  
  if (!quiet) message(
    "[stations] ", city_label, ": ", 
    data.table::uniqueN(long$year), " year(s), ", 
    length(pollutants), " pollutant(s)"
  )
  
  invisible(list(wide = wide, long = long))
}


# --------------------------------------------------------------------------------------------
# Function: compute_who_exceedances
#
# @Arg arrow_dir    : string; path to the cleaned Arrow pollution dataset.
# @Arg city_label   : string; city identifier added as a column in the output.
# @Arg pollutants   : character; default c("pm10","pm25").
# @Arg year_filter  : integer|NULL; restrict to a single year. Default NULL.
# @Arg who_annual   : named numeric; WHO AQG annual averages in μg/m³. 
# @Arg station_aggr : string; "mean" or "median". 
# @Arg legacy_mode  : logical; if TRUE, uses a pooled grand mean (duration-weighted).
#                     if FALSE, uses mean-of-means (spatial-weighted). Default FALSE.
# @Arg mem_gb       : numeric; DuckDB memory ceiling in GB. Default 12.
# @Arg quiet        : logical; suppress info messages. Default FALSE.
#
# @Output : data.table with city, year, pollutant, city_avg, who_aqg, 
#           exceedance_factor, n_stations, n_valid_hrs.
#
# @Written_on : 17/04/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_who_exceedances <- function(
    arrow_dir,
    city_label,
    pollutants   = c("pm10", "pm25"),
    year_filter  = NULL,
    who_annual   = c(pm10 = 15, pm25 = 5),
    station_aggr = c("mean", "median"),
    legacy_mode  = FALSE,
    mem_gb       = 12,
    quiet        = FALSE
) {
  # 1. Validate dependencies
  # ----------------------------------------------------------------------------------
  pkgs <- c("duckdb", "DBI", "data.table")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' required.")
  }
  
  station_aggr <- match.arg(station_aggr)
  stopifnot(dir.exists(arrow_dir), nzchar(city_label))
  
  # 2. Database Initialization
  # ----------------------------------------------------------------------------------
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown=TRUE), silent=TRUE), add=TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  
  # Install and load ICU for safe datetime extraction
  DBI::dbExecute(con, "INSTALL icu;")
  DBI::dbExecute(con, "LOAD icu;")
  
  # 3. Mount Parquet Dataset
  # ----------------------------------------------------------------------------------
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(",
    glob_q, ", hive_partitioning = true);"
  ))
  
  # 4. Schema Validation
  # ----------------------------------------------------------------------------------
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present  <- tolower(col_info$name)
  pollutants <- intersect(tolower(pollutants), present)
  
  if (length(pollutants) == 0L) stop("Requested pollutants are missing.")
  
  # 5. Query Preparation
  # ----------------------------------------------------------------------------------
  yr_filter_sql <- if (is.null(year_filter)) {
    ""
  } else {
    sprintf("AND EXTRACT(year FROM datetime) = %d", as.integer(year_filter))
  }
  
  collapse_fun <- if (station_aggr == "median") "MEDIAN" else "AVG"
  out_rows <- vector("list", length(pollutants))
  
  # 6. Execute Aggregation Loop
  # ----------------------------------------------------------------------------------
  for (i in seq_along(pollutants)) {
    poll <- pollutants[[i]]
    
    if (legacy_mode) {
      # -----------------------------------------------------------------------
      # LEGACY MODE (Pooled Grand Mean):
      # Pools all hourly observations together. Heavily biased towards stations 
      # with longer sensor uptimes. Replicates the older methodology exactly.
      # -----------------------------------------------------------------------
      q <- sprintf(
        "SELECT EXTRACT(year FROM datetime) AS year,
                %s(%s) AS city_avg,
                COUNT(DISTINCT station) AS n_stations,
                COUNT(%s) AS n_valid_hrs
         FROM pollution
         WHERE %s IS NOT NULL %s
         GROUP BY EXTRACT(year FROM datetime)
         ORDER BY year;",
        collapse_fun, poll, poll, poll, yr_filter_sql
      )
    } else {
      # -----------------------------------------------------------------------
      # UNBIASED MODE (Spatial Mean-of-Means):
      # CTE (station_year) calculates the standardized mean per physical station.
      # Main SELECT collapses these spatial means to prevent uptime bias.
      # -----------------------------------------------------------------------
      q <- sprintf(
        "WITH station_year AS (
           SELECT station,
                  EXTRACT(year FROM datetime) AS year,
                  AVG(%s) AS station_avg,
                  COUNT(%s) AS n_valid
           FROM pollution
           WHERE %s IS NOT NULL %s
           GROUP BY station, EXTRACT(year FROM datetime)
         )
         SELECT year,
                %s(station_avg)  AS city_avg,
                COUNT(DISTINCT station) AS n_stations,
                SUM(n_valid)     AS n_valid_hrs
         FROM station_year
         GROUP BY year
         ORDER BY year;",
        poll, poll, poll, yr_filter_sql, collapse_fun
      )
    }
    
    # Execute the query
    r <- data.table::as.data.table(DBI::dbGetQuery(con, q))
    if (nrow(r) == 0L) next
    
    # 7. Post-Processing
    # --------------------------------------------------------------------------------
    r[, `:=`(
      city      = city_label,
      pollutant = poll,
      who_aqg   = unname(who_annual[poll])
    )]
    
    r[, exceedance_factor := city_avg / who_aqg]
    
    data.table::setcolorder(r, c(
      "city", "year", "pollutant", "city_avg",
      "who_aqg", "exceedance_factor", "n_stations", "n_valid_hrs"
    ))
    
    out_rows[[i]] <- r
  }
  
  # 8. Finalize Output
  # ----------------------------------------------------------------------------------
  res <- data.table::rbindlist(Filter(Negate(is.null), out_rows), fill = TRUE)
  
  if (!quiet) {
    message(
      "[exceedances] ", city_label,
      " (Legacy: ", legacy_mode, "): ", 
      nrow(res), " city-year-pollutant rows"
    )
  }
  
  return(res)
}


# --------------------------------------------------------------------------------------------
# Function: compute_missing_proportions
#
# @Arg arrow_dir   : string; path to partitioned Arrow/Parquet pollution dataset.
# @Arg pollutants  : character; default c("pm10","pm25","o3","no2","co").
# @Arg dims        : character; dimensions to aggregate by. Valid values:
#                    "station", "month", "hour", "day_of_week", "year".
# @Arg year_filter : integer|NULL; restrict to a single year. Default NULL.
# @Arg out_dir     : string|NULL; directory to write Parquet files.
# @Arg out_name    : string; file prefix. Required if out_dir is provided.
# @Arg mem_gb      : numeric; DuckDB memory ceiling in GB. Default 8.
# @Arg quiet       : logical; suppress info messages. Default FALSE.
#
# @Output : Named list of data.tables.
# @Details: Calculates structural and algorithmic missingness cleanly inside DuckDB.
#           Avoids RAM bottlenecks by resolving all math before pulling to R.
#
# @Written_on : 17/04/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_missing_proportions <- function(
    arrow_dir,
    pollutants  = c("pm10", "pm25", "o3", "no2", "co"),
    dims        = c("station", "month", "hour"),
    year_filter = NULL,
    out_dir     = NULL,
    out_name    = NULL,
    mem_gb      = 8,
    quiet       = FALSE
) {
  # 1. Dependency and Input Validation
  # ----------------------------------------------------------------------------------
  pkgs <- c("duckdb", "DBI", "arrow", "data.table")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required but not installed.")
    }
  }
  
  if (!dir.exists(arrow_dir)) stop("`arrow_dir` not found: ", arrow_dir)
  
  valid_dims <- c("station", "month", "hour", "day_of_week", "year")
  dims <- intersect(dims, valid_dims)
  
  if (length(dims) == 0L) {
    stop("No valid dims. Use: ", paste(valid_dims, collapse = ", "))
  }
  
  if (!is.null(out_dir)) {
    if (is.null(out_name) || !nzchar(out_name)) {
      stop("`out_name` is required when `out_dir` is provided.")
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # 2. Database Initialization
  # ----------------------------------------------------------------------------------
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%dGB';", as.integer(mem_gb)))
  
  # Install ICU for safe datetime extraction (consistent with pipeline standards)
  DBI::dbExecute(con, "INSTALL icu;")
  DBI::dbExecute(con, "LOAD icu;")
  
  # Mount the Parquet dataset as a virtual table
  glob_q <- paste0("'", gsub("\\\\", "/", arrow_dir), "/**/*.parquet'")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS SELECT * FROM read_parquet(",
    glob_q, ", hive_partitioning = true);"
  ))
  
  # 3. Schema Validation
  # ----------------------------------------------------------------------------------
  col_info <- DBI::dbGetQuery(con, "PRAGMA table_info('pollution');")
  present_cols <- tolower(col_info$name)
  pollutants <- intersect(tolower(pollutants), present_cols)
  
  if (length(pollutants) == 0L) {
    stop("None of the requested pollutants are present in the dataset.")
  }
  
  # 4. Query Preparation
  # ----------------------------------------------------------------------------------
  # Map requested dimensions to their exact SQL extraction syntax
  dim_expr <- list(
    station     = "station",
    year        = "EXTRACT(year FROM datetime)",
    month       = "EXTRACT(month FROM datetime)",
    hour        = "EXTRACT(hour FROM datetime)",
    day_of_week = "EXTRACT(isodow FROM datetime)"
  )
  
  yr_filter_sql <- if (is.null(year_filter)) {
    ""
  } else {
    sprintf("WHERE EXTRACT(year FROM datetime) = %d", as.integer(year_filter))
  }
  
  out <- list()
  
  # 5. Execute Aggregation Loop
  # ----------------------------------------------------------------------------------
  for (d in dims) {
    if (!quiet) message("[missing] Aggregating dimension: ", d)
    
    # Build dynamic SQL columns to calculate NA ratios for all pollutants at once
    cols_sql <- paste(
      vapply(pollutants, function(p) {
        paste0(
          "100.0 * SUM(CASE WHEN ", p, " IS NULL THEN 1 ELSE 0 END) / ",
          "COUNT(*) AS ", p, "_missing_pct, ",
          "COUNT(*) AS ", p, "_total_hrs"
        )
      }, character(1)),
      collapse = ", "
    )
    
    # Construct the final aggregation query
    q <- sprintf(
      "SELECT %s AS %s, %s \nFROM pollution \n%s \nGROUP BY %s \nORDER BY %s;",
      dim_expr[[d]], d, cols_sql, yr_filter_sql, dim_expr[[d]], dim_expr[[d]]
    )
    
    # Execute query and pull into a data.table
    res <- data.table::as.data.table(DBI::dbGetQuery(con, q))
    
    # 6. Post-Processing
    # --------------------------------------------------------------------------------
    # Since each pollutant generates a 'total_hrs' column, we collapse duplicates
    dup_totals <- grep("_total_hrs$", names(res), value = TRUE)
    
    if (length(dup_totals) > 1L) {
      keep <- dup_totals[1L]
      res[, (setdiff(dup_totals, keep)) := NULL]
      data.table::setnames(res, keep, "total_hrs")
      
    } else if (length(dup_totals) == 1L) {
      data.table::setnames(res, dup_totals, "total_hrs")
    }
    
    out[[d]] <- res
    
    # 7. File Export
    # --------------------------------------------------------------------------------
    if (!is.null(out_dir)) {
      pth <- file.path(out_dir, paste0(out_name, "_missing_by_", d, ".parquet"))
      arrow::write_parquet(res, pth)
      if (!quiet) message("  -> Wrote: ", pth)
    }
  }
  
  invisible(out)
}


# --------------------------------------------------------------------------------------------
# Function: compute_exposure_ci_regression
#
# Estimates exposure differences across socioeconomic groups and returns tidy
# estimates with confidence intervals. Accepts IN-MEMORY data.tables so the caller
# can inspect exactly what is being modeled (see the runnable script).
#
# @Arg exposure_dt      : data.table; geo-level IDW exposure (already in memory).
# @Arg individual_dt    : data.table or NULL; individual/group table (already in memory).
# @Arg geo_id_col       : string; geographic ID column. Default "geo_id".
# @Arg pop_col          : string; population / expansion-weight column.
# @Arg group_col        : string; group variable, e.g. "edu_quintile".
# @Arg group_values     : numeric vector; valid groups, e.g. 1:5.
# @Arg base_group       : numeric; omitted reference group.
# @Arg pollutants       : character; pollutants to include.
# @Arg outcome_pattern  : string; regex for outcome columns.
# @Arg year_filter      : integer or NULL; restrict to one year.
# @Arg conf_level       : numeric; confidence level. Default 0.95.
# @Arg normalized       : logical; divide by base-group mean and report gaps.
# @Arg regression_unit  : string; "geo_group", "individual", or "geo".
# @Arg se_type          : string; "classic", "HC1", or "cluster_geo".
# @Arg quiet            : logical; suppress messages. Default FALSE.
#
# @Output : data.table with estimates, standard errors, CIs, and metadata.
# @Author : Marcos Paulo
# @Updated_on: May 2026
# --------------------------------------------------------------------------------------------
compute_exposure_ci_regression <- function(
    exposure_dt,
    individual_dt   = NULL,
    geo_id_col      = "geo_id",
    pop_col         = "n",
    group_col       = "edu_quintile",
    group_values    = 1:5,
    base_group      = max(group_values),
    pollutants      = c("pm10", "pm25"),
    outcome_pattern = "^(avg|hrs_d)_",
    year_filter     = NULL,
    conf_level      = 0.95,
    normalized      = FALSE,
    regression_unit = c("geo_group", "individual", "geo"),
    se_type         = c("classic", "HC1", "cluster_geo"),
    quiet           = FALSE
) {
  
  # 0. Dependencies and argument checks
  # -----------------------------------------------------------------------
  for (p in c("data.table")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required.")
    }
  }
  
  se_type         <- match.arg(se_type)
  regression_unit <- match.arg(regression_unit)
  
  if (se_type %in% c("HC1", "cluster_geo") &&
      !requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' required for robust or clustered SEs.")
  }
  
  if (!data.table::is.data.table(exposure_dt)) {
    stop("`exposure_dt` must be a data.table held in memory.")
  }
  
  if (!(conf_level > 0 && conf_level < 1)) {
    stop("`conf_level` must be between 0 and 1.")
  }
  
  if (!base_group %in% group_values) {
    stop("`base_group` must belong to `group_values`.")
  }
  
  # 1. Copy and (optionally) filter the exposure table
  # -----------------------------------------------------------------------
  # Copy so we never mutate the caller's inspected object by reference.
  dt <- data.table::copy(exposure_dt)
  
  if (!is.null(year_filter)) {
    dt <- dt[year == year_filter]
  }
  
  if (!geo_id_col %in% names(dt)) {
    stop("geo_id_col '", geo_id_col, "' not found in exposure data.")
  }
  
  dt[, (geo_id_col) := as.character(get(geo_id_col))]
  
  # 2. Merge individual/group data when the estimator needs it
  # -----------------------------------------------------------------------
  needs_individual <- regression_unit %in% c("geo_group", "individual")
  
  if (needs_individual) {
    if (is.null(individual_dt)) {
      stop("`individual_dt` required for regression_unit = ", regression_unit)
    }
    
    ind <- data.table::copy(individual_dt)
    
    for (col in c(geo_id_col, group_col, pop_col)) {
      if (!col %in% names(ind)) {
        stop("Column '", col, "' not found in individual data.")
      }
    }
    
    ind[, (geo_id_col) := as.character(get(geo_id_col))]
    
    # Drop a pre-existing group column on exposure to avoid merge conflicts.
    if (group_col %in% names(dt)) {
      dt[, (group_col) := NULL]
    }
    
    dt <- merge(
      dt,
      ind[, .SD, .SDcols = c(geo_id_col, group_col, pop_col)],
      by              = geo_id_col,
      allow.cartesian = TRUE
    )
  } else {
    for (col in c(group_col, pop_col)) {
      if (!col %in% names(dt)) {
        stop("Column '", col, "' not found in exposure data.")
      }
    }
  }
  
  # Keep valid groups and strictly positive weights only.
  dt <- dt[
    !is.na(get(group_col)) &
      get(group_col) %in% group_values &
      !is.na(get(pop_col)) &
      get(pop_col) > 0
  ]
  
  # 3. Identify the outcome columns to model
  # -----------------------------------------------------------------------
  out_cols <- select_outcome_cols(names(dt), outcome_pattern, pollutants)
  
  if (length(out_cols) == 0L) {
    stop("No outcome columns match the requested pattern and pollutants.")
  }
  
  # 4. Fit every outcome with the requested estimator
  # -----------------------------------------------------------------------
  # One named, top-level fitter per outcome -> easy to call on a single column
  # in RStudio when debugging (e.g. fit_geo_group_outcome(dt, out_cols[1], ...)).
  fit_one <- function(col) {
    if (regression_unit == "geo_group") {
      return(fit_geo_group_outcome(
        merged_dt = dt, col = col, geo_id_col = geo_id_col,
        group_col = group_col, pop_col = pop_col,
        group_values = group_values, base_group = base_group,
        pollutants = pollutants, se_type = se_type,
        conf_level = conf_level, normalized = normalized
      ))
    }
    
    fit_standard_outcome(
      model_source_dt = dt, col = col, geo_id_col = geo_id_col,
      group_col = group_col, pop_col = pop_col,
      group_values = group_values, base_group = base_group,
      pollutants = pollutants, regression_unit = regression_unit,
      se_type = se_type, conf_level = conf_level, normalized = normalized
    )
  }
  
  res <- data.table::rbindlist(lapply(out_cols, fit_one), fill = TRUE)
  
  # 5. Handle the empty case with a typed, zero-row table
  # -----------------------------------------------------------------------
  if (nrow(res) == 0L) {
    if (!quiet) {
      message("[ci] Warning: insufficient data to fit any models.")
    }
    
    return(data.table::data.table(
      outcome = character(), pollutant = character(), group = numeric(),
      estimate = numeric(), std_error = numeric(), ci_low = numeric(),
      ci_high = numeric(), n_units = integer(), base_group = numeric(),
      group_col = character(), regression_unit = character(),
      se_type = character()
    ))
  }
  
  data.table::setorder(res, outcome, pollutant, group)
  
  if (!quiet) {
    message(
      "[ci] ", length(out_cols), " outcome(s) fit using regression_unit = '",
      regression_unit, "' and se_type = '", se_type, "'."
    )
  }
  
  res[]
}


# --------------------------------------------------------------------------------------------
# Function: compute_exposure_group_summaries
#
# Weighted group-level exposure summaries (means, medians, counts). Accepts
# IN-MEMORY data.tables for the same inspectability reason as above.
#
# @Arg exposure_dt     : data.table; geo-level IDW exposure (in memory).
# @Arg individual_dt   : data.table; individual/group table (in memory).
# @Arg geo_id_col      : string; geographic ID column. Default "geo_id".
# @Arg pop_col         : string; population / expansion-weight column.
# @Arg group_col       : string; group variable, e.g. "edu_quintile".
# @Arg group_values    : numeric vector; valid groups.
# @Arg pollutants      : character; pollutants to include.
# @Arg outcome_pattern : string; regex for outcomes to summarize.
# @Arg year_filter     : integer or NULL; restrict to a year.
# @Arg quiet           : logical; suppress messages. Default FALSE.
#
# @Output : data.table with weighted means, weighted medians, and counts.
# @Author    : Marcos Paulo  
# @Updated_on: May 2026
# --------------------------------------------------------------------------------------------
compute_exposure_group_summaries <- function(
    exposure_dt,
    individual_dt,
    geo_id_col      = "geo_id",
    pop_col         = "n",
    group_col       = "edu_quintile",
    group_values    = 1:5,
    pollutants      = c("pm10", "pm25"),
    outcome_pattern = "^(avg|hrs_d)_",
    year_filter     = NULL,
    quiet           = FALSE
) {
  
  # 0. Dependencies and checks
  # -----------------------------------------------------------------------
  for (p in c("data.table")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required.")
    }
  }
  
  if (!data.table::is.data.table(exposure_dt)) {
    stop("`exposure_dt` must be a data.table held in memory.")
  }
  
  if (!data.table::is.data.table(individual_dt)) {
    stop("`individual_dt` must be a data.table held in memory.")
  }
  
  # 1. Copy, filter, and merge
  # -----------------------------------------------------------------------
  exp_dt <- data.table::copy(exposure_dt)
  ind_dt <- data.table::copy(individual_dt)
  
  if (!is.null(year_filter)) {
    exp_dt <- exp_dt[year == year_filter]
  }
  
  if (!geo_id_col %in% names(exp_dt)) {
    stop("Column '", geo_id_col, "' missing in exposure data.")
  }
  
  for (col in c(geo_id_col, pop_col, group_col)) {
    if (!col %in% names(ind_dt)) {
      stop("Column '", col, "' missing in individual data.")
    }
  }
  
  exp_dt[, (geo_id_col) := as.character(get(geo_id_col))]
  ind_dt[, (geo_id_col) := as.character(get(geo_id_col))]
  
  # Keep only valid groups / positive weights and the needed columns.
  ind_dt <- ind_dt[
    !is.na(get(group_col)) &
      get(group_col) %in% group_values &
      !is.na(get(pop_col)) &
      get(pop_col) > 0,
    .SD,
    .SDcols = c(geo_id_col, group_col, pop_col)
  ]
  
  dt <- merge(exp_dt, ind_dt, by = geo_id_col, allow.cartesian = TRUE)
  
  # 2. Identify outcomes
  # -----------------------------------------------------------------------
  out_cols <- select_outcome_cols(names(dt), outcome_pattern, pollutants)
  
  if (length(out_cols) == 0L) {
    stop("No outcome columns match the requested pattern and pollutants.")
  }
  
  # 3. Collapse to geo-by-group cells (exposure constant within geo unit)
  # -----------------------------------------------------------------------
  by_cols <- c(geo_id_col, group_col)
  
  geo_group <- dt[
    ,
    c(
      list(group_pop = sum(get(pop_col), na.rm = TRUE)),
      lapply(.SD, function(x) {
        stats::weighted.mean(x, get(pop_col), na.rm = TRUE)
      })
    ),
    by      = by_cols,
    .SDcols = out_cols
  ]
  
  geo_group <- geo_group[!is.na(group_pop) & group_pop > 0]
  
  # 4. Group-level weighted summaries, one outcome at a time
  # -----------------------------------------------------------------------
  res_list <- vector("list", length(out_cols))
  
  for (i in seq_along(out_cols)) {
    col  <- out_cols[[i]]
    meta <- parse_outcome_label(col, pollutants)
    
    tmp <- geo_group[
      !is.na(get(col)),
      .(
        weighted_mean = stats::weighted.mean(get(col), group_pop, na.rm = TRUE),
        weighted_median = weighted_median_value(get(col), group_pop),
        weighted_population = sum(group_pop, na.rm = TRUE),
        n_geo_group_cells = .N,
        n_geo_units = data.table::uniqueN(get(geo_id_col))
      ),
      by = group_col
    ]
    
    data.table::setnames(tmp, group_col, "group")
    
    tmp[, `:=`(
      outcome   = meta$outcome,
      pollutant = meta$pollutant,
      group_col = group_col
    )]
    
    res_list[[i]] <- tmp
  }
  
  res <- data.table::rbindlist(res_list, fill = TRUE)
  data.table::setorder(res, outcome, pollutant, group)
  
  if (!quiet) {
    message("[summary] ", length(out_cols), " outcome(s) summarized.")
  }
  
  res[]
}


# ------------------------------------------------------------------------------------
# Function: run_exposure_results_for_city
#
# City-level wrapper used by the runnable script. It receives the in-memory
# exposure and individual data.tables for ONE city (read eagerly in the script so
# coauthors can inspect them), runs the main CI regression and the raw group
# summaries, and stamps city/year/buffer metadata columns onto both outputs.
#
# @Arg exposure_dt      : data.table; this city's IDW exposure (in memory).
# @Arg individual_dt    : data.table; this city's individual/group table (in memory).
# @Arg spec_row         : one-row data.table from `city_specs` (city, city_id, pop_col).
# @Arg analysis_year    : integer; year to restrict to (e.g. 2023).
# @Arg buffer_km        : integer; buffer used for exposure (for metadata only).
# @Arg group_col        : string; group variable, e.g. "edu_quintile".
# @Arg group_values     : numeric vector; valid groups, e.g. 1:5.
# @Arg base_group       : numeric; omitted reference group.
# @Arg pollutants       : character; pollutants to include.
# @Arg summary_outcomes : string; regex for the raw-summary outcomes.
# @Arg ci_outcomes      : string; regex for the regression outcomes.
# @Arg confidence_level : numeric; confidence level for CIs.
# @Arg main_reg_unit    : string; main regression unit (e.g. "geo_group").
# @Arg main_se_type     : string; main SE type (e.g. "cluster_geo").
# @Arg normalized       : logical; normalized gaps for the CI regression.
# @Arg socioeconomic_var: string; human-readable label, e.g. "education".
# @Arg group_type       : string; human-readable label, e.g. "quintile" / "decile".
# @Arg validate_inputs  : logical; if TRUE, check required columns up front.
#
# @Output : list(ci = data.table, summary = data.table), each with metadata cols.
# @Author : Marcos Paulo
# @Updated_on: May 2026
# ------------------------------------------------------------------------------------
run_exposure_results_for_city <- function(
    exposure_dt,
    individual_dt,
    spec_row,
    analysis_year,
    buffer_km,
    group_col,
    group_values,
    base_group,
    pollutants,
    summary_outcomes,
    ci_outcomes,
    confidence_level,
    main_reg_unit,
    main_se_type,
    normalized,
    socioeconomic_var = "education",
    group_type        = "quintile",
    validate_inputs   = TRUE
) {
  
  # 0. Read the per-city population/weight column from the spec row
  # -----------------------------------------------------------------------
  pop_col <- spec_row$pop_col
  city    <- spec_row$city
  city_id <- spec_row$city_id
  
  # 1. Optional up-front validation (fail early with a clear message)
  # -----------------------------------------------------------------------
  if (isTRUE(validate_inputs)) {
    if (!"geo_id" %in% names(exposure_dt)) {
      stop("[", city, "] exposure data missing 'geo_id'.")
    }
    
    for (col in c("geo_id", group_col, pop_col)) {
      if (!col %in% names(individual_dt)) {
        stop("[", city, "] individual data missing '", col, "'.")
      }
    }
  }
  
  # 2. Main regression CIs (paper spec: geo_group + clustered SEs + gaps)
  # -----------------------------------------------------------------------
  ci <- compute_exposure_ci_regression(
    exposure_dt     = exposure_dt,
    individual_dt   = individual_dt,
    geo_id_col      = "geo_id",
    pop_col         = pop_col,
    group_col       = group_col,
    group_values    = group_values,
    base_group      = base_group,
    pollutants      = pollutants,
    outcome_pattern = ci_outcomes,
    year_filter     = analysis_year,
    conf_level      = confidence_level,
    normalized      = normalized,
    regression_unit = main_reg_unit,
    se_type         = main_se_type,
    quiet           = TRUE
  )
  
  # 3. Raw weighted group summaries (annual means + exceedance hours)
  # -----------------------------------------------------------------------
  summary <- compute_exposure_group_summaries(
    exposure_dt     = exposure_dt,
    individual_dt   = individual_dt,
    geo_id_col      = "geo_id",
    pop_col         = pop_col,
    group_col       = group_col,
    group_values    = group_values,
    pollutants      = pollutants,
    outcome_pattern = summary_outcomes,
    year_filter     = analysis_year,
    quiet           = TRUE
  )
  
  # 4. Stamp shared metadata columns expected by the script's setcolorder
  # -----------------------------------------------------------------------
  # socioeconomic_var and group_type are passed in as human-readable labels
  # (e.g. "education" / "quintile") rather than derived, for clear output.
  for (tbl in list(ci, summary)) {
    tbl[, `:=`(
      city              = city,
      city_id           = city_id,
      year              = analysis_year,
      buffer_km         = buffer_km,
      socioeconomic_var = socioeconomic_var,
      group_type        = group_type
    )]
  }
  
  list(ci = ci[], summary = summary[])
}


# --------------------------------------------------------------------------------------------
# Function: compute_exposure_ci_regression
#
# @Arg exposure_parquet   : string; path to geo-level IDW exposure Parquet.
# @Arg individual_pq      : string or NULL; path to individual group Parquet.
# @Arg geo_id_col         : string; geographic ID column. Default "geo_id".
# @Arg pop_col            : string; population or expansion-weight column.
# @Arg group_col          : string; group variable, e.g. "edu_quintile".
# @Arg group_values       : integer/numeric vector; valid groups, e.g. 1:5.
# @Arg base_group         : integer/numeric; omitted reference group.
# @Arg pollutants         : character; pollutants to include.
# @Arg outcome_pattern    : string; regex for outcome columns.
# @Arg year_filter        : integer or NULL; restrict to one year.
# @Arg conf_level         : numeric; confidence level. Default 0.95.
# @Arg normalized         : logical; divide by base group mean and report gaps.
# @Arg regression_unit    : string; "geo_group", "individual", or "geo".
# @Arg se_type            : string; "classic", "HC1", or "cluster_geo".
# @Arg quiet              : logical; suppress messages. Default FALSE.
#
# @Output : data.table with estimates, standard errors, CIs, and metadata.
#
# @Details:
#   Estimates exposure differences across socioeconomic groups. The default
#   "geo_group" mode reproduces the legacy paper estimator by collapsing the
#   merged individual-exposure data to geographic-unit-by-group cells and
#   weighting each cell by its share of total group population.
#
# @Written_by: Marcos Paulo
# @Updated_on: May 2026
# --------------------------------------------------------------------------------------------
compute_exposure_ci_regression <- function(
    exposure_parquet,
    individual_pq   = NULL,
    geo_id_col      = "geo_id",
    pop_col         = "n",
    group_col       = "edu_quintile",
    group_values    = 1:5,
    base_group      = max(group_values),
    pollutants      = c("pm10", "pm25"),
    outcome_pattern = "^(avg|hrs_d)_",
    year_filter     = NULL,
    conf_level      = 0.95,
    normalized      = FALSE,
    regression_unit = c("geo_group", "individual", "geo"),
    se_type         = c("classic", "HC1", "cluster_geo"),
    quiet           = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  pkgs <- c("arrow", "data.table")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required.")
    }
  }
  
  se_type <- match.arg(se_type)
  regression_unit <- match.arg(regression_unit)
  
  if (se_type %in% c("HC1", "cluster_geo") &&
      !requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' required for robust or clustered SEs.")
  }
  
  if (!file.exists(exposure_parquet)) {
    stop("`exposure_parquet` not found: ", exposure_parquet)
  }
  
  if (!(conf_level > 0 && conf_level < 1)) {
    stop("`conf_level` must be between 0 and 1.")
  }
  
  if (!base_group %in% group_values) {
    stop("`base_group` must belong to `group_values`.")
  }
  
  # 1. Read exposure data
  # -----------------------------------------------------------------------
  dt <- data.table::as.data.table(arrow::read_parquet(exposure_parquet))
  
  if (!is.null(year_filter)) {
    dt <- dt[year == year_filter]
  }
  
  if (!geo_id_col %in% names(dt)) {
    stop("geo_id_col '", geo_id_col, "' not found in exposure file.")
  }
  
  dt[, (geo_id_col) := as.character(get(geo_id_col))]
  
  # 2. Merge individual/group data when required
  # -----------------------------------------------------------------------
  needs_individual <- regression_unit %in% c("geo_group", "individual")
  
  if (needs_individual) {
    if (is.null(individual_pq)) {
      stop("`individual_pq` required for regression_unit = ", regression_unit)
    }
    
    if (!file.exists(individual_pq)) {
      stop("`individual_pq` not found: ", individual_pq)
    }
    
    ind <- data.table::as.data.table(arrow::read_parquet(individual_pq))
    
    for (col in c(geo_id_col, group_col, pop_col)) {
      if (!col %in% names(ind)) {
        stop("Column '", col, "' not found in individual file.")
      }
    }
    
    ind[, (geo_id_col) := as.character(get(geo_id_col))]
    
    # Remove group column from exposure data to avoid merge conflicts.
    if (group_col %in% names(dt)) {
      dt[, (group_col) := NULL]
    }
    
    dt <- merge(
      dt,
      ind[, .SD, .SDcols = c(geo_id_col, group_col, pop_col)],
      by = geo_id_col,
      allow.cartesian = TRUE
    )
  } else {
    for (col in c(group_col, pop_col)) {
      if (!col %in% names(dt)) {
        stop("Column '", col, "' not found in exposure file.")
      }
    }
  }
  
  # Keep valid groups and positive weights only.
  dt <- dt[
    !is.na(get(group_col)) &
      get(group_col) %in% group_values &
      !is.na(get(pop_col)) &
      get(pop_col) > 0
  ]
  
  # 3. Identify outcomes
  # -----------------------------------------------------------------------
  all_cols <- grep(outcome_pattern, names(dt), value = TRUE)
  
  out_cols <- all_cols[
    vapply(
      all_cols,
      function(x) any(grepl(paste(pollutants, collapse = "|"), x)),
      logical(1)
    )
  ]
  
  if (length(out_cols) == 0L) {
    stop("No outcome columns match the requested pattern and pollutants.")
  }
  
  # Parse outcome and pollutant labels.
  .parse_outcome <- function(col) {
    pol <- pollutants[
      which(vapply(
        pollutants,
        function(p) grepl(p, col, fixed = TRUE),
        logical(1)
      ))[1L]
    ]
    
    oc <- sub(paste0("_", pol, "_?"), "_", col)
    oc <- sub("_$", "", oc)
    oc <- sub("^_", "", oc)
    
    list(outcome = oc, pollutant = pol)
  }
  
  # 4. Variance-covariance helper
  # -----------------------------------------------------------------------
  .vcov_model <- function(fit, d) {
    if (se_type == "classic") {
      return(stats::vcov(fit))
    }
    
    if (se_type == "HC1") {
      return(sandwich::vcovHC(fit, type = "HC1"))
    }
    
    if (se_type == "cluster_geo") {
      if (!".cluster_geo" %in% names(d)) {
        stop("Internal cluster column missing.")
      }
      
      return(sandwich::vcovCL(fit, cluster = d$.cluster_geo, type = "HC1"))
    }
  }
  
  # Extract coefficient table using selected vcov.
  .coef_table <- function(fit, d) {
    vc <- .vcov_model(fit, d)
    est <- stats::coef(fit)
    se <- sqrt(diag(vc))
    
    alpha <- 1 - conf_level
    crit <- stats::qnorm(1 - alpha / 2)
    
    data.table::data.table(
      term = names(est),
      estimate = unname(est),
      std_error = unname(se),
      ci_low = unname(est - crit * se),
      ci_high = unname(est + crit * se)
    )
  }
  
  # Base group row for plotting.
  .base_row <- function(meta, n_obs) {
    data.table::data.table(
      outcome = meta$outcome,
      pollutant = meta$pollutant,
      group = base_group,
      estimate = 0,
      std_error = 0,
      ci_low = 0,
      ci_high = 0,
      n_obs = n_obs,
      base_group = base_group,
      group_col = group_col,
      regression_unit = regression_unit,
      se_type = se_type
    )
  }
  
  # 5. Legacy-style geo-by-group estimator
  # -----------------------------------------------------------------------
  .fit_geo_group <- function(col) {
    d0 <- dt[!is.na(get(col))]
    
    if (nrow(d0) < length(group_values)) {
      return(NULL)
    }
    
    if (isTRUE(normalized)) {
      base_mean <- d0[
        get(group_col) == base_group,
        stats::weighted.mean(get(col), get(pop_col), na.rm = TRUE)
      ]
      
      if (is.na(base_mean) || base_mean == 0) {
        return(NULL)
      }
      
      d0[, y_model := get(col) / base_mean]
    } else {
      d0[, y_model := get(col)]
    }
    
    by_cols <- c(geo_id_col, group_col)
    
    # Collapse to geographic-unit-by-group cells.
    d <- d0[
      !is.na(y_model),
      .(
        geo_population = sum(get(pop_col), na.rm = TRUE),
        y = stats::weighted.mean(y_model, get(pop_col), na.rm = TRUE)
      ),
      by = by_cols
    ]
    
    d <- d[!is.na(y) & !is.na(geo_population) & geo_population > 0]
    
    if (nrow(d) < length(group_values)) {
      return(NULL)
    }
    
    # Weight each cell by its population share within the group.
    d[, total_population_g := sum(geo_population, na.rm = TRUE),
      by = group_col]
    
    d[, w := geo_population / total_population_g]
    d <- d[!is.na(w) & w > 0]
    
    if (nrow(d) < length(group_values)) {
      return(NULL)
    }
    
    # Store cluster ID for clustered standard errors.
    d[, .cluster_geo := get(geo_id_col)]
    
    d[, g := factor(
      get(group_col),
      levels = c(base_group, setdiff(group_values, base_group))
    )]
    
    fit <- stats::lm(y ~ g, data = d, weights = w)
    ct <- .coef_table(fit, d)
    
    meta <- .parse_outcome(col)
    out_rows <- list(.base_row(meta, nrow(d)))
    
    non_base <- setdiff(group_values, base_group)
    
    for (grp in non_base) {
      rn <- paste0("g", grp)
      row <- ct[term == rn]
      
      if (nrow(row) == 0L) {
        next
      }
      
      if (isTRUE(normalized)) {
        est <- row$estimate
        low <- row$ci_low
        high <- row$ci_high
      } else {
        # Non-normalized coefficient is a difference from the base group.
        est <- row$estimate
        low <- row$ci_low
        high <- row$ci_high
      }
      
      out_rows[[length(out_rows) + 1L]] <- data.table::data.table(
        outcome = meta$outcome,
        pollutant = meta$pollutant,
        group = grp,
        estimate = est,
        std_error = row$std_error,
        ci_low = low,
        ci_high = high,
        n_obs = nrow(d),
        base_group = base_group,
        group_col = group_col,
        regression_unit = regression_unit,
        se_type = se_type
      )
    }
    
    data.table::rbindlist(out_rows, fill = TRUE)
  }
  
  # 6. Individual or geo-level estimator
  # -----------------------------------------------------------------------
  .fit_standard <- function(col) {
    d <- dt[
      !is.na(get(col)),
      .(
        y = get(col),
        g = factor(
          get(group_col),
          levels = c(base_group, setdiff(group_values, base_group))
        ),
        w = get(pop_col),
        .cluster_geo = get(geo_id_col)
      )
    ]
    
    if (nrow(d) < length(group_values)) {
      return(NULL)
    }
    
    if (isTRUE(normalized)) {
      base_mean <- d[
        g == as.character(base_group),
        stats::weighted.mean(y, w, na.rm = TRUE)
      ]
      
      if (is.na(base_mean) || base_mean == 0) {
        return(NULL)
      }
      
      d[, y := y / base_mean]
    }
    
    fit <- stats::lm(y ~ g, data = d, weights = w)
    ct <- .coef_table(fit, d)
    
    meta <- .parse_outcome(col)
    out_rows <- list(.base_row(meta, nrow(d)))
    
    non_base <- setdiff(group_values, base_group)
    
    for (grp in non_base) {
      rn <- paste0("g", grp)
      row <- ct[term == rn]
      
      if (nrow(row) == 0L) {
        next
      }
      
      out_rows[[length(out_rows) + 1L]] <- data.table::data.table(
        outcome = meta$outcome,
        pollutant = meta$pollutant,
        group = grp,
        estimate = row$estimate,
        std_error = row$std_error,
        ci_low = row$ci_low,
        ci_high = row$ci_high,
        n_obs = nrow(d),
        base_group = base_group,
        group_col = group_col,
        regression_unit = regression_unit,
        se_type = se_type
      )
    }
    
    data.table::rbindlist(out_rows, fill = TRUE)
  }
  
  # 7. Fit all outcomes
  # -----------------------------------------------------------------------
  fit_fun <- switch(
    regression_unit,
    geo_group  = .fit_geo_group,
    individual = .fit_standard,
    geo        = .fit_standard
  )
  
  res <- data.table::rbindlist(lapply(out_cols, fit_fun), fill = TRUE)
  
  if (nrow(res) == 0L) {
    if (!quiet) {
      message("[ci] Warning: insufficient data to fit any models.")
    }
    
    return(data.table::data.table(
      outcome = character(),
      pollutant = character(),
      group = numeric(),
      estimate = numeric(),
      std_error = numeric(),
      ci_low = numeric(),
      ci_high = numeric(),
      n_obs = integer(),
      base_group = numeric(),
      group_col = character(),
      regression_unit = character(),
      se_type = character()
    ))
  }
  
  data.table::setorder(res, outcome, pollutant, group)
  
  if (!quiet) {
    message(
      "[ci] ", length(out_cols), " outcome(s) fit using regression_unit = '",
      regression_unit, "' and se_type = '", se_type, "'."
    )
  }
  
  return(res)
}


# --------------------------------------------------------------------------------------------
# Function: compute_exposure_group_summaries
#
# @Arg exposure_parquet : string; path to geo-level IDW exposure Parquet.
# @Arg individual_pq    : string; path to individual-level group Parquet.
# @Arg geo_id_col       : string; geographic ID column. Default "geo_id".
# @Arg pop_col          : string; population or expansion-weight column.
# @Arg group_col        : string; group variable, e.g. "edu_quintile".
# @Arg group_values     : numeric vector; valid groups, e.g. 1:5 or 1:10.
# @Arg pollutants       : character; pollutants to include.
# @Arg outcome_pattern  : string; regex for outcomes to summarize.
# @Arg year_filter      : integer or NULL; restrict to a year.
# @Arg quiet            : logical; suppress messages. Default FALSE.
#
# @Output : data.table with weighted means, weighted medians, and counts.
#
# @Details:
#   Merges geo-level exposure with individual/group data and computes weighted
#   group-level summaries. Since exposure is constant within geographic unit,
#   the function first collapses to geo-by-group cells to avoid unnecessary
#   individual-level expansion in the final summaries.
#
# @Written_by: Marcos Paulo
# @Updated_on: May 2026
# --------------------------------------------------------------------------------------------
compute_exposure_group_summaries <- function(
    exposure_parquet,
    individual_pq,
    geo_id_col      = "geo_id",
    pop_col         = "n",
    group_col       = "edu_quintile",
    group_values    = 1:5,
    pollutants      = c("pm10", "pm25"),
    outcome_pattern = "^(avg|hrs_d)_",
    year_filter     = NULL,
    quiet           = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  pkgs <- c("arrow", "data.table")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required.")
    }
  }
  
  if (!file.exists(exposure_parquet)) {
    stop("`exposure_parquet` not found: ", exposure_parquet)
  }
  
  if (!file.exists(individual_pq)) {
    stop("`individual_pq` not found: ", individual_pq)
  }
  
  # 1. Read and merge data
  # -----------------------------------------------------------------------
  exp_dt <- data.table::as.data.table(arrow::read_parquet(exposure_parquet))
  ind_dt <- data.table::as.data.table(arrow::read_parquet(individual_pq))
  
  if (!is.null(year_filter)) {
    exp_dt <- exp_dt[year == year_filter]
  }
  
  for (col in c(geo_id_col)) {
    if (!col %in% names(exp_dt)) {
      stop("Column '", col, "' missing in exposure file.")
    }
  }
  
  for (col in c(geo_id_col, pop_col, group_col)) {
    if (!col %in% names(ind_dt)) {
      stop("Column '", col, "' missing in individual file.")
    }
  }
  
  exp_dt[, (geo_id_col) := as.character(get(geo_id_col))]
  ind_dt[, (geo_id_col) := as.character(get(geo_id_col))]
  
  # Keep necessary individual columns.
  ind_dt <- ind_dt[
    !is.na(get(group_col)) &
      get(group_col) %in% group_values &
      !is.na(get(pop_col)) &
      get(pop_col) > 0,
    .SD,
    .SDcols = c(geo_id_col, group_col, pop_col)
  ]
  
  dt <- merge(exp_dt, ind_dt, by = geo_id_col, allow.cartesian = TRUE)
  
  # 2. Identify outcomes
  # -----------------------------------------------------------------------
  all_cols <- grep(outcome_pattern, names(dt), value = TRUE)
  
  out_cols <- all_cols[
    vapply(
      all_cols,
      function(x) any(grepl(paste(pollutants, collapse = "|"), x)),
      logical(1)
    )
  ]
  
  if (length(out_cols) == 0L) {
    stop("No outcome columns match the requested pattern and pollutants.")
  }
  
  # Weighted median helper.
  .weighted_median <- function(x, w) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    
    if (!any(ok)) {
      return(NA_real_)
    }
    
    x <- x[ok]
    w <- w[ok]
    ord <- order(x)
    x <- x[ord]
    w <- w[ord]
    
    cw <- cumsum(w) / sum(w)
    x[which(cw >= 0.5)[1L]]
  }
  
  # Parse outcome and pollutant labels.
  .parse_outcome <- function(col) {
    pol <- pollutants[
      which(vapply(
        pollutants,
        function(p) grepl(p, col, fixed = TRUE),
        logical(1)
      ))[1L]
    ]
    
    oc <- sub(paste0("_", pol, "_?"), "_", col)
    oc <- sub("_$", "", oc)
    oc <- sub("^_", "", oc)
    
    list(outcome = oc, pollutant = pol)
  }
  
  # 3. Collapse to geo-by-group cells
  # -----------------------------------------------------------------------
  by_cols <- c(geo_id_col, group_col)
  
  geo_group <- dt[
    ,
    c(
      list(group_pop = sum(get(pop_col), na.rm = TRUE)),
      lapply(.SD, function(x) {
        stats::weighted.mean(x, get(pop_col), na.rm = TRUE)
      })
    ),
    by = by_cols,
    .SDcols = out_cols
  ]
  
  geo_group <- geo_group[!is.na(group_pop) & group_pop > 0]
  
  # 4. Compute group-level summaries
  # -----------------------------------------------------------------------
  res_list <- vector("list", length(out_cols))
  
  for (i in seq_along(out_cols)) {
    col <- out_cols[[i]]
    meta <- .parse_outcome(col)
    
    tmp <- geo_group[
      !is.na(get(col)),
      .(
        weighted_mean = stats::weighted.mean(
          get(col),
          group_pop,
          na.rm = TRUE
        ),
        weighted_median = .weighted_median(get(col), group_pop),
        weighted_population = sum(group_pop, na.rm = TRUE),
        n_geo_group_cells = .N,
        n_geo_units = data.table::uniqueN(get(geo_id_col))
      ),
      by = group_col
    ]
    
    data.table::setnames(tmp, group_col, "group")
    
    tmp[, `:=`(
      outcome = meta$outcome,
      pollutant = meta$pollutant,
      group_col = group_col
    )]
    
    res_list[[i]] <- tmp
  }
  
  res <- data.table::rbindlist(res_list, fill = TRUE)
  data.table::setorder(res, outcome, pollutant, group)
  
  if (!quiet) {
    message("[summary] ", length(out_cols), " outcome(s) summarized.")
  }
  
  return(res)
}


# --------------------------------------------------------------------------------------------
# Function: run_exposure_results_for_city
#
# @Arg spec_row          : one-row data.table with city inputs and metadata.
# @Arg analysis_year     : integer; year used in the regression and summaries.
# @Arg buffer_km         : integer/numeric; buffer used in the exposure file.
# @Arg group_col         : string; socioeconomic group variable.
# @Arg group_values      : numeric vector; valid group values.
# @Arg base_group        : numeric; omitted reference group in regressions.
# @Arg pollutants        : character vector; pollutants to include.
# @Arg summary_outcomes  : string; regex for raw summary outcomes.
# @Arg ci_outcomes       : string; regex for regression outcomes.
# @Arg confidence_level  : numeric; confidence level for intervals.
# @Arg main_reg_unit     : string; regression unit passed to compute function.
# @Arg main_se_type      : string; standard-error type passed to compute function.
# @Arg normalized        : logical; whether to normalize by base-group mean.
# @Arg validate_inputs   : logical; whether to read and check inputs before estimation.
#
# @Output : list with two data.tables: ci and summary.
#
# @Details:
#   Runs the two final exposure-output routines for a single city. The raw summary
#   reports weighted exposure values by socioeconomic group. The regression output
#   reports differences relative to the base group. The preferred paper
#   specification uses geo_id x education-quintile cells with geo-clustered
#   standard errors.
#
# @Written_by: Marcos Paulo
# @Updated_on: May 2026
# --------------------------------------------------------------------------------------------
run_exposure_results_for_city <- function(
    spec_row,
    analysis_year,
    buffer_km,
    group_col,
    group_values,
    base_group,
    pollutants,
    summary_outcomes,
    ci_outcomes,
    confidence_level = 0.95,
    main_reg_unit = "geo_group",
    main_se_type = "cluster_geo",
    normalized = TRUE,
    validate_inputs = TRUE
) {
  
  # 0. Basic checks
  # ------------------------------------------------------------------------------------------
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required.")
  }
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }
  
  if (nrow(spec_row) != 1L) {
    stop("`spec_row` must contain exactly one city.")
  }
  
  needed_spec_cols <- c(
    "city",
    "city_id",
    "exposure_parquet",
    "individual_pq",
    "pop_col"
  )
  
  missing_spec_cols <- setdiff(needed_spec_cols, names(spec_row))
  
  if (length(missing_spec_cols) > 0L) {
    stop(
      "Missing column(s) in `spec_row`: ",
      paste(missing_spec_cols, collapse = ", ")
    )
  }
  
  if (!file.exists(spec_row$exposure_parquet)) {
    stop("Exposure file not found: ", spec_row$exposure_parquet)
  }
  
  if (!file.exists(spec_row$individual_pq)) {
    stop("Individual/group file not found: ", spec_row$individual_pq)
  }
  
  message("Processing exposure results for ", spec_row$city, "...")
  
  # 1. Optional validation of input data
  # ------------------------------------------------------------------------------------------
  if (isTRUE(validate_inputs)) {
    exposure_dt <- data.table::as.data.table(
      arrow::read_parquet(spec_row$exposure_parquet)
    )
    
    individual_dt <- data.table::as.data.table(
      arrow::read_parquet(spec_row$individual_pq)
    )
    
    required_exp_cols <- c("geo_id", "year")
    required_ind_cols <- c("geo_id", group_col, spec_row$pop_col)
    
    missing_exp_cols <- setdiff(required_exp_cols, names(exposure_dt))
    missing_ind_cols <- setdiff(required_ind_cols, names(individual_dt))
    
    if (length(missing_exp_cols) > 0L) {
      stop(
        "Missing exposure column(s) for ", spec_row$city, ": ",
        paste(missing_exp_cols, collapse = ", ")
      )
    }
    
    if (length(missing_ind_cols) > 0L) {
      stop(
        "Missing individual column(s) for ", spec_row$city, ": ",
        paste(missing_ind_cols, collapse = ", ")
      )
    }
    
    rm(exposure_dt, individual_dt)
  }
  
  # 2. Regression coefficients and confidence intervals
  # ------------------------------------------------------------------------------------------
  ci_dt <- compute_exposure_ci_regression(
    exposure_parquet = spec_row$exposure_parquet,
    individual_pq    = spec_row$individual_pq,
    geo_id_col       = "geo_id",
    pop_col          = spec_row$pop_col,
    group_col        = group_col,
    group_values     = group_values,
    base_group       = base_group,
    pollutants       = pollutants,
    outcome_pattern  = ci_outcomes,
    year_filter      = analysis_year,
    conf_level       = confidence_level,
    normalized       = normalized,
    regression_unit  = main_reg_unit,
    se_type          = main_se_type,
    quiet            = FALSE
  )
  
  # 3. Raw weighted exposure values by group
  # ------------------------------------------------------------------------------------------
  summary_dt <- compute_exposure_group_summaries(
    exposure_parquet = spec_row$exposure_parquet,
    individual_pq    = spec_row$individual_pq,
    geo_id_col       = "geo_id",
    pop_col          = spec_row$pop_col,
    group_col        = group_col,
    group_values     = group_values,
    pollutants       = pollutants,
    outcome_pattern  = summary_outcomes,
    year_filter      = analysis_year,
    quiet            = FALSE
  )
  
  # 4. Add common metadata
  # ------------------------------------------------------------------------------------------
  common_meta <- list(
    city = spec_row$city,
    city_id = spec_row$city_id,
    buffer_km = buffer_km,
    year = analysis_year,
    socioeconomic_var = "education",
    group_type = "quintile"
  )
  
  ci_dt[, names(common_meta) := common_meta]
  summary_dt[, names(common_meta) := common_meta]
  
  return(list(ci = ci_dt, summary = summary_dt))
}


# --------------------------------------------------------------------------------------------
# Function: save_raw_data_tidy_formatted
#
# @Arg       : data          - data.frame or tibble to write.
# @Arg       : out_dir       - string; directory to write outputs (created if missing).
# @Arg       : out_name      - string|NULL; base filename without extension. If NULL,
#                              inferred from available columns ('city' and 'year').
# @Arg       : write_rds     - logical; write .rds (default TRUE).
# @Arg       : write_parquet - logical; write .parquet via {arrow} (default TRUE).
# @Arg       : write_csv_gz  - logical; write .csv.gz (default FALSE).
# @Arg       : rds_compress  - string; RDS compress method (default "xz").
# @Arg       : parquet_comp  - string; Parquet compression codec (default "zstd").
# @Arg       : quiet         - logical; suppress messages (default FALSE).
#
# @Output    : (invisible) Named list containing the written file paths.
# @Purpose   : Materializes a dataframe to standard formats with consistent naming.
# @Written_on: 27/08/2025
# @Written_by: Marcos Paulo
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
# Small shared helpers (top-level, individually testable)
# --------------------------------------------------------------------------------------------

# Parse an outcome column name into its outcome label and pollutant.
# Example: "hrs_d_pm25_it1" -> list(outcome = "hrs_d_it1", pollutant = "pm25").
parse_outcome_label <- function(col, pollutants) {
  # Find which pollutant string appears in the column name.
  hit <- which(vapply(
    pollutants,
    function(p) grepl(p, col, fixed = TRUE),
    logical(1)
  ))[1L]
  
  pol <- pollutants[hit]
  
  # Strip the pollutant token and tidy leftover underscores.
  oc <- sub(paste0("_", pol, "_?"), "_", col)
  oc <- sub("_$", "", oc)
  oc <- sub("^_", "", oc)
  
  list(outcome = oc, pollutant = pol)
}

# Select outcome columns by regex pattern AND pollutant membership.
select_outcome_cols <- function(dt_names, outcome_pattern, pollutants) {
  # First filter by the outcome regex (e.g. annual means or exceedance hours).
  candidate <- grep(outcome_pattern, dt_names, value = TRUE)
  
  # Then keep only those mentioning a requested pollutant.
  keep <- vapply(
    candidate,
    function(x) any(grepl(paste(pollutants, collapse = "|"), x)),
    logical(1)
  )
  
  candidate[keep]
}

# Weighted median used by the group summaries.
weighted_median_value <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  # Sort values, carry weights, and take the 50% cumulative-weight point.
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1L]]
}

# Build the variance-covariance matrix for a fitted lm under the chosen SE type.
build_vcov <- function(fit, model_dt, se_type) {
  if (se_type == "classic") {
    return(stats::vcov(fit))
  }
  
  if (se_type == "HC1") {
    return(sandwich::vcovHC(fit, type = "HC1"))
  }
  
  # Geo-clustered SEs require a cluster column placed on the model frame.
  if (se_type == "cluster_geo") {
    if (!".cluster_geo" %in% names(model_dt)) {
      stop("Internal cluster column '.cluster_geo' missing.")
    }
    
    return(sandwich::vcovCL(fit, cluster = model_dt$.cluster_geo, type = "HC1"))
  }
  
  stop("Unknown se_type: ", se_type)
}

# Turn a fitted lm + vcov into a tidy term/estimate/CI table.
coef_table_from_fit <- function(fit, model_dt, se_type, conf_level) {
  vc  <- build_vcov(fit, model_dt, se_type)
  est <- stats::coef(fit)
  se  <- sqrt(diag(vc))
  
  alpha <- 1 - conf_level
  
  # Critical value choice:
  #  - "classic": t-distribution on the model's residual df, matching the legacy
  #    confint() on lm() exactly (the paper's inference).
  #  - robust / clustered: normal approximation, the usual sandwich convention.
  if (se_type == "classic") {
    crit <- stats::qt(1 - alpha / 2, df = stats::df.residual(fit))
  } else {
    crit <- stats::qnorm(1 - alpha / 2)
  }
  
  data.table::data.table(
    term      = names(est),
    estimate  = unname(est),
    std_error = unname(se),
    ci_low    = unname(est - crit * se),
    ci_high   = unname(est + crit * se)
  )
}

# A zero-valued row for the omitted base group, so plots show all groups.
# n_units is the number of rows in the fitted model frame. Its meaning depends on
# regression_unit (the adjacent column): geo_group -> geo-by-group cells;
# individual -> individuals; geo -> geo units. It is identical across groups
# within one outcome, so it is a diagnostic, not a per-group sample size.
base_group_row <- function(meta, n_units, base_group, group_col,
                           regression_unit, se_type) {
  data.table::data.table(
    outcome         = meta$outcome,
    pollutant       = meta$pollutant,
    group           = base_group,
    estimate        = 0,
    std_error       = 0,
    ci_low          = 0,
    ci_high         = 0,
    n_units         = n_units,
    base_group      = base_group,
    group_col       = group_col,
    regression_unit = regression_unit,
    se_type         = se_type
  )
}

# ------------------------------------------------------------------------------------
# Estimator 1: legacy geo-by-group estimator (the paper's main spec)
#
# Collapses merged individual-exposure data to geographic-unit-by-group cells and
# weights each cell by its share of total group population. Returns one tidy table
# for a single outcome column, or NULL if there is not enough data.
# ------------------------------------------------------------------------------------
fit_geo_group_outcome <- function(merged_dt, col, geo_id_col, group_col, pop_col,
                                  group_values, base_group, pollutants,
                                  se_type, conf_level, normalized) {
  d0 <- merged_dt[!is.na(get(col))]
  
  if (nrow(d0) < length(group_values)) {
    return(NULL)
  }
  
  # Optionally normalize the outcome by the base-group weighted mean (gap form).
  if (isTRUE(normalized)) {
    base_mean <- d0[
      get(group_col) == base_group,
      stats::weighted.mean(get(col), get(pop_col), na.rm = TRUE)
    ]
    
    if (is.na(base_mean) || base_mean == 0) {
      return(NULL)
    }
    
    d0[, y_model := get(col) / base_mean]
  } else {
    d0[, y_model := get(col)]
  }
  
  by_cols <- c(geo_id_col, group_col)
  
  # Collapse to geo-by-group cells (exposure is constant within a geo unit).
  d <- d0[
    !is.na(y_model),
    .(
      geo_population = sum(get(pop_col), na.rm = TRUE),
      y = stats::weighted.mean(y_model, get(pop_col), na.rm = TRUE)
    ),
    by = by_cols
  ]
  
  d <- d[!is.na(y) & !is.na(geo_population) & geo_population > 0]
  
  if (nrow(d) < length(group_values)) {
    return(NULL)
  }
  
  # Each cell is weighted by its population share within its group.
  d[, total_population_g := sum(geo_population, na.rm = TRUE), by = group_col]
  d[, w := geo_population / total_population_g]
  d <- d[!is.na(w) & w > 0]
  
  if (nrow(d) < length(group_values)) {
    return(NULL)
  }
  
  # Cluster ID column for geo-clustered standard errors.
  d[, .cluster_geo := get(geo_id_col)]
  
  # Factor with the base group first so it becomes the omitted reference.
  d[, g := factor(
    get(group_col),
    levels = c(base_group, setdiff(group_values, base_group))
  )]
  
  fit <- stats::lm(y ~ g, data = d, weights = w)
  ct  <- coef_table_from_fit(fit, d, se_type, conf_level)
  
  meta <- parse_outcome_label(col, pollutants)
  
  # Start the output with the base-group reference row.
  out_rows <- list(
    base_group_row(meta, nrow(d), base_group, group_col, "geo_group", se_type)
  )
  
  # Add one row per non-base group from the coefficient table.
  for (grp in setdiff(group_values, base_group)) {
    row <- ct[term == paste0("g", grp)]
    
    if (nrow(row) == 0L) {
      next
    }
    
    out_rows[[length(out_rows) + 1L]] <- data.table::data.table(
      outcome         = meta$outcome,
      pollutant       = meta$pollutant,
      group           = grp,
      estimate        = row$estimate,
      std_error       = row$std_error,
      ci_low          = row$ci_low,
      ci_high         = row$ci_high,
      n_units         = nrow(d),
      base_group      = base_group,
      group_col       = group_col,
      regression_unit = "geo_group",
      se_type         = se_type
    )
  }
  
  data.table::rbindlist(out_rows, fill = TRUE)
}

# ------------------------------------------------------------------------------------
# Estimator 2: standard estimator for "individual" or "geo" regression units
#
# Fits the outcome directly (no geo-by-group collapse). Used for robustness checks.
# ------------------------------------------------------------------------------------
fit_standard_outcome <- function(model_source_dt, col, geo_id_col, group_col, pop_col,
                                 group_values, base_group, pollutants,
                                 regression_unit, se_type, conf_level, normalized) {
  d <- model_source_dt[
    !is.na(get(col)),
    .(
      y = get(col),
      g = factor(
        get(group_col),
        levels = c(base_group, setdiff(group_values, base_group))
      ),
      w = get(pop_col),
      .cluster_geo = get(geo_id_col)
    )
  ]
  
  if (nrow(d) < length(group_values)) {
    return(NULL)
  }
  
  # Optional normalization by base-group weighted mean.
  if (isTRUE(normalized)) {
    base_mean <- d[
      g == as.character(base_group),
      stats::weighted.mean(y, w, na.rm = TRUE)
    ]
    
    if (is.na(base_mean) || base_mean == 0) {
      return(NULL)
    }
    
    d[, y := y / base_mean]
  }
  
  fit <- stats::lm(y ~ g, data = d, weights = w)
  ct  <- coef_table_from_fit(fit, d, se_type, conf_level)
  
  meta <- parse_outcome_label(col, pollutants)
  
  out_rows <- list(
    base_group_row(meta, nrow(d), base_group, group_col, regression_unit, se_type)
  )
  
  for (grp in setdiff(group_values, base_group)) {
    row <- ct[term == paste0("g", grp)]
    
    if (nrow(row) == 0L) {
      next
    }
    
    out_rows[[length(out_rows) + 1L]] <- data.table::data.table(
      outcome         = meta$outcome,
      pollutant       = meta$pollutant,
      group           = grp,
      estimate        = row$estimate,
      std_error       = row$std_error,
      ci_low          = row$ci_low,
      ci_high         = row$ci_high,
      n_units         = nrow(d),
      base_group      = base_group,
      group_col       = group_col,
      regression_unit = regression_unit,
      se_type         = se_type
    )
  }
  
  data.table::rbindlist(out_rows, fill = TRUE)
}

# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")