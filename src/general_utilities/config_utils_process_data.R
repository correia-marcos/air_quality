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
# @Arg distance_metric      : string; "aeqd", "haversine", or "geosphere".
# @Arg representative_point : string; "point_on_surface", "math_centroid",
#                             or "math_centroid_legacy".
# @Arg overwrite            : logical; skip if output exists. Default TRUE.
# @Arg quiet                : logical; suppress messages. Default FALSE.
#
# @Output : Named list of data.tables for station and geo distances.
# @Details:
#   Calculates station-to-station and geo-to-station distance matrices. If
#   representative_point = "point_on_surface", it uses an internal point returned by
#   st_point_on_surface() for every geo unit. If representative_point = "math_centroid",
#   it uses st_centroid() with an internal-point fallback for polygons whose centroid is
#   outside. If representative_point = "math_centroid_legacy", it uses plain
#   st_centroid() with no fallback. The function also linearizes curved geometries
#   such as MULTISURFACE/CURVEPOLYGON before validity repair and distance calculation.
#   The "geosphere" metric is included for legacy replication because the old
#   station-distance scripts used geosphere::distm(..., fun = distHaversine).
#
#   Distance metrics:
#     - "geosphere" : great-circle Haversine via geosphere::distm. Legacy-matching
#                     metric for the station-to-station matrix.
#     - "haversine" : spherical (S2) great-circle via sf::st_distance on WGS84.
#                     Legacy-matching metric for the geo-to-station matrix.
#     - "aeqd"      : planar distance in an Azimuthal Equidistant projection centered
#                     on the data extent. Most accurate for metro-scale work; this is
#                     the intended/updated metric.
#
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
    distance_metric      = c("aeqd", "haversine", "geosphere"),
    representative_point = c("point_on_surface", "math_centroid",
                             "math_centroid_legacy"),
    overwrite            = TRUE,
    quiet                = FALSE
) {
  
  # 0. Match requested methods
  # -----------------------------------------------------------------------
  dist_metric <- match.arg(distance_metric)
  representative_point <- match.arg(representative_point)
  
  # 1. Check required packages
  # -----------------------------------------------------------------------
  pkgs <- c("sf", "data.table", "arrow", "stringi")
  
  if (dist_metric == "geosphere") {
    pkgs <- c(pkgs, "geosphere")
  }
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' required. Add to renv.")
    }
  }
  
  # 2. Inner helpers
  # -----------------------------------------------------------------------
  # Normalize station IDs: uppercase, strip accents and quotes.
  .normalize <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  # Build an AEQD proj4 string centered on a WGS84 lon/lat.
  .aeqd_proj <- function(lon0, lat0) {
    sprintf(
      "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
      lat0, lon0
    )
  }
  
  # Linearize curved geometries and prepare polygon layer.
  .prepare_geo_geometry <- function(x) {
    
    # Drop Z/M dimensions if present.
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    
    # Detect curved or surface geometry types.
    geom_types <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
    
    has_curves <- any(
      grepl("CURVE|SURFACE|CIRCULAR|COMPOUND", geom_types,
            ignore.case = TRUE)
    )
    
    # GDAL linearization avoids st_make_valid() failures on CURVEPOLYGON.
    if (has_curves) {
      tmp_in  <- tempfile("geo_curved_", fileext = ".gpkg")
      tmp_out <- tempfile("geo_linear_", fileext = ".gpkg")
      
      on.exit(unlink(c(tmp_in, tmp_out), recursive = TRUE, force = TRUE),
              add = TRUE)
      
      sf::st_write(
        x,
        tmp_in,
        layer = "geo",
        delete_dsn = TRUE,
        quiet = TRUE
      )
      
      sf::gdal_utils(
        util = "vectortranslate",
        source = tmp_in,
        destination = tmp_out,
        options = c(
          "-f", "GPKG",
          "-nlt", "CONVERT_TO_LINEAR",
          "-nln", "geo"
        )
      )
      
      x <- sf::st_read(tmp_out, layer = "geo", quiet = TRUE)
    }
    
    # Repair validity after curved geometries are converted.
    x <- sf::st_make_valid(x)
    
    # Extract polygonal components if validation creates collections.
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    
    # Promote to MULTIPOLYGON for stable downstream processing.
    x <- suppressWarnings(sf::st_cast(x, "MULTIPOLYGON", warn = FALSE))
    
    # Work in WGS84 after regularization.
    x <- sf::st_transform(x, crs = 4326)
    
    return(x)
  }
  
  # Create one representative point per polygon.
  .representative_points <- function(poly, method) {
    
    # Use guaranteed internal points for all polygons.
    if (method == "point_on_surface") {
      return(suppressWarnings(sf::st_point_on_surface(poly)))
    }
    
    # Legacy behavior: plain st_centroid() over all parts, no internal fallback.
    if (method == "math_centroid_legacy") {
      return(suppressWarnings(
        sf::st_centroid(poly, of_largest_polygon = FALSE)
      ))
    }
    
    # Calculate mathematical centroids first.
    cents <- suppressWarnings(
      sf::st_centroid(poly, of_largest_polygon = TRUE)
    )
    
    # Check whether centroid i intersects polygon i.
    inside_mat <- suppressWarnings(
      sf::st_intersects(cents, poly, sparse = FALSE)
    )
    
    # Extract diagonal relation: point i versus polygon i.
    is_inside <- as.logical(
      inside_mat[cbind(seq_len(nrow(poly)), seq_len(nrow(poly)))]
    )
    
    # Replace external centroids with guaranteed internal points.
    if (any(!is_inside, na.rm = TRUE)) {
      bad_idx <- which(!is_inside)
      
      cents[bad_idx, ] <- suppressWarnings(
        sf::st_point_on_surface(poly[bad_idx, ])
      )
    }
    
    return(cents)
  }
  
  # Calculate geosphere Haversine distances from two sf POINT objects.
  .geosphere_distance_km <- function(from_sf, to_sf) {
    
    # geosphere expects longitude-latitude coordinates in WGS84.
    from_wgs <- sf::st_transform(from_sf, crs = 4326)
    to_wgs   <- sf::st_transform(to_sf,   crs = 4326)
    
    # Extract coordinates as lon-lat matrices.
    from_xy <- sf::st_coordinates(from_wgs)[, c("X", "Y"), drop = FALSE]
    to_xy   <- sf::st_coordinates(to_wgs)[,   c("X", "Y"), drop = FALSE]
    
    # geosphere::distm returns meters.
    dist_m <- geosphere::distm(
      x = from_xy,
      y = to_xy,
      fun = geosphere::distHaversine
    )
    
    return(as.numeric(dist_m) / 1000)
  }
  
  # 3. Validate inputs
  # -----------------------------------------------------------------------
  if (!inherits(stations_sf, "sf")) {
    stop("`stations_sf` must be an sf object.")
  }
  
  if (!station_id_col %in% names(stations_sf)) {
    stop("Column '", station_id_col, "' not found.")
  }
  
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
  
  # 4. Output paths and early exit
  # -----------------------------------------------------------------------
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    
    if (!quiet) {
      message("Created output directory: ", out_dir)
    }
  }
  
  path_sta <- file.path(out_dir, paste0(out_name, "_station_distances.parquet"))
  path_geo <- file.path(out_dir, paste0(out_name, "_geo_station_distances.parquet"))
  
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
  
  # 5. Prepare stations
  # -----------------------------------------------------------------------
  # Enforce WGS84 and keep only the station ID column.
  stations_wgs <- sf::st_transform(stations_sf, crs = 4326)
  stations_wgs <- stations_wgs[, station_id_col]
  
  # Handle projection based on selected metric.
  if (dist_metric == "aeqd") {
    
    # Center AEQD on the full extent being measured: stations plus geo units
    # when geo_sf is supplied. Stations can sit outside the metro area (e.g.
    # inside a 20 km buffer) and geo units can extend past the station hull,
    # so we want the origin to sit in the middle of everything. We use the
    # midpoint of the COMBINED BOUNDING BOX rather than a polygon union: the
    # bbox already captures the farthest points in every direction and is far
    # cheaper than unioning thousands of census polygons.
    sta_bbox <- sf::st_bbox(stations_wgs)
    
    if (!is.null(geo_sf)) {
      geo_bbox <- sf::st_bbox(sf::st_transform(geo_sf, crs = 4326))
      
      # Outer envelope spanning both layers.
      xmin <- min(sta_bbox["xmin"], geo_bbox["xmin"])
      xmax <- max(sta_bbox["xmax"], geo_bbox["xmax"])
      ymin <- min(sta_bbox["ymin"], geo_bbox["ymin"])
      ymax <- max(sta_bbox["ymax"], geo_bbox["ymax"])
    } else {
      xmin <- sta_bbox["xmin"]; xmax <- sta_bbox["xmax"]
      ymin <- sta_bbox["ymin"]; ymax <- sta_bbox["ymax"]
    }
    
    # Bounding-box midpoint defines the AEQD origin.
    lon0 <- as.numeric((xmin + xmax) / 2)
    lat0 <- as.numeric((ymin + ymax) / 2)
    
    proj_aeqd <- .aeqd_proj(lon0 = lon0, lat0 = lat0)
    stations_eval <- sf::st_transform(stations_wgs, crs = proj_aeqd)
    
  } else {
    stations_eval <- stations_wgs
  }
  
  # Extract and normalize station IDs.
  station_ids <- .normalize(as.character(stations_wgs[[station_id_col]]))
  n_sta <- length(station_ids)
  
  # 6. Station-to-station distances
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[", out_name, "] Station distances.")
  }
  
  if (dist_metric == "geosphere") {
    dist_sta_km <- .geosphere_distance_km(stations_wgs, stations_wgs)
    
  } else {
    # "aeqd" (meters) or "haversine" (S2 meters).
    dist_sta_raw <- as.numeric(
      sf::st_distance(stations_eval, stations_eval)
    )
    
    dist_sta_km <- dist_sta_raw / 1000
  }
  
  # Generate a data table with stations and distances. The square matrix is
  # unrolled column-major, so station_to (column) varies slowest (each) and
  # station_from (row) varies fastest (times).
  station_dt <- data.table::data.table(
    station_from = rep(station_ids, times = n_sta),
    station_to   = rep(station_ids, each  = n_sta),
    distance_km  = dist_sta_km
  )
  
  if (!quiet) {
    message("[", out_name, "] Writing: ", path_sta)
  }
  
  arrow::write_parquet(station_dt, path_sta)
  
  geo_station_dt <- NULL
  
  # 7. Geo-to-station distances
  # -----------------------------------------------------------------------
  if (!is.null(geo_sf)) {
    
    if (!quiet) {
      message("[", out_name, "] Geo distances.")
    }
    
    # Linearize curved geometries, fix validity, and transform to WGS84.
    geo_wgs <- .prepare_geo_geometry(geo_sf)
    
    # Extract one representative point per geographic unit.
    geo_points <- .representative_points(
      poly = geo_wgs,
      method = representative_point
    )
    
    # Apply AEQD projection if requested.
    if (dist_metric == "aeqd") {
      geo_eval <- sf::st_transform(geo_points, crs = proj_aeqd)
    } else {
      geo_eval <- geo_points
    }
    
    # Extract geographic unit IDs after geometry preparation.
    geo_ids <- as.character(geo_points[[geo_id_col]])

    # Validity repair can split a unit into extra rows; stop before double-counting.
    if (anyDuplicated(geo_ids) > 0) {
      dup_ids <- unique(geo_ids[duplicated(geo_ids)])
      stop("Duplicated geo ids after geometry preparation: ",
           paste(dup_ids, collapse = ", "))
    }

    n_geo   <- length(geo_ids)
    
    # Calculate distances from representative points to stations. Both paths
    # produce an n_geo x n_sta matrix unrolled column-major (geo rows fastest,
    # station columns slowest).
    if (dist_metric == "geosphere") {
      dist_geo_km <- .geosphere_distance_km(geo_points, stations_wgs)
      
    } else {
      # "aeqd" (meters) or "haversine" (S2 meters).
      dist_geo_raw <- as.numeric(
        sf::st_distance(geo_eval, stations_eval)
      )
      
      dist_geo_km <- dist_geo_raw / 1000
    }
    
    # Generate a data table with geo ids, stations and distances. To match the
    # column-major unroll, geo_id (row) varies fastest -> times = n_sta, and
    # station_id (column) varies slowest -> each = n_geo.
    geo_station_dt <- data.table::data.table(
      geo_id      = rep(geo_ids,     times = n_sta),
      station_id  = rep(station_ids, each  = n_geo),
      distance_km = dist_geo_km
    )
    
    if (!quiet) {
      message("[", out_name, "] Writing: ", path_geo)
    }
    
    arrow::write_parquet(geo_station_dt, path_geo)
  }
  
  # 8. Return both matrices invisibly
  # -----------------------------------------------------------------------
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
# @Arg census_col          : data.frame; census data used for group assignment.
# @Arg geo_id_col          : string; geographic ID column in census_col.
# @Arg pop_col             : string; population or expansion-weight column.
# @Arg group_var           : string; continuous variable used to define groups
#                            (e.g. "escolaridad_avg" or "income").
# @Arg n_groups            : integer; number of equal-population groups (5 or 10).
# @Arg group_name          : string; output group column name
#                            (e.g. "edu_quintile" or "income_decile").
# @Arg quintile_level      : string; "geo" or "individual". Default "geo".
# @Arg indiv_adult_col     : string; adult filter column. Default "adult".
# @Arg buffer_km           : numeric; maximum geo-to-station distance. Default 3.
# @Arg distance_power      : numeric; IDW distance exponent. Default 1.
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
# @Arg edu_col             : string; deprecated alias for group_var, kept so old
#                            calls still run. Used only if group_var is NULL.
#
# @Output : Named list with saved file paths and, optionally, data.tables.
#
# @Details:
#   Aggregates hourly station pollution to geographic units using missingness-aware
#   inverse-distance weighting. For each geo-hour-pollutant cell, only stations
#   within buffer_km and with non-missing readings enter the numerator and
#   denominator. Socioeconomic groups are assigned generically: any continuous
#   group_var is cut into n_groups equal-population bins (group 1 = lowest), so the
#   same code path serves education quintiles and income deciles. One grouping is
#   produced per call (run separately for edu_quintile and income_decile).
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
    group_var           = NULL,
    n_groups            = 5L,
    group_name          = "edu_quintile",
    quintile_level      = c("geo", "individual"),
    indiv_adult_col     = "adult",
    buffer_km           = 3,
    distance_power      = 1,
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
    chunk_by_month      = TRUE,
    edu_col             = "escolaridad_avg"
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
  
  # Resolve the grouping variable: prefer group_var, fall back to edu_col so
  # that older education-only calls keep working unchanged.
  if (is.null(group_var)) {
    group_var <- edu_col
  }
  
  n_groups <- as.integer(n_groups)
  
  if (is.na(n_groups) || n_groups < 2L) {
    stop("`n_groups` must be an integer >= 2.")
  }
  
  # Validate main inputs.
  if (!dir.exists(arrow_dir)) {
    stop("`arrow_dir` not found: ", arrow_dir)
  }
  
  if (!file.exists(geo_sta_pq)) {
    stop("`geo_sta_pq` not found: ", geo_sta_pq)
  }
  
  # Check census required columns.
  for (col in c(geo_id_col, pop_col, group_var)) {
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
  indiv_path <- file.path(out_dir, paste0(out_name, "_indiv_groups.parquet"))
  
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
  
  # Assign 1..n_groups by cumulative expansion-weight share of group_var.
  # Group 1 is the lowest value; the last group catches the residual share.
  # This reproduces the previous hardcoded quintile cut when n_groups = 5.
  .assign_socio_group <- function(dt, var, wcol, n, out_col) {
    
    # Ascending sort so that group 1 corresponds to the lowest values.
    data.table::setorderv(dt, var)
    
    # Cumulative and total weight over rows with valid value and weight.
    dt[
      !is.na(get(var)) & !is.na(get(wcol)),
      `:=`(.cum_w = cumsum(get(wcol)), .tot_w = sum(get(wcol)))
    ]
    
    # Interior cut points k/n for k = 1..(n-1); left.open matches "<= edge".
    edges <- seq_len(n - 1L) / n
    
    dt[
      !is.na(.cum_w),
      (out_col) := pmin(
        findInterval(.cum_w / .tot_w, edges, left.open = TRUE) + 1L,
        n
      )
    ]
    
    dt[, c(".cum_w", ".tot_w") := NULL]
    invisible(dt)
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
  
  # 8. Census processing and group assignment
  # -----------------------------------------------------------------------
  census_dt <- data.table::copy(data.table::as.data.table(census_col))
  data.table::setnames(census_dt, geo_id_col, "geo_id")
  census_dt[, geo_id := .safe_chr(geo_id)]
  
  if (quintile_level == "geo") {
    
    # Assign groups from the geo-level group_var using population shares.
    .assign_socio_group(census_dt, group_var, pop_col, n_groups, group_name)
    
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
    
    # Assign groups from the individual group_var using expansion weights.
    .assign_socio_group(census_dt, group_var, pop_col, n_groups, group_name)
    
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
# @Arg city_id        : string; city identifier used in output folders and files.
# @Arg arrow_dir      : string; path to cleaned partitioned Arrow/Parquet data.
# @Arg distance_power : numeric; IDW distance exponent.
# @Arg geo_sta_pq     : string; path to geo-station distance Parquet file.
# @Arg geo_census     : data.frame; collapsed geographic-unit census data.
# @Arg micro_census   : data.frame; individual-level census microdata.
# @Arg geo_id_col     : string; geographic ID column in collapsed census data.
# @Arg geo_pop_col    : string; population column in collapsed census data.
# @Arg geo_group_var  : string; group variable in collapsed census data
#                       (e.g. "education_mean" or "income").
# @Arg micro_id_col   : string; geographic ID column in individual census data.
# @Arg micro_pop_col  : string; weight column in individual census data.
# @Arg micro_group_var: string; group variable in individual census data
#                       (e.g. "escolaridad" or "income").
# @Arg n_groups       : integer; number of equal-population groups (5 or 10).
# @Arg group_name     : string; output group column name
#                       (e.g. "edu_quintile" or "income_decile").
# @Arg buffer_km      : numeric; maximum geo-to-station distance.
# @Arg outdir_exp     : string; root output directory for IDW estimates.
# @Arg out_suffix     : string or NULL; extra tag in file names to separate
#                       groupings (e.g. "income"). NULL keeps the plain name.
# @Arg mem_gb         : numeric; DuckDB memory ceiling in GB. Default 40.
# @Arg n_threads      : integer; DuckDB worker threads. Default 2.
# @Arg overwrite      : logical; overwrite existing outputs. Default TRUE.
# @Arg return_data    : logical; return data objects in memory. Default FALSE.
#
# @Output : Named list with geo and individual output paths.
#
# @Details:
#   Computes the expensive IDW exposure table once using individual mode for the
#   requested grouping, then builds the geo-level exposure output by merging the
#   same exposure table with collapsed census and assigning geo-level groups.
#   One grouping is produced per call; call once for edu_quintile and once for
#   income_decile to obtain separate files.
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
    geo_group_var,
    micro_id_col,
    micro_pop_col,
    micro_group_var,
    n_groups       = 5L,
    group_name     = "edu_quintile",
    buffer_km,
    outdir_exp,
    out_suffix     = NULL,
    mem_gb      = 40,
    n_threads   = 8L,
    overwrite   = TRUE,
    return_data = FALSE
) {
  
  message("\n--- Processing ", city_label, " | ", buffer_km, " km | ",
          group_name, " ---")
  
  n_groups <- as.integer(n_groups)
  
  # Define output folder and common output prefix.
  city_out_dir <- here::here(outdir_exp, city_id)
  dir.create(city_out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Add an optional suffix so income and education outputs do not overwrite.
  out_base <- sprintf("%s_%dkm", city_id, buffer_km)
  
  if (!is.null(out_suffix) && nzchar(out_suffix)) {
    out_base <- paste0(out_base, "_", out_suffix)
  }
  
  # Compute IDW exposure once and save individual groups.
  exp_indiv <- aggregate_idw_exposure(
    arrow_dir      = arrow_dir,
    geo_sta_pq     = geo_sta_pq,
    census_col     = micro_census,
    geo_id_col     = micro_id_col,
    pop_col        = micro_pop_col,
    group_var      = micro_group_var,
    n_groups       = n_groups,
    group_name     = group_name,
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
  message(city_label, " individual groups: ", exp_indiv$individual_path)
  
  # Read the saved geo-level exposure table.
  exposure_dt <- data.table::as.data.table(
    arrow::read_parquet(exp_indiv$exposure_path)
  )
  
  # Prepare collapsed census data for geo-level groups.
  geo_dt <- data.table::copy(data.table::as.data.table(geo_census))
  
  data.table::setnames(geo_dt, geo_id_col, "geo_id")
  geo_dt[, geo_id := as.character(geo_id)]
  
  # Sort geographic units by group_var and assign population-weighted groups.
  data.table::setorderv(geo_dt, geo_group_var)
  
  geo_dt[
    !is.na(get(geo_group_var)) & !is.na(get(geo_pop_col)),
    `:=`(
      cum_pop = cumsum(get(geo_pop_col)),
      tot_pop = sum(get(geo_pop_col))
    )
  ]
  
  # Interior cut points k/n_groups; left.open matches the "<= edge" rule.
  edges <- seq_len(n_groups - 1L) / n_groups
  
  geo_dt[
    !is.na(cum_pop),
    (group_name) := pmin(
      findInterval(cum_pop / tot_pop, edges, left.open = TRUE) + 1L,
      n_groups
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
# Function: repair_bogota_geo_ids
#
# @Arg geo_ids          : character vector; spatial geographic IDs to repair.
# @Arg census_ids       : character vector; valid census geographic IDs.
# @Arg id_width         : integer; target Bogotá MGN ID width. Default 22.
# @Arg max_zero_suffix  : integer; maximum trailing digits to replace by zero.
# @Arg allow_broad_ids  : logical; allow broader zero-suffix repairs?
#
# @Output : data.table with original ID, repaired ID, method, and diagnostics.
#
# @Details:
#   Repairs Bogotá geographic IDs for spatial-to-census joins. The function
#   first preserves exact matches, then tries right-padding short IDs, and
#   then tries hierarchical trailing-zero repairs. By default, only local
#   repairs are allowed. Broad repairs should be used only for diagnostics.
#
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
repair_bogota_geo_ids <- function(
    geo_ids,
    census_ids,
    id_width        = 22L,
    max_zero_suffix = 2L,
    allow_broad_ids = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' required.")
  }
  
  # 1. Prepare inputs
  # -----------------------------------------------------------------------
  geo_ids <- trimws(as.character(geo_ids))
  census_ids <- trimws(as.character(census_ids))
  
  census_ids <- unique(census_ids[!is.na(census_ids) & census_ids != ""])
  
  if (length(geo_ids) == 0L) {
    return(data.table::data.table(
      geo_id_original = character(),
      geo_id_repaired = character(),
      repair_method = character(),
      zero_suffix_n = integer(),
      matched_repaired = logical(),
      changed_id = logical(),
      broad_repair = logical()
    ))
  }
  
  census_env <- new.env(hash = TRUE, parent = emptyenv())
  
  for (id in census_ids) {
    assign(id, TRUE, envir = census_env)
  }
  
  # 2. Inner helpers
  # -----------------------------------------------------------------------
  .in_census <- function(x) {
    !is.na(x) && exists(x, envir = census_env, inherits = FALSE)
  }
  
  .right_pad <- function(x) {
    if (is.na(x)) {
      return(NA_character_)
    }
    
    if (nchar(x) >= id_width) {
      return(substr(x, 1L, id_width))
    }
    
    paste0(x, strrep("0", id_width - nchar(x)))
  }
  
  .repair_one <- function(id) {
    
    if (is.na(id) || id == "") {
      return(list(id = NA_character_, method = "missing", suffix = NA_integer_))
    }
    
    if (.in_census(id)) {
      return(list(id = id, method = "exact", suffix = 0L))
    }
    
    id_pad <- .right_pad(id)
    
    if (!identical(id, id_pad) && .in_census(id_pad)) {
      return(list(id = id_pad, method = "right_pad", suffix = 0L))
    }
    
    max_suffix <- if (isTRUE(allow_broad_ids)) {
      id_width - 1L
    } else {
      as.integer(max_zero_suffix)
    }
    
    if (is.na(max_suffix) || max_suffix < 1L) {
      return(list(id = NA_character_, method = "unmatched", suffix = NA_integer_))
    }
    
    for (k in seq_len(max_suffix)) {
      
      prefix_len <- id_width - k
      
      candidate <- paste0(
        substr(id_pad, 1L, prefix_len),
        strrep("0", k)
      )
      
      if (.in_census(candidate)) {
        return(list(
          id = candidate,
          method = paste0("zero_suffix_", k),
          suffix = k
        ))
      }
    }
    
    list(id = NA_character_, method = "unmatched", suffix = NA_integer_)
  }
  
  # 3. Apply repair
  # -----------------------------------------------------------------------
  unique_geo <- unique(geo_ids)
  repairs <- lapply(unique_geo, .repair_one)
  
  out <- data.table::data.table(
    geo_id_original = unique_geo,
    geo_id_repaired = vapply(repairs, `[[`, character(1), "id"),
    repair_method = vapply(repairs, `[[`, character(1), "method"),
    zero_suffix_n = vapply(repairs, `[[`, integer(1), "suffix")
  )
  
  out[, matched_repaired := !is.na(geo_id_repaired)]
  out[, changed_id := geo_id_original != geo_id_repaired]
  out[, broad_repair := !is.na(zero_suffix_n) & zero_suffix_n > max_zero_suffix]
  
  return(out[])
}


# --------------------------------------------------------------------------------------------
# Function: canonical_geo_id
#
# @Arg x        : vector; raw geographic IDs (character, integer, numeric, int64).
# @Arg width    : integer or NULL; if given, left-pad with zeros to this width.
# @Arg state    : string or NULL; if given, prefix this state code (then pad).
#
# @Output : character vector of canonical geo IDs.
#
# @Purpose:
#   Single source of truth for geo-id formatting so that the distance matrix,
#   the IDW census merge, and the station-socio census merge all agree. Numeric
#   IDs are printed without scientific notation; an optional fixed width fixes
#   leading-zero loss (e.g. Mexico "9007" -> "09007").
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
canonical_geo_id <- function(x, width = NULL, state = NULL) {
  
  # Print numerics/int64 without scientific notation or decimals.
  if (inherits(x, "integer64")) {
    x <- as.character(x)
  } else if (is.numeric(x)) {
    x <- ifelse(is.na(x), NA_character_, sprintf("%.0f", x))
  } else {
    x <- trimws(as.character(x))
  }
  
  # Optionally prefix a fixed state code before padding.
  if (!is.null(state)) {
    x <- ifelse(is.na(x), NA_character_, paste0(state, x))
  }
  
  # Optionally left-pad with zeros to a fixed width (base-R only).
  if (!is.null(width)) {
    need <- pmax(0L, width - nchar(x))
    x <- ifelse(is.na(x), NA_character_, paste0(strrep("0", need), x))
  }
  
  return(x)
}


# --------------------------------------------------------------------------------------------
# Function: compute_station_pollution_summary
#
# @Arg arrow_dir        : string; path to partitioned Arrow/Parquet hourly data.
# @Arg year_filter      : integer; year to process. Default 2023.
# @Arg station_col      : string; station column in the pollution data.
# @Arg pollutants       : character vector; pollutant columns to summarize.
# @Arg who_it           : named list; WHO interim target thresholds.
# @Arg min_obs_active   : integer; minimum observations to define active station.
# @Arg quiet            : logical; suppress messages. Default FALSE.
#
# @Output : data.table with one row per active station.
#
# @Details:
#   Computes station-level annual means and hours above WHO thresholds. A station
#   is considered active if it has at least min_obs_active non-missing observation
#   for at least one pollutant in the requested year. Hours above a threshold count
#   hourly observations at or above it; the 24-hour IT values are used as a proxy
#   for an hourly extreme-pollution threshold, consistent with the IDW step.
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_station_pollution_summary <- function(
    arrow_dir,
    year_filter    = 2023L,
    station_col    = "station",
    pollutants     = c("pm10", "pm25"),
    who_it         = list(
      pm10 = c(it1 = 150, it2 = 100),
      pm25 = c(it1 = 75,  it2 = 50)
    ),
    min_obs_active = 1L,
    quiet          = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  pkgs <- c("arrow", "dplyr", "data.table", "stringi")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package missing: ", p)
    }
  }
  
  # 1. Validate inputs
  # -----------------------------------------------------------------------
  if (!dir.exists(arrow_dir)) {
    stop("`arrow_dir` not found: ", arrow_dir)
  }
  
  # Normalize station identifiers consistently across the pipeline.
  .normalize_station <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  # 2. Open Arrow dataset and collect the requested year
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[station_summary] Reading hourly data for year ", year_filter, ".")
  }
  
  ds <- arrow::open_dataset(arrow_dir)
  
  fields <- names(ds)
  keep_pollutants <- intersect(pollutants, fields)
  
  if (!station_col %in% fields) {
    stop("Column '", station_col, "' not found in pollution dataset.")
  }
  
  if (!"year" %in% fields) {
    stop("Column 'year' not found in pollution dataset.")
  }
  
  if (length(keep_pollutants) == 0L) {
    stop("None of the requested pollutants were found in the dataset.")
  }
  
  keep_cols <- unique(c(station_col, "year", keep_pollutants))
  
  dt <- ds |>
    dplyr::filter(year == year_filter) |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::collect() |>
    data.table::as.data.table()
  
  if (nrow(dt) == 0L) {
    stop("No pollution data found for year ", year_filter, ".")
  }
  
  data.table::setnames(dt, station_col, "station_raw")
  dt[, station_id := .normalize_station(station_raw)]
  
  # 3. Compute station-level outcomes for each pollutant
  # -----------------------------------------------------------------------
  out_list <- vector("list", length(keep_pollutants))
  names(out_list) <- keep_pollutants
  
  for (pol in keep_pollutants) {
    
    # Build threshold summaries for the pollutant.
    thr <- who_it[[pol]]
    
    stat_dt <- dt[
      ,
      .(
        station_name_raw = station_raw[which(!is.na(station_raw))[1]],
        n_obs = sum(!is.na(get(pol))),
        avg = mean(get(pol), na.rm = TRUE)
      ),
      by = station_id
    ]
    
    data.table::setnames(
      stat_dt,
      c("n_obs", "avg"),
      c(paste0("n_obs_", pol), paste0("avg_", pol))
    )
    
    # Add hours above each WHO threshold.
    if (!is.null(thr) && length(thr) > 0L) {
      for (nm in names(thr)) {
        col_nm <- paste0("hrs_d_", pol, "_", nm)
        
        tmp <- dt[
          ,
          .(value = sum(get(pol) >= thr[[nm]], na.rm = TRUE)),
          by = station_id
        ]
        
        data.table::setnames(tmp, "value", col_nm)
        stat_dt <- merge(stat_dt, tmp, by = "station_id", all.x = TRUE)
      }
    }
    
    out_list[[pol]] <- stat_dt
  }
  
  # 4. Merge pollutant-level summaries and define active stations
  # -----------------------------------------------------------------------
  out <- Reduce(
    function(a, b) merge(a, b, by = "station_id", all = TRUE),
    out_list
  )
  
  # Coalesce raw station names after pollutant-level merges.
  raw_cols <- grep("^station_name_raw", names(out), value = TRUE)
  
  if (length(raw_cols) > 1L) {
    out[, station_name := do.call(data.table::fcoalesce, .SD), .SDcols = raw_cols]
    out[, (raw_cols) := NULL]
  } else if (length(raw_cols) == 1L) {
    data.table::setnames(out, raw_cols, "station_name")
  }
  
  obs_cols <- grep("^n_obs_", names(out), value = TRUE)
  
  out[
    ,
    active_2023 := as.integer(
      rowSums(.SD >= min_obs_active, na.rm = TRUE) > 0
    ),
    .SDcols = obs_cols
  ]
  
  out <- out[active_2023 == 1L]
  
  if (!quiet) {
    message("[station_summary] Active stations: ", nrow(out), ".")
  }
  
  return(out[])
}


# --------------------------------------------------------------------------------------------
# Function: compute_station_socio_context
#
# @Arg stations_sf        : sf POINT object; monitoring stations.
# @Arg geo_sf             : sf POLYGON object; geographic units.
# @Arg census_col         : data.frame; collapsed census data by geographic unit.
# @Arg station_id_col     : string; station ID/name column in stations_sf.
# @Arg geo_id_col         : string; geographic unit ID column.
# @Arg pop_col            : string; population or expansion-weight column.
# @Arg socio_vars         : character vector; socioeconomic variables to attach.
# @Arg context_method     : string; "containing_geo" or "buffer".
# @Arg buffer_km          : numeric; buffer radius when context_method = "buffer".
# @Arg representative_pt  : string; "point_on_surface" or "centroid".
# @Arg geo_id_repair      : string; "none", "bogota", or "suffix".
# @Arg bogota_max_suffix  : integer; maximum suffix repair for Bogota IDs.
# @Arg bogota_broad_ids   : logical; allow broad Bogota ID repairs?
# @Arg quiet              : logical; suppress messages. Default FALSE.
#
# @Output : data.table with one row per station and socioeconomic context.
#
# @Details:
#   In containing_geo mode, each station receives the characteristics of the
#   polygon containing it; if a station falls on a shared boundary and matches
#   more than one polygon, only the first match is kept and a message reports it.
#   In buffer mode, each station receives population-weighted averages across
#   geographic units whose representative point lies within buffer_km. Buffer mode
#   suits cities with very small units (e.g. Bogota), where a single containing
#   unit is a noisy descriptor of the station's local socioeconomic context.
#
#   geo_id_repair = "suffix" handles cases where the spatial layer stores only the
#   municipality component while the census stores a full state-municipality code.
#   A repair is accepted only when the suffix match is unique.
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_station_socio_context <- function(
    stations_sf,
    geo_sf,
    census_col,
    station_id_col,
    geo_id_col,
    pop_col,
    socio_vars,
    context_method    = c("containing_geo", "buffer"),
    buffer_km         = 3,
    representative_pt = c("point_on_surface", "centroid"),
    geo_id_repair     = c("none", "bogota", "suffix"),
    bogota_max_suffix = 2L,
    bogota_broad_ids  = FALSE,
    quiet             = FALSE
) {
  
  # 0. Dependencies and argument matching
  # -----------------------------------------------------------------------
  pkgs <- c("sf", "data.table", "stringi")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package missing: ", p)
    }
  }
  
  context_method <- match.arg(context_method)
  representative_pt <- match.arg(representative_pt)
  geo_id_repair <- match.arg(geo_id_repair)
  
  # 1. Input checks
  # -----------------------------------------------------------------------
  if (!inherits(stations_sf, "sf")) {
    stop("`stations_sf` must be an sf object.")
  }
  
  if (!inherits(geo_sf, "sf")) {
    stop("`geo_sf` must be an sf object.")
  }
  
  if (!station_id_col %in% names(stations_sf)) {
    stop("Column '", station_id_col, "' not found in stations_sf.")
  }
  
  if (!geo_id_col %in% names(geo_sf)) {
    stop("Column '", geo_id_col, "' not found in geo_sf.")
  }
  
  req_census_cols <- c(geo_id_col, pop_col, socio_vars)
  miss_census <- setdiff(req_census_cols, names(census_col))
  
  if (length(miss_census) > 0L) {
    stop("census_col is missing: ", paste(miss_census, collapse = ", "))
  }
  
  # 2. Helpers
  # -----------------------------------------------------------------------
  .normalize_station <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  .safe_chr <- function(x) {
    if (inherits(x, "integer64")) {
      return(trimws(as.character(x)))
    }
    
    if (is.numeric(x)) {
      return(ifelse(is.na(x), NA_character_, sprintf("%.0f", x)))
    }
    
    trimws(as.character(x))
  }
  
  # Left-pad a character vector with zeros to a fixed width (base-R only).
  .pad_left0 <- function(x, width) {
    x <- as.character(x)
    need <- pmax(0L, width - nchar(x))
    paste0(strrep("0", need), x)
  }
  
  .aeqd_proj <- function(lon0, lat0) {
    sprintf(
      "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
      lat0, lon0
    )
  }
  
  .weighted_mean <- function(x, w) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    
    if (!any(ok)) {
      return(NA_real_)
    }
    
    sum(x[ok] * w[ok]) / sum(w[ok])
  }
  
  .repair_suffix_ids <- function(geo_ids, census_ids) {
    
    geo_chr <- .safe_chr(geo_ids)
    census_chr <- .safe_chr(census_ids)
    
    out <- data.table::data.table(
      geo_id_original = geo_chr,
      geo_id_repaired = geo_chr,
      repair_method = "exact",
      matched_repaired = geo_chr %in% census_chr
    )
    
    unmatched <- which(!out$matched_repaired & !is.na(out$geo_id_original))
    
    if (length(unmatched) == 0L) {
      return(out)
    }
    
    census_unique <- unique(census_chr[!is.na(census_chr)])
    
    # Modal census width used for left-pad attempts.
    width_modal <- as.integer(names(sort(
      table(nchar(census_unique)),
      decreasing = TRUE
    ))[1])
    
    for (i in unmatched) {
      id_i <- out$geo_id_original[i]
      
      # Try left-padding to the modal census ID width (base-R zero pad).
      id_pad <- .pad_left0(id_i, width_modal)
      
      if (id_pad %in% census_unique) {
        out$geo_id_repaired[i] <- id_pad
        out$repair_method[i] <- "left_pad"
        out$matched_repaired[i] <- TRUE
        next
      }
      
      # Try unique suffix matching. This fixes cases like 002 -> 9002.
      suffix_matches <- census_unique[endsWith(census_unique, id_i)]
      
      if (length(suffix_matches) == 1L) {
        out$geo_id_repaired[i] <- suffix_matches
        out$repair_method[i] <- "unique_suffix"
        out$matched_repaired[i] <- TRUE
        next
      }
      
      out$repair_method[i] <- "unmatched"
    }
    
    out[]
  }
  
  # 3. Prepare spatial and census data
  # -----------------------------------------------------------------------
  stations_wgs <- sf::st_transform(stations_sf, crs = 4326)
  geo_wgs <- sf::st_transform(sf::st_make_valid(geo_sf), crs = 4326)
  
  stations_wgs$station_id <- .normalize_station(stations_wgs[[station_id_col]])
  geo_wgs[[geo_id_col]] <- .safe_chr(geo_wgs[[geo_id_col]])
  
  census_dt <- data.table::copy(data.table::as.data.table(census_col))
  census_dt[, (geo_id_col) := .safe_chr(get(geo_id_col))]
  
  # Keep only variables needed for this step.
  census_keep <- unique(c(geo_id_col, pop_col, socio_vars))
  census_dt <- census_dt[, ..census_keep]
  
  # 4. Repair geographic IDs before merging census attributes
  # -----------------------------------------------------------------------
  if (geo_id_repair == "bogota") {
    
    if (!exists("repair_bogota_geo_ids", mode = "function")) {
      stop(
        "geo_id_repair = 'bogota' requires repair_bogota_geo_ids() ",
        "to be defined in config_utils_process_data.R."
      )
    }
    
    id_xwalk <- repair_bogota_geo_ids(
      geo_ids = geo_wgs[[geo_id_col]],
      census_ids = census_dt[[geo_id_col]],
      max_zero_suffix = bogota_max_suffix,
      allow_broad_ids = bogota_broad_ids
    )
    
    repair_cols <- c(
      "geo_id_original",
      "geo_id_repaired",
      "repair_method",
      "matched_repaired"
    )
    
    id_xwalk <- id_xwalk[, ..repair_cols]
    
    geo_wgs$geo_id_original <- geo_wgs[[geo_id_col]]
    geo_dt <- data.table::as.data.table(sf::st_drop_geometry(geo_wgs))
    
    geo_dt <- merge(
      geo_dt,
      id_xwalk,
      by.x = "geo_id_original",
      by.y = "geo_id_original",
      all.x = TRUE
    )
    
    geo_wgs[[geo_id_col]] <- geo_dt$geo_id_repaired
    
    if (!quiet) {
      msg <- id_xwalk[
        ,
        .N,
        by = repair_method
      ][order(repair_method)]
      
      message("[station_context] Bogota ID repair summary:")
      print(msg)
    }
  }
  
  if (geo_id_repair == "suffix") {
    
    id_xwalk <- .repair_suffix_ids(
      geo_ids = geo_wgs[[geo_id_col]],
      census_ids = census_dt[[geo_id_col]]
    )
    
    geo_wgs$geo_id_original <- geo_wgs[[geo_id_col]]
    geo_dt <- data.table::as.data.table(sf::st_drop_geometry(geo_wgs))
    
    geo_dt <- merge(
      geo_dt,
      id_xwalk,
      by.x = "geo_id_original",
      by.y = "geo_id_original",
      all.x = TRUE
    )
    
    geo_wgs[[geo_id_col]] <- geo_dt$geo_id_repaired
    
    if (!quiet) {
      msg <- id_xwalk[
        ,
        .N,
        by = repair_method
      ][order(repair_method)]
      
      message("[station_context] Suffix ID repair summary:")
      print(msg)
    }
  }
  
  # 5. Merge census attributes into geographic units
  # -----------------------------------------------------------------------
  geo_wgs <- merge(
    geo_wgs,
    census_dt,
    by = geo_id_col,
    all.x = TRUE
  )
  
  # 6. Method 1: socioeconomic context from containing polygon
  # -----------------------------------------------------------------------
  if (context_method == "containing_geo") {
    
    if (!quiet) {
      message("[station_context] Using containing geographic unit.")
    }
    
    joined <- suppressWarnings(
      sf::st_join(
        stations_wgs[, c("station_id", station_id_col)],
        geo_wgs[, c(geo_id_col, pop_col, socio_vars)],
        join = sf::st_intersects,
        left = TRUE
      )
    )
    
    out <- data.table::as.data.table(sf::st_drop_geometry(joined))
    
    # Guarantee one row per station. A station on a shared boundary can match
    # more than one polygon; keep the first match and report how many.
    n_before <- nrow(out)
    data.table::setkey(out, station_id)
    out <- out[, .SD[1L], by = station_id]
    n_dups <- n_before - nrow(out)
    
    if (n_dups > 0L && !quiet) {
      message(
        "[station_context] ", n_dups, " station(s) matched multiple ",
        "polygons on a boundary; kept the first match per station."
      )
    }
    
    data.table::setnames(out, geo_id_col, "station_geo_id")
    data.table::setnames(out, pop_col, "context_population")
    
    out[, context_method := "containing_geo"]
    out[, context_buffer_km := NA_real_]
    out[, n_geo_context := as.integer(!is.na(station_geo_id))]
    
    return(out[])
  }
  
  # 7. Method 2: population-weighted buffer context
  # -----------------------------------------------------------------------
  if (!quiet) {
    message("[station_context] Using buffer context: ", buffer_km, " km.")
  }
  
  # Build representative points for geographic units.
  if (representative_pt == "point_on_surface") {
    geo_pts <- suppressWarnings(sf::st_point_on_surface(geo_wgs))
  } else {
    geo_pts <- suppressWarnings(sf::st_centroid(geo_wgs))
  }
  
  # Use a local metric projection centered on the stations.
  cen <- sf::st_coordinates(sf::st_centroid(sf::st_union(stations_wgs)))
  
  proj_m <- .aeqd_proj(
    lon0 = cen[1, "X"],
    lat0 = cen[1, "Y"]
  )
  
  stations_m <- sf::st_transform(stations_wgs, crs = proj_m)
  geo_pts_m <- sf::st_transform(geo_pts, crs = proj_m)
  
  # Spatial relation: geo representative points within station buffer.
  idx <- sf::st_is_within_distance(
    stations_m,
    geo_pts_m,
    dist = buffer_km * 1000
  )
  
  # Build one output row per station.
  out_list <- vector("list", length(idx))
  
  for (i in seq_along(idx)) {
    
    station_i <- stations_wgs[i, ]
    geo_idx_i <- idx[[i]]
    
    base <- data.table::data.table(
      station_id = station_i$station_id,
      station_name = as.character(station_i[[station_id_col]]),
      context_method = "buffer",
      context_buffer_km = buffer_km,
      n_geo_context = length(geo_idx_i)
    )
    
    if (length(geo_idx_i) == 0L) {
      base[, context_population := NA_real_]
      
      for (v in socio_vars) {
        base[, (v) := NA_real_]
      }
      
      out_list[[i]] <- base
      next
    }
    
    geo_i <- data.table::as.data.table(
      sf::st_drop_geometry(geo_pts[geo_idx_i, ])
    )
    
    w <- geo_i[[pop_col]]
    base[, context_population := sum(w, na.rm = TRUE)]
    
    for (v in socio_vars) {
      base[, (v) := .weighted_mean(geo_i[[v]], w)]
    }
    
    out_list[[i]] <- base
  }
  
  out <- data.table::rbindlist(out_list, fill = TRUE)
  
  return(out[])
}


# ------------------------------------------------------------------------------------
# Function: build_station_scatter_inputs
#
# @Arg arrow_dir        : string; path to partitioned Arrow/Parquet hourly data.
# @Arg stations_sf      : sf POINT object; monitoring stations.
# @Arg geo_sf           : sf POLYGON object; geographic units.
# @Arg census_col       : data.frame; collapsed census data by geographic unit.
# @Arg station_id_col   : string; station ID/name column in stations_sf.
# @Arg geo_id_col       : string; geographic unit ID column.
# @Arg pop_col          : string; population or expansion-weight column.
# @Arg socio_vars       : character vector; socioeconomic variables to attach.
# @Arg year_filter      : integer; year to process. Default 2023.
# @Arg context_method   : string; "containing_geo" or "buffer".
# @Arg context_buffer_km: numeric; buffer radius when context_method = "buffer".
# @Arg geo_id_repair    : string; "none", "bogota", or "suffix".
# @Arg bogota_max_suffix: integer; maximum suffix repair for Bogota IDs.
# @Arg bogota_broad_ids : logical; allow broad Bogota ID repairs?
# @Arg pollutants       : character vector; pollutant columns to summarize.
# @Arg who_it           : named list; WHO interim target thresholds.
# @Arg out_dir          : string; output directory.
# @Arg out_name         : string; output file prefix.
# @Arg overwrite        : logical; overwrite existing output. Default TRUE.
# @Arg quiet            : logical; suppress messages. Default FALSE.
# @Arg return_data      : logical; return data.table in memory. Default TRUE.
#
# @Output : Named list with output path and, optionally, station-level data.
#
# @Details:
#   Produces the station-level scatterplot inputs used in the exposure section:
#   one row per active station with annual mean concentration, hours above WHO
#   targets, and the socioeconomic context of the geographic unit where the
#   station is located (or a buffer around it). It does not produce maps,
#   distance-by-radius tables, or the share-of-non-missing-by-quintile table.
#
# @Written_on : June 2026
# @Written_by : Marcos Paulo
# ------------------------------------------------------------------------------------
build_station_scatter_inputs <- function(
    arrow_dir,
    stations_sf,
    geo_sf,
    census_col,
    station_id_col,
    geo_id_col,
    pop_col,
    socio_vars,
    year_filter       = 2023L,
    context_method    = c("containing_geo", "buffer"),
    context_buffer_km = 3,
    geo_id_repair     = c("none", "bogota", "suffix"),
    bogota_max_suffix = 2L,
    bogota_broad_ids  = FALSE,
    pollutants        = c("pm10", "pm25"),
    who_it            = list(
      pm10 = c(it1 = 150, it2 = 100),
      pm25 = c(it1 = 75,  it2 = 50)
    ),
    out_dir,
    out_name,
    overwrite         = TRUE,
    quiet             = FALSE,
    return_data       = TRUE
) {
  
  # 0. Dependencies and argument matching
  # -----------------------------------------------------------------------
  pkgs <- c("arrow", "data.table")
  
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package missing: ", p)
    }
  }
  
  context_method <- match.arg(context_method)
  geo_id_repair <- match.arg(geo_id_repair)
  
  # 1. Output path and early exit
  # -----------------------------------------------------------------------
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  out_path <- file.path(out_dir, paste0(out_name, "_station_socio.parquet"))
  
  if (!overwrite && file.exists(out_path)) {
    if (!quiet) {
      message("[station_socio] Output exists; skipping.")
    }
    
    out <- list(station_socio_path = out_path)
    
    if (isTRUE(return_data)) {
      out$station_socio <- data.table::as.data.table(
        arrow::read_parquet(out_path)
      )
    }
    
    return(invisible(out))
  }
  
  # 2. Compute pollution summaries
  # -----------------------------------------------------------------------
  pol_dt <- compute_station_pollution_summary(
    arrow_dir    = arrow_dir,
    year_filter  = year_filter,
    station_col  = "station",
    pollutants   = pollutants,
    who_it       = who_it,
    quiet        = quiet
  )
  
  # 3. Compute socioeconomic context
  # -----------------------------------------------------------------------
  socio_dt <- compute_station_socio_context(
    stations_sf       = stations_sf,
    geo_sf            = geo_sf,
    census_col        = census_col,
    station_id_col    = station_id_col,
    geo_id_col        = geo_id_col,
    pop_col           = pop_col,
    socio_vars        = socio_vars,
    context_method    = context_method,
    buffer_km         = context_buffer_km,
    geo_id_repair     = geo_id_repair,
    bogota_max_suffix = bogota_max_suffix,
    bogota_broad_ids  = bogota_broad_ids,
    quiet             = quiet
  )
  
  # 4. Merge station-level pollution and socioeconomic context
  # -----------------------------------------------------------------------
  out_dt <- merge(
    pol_dt,
    socio_dt,
    by = "station_id",
    all.x = TRUE
  )
  
  out_dt[, year := year_filter]
  
  # A station is socioeconomically matched only if a spatial context exists
  # and at least one requested socioeconomic variable is non-missing.
  socio_present <- intersect(socio_vars, names(out_dt))
  
  if (length(socio_present) == 0L) {
    stop("None of `socio_vars` are present after merging station context.")
  }
  
  out_dt[
    ,
    matched_socio_context := as.integer(
      !is.na(n_geo_context) &
        n_geo_context > 0L &
        rowSums(!is.na(.SD)) > 0L
    ),
    .SDcols = socio_present
  ]
  
  if (!quiet) {
    n_good <- out_dt[matched_socio_context == 1L, .N]
    
    message(
      "[station_socio] Valid socioeconomic matches: ",
      n_good, " of ", nrow(out_dt), "."
    )
  }
  
  # 5. Save output
  # -----------------------------------------------------------------------
  arrow::write_parquet(out_dt, out_path)
  
  out <- list(station_socio_path = out_path)
  
  if (isTRUE(return_data)) {
    out$station_socio <- out_dt
  }
  
  return(invisible(out))
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
# Function: compute_exposure_summaries
#
# @Arg exposure_dt   : data.table; geo-level IDW exposure (one row per geo unit-year).
# @Arg individual_dt : data.table; geo-by-group population/expansion weights.
# @Arg geo_id_col    : string; geographic identifier column. Default "geo_id".
# @Arg pop_col       : string; population or expansion-weight column. Default "n".
# @Arg group_col     : string; socioeconomic group column. Default "edu_quintile".
# @Arg group_values  : integer vector; valid groups, e.g. 1:5.
# @Arg pollutants    : character vector; pollutants to keep, e.g. pm10/pm25.
# @Arg outcome_pattern : string; regex selecting exposure outcome columns.
# @Arg year_filter   : integer or NULL; if set, keeps only this exposure year.
# @Arg quiet         : logical; suppress progress messages. Default FALSE.
#
# @Output : data.table with weighted mean, weighted median, population, and counts
#           by outcome, pollutant, and group.
#
# @Details:
#   Raw exposure levels by socioeconomic group. Merges geo-level exposure with the
#   geo-by-group population, collapses to geo-unit-by-group cells (exposure is
#   constant within a geo unit), then aggregates to group level. Cells are weighted
#   by population so groups reflect the population they represent.
#
# @Written_by : Marcos Paulo
# @Updated_on : June 2026
# --------------------------------------------------------------------------------------------
compute_exposure_summaries <- function(exposure_dt,
                                       individual_dt,
                                       geo_id_col      = "geo_id",
                                       pop_col         = "n",
                                       group_col       = "edu_quintile",
                                       group_values    = 1:5,
                                       pollutants      = c("pm10", "pm25"),
                                       outcome_pattern = "^(avg|hrs_d)_",
                                       year_filter     = NULL,
                                       quiet           = FALSE) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }
  
  # 1. Merge exposure with the geo-by-group population
  # -----------------------------------------------------------------------
  # Both inputs are in-memory data.tables; copy so we never edit the caller's data.
  dt <- .exposure_merge_geo_group(
    exposure_dt   = exposure_dt,
    individual_dt = individual_dt,
    geo_id_col    = geo_id_col,
    group_col     = group_col,
    pop_col       = pop_col,
    group_values  = group_values,
    year_filter   = year_filter
  )
  
  # 2. Pick outcome columns that match the pattern and the pollutants
  # -----------------------------------------------------------------------
  out_cols <- .exposure_outcome_cols(dt, outcome_pattern, pollutants)
  by_cols  <- c(geo_id_col, group_col)
  
  # 3. Collapse to geo-by-group cells (exposure is constant within a geo unit)
  # -----------------------------------------------------------------------
  geo_group <- dt[
    ,
    c(
      list(group_pop = sum(get(pop_col), na.rm = TRUE)),
      lapply(.SD, function(x) stats::weighted.mean(x, get(pop_col), na.rm = TRUE))
    ),
    by = by_cols,
    .SDcols = out_cols
  ]
  
  geo_group <- geo_group[!is.na(group_pop) & group_pop > 0]
  
  # 4. Aggregate each outcome to group-level summaries
  # -----------------------------------------------------------------------
  res <- data.table::rbindlist(
    lapply(out_cols, function(col) {
      meta <- .exposure_parse_outcome(col, pollutants)
      
      tmp <- geo_group[
        !is.na(get(col)),
        .(
          weighted_mean       = stats::weighted.mean(get(col), group_pop, na.rm = TRUE),
          weighted_median     = .exposure_weighted_median(get(col), group_pop),
          weighted_population = sum(group_pop, na.rm = TRUE),
          n_geo_group_cells   = .N,
          n_geo_units         = data.table::uniqueN(get(geo_id_col))
        ),
        by = group_col
      ]
      
      # Stamp outcome/pollutant labels and standardize the group column name.
      data.table::setnames(tmp, group_col, "group")
      tmp[, `:=`(outcome = meta$outcome, pollutant = meta$pollutant,
                 group_col = group_col)]
      tmp
    }),
    fill = TRUE
  )
  
  data.table::setorder(res, outcome, pollutant, group)
  
  if (!quiet) {
    message("[summary] ", length(out_cols), " outcome(s) summarized.")
  }
  
  return(res[])
}


# --------------------------------------------------------------------------------------------
# Function: compute_exposure_regressions
#
# @Arg exposure_dt   : data.table; geo-level IDW exposure (one row per geo unit-year).
# @Arg individual_dt : data.table or NULL; geo-by-group population/expansion weights.
# @Arg geo_id_col    : string; geographic identifier column. Default "geo_id".
# @Arg pop_col       : string; population or expansion-weight column. Default "n".
# @Arg group_col     : string; socioeconomic group column. Default "edu_quintile".
# @Arg group_values  : integer vector; valid groups, e.g. 1:5.
# @Arg base_group    : integer; omitted reference group. Default max(group_values).
# @Arg pollutants    : character vector; pollutants to keep.
# @Arg outcome_pattern : string; regex selecting exposure outcome columns.
# @Arg year_filter   : integer or NULL; if set, keeps only this exposure year.
# @Arg conf_level    : numeric; confidence level for intervals. Default 0.95.
# @Arg normalized    : logical; if TRUE, divide each outcome by the base-group mean.
# @Arg regression_unit : string; "geo_group" (main), "individual", or "geo".
# @Arg se_type       : string; "cluster_geo" (preferred) or "classic" (legacy CI).
# @Arg quiet         : logical; suppress progress messages. Default FALSE.
#
# @Output : data.table with one row per outcome, pollutant, and group, giving the
#           gap relative to base_group with confidence interval.
#
# @Details:
#   Estimates exposure gaps versus base_group. The main paper estimator is
#   regression_unit = "geo_group": collapse merged data to geo-unit-by-group cells,
#   then weight each cell by its population share within group. "individual" runs
#   one row per individual; "geo" runs one row per geo unit (no group merge).
#   classic SEs use the t-distribution and reproduce the legacy confint(); 
#   cluster_geo clusters by geographic unit and uses the normal critical value.
#
# @Written_by : Marcos Paulo
# @Updated_on : June 2026
# --------------------------------------------------------------------------------------------
compute_exposure_regressions <- function(exposure_dt,
                                         individual_dt   = NULL,
                                         geo_id_col      = "geo_id",
                                         pop_col         = "n",
                                         group_col       = "edu_quintile",
                                         group_values    = 1:5,
                                         base_group      = max(group_values),
                                         pollutants      = c("pm10", "pm25"),
                                         outcome_pattern = "^hrs_d_.*_it[12]$",
                                         year_filter     = NULL,
                                         conf_level      = 0.95,
                                         normalized      = TRUE,
                                         regression_unit = c("geo_group",
                                                             "individual", "geo"),
                                         se_type         = c("cluster_geo",
                                                             "classic"),
                                         quiet           = FALSE) {
  
  # 0. Dependencies and argument checks
  # -----------------------------------------------------------------------
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }
  
  regression_unit <- match.arg(regression_unit)
  se_type         <- match.arg(se_type)
  
  # Clustered SEs need the sandwich package; classic SEs do not.
  if (se_type == "cluster_geo" && !requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required for clustered SEs.")
  }
  
  if (!(conf_level > 0 && conf_level < 1)) {
    stop("`conf_level` must be between 0 and 1.")
  }
  
  if (!base_group %in% group_values) {
    stop("`base_group` must belong to `group_values`.")
  }
  
  # 1. Merge exposure with the geo-by-group population (skip for "geo" unit)
  # -----------------------------------------------------------------------
  dt <- .exposure_merge_geo_group(
    exposure_dt      = exposure_dt,
    individual_dt    = individual_dt,
    geo_id_col       = geo_id_col,
    group_col        = group_col,
    pop_col          = pop_col,
    group_values     = group_values,
    year_filter      = year_filter,
    merge_individual = regression_unit != "geo"
  )
  
  # 2. Pick outcome columns and fit one model per outcome
  # -----------------------------------------------------------------------
  out_cols <- .exposure_outcome_cols(dt, outcome_pattern, pollutants)
  
  res <- data.table::rbindlist(
    lapply(
      out_cols,
      .exposure_fit_one,
      dt              = dt,
      geo_id_col      = geo_id_col,
      group_col       = group_col,
      pop_col         = pop_col,
      group_values    = group_values,
      base_group      = base_group,
      pollutants      = pollutants,
      regression_unit = regression_unit,
      se_type         = se_type,
      conf_level      = conf_level,
      normalized      = normalized
    ),
    fill = TRUE
  )
  
  if (nrow(res) == 0L) {
    if (!quiet) {
      message("[ci] Warning: insufficient data to fit any exposure models.")
    }
    return(data.table::data.table())
  }
  
  data.table::setorder(res, outcome, pollutant, group)
  
  if (!quiet) {
    message("[ci] ", length(out_cols), " outcome(s) fit | unit = '",
            regression_unit, "' | se = '", se_type, "'.")
  }
  
  return(res[])
}


# --------------------------------------------------------------------------------------------
# Internal helpers for the two exposure functions above.
# Kept small and shared so both functions read the data the same way.
# --------------------------------------------------------------------------------------------

# Merge geo-level exposure with the geo-by-group population table.
# Returns a filtered data.table ready for collapsing or fitting.
.exposure_merge_geo_group <- function(exposure_dt, individual_dt, geo_id_col,
                                      group_col, pop_col, group_values,
                                      year_filter = NULL, merge_individual = TRUE) {
  
  # Copy so the caller's in-memory tables are never modified.
  dt <- data.table::copy(data.table::as.data.table(exposure_dt))
  
  # Optional single-year filter.
  if (!is.null(year_filter)) {
    dt <- dt[year == year_filter]
  }
  
  dt[, (geo_id_col) := as.character(get(geo_id_col))]
  
  # Attach group population unless this is the geo-only unit.
  if (isTRUE(merge_individual)) {
    ind <- data.table::copy(data.table::as.data.table(individual_dt))
    ind[, (geo_id_col) := as.character(get(geo_id_col))]
    
    # Keep valid groups with positive weight only.
    ind <- ind[
      get(group_col) %in% group_values &
        !is.na(get(pop_col)) & get(pop_col) > 0,
      .SD,
      .SDcols = c(geo_id_col, group_col, pop_col)
    ]
    
    # Drop any pre-existing group column in exposure before the merge.
    if (group_col %in% names(dt)) {
      dt[, (group_col) := NULL]
    }
    
    dt <- merge(dt, ind, by = geo_id_col, allow.cartesian = TRUE)
  }
  
  # Final filter to valid groups with positive weight.
  dt <- dt[
    get(group_col) %in% group_values &
      !is.na(get(pop_col)) & get(pop_col) > 0
  ]
  
  return(dt)
}

# Select exposure outcome columns matching the pattern and the pollutants.
.exposure_outcome_cols <- function(dt, outcome_pattern, pollutants) {
  candidate <- grep(outcome_pattern, names(dt), value = TRUE)
  out_cols  <- candidate[grepl(paste(pollutants, collapse = "|"), candidate)]
  
  if (length(out_cols) == 0L) {
    stop("No outcome columns match `outcome_pattern` and `pollutants`.")
  }
  
  return(out_cols)
}

# Split an outcome column name into its outcome label and pollutant.
.exposure_parse_outcome <- function(col, pollutants) {
  hit <- which(vapply(pollutants, function(p) grepl(p, col, fixed = TRUE),
                      logical(1)))[1L]
  pollutant <- pollutants[hit]
  
  # Remove the pollutant token and tidy leftover underscores.
  outcome <- sub(paste0("_", pollutant, "_?"), "_", col)
  outcome <- sub("_$", "", sub("^_", "", outcome))
  
  return(list(outcome = outcome, pollutant = pollutant))
}

# Population-weighted median (used for the raw summaries only).
.exposure_weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  x[which(cumsum(w) / sum(w) >= 0.5)[1L]]
}

# Build the coefficient table from a fitted lm with the requested SE type.
.exposure_coef_table <- function(fit, model_dt, se_type, conf_level) {
  
  # classic: model-based vcov with the t critical value (reproduces confint()).
  if (se_type == "classic") {
    vcov_mat <- stats::vcov(fit)
    crit <- stats::qt(1 - (1 - conf_level) / 2, stats::df.residual(fit))
  }
  
  # cluster_geo: cluster-robust vcov by geo unit with the normal critical value.
  if (se_type == "cluster_geo") {
    vcov_mat <- sandwich::vcovCL(fit, cluster = model_dt$.cluster_geo, type = "HC1")
    crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  }
  
  estimate  <- stats::coef(fit)
  std_error <- sqrt(diag(vcov_mat))
  
  data.table::data.table(
    term      = names(estimate),
    estimate  = unname(estimate),
    std_error = unname(std_error),
    ci_low    = unname(estimate - crit * std_error),
    ci_high   = unname(estimate + crit * std_error)
  )
}

# Fit one outcome and return tidy rows (base group plus each comparison group).
.exposure_fit_one <- function(outcome_col, dt, geo_id_col, group_col, pop_col,
                              group_values, base_group, pollutants,
                              regression_unit, se_type, conf_level, normalized) {
  
  d0 <- dt[!is.na(get(outcome_col))]
  
  if (nrow(d0) < length(group_values)) {
    return(NULL)
  }
  
  # Optional normalization: divide the outcome by the base-group weighted mean.
  if (isTRUE(normalized)) {
    base_mean <- d0[
      get(group_col) == base_group,
      stats::weighted.mean(get(outcome_col), get(pop_col), na.rm = TRUE)
    ]
    
    if (is.na(base_mean) || base_mean == 0) {
      return(NULL)
    }
    
    d0[, y_model := get(outcome_col) / base_mean]
  } else {
    d0[, y_model := get(outcome_col)]
  }
  
  # Build the modeling table for the requested regression unit.
  if (regression_unit == "geo_group") {
    
    # Collapse to geo-by-group cells, then weight by population share in group.
    model_dt <- d0[
      !is.na(y_model),
      .(geo_population = sum(get(pop_col), na.rm = TRUE),
        y = stats::weighted.mean(y_model, get(pop_col), na.rm = TRUE)),
      by = c(geo_id_col, group_col)
    ]
    
    model_dt <- model_dt[!is.na(y) & !is.na(geo_population) & geo_population > 0]
    model_dt[, total_population_g := sum(geo_population), by = group_col]
    model_dt[, w := geo_population / total_population_g]
    model_dt <- model_dt[!is.na(w) & w > 0]
    
  } else {
    
    # individual/geo: one row per observation, weighted by population.
    model_dt <- d0[
      !is.na(y_model),
      .(y = y_model, w = get(pop_col),
        .cluster_geo = get(geo_id_col), group_value = get(group_col))
    ]
    data.table::setnames(model_dt, "group_value", group_col)
  }
  
  if (nrow(model_dt) < length(group_values)) {
    return(NULL)
  }
  
  # Cluster key and the group factor with base_group as the reference level.
  model_dt[, .cluster_geo := get(geo_id_col)]
  model_dt[, g := factor(get(group_col),
                         levels = c(base_group,
                                    setdiff(group_values, base_group)))]
  
  fit     <- stats::lm(y ~ g, data = model_dt, weights = w)
  coef_dt <- .exposure_coef_table(fit, model_dt, se_type, conf_level)
  meta    <- .exposure_parse_outcome(outcome_col, pollutants)
  
  # One assembled row builder reused for the base and comparison groups.
  make_row <- function(grp, est, se, lo, hi) {
    data.table::data.table(
      outcome = meta$outcome, pollutant = meta$pollutant, group = grp,
      estimate = est, std_error = se, ci_low = lo, ci_high = hi,
      n_units = nrow(model_dt), base_group = base_group, group_col = group_col,
      regression_unit = regression_unit, se_type = se_type, normalized = normalized
    )
  }
  
  # Base group has a zero gap by construction.
  out <- make_row(base_group, 0, 0, 0, 0)
  
  # Append each comparison group's coefficient and interval.
  for (grp in setdiff(group_values, base_group)) {
    row <- coef_dt[term == paste0("g", grp)]
    
    if (nrow(row) == 0L) {
      next
    }
    
    out <- data.table::rbindlist(
      list(out, make_row(grp, row$estimate, row$std_error,
                         row$ci_low, row$ci_high)),
      fill = TRUE
    )
  }
  
  return(out)
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



# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")