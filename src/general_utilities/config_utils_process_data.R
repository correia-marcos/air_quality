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
# Function: save_raw_data_tidy_formatted
# @Arg       : data            — data.frame / tibble to write
# @Arg       : out_dir         — string; directory to write outputs (created if missing)
# @Arg       : out_name        — string|NULL; base filename *without* extension
#                                 If NULL, inferred from available columns:
#                                   • if 'city' + 'year': "<city>_<minyear>_<maxyear>"
#                                   • if 'year' only   : "dataset_<minyear>_<maxyear>"
#                                   • else             : "dataset"
# @Arg       : write_rds       — logical; write .rds (default TRUE)
# @Arg       : write_parquet   — logical; write .parquet via {arrow} (default TRUE)
# @Arg       : write_csv_gz    — logical; write .csv.gz (default FALSE)
# @Arg       : rds_compress    — string; RDS compress method (default "xz")
# @Arg       : parquet_comp    — string; Parquet compression codec (default "zstd")
# @Arg       : quiet           — logical; suppress messages (default FALSE)
# @Output    : (invisible) list with paths written (rds, parquet, csv)
# @Purpose   : Materialize a dataframe to standard artifacts with a consistent base name.
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
  stopifnot(is.data.frame(data))

  # 1) ensure output dir exists
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # 2) infer default output name if not provided
  infer_stub <- function(df) {
    has_city <- "city" %in% names(df)
    has_year <- "year" %in% names(df)
    if (has_city && has_year) {
      city <- as.character(df$city[which(!is.na(df$city))[1]])
      ymin <- suppressWarnings(min(df$year, na.rm = TRUE))
      ymax <- suppressWarnings(max(df$year, na.rm = TRUE))
      if (is.finite(ymin) && is.finite(ymax) && nzchar(city)) {
        return(sprintf("%s_%d_%d", gsub("\\s+", "", city), ymin, ymax))
      }
    }
    if (has_year) {
      ymin <- suppressWarnings(min(data$year, na.rm = TRUE))
      ymax <- suppressWarnings(max(data$year, na.rm = TRUE))
      if (is.finite(ymin) && is.finite(ymax)) return(sprintf("dataset_%d_%d", ymin, ymax))
    }
    "dataset"
  }
  if (is.null(out_name) || !nzchar(out_name)) out_name <- infer_stub(data)
  
  # 3) write artifacts as requested
  paths <- list(rds = NA_character_, parquet = NA_character_, csv = NA_character_)

  if (isTRUE(write_rds)) {
    rds_path <- file.path(out_dir, paste0(out_name, ".rds"))
    saveRDS(data, rds_path, compress = rds_compress)
    paths$rds <- normalizePath(rds_path, winslash = "/", mustWork = FALSE)
    if (!quiet) message("💾 Wrote RDS → ", paths$rds)
  }

  if (isTRUE(write_parquet)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for Parquet output.
           Install it (e.g., renv::install('arrow')).")
    }
    pq_path <- file.path(out_dir, paste0(out_name, ".parquet"))
    arrow::write_parquet(data, pq_path, compression = parquet_comp)
    paths$parquet <- normalizePath(pq_path, winslash = "/", mustWork = FALSE)
    if (!quiet) message("🧱 Wrote Parquet → ", paths$parquet)
  }

  if (isTRUE(write_csv_gz)) {
    csv_path <- file.path(out_dir, paste0(out_name, ".csv.gz"))
    con <- gzfile(csv_path, open = "wt")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    readr::write_csv(data, con)
    paths$csv <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)
    if (!quiet) message("📝 Wrote CSV.GZ → ", paths$csv)
  }
  
  # 4) return the paths invisibly
  invisible(paths)
}


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
# @Arg stations_sf    : sf POINT object; monitoring stations. Must carry a unique station
#                       identifier column (station_id_col). Values are normalized (upper, 
#                       no accents) before being written to outputs so they match the
#                       station column in the Arrow pollution dataset.
# @Arg station_id_col : string; column in stations_sf with station IDs.
# @Arg geo_sf         : sf POLYGON object or NULL; geographic units (tracts,municipalities…). 
#                       Pass NULL to build only the station-to-station matrix.
# @Arg geo_id_col     : string or NULL; unique ID column in geo_sf. Required when geo_sf
#                       is not NULL.
# @Arg out_dir        : string; output directory (created recursively if absent).
# @Arg out_name       : string; file prefix, e.g. "bogota_2018". Outputs:
#                         {out_name}_station_distances.parquet
#                         {out_name}_geo_station_distances.parquet
# @Arg overwrite      : logical; if FALSE and outputs exist, read and return them without
#                       recomputing. Default TRUE.
# @Arg quiet          : logical; suppress progress messages. Default FALSE.
#
# @Output : Named list:
#   $station_matrix     — long data.table (station_from, station_to,
#                         distance_km). Diagonal (self = 0) is retained.
#   $geo_station_matrix — long data.table (geo_id, station_id, distance_km).
#                         NULL when geo_sf was not provided.
#
# @Details: An Azimuthal Equidistant projection (AEQD) centered on the station cloud 
# centroid is used for all distance computation. This gives accurate planar distances
# for local metro areas and is consistent with the buffer-filtering approach in 
# bogota_filter_stations_in_metro(). All outputs are saved as Parquet and can be 
# queried lazily with Arrow.
#
# @Written_on : 01/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
compute_distance_matrices <- function(
    stations_sf,
    station_id_col,
    geo_sf         = NULL,
    geo_id_col     = NULL,
    out_dir,
    out_name,
    overwrite      = TRUE,
    quiet          = FALSE
) {
  
  # 0. Dependency checks
  # ---------------------------------------------------------------------------
  pkgs <- c("sf", "data.table", "arrow", "stringi")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(
        "Package '", p, "' is required but not installed. ",
        "Add it to renv and run renv::restore()."
      )
    }
  }
  
  # 1. Helpers
  # ---------------------------------------------------------------------------
  
  # Normalize station IDs: uppercase, strip accents and quotes.
  # Mirrors standardize_name() in bogota_filter_stations_in_metro() so that
  # IDs in the distance Parquets match the 'station' column in Arrow data.
  .normalize <- function(x) {
    x <- toupper(trimws(x))
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    gsub('"', "", x)
  }
  
  # Build an AEQD proj4 string centred on a given WGS84 lon/lat.
  # AEQD preserves distances from the centre — ideal for metro areas of
  # ~50-200 km diameter, and consistent with bogota_filter_stations_in_metro.
  .aeqd_proj <- function(lon0, lat0) {
    sprintf(
      "+proj=aeqd +lat_0=%f +lon_0=%f +units=m +datum=WGS84 +no_defs",
      lat0, lon0
    )
  }
  
  # 2. Input validation
  # ---------------------------------------------------------------------------
  if (!inherits(stations_sf, "sf"))
    stop("`stations_sf` must be an sf object.")
  
  if (!station_id_col %in% names(stations_sf))
    stop(
      "Column '", station_id_col, "' not found in `stations_sf`. ",
      "Available: ", paste(names(stations_sf), collapse = ", ")
    )
  
  if (!is.null(geo_sf)) {
    if (!inherits(geo_sf, "sf"))
      stop("`geo_sf` must be an sf object or NULL.")
    if (is.null(geo_id_col))
      stop("`geo_id_col` is required when `geo_sf` is not NULL.")
    if (!geo_id_col %in% names(geo_sf))
      stop(
        "Column '", geo_id_col, "' not found in `geo_sf`. ",
        "Available: ", paste(names(geo_sf), collapse = ", ")
      )
  }
  
  # 3. Output paths + early-exit when overwrite = FALSE
  # ---------------------------------------------------------------------------
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    if (!quiet) message("Created output directory: ", out_dir)
  }
  
  path_sta <- file.path(
    out_dir,
    paste0(out_name, "_station_distances.parquet")
  )
  path_geo <- file.path(
    out_dir,
    paste0(out_name, "_geo_station_distances.parquet")
  )
  
  if (!overwrite) {
    geo_ready <- is.null(geo_sf) || file.exists(path_geo)
    if (file.exists(path_sta) && geo_ready) {
      if (!quiet) message(
        "Files exist and overwrite = FALSE — loading from disk."
      )
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
  }
  
  # 4. Build AEQD projection centred on the station cloud
  # ---------------------------------------------------------------------------
  if (!quiet) message("[", out_name, "] Building AEQD projection ...")
  
  stations_wgs <- sf::st_transform(stations_sf, crs = 4326)
  cen          <- sf::st_coordinates(
    sf::st_centroid(sf::st_union(stations_wgs))
  )
  proj_aeqd <- .aeqd_proj(lon0 = cen[1, "X"], lat0 = cen[1, "Y"])
  
  # Project and slim down to the ID column only (reduces st_distance memory)
  stations_aeqd <- sf::st_transform(
    stations_wgs[, station_id_col],
    crs = proj_aeqd
  )
  
  # Normalize IDs for consistent joins against the Arrow pollution dataset
  station_ids <- .normalize(
    as.character(stations_aeqd[[station_id_col]])
  )
  n_sta <- length(station_ids)
  
  # 5. Station-to-station distance matrix
  # ---------------------------------------------------------------------------
  if (!quiet) message(
    "[", out_name, "] Station x station distances (n = ", n_sta, ") ..."
  )
  
  # On an AEQD CRS, sf::st_distance() returns a plain numeric matrix in
  # metres without invoking the s2 spherical engine
  dist_sta_km <- as.numeric(
    sf::st_distance(stations_aeqd, stations_aeqd)
  ) / 1000
  
  station_dt <- data.table::data.table(
    station_from = rep(station_ids, times = n_sta),
    station_to   = rep(station_ids, each  = n_sta),
    distance_km  = dist_sta_km
  )
  
  if (!quiet) message("[", out_name, "] Writing: ", path_sta)
  arrow::write_parquet(station_dt, path_sta)
  
  # 6. Geographic unit centroid-to-station distances (optional)
  # ---------------------------------------------------------------------------
  geo_station_dt <- NULL
  
  if (!is.null(geo_sf)) {
    n_geo <- nrow(geo_sf)
    if (!quiet) message(
      "[", out_name, "] Geo x station distances",
      " (n_geo = ", n_geo, ", n_sta = ", n_sta, ") ..."
    )
    
    # Same AEQD projection keeps the scale consistent with step 5
    geo_aeqd <- sf::st_transform(
      sf::st_make_valid(geo_sf),
      crs = proj_aeqd
    )
    
    # of_largest_polygon = TRUE handles MultiPolygon robustly (e.g. RMSP).
    # Suppress the expected "attributes constant over geometry" warning.
    geo_centroids <- suppressWarnings(
      sf::st_centroid(geo_aeqd, of_largest_polygon = TRUE)
    )
    
    geo_ids <- as.character(geo_centroids[[geo_id_col]])
    
    # n_geo x n_sta matrix (metres), flattened column-major
    dist_geo_km <- as.numeric(
      sf::st_distance(geo_centroids, stations_aeqd)
    ) / 1000
    
    geo_station_dt <- data.table::data.table(
      geo_id      = rep(geo_ids,     times = n_sta),
      station_id  = rep(station_ids, each  = n_geo),
      distance_km = dist_geo_km
    )
    
    if (!quiet) message("[", out_name, "] Writing: ", path_geo)
    arrow::write_parquet(geo_station_dt, path_geo)
  }
  
  # 7. Return
  # ---------------------------------------------------------------------------
  if (!quiet) message(
    "[", out_name, "] compute_distance_matrices() completed."
  )
  
  invisible(list(
    station_matrix     = station_dt,
    geo_station_matrix = geo_station_dt
  ))
}


# --------------------------------------------------------------------------------------------
# Function: detect_pollution_outliers
#
# @Arg arrow_dir         : string; path to Arrow dataset of station's hourly data.
# @Arg station_dist_path : string; station_distances.parquet from compute_distance_matrices(). 
#                          Used to find each station's nearest geometric neighbor per 
#                          pollutant.
# @Arg out_dir           : string; output directory (created if absent).
# @Arg out_name          : string; prefix, e.g. "bogota_2018". Output folder: {out_name}_clean/.
# @Arg pollutants        : character; pollutants to process.
#                          Default c("pm10", "pm25").
# @Arg pct_flag          : numeric [0,1]; upper-tail quantile threshold for flagging within 
#                          station x year-month. Default 0.99.
# @Arg n_sd              : numeric; tolerance half-width in SD units. Default 2.
# @Arg overwrite         : logical; skip if output exists. Default TRUE.
# @Arg quiet             : logical; suppress messages. Default FALSE.
#
# @Output : Path to the output Arrow dataset (invisible string). All original columns
#           retained; partitioned by year (Hive-style). Added per pollutant p in the output:
#           {p}_outlier <int>  1 = observation was flagged and unreasonable.
#           Outlier {p} values are replaced with NA.
#
# @Details: Procedure per pollutant per year —
#   (1) Balance panel: complete station x hour grid for the year. One boundary row 
#       (last hr of yr-1; first hr of yr+1) is temporarily appended so lag/lead are 
#       defined at year edges. Boundary rows are removed before writing output.
#   (2) Nearest neighbour is pollutant-specific: the closest station (by km) with >= 1 
#       non-NA reading for the pollutant processed.
#       Dp   = p_t - p_{t-1}       (first difference within station).
#       Dp_n = p_t - p_{nearest,t} (difference vs nearest neighbour).
#   (3) Benchmark p-bar from lag/lead: avg if both present (Type 1), the available one 
#       if only one (Type 2), NA if neither (Type 3).
#   (4) Flag obs above pct_flag within station x year-month.
#   (5) Classify flagged obs as reasonable if
#       mu(Dp) - n_sd*sd(Dp) < p - p-bar < mu(Dp) + n_sd*sd(Dp).
#   (6) Reclassify unreasonable obs as reasonable if Dp_n falls
#       within mu(Dp_n) +/- n_sd*sd(Dp_n) (station x year-month).
#   (7) Flagged + unreasonable or no-benchmark -> NA; outlier flag = 1.
#
# @Written_on : 02/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
detect_pollution_outliers <- function(
    arrow_dir,
    station_dist_path,
    out_dir,
    out_name,
    pollutants  = c("pm10", "pm25"),
    pct_flag    = 0.99,
    n_sd        = 2,
    overwrite   = TRUE,
    quiet       = FALSE
) {
  
  # 0. Dependencies
  # -----------------------------------------------------------------------
  pkgs <- c("arrow", "data.table", "dplyr")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' required but not installed.")
  }
  
  # 1. Output path + early exit
  # -----------------------------------------------------------------------
  out_path <- file.path(out_dir, paste0(out_name, "_clean"))
  
  if (!overwrite && dir.exists(out_path)) {
    if (!quiet) message("Output exists; overwrite=FALSE — skipping.")
    return(invisible(out_path))
  }
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)
  if (dir.exists(out_path))
    unlink(out_path, recursive = TRUE)
  dir.create(out_path)
  
  # 2. Load full distance table
  #    Nearest-neighbour is resolved per pollutant inside .flag_pollutant,
  #    conditioning on stations that have data for that pollutant.
  # -----------------------------------------------------------------------
  dist_dt <- data.table::as.data.table(
    arrow::read_parquet(station_dist_path)
  )
  
  if (!quiet) message(
    "Distance table loaded: ",
    data.table::uniqueN(dist_dt$station_from), " stations."
  )
  
  # 3. Open Arrow dataset; collect year index
  # -----------------------------------------------------------------------
  arrow_ds <- arrow::open_dataset(arrow_dir)
  
  years <- arrow_ds |>
    dplyr::select(year) |>
    dplyr::distinct()   |>
    dplyr::collect()    |>
    dplyr::pull(year)   |>
    sort()
  
  if (!quiet) message(
    "Years to process: ",
    paste(range(years), collapse = "\u2013"),
    "  (n = ", length(years), ")"
  )
  
  # 4. Inner helper — flags one pollutant in-place on a data.table.
  #    Uses reference semantics (:=): the caller's object is updated
  #    without copying. Nearest neighbour is resolved here, restricted
  #    to stations with >= 1 non-NA value for the current pollutant.
  # -----------------------------------------------------------------------
  .flag_pollutant <- function(dt, pol, dist_dt, pct_flag, n_sd) {
    
    if (!pol %in% names(dt)) return(invisible(NULL))
    
    flag_col <- paste0(pol, "_outlier")
    dt[, (flag_col) := 0L]
    
    # Nothing to flag if this pollutant has no data this year
    if (all(is.na(dt[[pol]]))) return(invisible(NULL))
    
    # -- Pollutant-specific nearest neighbour -------------------------
    # For each station: the closest other station (by distance_km)
    # with at least one non-NA observation for this pollutant.
    has_data <- dt[!is.na(get(pol)), unique(station)]
    near_dt  <- dist_dt[
      distance_km > 0 & station_to %in% has_data
    ][
      , .SD[which.min(distance_km)], by = station_from
    ][
      , .(station = station_from, nearest = station_to)
    ]
    
    # -- (2) Lag, lead, and first differences within station ----------
    dt[,
       `:=`(
         .t_lag  = data.table::shift(get(pol), 1L, type = "lag"),
         .t_lead = data.table::shift(get(pol), 1L, type = "lead")
       ),
       by = station
    ]
    dt[, .t_diff := get(pol) - .t_lag]         # Dp = p_t - p_{t-1}
    
    # Neighbour difference via a keyed self-join:
    #   Step 1 — add the nearest-neighbour name for each station
    dt[near_dt, .t_near := i.nearest, on = "station"]
    #   Step 2 — slim lookup: station x datetime -> pollutant value
    tmp_lkp <- dt[, .(station, datetime, tmp_p = get(pol))]
    #   Step 3 — join: get the neighbour's value at the same time
    dt[
      tmp_lkp,
      .t_vn := i.tmp_p,
      on = .(.t_near = station, datetime)
    ]
    rm(tmp_lkp)
    dt[, .t_diff_nb := get(pol) - .t_vn]       # Dp_n = p_t - p_{n,t}
    dt[, c(".t_near", ".t_vn") := NULL]
    
    # -- (3) Benchmark p-bar and deviation from benchmark -------------
    dt[, .t_bench := data.table::fcase(
      !is.na(.t_lag) & !is.na(.t_lead),
      (.t_lag + .t_lead) / 2,   # Type 1: avg(lag, lead)
      !is.na(.t_lag),  .t_lag,  # Type 2a: lag only
      !is.na(.t_lead), .t_lead, # Type 2b: lead only
      default = NA_real_        # Type 3: no benchmark
    )]
    dt[, .t_btype := data.table::fcase(
      !is.na(.t_lag) & !is.na(.t_lead), 1L,
      !is.na(.t_lag) | !is.na(.t_lead), 2L,
      default = 3L
    )]
    dt[, .t_diff_b := get(pol) - .t_bench]     # p - p-bar
    
    # -- (4) Flag obs above pct_flag quantile (station x year-month) --
    dt[, .t_ym := format(datetime, "%Y-%m")]
    dt[,
       .t_p99 := as.numeric(
         stats::quantile(.SD[[1]], probs = pct_flag, na.rm = TRUE)
       ),
       by     = .(station, .t_ym),
       .SDcols = pol
    ]
    dt[,
       .t_flag := data.table::fifelse(
         !is.na(get(pol)) & get(pol) > .t_p99, 1L, 0L
       )
    ]
    
    # -- (5)/(6) Station-month stats on Dp and Dp_n ------------------
    dt[, `:=`(
      .t_md  = mean(.t_diff,    na.rm = TRUE),  # mean(Dp)
      .t_sd  = sd(.t_diff,      na.rm = TRUE),  # sd(Dp)
      .t_mnb = mean(.t_diff_nb, na.rm = TRUE),  # mean(Dp_n)
      .t_snb = sd(.t_diff_nb,   na.rm = TRUE)   # sd(Dp_n)
    ), by = .(station, .t_ym)]
    
    # -- (5) Classify: 1 = reasonable, 2 = unreasonable, 3 = no bench --
    dt[, .t_cat := data.table::fcase(
      .t_btype == 3L, 3L,
      !is.na(.t_diff_b) &
        .t_diff_b > (.t_md - n_sd * .t_sd) &
        .t_diff_b < (.t_md + n_sd * .t_sd), 1L,
      default = 2L
    )]
    
    # -- (6) Reclassify unreasonable using neighbour distribution -----
    dt[
      .t_cat     == 2L         &
        !is.na(.t_diff_nb)     &
        !is.na(.t_mnb)         &
        !is.na(.t_snb)         &
        .t_diff_nb > (.t_mnb - n_sd * .t_snb) &
        .t_diff_nb < (.t_mnb + n_sd * .t_snb),
      .t_cat := 1L
    ]
    
    # -- (7) Set outlier flag; replace pollutant value with NA --------
    dt[
      .t_flag == 1L & .t_cat %in% c(2L, 3L),
      (flag_col) := 1L
    ]
    dt[get(flag_col) == 1L, (pol) := NA_real_]
    
    # Drop all temporary columns
    dt[, c(
      ".t_lag", ".t_lead", ".t_diff", ".t_diff_nb",
      ".t_bench", ".t_btype", ".t_diff_b",
      ".t_ym", ".t_p99", ".t_flag",
      ".t_md", ".t_sd", ".t_mnb", ".t_snb", ".t_cat"
    ) := NULL]
    
    invisible(NULL)
  }

  # 5. Year loop
  # -----------------------------------------------------------------------
  for (yr in years) {
    if (!quiet) message("  [", yr, "] Collecting ...")
    
    # -- Collect current year -----------------------------------------
    dt_yr <- arrow_ds |>
      dplyr::filter(year == yr) |>
      dplyr::collect()           |>
      data.table::as.data.table()
    
    if (nrow(dt_yr) == 0) {
      if (!quiet) message("    No rows — skipped.")
      next
    }
    
    all_sta  <- unique(dt_yr$station)
    yr_start <- min(dt_yr$datetime)
    yr_end   <- max(dt_yr$datetime)
    
    # -- Boundary rows: 1 hr before yr_start and 1 hr after yr_end ---
    #    Appended temporarily so lag/lead are defined at year edges.
    #    Filtered to stations active in yr; removed before writing.
    #    Year of the boundary hour is derived from its timestamp so
    #    the correct Hive partition is scanned.
    prev_cutoff <- yr_start - 3600
    next_cutoff <- yr_end   + 3600
    
    prev_yr  <- as.integer(format(prev_cutoff, "%Y"))
    next_yr  <- as.integer(format(next_cutoff, "%Y"))
    
    bnd_prev <- arrow_ds |>
      dplyr::filter(
        year == prev_yr, datetime == prev_cutoff
      ) |>
      dplyr::collect()  |>
      data.table::as.data.table()
    bnd_prev <- bnd_prev[station %in% all_sta]
    
    bnd_next <- arrow_ds |>
      dplyr::filter(
        year == next_yr, datetime == next_cutoff
      ) |>
      dplyr::collect()  |>
      data.table::as.data.table()
    bnd_next <- bnd_next[station %in% all_sta]
    
    # -- Build balanced panel: all stations x every hour in the year --
    # Station set is derived from yr data so newly commissioned or
    # decommissioned stations are handled correctly.
    all_hours <- seq(yr_start, yr_end, by = "hour")
    grid      <- data.table::CJ(
      station  = all_sta,
      datetime = all_hours
    )
    
    non_key <- setdiff(
      names(dt_yr), c("station", "datetime", "year")
    )
    dt_bal <- data.table::merge.data.table(
      grid,
      dt_yr[, c("station", "datetime", non_key), with = FALSE],
      by    = c("station", "datetime"),
      all.x = TRUE
    )
    dt_bal[, year := yr]
    
    # Append boundary rows; sort by station x datetime
    dt_bal <- data.table::rbindlist(
      list(dt_bal, bnd_prev, bnd_next),
      fill      = TRUE,
      use.names = TRUE
    )
    data.table::setorder(dt_bal, station, datetime)
    
    # Logical index for target-year rows (counting + output filter)
    in_yr <- dt_bal$datetime >= yr_start &
      dt_bal$datetime <= yr_end
    
    if (!quiet) message(
      "    Balanced: ",
      format(sum(in_yr), big.mark = ","),
      " rows | ", length(all_sta), " stations"
    )
    
    # -- Flag each pollutant in-place via reference semantics ---------
    for (pol in pollutants) {
      if (!quiet) message("    Flagging: ", pol, " ...")
      .flag_pollutant(dt_bal, pol, dist_dt, pct_flag, n_sd)
      
      if (!quiet) {
        fc    <- paste0(pol, "_outlier")
        n_out <- if (fc %in% names(dt_bal))
          sum(dt_bal[[fc]][in_yr], na.rm = TRUE)
        else 0L
        n_obs <- sum(!is.na(dt_yr[[pol]]))
        pct   <- round(100 * n_out / max(n_obs, 1L), 2)
        message(
          "      Outliers: ", n_out,
          " / ", format(n_obs, big.mark = ","),
          " original obs (", pct, "%)"
        )
      }
    }
    
    # -- Drop boundary rows; write only target-year rows --------------
    dt_out <- dt_bal[in_yr]
    
    yr_dir <- file.path(out_path, paste0("year=", yr))
    dir.create(yr_dir, showWarnings = FALSE)
    arrow::write_parquet(
      dt_out,
      file.path(yr_dir, "data.parquet"),
      compression = "snappy"
    )
    
    rm(dt_yr, dt_bal, dt_out, grid, bnd_prev, bnd_next, in_yr)
    gc(verbose = FALSE)
  }
  
  if (!quiet) message("Completed. Output:\n  ", out_path)
  invisible(out_path)
}


# --------------------------------------------------------------------------------------------
# Function: aggregate_idw_exposure
#
# @Arg arrow_dir       : string; path to the partitioned Arrow/Parquet dataset.
# @Arg geo_sta_pq      : string; geo-station distance Parquet from compute_distance_matrices()
#                        (geo_id, station_id, distance_km).
# @Arg census_col      : data.frame; census data. Content depends on quintile_level
#                        see @Details.
# @Arg geo_id_col      : string; geo ID column in census_col. Default "GEO_ID".
# @Arg pop_col         : string; population/weight column in census_col.
#                        Default "n" (geo mode) — set to "fe" for individual.
# @Arg edu_col         : string; education column in census_col used to assign quintiles. 
#                        Default "escolaridad_avg" (geo mode) — set to "escolaridad" for 
#                        individual.
# @Arg quintile_level  : string; "geo" (default) or "individual". See @Details.
# @Arg indiv_adult_col : string; column used to filter to adults in individual mode. 
#                        Default "adult". Ignored in geo mode.
# @Arg buffer_km       : numeric; max centroid-to-station distance. Default 3.
# @Arg pollutants      : character vector; default c("pm10", "pm25").
# @Arg who_it          : named list of WHO interim target thresholds (μg/m³).
# @Arg mem_gb          : numeric; DuckDB memory ceiling in GB. Default 16.
# @Arg out_dir         : string; output directory.
# @Arg out_name        : string; file prefix.
# @Arg overwrite       : logical; default TRUE.
# @Arg quiet           : logical; default FALSE.
#
# @Output : Named list. Both modes include $exposure_yearly (geo-level data.table, one row 
#   per geo_id × year) and $exposure_path. Individual mode additionally includes 
#   $individual_quintiles (one row per person with edu_quintile assigned) and $individual_path.
#
# @Details:
#   QUINTILE MODES
#   "geo" (default) — census_col is the collapsed census (one row per geo
#     unit). Quintiles are assigned at the geographic level: geo units are
#     sorted by their average education (edu_col) and grouped into quintiles
#     based on cumulative population shares. This answers "do lower-educated
#     places face worse monitoring coverage and exposure?"
#
#   "individual" — census_col is the individual microdata (one row per
#     person). Quintiles are assigned city-wide across individuals: adults
#     (indiv_adult_col == 1) are sorted by their personal years of schooling
#     (edu_col) and grouped into quintiles using expansion factor weights
#     (pop_col). The geo-level exposure is saved separately; the user merges
#     the two outputs by geo_id for any specific year. This matches the
#     approach of the original coauthor scripts and answers "do lower-educated
#     individuals tend to live in places with worse exposure?"
#
#   GEO_ID TYPE NOTE
#   If geo_id_col is stored as numeric double in census_col (e.g. 22-digit
#   DANE codes), values > 1e15 cannot be represented exactly in double
#   precision. Store GEO_ID as character in the census processing step to
#   avoid silent join failures. The function will warn and attempt a best-
#   effort character conversion using sprintf("%.0f", x).
#
#   IDW COMPUTATION (identical for both modes)
#   All heavy computation runs inside DuckDB. R only receives the final
#   small geo-level summary. The year × pollutant loop bounds DuckDB's
#   working set to one year at a time. Requires DuckDB >= 0.9.2.
#
# @Written_on : 02/02/2026
# @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
aggregate_idw_exposure <- function(
    arrow_dir,
    geo_sta_pq,
    census_col,
    geo_id_col      = "GEO_ID",
    pop_col         = "n",
    edu_col         = "escolaridad_avg",
    quintile_level  = c("geo", "individual"),
    indiv_adult_col = "adult",
    buffer_km       = 3,
    pollutants      = c("pm10", "pm25"),
    who_it          = list(
      pm10 = c(it1 = 150, it2 = 100, it3 = 75,  it4 = 50),
      pm25 = c(it1 = 75,  it2 = 50,  it3 = 37.5, it4 = 25)
    ),
    mem_gb          = 16,
    out_dir         = "data/interim/exposure",
    out_name,
    overwrite       = TRUE,
    quiet           = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # 0. Dependencies + argument matching
  # ---------------------------------------------------------------------------
  pkgs <- c("duckdb", "DBI", "arrow", "data.table", "dplyr")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' required but not installed.")
  }
  if (utils::packageVersion("duckdb") < "0.9.2")
    stop("DuckDB >= 0.9.2 required.")
  
  quintile_level <- match.arg(quintile_level)
  
  if (!dir.exists(arrow_dir))
    stop("`arrow_dir` not found: ", arrow_dir)
  if (!file.exists(geo_sta_pq))
    stop("`geo_sta_pq` not found: ", geo_sta_pq)
  
  for (col in c(geo_id_col, pop_col, edu_col)) {
    if (!col %in% names(census_col))
      stop("Column '", col, "' not found in census_col.")
  }
  if (quintile_level == "individual" &&
      !indiv_adult_col %in% names(census_col)) {
    stop(
      "Column '", indiv_adult_col, "' not found in census_col. ",
      "Set indiv_adult_col to the adult-flag column name, ",
      "or filter adults before passing census_col."
    )
  }
  
  # ---------------------------------------------------------------------------
  # 1. Output paths + early exit
  # ---------------------------------------------------------------------------
  out_path   <- file.path(
    out_dir, paste0(out_name, "_idw_exposure.parquet")
  )
  indiv_path <- file.path(
    out_dir, paste0(out_name, "_individual_quintiles.parquet")
  )
  
  if (!overwrite) {
    geo_done   <- file.exists(out_path)
    indiv_done <- quintile_level == "geo" || file.exists(indiv_path)
    if (geo_done && indiv_done) {
      if (!quiet) message(
        "Outputs exist — skipping (overwrite = FALSE)."
      )
      out <- list(
        exposure_yearly = data.table::as.data.table(
          arrow::read_parquet(out_path)
        ),
        exposure_path = out_path
      )
      if (quintile_level == "individual") {
        out$individual_quintiles <- data.table::as.data.table(
          arrow::read_parquet(indiv_path)
        )
        out$individual_path <- indiv_path
      }
      return(invisible(out))
    }
  }
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---------------------------------------------------------------------------
  # 2. Helpers
  # ---------------------------------------------------------------------------
  
  # Normalise path separators for DuckDB (always forward slash)
  .dq_path <- function(p) {
    paste0("'", gsub("'", "''", gsub("\\\\", "/", p)), "'")
  }
  
  # Safely convert geo_id to character without scientific notation.
  # Doubles > 1e15 lose precision — warn the user.
  .safe_chr <- function(x) {
    if (is.character(x)) return(x)
    if (any(!is.na(x) & abs(x) > 1e15)) {
      warning(
        "geo_id values > 1e15 cannot be represented exactly as ",
        "double. Store GEO_ID as character in the census ",
        "processing step to avoid silent join failures. A best-",
        "effort conversion via sprintf('%.0f') is applied."
      )
    }
    ifelse(is.na(x), NA_character_, sprintf("%.0f", x))
  }
  
  # ---------------------------------------------------------------------------
  # 3. DuckDB disk-backed connection
  # ---------------------------------------------------------------------------
  if (!quiet) message("[", out_name, "] Starting DuckDB engine ...")
  
  dbdir <- tempfile("idw_duck_", fileext = ".db")
  con   <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    try(unlink(dbdir, recursive = TRUE, force = TRUE), silent = TRUE)
  }, add = TRUE)
  
  n_thr <- max(1L, parallel::detectCores() - 1L)
  DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", n_thr))
  DBI::dbExecute(con, sprintf(
    "PRAGMA memory_limit='%dGB';", as.integer(mem_gb)
  ))
  
  # ---------------------------------------------------------------------------
  # 4. dist_tbl — pre-compute inv_d once; persists across the year loop
  # ---------------------------------------------------------------------------
  if (!quiet) message("[", out_name, "] Loading distances ...")
  
  DBI::dbExecute(con, paste0(
    "CREATE TABLE dist_tbl AS\n",
    "SELECT geo_id, station_id,\n",
    "       1.0 / distance_km AS inv_d\n",
    "FROM read_parquet(", .dq_path(geo_sta_pq), ")\n",
    "WHERE distance_km > 0 AND distance_km <= ", buffer_km, ";"
  ))
  
  n_geo <- DBI::dbGetQuery(
    con, "SELECT COUNT(DISTINCT geo_id) AS n FROM dist_tbl;"
  )$n
  n_sta <- DBI::dbGetQuery(
    con, "SELECT COUNT(DISTINCT station_id) AS n FROM dist_tbl;"
  )$n
  
  if (n_geo == 0L)
    stop("No geo-station pairs within ", buffer_km, " km.")
  if (!quiet) message(
    "  ", n_geo, " geo units | ",
    n_sta, " stations within ", buffer_km, " km"
  )
  
  # ---------------------------------------------------------------------------
  # 5. Pollution VIEW — year filter pushed as Hive partition predicate
  # ---------------------------------------------------------------------------
  poll_glob <- paste0(gsub("\\\\", "/", arrow_dir), "/**/*.parquet")
  DBI::dbExecute(con, paste0(
    "CREATE VIEW pollution AS\n",
    "SELECT * FROM read_parquet(",
    .dq_path(poll_glob),
    ", hive_partitioning = true);"
  ))
  
  # ---------------------------------------------------------------------------
  # 6. Year list (no data collected yet)
  # ---------------------------------------------------------------------------
  years <- sort(DBI::dbGetQuery(
    con, "SELECT DISTINCT year FROM pollution ORDER BY year;"
  )$year)
  
  if (!quiet) message(
    "[", out_name, "] ",
    length(years), " year(s): ", min(years), "-", max(years)
  )
  
  # ---------------------------------------------------------------------------
  # 7. Year × pollutant loop
  #
  #   Per query working set:
  #     Non-null station readings for the year  (filtered, streamed)
  #   → JOIN dist_tbl       (only pairs within buffer)
  #   → GROUP BY geo × hour (IDW per geo per hour)
  #   → GROUP BY geo        (yearly avg + WHO hours) → collected into R
  #
  #   DuckDB spills to dbdir when mem_gb is exceeded.
  # ---------------------------------------------------------------------------
  yearly_list <- vector("list", length(years))
  names(yearly_list) <- as.character(years)
  
  for (yr in years) {
    if (!quiet) message("[", out_name, "] Year ", yr, " ...")
    poll_results <- vector("list", length(pollutants))
    names(poll_results) <- pollutants
    
    for (poll in pollutants) {
      
      # Build the WHO threshold aggregation fragment for this pollutant.
      # Each threshold becomes one SUM(CASE WHEN ...) column.
      who_frag <- ""
      thr <- who_it[[poll]]
      if (!is.null(thr) && length(thr) > 0) {
        who_frag <- paste(
          vapply(names(thr), function(nm) {
            sprintf(
              paste0(",\n         SUM(CASE WHEN idw >= %s",
                     " THEN 1 ELSE 0 END) AS %s"),
              thr[[nm]],
              paste0("hrs_d_", poll, "_", nm)
            )
          }, character(1)),
          collapse = ""
        )
      }
      
      # Core IDW query — entirely inside DuckDB:
      #   h      : non-null hourly readings for this year
      #   hr_geo : IDW = Σ(v_i/d_i) / Σ(1/d_i) per geo × hour
      #            denominator uses only stations reporting that hour,
      #            so missing station-hours do not dilute active ones
      #   result : yearly avg concentration + WHO hours per geo unit
      query <- paste0(
        "WITH hr_geo AS (\n",
        "  SELECT d.geo_id, h.datetime,\n",
        "         SUM(h.val * d.inv_d) / SUM(d.inv_d) AS idw\n",
        "  FROM (\n",
        "    SELECT station, datetime, ", poll, " AS val\n",
        "    FROM pollution\n",
        "    WHERE year = ", yr, " AND ", poll, " IS NOT NULL\n",
        "  ) h\n",
        "  JOIN dist_tbl d ON h.station = d.station_id\n",
        "  GROUP BY d.geo_id, h.datetime\n",
        ")\n",
        "SELECT geo_id,\n",
        "       AVG(idw)    AS avg_", poll, ",\n",
        "       COUNT(*)    AS total_hrs_", poll,
        who_frag, "\n",
        "FROM hr_geo\n",
        "GROUP BY geo_id;"
      )
      
      res <- tryCatch(
        data.table::as.data.table(DBI::dbGetQuery(con, query)),
        error = function(e) {
          warning(
            "Query failed for ", poll, " / ", yr,
            ": ", e$message
          )
          NULL
        }
      )
      
      if (!is.null(res) && nrow(res) > 0L)
        poll_results[[poll]] <- res
    }
    
    valid <- Filter(Negate(is.null), poll_results)
    if (length(valid) == 0L) next
    
    yr_exp <- Reduce(
      function(a, b) merge(a, b, by = "geo_id", all = TRUE),
      valid
    )
    yr_exp[, year := yr]
    yearly_list[[as.character(yr)]] <- yr_exp
  }
  
  # Stack all years (geo-level, small)
  all_years <- data.table::rbindlist(
    Filter(Negate(is.null), yearly_list),
    fill = TRUE
  )
  if (nrow(all_years) == 0L)
    stop("No exposure data produced. Check arrow_dir and pollutants.")
  
  # Ensure geo_id is character for consistent joining
  all_years[, geo_id := as.character(geo_id)]
  
  # ---------------------------------------------------------------------------
  # 8. Census processing — branches on quintile_level
  # ---------------------------------------------------------------------------
  
  census_dt <- data.table::as.data.table(census_col)
  data.table::setnames(census_dt, geo_id_col, "geo_id")
  census_dt[, geo_id := .safe_chr(geo_id)]
  
  if (quintile_level == "geo") {
    # ------------------------------------------------------------------
    # GEO MODE
    # Quintile boundaries are drawn at the geographic unit level.
    # Units are sorted by their average education; quintile 1 contains
    # the 20% of the city's *population* living in the least-educated
    # geo units. Within a unit, everyone shares the same quintile.
    # ------------------------------------------------------------------
    data.table::setorderv(census_dt, edu_col)
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
    
    if (!quiet) message(
      "[", out_name, "] Written (geo mode): ", out_path,
      "\n  Rows: ",  nrow(result),
      " | Geo: ",   data.table::uniqueN(result$geo_id),
      " | Years: ", data.table::uniqueN(result$year)
    )
    
    return(invisible(list(
      exposure_yearly = result,
      exposure_path   = out_path
    )))
    
  } else {
    # ------------------------------------------------------------------
    # INDIVIDUAL MODE
    # Quintile boundaries are drawn city-wide at the individual level.
    # Adults are sorted by their personal years of schooling; quintile 1
    # contains the 20% of *individual adults* with the fewest years of
    # schooling, regardless of where they live.
    # This matches the coauthor's individual-level approach.
    #
    # Because the census is a static snapshot (one year) and exposure
    # varies annually, the two outputs are saved separately:
    #   exposure_path  : geo-level exposure × year (small)
    #   individual_path: individual microdata + edu_quintile (large)
    # The user merges them by geo_id for whichever year they need,
    # avoiding creating a huge (individuals × years) object in memory.
    # ------------------------------------------------------------------
    
    # Filter to adults
    census_dt <- census_dt[get(indiv_adult_col) == 1]
    
    if (nrow(census_dt) == 0L)
      stop(
        "No adult rows in census_col after filtering on '",
        indiv_adult_col, "' == 1."
      )
    
    # Sort individuals by personal education and compute city-wide
    # cumulative population share using expansion factors
    data.table::setorderv(census_dt, edu_col)
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
    
    # Save geo-level exposure (same as geo mode, but without census join)
    arrow::write_parquet(all_years, out_path)
    
    # Save individual quintiles separately
    arrow::write_parquet(census_dt, indiv_path)
    
    if (!quiet) message(
      "[", out_name, "] Written (individual mode):",
      "\n  Geo exposure:          ", out_path,
      "\n    Rows: ",  nrow(all_years),
      " | Geo: ", data.table::uniqueN(all_years$geo_id),
      " | Years: ", data.table::uniqueN(all_years$year),
      "\n  Individual quintiles:  ", indiv_path,
      "\n    Rows: ", format(nrow(census_dt), big.mark = ","),
      " | Quintile dist: ",
      paste(
        census_dt[, .N, by = edu_quintile
        ][order(edu_quintile)]$N,
        collapse = "/"
      )
    )
    
    return(invisible(list(
      exposure_yearly      = all_years,
      exposure_path        = out_path,
      individual_quintiles = census_dt,
      individual_path      = indiv_path
    )))
  }
}


# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")