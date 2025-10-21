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
  "lubridate",
  "memuse",
  "readr",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "stringi",
  "terra",
  "tibble",
  "tidyr",
  "rlang")

# Strict check: fail fast if something isn't in the project library
ensure_installed <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop(
      "Missing packages: ", paste(miss, collapse = ", "),
      ". Run renv::restore() (or install locally with renv::install() then renv::snapshot())."
    )
  }
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

# Print a success message for when running inside Docker Container
cat("Config script parsed successfully!\n")