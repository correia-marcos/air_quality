# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Download MERRA-2 aerosol diagnostics files for a specified date range
# 
# @Description: This script uses functions previously created on the config utilities file to 
# generate daily URLs as a vector and then download all .nc4 files the vector defines
# 
# @Summary: 
#   I.   Load packages and utility functions
#   II.  Parse command-line arguments
#   III. Generate MERRA-2 URLs
#   IV.  Download files and report status
# 
# @Date: Apr 2025
# @Author: Marcos Paulo
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "config_utils_download_data.R"))

# ============================================================================================
# I: Import (create) data
# ============================================================================================
# If we’re inside RStudio or other IDE (interactive format), supply sensible defaults:
if (interactive()) {
  start_date      <- "2023-01-01"
  end_date        <- "2023-12-31"
  dataset_version <- "M2T1NXAER.5.12.4"
  destination_dir <- "data/raw/merra2_aerosol_products"
  var_name        <- "tavg1_2d_aer_Nx"
  } else {
  # running via Rscript / Docker entrypoint
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 4) {
    stop("Usage: Rscript run_download_merra2.R",
         "<start_date> <end_date> <dataset_version> <dest_dir> [var_name]")
  }
  start_date      <- args[1]                # "YYYY-MM-DD"
  end_date        <- args[2]                # "YYYY-MM-DD"
  dataset_version <- args[3]                # e.g. "M2T1NXAER.5.12.4"
  dest_dir        <- args[4]                # e.g. "data/raw/merra2"
  var_name        <- if (!is.na(args[5])) args[5] else "tavg1_2d_aer_Nx"    # optional
}

# ============================================================================================
# II: Process and download data 
# ============================================================================================
# Ensure output folder exists
outdir <- here("data", "raw", "merra2_aerosol_products")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Generate URLs
urls    <- generate_merra2_urls(
            start_date      = start_date,
            end_date        = end_date,
            dataset_version = dataset_version,
            tile_id         = "400",          # fixed for aerosol diagnostics
            var_name         = var_name)

# Download files
results <- download_merra2_files(
            urls     = urls,
            dest_dir = destination_dir,
            overwrite = TRUE)
# ---
# Check the download process
# ---

# Summary
total   <- length(results)
success <- sum(results)
failed  <- total - success

# Show the result 
cat(sprintf("Downloaded %d/%d files successfully. %d failed.\n", success, total, failed))
if (failed > 0) {
  quit(status = 1)
} else {
  quit(status = 0)
}
