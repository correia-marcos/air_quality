# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: COMPARE LASTEST VERSION AND THE NEW ONE
# 
# @Description: From 2000 to 2023 (NEED TO FINISH DOCUMENTATION)
# 
# @Summary: 
#   I.   Load libraries, utility functions and necessary data
#   II.  
#   III. 
# 
# @Date: May 2025
# @Author: Marcos
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_validation_old_version.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Import the raw panel of ground stations
bogota_stations_new <- read_parquet(here::here("data", "raw",
                                               "pollution_ground_stations",
                                               "Bogota",
                                               "bogota_stations_2000_2023.parquet"))

# Define the directory with legacy data
legacy_dir         <- here::here("data", "_legacy", "pollution", "Bogota")

# Define station dictionary (old -> new) to match older recording names
station_map <- c(
  "Centro de Alto Rendimiento" = "CAR",
  "Carvajal - Sevillana"       = "Carvajal-Sevillana",
  "Ciudad Bolivar"             = "CiudadBolivar",
  "Las Ferias"                 = "LasFerias",
  "Movil 7ma"                  = "Movil7ma",
  "Movil Fontibon"             = "MovilFontibon",
  "Puente Aranda"              = "PuenteAranda",
  "San Cristobal"              = "SanCristobal"
)

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function in Legacy data to read all four CSVs and harmonize them
legacy_raw <- read_legacy_period_csvs(
  dir = legacy_dir,
  pattern = "^Air_Pollution_Bogota_\\d{4}_\\d{4}\\.csv$"
)

# Apply function to harmonize values,types and the order of the old dataframe
bogota_old <- prepare_legacy_bogota(
  legacy_df     = legacy_raw,
  rename_map    = station_map,
  tz            = "America/Bogota",
  verbose       = TRUE) %>%
  arrange(station, datetime)

# Apply function to harmonize the new dataframe making it like the legacy one
# - restrict to 2002–2023 and shift hour (+1)
bogota_new <- prepare_new_bogota_like_legacy(
  new_df        = bogota_stations_new,
  rename_map    = station_map,
  year_keep     = 2002:2023,
  hour_shift    = 0L,                    # ← your earlier manual +1 hour
  tz            = "America/Bogota"
) %>%
  arrange(station, datetime)

# Apply function to Compare both datasets
res <- compare_panels(
  old_df = bogota_old,
  new_df = bogota_new,
  keys   = c("station", "year", "month", "day", "hour"),
  values = c("pm10", "pm25", "ozone", "co", "no2"),
  tol    = c(pm10 = 0, pm25 = 0, ozone = 0, co = 0, no2 = 0)
)

# Quick summaries for the console
message("\nRows only in legacy: ", nrow(res$only_old))
message("Rows only in new    : ", nrow(res$only_new))
print(res$diff_summary)

# Separate values to save
differences       <- res$diffs_long

# Quick check on the non missing data
differences_no_na <- differences %>%
  filter(within_tol == FALSE)

# Quick check on the missing data from before
new_only <- res$only_new %>%
  distinct(station, year, month, day, hour) %>%
  arrange(station, year, month, day, hour)

# Check the rows that exist only on the old data
old_only <- res$only_old
# ============================================================================================
# III: Save  data
# ============================================================================================
# Ensure output folder exists
outdir <- here::here("results", "validation_rep_package", "Bogota")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Save important file
saveRDS(object   = differences_no_na,
        file     = file.path(outdir, "differences_raw_ground_stations.rds"),
        compress = "xz")

arrow::write_parquet(x   = differences_no_na,
                    sink = file.path(outdir, "differences_raw_ground_stations.parquet"),
                    compression = "zstd")

write.csv(x = new_only,
          file = file.path(outdir, "missing_data.csv"))

# Print a success message for when running inside Docker Container
cat("Script from the IDB projected executed successfully in the Docker container!\n")