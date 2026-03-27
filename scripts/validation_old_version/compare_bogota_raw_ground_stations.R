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
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "bogota.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Define the output general folders
legacy_dir_area   <- here::here("data", "_legacy", "cities_shapefiles")
legacy_dir_pol    <- here::here("data", "_legacy", "pollution")
outdir_pollution  <- here::here(bogota_cfg$out_dir, "monitoring_stations")
outdir_geospatial <- here::here(bogota_cfg$out_dir, "geospatial_data")

# Define the file's specific location
bogota_metro_2018_gpkg <- here::here(outdir_geospatial, "bogota", "bogota_area_metro_2018.gpkg")
bogota_metro_2005_gpkg <- here::here(outdir_geospatial, "bogota", "bogota_area_metro_2005.gpkg")
bogota_pollution       <- here::here(outdir_pollution, "bogota_metro_dataset")
legacy_metro_2005_shp  <- here::here(legacy_dir_area, "Bogota_metro")
legacy_pollution_files <- here::here(legacy_dir_pol, "Bogota")

# Read the necessary files
bogota_2018_metro   <- st_read(bogota_metro_2018_gpkg)
bogota_2005_metro   <- st_read(bogota_metro_2005_gpkg)
legacy_bogota_metro <- sf::st_read(legacy_metro_2005_shp)
bogota_stations_new <- open_dataset(bogota_pollution)

# Define station dictionary (old -> new) to match older recording names
station_map <- c(
  "CENTRO DE ALTO RENDIMIENTO" = "CAR",
  "CARVAJAL-SEVILLANA"         = "Carvajal-Sevillana",
  "CIUDAD BOLIVAR"             = "CiudadBolivar",
  "LAS FERIAS"                 = "LasFerias",
  "MOVIL 7MA"                  = "Movil7ma",
  "MOVIL FONTIBON"             = "MovilFontibon",
  "PUENTE ARANDA"              = "PuenteAranda",
  "SAN CRISTOBAL"              = "SanCristobal",
  "GUAYMARAL"                  = "Guaymaral",
  "USAQUEN"                    = "Usaquen",
  "MINAMBIENTE"                = "MinAmbiente",
  "KENNEDY"                    = "Kennedy",
  "TUNAL"                      = "Tunal",
  "BOLIVIA"                    = "Bolivia",
  "FONTIBON"                   = "Fontibon",
  "COLINA"                     = "Colina",
  "USME"                       = "Usme",
  "SUBA"                       = "Suba",
  "EL JAZMIN"                  = "Jazmin",
  "JAZMIN"                     = "Jazmin"
)

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function in Legacy data to read all four CSVs and harmonize them
legacy_raw <- read_legacy_period_csvs(
  dir = legacy_pollution_files,
  pattern = "^Air_Pollution_Bogota_\\d{4}_\\d{4}\\.csv$"
)

# Apply function to harmonize values,types and the order of the old dataframe
bogota_old <- prepare_legacy_bogota(
  legacy_df     = legacy_raw,
  rename_map    = station_map,
  tz            = "UTC",
  verbose       = TRUE) %>%
  arrange(station, datetime) %>% 
  filter(year == 2023)

# Apply function to harmonize the new dataframe making it like the legacy one
# - restrict to 2002–2023 and shift hour (+1)
bogota_new <- prepare_new_bogota_like_legacy(
  new_df        = bogota_stations_new,
  rename_map    = station_map,
  year_keep     = 2002:2023,
  hour_shift    = 0L,                    # ← your earlier manual +1 hour
  tz            = "UTC"
) %>%
  filter(year == 2023) %>%
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
  filter(within_tol == FALSE) %>% 
  filter(year == 2023)

# Are there big differences?
big_diff <- differences_no_na %>% 
  filter(absv >= 1) 
#  Nope!

# Quick check on the missing data for PM10
differences_na_pm10 <- differences %>%
  filter(is.na(value_old)) %>%
  filter(year == 2023) %>%
  filter(variable == "pm10")

# Quick check on the missing data for PM2.5
differences_na_pm25 <- differences %>%
  filter(is.na(value_old)) %>%
  filter(year == 2023) %>%
  filter(variable == "pm25")

# Quick check on the missing data from before
new_only <- res$only_new %>%
  distinct(station, year, month, day, hour) %>%
  arrange(station, year, month, day, hour) %>% 
  filter(year == 2023)

no_dec <- differences_na_pm10 %>% 
  filter(month != 12)
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
