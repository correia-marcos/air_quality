# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Process all downloaded data from the Bogota city ground stations. 
# The idea here is to transform the initial data Bogota's metro area - with all theirs specifics
# into a format that is standard for all cities we assess in the project.
# 
# @Description: 
# 
# @Summary: 
#   I.   Load libraries, utility functions and necessary data
#   II.  
#   III. 
# 
# @Date: May 2025
# @Author: Marcos
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src","city_specific", "registry.R"))
source(here::here("src","city_specific", "bogota.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Define the output folders
outdir_pollution  <- here::here(bogota_cfg$out_dir, "air_monitoring_stations")
outdir_geospatial <- here::here(bogota_cfg$out_dir, "geospatial_data")

# Read the geospatial data
bogota_metro      <- st_read(here::here(outdir_geospatial,
                                        "metro_areas",
                                        "bogota_metro.gpkg"))
bogota_metro_old  <- st_read(here::here(bogota_cfg$out_dir,
                                        "cities_shapefiles(old)",
                                        "Bogota_metro"))
# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function to merge all downloaded file into a single tidy dataframe
bogota_stations_data <- bogota_merge_stations_downloads(
  downloads_folder = here::here(bogota_cfg$dl_dir, "Ground_stations"),
  cleanup = FALSE,
  tz = "America/Bogota"
)

# Check coverage
miss <- bogota_missing_matrix(bogota_stations_data, years = bogota_cfg$years)
if (nrow(miss)) print(head(miss, 20))

# Apply function to process census data (unzip, filter and harmonize)
res <- bogota_filter_harmonize_census(
  census_zip = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip"),
  out_dir    = here::here("data", "raw", "census", "Bogota", "CG2005"),
  overwrite  = FALSE,
  quiet      = FALSE
)


# ============================================================================================
# II: CHECK  data
# ============================================================================================

check = unzip(here::here(bogota_cfg$dl_dir, "census", "11.Bogota.zip"),
              exdir = here::here(bogota_cfg$dl_dir, "census"))

list_files <- unzip(here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip"),
                    list = TRUE)

file.rename(from = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip",
                              "CG2005_AMPLIADO_ANDA/11.Bogot\xa0.zip"),
            to   = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip",
                              "CG2005_AMPLIADO_ANDA/11.Bogota.zip"))

insiside_bogota <- unzip(here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip",
                                    "CG2005_AMPLIADO_ANDA/11.Bogot\xa0.zip"),
                         list = TRUE)


bogota_csv = unz(description = bogota, filename = "CG2005_AMPLIADO_AN_11.CSV.zip")


unzip(unz(description = census_zip, filename = "11.Bogot\xa0.zip"),
      exdir = here::here("data", "downloads", "Bogota", "census"),
      unzip = "unzip")



lst <- archive::archive(census_zip)

print(lst$path)

# ============================================================================================
# III: Save  data
# ============================================================================================
# Save the raw data of ground stations
save_raw_data_tidy_formatted(
  data          = bogota_stations_data,
  out_dir       = here::here(bogota_cfg$out_dir, "pollution_ground_stations", "Bogota"),
  out_name      = "bogota_stations_2000_2023",   # or leave NULL to auto-infer
  write_rds     = TRUE,
  write_parquet = TRUE,
  write_csv_gz  = FALSE
)
