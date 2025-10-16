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


census_zip = here::here(bogota_cfg$dl_dir, "census", "CG2005_AMPLIADO.zip")
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



# ============================================================================================
# II: CHECK  data
# ============================================================================================
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# List files inside the ZIP
lst <- archive::archive(census_zip)
print(lst$path)

paths_norm <- stringi::stri_trans_general(lst$path, "Latin-ASCII")
print(paths_norm)

translates = stri_trans_list()
# print(translates)

out_dir <- here::here(bogota_cfg$dl_dir, "census", "unzipped")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

keep <- grepl("Bogot.*|Cundina.*", paths_norm, ignore.case = TRUE)
archive::archive_extract(census_zip, dir = out_dir)

any(!validUTF8(lst$path))

# If you want the same two entries by position:
needed <- paths_norm[c(12, 4)]


archive::archive_extract(census_zip, dir = out_dir, file = needed)


utils::unzip(zipfile   = census_zip,
             files     = needed,
             exdir     = out_dir,
             overwrite = TRUE,
             junkpaths = TRUE)

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
