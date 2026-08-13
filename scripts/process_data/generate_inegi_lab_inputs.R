# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Build the two auxiliary inputs sent to INEGI's remote lab (folio LM 2786) together
# with scripts/process_data/LM2786CPV2020_2026-07-14_script.R.
#
# @Description: Produces (1) LM2786_insumo_contaminacion_horaria.csv — the outlier-cleaned
#   hourly PM data for CDMX 2023, with residual sensor sentinel values (negative or >= 9999)
#   masked to NA; and (2) LM2786_insumo_distancias_ageb_estacion.csv — distances from every
#   urban AGEB representative point in the metro municipalities to every station, computed
#   with compute_distance_matrices() (same AEQD projection as the whole project). Requires
#   the Marco Geoestadistico urban AGEB shapefiles (see section I) downloaded beforehand.
#
# @Summary:
#   I.   Setup: load dependencies, city config, and check required input files.
#   II.  Pollution: export cleaned hourly PM 2023 to CSV, masking sentinel values.
#   III. Distances: AGEB-to-station matrix at AGEB level, exported to CSV.
#
# @Date: July 2026
# @Author: Marcos
# ============================================================================================

# Get all libraries and functions
source(here::here("src", "general_utilities", "config_utils_process_data.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "cdmx.R"))

# ============================================================================================
# I: Setup and input checks
# ============================================================================================
# Cleaned pollution data (outliers already masked) and station locations.
arrow_cdmx_2023 <- here::here("data", "processed", "monitoring_stations_outliers",
                              "cdmx_metro_clean", "year=2023")
gpkg_stations   <- here::here("data", "raw", "geospatial_data", "cdmx",
                              "cdmx_stations_buffer_metro.gpkg")

# Urban AGEB polygons from the "Marco Geoestadistico, Censo de Poblacion y Vivienda
# 2020" (INEGI, December 2020 edition — the vintage the census microdata keys use).
# Download the state packages for CDMX (09), Hidalgo (13), and Mexico (15) and place
# each state's `XXa.shp` (with .dbf/.shx/.prj) in the folder below before running.
dir_mg    <- here::here("data", "downloads", "cdmx", "marco_geoestadistico_2020")
shp_agebs <- file.path(dir_mg, c("09a.shp", "13a.shp", "15a.shp"))

# Everything this script produces is a deliverable for INEGI, kept in one folder.
outdir_lab <- here::here("data", "processed", "inegi_remote_lab")
dir.create(outdir_lab, recursive = TRUE, showWarnings = FALSE)

# Fail early with a clear message if the AGEB layer has not been downloaded yet.
missing_shp <- shp_agebs[!file.exists(shp_agebs)]
if (length(missing_shp) > 0) {
  stop("Missing Marco Geoestadistico AGEB shapefile(s):\n  ",
       paste(missing_shp, collapse = "\n  "))
}

# ============================================================================================
# II: Pollution — cleaned hourly PM data, 2023
# ============================================================================================
# Read only the columns the lab script consumes; the year=2023 folder is one file.
poll <- arrow::open_dataset(arrow_cdmx_2023) |>
  dplyr::select(station, datetime, pm25, pm10) |>
  dplyr::collect() |>
  data.table::as.data.table()

# Residual sensor codes survive outlier detection at three stations (CALPULALPAN
# reaches 79999 with 671 negative hours). They sit outside every 5 km buffer at the
# MUNICIPALITY level, so published results are unaffected — but at AGEB level they
# would contaminate exposure, so negative and >= 9999 readings are masked here.
for (pol in c("pm25", "pm10")) {
  bad <- !is.na(poll[[pol]]) & (poll[[pol]] < 0 | poll[[pol]] >= 9999)
  if (any(bad)) {
    cat("Masking", sum(bad), "sentinel", pol, "hour(s) at:",
        paste(unique(poll$station[bad]), collapse = ", "), "\n")
    data.table::set(poll, i = which(bad), j = pol, value = NA_real_)
  }
}

# Fix the datetime text format the lab script documents (UTC, no timezone suffix).
poll[, datetime := format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")]

data.table::fwrite(poll, file.path(outdir_lab,
                                   "LM2786_insumo_contaminacion_horaria.csv"))
cat("Pollution input:", nrow(poll), "station-hours,",
    data.table::uniqueN(poll$station), "stations.\n")

# ============================================================================================
# III: Distances — AGEB representative points to stations
# ============================================================================================
# Stack the three state AGEB layers and keep the metro-area municipalities only.
# agebs <- do.call(rbind, lapply(shp_agebs, sf::st_read, quiet = TRUE))
# agebs$cve_mun <- as.integer(agebs$CVE_ENT) * 1000L + as.integer(agebs$CVE_MUN)
# agebs_metro <- agebs[agebs$cve_mun %in% cdmx_cfg$cities_in_metro, ]
agebs_metro <- st_read(here::here("data", "raw", "geospatial_data", "cdmx",
                                  "cdmx_area_metro_2024.gpkg"))
# The 13-char CVEGEO (ENT+MUN+LOC+AGEB) is the join key the lab script rebuilds
# from the microdata; stop if the layer does not carry it in that exact shape.
stopifnot(all(nchar(agebs_metro$CVEGEO) == 13),
          !anyDuplicated(agebs_metro$CVEGEO))
cat("Urban AGEBs in metro area:", nrow(agebs_metro), "\n")

# Same machinery and settings as generate_distance_matrices.R, at AGEB level.
station_sf <- sf::st_read(gpkg_stations, quiet = TRUE)
ageb_distances <- compute_distance_matrices(
  stations_sf          = station_sf,
  station_id_col       = "station",
  geo_sf               = agebs_metro,
  geo_id_col           = "CVEGEO",
  distance_metric      = "aeqd",
  representative_point = "point_on_surface",
  out_dir              = here::here("data", "processed", "distances_matrices",
                                    "cdmx_2020_ageb"),
  out_name             = "matrix_ageb"
)

# Export the geo-station matrix as the CSV the lab script reads; keep all pairs
# (the lab script applies the 3/5 km buffers itself).
dist_dt <- ageb_distances$geo_station_matrix
data.table::fwrite(dist_dt, file.path(outdir_lab,
                                      "LM2786_insumo_distancias_ageb_estacion.csv"))

# Quick coverage diagnostic: AGEBs with at least one station within 5 km.
n_cov <- data.table::uniqueN(dist_dt[distance_km > 0 & distance_km <= 5, geo_id])
cat("AGEB-station pairs:", nrow(dist_dt),
    "| AGEBs with a station within 5 km:", n_cov, "\n")

# Print a success message for when running inside Docker Container
cat("Script from the IDB project executed successfully in the Docker container!\n")
