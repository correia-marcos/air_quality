# Small local sources for the MERRA-2 extraction and station-joining recipes.
write_temporal_fixture <- function(root, exact_values = FALSE) {
  raw <- file.path(root, "raw")
  dir.create(file.path(raw, "merra2_aerosol_products"), recursive = TRUE,
             showWarnings = FALSE)
  fields <- c("DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS")
  file <- file.path(raw, "merra2_aerosol_products", "toy.20230101.nc4")
  raster <- terra::rast(nrows = 2, ncols = 2, nlyrs = 120,
    xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  names(raster) <- unlist(lapply(fields, function(name) paste0(name, "_", 1:24)))
  # Four cells have mean 2.5; the hour and species offsets are known exactly.
  terra::values(raster) <- do.call(cbind, lapply(seq_along(fields), function(i) {
    cells <- outer(1:4, 0:23, "+")
    if (exact_values) (cells + i) * 2^-30 else cells * 1e-9 + i * 1e-9
  }))
  # A named GeoTIFF tests extraction and filename dates without a netCDF-writing dependency.
  # The MERRA-style suffix does not make this fixture a netCDF import test.
  terra::writeRaster(raster, file, filetype = "GTiff", datatype = "FLT8S", overwrite = TRUE)
  square <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0),
                                        ncol = 2, byrow = TRUE)))
  geo <- sf::st_sf(id = 1L, geometry = sf::st_sfc(square, crs = 4326))
  for (city in c("Bogota_metro", "Mexico_city", "Santiago", "Sao_Paulo")) {
    folder <- file.path(raw, "cities_shapefiles", city)
    dir.create(folder, recursive = TRUE)
    sf::st_write(geo, file.path(folder, "boundary.shp"), quiet = TRUE)
  }
  folder <- file.path(raw, "cities_shapefiles", "Sao_Paulo_metro_stations")
  dir.create(folder, recursive = TRUE)
  stations_geo <- sf::st_as_sf(data.frame(sttn_cd = c(1L, 2L), x = c(0.5, 1.5), y = 1),
                               coords = c("x", "y"), crs = 4326)
  sf::st_write(stations_geo, file.path(folder, "stations.shp"), quiet = TRUE)
  stations <- data.frame(datetime = as.POSIXct(paste("2023-01-01",
    c("00:00:00", "00:00:00", "01:00:00", "02:00:00", "00:00:00")), tz = "UTC"),
    pm25 = c(10, 30, NA, 40, 100), station_code = c(1L, 2L, 1L, 1L, 9L))
  cities <- c("Bogota", "Mexico_city", "Santiago", "Sao_paulo")
  names <- c("pollution_pm10_pm25_data_balanced_2023.rds",
    "pollution_pm25_data_balanced_2023.rds", "pollution_data_balanced_2023_pm25.rds",
    "pollution_data_balanced_2023_pm25.rds")
  for (i in seq_along(cities)) {
    folder <- file.path(raw, "pollution_ground_stations", cities[i])
    dir.create(folder, recursive = TRUE)
    data <- stations
    if (cities[i] == "Santiago") {
      data$date2_hour <- data$datetime
      data$pm25_validated <- data$pm25
    }
    saveRDS(data, file.path(folder, names[i]))
  }
  list(raw = raw, nc_file = file, geography = geo, stations = stations)
}
