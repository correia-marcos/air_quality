# Run the production CDMX spatial commands on a tiny preserved-source fixture.
test_that("city spatial objects and saved checkpoints have separate owners", {
  work <- tempfile("city-spatial-targets-", tmpdir = here::here("tests", "_cache"))
  dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  sources <- file.path(work, "sources")
  parts <- file.path(sources, "metro_area", "conjunto_de_datos")
  locations_dir <- file.path(sources, "ground_stations_geolocation")
  dir.create(parts, recursive = TRUE)
  dir.create(locations_dir, recursive = TRUE)
  polygon <- sf::st_polygon(list(matrix(c(0, 0, .01, 0, .01, .01, 0, .01, 0, 0),
                                        ncol = 2, byrow = TRUE)))
  geometry <- sf::st_sfc(polygon, crs = 4326)
  municipalities <- sf::st_sf(CVEGEO = "09002", CVE_ENT = "09", CVE_MUN = "002",
                              NOMGEO = "fixture", geometry = geometry)
  ageb <- sf::st_sf(CVEGEO = "0900200010010", CVE_ENT = "09", CVE_MUN = "002",
    CVE_LOC = "0001", CVE_AGEB = "0010", AMBITO = "Urbano", geometry = geometry)
  sf::st_write(municipalities, file.path(parts, "00mun.shp"), quiet = TRUE)
  sf::st_write(ageb, file.path(parts, "00a.shp"), quiet = TRUE)
  zip::zipr(file.path(sources, "metro_area", "mg_2024_integrado.zip"),
    files = "conjunto_de_datos", root = dirname(parts))
  station_file <- file.path(locations_dir, "all_station_location.csv")
  locations <- data.frame(station = c("Mguel Hidalgo", "second", "outside"),
                          lon = c(.005, .05, .3), lat = .005)
  write.csv(locations, station_file, row.names = FALSE)

  manifest <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  nodes <- c("cdmx_geography_inputs", "cdmx_municipalities_sf", "cdmx_metro_area_sf",
    "cdmx_geography", "cdmx_stations_filter_inputs", "cdmx_station_locations",
    "cdmx_stations_sf", "cdmx_stations_filter")
  script <- file.path(work, "pipeline.R")
  store <- file.path(work, "store")
  output <- file.path(work, "outputs")
  write_pipeline <- function(radius = 20, changed_writer = FALSE) {
    cfg <- list(dl_dir = sources, out_dir = output, cities_in_metro = 9002,
      base_url_shp = "unused offline", station_buffer_km = radius,
      station_nme_map = c("Mguel Hidalgo" = "Miguel Hidalgo"))
    declarations <- paste0("targets::tar_target(cdmx_config, ",
      paste(deparse(cfg), collapse = "\n"), ")")
    for (name in nodes) {
      row <- manifest[manifest$name == name, ]
      declarations <- c(declarations, paste0("targets::tar_target_raw('", name,
        "', quote(", row$command, "), format = '", row$format,
        "', packages = 'dplyr')"))
    }
    files <- c("src/general_utilities/base_utils.R",
      "src/general_utilities/reproducibility.R", "src/city_specific/registry.R",
      "src/city_specific/cdmx.R", "src/general_utilities/process/spatial_files.R")
    definitions <- paste0("source(", encodeString(here::here(files), quote = '"'),
                           ", local = TRUE)")
    if (changed_writer) definitions <- c(definitions,
      "body(write_geopackage) <- as.call(list(as.name('{'),",
      "  quote(invisible(NULL)), body(write_geopackage)))")
    writeLines(c(definitions, "list(", paste(declarations, collapse = ",\n"), ")"),
               script)
  }
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  outdated <- function() targets::tar_outdated(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    result <- targets::tar_meta(fields = c("name", "time"), store = store)
    result <- result[result$name %in% nodes, ]
    result[order(result$name), ]
  }
  write_pipeline()
  targets::tar_make(names = cdmx_stations_sf, script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  expect_false(dir.exists(output))
  selected <- targets::tar_read_raw("cdmx_stations_sf", store = store)
  expect_identical(selected$station, c("Miguel Hidalgo", "second"))
  make()
  expect_length(outdated(), 0L)
  before <- metadata()
  make()
  expect_identical(metadata(), before)
  for (owner in c("cdmx_geography", "cdmx_stations_filter")) {
    files <- targets::tar_read_raw(owner, store = store)
    for (file in files) {
      before <- metadata()
      unlink(file)
      expect_identical(outdated(), owner)
      make()
      expect_true(file.exists(file))
      after <- metadata()
      expect_identical(before[before$name != owner, ], after[after$name != owner, ])
    }
  }
  write_pipeline(radius = 1)
  make()
  selected <- targets::tar_read_raw("cdmx_stations_sf", store = store)
  expect_identical(selected$station, "Miguel Hidalgo")
  write_pipeline(radius = 1, changed_writer = TRUE)
  expect_setequal(outdated(), c("cdmx_geography", "cdmx_stations_filter"))
  make()
  locations$station[1] <- "renamed"
  write.csv(locations, station_file, row.names = FALSE)
  before <- metadata()
  make()
  after <- metadata()
  spatial <- c("cdmx_municipalities_sf", "cdmx_metro_area_sf", "cdmx_geography")
  expect_identical(before[before$name %in% spatial, ], after[after$name %in% spatial, ])
  selected <- targets::tar_read_raw("cdmx_stations_sf", store = store)
  expect_identical(selected$station, "renamed")

  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/spatial_files.R"), e)
  path <- file.path(work, "writer", "stations.gpkg")
  expect_identical(e$write_geopackage(selected, path), path)
  restored <- sf::st_read(path, quiet = TRUE)
  expect_equal(sf::st_drop_geometry(restored), sf::st_drop_geometry(selected))
  expect_identical(sf::st_as_binary(sf::st_geometry(restored)),
                   sf::st_as_binary(sf::st_geometry(selected)))
  expect_true(sf::st_crs(restored) == sf::st_crs(selected))
  hash <- tools::md5sum(path)
  expect_error(e$write_geopackage(selected, path, overwrite = FALSE), "already exists")
  expect_identical(tools::md5sum(path), hash)
})
