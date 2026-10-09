# Small provider-shaped inputs for all five Bogotá preparation products.
bogota_geography_fixture <- function(work) {
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  rectangle <- function(left, right) {
    sf::st_polygon(list(rbind(c(left, 0), c(right, 0), c(right, 1),
                              c(left, 1), c(left, 0))))
  }
  geometry <- sf::st_sfc(rectangle(0, 1), rectangle(2, 3), rectangle(4, 5), crs = 4326)
  archive <- function(name, layers) {
    parts <- file.path(work, tools::file_path_sans_ext(name))
    dir.create(parts)
    for (layer in names(layers)) {
      sf::st_write(layers[[layer]], file.path(parts, paste0(layer, ".shp")), quiet = TRUE)
    }
    path <- file.path(work, name)
    zip::zipr(path, files = list.files(parts), root = parts)
    path
  }
  sources <- c(
    old = archive("SHP_MGN2005_COLOMBIA.zip", list(
      MGN_Municipio = sf::st_sf(DPTO_DPTO_ = c("11", "25", "25"),
        MPIO_CCDGO = c("001", "001", "002"), geometry = geometry),
      MGN_Manzana = sf::st_sf(SETR_CLSE_ = c("11", "25", "25"),
        SECR_SETR_ = c("001", "001", "002"), MANZ_CCNCT = paste0("urban", 1:3),
        geometry = geometry),
      MGN_Seccion_rural = sf::st_sf(SETR_CLSE_ = c("11", "25", "25"),
        SETR_CLSE1 = c("001", "001", "002"), SECR_CCNCT = paste0("rural", 1:3),
        geometry = geometry),
      MGN_CentroPoblado = sf::st_sf(CPOB_CCNCT = c("11001001", "25001001", "25002001"),
        geometry = geometry))),
    municipalities = archive("SHP_MGN2018_INTGRD_MPIO.zip", list(
      MPIO_fixture = sf::st_sf(MPIO_CDPMP = c("11001", "25001", "25002"),
        geometry = geometry))),
    urban = archive("SHP_MGN2018_INTGRD_MANZ.zip", list(
      MANZ_fixture = sf::st_sf(MPIO_CDPMP = c("11001", "25001", "25002"),
        COD_DANE_A = paste0("urban", 1:3), geometry = geometry))),
    rural = archive("SHP_MGN2018_INTGRD_SECCR.zip", list(
      SECCR_fixture = sf::st_sf(MPIO_CDPMP = c("11001", "25001", "25002"),
        SECR_CCNCT = paste0("rural", 1:3), geometry = geometry))))
  localities <- sf::st_sf(LocCodigo = c("01", "02"), LocNombre = c("west", "east"),
    geometry = sf::st_sfc(rectangle(0, 0.5), rectangle(0.5, 1), crs = 4326))
  localities_file <- file.path(work, "bogota_loca.gpkg")
  sf::st_write(localities, localities_file, quiet = TRUE)
  c(sources, localities = localities_file)
}

bogota_geography_functions <- function() {
  env <- new.env(parent = globalenv())
  for (path in c("src/city_specific/registry.R",
                 "src/general_utilities/reproducibility.R",
                 "src/general_utilities/process/spatial_files.R",
                 "src/city_specific/bogota.R")) {
    sys.source(here::here(path), env)
  }
  env
}

test_that("Bogotá preparation reads explicit sources without acquiring or saving", {
  env <- bogota_geography_functions()
  work <- tempfile("bogota-geography-")
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  sources <- bogota_geography_fixture(work)
  before <- tools::md5sum(sources)
  files_before <- list.files(work, recursive = TRUE)
  testthat::local_mocked_bindings(RETRY = function(...) stop("Network must not be used"),
                                .package = "httr")

  # Reusing acquired files must return paths without running preparation.
  acquired <- env$bogota_download_geography(download_dir = work, quiet = TRUE)
  expect_setequal(unname(acquired), unname(sources))

  cases <- list(
    metro_2005 = list(source_zips = sources["old"], level = "mpio_localidad",
                      mgn_year = 2005),
    municipalities_2005 = list(source_zips = sources["old"], level = "mpio",
                               mgn_year = 2005),
    tracts_2005 = list(source_zips = sources["old"], level = "manzana", mgn_year = 2005),
    metro_2018 = list(source_zips = sources["municipalities"], level = "mpio_localidad",
                      mgn_year = 2018),
    tracts_2018 = list(source_zips = sources[c("urban", "rural")], level = "manzana",
                       mgn_year = 2018))
  results <- lapply(cases, function(args) do.call(env$bogota_prepare_metro_area,
    c(args, list(municipality_codes = c("11001", "25001"),
                 localities_file = sources["localities"], quiet = TRUE))))
  expect_identical(vapply(results, nrow, integer(1)),
                   c(metro_2005 = 3L, municipalities_2005 = 2L, tracts_2005 = 6L,
                     metro_2018 = 3L, tracts_2018 = 4L))
  for (result in results) {
    expect_true(all(result$MPIO_FULL %in% c("11001", "25001")))
    expect_equal(sf::st_crs(result)$epsg, 4326)
  }
  expect_identical(results$metro_2018$GEO_ID, c("25001", "1100101", "1100102"))
  expect_identical(results$tracts_2018$GEO_ID, c("urban1", "urban2", "rural1", "rural2"))
  expect_identical(list.files(work, recursive = TRUE), files_before)
  expect_identical(tools::md5sum(sources), before)

  # A renamed archive in a different folder remains the actual input.
  alternate <- tempfile("different-source-", fileext = ".zip")
  on.exit(unlink(alternate), add = TRUE)
  file.copy(sources["municipalities"], alternate)
  renamed <- env$bogota_prepare_metro_area(source_zips = alternate, level = "mpio",
    municipality_codes = "25002", quiet = TRUE)
  expect_identical(renamed$MPIO_FULL, "25002")
  expect_error(env$bogota_prepare_metro_area(source_zips = alternate,
    level = "mpio_localidad"), "localities_file is required")
  expect_error(env$bogota_prepare_metro_area(source_zips = sources["urban"],
    level = "manzana"), "Expected 2 source archive")
  missing <- file.path(work, "missing.zip")
  expect_error(env$bogota_prepare_metro_area(source_zips = missing),
               "Missing preserved source inputs")
})
