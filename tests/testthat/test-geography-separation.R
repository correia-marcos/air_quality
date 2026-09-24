# Small archives exercise the layer choices that differ between geographic vintages.
test_that("Santiago 2024 preserves boundary definitions and commune dissolution", {
  env <- new.env(parent = globalenv())
  source(here::here("src/city_specific/registry.R"), local = env)
  source(here::here("src/city_specific/santiago.R"), local = env)
  source(here::here("src/general_utilities/reproducibility.R"), local = env)
  work <- tempfile("santiago-2024-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  rectangle <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0),
    c(x + 1, 1), c(x, 1), c(x, 0))))
  geometry <- sf::st_sfc(rectangle(0), rectangle(1), rectangle(3), crs = 32719)
  urban <- sf::st_sf(LOCALIDAD = c("ignore", "ignore", "GRAN SANTIAGO"),
    N_LOCALIDAD = c("GRAN SANTIAGO", "GRAN SANTIAGO", "OTHER"),
    CUT = c("13101", "13101", "13201"), ID_ENTIDAD = c("A", "B", "C"),
    geometry = geometry)
  districts <- sf::st_sf(COMUNA = c("SANTIAGO", "SANTIAGO", "OTHER"),
    CUT = urban$CUT, ID_DISTRITO = c("D1", "D2", "D3"), geometry = geometry)
  blocks <- sf::st_sf(ID_ENTIDAD = c("A", "B", "C"),
    ID_DISTRITO = c("D1", "D2", "D3"), geometry = geometry)
  gpkg <- file.path(work, "Cartografia_censo2024_Pais.gpkg")
  sf::st_write(urban, gpkg, layer = "Limite_Urbano_CPV24", quiet = TRUE)
  sf::st_write(districts, gpkg, layer = "Distrital_CPV24", quiet = TRUE)
  sf::st_write(blocks, gpkg, layer = "Manzanas_CPV24", quiet = TRUE)
  archive <- file.path(work, "Cartografia_censo2024_Pais.zip")
  zip::zipr(archive, basename(gpkg), root = work)
  before <- tools::md5sum(archive)
  files_before <- list.files(work)

  for (type in c("gran_santiago", "metro_santiago")) {
    result <- env$santiago_prepare_metro_area_2024(source_zip = archive,
      type = type, level = "mpio", keep_municipality = "Santiago",
      dissolve_by = "CUT", quiet = TRUE)
    expect_identical(result$CUT, "13101")
    expect_equal(as.numeric(sf::st_area(result)), 2)
    expect_true(all(sf::st_is_valid(result)))
    expect_identical(as.character(sf::st_geometry_type(result)), "MULTIPOLYGON")
    expect_equal(sf::st_crs(result)$epsg, 32719)
    result_blocks <- env$santiago_prepare_metro_area_2024(source_zip = archive,
      type = type, level = "manzana", keep_municipality = "Santiago", quiet = TRUE)
    expect_identical(result_blocks$ID_ENTIDAD, c("A", "B"))
  }
  expect_identical(tools::md5sum(archive), before)
  expect_identical(list.files(work), files_before)
  acquired <- env$santiago_download_geography_2024(download_dir = work, quiet = TRUE)
  expect_identical(acquired, archive)
})

test_that("São Paulo archive preparation preserves municipal and tract identifiers", {
  env <- new.env(parent = globalenv())
  source(here::here("src/city_specific/registry.R"), local = env)
  source(here::here("src/city_specific/sao_paulo.R"), local = env)
  source(here::here("src/general_utilities/reproducibility.R"), local = env)
  work <- tempfile("sp-geography-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  rectangle <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0),
    c(x + 1, 1), c(x, 1), c(x, 0))))
  geometry <- sf::st_sfc(rectangle(0), rectangle(2), crs = 4674)
  for (level in c("mpio", "setor_censitario")) {
    parts <- file.path(work, level)
    dir.create(parts)
    data <- if (level == "mpio") {
      sf::st_sf(CD_GEOCODM = c("3550308", "3509502"), geometry = geometry)
    } else {
      sf::st_sf(CD_GEOCODI = c("355030800000001", "350950200000001"),
        geometry = geometry)
    }
    sf::st_write(data, file.path(parts, "source.shp"), quiet = TRUE)
    preserved <- sf::st_read(file.path(parts, "source.shp"), quiet = TRUE)
    archive <- file.path(work, paste0(level, ".zip"))
    zip::zipr(archive, list.files(parts), root = parts)
    before <- tools::md5sum(archive)
    files_before <- list.files(work, recursive = TRUE)
    result <- env$sao_paulo_prepare_metro_area(source_zip = archive, level = level,
      keep_municipality = "3550308", quiet = TRUE)
    expect_identical(result$CD_GEOCODM, "3550308")
    if (level == "setor_censitario") {
      expect_identical(result$CD_GEOCODI, "355030800000001")
    }
    expect_equal(sf::st_crs(result)$epsg, 4674)
    expect_identical(sf::st_as_binary(sf::st_geometry(result)),
                     sf::st_as_binary(sf::st_geometry(preserved)[1]))
    expect_identical(tools::md5sum(archive), before)
    expect_identical(list.files(work, recursive = TRUE), files_before)
    expect_error(env$sao_paulo_prepare_metro_area(source_zip = archive, level = level,
      keep_municipality = "9999999", quiet = TRUE), "No data matched")
  }
})
