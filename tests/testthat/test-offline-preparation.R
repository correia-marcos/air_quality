# Load city builders without acquisition/config-loader side effects.
offline_city_builders <- function() {
  env <- new.env(parent = asNamespace("dplyr"))
  for (file in c("registry", "bogota", "cdmx", "santiago", "sao_paulo")) {
    source(here::here("src", "city_specific", paste0(file, ".R")), local = env)
  }
  source(here::here("src", "general_utilities", "reproducibility.R"), local = env)
  env
}

# Construct small projected rectangles so boundary membership is unambiguous.
offline_rectangle <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(rbind(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
                            c(xmin, ymax), c(xmin, ymin))))
}

test_that("all geographic builders require preserved sources before acquisition", {
  env <- offline_city_builders()
  old_locale <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old_locale), add = TRUE)
  work <- tempfile("missing-geography-")
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  functions <- c("bogota_download_metro_area", "cdmx_download_metro_area",
    "santiago_download_metro_area_2024", "santiago_download_metro_area_2017",
    "sao_paulo_download_metro_area", "sao_paulo_download_weighting_areas")
  for (name in functions) {
    args <- list(download_dir = file.path(work, name),
      out_file = file.path(work, paste0(name, ".gpkg")),
      allow_download = FALSE, quiet = TRUE)
    if (name == "sao_paulo_download_weighting_areas") args$keep_municipality <- 3550308
    expect_error(do.call(env[[name]], args), "Missing preserved source inputs",
                 info = name)
    expect_false(dir.exists(args$download_dir), info = name)
    expect_false(file.exists(args$out_file), info = name)
  }
})

test_that("Santiago local responses preserve representative-point membership and keys", {
  env <- offline_city_builders()
  work <- tempfile("santiago-source-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  metro <- sf::st_sf(CONURB = "GRAN SANTIAGO", geometry = sf::st_sfc(
    offline_rectangle(350000, 6300000, 351000, 6301000), crs = 32719))
  # Zone 2 intersects the metro but its representative point lies outside it.
  zones <- sf::st_sf(CUT = c(13101, 13102, 13103), COD_DISTRI = c(2, 3, 4),
    COD_ZONA = c(7, 8, 9), d_COMUNA = c("inside", "sliver", "outside"),
    geometry = sf::st_sfc(
      offline_rectangle(350100, 6300100, 350200, 6300200),
      offline_rectangle(350900, 6300100, 351300, 6300300),
      offline_rectangle(352000, 6300100, 352100, 6300200), crs = 32719))
  metro_path <- file.path(work, "GRAN_SANTIAGO_13_metro.geojson")
  zones_path <- file.path(work, "GRAN_SANTIAGO_13_zonas.geojson")
  count_path <- file.path(work, "GRAN_SANTIAGO_13_count.json")
  sf::st_write(sf::st_transform(metro, 4326), metro_path, quiet = TRUE)
  sf::st_write(sf::st_transform(zones, 4326), zones_path, quiet = TRUE)
  writeLines('{"count":3}', count_path)
  sources <- c(metro_path, zones_path, count_path)
  before <- tools::md5sum(sources)
  output <- file.path(work, "derived", "zones.gpkg")
  result <- env$santiago_download_metro_area_2017(download_dir = work,
    out_file = output, allow_download = FALSE, quiet = TRUE)
  expect_identical(result$zona_id, "13101021007")
  expect_identical(result$d_COMUNA, "inside")
  expect_equal(sf::st_crs(result)$epsg, 4326)
  original <- sf::st_read(zones_path, quiet = TRUE)[1, ]
  expect_identical(sf::st_as_binary(sf::st_geometry(result)),
                   sf::st_as_binary(sf::st_geometry(original)))
  expect_equal(tools::md5sum(sources), before)
  expect_equal(sf::st_read(output, quiet = TRUE)$zona_id, result$zona_id)

  writeLines('{"count":4}', count_path)
  expect_error(env$santiago_download_metro_area_2017(download_dir = work,
    out_file = output, allow_download = FALSE, quiet = TRUE), "query was truncated")
  writeLines('{}', count_path)
  expect_error(env$santiago_download_metro_area_2017(download_dir = work,
    out_file = output, allow_download = FALSE, quiet = TRUE), "Missing zone count")
})

test_that("São Paulo local weighting source filters codes without changing geometry", {
  env <- offline_city_builders()
  work <- tempfile("sp-source-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  source_sf <- sf::st_sf(code_muni = c(3550308, 3550308, 3509502),
    code_weighting = c("3550308001", "3550308002", "3509502001"),
    geometry = sf::st_sfc(
      offline_rectangle(0, 0, 1, 1), offline_rectangle(1, 0, 2, 1),
      offline_rectangle(2, 0, 3, 1), crs = 4674))
  source_path <- file.path(work, "sp_weighting_areas_2010.rds")
  saveRDS(source_sf, source_path)
  before <- tools::md5sum(source_path)
  output <- file.path(work, "derived", "weights.gpkg")
  result <- env$sao_paulo_download_weighting_areas(keep_municipality = "3550308",
    download_dir = work, out_file = output, allow_download = FALSE, quiet = TRUE)
  expect_identical(result$code_muni, c("3550308", "3550308"))
  expect_identical(result$code_weighting, source_sf$code_weighting[1:2])
  expect_identical(sf::st_geometry(result), sf::st_geometry(source_sf[1:2, ]))
  expect_equal(sf::st_crs(result)$epsg, 4674)
  expect_equal(tools::md5sum(source_path), before)
  expect_error(env$sao_paulo_download_weighting_areas(keep_municipality = "9999999",
    download_dir = work, out_file = output, allow_download = FALSE, quiet = TRUE),
    "No data matched")
})

test_that("archived Bogotá and CDMX municipality layers rebuild with unchanged codes", {
  env <- offline_city_builders()
  old_locale <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old_locale), add = TRUE)
  work <- tempfile("archive-geography-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  geometry <- sf::st_sfc(offline_rectangle(0, 0, 1, 1),
                         offline_rectangle(2, 0, 3, 1), crs = 4326)
  cases <- list(
    bogota = list(file = "MPIO_fixture.shp", zip = "SHP_MGN2018_INTGRD_MPIO.zip",
      data = sf::st_sf(MPIO_CDPMP = c("11001", "25001"), geometry = geometry)),
    cdmx = list(file = "00mun.shp", zip = "mg_2024_integrado.zip",
      data = sf::st_sf(CVEGEO = c("09002", "15001"), CVE_ENT = c("09", "15"),
        CVE_MUN = c("002", "001"), NOMGEO = c("selected", "excluded"),
        geometry = geometry)))
  for (city in names(cases)) {
    case <- cases[[city]]
    sources <- file.path(work, city)
    parts <- file.path(sources, "conjunto_de_datos")
    dir.create(parts, recursive = TRUE)
    sf::st_write(case$data, file.path(parts, case$file), quiet = TRUE)
    zip_path <- file.path(sources, case$zip)
    zip::zipr(zip_path, files = "conjunto_de_datos", root = sources)
    before <- tools::md5sum(zip_path)
    output <- file.path(work, "derived", paste0(city, ".gpkg"))
    args <- list(download_dir = sources, out_file = output,
                 allow_download = FALSE, quiet = TRUE)
    if (city == "bogota") {
      args$municipality_codes <- "11001"
      result <- do.call(env$bogota_download_metro_area, args)
      expect_identical(result$MPIO_FULL, "11001")
    } else {
      args$keep_municipality <- 9002
      result <- do.call(env$cdmx_download_metro_area, args)
      expect_identical(result$CVE_MUN, "09002")
      expect_identical(result$MUN, "002")
    }
    expect_equal(nrow(result), 1L)
    expect_equal(sf::st_crs(result)$epsg, 4326)
    expect_equal(tools::md5sum(zip_path), before)
    expect_true(file.exists(output))
  }
})
