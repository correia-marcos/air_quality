test_that("distance groups preserve weighted totals, missingness and boundary membership", {
  e <- new.env(parent = globalenv())
  for (file in c("base_utils.R", "process/geo_ids.R", "process/diagnostics.R")) {
    sys.source(here::here("src/general_utilities", file), e)
  }
  root <- tempfile("distance-bands-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  census <- data.frame(geo_id = c("a", "a", "b", "c", "a", "a"),
    person_weight = c(2, 1, 6, 1, 0, NA), educ_years = c(10, NA, 20, 5, 999, 999),
    women = c(1, 0, 0, 1, 1, 1))
  dist <- data.frame(geo_id = c("a", "a", "b", "b", "c", "c"),
    distance_km = c(1, NA, 3, 4, 21, 22))
  area <- data.table::data.table(geo_id = c("a", "b", "c"), area_km2 = c(1, 4, 2))
  census_file <- file.path(root, "census.parquet")
  matrix_file <- file.path(root, "distances.parquet")
  arrow::write_parquet(census, census_file)
  arrow::write_parquet(dist, matrix_file)
  before <- list.files(root)
  result <- e$compute_distance_band_summary(matrix_file, census_file, area,
    city = "Toy", unit_label = "units", share_vars = c(Women = "women"),
    mean_vars = c(Schooling = "educ_years"), radii_km = c(1, 3))
  value <- function(group, label) result[band == group & statistic == label, value]

  expect_identical(names(result), c("city", "band", "statistic", "value", "value_label"))
  expect_equal(value("All", "Population"), 10)
  expect_equal(value("Within 1 km", "Population"), 3)
  expect_equal(value("Within 3 km", "Population"), 9)
  expect_equal(value("Within 1 km", "Women"), 2 / 3)
  expect_equal(value("Within 3 km", "Schooling"), (2 * 10 + 6 * 20) / 8)
  expect_equal(value("Within 3 km", "Total population density (pop/km2)"), 9 / 5)
  expect_equal(value("Within 3 km", "Average population density (pop/km2)"), (3 + 1.5) / 2)
  expect_identical(list.files(root), before)

  # A missing distance changes eligibility for radius groups, not the metro population.
  dist <- dist[dist$geo_id != "c", ]
  arrow::write_parquet(dist, matrix_file)
  unmatched <- e$compute_distance_band_summary(matrix_file, census_file, area,
    city = "Toy", unit_label = "units", share_vars = c(Women = "women"),
    mean_vars = c(Schooling = "educ_years"), radii_km = c(1, 3))
  expect_equal(unmatched, result)
})

test_that("distance-band targets isolate cities and track both saved summary formats", {
  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  root <- tempfile("band-targets-", tmpdir = cache); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "pipeline.R")
  store <- file.path(root, "store")
  destination <- file.path(root, "output")
  slugs <- c("bogota", "cdmx", "santiago", "sao_paulo")
  contexts <- c("bogota_2018", "cdmx_2020", "santiago_2017", "sao_paulo_2010")
  identifiers <- c("GEO_ID", "CVE_MUN", "zona_id", "code_weighting")
  geo_names <- c("bogota_area_metro_census_tracts_2018.gpkg",
    "cdmx_area_metro_municipalities_2024.gpkg", "gran_santiago_zonas_2017.gpkg",
    "sao_paulo_metro_2010_weighting_areas.gpkg")
  census_names <- c("census_2018_metro_individual.parquet",
    "census_metro_individual_2020.parquet", "census_individual_2017.parquet",
    "census_sp_individual_2010.parquet")
  census <- data.frame(geo_id = c("a", "b", "c"), person_weight = c(2, 3, 1),
    educ_years = c(10, 15, 20), age = c(35, 45, 55), raw_p09 = c(35, 45, 55),
    income = c(100, 200, 300))
  for (column in c("adult", "women", "employed", "no_education", "graduate_educ",
                    "hh_head_women", "indigena", "white", "black_pardo", "formal_emp",
                    "informal_emp")) census[[column]] <- c(0, 1, 0)
  distances <- data.frame(geo_id = c("a", "b", "c"), station = "toy",
                          distance_km = c(1, 3, 21))
  polygons <- lapply(0:2, function(i) {
    x <- 500000 + i * 2000
    sf::st_polygon(list(matrix(c(x, 0, x + 1000, 0, x + 1000, 1000,
                                 x, 1000, x, 0), ncol = 2, byrow = TRUE)))
  })
  geometry <- sf::st_sfc(polygons, crs = 32618)
  inputs <- character()
  quote_text <- function(x) encodeString(x, quote = '"')
  file_target <- function(name, path) {
    paste0("targets::tar_target_raw(", quote_text(name), ", quote(",
            quote_text(path), "), format = 'file')")
  }
  for (i in seq_along(slugs)) {
    dir <- file.path(root, "alternate-inputs", slugs[i])
    dir.create(dir, recursive = TRUE)
    geo <- sf::st_sf(geo_id = c("a", "b", "c"), geometry = geometry)
    names(geo)[1] <- identifiers[i]
    sf::st_write(geo, file.path(dir, geo_names[i]), quiet = TRUE)
    arrow::write_parquet(census, file.path(dir, census_names[i]))
    arrow::write_parquet(distances, file.path(dir, "matrix_geo_station_distances.parquet"))
    inputs <- c(inputs,
      file_target(paste0(slugs[i], "_geography"), file.path(dir, geo_names[i])),
      file_target(paste0(slugs[i], "_census"), file.path(dir, census_names[i])),
      file_target(paste0(contexts[i], "_distances"),
                  file.path(dir, "matrix_geo_station_distances.parquet")))
  }
  graph <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  names <- c(paste0(contexts, "_distance_geography"),
    paste0(slugs, "_distance_band_area"), paste0(slugs, "_distance_bands"),
    "distance_band_summary", "compute_distance_band_descriptives")
  declarations <- inputs
  for (name in names) {
    row <- graph[graph$name == name, ]
    command <- str2lang(row$command)
    if (name == "compute_distance_band_descriptives") command$out_dir <- destination
    declarations <- c(declarations, paste0("targets::tar_target_raw(", quote_text(name),
      ", quote(", paste(deparse(command), collapse = "\n"), "), format = ",
      quote_text(row$format), ", packages = 'data.table')"))
  }
  modules <- here::here("src/general_utilities",
    c("base_utils.R", "process/geo_ids.R", "process/diagnostics.R",
       "process/exposure_regressions.R"))
  write_pipeline <- function(radii = c(1, 3, 5, 10, 20)) {
    writeLines(c(paste0("source(", quote_text(modules), ")"),
      paste0("source(", quote_text(here::here("config/analysis_settings.R")), ")"),
      paste0("distance_band_radii_km <- c(", paste(radii, collapse = ","), ")"),
      "list(", paste(declarations, collapse = ",\n"), ")"), script)
  }
  make <- function() targets::tar_make(script = script, store = store,
    callr_function = NULL, reporter = "silent", envir = new.env(parent = globalenv()))
  metadata <- function() {
    x <- targets::tar_meta(fields = c("name", "time"), store = store)
    x[order(x$name), ]
  }
  write_pipeline()
  make()
  files <- targets::tar_read_raw("compute_distance_band_descriptives", store = store)
  expect_setequal(basename(files),
                  paste0("distance_band_descriptives.", c("parquet", "csv")))
  before <- metadata()
  make()
  expect_identical(metadata(), before)

  for (file in files) {
    unlink(file)
    make()
    expect_true(all(file.exists(files)))
    after <- metadata()
    summary_names <- c(paste0(slugs, "_distance_bands"), "distance_band_summary")
    expect_identical(after[after$name %in% summary_names, ],
                     before[before$name %in% summary_names, ])
  }

  census$person_weight[1] <- 7
  arrow::write_parquet(census, file.path(root, "alternate-inputs/bogota", census_names[1]))
  make()
  after <- metadata()
  expect_identical(after[after$name == "cdmx_distance_bands", ],
                   before[before$name == "cdmx_distance_bands", ])
  revised <- targets::tar_read_raw("bogota_distance_bands", store = store)
  expect_equal(revised[band == "Within 1 km" & statistic == "Population", value], 7)

  write_pipeline(radii = c(1, 3))
  make()
  result <- targets::tar_read_raw("distance_band_summary", store = store)
  expect_setequal(unique(result$band), c("All", "Within 1 km", "Within 3 km"))
  after_settings <- metadata()
  area_names <- paste0(slugs, "_distance_band_area")
  expect_identical(after_settings[after_settings$name %in% area_names, ],
                   after[after$name %in% area_names, ])
})
