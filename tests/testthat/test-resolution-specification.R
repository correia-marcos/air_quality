# The declared A/B/C variants map to the existing folders and prefixes, and invalid
# combinations are refused before any computation.
declared <- local({
  e <- new.env()
  sys.source(here::here("config", "analysis_settings.R"), envir = e)
  e
})

test_that("declared variants keep the saved folders and file prefixes", {
  specs <- lapply(names(declared$resolution_specifications), resolution_specification,
                  specifications = declared$resolution_specifications,
                  cities = declared$resolution_cities)
  names(specs) <- names(declared$resolution_specifications)
  root <- here::here("data", "processed", "resolution_sensitivity")
  expected <- c(quintile_3km = root, quintile_20km = file.path(root, "buffer_20km"),
    quintile_split_3km = file.path(root, "quintile_split"),
    quintile_split_20km = file.path(root, "quintile_split", "buffer_20km"),
    education_group3_3km = file.path(root, "education_group3"),
    education_group3_20km = file.path(root, "education_group3", "buffer_20km"),
    education_level_3km = file.path(root, "education_level"),
    education_level_20km = file.path(root, "education_level", "buffer_20km"))
  expect_identical(vapply(specs, `[[`, "", "processed_root"), expected)
  expect_identical(unname(vapply(specs, `[[`, "", "prefix")),
    c("resolution_multicity_", "resolution_multicity_20km_",
      "resolution_multicity_quintile_split_", "resolution_multicity_quintile_split_20km_",
      "resolution_multicity_education_group3_",
      "resolution_multicity_education_group3_20km_",
      "resolution_multicity_education_level_",
      "resolution_multicity_education_level_20km_"))
  expect_identical(names(which(vapply(specs, `[[`, TRUE, "reference_anchor"))),
                   "quintile_3km")
  # Every grouping reuses the quintile B exposure of its own buffer.
  expect_identical(specs$education_group3_20km$exposure_root, expected[["quintile_20km"]])
  expect_identical(specs$quintile_split_3km$prepared_root, root)
  expect_true(all(declared$resolution_figure_specifications %in% names(specs)))
})

test_that("invalid or misordered variants are refused", {
  cities <- "bogota_2018"
  expect_error(resolution_specification("x", list(), cities), "Undeclared")
  expect_error(resolution_specification("x",
    list(x = list(buffer_km = 3, grouping = "income")), cities), "Unknown grouping")
  expect_error(resolution_specification("x",
    list(x = list(buffer_km = -1, grouping = "edu_quintile")), cities), "positive")
  expect_error(resolution_specification("x",
    list(x = list(buffer_km = 3, grouping = "edu_quintile", city = "a")), cities),
    "exactly")
  # A grouping before its buffer's quintile run cannot reuse that run's B exposure.
  misordered <- list(levels_5 = list(buffer_km = 5, grouping = "edu_level"),
                     quintile_5 = list(buffer_km = 5, grouping = "edu_quintile"))
  expect_error(resolution_specification("levels_5", misordered, cities), "first")
  expect_silent(resolution_specification("levels_5", rev(misordered), cities))
})

test_that("an exploration root redirects outputs but not the frozen inputs", {
  run <- file.path(tempdir(), "exploration-run")
  s <- resolution_specification("quintile_3km", declared$resolution_specifications,
                                declared$resolution_cities, root = run,
                                input_root = here::here())
  expect_true(startsWith(s$processed_root, run))
  expect_true(startsWith(s$table_dir, run))
  expect_identical(s$frozen_root,
                   here::here("data", "interim", "resolution_sensitivity"))
})

test_that("recipes read no command-line options", {
  scripts <- list.files(here::here("scripts"), pattern = "[.]R$", recursive = TRUE,
                        full.names = TRUE)
  launchers <- grepl("(^|/)run_[a-z_]+[.]R$|download_merra2_data[.]R$", scripts)
  readers <- scripts[vapply(scripts, function(f) {
    any(grepl("commandArgs", readLines(f, warn = FALSE), fixed = TRUE))
  }, logical(1))]
  expect_identical(setdiff(readers, scripts[launchers]), character())
})
