test_that("manifest producers and result topics obey the shared contract", {
  manifest <- artifact_manifest(here::here("config/paper_artifacts.csv"))
  expect_true(all(file.exists(here::here(manifest$producer_script))))
  topics <- c("maps", "monitoring", "exposure", "imputation", "temporal", "satellite", "diagnostics")
  figures <- manifest$source_path[startsWith(manifest$source_path, "results/figures/")]
  expect_true(all(vapply(strsplit(figures, "/", fixed = TRUE), `[`, "", 3L) %in% topics))
  tables <- manifest$source_path[startsWith(manifest$source_path, "results/tables/")]
  expect_true(all(lengths(strsplit(tables, "/", fixed = TRUE)) == 3L))
  # The image deliberately has no prebuilt results; existing local outputs must follow layout.
  if (dir.exists(here::here("results"))) {
    expect_setequal(list.files(here::here("results"), all.files = FALSE), c("figures", "tables"))
    expect_true(all(list.files(here::here("results/figures")) %in% topics))
  }
})

test_that("all four city configurations send generated products to interim", {
  e <- new.env(parent = environment())
  sys.source(here::here("src/city_specific/registry.R"), envir = e)
  for (city in c("bogota", "cdmx", "santiago", "sao_paulo")) {
    sys.source(here::here("src/city_specific", paste0(city, ".R")), envir = e)
    expect_equal(get(paste0(city, "_cfg"), envir = e)$out_dir, here::here("data/interim"))
  }
})
