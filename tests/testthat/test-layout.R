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

test_that("only the specification-sensitivity pilot adds result subfolders", {
  # Pilot of the subfolder layout; any other subfolder is unreviewed drift.
  pilot <- "specification_sensitivity"
  roles <- c("design_a", "robustness_designs", "review", "panel", "reference_bogota")
  for (root in here::here("results", c("tables", "figures/diagnostics"))) {
    if (!dir.exists(root)) next
    subdirs <- list.dirs(root, full.names = FALSE, recursive = FALSE)
    expect_true(all(subdirs %in% pilot), info = root)
    if (dir.exists(file.path(root, pilot))) {
      expect_true(all(list.dirs(file.path(root, pilot), full.names = FALSE,
                                recursive = FALSE) %in% roles), info = root)
    }
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
