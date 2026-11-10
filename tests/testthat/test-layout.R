# The analysis a path belongs to: its first folder, or its first two when the first is
# robustness/ or not_in_paper/. Applies to the part after results/<type>/ or
# scripts/<stage>/.
analysis_of <- function(path, skip) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]][-seq_len(skip)]
  parts <- parts[-length(parts)]
  n <- if (parts[1] %in% c("robustness", "not_in_paper")) 2L else 1L
  paste(parts[seq_len(n)], collapse = "/")
}

# Every analysis folder under one results/<type>/ root.
result_analysis_dirs <- function(root) {
  tops <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  unlist(lapply(tops, function(top) {
    if (!top %in% c("robustness", "not_in_paper")) return(top)
    file.path(top, list.dirs(file.path(root, top), full.names = FALSE, recursive = FALSE))
  }))
}

test_that("each manuscript file sits in its producer's analysis folder", {
  manifest <- artifact_manifest(here::here("config/paper_artifacts.csv"))
  expect_true(all(file.exists(here::here(manifest$producer_script))))
  source_analysis   <- vapply(manifest$source_path, analysis_of, "", skip = 2L)
  producer_analysis <- vapply(manifest$producer_script, analysis_of, "", skip = 2L)
  expect_equal(unname(source_analysis), unname(producer_analysis))
})

test_that("result folders mirror the analysis folders of the recipes", {
  # The image deliberately has no prebuilt results; existing local outputs must follow layout.
  skip_if_not(dir.exists(here::here("results")))
  expect_setequal(list.files(here::here("results")), c("figures", "tables"))
  stages <- list.dirs(here::here("scripts"), recursive = FALSE)
  for (type in c("tables", "figures")) {
    root <- here::here("results", type)
    # Files live only inside analysis folders.
    loose <- setdiff(list.files(root), list.dirs(root, full.names = FALSE, recursive = FALSE))
    expect_length(loose, 0L)
    for (group in c("robustness", "not_in_paper")) {
      inside <- file.path(root, group)
      if (dir.exists(inside)) {
        expect_length(setdiff(list.files(inside),
                              list.dirs(inside, full.names = FALSE, recursive = FALSE)), 0L)
      }
    }
    for (analysis in result_analysis_dirs(root)) {
      expect_true(any(dir.exists(file.path(stages, analysis))),
                  info = file.path("results", type, analysis))
    }
  }
})

test_that("only the specification-sensitivity pilot adds subfolders by role", {
  roles <- c("design_a", "robustness_designs", "review", "panel", "station_sets")
  for (type in c("tables", "figures")) {
    root <- here::here("results", type)
    if (!dir.exists(root)) next
    for (analysis in result_analysis_dirs(root)) {
      subfolders <- list.dirs(file.path(root, analysis), full.names = FALSE,
                              recursive = FALSE)
      allowed <- if (analysis == "specification_sensitivity") roles else character()
      expect_true(all(subfolders %in% allowed), info = analysis)
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
