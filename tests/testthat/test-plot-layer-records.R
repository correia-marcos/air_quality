# Plot-layer records: what write_plot_layers() keeps of a figure, and the rule that every
# plot a figure writer saves is recorded and registered for comparison.

test_that("plot layers record the drawn data whatever the input row order", {
  source(here::here("src/general_utilities/reproducibility.R"), local = TRUE)
  root <- tempfile("plot-layers-"); dir.create(root)
  old_theme <- ggplot2::theme_get()
  on.exit({
    ggplot2::theme_set(old_theme)
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  # A line with points, a weighted density, a continuous fill and a map, nested as the
  # plot targets nest them.
  lines <- data.frame(x = c(3, 1, 2, 2), y = c(30, 10, 20, 25),
                      city = c("b", "a", "a", "b"))
  density <- data.frame(v = c(1, 2, 2, 3, 5), w = c(1, 2, 1, 1, 3))
  tiles <- data.frame(x = 1:3, y = 1, value = c(0.1, 0.5, 0.9))
  square <- function(x0) {
    sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
  }
  shapes <- sf::st_sf(quintile = factor(c("1", "2")),
                      geometry = sf::st_sfc(square(0), square(2), crs = 32719))
  build <- function(lines, shapes) {
    list(
      lines = ggplot2::ggplot(lines, ggplot2::aes(x, y, colour = city)) +
        ggplot2::geom_line() + ggplot2::geom_point(),
      panels = list(
        density = ggplot2::ggplot(density, ggplot2::aes(v, weight = w)) +
          ggplot2::geom_density(),
        tiles = ggplot2::ggplot(tiles, ggplot2::aes(x, y, fill = value)) +
          ggplot2::geom_tile()),
      map = ggplot2::ggplot(shapes) + ggplot2::geom_sf(ggplot2::aes(fill = quintile)))
  }
  keys <- c("plot", "layer", "row")

  # Record the layers under a theme with a larger base size, as the paper theme sets.
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 14))
  file <- write_plot_layers(build(lines, shapes), file.path(root, "layers.parquet"))
  recorded <- as.data.frame(arrow::read_parquet(file))
  expect_setequal(unique(recorded$plot),
                  c("lines", "panels/density", "panels/tiles", "map"))
  expect_equal(anyDuplicated(recorded[keys]), 0L)
  expect_false(any(vapply(recorded, function(v) is.list(v) || is.object(v), logical(1))))

  # Geometry becomes a hash and a bounding box per feature; the discrete fill stays.
  map <- recorded[recorded$plot == "map", ]
  expect_false("geometry" %in% names(recorded))
  expect_length(unique(map$geometry_sha256), 2L)
  expect_equal(sort(map$geometry_xmax), c(1, 3))
  expect_false(anyNA(map$fill))

  # A continuous fill stays as hex text, one colour per tile.
  expect_length(unique(recorded$fill[recorded$plot == "panels/tiles"]), 3L)

  # Default sizes follow the active theme, so records must be built under the paper's.
  ggplot2::theme_set(ggplot2::theme_grey())
  grey <- as.data.frame(arrow::read_parquet(write_plot_layers(build(lines, shapes),
    file.path(root, "grey.parquet"))))
  expect_false(identical(grey$size[grey$geom == "GeomPoint"],
                         recorded$size[recorded$geom == "GeomPoint"]))

  # Shuffled input rows give the same record; a changed value fails the comparison.
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 14))
  shuffled <- write_plot_layers(build(lines[4:1, ], shapes[2:1, ]),
                                file.path(root, "shuffled.parquet"))
  expect_true(all(compare_registered_artifact(shuffled, file, keys)$max_abs_diff == 0))
  lines$y[1] <- 31
  changed <- write_plot_layers(build(lines, shapes), file.path(root, "changed.parquet"))
  expect_error(compare_registered_artifact(changed, file, keys), "y \\(scaled")
})

test_that("every plot that a figure writer saves is recorded and registered", {
  manifest <- targets::tar_manifest(fields = c("name", "command"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  # Plot targets end in _plots or _map; writers pass them to the saving functions.
  writers <- manifest$command[grepl("save_plot_pdf|save_exposure_plot_family|ggsave",
                                    manifest$command)]
  saved <- unique(unlist(regmatches(writers,
    gregexpr("[a-z0-9_]+_(plots|map)\\b", writers, perl = TRUE))))
  saved <- intersect(saved, manifest$name)
  recorded <- manifest$command[manifest$name == "plot_layer_records"]
  expect_gt(length(saved), 30L)
  for (name in saved) expect_match(recorded, paste0("\\b", name, "\\b"), perl = TRUE)

  registry <- read.csv(here::here("config", "verification_comparisons.csv"))
  expect_true(all(sprintf("data/processed/plot_layers/%s.parquet", saved) %in%
                  registry$actual_path))
})
