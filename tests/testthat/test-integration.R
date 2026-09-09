# Synthetic stations/census -> real distances -> IDW -> regression -> exhibit export.
test_that("small prepared inputs connect through the analytical and export stages", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  stations <- sf::st_as_sf(data.frame(station = c("s1", "s2"),
    x = c(0, .02), y = 0), coords = c("x", "y"), crs = 4326)
  geos <- sf::st_as_sf(data.frame(geo_id = paste0("g", 1:4),
    x = c(.005, .008, .012, .015), y = .001), coords = c("x", "y"), crs = 4326)
  geos <- sf::st_buffer(sf::st_transform(geos, 3857), 10) |> sf::st_transform(4326)
  dist <- compute_distance_matrices(stations, "station", geos, "geo_id",
    out_dir = root, out_name = "toy", distance_metric = "haversine", quiet = TRUE)
  expect_equal(nrow(dist$geo_station_matrix), 8L)
  expect_true(all(dist$geo_station_matrix$distance_km > 0))
  panel <- expand.grid(station = c("s1", "s2"), hour = 0:2)
  panel$datetime <- as.POSIXct("2023-01-01", tz = "UTC") + panel$hour * 3600
  panel$pm10 <- 60; panel$year <- 2023L
  arrow::write_dataset(panel[, c("station", "datetime", "pm10", "year")],
                       file.path(root, "panel"), partitioning = "year")
  census <- data.frame(geo_id = paste0("g", 1:4), pop_total = 100, education_mean = 1:4)
  out <- aggregate_idw_exposure(file.path(root, "panel"),
    file.path(root, "toy_geo_station_distances.parquet"), census_col = census,
    pop_col = "pop_total", group_var = "education_mean", n_groups = 2L,
    group_name = "edu_half", quintile_level = "geo", buffer_km = 3,
    distance_power = 1, target_years = 2023, pollutants = "pm10", mem_gb = 1,
    n_threads = 1, out_dir = root, out_name = "exposure", chunk_by_month = FALSE,
    quiet = TRUE, return_data = TRUE)
  exposure <- data.table::as.data.table(out$exposure_yearly)
  # Convex combination of constant 60 is exactly 60 irrespective of distances.
  expect_equal(exposure$avg_pm10, rep(60, 4), tolerance = 1e-10)
  individual <- data.table::data.table(geo_id = rep(paste0("g", 1:4), each = 2),
    edu_quintile = rep(1:2, 4), person_weight = rep(c(1, 2), 4))
  fit <- suppressWarnings(compute_exposure_regressions(exposure, individual,
    group_values = 1:2, base_group = 2, pollutants = "pm10", year_filter = 2023,
    normalized = FALSE, se_type = "classic", quiet = TRUE))
  expect_true(all(abs(fit$estimate) < 1e-10))
  dir.create(file.path(root, "results/figures/exposure"), recursive = TRUE)
  dir.create(file.path(root, "results/tables"), recursive = TRUE)
  dir.create(file.path(root, "scripts"))
  writeLines("# Synthetic producer", file.path(root, "scripts/toy.R"))
  grDevices::pdf(file.path(root, "results/figures/exposure/toy.pdf"))
  plot(exposure$avg_pm10); grDevices::dev.off()
  writeLines("\\begin{tabular}{r}60\\end{tabular}", file.path(root, "results/tables/toy.tex"))
  manifest <- data.frame(artifact_id = c("figure", "table"),
    source_path = c("results/figures/exposure/toy.pdf", "results/tables/toy.tex"),
    paper_path = c("figures/toy.pdf", "tables/toy.tex"), producer_script = "scripts/toy.R")
  plan <- export_paper_artifacts(manifest, root, file.path(root, "paper"))
  expect_true(all(file.exists(file.path(root, "paper", manifest$paper_path))))
})

test_that("isolated peaks distinguish temporal failure and missing neighbors", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dist <- data.frame(station_from = c("A", "B"), station_to = c("B", "A"), distance_km = 1)
  arrow::write_parquet(dist, file.path(root, "dist.parquet"))
  run <- function(values, tag, neighbor = rep(NA_real_, length(values))) {
    d <- data.frame(station = rep(c("A", "B"), each = length(values)),
      datetime = rep(as.POSIXct("2023-01-01", tz = "UTC") + seq_along(values) * 3600, 2),
      pm10 = c(values, neighbor), year = 2023L)
    arrow::write_dataset(d, file.path(root, paste0(tag, "in")), partitioning = "year")
    out <- detect_pollution_outliers(file.path(root, paste0(tag, "in")),
      file.path(root, "dist.parquet"), root, tag, pollutants = "pm10",
      pct_flag = .9, neighbor_eligibility = "all", quiet = TRUE)
    as.data.frame(arrow::open_dataset(out))
  }
  # Twenty ordinary zeros and one isolated 100: benchmark=0, temporal SD<100/2.
  a <- run(c(rep(0, 10), 100, rep(0, 10)), "peak")
  expect_equal(sum(a$pm10_outlier_reason == 2L), 1)
  expect_equal(sum(a$pm10 == 0, na.rm = TRUE), 20)
  # A simultaneous neighbor tracks the peak. Differences alternate +/-1:
  # the spatial deviation is within two sample SDs, so the peak is rescued.
  values <- c(rep(0, 10), 100, rep(0, 10))
  rescued <- run(values, "rescue", values + (-1)^seq_along(values))
  expect_equal(rescued$pm10[rescued$station == "A" & rescued$datetime ==
    as.POSIXct("2023-01-01", tz = "UTC") + 11 * 3600], 100)
  # No adjacent reading around the peak: no temporal or spatial rescue exists.
  b <- run(c(0, NA, 100, NA, 0), "missing")
  expect_equal(sum(b$pm10_outlier_reason == 1L), 1)
  expect_true(all(is.na(b$pm10[b$pm10_outlier_reason > 0])))
})
