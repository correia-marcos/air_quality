# Independent oracles: see doc/ai/methods-tests.md. No production output is a golden file.
test_that("cluster covariance equals an independently assembled weighted score sandwich", {
  d <- data.frame(x = rep(c(0, 1), 4), y = c(1, 3, 2, 1, 4, 6, 3, 8),
                  w = c(1, 3, 2, 1, 4, 2, 3, 1), .cluster_geo = rep(1:4, each = 2))
  X <- cbind(1, d$x)
  bread <- solve(t(X) %*% (d$w * X))
  beta <- bread %*% t(X) %*% (d$w * d$y)
  scores <- rowsum(X * as.vector(d$w * (d$y - X %*% beta)), d$.cluster_geo)
  # HC1 and cluster corrections: (N-1)/(N-k) times G/(G-1).
  V <- bread %*% crossprod(scores) %*% bread * (7 / 6) * (4 / 3)
  actual <- .exposure_coef_table(lm(y ~ x, data = d, weights = w), d, "cluster_geo", .95)
  expect_equal(actual$estimate, as.vector(beta), tolerance = 1e-10)
  expect_equal(actual$std_error, sqrt(diag(V)), tolerance = 1e-10)
  expect_equal(actual$ci_high, as.vector(beta) + qt(.975, 3) * sqrt(diag(V)),
               tolerance = 1e-10)
})

test_that("IDW includes the exact boundary and ignores row ordering", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fx <- make_toy_fixture(root)
  distances <- as.data.frame(arrow::read_parquet(fx$dist_pq))
  distances$distance_km[distances$geo_id == "g1" & distances$station_id == "s3"] <- 3
  run <- function(tag) aggregate_idw_exposure(
    arrow_dir = fx$arrow, geo_sta_pq = fx$dist_pq, census_col = fx$census,
    pop_col = "pop_total", group_var = "education_mean", n_groups = 2L,
    group_name = "edu_half", quintile_level = "geo", buffer_km = 3,
    distance_power = 1, target_years = 2023, pollutants = "pm10", mem_gb = 1,
    n_threads = 1, out_dir = file.path(root, tag), out_name = "toy",
    chunk_by_month = FALSE, quiet = TRUE, return_data = TRUE)$exposure_yearly
  arrow::write_parquet(distances, fx$dist_pq)
  a <- data.table::as.data.table(run("a"))
  # h0=(60+120/2+300/3)/(1+1/2+1/3)=120; h1=(40+60/3)/(1+1/3)=45.
  expect_equal(a[geo_id == "g1"]$avg_pm10, 82.5, tolerance = 1e-10)
  arrow::write_parquet(distances[nrow(distances):1, ], fx$dist_pq)
  fx$census <- fx$census[3:1, ]
  b <- data.table::as.data.table(run("b"))
  data.table::setorder(a, geo_id); data.table::setorder(b, geo_id)
  expect_equal(a, b, tolerance = 1e-10)
})

test_that("Mexican census education uses weighted reporting adults as denominator", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  e <- new.env(parent = environment())
  e$`%>%` <- dplyr::`%>%`
  sys.source(here::here("src/city_specific/registry.R"), envir = e)
  sys.source(here::here("src/city_specific/cdmx.R"), envir = e)
  d <- data.frame(ENT = 9, MUN = 2, SEXO = 3, PARENTESCO = 101,
    PERTE_INDIGENA = 2, EDAD = c(30, 30, 30, 20), CONACT = 10, INGTRMEN = 0,
    FACTOR = c(1, 3, 100, 1000), ESCOACUM = c(0, 12, 99, 12),
    NIVACAD = 0, ESCOLARI = c(0, 12, 99, 12))
  file <- file.path(root, "census.csv"); write.csv(d, file, row.names = FALSE)
  out <- e$mexico_harmonize_census_data(data.frame(file = file), out_dir = root, quiet = TRUE)
  expect_equal(out$collapsed$pop_total, 104)
  expect_equal(out$collapsed$pop_educ_known, 4)
  expect_equal(out$collapsed$education_mean, (0 * 1 + 12 * 3) / 4)
  expect_equal(nrow(out$individual), 4L)
})

test_that("imputation preserves observations and isolates the requested year", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  d <- expand.grid(hour = 0:239, station = c("A", "B"), year = 2022:2023)
  d$datetime <- as.POSIXct(paste0(d$year, "-01-01"), tz = "UTC") + d$hour * 3600
  d$pm10 <- 20 + sin(d$hour / 4) + (d$station == "B") * 10
  d$pm10[d$station == "A" & d$hour %in% c(100, 120) & d$year == 2023] <- NA_real_
  arrow::write_dataset(d[, c("station", "datetime", "pm10", "year")],
                       file.path(root, "in"), partitioning = "year")
  out <- impute_missing_hourly_ols(file.path(root, "in"), root, "imputed",
                                 pollutants = "pm10", years = 2023, quiet = TRUE)
  actual <- as.data.frame(arrow::open_dataset(out$out_path))
  expect_equal(unique(actual$year), 2023L)
  expected <- d[d$year == 2023, ]
  idx <- match(paste(expected$station, expected$datetime), paste(actual$station, actual$datetime))
  observed <- !is.na(expected$pm10)
  expect_equal(actual$pm10[idx[observed]], expected$pm10[observed], tolerance = 0)
  expect_true(all(is.finite(actual$pm10[idx[!observed]])))
  expect_error(impute_missing_hourly_ols(file.path(root, "in"), root, "absent",
                                       pollutants = "pm10", years = 1999, quiet = TRUE), "not in")
})
