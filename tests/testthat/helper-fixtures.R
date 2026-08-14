# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Build the toy fixture shared by the golden-value tests.
#
#' @Description: Three stations, three geographic units, three hours of 2023, one
#   pollutant (pm10). The fixture is built so that every golden value in
#   test-idw-exposure-golden.R can be recomputed by hand:
#
#     distances (km)   g1: s1 = 1, s2 = 2, s3 = 4  (s3 outside the 3 km buffer)
#                      g2: s1 = 1, s2 = 1, s3 = 2
#                      g3: s1 = 0                  (zero distance: excluded)
#     inverse weights  g1: 1, 1/2        g2: 1, 1, 1/2
#     pm10 by hour     h0: s1 = 60, s2 = 120, s3 = 300
#                      h1: s1 = 40, s2 = NA,  s3 = 60   (missingness re-normalizes)
#                      h2: all NA                        (no geo-hour rows at all)
#
#   make_toy_fixture() is only *called* from tests that skip unless arrow and duckdb
#   are installed, so sourcing this helper is free for the pure tests.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

make_toy_fixture <- function(root) {

  dist <- data.table::data.table(
    geo_id      = c("g1", "g1", "g1", "g2", "g2", "g2", "g3"),
    station_id  = c("s1", "s2", "s3", "s1", "s2", "s3", "s1"),
    distance_km = c(  1,    2,    4,    1,    1,    2,    0)
  )
  dist_pq <- file.path(root, "dist.parquet")
  arrow::write_parquet(dist, dist_pq)

  # The Arrow dataset carries no `year` column: hive partitioning supplies it
  # from the path, exactly as in the production datasets.
  poll <- data.table::data.table(
    station  = rep(c("s1", "s2", "s3"), times = 3),
    datetime = rep(
      as.POSIXct("2023-01-01 00:00:00", tz = "UTC") + 0:2 * 3600,
      each = 3
    ),
    pm10 = c(60, 120, 300, 40, NA, 60, NA, NA, NA)
  )
  arrow_dir <- file.path(root, "arrow")
  dir.create(file.path(arrow_dir, "year=2023"), recursive = TRUE)
  arrow::write_parquet(poll, file.path(arrow_dir, "year=2023", "part-0.parquet"))

  # Collapsed census for geo-level grouping; values only feed group labels.
  census <- data.table::data.table(
    geo_id         = c("g1", "g2", "g3"),
    pop_total      = c(100, 200, 50),
    education_mean = c(5, 15, 10)
  )

  list(dist_pq = dist_pq, arrow = arrow_dir, census = census)
}
