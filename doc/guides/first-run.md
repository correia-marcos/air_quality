# First run: three stations, three distances

Start at the repository root. Use `docker compose up` and open the project in its RStudio
session, or restore the project packages locally with `renv::restore()` before opening
`Coding.Rproj`. The local route also needs the system libraries used by sf/terra and Arrow;
[the setup guide](../HOW_TO_RUN.md#development-and-acquisition) describes both routes.
Analysis scripts load installed packages; they do not install missing dependencies.

Open `Coding.Rproj` in RStudio with the restored project environment. This example
needs no city data. Three stations form a triangle with sides 3, 4 and 5 kilometres.
We can check each number before following the four-city analysis.

## Read the definitions and create the stations

```r
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "distances.R"))

crs <- aeqd_crs(0, 0)
stations <- sf::st_as_sf(
  data.frame(station = c("A", "B", "C"),
             x = c(0, 3000, 0), y = c(0, 0, 4000)),
  coords = c("x", "y"), crs = crs
)
```

The coordinates are metres on this projected grid. A is at the origin; B is 3 km
away horizontally and C is 4 km away vertically. Their separation is
`sqrt(3^2 + 4^2) = 5` km. Inspect `stations` in RStudio.

## Compute the distances

```r
distances <- compute_distance_matrices(
  stations_sf = stations,
  station_id_col = "station",
  distance_metric = "aeqd",
  evaluation_crs = crs
)
distances$station_matrix
stopifnot(nrow(distances$station_matrix) == 9L)
```

The table contains every ordered pair: A–B and B–A both have distance 3; A–C and
C–A have distance 4; B–C and C–B have distance 5. A station's distance to itself
is zero. `geo_station_matrix` is NULL because we supplied no geographic polygons.
The function has not saved an analytical output.

## Save the result

```r
files <- write_distance_matrices(
  result = distances,
  out_dir = here::here("data", "interim", "distance_example")
)
saved_distances <- arrow::read_parquet(files[["stations"]])
```

`saved_distances` is the same table, read from its Parquet checkpoint.
[The synthetic check](../../tests/testthat/test-distance-matrices.R) tests this triangle
and the computation/saving contract. It is a small calculation, not a manuscript run.

For real data, open
[generate_distance_matrices.R](../../scripts/process_data/generate_distance_matrices.R).
Its first section reads the station and geographic files. Its second section calls the
same function five times, including both Santiago geographic vintages. Its third section
saves the results in matching order. The geographic matrix connects each geographic unit
to stations; the [IDW example worked by hand](../reference/idw_golden_test.md) shows how
those distances become exposure estimates.

Use [HOW_TO_RUN](../HOW_TO_RUN.md) for environment setup, the complete analysis, and
verification. You do not need to learn targets to follow the individual script.
