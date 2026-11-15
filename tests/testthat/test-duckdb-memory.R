# The DuckDB memory limit of the IDW runs: a number is used as given, "auto" leaves 6 GB
# of the machine's memory for R, and an unreadable machine asks for a number.
test_that("the IDW DuckDB limit takes a number as given and sizes auto to the machine", {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src/general_utilities/process/idw_exposure.R"), e)
  expect_identical(e$duckdb_memory_limit_gb(18), 18L)
  expect_identical(e$duckdb_memory_limit_gb(0.5), 1L)

  e$machine_memory_gb <- function() 48
  expect_identical(e$duckdb_memory_limit_gb("auto"), 42L)
  e$machine_memory_gb <- function() 4
  expect_identical(e$duckdb_memory_limit_gb("auto"), 1L)

  e$machine_memory_gb <- function() NA_real_
  expect_error(e$duckdb_memory_limit_gb("auto"), "set idw_duckdb_mem_gb")
  expect_error(e$duckdb_memory_limit_gb("large"), "number of GB")
})

test_that("every manuscript IDW call passes the declared memory setting", {
  files <- c(here::here("config/targets", c("exposure.R", "robustness.R", "imputation.R")),
             here::here("scripts/process_data", c("exposure/estimate_idw.R",
               "robustness/buffer_5km/estimate_idw_5km.R",
               "imputation/estimate_exposure_imputed.R")))
  for (file in files) {
    text  <- readLines(file)
    calls <- sum(grepl("^[^#]*run_idw_city\\(", text))
    passed <- sum(grepl("mem_gb +=.*idw_duckdb_mem_gb", text))
    expect_equal(passed, calls, info = basename(file))
  }
})
