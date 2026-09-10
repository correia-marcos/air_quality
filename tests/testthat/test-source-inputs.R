# Preserved sources and source-control metadata must work without network or Git state.
source(here::here("src", "general_utilities", "reproducibility.R"))

test_that("source preflight lists missing files and acquisition instructions", {
  root <- tempfile("source_inputs_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  file.create(file.path(root, "present"))
  expect_error(require_local_sources(file.path(root, c("present", "absent")),
    "acquire_sources()"), "absent.*acquire_sources", fixed = FALSE)
  expect_invisible(require_local_sources(file.path(root, "present"), "acquire()"))
  manifest <- file.path(root, "inputs.csv")
  write.csv(data.frame(root = c("present", "absent", "unrelated"),
    role = c("offline_preparation", "offline_preparation", "legacy_comparison")),
    manifest, row.names = FALSE)
  status <- preparation_input_status(manifest, root)
  expect_equal(status$root, c("present", "absent"))
  expect_equal(status$present, c(TRUE, FALSE))
})

test_that("new acquisition records provider identity and actual file hash", {
  path <- tempfile()
  on.exit(unlink(c(path, paste0(path, ".source.json"))))
  writeLines("provider source", path)
  record_source_acquisition(path, "fixture provider", "2017")
  record <- jsonlite::read_json(paste0(path, ".source.json"))
  expect_identical(record$provider, "fixture provider")
  expect_identical(record$requested_version, "2017")
  expect_identical(record$sha256,
    digest::digest(file = path, algo = "sha256", serialize = FALSE))
  expect_true(nzchar(record$retrieved_utc))
})

test_that("Git-free images use supplied revision without claiming clean Git state", {
  root <- tempfile("git_free_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  record <- verification_revision(root, revision = "reviewed-source-revision")
  expect_identical(record$revision, "reviewed-source-revision")
  expect_false(record$git_available)
  expect_null(record$changes)
  expect_null(record$patch)
  expect_identical(verification_revision(root, revision = "")$revision, "unrecorded")
})

test_that("census processing copies its preserved database before checking tables", {
  env <- new.env(parent = globalenv())
  sys.source(here::here("src", "city_specific", "registry.R"), envir = env)
  sys.source(here::here("src", "city_specific", "santiago.R"), envir = env)
  root <- tempfile("census_source_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  original <- file.path(root, "source.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = original))
  DBI::dbWriteTable(con, "fixture", data.frame(id = 1L))
  DBI::dbDisconnect(con, shutdown = TRUE)
  before <- digest::digest(file = original, algo = "sha256", serialize = FALSE)
  polygon <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2,
                                       byrow = TRUE)))
  geography <- sf::st_sf(zona_id = "13001011001", geometry = sf::st_sfc(polygon))
  expect_error(env$santiago_process_census_2017(geography,
    source_db = original, work_dir = file.path(root, "work"),
    out_dir = file.path(root, "out"), quiet = TRUE), "lacks required tables")
  expect_true(file.exists(file.path(root, "work", "censo2017.duckdb")))
  expect_identical(digest::digest(file = original, algo = "sha256", serialize = FALSE),
                   before)
})
