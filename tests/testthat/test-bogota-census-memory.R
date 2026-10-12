# Bogotá's streaming census keeps the original sample, join multiplicity and adult totals.
bogota_census_test_environment <- function() {
  env <- new.env(parent = globalenv())
  for (file in c("registry", "bogota")) {
    sys.source(here::here("src", "city_specific", paste0(file, ".R")), env)
  }
  env
}

make_bogota_census_fixture <- function(root) {
  ids <- c("1100100000000000000001", "1100100000000000000002",
    "2509900000000000000001", "2509900000000000000002")
  persons <- data.frame(
    COD_ENCUESTAS = c("age_missing", paste0("p", 1:10), "unknown", "child",
      "only_unknown", "outside", "unmatched", "dup", "dup", NA),
    P_SEXO = c("1", rep("2", 10), NA, "1", "2", "1", "2", "1", "2", "2"),
    P_EDADR = c(NA, rep("6", 11), "5", rep("6", 6)),
    P_NIVEL_ANOSR = c("2", "10", as.character(1:9), NA, "8", NA, "1", "1",
      "9", "3", "5"), P_TRABAJO = "1", unused = "ignored source field")
  geography <- data.frame(
    COD_ENCUESTAS = c(persons$COD_ENCUESTAS[1:14], "outside", "dup", "dup", "dup", NA),
    COD_DANE_ANM = c(rep(ids[1], 12), ids[2], ids[4], "9900100000000000000001",
      ids[3], ids[2], "9900100000000000000001", ids[1]))
  folders <- file.path(root, c("bogota", "cundinamarca"))
  for (folder in folders) dir.create(folder)
  write.csv(persons, file.path(folders[1], "CNPV2018_5PER_fixture.CSV"),
    row.names = FALSE, na = "NA", quote = FALSE)
  write.csv(geography, file.path(folders[1], "CNPV2018_MGN_fixture.CSV"),
    row.names = FALSE, na = "NA", quote = FALSE)
  write.csv(persons[1, ], file.path(folders[2], "CNPV2018_5PER_fixture.CSV"),
    row.names = FALSE, na = "NA", quote = FALSE)
  write.csv(data.frame(COD_ENCUESTAS = "age_missing", COD_DANE_ANM = ids[3]),
    file.path(folders[2], "CNPV2018_MGN_fixture.CSV"),
    row.names = FALSE, quote = FALSE)
  list(inputs = stats::setNames(as.list(folders), c("bogota", "cundinamarca")), ids = ids)
}

test_that("streamed Bogotá census matches hand-computed records and reporting adults", {
  env <- bogota_census_test_environment()
  root <- tempfile("bogota-census-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_bogota_census_fixture(root)
  source_files <- list.files(root, recursive = TRUE, full.names = TRUE)
  source_hashes <- tools::md5sum(source_files)
  reference <- NULL

  for (size in c(1L, 7L, 10000L)) {
    paths <- env$bogota_harmonize_census_2018_data(fixture$inputs,
      out_dir = file.path(root, paste0("derived-", size)),
      return_data = FALSE, chunk_size = size, quiet = TRUE)
    expect_named(paths, c("individual", "collapsed"))
    expect_true(all(file.exists(paths)))
    micro <- data.table::as.data.table(arrow::read_parquet(paths[["individual"]]))
    collapsed <- data.table::as.data.table(arrow::read_parquet(paths[["collapsed"]]))
    expect_equal(nrow(micro), 20L)
    expect_equal(sum(micro$adult, na.rm = TRUE), 17)
    expect_false("unused" %in% names(micro))
    expect_identical(micro[raw_COD_ENCUESTAS == "dup", geo_id],
      rep(fixture$ids[c(3, 2)], 2))
    expect_equal(micro[is.na(raw_COD_ENCUESTAS), .N], 1L)
    expect_true(all(nchar(micro$geo_id) == 22L))

    # The ten schooling levels contribute 99 years; the missing-key adult adds 11.
    known <- collapsed[geo_id == fixture$ids[1]]
    expect_equal(known$pop_total, 12)
    expect_equal(known$n_records, 12L)
    expect_equal(known$pop_educ_known, 11)
    expect_equal(known$education_mean, 110 / 11)
    bands <- c("count_no_ed", "count_hs_inc", "count_hs_com", "count_col_inc",
      "count_col_com", "count_grad")
    expect_equal(unname(as.numeric(known[, ..bands])), c(2, 2, 3, 2, 1, 1))
    shares <- sub("count_", "share_", bands)
    shares <- paste0(shares, "_pop")
    expect_equal(sum(as.numeric(known[, ..shares])), 1)
    unknown <- collapsed[geo_id == fixture$ids[4]]
    expect_equal(unknown$pop_total, 1)
    expect_equal(unknown$pop_educ_known, 0)
    expect_true(is.nan(unknown$education_mean))
    expect_equal(as.numeric(unknown[, ..bands]), rep(0, 6))
    expect_true(all(is.nan(as.numeric(unknown[, ..shares]))))
    duplicate_groups <- collapsed[geo_id %in% fixture$ids[c(2, 3)]]
    expect_equal(duplicate_groups$n_records, c(2L, 2L))
    expect_equal(duplicate_groups$education_mean, c(14, 14))

    actual <- list(individual = as.data.frame(micro), collapsed = as.data.frame(collapsed))
    if (is.null(reference)) reference <- actual else expect_identical(actual, reference)
    for (kind in names(paths)) {
      metadata <- arrow::read_parquet(paths[[kind]], as_data_frame = FALSE)$metadata
      expect_identical(metadata$city_id, "bogota_2018")
      expect_identical(metadata$census_year, "2018")
      expect_identical(metadata$geo_id_source, "GEO_ID")
      expect_identical(metadata$table_level, if (kind == "individual") "micro" else "geo")
    }
  }
  expect_identical(tools::md5sum(source_files), source_hashes)
  returned <- env$bogota_harmonize_census_2018_data(fixture$inputs,
    out_dir = file.path(root, "interactive"), quiet = TRUE, chunk_size = 3L)
  expect_identical(lapply(returned, as.data.frame), reference)
})

test_that("empty metropolitan and adult samples still write inspectable census files", {
  env <- bogota_census_test_environment()
  root <- tempfile("bogota-empty-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_bogota_census_fixture(root)
  paths <- env$bogota_harmonize_census_2018_data(fixture$inputs, metro_codes = "99999",
    out_dir = file.path(root, "empty"), quiet = TRUE, return_data = FALSE, chunk_size = 2L)
  for (path in paths) expect_equal(nrow(arrow::read_parquet(path)), 0L)
  expect_type(arrow::read_parquet(paths[["individual"]])$employed, "logical")
  expect_true(all(c("geo_id", "person_weight", "educ_years", "raw_COD_DANE_ANM") %in%
    names(arrow::read_parquet(paths[["individual"]]))))
  child <- data.frame(COD_ENCUESTAS = "child", P_SEXO = "2", P_EDADR = "5",
    P_NIVEL_ANOSR = "8", P_TRABAJO = "1")
  write.csv(child, file.path(fixture$inputs$bogota, "CNPV2018_5PER_fixture.CSV"),
    row.names = FALSE, quote = FALSE)
  children <- env$bogota_harmonize_census_2018_data(list(bogota = fixture$inputs$bogota),
    out_dir = file.path(root, "children"), quiet = TRUE, return_data = FALSE)
  expect_equal(arrow::read_parquet(children[["individual"]])$adult, 0)
  expect_equal(nrow(arrow::read_parquet(children[["collapsed"]])), 0L)
})
