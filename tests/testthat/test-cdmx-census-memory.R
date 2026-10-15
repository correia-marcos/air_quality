test_that("selected census columns preserve pooled income caps and reporting adults", {
  env <- new.env(parent = environment())
  sys.source(here::here("src", "city_specific", "registry.R"), env)
  sys.source(here::here("src", "city_specific", "cdmx.R"), env)
  root <- tempfile("cdmx-census-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  persons <- data.frame(
    ENT = c(rep(9, 4), rep(13, 4), rep(15, 6)), MUN = c(rep(2, 4), rep(48, 4), rep(2, 6)),
    SEXO = 3, PARENTESCO = 101, PERTE_INDIGENA = 2,
    EDAD = c(30, 30, 30, 20, rep(30, 10)), CONACT = 10,
    INGTRMEN = c(0, 20, 30, 2000, 0, 10, 40, 50, 60, 70, 80, 90, 100, NA),
    FACTOR = c(1, 3, 100, 1000, rep(1, 10)),
    ESCOACUM = c(0, 12, 99, 17, rep(0, 9), 99), NIVACAD = 0,
    ESCOLARI = c(0, 12, 99, 17, rep(0, 9), 99))
  outside <- persons[1, ]
  outside$ENT <- 20
  outside$INGTRMEN <- 1000000
  persons <- rbind(persons, outside)
  for (index in 1:80) persons[[paste0("UNUSED_", index)]] <- paste0("unused-", index)
  files <- file.path(root, paste0("state-", 1:3, ".csv"))
  groups <- list(1:4, 5:8, 9:15)
  for (index in 1:3) {
    frame <- persons[groups[[index]], ]
    if (index == 2L) names(frame) <- tolower(names(frame))
    write.csv(frame, files[index], row.names = FALSE, na = "999")
  }
  hashes <- tools::md5sum(files)
  out <- env$mexico_harmonize_census_data(data.frame(file = files),
    metro_codes = c(9002, 13048, 15002), out_dir = file.path(root, "derived"), quiet = TRUE)
  micro <- out$individual
  collapsed <- out$collapsed
  expect_equal(nrow(micro), 14L)
  expect_identical(micro$geo_id, c(rep("09002", 4), rep("13048", 4), rep("15002", 6)))
  expect_false(any(grepl("UNUSED", names(micro))))
  expect_true(all(nchar(micro$geo_id) == 5L))

  # Eleven positive incomes give q01=11; thirteen known incomes give q99=1,772.
  # The child supplies the upper tail. The unselected municipality does not enter it.
  expect_equal(micro$income[6], 11)
  expect_equal(micro$income[4], 1772)
  expect_equal(micro$income_raw[4], 2000)
  expect_equal(micro$adult[4], 0)
  expect_true(is.na(micro$income[14]))
  expect_equal(micro$income[c(1, 5)], c(0, 0))
  first <- collapsed[geo_id == "09002"]
  expect_equal(first$pop_total, 104)
  expect_equal(first$n_records, 3L)
  expect_equal(first$pop_educ_known, 4)
  expect_equal(first$education_mean, 9)
  expect_equal(first$share_no_ed_pop, 1 / 4)
  expect_equal(first$share_hs_com_pop, 3 / 4)
  expect_equal(first$income_raw, 3060 / 104)
  expect_equal(first$income_mean, 3060 / 104)
  second <- collapsed[geo_id == "13048"]
  expect_equal(second$income_raw, 100 / 4)
  expect_equal(second$income_mean, 101 / 4)
  third <- collapsed[geo_id == "15002"]
  expect_equal(third$pop_total, 6)
  expect_equal(third$pop_educ_known, 5)
  expect_equal(third$income_raw, 400 / 6)
  expect_equal(third$income_mean, 400 / 5)
  expect_identical(tools::md5sum(files), hashes)

  paths <- env$mexico_harmonize_census_data(data.frame(file = files),
    metro_codes = c(9002, 13048, 15002), out_dir = file.path(root, "file-backed"),
    quiet = TRUE, return_data = FALSE)
  expect_named(paths, c("individual", "collapsed"))
  expect_equal(arrow::read_parquet(paths[["individual"]]), micro)
  expect_equal(arrow::read_parquet(paths[["collapsed"]]), collapsed)
  metadata <- arrow::read_parquet(paths[["individual"]], as_data_frame = FALSE)$metadata
  expect_identical(metadata$city_id, "cdmx_2020")
  expect_identical(metadata$geo_id_source, "CVE_MUN")
})
