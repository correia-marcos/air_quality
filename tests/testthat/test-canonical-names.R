# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Tests for apply_canonical_names(), the one step where a provider's column
#   vocabulary is translated into the project's canonical schema.
#
#' @Description: Checks the four behaviours downstream code depends on: mapped columns are
# renamed, unmapped provider columns survive with a raw_ prefix, geo_id always comes back as
# character with leading zeros intact, and a mapping that names a column the data does not
# have fails loudly rather than silently doing nothing. The last one is the important
# guarantee: a silent no-op would let a city's file keep its native names and only surface
# as a confusing join failure several stages later.
#
#' @Summary:
#   I.   Renaming and raw_ passthrough
#   II.  Identifier typing
#   III. Failure modes
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

# CDMX shape: a numeric municipality key that must survive as "09002", not 9002.
cdmx_micro <- function() {
  data.table::data.table(
    CVE_MUN     = c(9002, 9003, 15125),
    FACTOR      = c(142.7, 98.3, 51.0),
    escolaridad = c(11, 6, 9),
    ingtrmen    = c(8000, 3200, 5100),
    edad        = c(34, 51, 27),
    adult       = c(1, 1, 1)
  )
}

cdmx_map <- c(CVE_MUN = "geo_id", FACTOR = "person_weight",
              escolaridad = "educ_years", ingtrmen = "income_raw")

test_that("mapped columns are renamed and unmapped ones get a raw_ prefix", {
  out <- apply_canonical_names(cdmx_micro(), cdmx_map, geo_level = "municipio",
                               quiet = TRUE)

  expect_true(all(c("geo_id", "person_weight", "educ_years", "income_raw") %in% names(out)))
  expect_false(any(c("CVE_MUN", "FACTOR", "escolaridad", "ingtrmen") %in% names(out)))

  # edad and adult were not in the map, so they survive as provider-native columns
  expect_true(all(c("raw_edad", "raw_adult") %in% names(out)))

  # values travel with the name, they are not reordered or recomputed
  expect_equal(out$person_weight, c(142.7, 98.3, 51.0))
  expect_equal(out$educ_years, c(11, 6, 9))
})

test_that("geo_level is stamped on every row when supplied", {
  out <- apply_canonical_names(cdmx_micro(), cdmx_map, geo_level = "municipio",
                               quiet = TRUE)
  expect_equal(out$geo_level, rep("municipio", 3))

  no_level <- apply_canonical_names(cdmx_micro(), cdmx_map, quiet = TRUE)
  expect_false("geo_level" %in% names(no_level))
})

test_that("geo_id comes back as character and keeps its leading zero", {
  out <- apply_canonical_names(cdmx_micro(), cdmx_map, quiet = TRUE)

  expect_type(out$geo_id, "character")
  # 9002 read as a number would print as "9002"; canonical_geo_id keeps it printable,
  # and the width padding that makes it "09002" happens at the join (see @details).
  expect_equal(out$geo_id, c("9002", "9003", "15125"))
  # the failure this guards against is scientific notation on long IBGE codes
  wide <- apply_canonical_names(
    data.table::data.table(code_weighting = 3550308005001),
    c(code_weighting = "geo_id"), quiet = TRUE)
  expect_equal(wide$geo_id, "3550308005001")
})

test_that("comuna_id is treated as an identifier too", {
  out <- apply_canonical_names(
    data.table::data.table(zona_id = "13101021", comuna = 13101, fe = 1),
    c(zona_id = "geo_id", comuna = "comuna_id", fe = "person_weight"),
    geo_level = "zona_censal", quiet = TRUE)

  expect_type(out$comuna_id, "character")
  expect_equal(out$comuna_id, "13101")
})

test_that("a mapping naming an absent column fails loudly", {
  bad <- c(CVE_MUN = "geo_id", NOT_THERE = "person_weight")
  expect_error(apply_canonical_names(cdmx_micro(), bad, quiet = TRUE),
               "NOT_THERE")
})

test_that("already-canonical and already-prefixed columns are left alone", {
  dt <- data.table::data.table(geo_id = "g1", pop_total = 100, raw_V0010 = 7)
  out <- apply_canonical_names(dt, c(pop_total = "pop_total"), quiet = TRUE)

  expect_equal(sort(names(out)), sort(c("geo_id", "pop_total", "raw_V0010")))
  expect_false("raw_raw_V0010" %in% names(out))
})

test_that("raw_keep restricts which provider columns survive", {
  out <- apply_canonical_names(cdmx_micro(), cdmx_map, raw_keep = "adult", quiet = TRUE)

  expect_true("raw_adult" %in% names(out))
  expect_false("raw_edad" %in% names(out))
})
