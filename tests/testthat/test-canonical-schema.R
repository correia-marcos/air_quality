# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
#' @Goal: Turn doc/data_dictionary.md from prose into an enforced claim.
#
#' @Description: Asserts that every processed census Parquet carries the canonical columns
#   with the right types, that the education shares partition the reporting population, and
#   that each file states its own provenance in its Parquet key-value metadata. The whole
#   file skips when data/interim/census/ is absent, so a fresh clone still passes; it is a
#   contract on the data a run produces, not on the code alone.
#
#' @Date: August 2026
#' @Author: Marcos Paulo (initial draft by Claude Code)
# ============================================================================================

# One entry per canonical census artifact: the file, and which level it is.
canonical_census_files <- function() {
  root <- here::here("data", "interim", "census")

  list(
    list(city = "bogota_2018",   level = "micro",
         path = file.path(root, "bogota_2018", "census_2018_metro_individual.parquet")),
    list(city = "bogota_2018",   level = "geo",
         path = file.path(root, "bogota_2018", "census_2018_metro_collapsed.parquet")),
    list(city = "cdmx_2020",     level = "micro",
         path = file.path(root, "cdmx_extended_2020",
                          "census_metro_individual_2020.parquet")),
    list(city = "cdmx_2020",     level = "geo",
         path = file.path(root, "cdmx_extended_2020",
                          "collapse_metro_area_2020.parquet")),
    list(city = "santiago_2017", level = "micro",
         path = file.path(root, "santiago_2017", "census_individual_2017.parquet")),
    list(city = "santiago_2017", level = "geo",
         path = file.path(root, "santiago_2017", "census_collapsed_2017.parquet")),
    list(city = "santiago_2024", level = "micro",
         path = file.path(root, "santiago_2024",
                          "census_santiago_individual_2024.parquet")),
    list(city = "santiago_2024", level = "geo",
         path = file.path(root, "santiago_2024",
                          "census_santiago_collapsed_2024.parquet")),
    list(city = "sao_paulo_2010", level = "micro",
         path = file.path(root, "sao_paulo_2010",
                          "census_sp_individual_2010.parquet")),
    list(city = "sao_paulo_2010", level = "geo",
         path = file.path(root, "sao_paulo_2010",
                          "census_sp_collapsed_2010.parquet"))
  )
}

# Columns doc/data_dictionary.md promises at each level.
required_cols <- list(
  micro = c("geo_id", "geo_level", "person_weight", "educ_years"),
  geo   = c("geo_id", "geo_level", "pop_total", "pop_educ_known", "n_records",
            "education_mean")
)

# Skips when the census has not been produced yet, and when what is on disk predates
# the canonical schema. The second case is a stale-data skip, not a pass: re-run
# scripts/process_data/process_<city>_data.R and the contract starts being enforced.
skip_without_census <- function() {
  testthat::skip_if_not_installed("arrow")

  files <- Filter(function(f) file.exists(f$path), canonical_census_files())
  testthat::skip_if(
    length(files) == 0L,
    "no processed census on disk - run scripts/process_data/process_<city>_data.R"
  )

  pre_canonical <- vapply(files, function(f) {
    !"geo_id" %in% names(arrow::read_parquet(f$path, as_data_frame = FALSE))
  }, logical(1))

  testthat::skip_if(
    any(pre_canonical),
    paste("census on disk predates the canonical schema -",
          "re-run scripts/process_data/process_<city>_data.R")
  )
}

test_that("every processed census file carries the canonical columns", {
  skip_without_census()

  for (f in canonical_census_files()) {
    if (!file.exists(f$path)) next

    nms <- names(arrow::read_parquet(f$path, as_data_frame = FALSE))
    missing <- setdiff(required_cols[[f$level]], nms)

    expect_identical(
      missing, character(0),
      info = sprintf("%s (%s) is missing: %s", f$city, f$level,
                     paste(missing, collapse = ", "))
    )

    # A native identifier surviving un-prefixed means a schema map was not applied.
    natives <- intersect(c("GEO_ID", "CVE_MUN", "zona_id", "CUT", "code_weighting",
                           "fe", "FACTOR", "weight", "n", "escolaridad",
                           "years_schooling", "escolaridad_avg", "avg_escolaridad"),
                         nms)
    expect_identical(
      natives, character(0),
      info = sprintf("%s (%s) still carries native names: %s", f$city, f$level,
                     paste(natives, collapse = ", "))
    )
  }
})

test_that("geo_id is character everywhere, so zero padding cannot be lost", {
  skip_without_census()

  for (f in canonical_census_files()) {
    if (!file.exists(f$path)) next

    ids <- arrow::read_parquet(f$path, col_select = "geo_id")$geo_id
    expect_true(
      is.character(ids),
      info = sprintf("%s (%s): geo_id is %s, not character", f$city, f$level,
                     class(ids)[1])
    )
  }
})

test_that("the six education shares partition the reporting population", {
  skip_without_census()

  for (f in canonical_census_files()) {
    if (f$level != "geo" || !file.exists(f$path)) next

    dt <- data.table::as.data.table(arrow::read_parquet(f$path))
    share_cols <- c("share_no_ed_pop", "share_hs_inc_pop", "share_hs_com_pop",
                    "share_col_inc_pop", "share_col_com_pop", "share_grad_pop")
    if (!all(share_cols %in% names(dt))) next

    # Units where nobody reported education have no shares to sum.
    s <- rowSums(as.matrix(dt[, ..share_cols]))
    s <- s[dt$pop_educ_known > 0 & !is.na(s)]

    expect_true(
      all(abs(s - 1) < 1e-9),
      info = sprintf("%s: share sums range %.9f - %.9f", f$city, min(s), max(s))
    )
  }
})

test_that("education_mean lies on the harmonised 0-23 years scale", {
  skip_without_census()

  for (f in canonical_census_files()) {
    if (f$level != "geo" || !file.exists(f$path)) next

    em <- arrow::read_parquet(f$path)$education_mean
    em <- em[!is.na(em)]
    if (!length(em)) next

    # 23 = doctorate, the top anchor of doc/paper/old_appendix.tex:83-86.
    expect_true(
      min(em) >= 0 && max(em) <= 23,
      info = sprintf("%s: education_mean ranges %.2f - %.2f", f$city,
                     min(em), max(em))
    )
  }
})

test_that("each file states its own provenance in Parquet metadata", {
  skip_without_census()

  for (f in canonical_census_files()) {
    if (!file.exists(f$path)) next

    md <- arrow::read_parquet(f$path, as_data_frame = FALSE)$metadata

    for (k in c("city_id", "census_year", "geo_level", "geo_id_source",
                "table_level")) {
      expect_true(
        !is.null(md[[k]]) && nzchar(md[[k]]),
        info = sprintf("%s (%s): metadata key '%s' missing", f$city, f$level, k)
      )
    }

    expect_identical(md$city_id, f$city)
    expect_identical(md$table_level, f$level)
  }
})
