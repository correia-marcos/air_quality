# Independent temporary files exercise the export boundary without manuscript data.
test_that("export preflights all sources and preserves unrelated destinations", {
  root <- tempfile(); dir.create(root)
  dir.create(file.path(root, "results/figures/maps"), recursive = TRUE)
  dir.create(file.path(root, "scripts"))
  writeLines("producer", file.path(root, "scripts/plot.R"))
  source <- file.path(root, "results/figures/maps/toy.pdf")
  writeLines("known artifact bytes", source)
  m <- data.frame(artifact_id = "toy", source_path = "results/figures/maps/toy.pdf",
                  paper_path = "figures/old_name.pdf", producer_script = "scripts/plot.R")
  dest <- tempfile()
  expect_error(export_paper_artifacts(transform(m, source_path = "results/figures/maps/missing.pdf"),
                                       root, dest), "Missing artifact")
  expect_false(dir.exists(dest))
  plan <- export_paper_artifacts(m, root, dest, dry_run = TRUE)
  expect_false(dir.exists(dest))
  export_paper_artifacts(m, root, dest)
  expect_identical(readLines(file.path(dest, m$paper_path)), "known artifact bytes")
  writeLines("keep", file.path(dest, "unrelated.txt"))
  expect_error(export_paper_artifacts(m, root, dest), "--overwrite")
  export_paper_artifacts(m, root, dest, overwrite = TRUE)
  expect_identical(readLines(file.path(dest, "unrelated.txt")), "keep")
  expect_identical(plan$sha256, digest::digest(source, file = TRUE, algo = "sha256"))
})

test_that("manifest rejects path traversal and colliding destinations", {
  m <- data.frame(artifact_id = "a", source_path = "results/figures/maps/a.pdf",
                  paper_path = "figures/a.pdf", producer_script = "scripts/plot.R")
  file <- tempfile(fileext = ".csv")
  write.csv(rbind(m, transform(m, artifact_id = "b")), file, row.names = FALSE)
  expect_error(artifact_manifest(file), "Duplicate")
  write.csv(transform(m, paper_path = "figures/../../a"), file, row.names = FALSE)
  expect_error(artifact_manifest(file), "Unsafe")
})

test_that("TeX scanning ignores comments and identifies incomplete coverage", {
  root <- tempfile(); dir.create(root)
  writeLines(c("% \\includegraphics{figures/comment.pdf}",
               "\\includegraphics[width=2cm]{figures/a.pdf}",
               "\\input{child}", "\\input{absent}"), file.path(root, "main.tex"))
  writeLines(c("\\input{tables/b}", "\\includegraphics{\\dynamic}"),
             file.path(root, "child.tex"))
  r <- manuscript_references(file.path(root, "main.tex"))
  expect_setequal(r$artifacts, c("figures/a.pdf", "tables/b.tex"))
  expect_length(r$issues, 2L)
})

test_that("parity checks reject identifier, schema, missingness and numeric changes", {
  a <- data.frame(id = c("a", "b"), count = c(1L, 2L), value = c(1, NA))
  expect_true(compare_numerical_tables(a, a[2:1, ], "id"))
  b <- a; b$value[1] <- 1 + 1e-9
  expect_true(compare_numerical_tables(a, b, "id"))
  b$value[1] <- 2
  expect_error(compare_numerical_tables(a, b, "id"), "differences")
  b <- a; b$count <- as.double(b$count)
  expect_error(compare_numerical_tables(a, b, "id"), "classes")
  b <- a; b$id[1] <- "x"
  expect_error(compare_numerical_tables(a, b, "id"), "differences")
  b <- a; b$value[2] <- 0
  expect_error(compare_numerical_tables(a, b, "id"), "differences")
})

test_that("unsupported unbraced TeX references remain explicit", {
  p <- tempfile(fileext = ".tex")
  writeLines("\\input\\sectionmacro", p)
  expect_match(manuscript_references(p)$issues, "Unsupported")
})

test_that("export rejects symlink escapes before copying", {
  root <- tempfile(); dir.create(root)
  outside <- tempfile(); dir.create(outside)
  dir.create(file.path(root, "results/figures"), recursive = TRUE)
  file.symlink(outside, file.path(root, "results/figures/maps"))
  writeLines("artifact", file.path(outside, "a.pdf"))
  m <- data.frame(artifact_id = "a", source_path = "results/figures/maps/a.pdf",
                  paper_path = "figures/a.pdf", producer_script = "scripts/a.R")
  expect_error(export_paper_artifacts(m, root, tempfile()), "symlink")
  on.exit(unlink(c(root, outside), recursive = TRUE), add = TRUE)
})
