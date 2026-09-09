# Data layers & paths

The data flow is a one-way ratchet. Respect the direction; never write backwards.

```
data/downloads/  raw pulls, as fetched (Selenium/API landing zone)
data/raw/        immutable original inputs  ── read-only, never write here from code
data/interim/    intermediate, reproducible (Parquet/RDS)
data/processed/  analysis-ready datasets    ── principal inputs to tables_images/
results/         figures/ tables/           ── final artefacts
data/_legacy/    coauthor's original data   ── read-only, validation track only
```

## Rules

- **`data/raw/` and `data/_legacy/` are read-only from code.** Scripts read them; nothing writes
  to them. (Enforce with read-only mounts where possible.) Re-fetching raw data is a deliberate download-script run.
- **`tables_images/` scripts read only from `data/processed/` (or `data/interim/`)**, with an explicit exception for original geographic context/legacy satellite inputs whose specification requires them. Keep cleaning out of plotting code.
- **Every path is `here::here("segment", "segment", ...)`** relative to the project root. No
  absolute paths, no `setwd()`. This is what lets the same script run in Docker and in RStudio.
- **All of `data/` is git-ignored.** Authorized raw inputs must be archived externally with recorded access/provenance; no deposit is asserted here. Don't add data
  files to git or assume a teammate has your local files — derive them from scripts.
- Big tables: prefer `arrow`/`duckdb` (scan on disk) over reading everything into memory. Write
  intermediates as Parquet so each stage's output can be opened and checked independently.
- Name outputs so a human can find them: `<city>_<year>_<what>.parquet`, matching the existing
  patterns in `results/`.

## Why this matters

A reader should be able to stop after any stage, open the interim/processed file, and see exactly
what the code produced. One-way layers + Parquet checkpoints + `here::here()` are what make that
possible. Don't shortcut them for convenience.

Standardized station panels and generated geography belong under data/interim/.
Census extraction work belongs under data/interim/census_extracted/. Raw geographic
source files may be read by context maps; generated layers are read from interim.
Canonical outputs: results/figures/{maps,monitoring,exposure,imputation,temporal,
satellite,diagnostics}/ and flat results/tables/. Export uses config/paper_artifacts.csv.
