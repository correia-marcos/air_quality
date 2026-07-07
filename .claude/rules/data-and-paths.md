---
description: Data-layer discipline and path rules for scripts and src
paths:
  - "scripts/**"
  - "src/**"
---

# Data layers & paths

The data flow is a one-way ratchet. Respect the direction; never write backwards.

```
data/downloads/  raw pulls, as fetched (Selenium/API landing zone)
data/raw/        immutable original inputs  ── read-only, never write here from code
data/interim/    intermediate, reproducible (Parquet/RDS)
data/processed/  analysis-ready datasets    ── the only inputs to tables_images/
results/         figures/ tables/           ── final artefacts
data/_legacy/    coauthor's original data   ── read-only, validation track only
```

## Rules

- **`data/raw/` and `data/_legacy/` are read-only from code.** Scripts read them; nothing writes
  to them. (A hook will stop writes.) Re-fetching raw data is a deliberate download-script run.
- **`tables_images/` scripts read only from `data/processed/` (or `data/interim/`)**, never from
  `data/raw/`. Keep cleaning out of plotting code.
- **Every path is `here::here("segment", "segment", ...)`** relative to the project root. No
  absolute paths, no `setwd()`. This is what lets the same script run in Docker and in RStudio.
- **All of `data/` is git-ignored.** Raw inputs are archived externally (Zenodo). Don't add data
  files to git or assume a teammate has your local files — derive them from scripts.
- Big tables: prefer `arrow`/`duckdb` (scan on disk) over reading everything into memory. Write
  intermediates as Parquet so each stage's output can be opened and checked independently.
- Name outputs so a human can find them: `<city>_<year>_<what>.parquet`, matching the existing
  patterns in `results/`.

## Why this matters

A reader should be able to stop after any stage, open the interim/processed file, and see exactly
what the code produced. One-way layers + Parquet checkpoints + `here::here()` are what make that
possible. Don't shortcut them for convenience.
