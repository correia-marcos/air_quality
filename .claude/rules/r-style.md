---
description: R coding conventions for this project
paths:
  - "**/*.R"
  - "**/*.qmd"
  - "**/*.Rmd"
---

# R style

Applies whenever you write or edit R. The overriding goal: a referee or student can read
the code top-to-bottom and understand what happens **to the data**.

## Hard rules

- **Line length ≤ 90 characters.** Break long calls onto aligned argument lines.
- **Comment roughly every ~4 lines** where it helps. Comments say *why* and *what happens to
  the data*, never restate the syntax. Keep them short.
- **Paths are always `here::here(...)`.** No absolute paths, no `setwd()`, no `~`.
- **No scattered `library()` calls.** Packages load through the stage's
  `src/general_utilities/config_utils_*.R`. If a script needs a new package, add it there and
  to `DESCRIPTION`, then `renv::snapshot()`.
- **Set a seed** (`set.seed(...)`) in any script with randomness (sampling, jitter, bootstraps).
- **`src/` holds functions only.** No top-level side-effects there. Runnable code lives in `scripts/`.

## Every script starts with this header

```r
# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: <one line: what this script produces>
#
# @Description: <2-4 lines: inputs, what is done, outputs and where they land>
#
# @Summary:
#   I.   Setup: load dependencies, utilities, city config
#   II.  <stage>
#   III. <stage>
#
# @Date: <Month Year>
# @Author: <name>
# ============================================================================================
```

Match the existing files exactly (see `scripts/process_data/process_bogota_data.R`). Section
dividers inside the script reuse the same `# ===` rule with a `# I:` / `# II:` label.

## Idioms in this codebase

- Stack: `data.table`/`dplyr` for tables, `arrow`/`duckdb` for on-disk data, `sf`/`terra` for
  geospatial. Prefer Arrow/DuckDB over loading big data fully into memory.
- Write intermediates as **Parquet** (`arrow::write_parquet`) to `data/interim/` or
  `data/processed/`; use RDS only for non-tabular R objects. This keeps outputs inspectable.
- Add a city through `src/city_specific/registry.R` (`register_city(...)`) and a per-city module,
  never by copy-pasting a whole script. See `/add-city`.
- Prefer clear intermediate objects over deep pipe chains when it helps a reader see the data's
  shape at each step.

## Don't

- Don't add abstraction, configurability, or error handling that wasn't asked for. If 200 lines
  could be 50, write 50. Single-use code stays flat.
- Don't wrap uncertainty in defensive code — if you're unsure what an output looks like, ask
  Marcos to run and check it first.
