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
- **Comments: one home for rationale.** *Why* a function does what it does belongs in its
  `@Description` / `@details` block — written once, updated in place. Comments **inside** a
  function body are at most **2 lines** and say only what happens *to the data* here, or point
  back: `# see @Details: cluster identification`. Never restate in the body what the block
  above already says. If a change needs more explanation, extend `@details` — do not grow a
  paragraph mid-function. *Exempt:* the file header block and the `# ---` doc block above a
  `src/` function; those are the designated home and are long by design.
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
#' @Goal: <one line: what this script produces>
#
#' @Description: <2-4 lines: inputs, what is done, outputs and where they land>
# <continuation lines keep the plain # prefix>
#
#' @Summary:
#   I.   Setup: load dependencies, utilities, city config
#   II.  <stage>
#   III. <stage>
#
#' @Date: <Month Year>
#' @Author: <name>
# ============================================================================================
```

**The `@tag` line takes `#'`; continuation lines and the `# ===` banner rules stay plain `#`.**
The `#'` prefix is for RStudio only — it colours the tags and is what makes these blocks
scannable. roxygen2 never runs here (no `R/`, no `NAMESPACE`, `DESCRIPTION` is `Type: Project`,
`Coding.Rproj` is `BuildType: Makefile`), so the custom `@Goal` / `@Summary` tags cost nothing.

Match the existing files exactly (see `scripts/process_data/estimate_exposure.R`). Section
dividers inside the script reuse the same `# ===` rule with a `# I:` / `# II:` label. Banner
rules are 94 characters wide throughout.

## Every `src/` function has a doc block

Same `#'`-on-the-tag-line rule, with roxygen's standard argument tags:

```r
# --------------------------------------------------------------------------------------------
# Function: assign_socio_group
#
#' @param dt      data.table; modified in place. Must contain a `geo_id` column.
#' @param out_col string; name of the group column to create.
#
#' @return  the same data.table, invisibly, with `out_col` added.
#
#' @details
#   Why it does what it does. This is the one home for rationale.
#
#' @Written_on : July 2026
#' @Written_by : Marcos Paulo
# --------------------------------------------------------------------------------------------
```

Use `@param` / `@return` / `@details` — **not** the old `@Arg` / `@Output` / `@Details`. `@param`
takes the name then the description, with no colon between them. `@Purpose`, `@Written_on` and
`@Written_by` stay as project-specific tags.

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
