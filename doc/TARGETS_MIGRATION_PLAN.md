# Migration plan: `run_pipeline.R` → `{targets}`

A plan, not code. It lays out whether and how to move the pipeline to the R-native
[`{targets}`](https://docs.ropensci.org/targets/) engine (rOpenSci; Will Landau) — the R
equivalent of Make. Read it, decide, then we implement incrementally.

## Why consider it

`run_pipeline.R` sources scripts top-to-bottom every time: no dependency graph, no caching, it
re-runs everything even if only one figure changed. `{targets}` fixes exactly that:

- **Dependency DAG.** It parses which target reads which, and runs only what's stale.
- **Skip-unchanged.** Change one script or input and only its downstream targets rebuild — big for
  a slow geospatial pipeline.
- **Provenance.** `tar_visnetwork()` draws the graph (great for the README and for referees);
  `tar_meta()` records what ran and when.
- **Parallelism** across independent targets (the four cities are embarrassingly parallel).
- **Reproducibility fit.** It layers cleanly under Docker + renv — `{targets}` is just another
  pinned package; results still come from the same functions.

Cost, stated plainly: the pipeline must become **function-based**. Today your `scripts/` do work as
top-level side effects (read → transform → `write_parquet`). `{targets}` wants functions that
*return* objects (or return output paths). You already separate `src/` functions from `scripts/`
execution, so you are halfway there — this is a refactor, not a rewrite.

## How it maps onto this repo

`{targets}` shines when a stage is `output <- f(inputs)`. Proposed shape:

- **Raw inputs as file targets:** `tar_target(bogota_raw, "data/raw/.../file", format = "file")`.
  If the file changes, everything downstream rebuilds.
- **Per-city branching:** the four cities share structure, so use static branching with
  `tarchetypes::tar_map(values = cities, ...)` (or `tar_eval`) to stamp out
  process/distances/outliers/exposure targets per city without copy-paste — the `{targets}`
  analogue of your `registry.R` dispatch.
- **Data-frame targets:** return tibbles/arrow tables and let `{targets}` store them
  (`format = "parquet"` via the `arrow` integration), *or* keep writing Parquet yourself and
  return the path as a `format = "file"` target so the on-disk file stays inspectable in RStudio
  (recommended — it preserves your "referee can open the checkpoint" property).
- **Figures/tables as file targets:** each returns the path it wrote to `results/…`, so a changed
  upstream dataset invalidates exactly the affected figures.
- **Validation track** stays separate: either its own `_targets.R` in
  `scripts/validation_old_version/`, or Quarto reports built with `tarchetypes::tar_quarto()` so a
  report re-renders only when its inputs move.

### Illustrative `_targets.R` sketch (one city, abbreviated)

```r
library(targets)
library(tarchetypes)
tar_option_set(packages = c("arrow", "sf", "data.table", "dplyr", "here"))
tar_source("src")  # source all src/ helpers as functions

list(
  # raw inputs tracked as files -> downstream rebuilds when they change
  tar_target(bogota_stations_file,
             here::here("data/raw/monitoring_stations/bogota_stations.csv"),
             format = "file"),

  # processing returns the path it wrote (keeps Parquet inspectable on disk)
  tar_target(bogota_processed,
             process_city("bogota", bogota_stations_file),   # function in src/
             format = "file"),

  tar_target(bogota_distances, make_distances("bogota", bogota_processed), format = "file"),
  tar_target(bogota_outliers,  detect_outliers_city("bogota", bogota_processed), format = "file"),
  tar_target(bogota_exposure,
             estimate_idw("bogota", bogota_distances, bogota_outliers), format = "file"),

  # a figure re-renders only when its input dataset changes
  tar_target(fig_quintile_ci,
             plot_exposure_quintile_ci(bogota_exposure), format = "file")
)
```

Run with `tar_make()`; inspect with `tar_visnetwork()`; read a result with `tar_read(bogota_exposure)`.

## Incremental, low-risk migration

Do this in slices so the repo always works:

1. **Add deps:** `renv::install(c("targets", "tarchetypes"))`, then `renv::snapshot()`. Commit the
   lockfile change yourself.
2. **Refactor one stage of one city into a function** in `src/` that returns its output path (start
   with Bogotá `process`). Keep the existing `scripts/` version working in parallel.
3. **Write a minimal `_targets.R`** covering just that target; confirm `tar_make()` reproduces the
   same Parquet file byte-for-byte.
4. **Expand across stages, then across cities** via `tar_map` over the city registry.
5. **Fold in figures/tables** as file targets; add `tar_visnetwork()` output to the README.
6. **Add the validation reports** with `tar_quarto()`.
7. **Retire `run_pipeline.R`** (or keep it as a one-line `targets::tar_make()` shim) once parity is
   proven. Update the Makefile's stage recipes to call `Rscript -e 'targets::tar_make()'` if you
   want to keep `make` as the outer entry point.

## How it coexists with what you have

- **Docker/renv:** unchanged; `{targets}` runs inside the container, pinned like any package.
- **Makefile:** complementary — Make can stay the human-facing entry point (`make all` →
  `tar_make()`), or you drop it once `{targets}` is the orchestrator. Don't maintain two DAGs.
- **`here::here()` + Parquet checkpoints:** keep both. Using `format = "file"` targets means every
  intermediate is still a real file a referee can open — you gain caching without losing transparency.
- **`/reproduce`:** still valid; it would just say `tar_make()` and point at `tar_visnetwork()`.

## TL;DR

| Question | Answer |
|---|---|
| What is it? | R-native Make: dependency DAG + caching + provenance |
| Main benefit | Rebuild only what changed; auto graph of the pipeline |
| Main cost | Refactor scripts into functions that return objects/paths |
| Fits your repo? | Well — you already split `src/` functions from `scripts/`; 4 cities map to `tar_map` |
| Keeps data inspectable? | Yes, if you use `format = "file"` targets writing Parquet |
| Replaces Docker/renv? | No — layers under them |
| Replaces the Makefile? | Optional; keep Make as the outer entry point or retire it |
| Migration risk | Low if done stage-by-stage with the old pipeline kept alive |
