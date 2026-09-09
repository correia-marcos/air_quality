# Reproducibility

This repo uses environment controls and recorded verification to support reproduction.
A completed run and reviewed comparisons are required; environment controls alone are insufficient.

## The three layers

1. **Docker** packages system libraries (GDAL, GEOS, PROJ, Java, R itself via `rocker/rstudio:4.6.1`).
   Retain the tested image by identity; mutable tags and apt repositories do not pin every system package.
   This tag must stay in sync with the `R.Version` field at the top of `renv.lock`.
2. **renv** pins every R package (`renv.lock`). It is an **input of record** — never hand-edit it.
   To change packages: `renv::install(...)` / update `DESCRIPTION`, then, only when the dependency change is authorized, `renv::snapshot()`.
3. **`here::here()`** pins paths so scripts run identically inside Docker, in RStudio, or from a
   terminal.

## When editing infra, keep these invariants

- The Dockerfile's three stages (`base` → `builder` → `final`) exist so `renv::restore()` is
  cached in `builder`. Don't collapse them or you lose fast rebuilds.
- `R CMD javareconf` must stay — `rJava`/`XLConnect` break without it.
- New system libraries go in the `base` stage `apt-get` block, grouped with a comment, and must
  be reflected in the corresponding R package in `DESCRIPTION` + `renv.lock`.
- `.Rprofile` detects Docker (`IN_DOCKER`) and `setwd("/air_monitoring")` **before** sourcing
  `renv/activate.R`. Preserve that order.
- `docker-compose.yml` is for **development** (mounts `src/`, `scripts/`, `results/` live);
  `docker-compose.release.yml` retains the older interactive setup; `docker-compose.verify.yml` is the batch isolated verification path.

## Credentials — never commit, never print

`.env`, `.Renviron`, `.netrc` hold Earthdata / Stadia Maps credentials. They are git-ignored.
Only `.env.example` is tracked. Never read these into code output, logs, or a commit. If a
download script needs a new secret, add a placeholder to `.env.example` and document it in `README.md`.

## The reader must be able to run it

A colleague should be able to follow doc/HOW_TO_RUN.md without prior conversation context.
Keep acquisition/access, computation, export and verification explicit. Record missing resources; do not invent successful runs.
