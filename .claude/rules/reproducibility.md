---
description: Reproducibility infrastructure (Docker, renv, credentials)
paths:
  - "Dockerfile"
  - "docker-compose*.yml"
  - "entrypoint.sh"
  - ".Rprofile"
  - "renv.lock"
  - "DESCRIPTION"
  - ".env*"
---

# Reproducibility

This repo promises bit-for-bit reproducible results for anyone, on any OS. That promise rests
on three pinned layers — don't weaken any of them without saying so explicitly.

## The three layers

1. **Docker** pins system libraries (GDAL, GEOS, PROJ, Java, R itself via `rocker/rstudio:4.5.3`).
2. **renv** pins every R package (`renv.lock`). It is an **input of record** — never hand-edit it.
   To change packages: `renv::install(...)` / update `DESCRIPTION`, then `renv::snapshot()`.
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
  `docker-compose.release.yml` is the immutable replication run. Keep that split.

## Credentials — never commit, never print

`.env`, `.Renviron`, `.netrc` hold Earthdata / Stadia Maps credentials. They are git-ignored.
Only `.env.example` is tracked. Never read these into code output, logs, or a commit. If a
download script needs a new secret, add a placeholder to `.env.example` and document it in `README.md`.

## The reader must be able to run it

Any change here is judged by one question: can a journal editor still `docker compose up`, open
RStudio, and reproduce the results without touching anything else? If a change makes that harder,
flag it and propose an alternative.
