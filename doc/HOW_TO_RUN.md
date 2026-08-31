# How to run this repository from scratch

For a student or a journal reviewer with no prior exposure to the project: install the
container, understand how the repository is divided, and reproduce the paper's figures
and tables. The whole setup in five commands:

```bash
git clone <repository-url> Coding && cd Coding
cp .env.example .env
docker compose up          # first run builds the image; then open http://localhost:8787
```

then, inside the RStudio that opens in your browser:

```r
source(here::here("scripts", "run_pipeline.R"))
```

The rest of this guide explains each step and the alternative entry points (Makefile,
single scripts, no-Docker).

## 1. What you need

- **git** and **Docker** (Docker Desktop on macOS/Windows, Docker Engine on Linux).
  Nothing else: R, every R package, and all system libraries (GDAL, GEOS, PROJ, Java)
  live inside the container, pinned by `renv.lock` and the `Dockerfile`.
- **Disk and patience:** the image is several GB and the first build takes a while
  because R packages are compiled. Later builds reuse the cache.
- **The raw data** (see §3): `data/` is git-ignored, so a fresh clone contains no data.

## 2. Install the container (one-time)

There is no image to `docker pull` from a registry — `docker compose up` **builds** it
locally from the repo's `Dockerfile` and then starts it.

1. **Clone and configure:**

   ```bash
   git clone <repository-url> Coding && cd Coding
   cp .env.example .env
   ```

   `.env` holds the RStudio login (`RSTUDIO_USER` / `RSTUDIO_PASSWORD`) and, only if
   you will re-download raw data, Earthdata / Stadia Maps credentials. The defaults
   (`rstudio` / `replication`) are fine for replication. `.env` is git-ignored —
   never commit credentials.

2. **Build and start:**

   ```bash
   docker compose up          # add -d to leave it running in the background
   ```

   Two services start: `analysis` (R + RStudio + the pinned package library, port
   8787) and `selenium` (a headless Firefox used *only* by the download scripts).

3. **Open RStudio** at <http://localhost:8787> and log in with the `.env` credentials.
   You land in `/air_monitoring`, with `src/`, `scripts/` and `results/` mounted live
   from your clone (edit on the host, the change is visible in the container).

Stop with `docker compose down` (add `-v` to also drop the cached package volume).
For the **immutable replication run** — source code baked into the image, `data/raw`
mounted read-only — use `docker compose -f docker-compose.release.yml up` instead.

## 3. Get the data in place

`data/` is entirely git-ignored (size and licensing). Two options:

- **Replication (recommended):** unpack the raw-data archive distributed with the
  package into `data/raw/`. The pipeline then needs no network access and no
  credentials.
- **Re-download from source:** `scripts/download_data/` reconstructs each source as
  of the analysis vintage. These scripts are credential-gated (Earthdata for MERRA-2),
  slow, and web APIs drift — expect to adapt them. `make download` runs them all.

`data/raw/` is an input of record: pipeline code reads it and never writes to it.

## 4. How the repository is divided

Two rules organize everything. **Functions in `src/`, execution in `scripts/`** — a
referee can read any script top-to-bottom and watch the data change. And **data flows
one way** — `data/raw` → `data/interim` → `data/processed` → `results` — with every
stage's output written as Parquet that you can open and inspect on its own.

| Path | What lives there |
|---|---|
| `src/city_specific/` | One module per city + `registry.R` (dispatch by city id) |
| `src/general_utilities/` | Shared helpers, stage loaders (`config_utils_*.R`), `process/`, `plot/`, `validation/` |
| `scripts/download_data/` | Pull raw inputs (APIs, Selenium, Earthdata) |
| `scripts/process_data/` | raw → interim → processed |
| `scripts/tables_images/` | processed → figures and tables |
| `scripts/validation_old_version/` | Legacy-comparison track (audits the pipeline against the code behind the published numbers) |
| `scripts/run_pipeline.R` | **The single record of run order** — sources every stage in dependency order |
| `data/raw/` | Immutable original inputs (read-only from code) |
| `data/interim/`, `data/processed/` | Reproducible intermediates; analysis-ready datasets (Parquet) |
| `data/_legacy/` | Coauthor's original data — read-only, validation track only |
| `results/paper/` | **Exactly what the manuscript prints** — nothing else |
| `results/figures/`, `results/tables/` | The repo's own working artefacts (5 km robustness, companions, diagnostics) |
| `tests/` | testthat suite (`Rscript tests/testthat.R`) |
| `doc/` | This guide, the audit trail (`doc/audits/`), remaining-work notes |
| `Makefile` | Stage-level wrapper around the same scripts (see §5B) |
| `Dockerfile`, `renv.lock` | The two pinned layers: system + R version; package versions |

Scripts are named for **what they produce**, never numbered — the run order lives in
`run_pipeline.R` and in the `Makefile`, and a test fails if the two drift apart.

## 5. Three ways to run the pipeline

### A. `run_pipeline.R` — read it top-to-bottom (best first visit)

`scripts/run_pipeline.R` sources each stage script in strict dependency order; it is
plain `source(...)` lines, so reading it *is* reading the pipeline. In RStudio
(inside the container, or locally after §6):

```r
source(here::here("scripts", "run_pipeline.R"))
```

Download steps are commented out at the top (Step 0). The full run takes hours; the
20 km IDW pass over Bogotá's ~57,000 units dominates.

### B. The Makefile — stage level, skips unchanged work (best for reruns)

`make` runs the stages in order and **skips stages whose inputs have not changed**
(stamp files under `data/.make`; touching any `src/*.R` invalidates downstream
stages):

| Command | What it does |
|---|---|
| `make` (or `make all`) | process → distances → outliers → exposure → descriptives + scatter + imputed → figures + tables |
| `make <stage>` | One stage and its prerequisites: `process`, `distances`, `outliers`, `exposure`, `descriptives`, `scatter`, `imputed`, `figures`, `tables` |
| `make merra2` | Satellite track (MERRA-2 panels, station comparison, aerosol figures) — deliberately not part of `all` |
| `make download` | Credential-gated raw pulls — deliberately not part of `all` |
| `make validate` | Legacy-comparison track |
| `make clean` | Remove stage stamps only — never deletes data or results |
| `make help` | List targets |

`make` uses the R on your host. To run every recipe inside the container instead:

```bash
make DOCKER=1            # each step becomes docker compose run --rm analysis Rscript <script>
```

The Makefile is a convenience layer, **not** the reproducibility guarantee — that is
Docker + renv + `here::here()` paths. If in doubt, prefer `make DOCKER=1` or the
RStudio route.

### C. One script at a time

Every script is self-contained (`here::here()` resolves paths from the project root),
so you can run any single one once its inputs exist — from RStudio, or from the host:

```bash
docker compose run --rm analysis Rscript scripts/process_data/detect_outliers.R
```

Useful for re-running one stage after changing one function, or for stepping through
the pipeline stage by stage while inspecting each output.

## 6. No Docker? Local R + renv

Possible, but you own the system libraries (GDAL, UDUNITS, Java, … — see the
`Dockerfile`'s apt list), so Docker is the supported path. With an R version matching
`renv.lock` (R 4.6):

```r
install.packages("renv")
renv::restore()           # installs every package at its pinned version
```

then open `Coding.Rproj` and run as in §5A. The same scripts run identically in both
environments — that is what `here::here()` buys.

## 7. Reviewer checklist: the paper's results in five steps

1. `docker compose up`, open <http://localhost:8787>, put the raw data in `data/raw/`.
2. Run `source(here::here("scripts", "run_pipeline.R"))` (or `make DOCKER=1` from the
   host). Hours, one command.
3. **Look in `results/paper/`** — it holds exactly the manuscript's deliverables:
   every figure and table the paper cites, under the `.tex`'s own filenames, all PDF.
   `results/paper/tex_path_mapping.csv` maps each old `\includegraphics` path to the
   produced file, and `results/paper/update_tex_paths.sh` applies the mapping to a
   copy of the manuscript. `results/figures/` and `results/tables/` hold the repo's
   own working artefacts — never cited by the paper.
4. **Verify without re-reading code:** `Rscript tests/testthat.R` covers hand-worked
   golden values for the IDW estimator, the canonical data schema across all four
   cities, and pipeline path integrity.
5. **Inspect any intermediate:** every Parquet under `data/interim/` and
   `data/processed/` opens directly (`arrow::open_dataset()`, DuckDB, or RStudio's
   data viewer). The design goal is that you can stop after any stage and see exactly
   what the code produced.

For a bit-for-bit, frozen-environment run: `docker compose -f docker-compose.release.yml
up` starts the same pipeline from an image whose code cannot be edited from the host,
with `data/raw` mounted read-only.

## 8. Troubleshooting

- **First `docker compose up` is slow** — expected; the multi-stage build compiles the
  R library once and caches it (a named volume `renv_cache`).
- **Port 8787 already in use** — change the left-hand side of the port map in
  `docker-compose.yml` (`"8788:8787"`) and open the new port.
- **The `selenium` service fails its health check** — it is only used by the download
  scripts. For analysis-only work, bypass it:
  `docker compose run --rm --no-deps analysis bash`.
- **`make` on the host fails on missing packages** — the host R is not the pinned
  library; use `make DOCKER=1` (or `renv::restore()` locally, §6).
- **Scripts inside the container cannot see your edits** — the dev compose mounts
  `src/`, `scripts/`, `results/` live; only `data/` subfolders other than `raw/` and
  `downloads/` are not mounted, and are written inside the container. If you need
  them on the host, copy them out with `docker compose cp`.
