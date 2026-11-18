# Inequality in Air Pollution Monitoring and Exposure

Replication repository for *Inequality in Air Pollution Monitoring and Exposure:
Evidence from Four Latin American Cities*. It analyzes monitoring coverage and pollutant
exposure across Bogotá, Mexico City, Santiago, and São Paulo.

## Current status

The repository records a reproducible workflow; it does not yet establish complete
scientific reproduction. That claim requires declared source inputs, a fresh isolated run,
revision-matched numerical comparisons, and review of plot data and rendering. Historical
outputs are not a reviewed baseline. See [doc/HOW_TO_RUN.md](doc/HOW_TO_RUN.md) for the
current procedure and limitations.

The reference environment is the Docker image. A result counts as reproduced when an
isolated run from the image matches a reviewed baseline from the same image; no baseline is
registered yet. Isolated runs of the final revision pass their tests and export, and they
agree with the local macOS build: identical tables, data within 2 × 10⁻¹⁰ relative. The
committed results were built on macOS and will be rebuilt in the image. The
[implementation record](doc/planning/implementation-record.md) lists every run and
comparison. Independent reproduction by someone else is a separate claim.

Choose a route:

| Goal | Start here |
|---|---|
| Find documentation by audience or topic | [Documentation index](doc/README.md) |
| Learn the repository as a student | [First run](doc/guides/first-run.md) |
| Run the analysis script by script in RStudio | [Run order](doc/guides/run-order.md), [scripts map](scripts/README.md) |
| Contribute changes | [Contributor guide](doc/guides/contributing.md) |
| Assess repository evidence and limitations | [Repository review, September 2026](doc/REPO_REVIEW.md), [review catalog](doc/reviews/README.md) |
| Study specification sensitivity (geographic support, IDW buffer and exponent) | [Supporting analysis guide](doc/RESOLUTION_SENSITIVITY.md) |
| Reproduce manuscript outputs | [Run and verify](doc/HOW_TO_RUN.md#manuscript-pipeline) |
| Run optional satellite comparisons | [Supporting analyses](doc/HOW_TO_RUN.md#supporting-analyses) |
| Develop or acquire inputs | [Development and acquisition](doc/HOW_TO_RUN.md#development-and-acquisition) |
| Record an isolated release check | [Isolated batch verification](doc/HOW_TO_RUN.md#isolated-batch-verification) |

## Repository layout

```text
src/                 Reusable R functions
scripts/             Recipes by stage and analysis (map in scripts/README.md)
data/downloads/      Preserved acquired source files (ignored; inputs of record)
data/raw/            Authorized original inputs (ignored; inputs of record)
data/interim/        Regenerated intermediate data, including prepared geography
data/processed/      Regenerated analytical products
results/             Regenerated figures and tables
config/paper_artifacts.csv  Manuscript artifact paths and producing scripts
doc/HOW_TO_RUN.md    Operational instructions and reproduction limits
doc/ai/              Shared guidance for coding assistants
tools/harness/       Optional Claude/Codex development-tooling policy
```

`src/` contains reusable logic; `scripts/` runs it. The targets graph (`_targets.R`, with
one module per analysis in `config/targets/`) calls the same functions in dependency order.

## Inputs and geography

External acquisition is a separate, online step. Keep authorized downloads and provider
responses under `data/downloads/`, with their requested year/version, retrieval date and
checksum. City processing then prepares the geographic layers it needs from those local
sources, with downloads disabled. A missing source must be acquired through the relevant
download script; a pre-existing generated layer is not a substitute for preparation.

The isolated verifier mounts `data/raw/`, `data/downloads/`, and `data/_legacy/` read-only
and disables runtime networking. This tests whether declared local inputs are sufficient;
it does not acquire data. Some sources have access restrictions or changing providers, so
availability and redistribution rights remain separate requirements.

MERRA-2 acquisition requires NASA Earthdata access. Configure credentials outside the
repository and follow the acquisition guidance in [doc/HOW_TO_RUN.md](doc/HOW_TO_RUN.md).
Do not commit credentials or source inputs.

## Containers

Three Compose files serve distinct uses:

| Command | Use |
|---|---|
| `docker compose up --build` | Interactive RStudio with live project code for development. |
| `docker compose --profile acquisition up --build` | Development plus Selenium for acquisition scripts that need a browser. |
| `docker compose -f docker-compose.release.yml up --build` | Interactive RStudio using code baked into the image, with source inputs read-only. |
| `Rscript tools/reproduction/verify.R --full` | Batch-only isolated verification with fresh derived outputs and no runtime network. |

`--build` brings the image up to date with the current code; without it, Compose reuses
the last image it built, however old. The containers keep their own targets store in
`_targets_docker/`.

**Memory.** The full manuscript run needs far more memory than Docker Desktop's default.
Before `verify.R --full`, give Docker as much as the computer can spare, leaving about
8 GB for the system (Docker Desktop → Settings → Resources → Memory; 48 GB on a 64 GB
machine is recommended). The IDW steps size DuckDB to whatever memory the machine or
container has (`idw_duckdb_mem_gb <- "auto"` in `config/analysis_settings.R`).
The verifier prints Docker's memory before it starts and the run's peak memory at the end;
see [machine memory](doc/HOW_TO_RUN.md#machine-memory).

Copy `.env.example` to `.env` only for interactive RStudio settings. The verification
container uses neither credentials nor Selenium. `renv.lock` controls R packages; image
identity, code revision, lockfile hash, installed versions, and source inventories provide
the environment evidence. There is no active dated package-repository setting.

## Manuscript pipeline

After authorized inputs have been acquired, run from the repository root:

```sh
Rscript scripts/run_pipeline.R
# or, in R:
source(here::here("scripts", "run_pipeline.R"))
```

Through the targets graph it builds city processing, the core analysis, station-only hourly
preparation, manuscript figures and tables, and the reported robustness checks; later
builds rerun only what changed. The appendix uses city-hour means of available observed metropolitan stations from the
current cleaned partitions. Missing hours break pollution episodes. MERRA-2 is optional.
To run the same recipes one at a time in RStudio, follow the
[run order](doc/guides/run-order.md).

Optional satellite extraction and comparisons run separately, after current station and
geography products and additional satellite/NASA inputs are available; their commands are
in [supporting analyses](doc/HOW_TO_RUN.md#supporting-analyses).

```mermaid
flowchart TD
  D[Online acquisition through existing functions] --> I[Preserved source inputs]
  I --> P[City processing: prepare geography, stations and census]
  P --> A[Distances, outliers, exposure and robustness]
  A --> F[Manuscript figures and tables]
  A --> H[Observed station city-hour summaries]
  H --> F
  H --> S[Optional satellite comparisons]
  N[Additional satellite and NASA inputs] --> S
  L[Legacy inputs] --> V[Separate legacy validation]
  B[Offline release verification] -. records and checks .-> P
  B -. compares and reviews .-> F
```

## Checks and export

```sh
Rscript tests/testthat.R --mode=synthetic
Rscript tools/reproduction/verify.R
Rscript tools/reproduction/verify.R --full
```

Passing tests or a completed run are evidence for the checks that actually ran; they do not
by themselves establish scientific or independent reproduction. The verifier records inputs,
outputs, environment details, failures, and comparison limitations. Export selected artifacts
with `tools/reproduction/export_paper.R`; see the operational guide for commands and constraints.

## Contributing and citation

Keep scientific specifications and inputs of record unchanged unless explicitly authorized.
Coding agents start at [AGENTS.md](AGENTS.md); its detail is in [doc/ai](doc/ai/README.md). The project
is released under the [MIT License](LICENSE.md); cite the repository and related publication
when reusing its methods or results.

Contact: Bridget Hoffmann, Inter-American Development Bank,
[bridgeth@iadb.org](mailto:bridgeth@iadb.org).
