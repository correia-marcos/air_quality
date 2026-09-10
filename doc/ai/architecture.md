# Project architecture and working agreement

Shared guidance for every coding assistant. Read the applicable rules explicitly.

## What this project is

Replication package for the paper *"Inequality in Air Pollution Monitoring and
Exposure: Evidence from Four Latin American Cities"* (Bogotá, Mexico City, Santiago,
São Paulo). R analytical pipeline with Docker and renv for environment control. Small Python/shell adapters support harness integration. Full reproduction must be established by an executed, recorded run; containerization alone does not guarantee it.

Two tracks live in one repo:

1. **Main pipeline** — download → process → results (figures/tables). Fully tracked in git.
2. **Legacy validation** — audits the new pipeline against the coauthor's original
   ("legacy") code associated with earlier numerical results, and quantifies how results change
   as data/definitions are updated (the Step 0–4 framework, see below). Partly git-ignored:
   validation *functions and reports* are tracked; rendered artefacts and data are not.

## The reader is a human, not just a machine

This is the single most important constraint. Economists, students, referees, and journal
editors must be able to clone this repo, open RStudio, run it, **inspect the intermediate
data frames, and understand what each function does to the data.** Optimize every choice for
that. Concretely:

- Prefer readable, linear, inspectable code over clever abstraction. A referee should be able
  to step through a script top-to-bottom and see the data change.
- Keep intermediate objects in the environment where it aids inspection; avoid deep pipe
  chains that hide the shape of the data.
- Write outputs to `data/interim/` and `data/processed/` as Parquet/RDS so they can be opened
  and checked independently.
- When you add a function, add a one-line comment saying what it does *to the data*.

## Repository layout

```text
src/                 Functions (logic). Sourced, never run directly.
  city_specific/      One module per city + registry.R (dispatch by city id)
  general_utilities/
    base_utils.R        The one copy of each shared helper. No packages, no side effects.
    setup_packages.R    ensure_installed() / attach_packages()
    theme_paper.R       set_paper_theme() — the only place graphics state is set
    config_utils_*.R    One thin loader per stage: pkgs vector + source() of its parts
    process/            merra2, distances, outliers, idw_exposure, geo_ids,
                        station_socio, imputation, diagnostics, exposure_regressions
    plot/               maps, timeseries_hourly, exposure_figures, latex_tables,
                        station_monitoring
    validation/         prepare_panels, compare_inputs, compare_results, progression
scripts/             Execution. Each script sources the src/ it needs, then runs.
  download_data/      Pull raw inputs (APIs, Selenium, Earthdata)
  process_data/       raw -> interim -> processed
  tables_images/      processed -> results (manuscript), results/figures, results/tables
  validation_old_version/  Legacy comparison + Quarto reports
  run_pipeline.R      Master orchestrator; Makefile also declares stage dependencies
data/                raw/ interim/ processed/ downloads/ _legacy/   (all git-ignored)
results/             Only figures/ (seven topic folders) and tables/ (flat).
config/paper_artifacts.csv  Canonical source -> unchanged manuscript path + producer.
data/validation/     Ignored legacy comparison artifacts and HTML.
```

Scripts are named for what they produce, never numbered: the run order lives in
`run_pipeline.R` and the dependencies in the `Makefile`. Both spell every basename as its own
literal string, so **a rename must update both by hand** — `tests/testthat/test-pipeline-paths.R`
fails if you forget.

`src/` holds functions; `scripts/` executes them. Never put runnable side-effects in `src/`.

## How to run

- **Development:** `docker compose up` starts live-code RStudio. Add
  `--profile acquisition` only when a download script needs Selenium.
- **Interactive image-baked code:** `docker compose -f docker-compose.release.yml up` mounts
  declared source inputs read-only and derived/output roots writable.
- **Manuscript pipeline:** `make all` or
  `source(here::here("scripts", "run_pipeline.R"))` runs the maintained manuscript stages,
  including preserved temporal preparation. `make merra2` is optional supporting analysis.
- **Isolated verification:** `Rscript scripts/verification/verify.R --full` rebuilds derived
  outputs with source mounts read-only and runtime networking disabled.
- **One stage:** run the relevant script in `scripts/`; each is self-contained via `here::here()`.
  Restore packages with `renv::restore()` when required. See `doc/HOW_TO_RUN.md` for the
  acquisition and evidence contract.

## Conventions (summary — full detail in `doc/ai/rules/`)

- **R only.** Stack: renv, Docker, DuckDB, Arrow, data.table, dplyr, sf/terra (geospatial).
- **Line length ≤ 90 characters.**
- **Comments: rationale lives in `@Description` / `@details`, once.** Comments inside a function
  body are ≤ 2 lines and say what happens *to the data* here, or point back to that block. Full
  rule (with the header/doc-block exemption) in `doc/ai/rules/r-style.md`.
- Every script starts with the standard header block (`@Goal / @Description / @Summary /
  @Date / @Author`), with `#'` on the `@tag` lines and plain `#` on continuations. `src/`
  function blocks use roxygen's `@param` / `@return` / `@details`. Match the existing style
  exactly — see any file in `scripts/process_data/`.
- Paths are **always** `here::here(...)`. Never hard-code absolute paths or `setwd()`.
- Packages are loaded through the stage's `config_utils_*.R`, not with scattered `library()` calls.
- New cities are added through `src/city_specific/registry.R`, not by copy-pasting scripts.

## Behavior guidelines (how I want you to work)

- **Think before coding.** Don't assume, don't hide confusion. State assumptions explicitly.
  If uncertain, say so with a confidence level and cite sources when it applies.
- **Surface trade-offs.** If multiple interpretations exist, present them — don't silently pick
  one. If a simpler approach exists, say so. Push back when warranted.
- Resolve routine questions by inspection. Ask when ambiguity changes scientific methods,
  source data, protected inputs, or authorized scope; continue independent work.
- **Don't add guardrails when I could just test instead.** If an output is uncertain, run an appropriately scoped check when authorized — don't wrap it in defensive code.
- **No "vibe-coded" bloat:** no abstractions for single-use code; no unrequested "flexibility" or
  configurability; no error handling for impossible scenarios. If 200 lines could be 50, rewrite it.
- **Be succinct** in code comments and in your replies. If there's a shorter way to say it, use it.
  When a reply carries a lot of concepts, end with a short TL;DR table/list so nothing is missed.
- **Never touch raw or legacy data.** `data/raw/`, `data/_legacy/`, and `renv.lock` are inputs of
  record. Don't edit or regenerate them without me explicitly asking. (Hooks supplement filesystem protections; they do not guarantee coverage.)
- **Secrets stay out of git.** `.Renviron`, `.netrc`, `.env` hold credentials — never read them into
  code output, never commit them.

## The validation Step 0–4 framework

The legacy track reports how each city's results shift as we update inputs, one layer at a time:

- **Step 0** — old data + old (assumed) metro-area definitions. Should reproduce legacy numbers.
- **Step 1** — new legal metro-area definitions (where applicable).
- **Step 2** — Step 1 + new ground-station data.
- **Step 3** — Step 2 + updated census microdata.
- **Step 4** — Step 3 + corrected/updated processing code.

Each city renders one self-contained Quarto report under
`data/validation/<city>/`. Details in `doc/ai/rules/validation.md`.

## Pointers

- `doc/ai/rules/` — R style, reproducibility (Docker/renv), validation, data-and-paths discipline.
- `doc/ai/workflows/` — `/new-process-script`, `/new-figure`, `/add-city`, `/validate-city`,
  `/reproduce`, `/review-r`.
- `doc/ai/roles/` — `r-reproducibility-reviewer`, `legacy-validation-auditor`.
- `doc/CLAUDE_CODE_SETUP.md` — what every piece of this setup is and why it exists.
- `doc/REMAINING_WORK.md` — which of the paper's figures and tables the default pipeline
  cannot yet produce, and why. Read this before assuming a missing figure is a bug.
- `doc/deletion_candidates.md` — functions with no caller, kept pending Marcos's decision.


Python/shell are permitted for harness adapters and local orchestration; analytical and
export logic remains R. Test fixtures may define helpers under tests/.
Use specialist subagents only for explicitly requested independent work.
