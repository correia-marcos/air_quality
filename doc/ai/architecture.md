# Project architecture and working agreement

Shared guidance for every coding assistant. Read the applicable rules explicitly.

## The reader is a human, not just a machine

A reader must be able to open RStudio, execute a few lines of a script—including
sourcing functions and reading files—inspect meaningful intermediate objects, identify
the function applied, and understand the analysis without first learning the pipeline's
internal machinery.

This is the highest structural priority. It takes precedence over minimizing script
length, eliminating every repeated function call, or abstracting orchestration.
Scientific definitions and protected inputs remain unchanged.

- Analytical scripts are executable recipes with three or four meaningful sections.
- Section I shows sources, settings, all input/output paths and reads. Section II computes
  named objects; Section III only saves them in the same order. Optional inspection belongs
  in the guide and reuses returned objects, without a separate script inspection block.
- Keep tables, spatial objects, estimates and plots in named objects where useful.
- A script must run from declared files and sourced functions without a targets cache.
- Shared functions define transformations; targets alone will schedule the accepted
  manuscript pipeline. Repeating short calls in an interactive recipe is acceptable.
- Every new script, function or structural change must identify the objects a reader
  can inspect and the function that transforms them. Readability is an acceptance
  criterion, not something inferred from heading counts or passing automated tests.
- Write inspectable intermediate files to data/interim/ and data/processed/.

Use `scripts/process_data/generate_distance_matrices.R` as the structural reference and
`doc/reference/idw_golden_test.md` as the model for explanations. Explicit calls with named
arguments and blank lines are preferable to specification tables and nested adapters.
Shared choices live in `config/analysis_settings.R`; city definitions stay in city modules.
Marcos accepted the distance pilot's readability on 23 September 2026. Use his edited
recipe and the [R style guidance](rules/r-style.md#spacing-calls-and-comments) for the
remaining migration. Scientific acceptance and default cutover remain separate.

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

## Repository layout

```text
src/                 Functions (logic). Sourced, never run directly.
  city_specific/      One module per city + registry.R (dispatch by city id)
                       processing.R input/output contracts; preparation.R offline stages
  pipeline/           Transitional loader/adapters awaiting the remaining migration
    stages/            Extracted logic to move into subject modules
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
  run_pipeline.R      Transitional sequential runner, pending scientific acceptance
  run_targets.R       Candidate incremental manuscript runner; explicit stage selection
_targets.R           Candidate manuscript graph with explicit target declarations
config/analysis_settings.R  Plain named choices shared by migrated scripts and targets
data/                raw/ interim/ processed/ downloads/ _legacy/   (all git-ignored)
results/             Only figures/ (seven topic folders) and tables/ (flat).
config/paper_artifacts.csv  Canonical source -> unchanged manuscript path + producer.
data/validation/     Ignored legacy comparison artifacts and HTML.
```

Scripts are named for what they produce, never numbered. The complete entry-point inventory
is `doc/planning/remaining-work.md`; tests require exactly one row per script/report.
The candidate graph uses ordinary `tar_target()` declarations. Until acceptance, the
Makefile and sequential runner remain transitional and their paths must also stay valid.
After acceptance, remove Makefile and its RStudio build setting; make `run_pipeline.R`
a compatibility launcher for targets. Optional analyses remain explicit separate commands.

The distance and four city recipes call scientific functions directly. Other recipes need
simplification; their heading counts do not establish readability. The migration candidate
shares transformation functions with the interactive recipes. The new
graph declares concrete city functions, configurations and upstream file targets; it does not
dispatch an opaque `city_process(id)` target. `city_process()` remains a convenience dispatcher, running prerequisites in order.
Reader-facing city scripts expose source paths, geographic objects, selected stations and
census output paths. Bogotá's geographic preparation consumes explicit local archives and
returns `sf` objects; acquisition is a separate function. Other cities still use the
transitional `allow_download = FALSE`, `out_file = NULL` interface. Section III saves spatial
objects with `write_geopackage()`. Large census calls use `return_data = FALSE`
and expose saved paths rather than retain individual records. Registry configuration
remains available to validation.
All four wrappers prepare geography, stations, partitioned pollution, and all supported census
variants. Acquisition remains separate. `read_raw` and `normalize` registry slots were removed.

One target owns each complete station dataset and city/vintage IDW family. File targets retain
Parquet directories; consumers reopen data. File targets discard vector names, so manuscript
city commands select explicit filenames. Named stage contracts remain in the manual
`city_process()` convenience interface. Targets caches the spatial computations separately
from their writers, and sources definitions/settings into its own environment.
Whole source directories track membership and sidecars.
The default runner changes only after isolated scientific acceptance; see the development
record in `doc/planning/targets-migration.md` (excluded from runtime images).

`src/` defines functions and configuration; `scripts/` executes the analysis. Modules may
attach their required installed packages at the top, as in `bogota.R`. Sourcing a module
must not install packages, acquire data or run an analysis.

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
- **Migration candidate:** `make targets STAGE=bogota_geography` runs the preparation pilot;
  `make targets` selects manuscript export and its prerequisites. `make targets-outdated`
  inspects that selection. `Rscript scripts/verification/verify.R --full --targets` requests
  a fresh isolated targets store and records target metadata. These commands require restored
  dependencies and preserved inputs; they do not establish acceptance merely by existing.
- **One stage:** run the relevant script in `scripts/`; each is self-contained via `here::here()`.
  Restore packages with `renv::restore()` when required. See `doc/HOW_TO_RUN.md` for the
  acquisition and evidence contract.

## Conventions (summary — full detail in `doc/ai/rules/`)

- **R only.** Stack: renv, Docker, DuckDB, Arrow, data.table, dplyr, sf/terra (geospatial).
- **R layout:** follow the author-edited distance recipe and shared R style. The maximum
  line length is 90 characters; use shorter indentation or wrap arguments to fit.
- **Comments: rationale lives in `@Description` / `@details`, once.** Comments inside a function
  body are ≤ 2 lines and say what happens *to the data* here, or point back to that block. Full
  rule (with the header/doc-block exemption) in `doc/ai/rules/r-style.md`.
- Every script starts with the standard header block (`@Goal / @Description / @Summary /
  @Date / @Author`), with `#'` on the `@tag` lines and plain `#` on continuations. `src/`
  function blocks use roxygen's `@param` / `@return` / `@details`. Match the existing style
  exactly — see any file in `scripts/process_data/`.
- Paths are **always** `here::here(...)`. Never hard-code absolute paths or `setwd()`.
- Scripts source required subject modules and plain settings in Section I. Modules may
  attach their own package requirements. Do not require a targets loader or install packages
  during analysis. Existing broad setup loaders are transitional, not new-script templates.
- New cities are added through `src/city_specific/registry.R`, not by copy-pasting scripts.

## Behavior guidelines (how I want you to work)

- **Git recommendations only.** Follow [Git safety](rules/git-safety.md): inspect and
  recommend file groupings and commit names; never stage, commit, or push.
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
- `doc/README.md` — routes for students, reviewers, and contributors.
- `doc/ai/harnesses.md` — client setup and its verified boundaries.
- `doc/planning/remaining-work.md` — which of the paper's figures and tables the default pipeline
  cannot yet produce, and why. Read this before assuming a missing figure is a bug.
- `doc/planning/deletion-candidates.md` — functions with no caller, kept pending Marcos's decision.


Python/shell are permitted for harness adapters and local orchestration; analytical and
export logic remains R. Test fixtures may define helpers under tests/.
Use specialist subagents only for explicitly requested independent work.
