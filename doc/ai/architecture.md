# Repository architecture and migration status

Descriptive reference for contributors and agents. Working rules for agents are in
[AGENTS.md](../../AGENTS.md); R conventions are in [R style](rules/r-style.md).

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
src/                    - Functions (logic). Sourced, never run directly.
  city_specific/           - One module per city + registry.R (dispatch by city id)
                           processing.R input/output contracts; preparation.R
  general_utilities/       - Folder containing all non specific functions and utilities
    base_utils.R             - The one copy of each shared helper. No packages, no side effects.
    setup_packages.R         ensure_installed() / attach_packages()
    theme_paper.R            - set_paper_theme() — the only place graphics state is set
    config_utils_*.R         - One thin loader per stage: pkgs vector + source() of its parts
    process/                   - merra2, distances, outliers, idw_exposure, geo_ids, 
                               station_socio, imputation, diagnostics, exposure_regressions
    plot/                      - maps, timeseries_hourly, exposure_figures, latex_tables,
                               station_monitoring
    validation/                - prepare_panels, compare_inputs, compare_results, progression
  
scripts/                - Execution. Each script sources the src/ it needs, then runs.
  run_pipeline.R           - Transitional sequential runner, pending scientific acceptance
  run_targets.R            - Candidate incremental manuscript runner; explicit stage selection
  download_data/             - Pull raw inputs (APIs, Selenium, Earthdata)
  process_data/              - raw -> interim -> processed
  tables_images/             - processed -> results (manuscript), results/figures, results/tables
  validation_old_version/    - Legacy comparison + Quarto reports

config/                 - Information. Source of rules and definitions
  analysis_settings.R      - Plain named choices shared by scripts and targets
  paper_artifacts.csv      - Canonical source -> unchanged manuscript path + producer.
  targets/                   - Ordinary target declarations grouped by scientific stage

data/                   - raw/ interim/ processed/ downloads/ _legacy/   (all git-ignored)
  validation/              - Ignored legacy comparison artifacts and HTML.

results/                - figures/ (seven topics plus the pilot); tables/ (flat except the pilot).

_targets.R              - Small candidate manuscript graph entry point
tools/reproduction/     - Verification, stage reporting, manifest checks and export
```

Layout pilot (October 2026): the optional specification-sensitivity exercise groups its
scripts, functions and outputs in `specification_sensitivity/` subfolders of each stage,
with results by role in `results/{tables,figures}/specification_sensitivity/`.
Its file map is `scripts/process_data/specification_sensitivity/README.md`. Repo-wide
adoption is a separate plan; other files keep the flat layout until then.

Scripts are named for what they produce, never numbered. The complete entry-point inventory
is `doc/planning/remaining-work.md`; tests require exactly one row per script/report.
The candidate graph combines literal target lists from `config/targets/` using base
`source(..., local = TRUE)$value`. Scientific logic stays in `src/`; declaration modules
show names, dependencies and output paths. Explicit common package defaults replace
incidental package attachment. Station temporal preparation uses current observed cleaned
partitions, never optional satellite inputs. PDF writers preserve explicit rendering options.
Worker font/theme setup remains where plots are built or rendered.
The candidate graph uses ordinary `tar_target()` declarations. Until acceptance, the
Makefile and sequential runner remain transitional and their paths must also stay valid.
After acceptance, remove Makefile and its RStudio build setting; make `run_pipeline.R`
a compatibility launcher for targets. Optional analyses remain explicit separate commands.

The distance, city, outlier, IDW, observed/imputed-exposure and hourly-imputation recipes
call scientific functions directly. Imputation diagnostics expose station summaries and plots
separately from PDF saving. The four table recipes retain named LaTeX objects, and exposure
figures retain named plot lists; both save in Section III. Other recipes need simplification;
heading counts do not establish readability.
Distance-band descriptions also expose area tables, four city summaries and the combined
table before saving. Targets caches each city's summary and writes the two formats separately
from computation, using the same scientific functions and shared indicator settings.
The broader descriptive recipe also exposes each city's missingness, station counts,
WHO comparisons, hourly thresholds, education-quintile availability and census summaries.
Separate writers report all 41 files; table-rendering targets consume the required families.
The migration candidate
shares transformation functions with the interactive recipes. The new
graph declares concrete city functions, configurations and upstream file targets; it does not
dispatch an opaque `city_process(id)` target. `city_process()` remains a convenience dispatcher, running prerequisites in order.
Reader-facing city scripts expose source paths, geographic objects, selected stations and
census output paths. Bogotá's geographic preparation consumes explicit local archives and
returns `sf` objects; acquisition is a separate function. The other three cities now use
the same acquisition/preparation separation. Section III saves spatial
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
Observed exposure estimates and their writers are separate targets. Individual census/group
files stay file-backed; consumers read them explicitly. Outlier datasets retain all years,
and each IDW city/vintage target serializes all buffers and income reuse. Shared year,
buffer and distance-power choices are in `config/analysis_settings.R`.
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
  including station-only hourly preparation. `make merra2` is optional supporting analysis.
- **Isolated verification:** `Rscript tools/reproduction/verify.R --full` rebuilds derived
  outputs with source mounts read-only and runtime networking disabled.
- **Migration candidate:** `make targets STAGE=bogota_geography` runs the preparation pilot;
  `make targets` selects manuscript export and its prerequisites. `make targets-outdated`
  inspects that selection. `Rscript tools/reproduction/verify.R --full --targets` requests
  a fresh isolated targets store and records target metadata. These commands require restored
  dependencies and preserved inputs; they do not establish acceptance merely by existing.
- **One stage:** run the relevant script in `scripts/`; each is self-contained via `here::here()`.
  Restore packages with `renv::restore()` when required. See `doc/HOW_TO_RUN.md` for the
  acquisition and evidence contract.

## The validation Step 0–4 framework

The legacy track reports how each city's results shift as we update inputs, one layer at a time:

- **Step 0** — old data + old (assumed) metro-area definitions. Should reproduce legacy numbers.
- **Step 1** — new legal metro-area definitions (where applicable).
- **Step 2** — Step 1 + new ground-station data.
- **Step 3** — Step 2 + updated census microdata.
- **Step 4** — Step 3 + corrected/updated processing code.

Each city renders one self-contained Quarto report under
`data/validation/<city>/`. Details in `doc/ai/rules/validation.md`.
