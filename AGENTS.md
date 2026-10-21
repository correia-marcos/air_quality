# Agent instructions

Replication package for *Inequality in Air Pollution Monitoring and Exposure: Evidence from
Four Latin American Cities* (Bogotá, Mexico City, Santiago, São Paulo). The analysis is R,
with Docker and renv controlling the environment; small Python and shell adapters serve the
coding harnesses only. This file is the shared core for every coding agent. Detailed rules
live in `doc/ai/`; the table at the end says when to read each one.

## The reader comes first

A reader must be able to open RStudio, run a few lines of a script (including sourcing
functions and reading files), inspect meaningful intermediate objects, identify the function
applied, and understand the analysis without first learning the pipeline's machinery.
This is the highest structural priority. It takes precedence over shorter scripts, removing
every repeated call, or abstracting orchestration. Readability is an acceptance criterion;
passing tests and heading counts do not establish it.

## Layout

| Path | Role |
|---|---|
| `src/` | Functions only. Sourcing a file never installs packages, acquires data or runs analysis |
| `src/city_specific/` | One module per city plus `registry.R`; add cities through the registry |
| `scripts/` | Recipes that produce the paper's results: download, process, tables and figures |
| `config/analysis_settings.R` | Shared analytical choices as plain named values |
| `_targets.R`, `config/targets/` | Candidate manuscript graph of ordinary `tar_target()` calls |
| `tools/` | Launchers and checks: reproduction, harness adapters, documentation links |
| `data/` | `downloads/ raw/ interim/ processed/ _legacy/`: git-ignored, one-way flow |
| `results/` | `figures/` and `tables/`; `config/paper_artifacts.csv` maps manuscript exports |
| `doc/` | Human documentation ([index](doc/README.md)); `doc/ai/` holds agent rules |

Scripts are named for what they produce, never numbered. Every script and report needs one
row in [remaining work](doc/planning/remaining-work.md), which the tests check; read it
before treating a missing figure as a bug. Repository structure, migration status and the
validation Step 0–4 framework are in [architecture](doc/ai/architecture.md).

## Run and check

- Portable checks: `Rscript tests/testthat.R --mode=synthetic`
- Harness adapters: `python3 -B -m unittest discover -s tests/harness`
- Documentation links: `python3 -B tools/docs/check_links.py`
- Isolated release verification: `Rscript tools/reproduction/verify.R --full`
  ([how to run](doc/HOW_TO_RUN.md))
- One stage: run its script in `scripts/`; each is self-contained through `here::here()`

A passing check is evidence for what it checked, not a reproduction claim.

## Writing R

Full rules: [R style](doc/ai/rules/r-style.md). The essentials:

- Every script opens with the standard header (`@Goal`, `@Description`, `@Summary`, `@Date`,
  `@Author`; `#'` on tag lines, plain `#` on continuations) and has three or four sections:
  I. Import data (sources, settings, every path, reads); II. Process data (named objects);
  III. Save outputs (saves only, in the same order).
- Put one short comment above each operation saying what happens to the data. A function's
  rationale goes once in its `@details`; comments inside `src/` function bodies stay short.
- Call scientific functions with named, aligned arguments and blank lines between steps.
  Keep every line within 92 characters.
- Recipes in `scripts/` read no command-line options. Variants are declared settings in
  `config/analysis_settings.R`; options belong to launchers (`scripts/run_*.R`, `tools/`).
- A wrapper performs one scientific operation a student could name in a sentence and
  returns one object the script keeps under one name. Never unpack a returned list into
  many top-level variables.
- Paths are always `here::here(...)`. Set a seed wherever there is randomness. Write
  intermediates as Parquet to `data/interim/` or `data/processed/`.

Exemplars: the Bogotá recipes (`scripts/download_data/download_bogota_data.R`,
`scripts/process_data/process_bogota_data.R`) for layout;
`scripts/process_data/generate_distance_matrices.R` for computing, then saving;
`scripts/process_data/compute_distance_band_descriptives.R` for comments;
[the IDW worked example](doc/reference/idw_golden_test.md) for explaining a method.

## Boundaries

- **Git is inspection-only.** Recommend commit subjects and exact file groupings; never
  stage, commit, push or rewrite history, directly or through another tool. See
  [Git safety](doc/ai/rules/git-safety.md).
- **Inputs of record are read-only:** `data/raw/`, `data/downloads/`, `data/_legacy/` and
  `renv.lock`. Never print or commit credentials (`.env`, `.Renviron`, `.netrc`).
- **Scientific specifications** (estimands, samples, definitions, source inputs and
  dependencies) change only with explicit authorization. Resolve routine questions by
  inspection; ask when an ambiguity would change methods, inputs or scope.
- **Refuse and explain.** When an action is denied or falls outside the boundary (network,
  Docker, deletion, writes outside the workspace), do not retry it or seek escalation.
  Explain what the command does and why it is needed, and give Marcos the exact command.
- Add no unrequested abstraction, configurability or defensive code; test instead.
- Use specialist agents only for explicitly requested independent work.

## How to work and when you are done

State assumptions and your confidence; present competing interpretations rather than
picking one silently; push back when a simpler approach exists. Keep replies succinct.

A task is done when you have reported the files changed, the commands actually run and their
results, checks skipped or failed, unresolved scientific decisions, and proposed commits
with explicit file lists.

## Where to read more

| When | Read |
|---|---|
| R, Quarto or R Markdown changes | [R style](doc/ai/rules/r-style.md) |
| Data, `src/` or `scripts/` | [Data and paths](doc/ai/rules/data-and-paths.md) |
| Docker, renv or credentials | [Reproducibility](doc/ai/rules/reproducibility.md) |
| Legacy comparisons | [Validation](doc/ai/rules/validation.md) |
| A repeated task | The matching file in [workflows](doc/ai/workflows/) |
| An independent review | The matching file in [roles](doc/ai/roles/) |
| Scientific decisions | [Evidence](doc/ai/evidence.md), [methods and tests](doc/ai/methods-tests.md) |
| Collaboration and handoff | [Collaboration](doc/ai/collaboration.md), [handoff](doc/ai/handoff.md) |
| Structure and migration status | [Architecture](doc/ai/architecture.md) |
| Harness setup and enforcement | [Harnesses](doc/ai/harnesses.md), [host](doc/ai/host-enforcement.md) |
| Implementation history | [Implementation record](doc/ai/implementation.md) |
| Functions with no caller | [Deletion candidates](doc/planning/deletion-candidates.md) |
