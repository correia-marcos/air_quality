# CLAUDE.md

Guidance for Claude Code when working in this repository. Keep this file short;
detailed rules live in `.claude/rules/` and load only when relevant files are touched.

## What this project is

Replication package for the paper *"Inequality in Air Pollution Monitoring and
Exposure: Evidence from Four Latin American Cities"* (Bogotá, Mexico City, Santiago,
São Paulo). R-only, fully containerized (Docker + renv) for bit-for-bit reproducibility.

Two tracks live in one repo:

1. **Main pipeline** — download → process → results (figures/tables). Fully tracked in git.
2. **Legacy validation** — audits the new pipeline against the coauthor's original
   ("legacy") code that produced the published numbers, and quantifies how results change
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

```
src/                 Functions (logic). Sourced, never run directly.
  city_specific/      One module per city + registry.R (dispatch by city id)
  general_utilities/  config_utils_*.R: load packages + shared helpers per stage
scripts/             Execution. Each script sources the src/ it needs, then runs.
  download_data/      Pull raw inputs (APIs, Selenium, Earthdata)
  process_data/       raw -> interim -> processed
  tables_images/      processed -> results/figures, results/tables
  validation_old_version/  Legacy comparison + Quarto reports
  run_pipeline.R      Master orchestrator (source in order, top to bottom)
data/                raw/ interim/ processed/ downloads/ _legacy/   (all git-ignored)
results/             figures/ tables/ validation_old_version/
```

`src/` holds functions; `scripts/` executes them. Never put runnable side-effects in `src/`.

## How to run

- **Preferred (reproducible):** `docker compose up` → open RStudio at `http://localhost:8787`
  (user/pass from `.env`, copied from `.env.example`). See `README.md` §3–4.
- **Full pipeline:** open `Coding.Rproj`, then `source(here::here("scripts","run_pipeline.R"))`.
  Download steps are commented out by default (large); enable only when re-fetching raw data.
- **One stage:** run the relevant script in `scripts/`; each is self-contained via `here::here()`.
- Packages are pinned with **renv**. If something is missing, run `renv::restore()`.

## Conventions (summary — full detail in `.claude/rules/`)

- **R only.** Stack: renv, Docker, DuckDB, Arrow, data.table, dplyr, sf/terra (geospatial).
- **Line length ≤ 90 characters.**
- **Comment roughly every ~4 lines** where it aids understanding — say *why*, and what happens
  *to the data*, not what the syntax does.
- Every script starts with the standard header block (`@Goal / @Description / @Summary /
  @Date / @Author`). Match the existing style exactly — see any file in `scripts/process_data/`.
- Paths are **always** `here::here(...)`. Never hard-code absolute paths or `setwd()`.
- Packages are loaded through the stage's `config_utils_*.R`, not with scattered `library()` calls.
- New cities are added through `src/city_specific/registry.R`, not by copy-pasting scripts.

## Behavior guidelines (how I want you to work)

- **Think before coding.** Don't assume, don't hide confusion. State assumptions explicitly.
  If uncertain, say so with a confidence level and cite sources when it applies.
- **Surface trade-offs.** If multiple interpretations exist, present them — don't silently pick
  one. If a simpler approach exists, say so. Push back when warranted.
- **If something is unclear, stop, name what's confusing, and ask.**
- **Don't add guardrails when I could just test instead.** If you're unsure what an output will
  look like, ask me to run/check something before you proceed — don't wrap it in defensive code.
- **No "vibe-coded" bloat:** no abstractions for single-use code; no unrequested "flexibility" or
  configurability; no error handling for impossible scenarios. If 200 lines could be 50, rewrite it.
- **Be succinct** in code comments and in your replies. If there's a shorter way to say it, use it.
  When a reply carries a lot of concepts, end with a short TL;DR table/list so nothing is missed.
- **Never touch raw or legacy data.** `data/raw/`, `data/_legacy/`, and `renv.lock` are inputs of
  record. Don't edit or regenerate them without me explicitly asking. (A hook will ask you to confirm.)
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
`results/validation_old_version/<city>/`. Details in `.claude/rules/validation.md`.

## Pointers

- `.claude/rules/` — R style, reproducibility (Docker/renv), validation, data-and-paths discipline.
- `.claude/commands/` — `/new-process-script`, `/new-figure`, `/add-city`, `/validate-city`,
  `/reproduce`, `/review-r`.
- `.claude/agents/` — `r-reproducibility-reviewer`, `legacy-validation-auditor`.
- `doc/CLAUDE_CODE_SETUP.md` — what every piece of this setup is and why it exists.
