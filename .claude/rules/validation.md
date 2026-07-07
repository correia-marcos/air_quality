---
description: Legacy validation track — comparing new pipeline vs coauthor's original code
paths:
  - "scripts/validation_old_version/**"
  - "src/general_utilities/config_utils_validation_old_version.R"
  - "results/validation_old_version/**"
  - "**/*_report.qmd"
---

# Legacy validation

This track audits the new pipeline against the **legacy** code (the coauthor's original scripts
that produced the published numbers) and measures how results move as inputs are updated. It is
internal audit, not part of the paper pipeline — the `.dockerignore` excludes it from the image.

## What is / isn't tracked in git

- **Tracked:** comparison functions, the `compare_*.R` scripts, and the `*_report.qmd` reports.
- **Ignored:** every rendered artefact — `*.html`, `*_files/`, `*.parquet`, `*.csv`, `*.rds`,
  `*.gpkg`, and validation `*.pdf` maps under `results/validation_old_version/`.

So: edit *logic and reports*; treat *rendered outputs* as disposable and regenerate them by
running the script. Don't commit rendered artefacts.

## The Step 0-4 framework

Each city reports how its results change as inputs are updated one layer at a time. Keep the
steps additive and labelled exactly:

- **Step 0** — old data + old (assumed) metro-area definitions. Target: reproduce legacy numbers.
- **Step 1** — Step 0 + new legal metro-area definitions (where applicable).
- **Step 2** — Step 1 + new ground-station data (where applicable).
- **Step 3** — Step 2 + updated census microdata (where applicable).
- **Step 4** — Step 3 + corrected/updated processing code.

When a step doesn't apply to a city, say so explicitly in the report rather than skipping silently.
The point is to attribute every change in a result to a specific input, so isolate one layer per step.

## Conventions

- One entry-point script per city (`compare_<city>.R`) that runs each comparison in pipeline
  order (raw → interim → processed → results) and renders one self-contained `<city>_report.qmd`.
- Comparison helpers come from `config_utils_validation_old_version.R` (e.g. `build_compare_cfg`,
  `compare_ground_stations`, `compare_metro_area`). Reuse them; don't inline ad-hoc diffs.
- Artefacts for a city go under `results/validation_old_version/<city>/<data_type>/`.
- A comparison should quantify the difference (counts, summary stats, only-in-legacy /
  only-in-new sets), not just assert "matches" / "differs".

See `/validate-city` to scaffold or run a city comparison.
