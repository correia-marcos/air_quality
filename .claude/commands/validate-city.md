---
description: Run or scaffold the legacy validation comparison + Quarto report for a city
argument-hint: '[city_id]'
---

# Validate a city against legacy

Work the legacy-validation track for `$1` (see `.claude/rules/validation.md`).

If `scripts/validation_old_version/compare_$1.R` **exists**: read it, confirm the `data/_legacy/`
inputs are present, then run it and render `results/validation_old_version/$1/$1_report.qmd`.
Summarize what matched, what differed, and by how much — never just "matches/differs".

If it **doesn't exist**: scaffold it, mirroring `compare_bogota.R`:
1. Setup: source `config_utils_validation_old_version.R`, `config_utils_process_data.R`,
   `registry.R`, and the `$1` module; build `cfg$compare <- build_compare_cfg("$1")`.
2. Run comparisons in pipeline order (raw ground stations → metro area → distances → outliers →
   census → results), reusing the existing `compare_*` helpers.
3. Report the **Step 0-4** deltas for $1; where a step doesn't apply, say so explicitly.
4. Render one self-contained `$1_report.qmd` HTML.

Reminder: rendered artefacts (`*.html`, `*.parquet`, `*_files/`) are git-ignored — regenerate,
don't commit them. Treat `data/_legacy/` as read-only.
