# Validate a city against legacy

Work the legacy-validation track for `city_id` (see `doc/ai/rules/validation.md`).

If `scripts/validation_old_version/compare_city_id.R` **exists**: read it, confirm the `data/_legacy/`
inputs are present, then run it and render `scripts/validation_old_version/city_id_report.qmd` to `data/validation/city_id/`.
Summarize what matched, what differed, and by how much — never just "matches/differs".

If it **doesn't exist**: scaffold it, mirroring `compare_bogota.R`:
1. Setup: source `config_utils_validation_old_version.R`, `config_utils_process_data.R`,
   `registry.R`, and the `city_id` module; build `cfg$compare <- build_compare_cfg("city_id")`.
2. Run comparisons in pipeline order (raw ground stations → metro area → distances → outliers →
   census → results), reusing the existing `compare_*` helpers.
3. Report the **Step 0-4** deltas for city_id; where a step doesn't apply, say so explicitly.
4. Render one self-contained `city_id_report.qmd` HTML.

Reminder: rendered artefacts (`*.html`, `*.parquet`, `*_files/`) are git-ignored — regenerate,
don't commit them. Treat `data/_legacy/` as read-only.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
