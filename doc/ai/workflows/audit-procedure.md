# Audit a procedure: spec ↔ legacy ↔ new

Compare how the analytical procedure "procedure" is implemented in the **legacy** repo and the **new**
repo, and whether each matches the method described in the **paper draft**. Read-and-report only —
do not modify analysis code; surface findings and let Marcos decide.

## Gather the three inputs (ask me for any you cannot locate)

1. **Paper spec** — the relevant appendix/section of the draft. Read the local copy under
   `doc/paper/` (synced by the researcher; see [the human guide](../../guides/procedure-audit.md)). If it is not there,
   ask me for the section text or the file — don't guess the method.
2. **Legacy code** — the original script(s), which live **outside this repo**. They must be exposed
   via `--add-dir` or `permissions.additionalDirectories` (see the workflow doc). If you cannot read
   the path, stop and tell me exactly which file you need.
3. **New code** — the matching script/function here (for IDW:
   `scripts/process_data/estimate_idw.R` plus any `src/` helpers it calls).

Read all three fully before writing. Use the legacy reviewer role for independent review when the user requests delegation.

## Produce the report around these three goals

**Goal 1 — Do legacy and new match, and under what parameters?**
Find the parameter set under which both implementations give the same result. Present a table:
`parameter | legacy value | new value | matches?`. Cover at least: buffer radius, distance metric
and **units**, centroid vs representative-point method, missing-data handling / re-normalization,
weight exponent, CRS, and any rounding. State plainly the exact configuration needed for a match.

**Goal 2 — Does each match the paper's intended method? Any bugs?**
Walk both implementations against the spec's formulas step by step. Flag deviations from the paper
and any bugs (wrong denominator, missingness not re-normalized, distance in the wrong units,
representative point outside the polygon, silent NA drops, off-by-one). Cite `file:line` and rate
your confidence per finding.

**Goal 3 — Intentional deviations (legacy outdated vs new improved).**
Some mismatches are deliberate improvements — e.g. the new code uses `st_point_on_surface()` where
legacy used a plain centroid. For each: name the change, say which version the spec endorses, and
identify the **parameter/flag in the new code that reproduces the legacy (outdated) behavior**, so
the difference can be toggled and quantified in the Step 0-4 framework.

## Output

Use the [procedure report template](../../reviews/procedure-template.md). Record the audit
kind, workflow revision, input/code hashes, author/model/harness, date, actual checks,
review status, and limitations. Write a new report to
`doc/audits/procedures/<procedure>/<YYYY-MM-DD>-<run-id>.md`, where `<procedure>` is the
short slug and `<run-id>` distinguishes repeated runs. Never overwrite an earlier report.
Existing reports keep their historical paths; use the [catalog](../../reviews/README.md)
to find them. The report contains: a one-line verdict, the
Goal-1 parameter table, Goal-2 findings (grouped Blocker / Should-fix / Nit with
`file:line`), the Goal-3 deviation/toggle table, and a short TL;DR. Do not edit analysis
code. End by asking me which findings to act on.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
