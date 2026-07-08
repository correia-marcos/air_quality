---
description: Three-way audit of one procedure — paper spec vs legacy code vs new code — with a parameter-match analysis
argument-hint: "[procedure] e.g. idw-exposure"
---

# Audit a procedure: spec ↔ legacy ↔ new

Compare how the analytical procedure "$1" is implemented in the **legacy** repo and the **new**
repo, and whether each matches the method described in the **paper draft**. Read-and-report only —
do not modify analysis code; surface findings and let Marcos decide.

## Gather the three inputs (ask me for any you cannot locate)

1. **Paper spec** — the relevant appendix/section of the draft. Read the local copy under
   `doc/paper/` (synced from Overleaf; see `doc/PROCEDURE_AUDIT_WORKFLOW.md`). If it is not there,
   ask me for the section text or the file — don't guess the method.
2. **Legacy code** — the original script(s), which live **outside this repo**. They must be exposed
   via `--add-dir` or `permissions.additionalDirectories` (see the workflow doc). If you cannot read
   the path, stop and tell me exactly which file you need.
3. **New code** — the matching script/function here (for IDW:
   `scripts/process_data/estimate_idw_exposure.R` plus any `src/` helpers it calls).

Read all three fully before writing. For large files, delegate the deep read to the
`legacy-validation-auditor` subagent (method-audit mode).

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

Write to `doc/audits/$1.md`: a one-line verdict, the Goal-1 parameter table, Goal-2 findings
(grouped Blocker / Should-fix / Nit with `file:line`), the Goal-3 deviation/toggle table, and a
short TL;DR. Do not edit analysis code. End by asking me which findings to act on.
