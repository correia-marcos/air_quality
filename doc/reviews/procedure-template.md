# Procedure audit template

Copy this structure into a new local report using the
[canonical workflow](../ai/workflows/audit-procedure.md). Unknown fields stay unknown.

## Record

- Report ID / date / procedure:
- Audit kind: procedure
- Workflow: `doc/ai/workflows/audit-procedure.md`, revision/hash:
- Paper specification: version/hash and relevant section:
- Legacy code: revision/hash and files:
- Current code: revision/hash, files, and uncommitted changes:
- Author, model/version, harness/version:
- Checks actually executed, outcomes, and evidence locations:
- Researcher review: pending / reviewer and date:
- Disclosure/access restrictions:

## Verdict and scope

State what the evidence establishes, what was not checked, and the scope of any agreement.

## Goal 1 — Parameter match

| Parameter | Legacy | Current | Match / required configuration | Evidence |
|---|---|---|---|---|

Include units, radius, CRS, representative point, missingness, weight exponent, and rounding
where applicable. Do not claim numerical equivalence from code inspection alone.

## Goal 2 — Specification and defects

| Finding ID / severity | Claim | Revision and file:line | Evidence / counterexample | Confidence / limitation |
|---|---|---|---|---|

Separate defects from unresolved scientific choices. Preserve the original case before
marking a finding fixed. Use the [existing status vocabulary](../ai/evidence.md).

## Goal 3 — Intentional deviations

| Difference | Specification authority | Existing legacy switch or absent | Step 0–4 implication | Limitation |
|---|---|---|---|---|

## Handoff

List unresolved decisions, next checks, and proposed actions. Report-only work ends here;
implementation requires scope authorization. Recommend human-created commits only.
