# Audit one analytical procedure

A procedure audit compares the paper specification, legacy implementation, and current
implementation. It is distinct from a container check, repository review, or focused defect
investigation. See the [taxonomy](../reviews/README.md#report-types).

The [shared workflow](../ai/workflows/audit-procedure.md) is canonical for every harness.
Its three goals remain parameter matching, specification correctness, and documented
intentional deviations. An audit reports findings; it does not authorize analytical edits.

## Prepare the inputs

Supply the relevant paper section under `doc/paper/`, read access to the external legacy
scripts, and the matching current functions/scripts. Record the paper version/hash and
both code revisions (or content hashes when a revision is unavailable). Do not substitute
a current paper draft for the historically reviewed specification without saying so.

Humans may export the relevant TeX from Overleaf or synchronize a separate manuscript
checkout. `doc/paper/` already contains a tracked README, so cloning directly into that
nonempty directory is not a reliable setup command. Keep manuscript synchronization outside
agent work; never put tokens in commands or tracked files. See [manuscript storage](../paper/README.md).

Additional-directory access is not itself a read-only filesystem guarantee. Protect external
legacy inputs at the host level and do not modify them during an audit. If a required input
is missing, request it and report the missing evidence; do not guess the intended method.

## Run and report

In Claude, invoke `/audit-procedure <procedure>`. In Codex, invoke the `audit-procedure`
skill with the named procedure, or read the canonical workflow directly. Independent
subagent review occurs only when explicitly requested. A reviewer returns findings to the
parent; report writing and analytical execution have their own authorized scope.

Use [the report template](../reviews/procedure-template.md). New local reports go to
`doc/audits/procedures/<slug>/<YYYY-MM-DD>-<run-id>.md`; use a unique run identifier and
never overwrite an earlier report. Existing reports remain at their historical paths.
The filename or model name alone does not establish which workflow produced a report.

For IDW, record buffer radius, distance units, weight exponent, representative-point
method, CRS, and missingness renormalization. The historical worked example discussed
centroids versus `st_point_on_surface()`; verify the actual specification and available
parameters for the revision under review. If a legacy switch does not exist, report its
absence instead of inventing or implementing one.

Do not equate a newer input with a better specification. Preserve the
[Step 0–4 qualifications](../ai/rules/validation.md#the-step-0-4-framework), including the
Santiago spatial-resolution trade-off. Publish only explicitly reviewed, sanitized excerpts;
the human performs any Git staging, commit, and push.
