# Reviews and audit catalog

[Documentation index](../README.md) · [Current repository review](../REPO_REVIEW.md)

## Report types

Classify reports by their method and purpose. Record review status separately: an audit
produced by a workflow may still contain unreviewed or unresolved findings.

| Kind | Evidence required | New report location |
|---|---|---|
| Procedure audit | Paper specification, legacy and current code; parameter match, deviations, workflow revision and actual checks | Local `doc/audits/procedures/<slug>/<date>-<run-id>.md` |
| Repository review | Dated repository snapshot, file/line evidence, architecture and process assessment | Sanitized public `doc/reviews/repository/`; local details in `doc/audits/repository/` |
| Reproducibility audit | Declared inputs, environment, commands, output comparisons and limitations | Local `doc/audits/reproducibility/`; machine evidence remains under `data/verification/` |
| Focused investigation | A specific suspected defect or provenance question, bounded evidence and disposition | Local `doc/audits/investigations/` |
| Research note / explanation | Reasoning, exploratory results or teaching material; no implied audit verdict | Local `doc/notes/`; reviewed public material in `doc/reference/` or an existing stable guide |

A three-way comparison is a procedure audit by method. Label it **workflow-produced** only
when its provenance identifies the workflow and version actually followed. Existing report
names, folders, or model names do not prove that provenance. Use
[the procedure template](procedure-template.md) for new reports. Missing historical metadata
stays unknown; do not fill it retrospectively by inference.

## Historical material

The following exact copies preserve what earlier documents said. They are historical
sources, not current instructions. Relative links inside them refer to their original
locations and are excluded from the current-navigation check. The
[move ledger](document-moves.csv) records their original paths, revision, and checksums.

| Date / document | Purpose and limitation |
|---|---|
| [8 August 2026 repository review](repository/2026-08-08.md) | Original assessment; claims about absent tests and strong hook/container guarantees are outdated or overstated. See the current review. |
| [Claude setup before this reorganization](repository/claude-setup-before-2026-09-21.md) | Historical setup description; current policy lives in `doc/ai/`. |
| [Earlier procedure guide](repository/procedure-guide-before-2026-09-21.md) | Historical human guide; current route is [procedure audits](../guides/procedure-audit.md). |

Local audit families already include `idw`, `distance_matrices`, `outlier_procedure`,
`exposure_regressions`, `census_processing`, `geo_ids`, `legacy_provenance`, `repo_structure`,
and `resolution_sesitivity_analysis` (historical spelling). These are discovery hints,
not certifications that every contained report followed an audit procedure. Existing
files stay at their original paths; no bulk renaming or reclassification is necessary.
They may be absent from a fresh clone. See [local storage](../audits/README.md).

## Promoting a finding

Record the exact claim, original input/code revisions, check results, current disposition,
named reviewer, review date, and remaining uncertainty in [the evidence index](../ai/evidence.md).
Preserve the original report and link any replacement. Only reviewed, sanitized material
belongs in the public catalog. A human handles Git staging, commits, and pushes.
