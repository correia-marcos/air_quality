# Local audit reports

Only this README is tracked. Report contents remain ignored and are not included in the
container image. A clean clone will not contain them. Public, reviewed excerpts belong in
`doc/reviews/` only after explicit disclosure review; agents recommend the human's commit.

New reports use these categories:

- `procedures/<slug>/<date>-<run-id>.md`: specification ↔ legacy ↔ current procedure audits.
- `repository/<date>-<topic>.md`: architecture, instructions, and process reviews.
- `reproducibility/<date>-<run-id>.md`: environment and execution assessments.
- `investigations/<date>-<topic>.md`: focused defects or data discrepancies.

Use the [report taxonomy and catalog](../reviews/README.md), and the
[procedure template](../reviews/procedure-template.md) where applicable. Record workflow
provenance explicitly; a three-goal format is not proof of a particular execution workflow.

Historical directories and filenames are preserved. Do not bulk-move, rewrite, delete, or
publish them. In particular, the existing `repo_structure/` mixes a container assessment
with a Codex setup guide, and `resolution_sesitivity_analysis/` contains supporting research.
Those records retain their original location; the catalog describes their purpose.

Explanatory/research notes should use [local notes](../notes/README.md) for new work.
Generated comparison artifacts remain under `data/validation/`, and verification outputs
under `data/verification/`; do not relocate those computational artifacts here.
