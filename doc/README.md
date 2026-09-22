# Documentation

Start with the route that matches your task. Computational repeatability, numerical
agreement, scientific validity, and independent reproduction are separate claims.

| Reader or task | Route |
|---|---|
| Student / first encounter | [First run](guides/first-run.md) → [data dictionary](reference/data_dictionary.md) → [IDW worked example](reference/idw_golden_test.md) |
| Reviewer / replicator | [Run and verify](HOW_TO_RUN.md) → [artifact manifest](../config/paper_artifacts.csv) → [evidence and decisions](ai/evidence.md) |
| New contributor | [Contributing](guides/contributing.md) → [architecture](ai/architecture.md) → [shared workflows](ai/README.md) |
| Procedure audit | [Human guide](guides/procedure-audit.md) → [canonical workflow](ai/workflows/audit-procedure.md) → [report template](reviews/procedure-template.md) |
| Supporting research | [Geographic-resolution sensitivity](RESOLUTION_SENSITIVITY.md); optional, separate from manuscript production |
| Agent setup | [Git boundary](ai/rules/git-safety.md) → [harness setup](ai/harnesses.md) → [host enforcement](ai/host-enforcement.md) |
| Repository assessment | [Current review](REPO_REVIEW.md) → [review catalog](reviews/README.md) |

## Where documents belong

- `guides/`: instructions for learning or doing a task.
- `reference/`: definitions and worked analytical reference material.
- `planning/`: dated findings and proposals; inclusion is not implementation authorization.
- `reviews/`: public assessments, report templates, and preserved historical reviews.
- `ai/`: the sole canonical source of shared agent instructions, roles, and workflows.
- `audits/`: local reports, ignored except its README. See [audit storage](audits/README.md).
- `notes/`: local explanatory/research notes, ignored except its README.
- `paper/`: local manuscript sources, ignored except its README.

[Remaining work](planning/remaining-work.md), [deletion candidates](planning/deletion-candidates.md),
and the [targets proposal](planning/targets-migration.md) retain their original scope and
qualifications. Recheck their evidence before acting. Historical findings are not silently
promoted to current specifications.

`HOW_TO_RUN.md`, `REPO_REVIEW.md`, and `RESOLUTION_SENSITIVITY.md` retain stable entry paths.
Other former root pages retain anchor redirects. The [move ledger](reviews/document-moves.csv)
records source revisions and checksums. Local evidence links may be unavailable in a clone;
they are not a claim that restricted data or unpublished drafts are distributed.
