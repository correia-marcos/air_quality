# doc/paper/ — local manuscript draft (NOT committed)

This folder holds a **local copy of the paper draft** used by the `/audit-procedure` workflow.
Its contents are intentionally git-ignored (see `.gitignore`): the manuscript never enters the
code repo's history. Only this README is tracked, so the convention travels with the repo.

Put the draft here in whichever way suits you:

- **Overleaf git bridge:** `git clone https://git@git.overleaf.com/<project-id> doc/paper`
  (username `git`, password = your Overleaf auth token). Because this folder is git-ignored, the
  nested clone is harmless — the outer repo won't try to track it. Run `git -C doc/paper pull`
  before an audit to refresh.
- **Manual export:** download the relevant `.tex` (e.g. the spatial-aggregation appendix) from
  Overleaf and drop it here.

See `doc/PROCEDURE_AUDIT_WORKFLOW.md` for the full workflow.
