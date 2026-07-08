# Procedure-audit workflow

A repeatable way to run the check you do by hand today: take one procedure (e.g. IDW exposure),
and compare the **paper spec**, the **legacy code**, and the **new code** — without re-uploading
files each time. It removes the three things you re-send every session:

| You used to paste... | Now Claude reads it from... | Set up once via |
|---|---|---|
| The appendix / paper section | `doc/paper/` (local copy of the draft) | Overleaf git bridge (below) |
| The legacy scripts | the legacy repo folder, in place | `additionalDirectories` / `--add-dir` |
| The instructions + goals | the `/audit-procedure` command | already in `.claude/commands/` |

After the one-time setup, the whole task becomes: `/audit-procedure idw-exposure`.

## One-time setup

### 1. Expose the legacy repo (it lives outside this project)

Claude Code only sees the current repo by default. Grant read access to the legacy folder in one of
two ways:

- **Per session:** `claude --add-dir /path/to/legacy-air-monitoring-repo`
- **Always:** copy `.claude/settings.local.json.example` to `.claude/settings.local.json` and put the
  legacy path in `permissions.additionalDirectories`. This file is git-ignored, so the
  machine-specific path never gets committed. On first launch, accept the workspace-trust dialog
  (it lists the extra directories).

Keep the legacy repo read-only in practice — you're auditing it, not changing it.

### 2. Get the paper draft into `doc/paper/` (from Overleaf)

Overleaf's **git integration** (a premium feature) lets you clone the project like any git remote:

1. In Overleaf: **Menu → Git** to get the clone URL (`https://git@git.overleaf.com/<project-id>`).
2. In **Account Settings**, create a Git authentication **token**. Username is `git`, password is the
   token. Treat the token like a password — never commit it.
3. Clone once, then pull to refresh before an audit:
   ```bash
   git clone https://git@git.overleaf.com/<project-id> doc/paper     # first time
   git -C doc/paper pull                                             # before each audit
   ```
   Cloning directly into `doc/paper/` is safe: that folder is git-ignored (below), so the nested
   clone never gets staged by the outer repo.

No git access? Just **download the relevant `.tex`** (e.g. the spatial-aggregation appendix) from
Overleaf and drop it in `doc/paper/`. The command only needs the section text, not the whole project.

**The draft is never committed.** `doc/paper/*` is git-ignored (only `doc/paper/README.md` is
tracked), so the manuscript stays local and out of the code repo's history — regardless of whether
you clone or copy it in.

## Running an audit

```
/audit-procedure idw-exposure
```

The command (backed by the `legacy-validation-auditor` subagent in method-audit mode) reads the
three inputs and writes `doc/audits/idw-exposure.md` answering:

1. **Match + parameters** — the exact parameter set under which legacy and new agree, as a table.
2. **Correctness + bugs** — each implementation checked against the paper's formulas, with
   `file:line` and confidence.
3. **Intentional deviations** — where new deliberately improved on legacy, and the flag in the new
   code that reproduces the old behavior (so it slots into the Step 0-4 framework).

It reports only — it won't edit analysis code. You decide what to act on.

## Worked example: IDW exposure

The inputs for `idw-exposure` map like this:

- **Spec:** the "Spatial Aggregation of Air Quality Data" appendix (new draft) and the older IDW
  appendix (both in `doc/paper/`).
- **Legacy:** `4_exposure_plots_3km_quintiles_2023_regCI.R` in the legacy repo.
- **New:** `scripts/process_data/estimate_idw_exposure.R` (+ `src/` helpers it sources).

A known deviation to expect the audit to surface under Goal 3: the new spec uses
`sf::st_point_on_surface()` for the representative point, while the legacy code used a plain
polygon **centroid**. The audit should name that, confirm which the current spec endorses, and point
to the parameter in the new code that switches back to a centroid — the toggle you'd use to measure
how much that one change moves the results. Other parameters to pin for an exact match: buffer radius
(baseline 3 km), distance **units**, the weight exponent (spec: `1/d`), missingness re-normalization,
and CRS.

## Tips

- **One procedure per session.** Start fresh (`/clear`) between audits — these audits read a lot of
  code, and accuracy degrades in long sessions.
- **Let the subagent do the heavy read**, then you verify the numbers and formulas yourself. The
  audit is a draft argument, not a verdict — you're still the referee.
- **Pull `doc/paper/` before each run** so you're auditing against the current draft.

## TL;DR

| Step | Do it |
|---|---|
| Expose legacy repo | `--add-dir` or `additionalDirectories` in `settings.local.json` |
| Get the draft locally | Overleaf git bridge → `doc/paper/` (or download the `.tex`) |
| Run the audit | `/audit-procedure <procedure>` → report in `doc/audits/` |
| Review | Check the parameter table, bugs, and the legacy-toggle yourself |
