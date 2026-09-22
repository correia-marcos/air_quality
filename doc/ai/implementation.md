# Implementation and evidence

## Redirect cleanup — 22 September 2026

Implemented from clean baseline `4c063cb`, after user authorization. Removed the seven
root redirect pages: `CLAUDE_CODE_SETUP.md`, `PROCEDURE_AUDIT_WORKFLOW.md`,
`REMAINING_WORK.md`, `TARGETS_MIGRATION_PLAN.md`, `data_dictionary.md`,
`deletion_candidates.md`, and `idw_golden_test.md`. Maintained navigation already used
their destinations. Corrected two dictionary references and one remaining-work reference
in R-test comments, and updated the documentation index and current review descriptions.
Historical reports, earlier implementation entries, and the migration ledger retain their
original paths. External bookmarks are unknown. `doc/RESOLUTION_SENSITIVITY.md` stays in place.

The link checker now excludes working-tree deletions from its source inventory without
requiring staging. Incoming links to deleted targets still fail. A regression fixture
covers both inventory modes and the incoming-link failure.

Checks actually performed:

- Both default and tracked-only link checks: **162 local links, zero errors**, four
  declared local-only targets, three historical pages excluded.
- Python documentation suite: nine tests attempted; ten cleanup errors (including two
  subtests) because the managed environment denied removal of temporary directories.
  The same restriction occurred with `/private/tmp`; a clean suite pass remains unverified.
- Standard R command stalled acquiring the renv sandbox lock and was interrupted.
  The existing framework R/library command recorded below completed with **279 passed**,
  zero test failures/errors/skips/warnings. Environment diagnostics included Arrow CPU
  queries and denied temporary-directory cleanup; process exit status was zero.
- Non-comment R lines match the baseline exactly. Git comparison confirms unchanged
  scientific/runtime files, maintained references/plans, historical records, and ledger.
  Whitespace checks passed; HEAD and index unchanged. No staging, commits, or pushes.

Recommended human-created commit: **Remove unused documentation redirects and update references**.
Group the seven deletions with `doc/README.md`, `doc/REPO_REVIEW.md`, this record,
`tests/testthat/test-canonical-schema.R`, `tests/testthat/test-pipeline-paths.R`,
`tools/docs/check_links.py`, and `tests/docs/test_links.py`.
Re-run the Python suite in an environment permitting fixture cleanup. These checks do
not constitute scientific reproduction.

## Documentation and host-policy continuation — 21 September 2026

Implemented as unstaged changes from `512e96ea571a24f7e40289e3a4060cbc1cabc230` after
the user reviewed P0. The user selected every Codex project on this Mac for machine policy.
No staging, commits, or pushes were performed. Earlier records below describe their dates;
their pending items are not overwritten retrospectively.

Completed: audience navigation, five content-preserving document relocations with redirects,
three exact historical copies, current repository review, audit taxonomy/template, local-only
storage READMEs, and an offline link checker with CI integration. `doc/ai/` remains canonical.
The scientific resolution-sensitivity guide and all analytical code/configuration are unchanged.

Live Codex activation needed no change: `hooks/list` reported the existing project hook
enabled/trusted; a harmless invalid Git option was denied before dispatch. A machine bundle
adds administrator-owned requirements and a Git-only hook, with explicit commit/push denials,
an offline workspace profile, credential restrictions, and external publishing tools disabled.
See [host enforcement](host-enforcement.md) for exact scope and limitations.

Checks actually performed:

- Harness suite: **13 tests passed**, including the existing 39 forbidden command strings
  and 13 inspection/documentation strings, plus the new managed adapter. No forbidden
  command was executed.
- Documentation checker fixtures: **8 tests passed**, including missing targets/anchors,
  repository escape, local-only evidence, redirect anchors, and untracked-file masking.
- Candidate macOS sandbox: **10 checks passed**, protected dummy files unchanged. Explicitly
  registered nested/common paths are protected; unregistered nested metadata was writable
  in an earlier diagnostic fixture. Read-access globs were rejected by the client.
- Installer shell syntax and TOML parsing checked. Privileged installation was not performed:
  `sudo -n /usr/bin/true` reported that an administrator password is required.
- Standard `Rscript tests/testthat.R --mode=synthetic` stalled in renv sandbox-lock acquisition
  and was interrupted before tests. The existing framework R/library route below passed
  **279 checks**, zero failures/errors/skips/testthat warnings. Arrow printed sandbox CPU
  query diagnostics; exit status was zero. No package install or lockfile change.

```sh
R_LIBS_USER="$PWD/renv/library/macos/R-4.6/aarch64-apple-darwin23" /Library/Frameworks/R.framework/Versions/4.6/Resources/bin/Rscript --vanilla tests/testthat.R --mode=synthetic
```

- Final navigation: **235 local links, zero errors**; four explicitly local-only targets
  reported and three exact historical pages excluded. A disposable public-documentation
  simulation copied candidate Markdown files and used placeholders for other distributed
  files, with ignored evidence absent: the same 235 links passed. This was not an actual
  fresh Git clone; the new files remain unstaged. CI uses the tracked-only inventory.
- Eight ledger rows matched original Git blobs and recorded target hashes; the five moved
  references/plans differed only by relative-link adjustments in two files. Three
  archives were byte-identical. Baseline hash comparison found no scientific/runtime file
  changes. `git diff --check` passed, HEAD remained at the baseline, and the index was unchanged.
- Native Codex rule evaluation classified commit/push as forbidden and inspection as
  unmatched. This evaluates command strings without executing them.

These checks do not establish container execution, full source-to-manuscript reproduction,
or independent review.
Remaining deployment: the researcher installs the bundle locally with administrator
authentication, restarts clients, and verifies effective requirements, managed hooks, tool
availability, and all actual/common Git-directory paths. Live Claude checks are pending
because that client was not found on PATH.

Recommended human-created commit groups, in this order (recommendations only):

1. **Prepare machine-wide Codex Git and publishing restrictions**:
   `tools/harness/managed-requirements.toml`, `tools/harness/managed_git_guard.py`,
   `tools/harness/install_managed_policy.sh`, `tools/harness/verify_host_profile.py`,
   `tests/harness/test_managed_guard.py`, `doc/ai/host-enforcement.md`, `doc/ai/harnesses.md`,
   `.claude/hooks/README.md`.
2. **Organize research documentation and preserve historical evidence**:
   `.gitignore`, `.dockerignore`, `README.md`, `doc/README.md`, `doc/HOW_TO_RUN.md`,
   `doc/REPO_REVIEW.md`, `doc/CLAUDE_CODE_SETUP.md`, `doc/PROCEDURE_AUDIT_WORKFLOW.md`,
   `doc/REMAINING_WORK.md`, `doc/TARGETS_MIGRATION_PLAN.md`, `doc/data_dictionary.md`,
   `doc/deletion_candidates.md`, `doc/idw_golden_test.md`, `doc/paper/README.md`,
   `doc/audits/README.md`, `doc/notes/README.md`, `doc/guides/first-run.md`,
   `doc/guides/contributing.md`, `doc/guides/procedure-audit.md`,
   `doc/reference/data_dictionary.md`, `doc/reference/idw_golden_test.md`,
   `doc/planning/remaining-work.md`, `doc/planning/deletion-candidates.md`,
   `doc/planning/targets-migration.md`, `doc/reviews/README.md`,
   `doc/reviews/procedure-template.md`, `doc/reviews/document-moves.csv`,
   `doc/reviews/repository/2026-08-08.md`,
   `doc/reviews/repository/claude-setup-before-2026-09-21.md`,
   `doc/reviews/repository/procedure-guide-before-2026-09-21.md`,
   `doc/ai/README.md`, `doc/ai/architecture.md`, `doc/ai/methods-tests.md`,
   `doc/ai/workflows/audit-procedure.md`, `doc/ai/evidence.md`, `doc/ai/implementation.md`,
   `tools/docs/check_links.py`, `tests/docs/test_links.py`, `.github/workflows/synthetic.yml`.

Do not include ignored audits, notes, manuscript sources, data, or temporary fixture files.
The second group includes the checker so new contributor links and CI become valid together.

## Git safety P0 — 16 September 2026

Repository-local implementation based on `4c80f71a8710e9bc913a622ab34b8aa310f97d39`.
The changes are unstaged. No commits or pushes were made; HEAD and the index are unchanged.
Analytical files, scientific specifications, source inputs, and dependencies were not edited.

Added the canonical [recommendation-only Git policy](rules/git-safety.md), required by
both root wrappers and the shared index. Removed standing historical authorization from
current collaboration guidance. Updated handoffs to recommend human-created commits only.
Native Codex rules and Claude denials now cover staging, commits, pushes, and common
history-producing commands. The shared guard rejects additional literal invocation forms,
unknown aliases/config overrides, and direct checkout .git edits. Adapter code was reused.

Checks performed:

- `python3 -B -m unittest discover -s tests/harness -v`: 10 tests passed. Fixtures include
  39 forbidden command strings and 13 inspection/documentation strings, each also checked
  through both adapters. No forbidden command was executed.
- Native `codex execpolicy check` evaluated plain commit and push strings as `forbidden`;
  Git status was unmatched. This evaluates policy without executing the supplied command.
  The CLI printed a sandbox warning about creating PATH aliases; evaluation exited zero.
- Claude JSON denial checks, root-wrapper equality, guidance paths, and `git diff --check`
  passed. These are configuration checks, not live Claude execution evidence.
- `Rscript tests/testthat.R --mode=synthetic` stalled in renv sandbox-lock acquisition
  before tests and was interrupted. No packages were installed or lockfile changed.
- The same synthetic suite completed with the existing framework R 4.6.1 installation,
  `--vanilla`, and the existing project library at
  `renv/library/macos/R-4.6/aarch64-apple-darwin23`: 222 passed, zero failures, errors,
  skips, or testthat warnings. Arrow printed sandbox CPU-query diagnostics; exit status
  was zero. This native run is not container or full scientific reproduction evidence.

Still pending: installed-client hook activation/dispatch tests, host-enforced Git metadata
protection, publishing-capability restrictions, and the documentation reorganization and
repository-review refresh. Arbitrary scripts, dynamic shell expansion, Git libraries,
remote tools, and inactive hooks remain outside this bounded command inspection. See
[harness verification](harnesses.md#verification-before-enabling-an-implementation-session).

Recommended human-created commit groups (recommendations only):

1. **Define recommendation-only Git policy** — `AGENTS.md`, `CLAUDE.md`,
   `doc/ai/rules/git-safety.md`, `doc/ai/README.md`, `doc/ai/architecture.md`,
   `doc/ai/collaboration.md`, `doc/ai/handoff.md`, `doc/TARGETS_MIGRATION_PLAN.md`.
2. **Deny agent Git mutations and test both adapters** — `tools/harness/guard_policy.py`,
   `tests/harness/test_guard.py`, `.codex/rules/project.rules`, `.claude/settings.json`,
   `.claude/hooks/README.md`, `doc/CLAUDE_CODE_SETUP.md`, `doc/ai/harnesses.md`,
   `doc/ai/evidence.md`, `doc/ai/implementation.md`.

## Container remediation — 9–10 September 2026

The accepted remediation prepares geography inside existing city processors, preserves
provider/package source copies, separates temporal manuscript production from optional
satellite comparisons, and retains three distinct container modes. `snapshot.dev = TRUE`
captures project testing dependencies without selecting downstream optional dependencies.
Scientific definitions and manuscript destinations are retained.

Measured checks during implementation:

- Native synthetic suite: 222 passed, no failures/errors/skips/warnings, using framework R
  with the declared existing local library. This is separate from container testing.
- Ten geographic products regenerated from local archives matched historical interim
  rows, identifiers, attributes, CRS and binary geometry exactly: Bogotá 2005 metro,
  municipalities and tracts; Bogotá 2018 metro and tracts; CDMX municipalities and AGEB;
  Santiago 2024 metro; São Paulo municipalities and census tracts. All nine unique source-file
  SHA-256 hashes were unchanged.
- The existing native censo2017 database and censobr v0.5.0 population cache were copied
  to declared source locations with identical SHA-256 hashes and unchanged originals.
  Their original provider retrieval dates remain unknown; copying is not baseline approval.
- Census component runs using those sources and historical geography matched both collapsed
  tables exactly: Santiago 5,931,919 individuals / 1,654 zones; São Paulo 1,216,611
  individuals / 633 weighting areas. Individual records matched exactly as multisets, with
  row ordering differing. All four source/geography hashes were unchanged. These checks do
  not establish fresh geography or complete reproduction.
- Temporal fixtures passed; the extracted 33 preparation expressions and both plotting
  loops matched their original expressions. Real temporal inputs are missing locally.
- Five harness tests passed from the repository and an unrelated working directory.

The fresh image restore initially exceeded renv's one-hour installation deadline while
DuckDB and Arrow were still compiling. The retry completed with a two-hour deadline and
unchanged package versions. The image also exposes the restored library to `Rscript --vanilla`
subprocesses; CI exercises both normal and verifier-compatible synthetic invocations.

Local evidence is under `data/verification/remediation-20260909/`. Historical intermediates
are comparison evidence, not an approved revision-matched baseline. Four declared geographic
source snapshots remain unavailable; the verifier reports these before analytical
rebuilding. Full source-to-manuscript execution, rendering review, and independent researcher
reproduction remain incomplete. No provider version was substituted to bypass these limits.

## Historical migration record — 7 September 2026

Implemented in the working tree based on revision `720f6412761bbfdcdaba008a8909bc2ce71b4889` on 7 September 2026. The following records describe that historical migration and its review state at the time. Analytical formulas, samples, weighting and missingness specifications were preserved. Existing audits retain their evidential value and original scope.

## What changed

1. **Baseline captured.** Local `data/verification/migration-baseline/` contains the original code archive, file inventory, original report source and migration checks. Existing result files are historical references because their producing revision was not established.
2. **Shared agent guidance.** `doc/ai/` is canonical. Root entry points and Claude commands/rules/reviewers route to it. Seven Codex skills, two reviewer definitions, minimal project configuration, command rules and hook wiring were added. Optional common guard policy lives in `tools/harness/guard_policy.py`; client adapters retain supported native decisions.
3. **Reliable checks.** Development, synthetic and release modes return nonzero on assertion failures/errors; synthetic and release reject skips. Deliberate failure, missing-input and subprocess fixtures exercise the runner. Freshness checks use the selected files of each producer, so unrelated historical plots in a newly shared topic folder do not falsely date that producer.
4. **Data layers corrected.** The four city configurations and their generated-geography/station/extraction producers and consumers now use interim paths. Bogotá's callable processing wrapper was also corrected, not just its standalone script. Original raw files remain. Original legacy satellite panels/geographies retain their specification-specific source paths; substituting current standardized panels would change the analysis and was not done.
5. **Results and export.** Only figures/ and tables/ remain under results. Figure topics follow the agreed seven categories; tables are flat. Interactive assets stay beside their HTML. The tracked manifest selects 117 current figure paths and 11 tables while preserving manuscript destinations. Eight matching table copies were consolidated; the additional identical census-table alias was consolidated too. Fourteen analogous exposure-figure pairs have different bytes and remain preserved historical presentations; no visual equivalence was assumed. Current producers save selected plot objects only once.
6. **Numerical evidence and packaging.** Added independent covariance, reporting-population denominator, IDW boundary/order, observed-value imputation, outlier temporal/spatial and synthetic integration checks. Added table parity comparisons, run/stage reports, extended optional Parquet provenance, a synthetic CI workflow, batch container configuration, font inclusion, build-time ICU caching and citation metadata from DESCRIPTION.

## Checks actually completed

| Check | Observed result |
|---|---|
| Synthetic suite before the final two exporter edge cases | 119 passed; no failures, errors, skips or warnings |
| Local release test suite | 245 passed; no failures, errors, skips or warnings |
| Additional exporter edge cases | Unbraced TeX references are reported; symlink escape is rejected; exporter test file passes |
| Shared guard and native adapter tests | Five Python tests passed, including multi-file patches, rename/delete, protected inputs, shell behavior and .env.example |
| Migration integrity | 417 artifact-map records accounted for; 146 copied derived files have identical source/destination SHA-256; no integrity findings |
| Original inputs | 7,111 original files retain baseline size and nanosecond mtime. SHA-256 inventories were subsequently captured. Initial source hashes were not captured, so this is not a retrospective cryptographic proof of their pre-migration contents. |
| Manuscript export | Dry-run and actual local export completed for all 128 selected artifacts, with copied-file checksum verification |
| Manuscript scanner | No unmapped or unused manifest paths; missing `doc/paper/data_appendix.tex` prevents complete coverage |
| Static checks | R source parsing, project TOML/JSON parsing, Compose configuration and YAML parsing passed; seven skill targets and frontmatters resolve |
| Installed Codex discovery | CLI 0.153.0 lists the pinned Context7 and DeepWiki configurations; startup remains unverified because npx is absent from this process's PATH |

The skill-creator quick_validate.py could not start because PyYAML is unavailable. Frontmatter was independently validated with the installed R YAML parser; this is not a claim that the official Python validator ran successfully. Hooks were tested as adapters; installed-client trust and live hook operation were not verified. No hook protection is advertised as active.

The first local inventory/check/export report is [data/verification/20260907T201921/report.json](../../data/verification/20260907T201921/report.json). Its development test stage took about 28 seconds (231 checks at that point, one explicitly skipped release-only check). Later tests include the additional layout and runner assertions shown above. The local [release verification](../../data/verification/release-check/report.json) additionally records the absence of a reviewed baseline and fresh isolated rebuild. Reports describe the code snapshot they captured; later documentation edits are not silently attributed to earlier runs.

## Acceptance still outstanding

- **Full container reproduction was not executed.** Docker Desktop startup was approved, but the separate Docker build/run escalation was rejected by the user and was not retried. The [attempt record](../../data/verification/full-attempt/report.json) records that boundary. No tested image or full runtime measurement exists from this task.
- **Revision-matched numerical parity remains unverified.** The comparison registry is intentionally empty until genuine baseline provenance, unique keys and review are supplied. The verifier reports incomplete evidence rather than fabricating a baseline. Existing historical files were preserved.
- **Clean source-to-geography preparation requires execution evidence.** City processors now
  rebuild their geographic prerequisites from preserved local sources with acquisition disabled.
  A fresh offline run from empty derived directories, source-hash checks, and geographic
  comparisons remain required before clean-room acceptance.
- **External manuscript coverage is incomplete.** The absent appendix was not invented or replaced. The local TeX draft was not edited.
- **New image/CI and Quarto rendering have not been executed.** Their configurations parse, but runtime operation requires their declared environment. Scientific rendering review, human review of new numerical evidence, input redistribution/access review and independent researcher reproduction remain separate recorded actions.

The repository now distinguishes a passing local test suite, successful copying of existing exhibits, and a verified clean reproduction. Only the first two have been established here. Use [HOW_TO_RUN](../HOW_TO_RUN.md) for commands and the clean-room protocol; use [evidence.md](evidence.md) to record reviewed scientific decisions.
