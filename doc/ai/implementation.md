# Implementation and evidence

## City geography separation and recipe style — 24 September 2026

The author-edited Bogotá download/processing recipes are the layout references. Shared
guidance now requires **92 characters**, grouped paths in Section I, concrete step comments,
named results and ordered saving. English corrections preserve that layout. Removed an
empty trailing argument from the Bogotá station-catalog call. Early source checks remain:
the 2018 census extractor otherwise skips a missing department archive. Availability checks
do not verify provenance, schemas or scientific completeness.

CDMX, Santiago and São Paulo now have separate geographic acquisition and preparation APIs.
Acquisition returns preserved sources; preparation consumes explicit local paths and returns
spatial objects; writers save derived GeoPackages. The recipes, `city_process()` convenience
interface and seven target commands use these functions. Removed the unused helper that
required geographic sources to share a folder. Santiago's optional administrative boundary
has its own preparation recipe; it remains outside the manuscript graph. The inventory now
covers **58 entry points**. These changes supersede the pending-separation status below.

Evidence under `data/verification/city-geography-20260924-183629/`:

- `before/` preserves the incoming working-tree definitions and callers, including the
  author's Bogotá edits. `compare.R` checks 111 unchanged shared function definitions and
  all four city configurations; only seven of 144 target declarations changed.
- Ten fixture comparisons are identical to the preserved implementation. Cases include
  Santiago 2017 representative-point membership and keys, both Santiago 2024 boundary
  definitions at commune/block levels, São Paulo tract-key derivation and weighting codes.
- `compare-real.R` compares old computations, current recipe calls and target commands on
  existing sources: CDMX municipalities (63) and AGEBs (5,913), Santiago 2024 communes (39),
  São Paulo municipalities (39) and census tracts (30,815). Attributes, order, geometry and
  CRS agree exactly. Separate old/new GeoPackage round trips agree; source hashes are
  unchanged. Only isolated comparison outputs were written.
- Final synthetic suite: **402 passed; zero failures, errors, skips or test warnings**,
  using the framework R executable and existing project library with `--vanilla`.
  An initial run caught a misplaced São Paulo script section (fixed) and a fixture that
  compared pre-serialization rings with Shapefile rings (fixed to read the preserved
  source). The isolated comparison harness also required a namespaced-call parsing fix.
- Target tests exercise no-op reruns, deleted outputs, changed inputs/configuration and
  writer changes. Fixture tests cover missing local inputs, source immutability, truncated
  Santiago responses and separate preparation/saving. Cached acquisition-source reuse was
  exercised; live downloads were not.
- Final static checks preserve 37 station/pollution/census/writer assignments, confirm
  that all nine city recipes and the graph parse within 92 characters, and pass all
  555 inventory/reader assertions. Checked local documentation links resolve. Whitespace
  checks pass for this batch; protected-source and lockfile Git paths have no changes.

Real comparisons execute geography calls, not whole city pipelines. Section I availability
checks for unrelated census/pollution branches were skipped only in that comparison harness.
No full census/pollution reproduction, GUI RStudio review, live acquisition, container build
or release verification was run. Three Santiago 2017 sources, the São Paulo weighting source,
a reviewed comparison baseline and the manuscript appendix remain unavailable. The optional
Santiago boundary has fixture parity; its real-source comparison remains pending. Existing
download setup helpers can still install missing packages; removing that legacy behavior
remains part of the broader loader cleanup. They were not executed in this verification.
No packages, lockfile, protected inputs or production analytical outputs were changed.

Sandbox CPU probes and temporary-directory cleanup emitted diagnostics outside the passing
test report. The old Santiago builder also warned about its fixed temporary directory during
repeated fixture comparisons; the new preparation function uses a unique temporary directory.

Suggested review/commit groups (recommendations only; no staging, `git commit` or `git push`):

1. `style: adopt Bogota recipe layout and the 92-character limit`:
   `scripts/download_data/download_bogota_data.R`,
   `scripts/process_data/process_bogota_data.R`, `doc/ai/architecture.md`,
   `doc/ai/rules/r-style.md`, `doc/ai/workflows/new-process-script.md`,
   `doc/ai/workflows/review-r.md`, `doc/ai/roles/r-reproducibility-reviewer.md`.
2. `refactor: separate geographic acquisition and preparation across cities`:
   `src/city_specific/cdmx.R`, `src/city_specific/santiago.R`,
   `src/city_specific/sao_paulo.R`, `src/city_specific/preparation.R`,
   `src/city_specific/processing.R`, `_targets.R`,
   `scripts/download_data/download_cdmx_data.R`,
   `scripts/download_data/download_santiago_data.R`,
   `scripts/download_data/download_sao_paulo_data.R`,
   `scripts/process_data/process_cdmx_data.R`,
   `scripts/process_data/process_santiago_data.R`,
   `scripts/process_data/process_sao_paulo_data.R`,
   `scripts/process_data/prepare_santiago_alternative_geography.R`,
   `tests/check-targets-contracts.R`, `tests/testthat/test-offline-preparation.R`,
   `tests/testthat/test-geography-separation.R`, `doc/HOW_TO_RUN.md`,
   `doc/planning/remaining-work.md`, `doc/planning/targets-migration.md`,
   `doc/ai/implementation.md`.

These are review groups for this batch, not instructions to include every earlier diff in
those files. Keep the pre-existing migration changes with their corresponding review groups.

## Bogotá geography separation — 24 September 2026

`bogota_download_geography()` acquires preserved source files and returns paths.
`bogota_prepare_metro_area()` consumes explicit local archive/locality paths and returns
an `sf` object; it has no network or analytical-output arguments. `write_geopackage()`
owns saving. The recipe, targets and complete `city_process()` interface use that split.
The former combined `bogota_download_metro_area()` API and its repository callers were
replaced. Bogotá's module now declares its dplyr requirement rather than its recipe.

The download recipe no longer writes the five derived layers. It still prepares the 2018
footprint explicitly for its existing source-region diagnostic. Provider URLs and archive
choices are retained. Locality acquisition metadata no longer labels that source with a
DANE census year: its vintage is unknown, and its archived provider URL remains recorded.
Existing sources and metadata were not changed. The other cities' separation is pending.

Evidence under `data/verification/geography-separation-20260924-133511/`:

- Saved the pre-refactor definitions and callers. The geographic transformation block is
  unchanged apart from its heading; acquisition, extraction and saving were separated.
- `compare.R` reproduces all five products from small provider-shaped archives, comparing
  the preserved function, manual recipe calls, target commands and convenience wrapper.
  In-memory results agree in attributes, row order, binary geometry and CRS; saved old/new
  products also agree. GeoPackage serialization's existing polygon promotion/CRS text
  normalization is handled by comparing saved products separately from computed objects.
- `compare-real-2018.R` rebuilds the 2018 municipality/locality layer from existing DANE
  sources: all **40 units** are identical as `sf` objects; both input hashes are unchanged.
- Synthetic suite: **376 passed; zero failures, errors, skips or test warnings**. New
  fixtures check all five products, explicit alternate paths, missing inputs, source
  immutability and preparation with HTTP requests blocked. An initial fixture used the
  wrong rural identifier; corrected it to the existing `SECR_CCNCT` contract.
- Recipe/new-function lines fit 90 characters; no old combined-function callers remain
  in active code. All 545 reader/inventory assertions pass after the documentation update.
  The module loads dplyr in a clean session; 20 unrelated function definitions are unchanged.

The suite uses the framework R executable and existing project library with `--vanilla`.
Sandbox CPU-probe/temporary-directory cleanup diagnostics remain outside the test report.
Live acquisition, the remaining full geographic products, full city processing, RStudio GUI
and release reproduction were not run. No source inputs or lockfile were rewritten.

Suggested commit: `refactor: separate Bogota geographic acquisition and preparation`.
Exact code group: `src/city_specific/bogota.R`, `src/city_specific/preparation.R`,
`src/city_specific/registry.R`, `scripts/download_data/download_bogota_data.R`,
`scripts/process_data/process_bogota_data.R`, `_targets.R`,
`tests/check-targets-contracts.R`, `tests/check-targets-engine.R`,
`tests/testthat/test-offline-preparation.R`, `tests/testthat/test-bogota-geography.R`.
Documentation group: `doc/HOW_TO_RUN.md`, `doc/ai/architecture.md`,
`doc/ai/implementation.md`, `doc/planning/targets-migration.md`,
`doc/planning/remaining-work.md`. Staging, committing and pushing remain human actions.

## City recipe correction — 24 September 2026

All four city recipes now declare census extraction/output paths in Section I. Section III
only saves the spatial objects, in processing order. Removed automatic pollution previews,
reconstructed census inventories and summary reads. The guide's optional inspection uses
the returned Arrow Dataset and census paths. Following Marcos's correction, `bogota.R`
declares and attaches its dplyr requirement; the recipe has no `library(dplyr)` call.

Geographic acquisition/preparation remains coupled in the existing functions. Their
processing calls still pass `allow_download = FALSE`; scientific function bodies and the target
graph were not changed in this correction. Separating those responsibilities is pending.

Checks: all four recipes parse, their setup runs with the framework R executable, and their
processing/writer expressions and moved path expressions match the pre-edit versions.
Every recipe line is at most 90 characters. The synthetic suite reports **356 passed**, with
zero failures, errors, skips or test warnings. It used the existing project library with
`--vanilla`; sandbox temporary-directory cleanup messages remain outside the test report.
No full city processing, downloads, RStudio GUI or release reproduction was run.

Suggested review groups (human staging/committing only):

- `fix: keep city recipe saving sections focused`: the four files
  `scripts/process_data/process_bogota_data.R`, `process_cdmx_data.R`,
  `process_santiago_data.R`, and `process_sao_paulo_data.R` in that folder, plus
  `src/city_specific/bogota.R` for its package requirement.
- `docs: clarify saving and optional inspection`: `doc/ai/rules/r-style.md`,
  `doc/ai/architecture.md`, `doc/HOW_TO_RUN.md`, `doc/planning/targets-migration.md`,
  and `doc/ai/implementation.md`.

## City preparation — 23 September 2026

Continued after Marcos accepted the distance pilot and confirmed the strict 90-character
limit. The four city recipes now source their scientific modules directly, declare local
inputs, retain named spatial objects, and save them in processing order. Large census
functions can return saved paths; scripts read the smaller geographic summaries.

The manuscript graph calls those same scientific functions directly. Spatial computations
and their GeoPackage writers are separate targets. Existing public stage names, output
paths and `city_process()` capabilities are retained. City configurations now name the
existing 20 km station-selection radius and UTC processing timezone.

Checks and evidence:

- Preserved 186 code/documentation files before editing under the ignored local directory
  `data/verification/readable-cities-20260923-215652/before/`.
- `compare-stations.R` compares the old preparation functions with the actual new recipe
  calls and target commands, using the same existing prepared geography and station sources.
  Attributes, ordering, geometries and CRS agree exactly for all five selections:
  Bogotá 2018 and 2005: 58 each; CDMX: 79; Santiago: 14; São Paulo: 43.
  Outputs were written only to isolated comparison directories.
- `check-algorithms.R` confirms unchanged bodies for the six census transformations after
  excluding the new path-return branch, and unchanged bodies/signatures for six pollution
  functions/engines. Existing city configuration entries also compare identically.
  This source comparison does not establish complete numerical census/pollution parity.
- Synthetic suite: **356 passed; zero failures, errors, skips or test warnings**.
  Geographic fixtures exercise in-memory returns, preserved-source checks and membership.
  A weighted census fixture compares data returns with reopened file returns.
  Production CDMX spatial target commands exercise no-write computation, no-op reruns,
  deleted-output regeneration, setting/input/writer changes, and overwrite refusal.
- The graph now sources definitions/settings with `local = TRUE`. This makes the writer
  and settings visible to dependency analysis in an isolated targets environment.
  The dependency check uses the official
  [tar_deps_raw() interface](https://docs.ropensci.org/targets/reference/tar_deps.html),
  so local assignments are not mistaken for upstream targets.
- All 19 recorded original geographic-product/lockfile hashes are unchanged. The four
  city recipes, new spatial writer, preparation module, new test and graph fit 90 characters.
- All 87 non-city target commands are unchanged; the graph has 144 declarations.
  Documentation links and working-tree whitespace checks pass.

The suite used the same framework R executable and inherited project library as the
distance pilot's successful command below. Arrow CPU probes and temporary-directory
cleanup still emit sandbox diagnostics outside the test reporter; the suite exits zero.

Limits: the comparison uses existing prepared geography. The complete city scripts,
full census/pollution rebuilds, full manuscript export, Docker release and RStudio UI were
not executed. Three Santiago 2017 responses and São Paulo's weighting-area source remain
absent; the comparison manifest has no reviewed baseline and the manuscript appendix is
unavailable. Full verification and default cutover remain pending. Other analytical
recipes, pipeline-adapter removal and operational-tool relocation remain to be implemented.

### City-preparation review groups

These groups describe this batch relative to its preserved working tree. Earlier pending
changes in these files must also be reviewed. Agents did not stage, commit or push.

Suggested subject: `refactor: expose city preparation objects and saved checkpoints`

```text
scripts/process_data/process_bogota_data.R
scripts/process_data/process_cdmx_data.R
scripts/process_data/process_santiago_data.R
scripts/process_data/process_sao_paulo_data.R
src/city_specific/bogota.R
src/city_specific/cdmx.R
src/city_specific/santiago.R
src/city_specific/sao_paulo.R
src/city_specific/preparation.R
src/general_utilities/process/spatial_files.R
_targets.R
tests/check-targets-contracts.R
tests/check-targets-engine.R
tests/testthat/test-city-preparation-targets.R
tests/testthat/test-offline-preparation.R
tests/testthat/test-numerical-contracts.R
```

Suggested subject: `docs: record readable city recipes and verification limits`

```text
doc/HOW_TO_RUN.md
doc/ai/architecture.md
doc/ai/implementation.md
doc/planning/targets-migration.md
doc/planning/remaining-work.md
```

## Distance pilot — 23 September 2026

Implemented the first reviewable batch of the approved readable-script revision.
`generate_distance_matrices.R` now exposes paths, reads, five named calculations and
five saves in matching order. The scientific function returns objects; its separate
writer returns paths and rejects existing destinations when overwrite is disabled.
INEGI and resolution callers use the new API. The latter retains returned points and
fixed-CRS behavior. No compatibility wrapper or dependency change was introduced.

`_targets.R` contains ordinary declarations, with separate distance reads, calculations
and writers. The former graph factory/parser and distance adapters were removed.
Other adapters/loaders and Make remain transitional, pending the remaining migration and
scientific acceptance. Guidance now uses the distance recipe and hand-worked examples;
the main migration record marks the previous architecture superseded.

Checks performed:

- Preserved the pre-edit working code and rebuilt old distance outputs in the ignored
  `data/verification/distance-pilot-20260923-192149/` directory before editing.
- Executed all three recipe sections in a clean R process, changing only its output root
  for isolation. All five contexts agree exactly with the preserved implementation:
  11,846 station pairs and 3,363,768 geographic pairs; maximum numerical difference zero.
  Identifier/order/schema, 0/3/5/20 km membership, points/CRS and Parquet round-trip checks pass.
- Executed the actual production distance target commands with existing prepared inputs
  and isolated outputs/store. Results agree with the recipe; no targets remain outdated.
  This replaces upstream preparation with declared prepared files and is not source reproduction.
- Portable tests exercise the 3–4–5 km triangle, all distance metrics, file contracts,
  no-op runs, deletion of either output, configuration/function/input changes,
  unaffected-city caching and failed writes. The synthetic suite passes **326 checks**,
  with zero failures, errors, skips or test warnings.
- All 29 recorded geographic/product/lock hashes are unchanged. Original source trees
  were not written. INEGI/resolution full analyses, manuscript rendering/export and
  isolated full release verification were not executed in this pilot.
- All 104 non-distance target commands and formats are unchanged by moving declarations.
  The launcher resolves `distances --outdated` against an empty isolated store. That CLI
  check used the inherited project library and disabled the renv sandbox for the process.
- The documentation check passes: 253 local links, zero errors; four declared local-only
  targets and three historical pages excluded. Working-tree whitespace checks pass.
  All three first-run R examples execute with their output redirected to the isolated
  directory and reproduce the expected station table.

Environment: the normal framework-R command stalled in renv sandbox-lock acquisition
and was interrupted. An initial vanilla run passed 325 checks but failed one subprocess
runner assertion because the child lacked the project library. Rerunning with the same
installed library inherited through `R_LIBS_USER` passed:

```sh
R_LIBS_USER="$PWD/renv/library/macos/R-4.6/aarch64-apple-darwin23" \
  /Library/Frameworks/R.framework/Resources/bin/Rscript --vanilla \
  tests/testthat.R --mode=synthetic
```

This is an environment-specific verification command, not a replacement dependency
setup. Arrow CPU probes and temporary-directory cleanup produced sandbox diagnostics
outside the test reporter; the successful suite exited zero. No packages or lockfiles
were altered to resolve them. The automated checks did not exercise the RStudio UI.
Marcos subsequently verified the pilot and accepted its readability, clearing that gate
for repository-wide rollout. His revised step comments and R layout are now the style
reference; shared guidance records that preference. Full source reproduction still
requires the four missing geographic sources, reviewed baseline and manuscript appendix.

The style follow-up changes comments and whitespace only in the distance recipe.
R parsing confirms identical executable expressions before and after those edits;
the documentation check passes 259 local links, with zero errors. The first-run guide
and lockfile are unchanged. The analytical tests and full reproduction were not rerun
for this presentation-only change.

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

## Manuscript targets migration candidate — 2026-09-23

The authorized migration now has complete four-city offline interfaces and a 96-target
manuscript graph. Twenty-eight main scripts call shared reusable stages. The registry retains
configuration/manual entry points; target commands expose concrete functions and upstream files.
Whole station datasets and city/vintage IDW families have one owner. The graph preserves temporal,
imputation, descriptive and scatter branches; optional workflows remain separate.

The target engine exposed two integration issues that were corrected: file targets discard path
names, and separate-process selections need literal names. File roles are reconstructed from
explicit contracts. The CLI inspection route succeeds and selects 90 manuscript targets/ancestors.
Processing targets pin the original s2 setting. CDMX rebuilds remove obsolete year partitions;
census extraction refreshes derived checkpoints. Writers expose required outputs, and missing
required plot objects now fail instead of accepting stale files.

Current evidence: 109 portable contract assertions and 24 engine assertions passed; all 21
extracted downstream bodies matched the reference after normalizing input/output plumbing;
145 R files parsed; three Compose configurations parsed. All 128 manifest producers and existing
artifacts resolve. The manuscript scanner reports no unmapped/unused destinations, but the absent
`data_appendix.tex` still prevents complete coverage. These checks do not establish regeneration,
numerical parity, rendered-output quality, or an executed container image.

The full synthetic command crashed in native `brio` code before assertions. Isolated full targets
verification crashed loading a compiled `sf` dependency before rebuilding; normal renv activation
also waited on a sandbox lock. Successful fixture processes emitted cleanup permission messages
at exit. Four preserved geographic inputs and a reviewed comparison baseline remain absent.

The user installed targets 1.12.0, but their concurrent lockfile change reduced the package set
from 175 to 48. Its intent was queried; the agent left the protected lockfile untouched. Restore
the complete scientific environment before interpreting any migration as accepted. The existing
default runner is retained pending parity. No data acquisition, scientific result regeneration,
Git staging, commit or push was performed.

Commands are in [HOW_TO_RUN](../HOW_TO_RUN.md#candidate-manuscript-migration-to-targets).
The public development [migration record](../planning/targets-migration.md) contains acceptance
criteria, model responsibility and recommended review groups; it is excluded from runtime images.
## Reader-first targets revision — 2026-09-23

This entry supersedes the earlier thin-script candidate's structural design and check counts.
The governing requirement is now first in architecture/R style and in the four creation/review
workflows and reviewer guidance. Twenty-eight manuscript scripts expose individual operations
and named objects. All 56 R scripts have three/four ordered sections; the Quarto report keeps
its narrative organization and explicitly reads its comparison objects. Optional resolution,
satellite, acquisition and validation workflows remain separate from manuscript targets.

The graph has **109 literal target declarations**, including **13 rendering-data checkpoints**.
City preparation, whole station datasets and city/vintage IDW families retain their ownership.
Small tables, plot collections and distance objects are inspectable; large partitioned datasets
remain file-backed. Shared functions read supplied input roots instead of discarding them for
canonical roots. Renderers recreate their theme/directories when cached input objects are reused.
Render commands also reference their actual upstream files: a path-only checkpoint can
stay identical after file contents change. Public aggregate selections use file storage
to preserve content hashes; they forward files and never write competing products.
Scientific settings were preserved at source level; executed parity remains pending.

The [workflow inventory](../planning/remaining-work.md) covers **57 entry points** with commands,
prerequisites, target mappings/separation reasons, principal inputs/outputs/consumers, readiness
and next actions. Tests use that inventory instead of a hard-coded unwired list. It is the one
planning file included in the runtime image, because runtime checks consume it; verification
provenance includes its hash. Other planning material remains excluded. The deletion record
distinguishes current evidence from its preserved August claims. No optional analysis was deleted.

Checks actually run for this revision:

| Check | Evidence and limit |
|---|---|
| Portable reader contracts | 545 assertions passed: exact inventory coverage/links/target names, section/header structure, cache-independent recipes, alternate input roots, and mocked manual/render agreement with missing outputs. Mocked rendering does not establish numerical or graphical parity. |
| Portable city/targets contracts | 111 assertions passed, including offline geography contracts, aliases, missing inputs, failure propagation, output roles and acyclic dependency checks. |
| Targets engine fixtures | 32 assertions passed: selection, no-op reuse, changed files/functions/settings, source membership changes, missing-output regeneration, separately cached computation/rendering and unaffected branches. An additional fixture checks changes through a file selection and an unchanged path-only checkpoint. The real graph's city/outlier/render-data edges were inspected. |
| Source/structure checks | 151 R files plus Quarto R chunks parsed. Revised script/pipeline/resolution R code has no code lines above 90 characters. Formatting was separately checked for unchanged parsed expressions across 86 files. |
| Extraction audit | Compared manuscript operation statements with the pre-edit working-tree snapshot and optional resolution call expressions with their original scripts. Reviewed differences are explicit paths, retained result collections, setup/reporting, and explicit replacement of name-based checkpoint lookup. A missing cross-operation resolution collection was corrected. Static comparison is not executed scientific parity. |
| Transitional scheduler | Make dry run and active sequential runner cover the same 28 manuscript scripts; acquisition remains disabled. No analyses were run by this check. |
| Required synthetic suite | `Rscript tests/testthat.R --mode=synthetic`, with renv sandbox activation disabled, stopped because the project library lacks `here`. A vanilla fallback found `here` but lacks `testthat`; the full suite did not execute. |
| Required isolated verification | `Rscript scripts/verification/verify.R --full --targets`, with renv sandbox activation disabled, stopped on missing `here` before Docker execution. No release verification or scientific rebuild occurred. |
| Manuscript references | No unmapped references or unused manifest destinations for the available draft; check fails on missing `doc/paper/data_appendix.tex`. |
| Public documentation | 252 local links checked, zero errors; four explicitly local-only targets and three historical pages excluded. Git whitespace check passed. |
| Preservation | No acquisition or analytical execution occurred. renv.lock SHA-256 remains `272bc378fb6c415c74da9c0742e79acfa3f40360c69c535f7986ac724307ce41`, matching the pre-edit working tree. No dependency installation/upgrade or lockfile regeneration was performed. |

Portable checks used existing installed libraries, not a restored release environment. Base-R
checks used `Rscript --vanilla`; engine checks added the existing Homebrew site library to the
activated project library so `here` and `targets` could coexist. Some successful R processes
printed temporary-directory cleanup permission errors at exit. These accommodations are bounded
development evidence, not a portable environment or image certification.

Rechecked blockers: three Santiago 2017 geographic responses and São Paulo's weighting-area
source are absent; `config/verification_comparisons.csv` still has zero baseline entries; the
manuscript appendix is absent. The package environment remains incomplete. Historical native
`brio`/`sf` failures are not claimed fixed; this run stopped earlier on missing packages.

**Scientific acceptance and cutover remain pending.** A real clean-RStudio walkthrough, isolated
pre/post numerical and rendered-output comparisons, fresh offline four-city execution, real
pipeline invalidation checks and container/release inspection have not been performed. The
pre-edit snapshot is a candidate-code comparison aid, not an approved independent baseline.
Makefile, its RStudio build setting and the sequential runner therefore remain transitional.
After acceptance, remove Makefile, translate its optional commands using the inventory, and
replace the sequential body with a targets compatibility launcher. Do not upgrade dependencies
or substitute missing scientific inputs as part of this structural change.
