# Migration continuation — 8 October 2026

Several migration batches are committed, but their dependencies are not all included.
The task began at `ffbe43f` with 58 modified/deleted tracked files, 14 untracked files and
nothing staged. Uncommitted does not mean unreviewed: Marcos reports reviewing earlier
changes, but Git cannot establish which remaining hunks received that review.

The complete [pending file list](migration-review-files-20261008.csv) names every file
that differs from HEAD, its state, a proposed review group and whether it changed during
this continuation. Include dependencies together; review mixed files by hunk. The list
also names the new handoff files themselves. No agent stages, commits or pushes.

## What is ready and what remains

The working candidate constructs 352 targets through the 48-line entry point and 14
declaration modules. A separate code-only copy of committed HEAD fails while sourcing
`src/general_utilities/process/station_temporal.R`, which is still untracked. This is a
dependency gap in the committed checkout, not an absent local file. The targets launcher,
hourly recipe, optional satellite functions and required test/validation helpers also
remain untracked. The CSV identifies all 14, including eight test files.

The approved PM2.5/PM10 screening bounds are 2,000/6,000 µg/m³, with equality retained.
The compact observed audit schema and accepted Primaria Revolución readings are preserved.
The bounded bounds/schema evidence from the other conversation was inspected; it is not
a complete manuscript rebuild. Production cleaned panels still need regeneration under
this schema, followed by their downstream products. The initial 5 October baseline and
its unscreened CDMX episode counts are historical references.

The local manuscript appendix and bibliography now exist. Literal manuscript-reference
checking passes, and all 128 export artifacts are available. Existing artifacts have
mixed producing revisions; availability does not establish freshness. The bounds evidence
records a provisional manuscript compilation; this continuation did not recompile it.
The newest appendix wording still requires author compilation/rendering review.

The comparison registry has 94 rows; 42 registered products are currently missing from
the canonical output locations, principally quality reports, source metadata and audit
partitions. Preparing a new canonical baseline now would fail its input check. Do not
copy older outputs into these locations merely to satisfy the registry. Remaining
generated products, geography and underlying plot data need registry coverage and
human review after the fresh rebuild. No baseline approval was assigned here.

## Work completed in this continuation

The verifier's generated-product inventory excluded CSVs under
`data/interim/monitoring_stations/`, although the registry includes the CDMX source
manifest and coverage table there. `verification_product_paths()` now includes those
generated metadata tables alongside processed tables, geography and manuscript tables.
It still excludes original inputs and PDF renderings. A regression fixture checks this
scope, including audit products and excluded raw/legacy files.

`verification_command()` now records Linux cgroup memory events before and after a
subprocess when available. The supplied Docker run at `99daf2f` ended while reading
Bogotá's 2018 person/geography CSVs. Its generic callr crash message does not establish
OOM, a native crash or another termination cause. Increased `oom_kill` counts in the
next run support a container memory-limit kill; unchanged counts do not identify the
alternative cause. No speculative census rewrite or scientific change was made.

Current migration pointers and the decision index now distinguish the approved
screening/schema changes from dated 5 October findings. Make and its RStudio build setting
remain until complete scientific acceptance. Optional resolution outputs and original
inputs were preserved. Docker commands are reserved for Marcos.

## Checks actually executed

| Check | Result and limitation |
|---|---|
| Portable R checks | 829 passed; zero failures, errors, warnings or skips. Framework R 4.6.1 and the existing matching project library were used with renv autoactivation disabled. The initial unchanged suite passed 825 checks. |
| Committed graph | A code-only copy of HEAD fails on the missing station-hourly source. Git history/index were not changed. |
| Working graph | A public working-file candidate parses and constructs 352 targets without analytical datasets. This does not execute the full graph. |
| Harness and documentation units | 13 harness tests and nine documentation tests passed. Live client enforcement was not re-tested. |
| Documentation links | Working-file links pass. Tracked-only checking reports seven links to required untracked dependencies. New handoff links also depend on including the new documents together. |
| Source preparation inventory | All 15 offline-preparation paths exist locally; availability does not establish provenance. |
| Manuscript and export | All literal references match; all 128 artifacts validate in dry-run mode. Neither check establishes fresh numerical outputs or current manuscript compilation. |
| Protected inputs and Git | No edits to original source data or `renv.lock`; index remains empty. No Docker command, staging, commit or push was performed. |
| Linux memory instrumentation | The failed-subprocess status/log fixture passes locally. Actual Linux cgroup capture requires the human-run container check. |

Evidence and starting file-state records are local under
`data/verification/migration-continuation-20261008/`. These ignored files do not appear in
a fresh clone. Earlier bounds/schema and failed Docker reports retain their own revisions.

## Next human-run Docker check

First review/include the pending dependency groups. The build uses the current working
files, so record any intentional remaining diff separately from the commit. From the
repository root on this Mac, run:

```sh
export AIR_VERIFY_IMAGE=air-monitoring-verification:bounds-20261008
export AIR_VERIFY_RUN="$PWD/data/verification/manuscript-$(date -u +%Y%m%dT%H%M%SZ)"
PATH="/Library/Frameworks/R.framework/Resources/bin:$PATH" \
  RENV_CONFIG_AUTOLOADER_ENABLED=false \
  R_LIBS="$PWD/renv/library/macos/R-4.6/aarch64-apple-darwin23" \
  Rscript --vanilla tools/reproduction/verify.R --full --targets
```

The command builds current image-baked code using the existing package-layer cache, mounts
original inputs read-only, creates fresh derived/output/cache directories, and runs the
manuscript selection without satellite preparation. It preserves previous run directories.
The image tag is convenient for cache reuse; the report records the actual image identity.
Even a complete rebuild can exit 1 because the revision-matched baseline, comparison
coverage or rendering review is incomplete. Read the stage results rather than interpreting
that exit as a build failure or claiming reproduction.

Keep `$AIR_VERIFY_RUN/report/report.json`, `$AIR_VERIFY_RUN/report/rebuild.log` and
`$AIR_VERIFY_RUN/report/tests.log`, as well as the outer `report.json` and build log.
If the worker dies again, provide the rebuild stage's memory-event fields and the log tail.
That evidence will determine whether to optimize census memory use, investigate a native
reader crash or address another problem.

If the rebuild produces every registered product, a baseline snapshot can be prepared
from those same isolated outputs without adopting them as canonical data. In the same
shell, with the run directory above still selected:

```sh
export AIR_SOURCE_ROOT="$PWD"
export AIR_BASELINE_ROOT="$PWD/data/verification/baseline"
export AIR_CODE_REVISION="$(git rev-parse HEAD)"
docker compose -f docker-compose.verify.yml run --rm --entrypoint Rscript verify \
  tools/reproduction/prepare_baseline.R \
  --destination data/verification/baseline-candidate
```

The candidate is written to `$AIR_VERIFY_RUN/report/baseline-candidate/`; every approval
flag remains false. Run this immediately against the same image/code revision used for
the rebuild. The tool refuses an occupied destination or missing registered products.
This snapshot covers the registry, not all omitted outputs. Extend coverage and review
input identity, station selection, numerical results, plot data and renderings before
acceptance. Do not mark the baseline reviewed merely to obtain a passing verifier.

Complete the human RStudio walkthrough as part of acceptance. After the complete route
is accepted, finish the scheduler cutover. Independent reproduction remains a separate
executed check.

## Suggested review groups

The CSV provides exact filenames; these subjects describe the groups, not agent commits.

| Group | Suggested subject and purpose |
|---|---|
| Station route and graph checks | `fix: include station-only migration dependencies` — hourly recipe/functions, launcher, plotting/writer changes, transitional runners and their numerical/graph tests. |
| Particulate integration | `fix: wire compact particulate audit schema consistently` — shared settings, outlier target calls, validation adapter, imputation fixture and scientific reference documentation. |
| Reader-facing recipes | `refactor: finish direct manuscript recipes` — existing named-object processing/rendering revisions and shared exposure helpers, with their tests. |
| Optional satellite/context work | `refactor: complete current-input satellite and context recipes` — optional inputs/functions, context maps and the historical-year station diagnostic. |
| Verification and packaging | `fix: compare station provenance and record rebuild memory events` — shared comparisons/CLI, verifier, tests, producer manifest, Compose wiring and removal of superseded operational paths. |
| Optional resolution | `fix: include resolution workflow dependencies` — the required workflow source and remaining shared review/grouping changes. Scientific output acceptance remains separate. |
| Legacy validation | `refactor: complete direct validation recipes` — report helper, historical comparison recipes and loader. Preserve historical input comparisons. |
| Current documentation | `docs: record current migration dependencies and Docker handoff` — this record, exact file CSV, current planning pointers, run guide and decision/evidence index. |

The local manuscript sources belong to the authors' separate manuscript workflow; the
repository intentionally ignores `doc/paper/`. No result PDFs were regenerated in this
continuation. Public-source review and manuscript/scientific acceptance are separate.
