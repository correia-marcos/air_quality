# Migration verification and census repair — 14 October 2026

The starting checkout was clean at `b6eb80b`. The required graph modules, station-only
appendix route, reproduction tools, Bogotá census memory repair, CDMX pollution aggregation
repair and optional MERRA script rename are committed. The scientific baseline/results
review is still in progress. No scientific acceptance or independent reproduction is recorded.

The 14 October Docker run reached the container, but did not finish the manuscript rebuild.
The user asked to repair verification before removing the existing scheduler. Makefile,
the RStudio Make build setting and the sequential runner therefore remain in place.

## What the Docker run establishes

The recorded run is `data/verification/manuscript-20261014T140629Z/`, against `b6eb80b`.
Docker Desktop and the image build returned zero. The image identity is
`sha256:9c3592e7b1a0a3abe41db1a100e0c0e27cff9c55bc79897dd38c08486d23e0dc`.
The user reported a Docker VM with 10 CPUs and 16.06 GiB memory.

| Stage | Executed result |
|---|---|
| Preparation inputs | All declared preparation paths present. Availability alone does not establish provenance. |
| Rebuild | Exit 1 during CDMX census processing, after all three person CSVs were extracted. Bogotá census preparation completed. |
| Memory evidence | Container `oom_kill` increased from 0 to 1 during rebuilding. This supports an OOM kill during that stage; the counters do not identify the narrower VM/container limit. |
| Release tests | 948 passes, three failures, five errors, no skips or warnings. Missing downstream census files, outputs and freshness follow the interrupted rebuild. |
| Export | Failed because manuscript products were not generated. |
| Baseline | No reviewed revision-matched baseline registered. |

The outer report's successful build is not a successful manuscript execution. Read both
`report.json` and `report/report.json`; detailed failures are in `report/rebuild.log` and
`report/tests.log`. Reviewing existing local results can continue, but those complete
results were not produced by this interrupted isolated run.

## Repair and bounded checks

`mexico_harmonize_census_data()` previously read all 91 source columns and filtered the
wide table after harmonization. It now reads only the twelve required columns, preserving
case-insensitive source names, and selects metropolitan municipalities before harmonizing.
The retained individual table has 879,968 rows and is manageable; this repair does not add
a second streaming pipeline or change the function's return/output contract.

Schooling replacements, demographic variables, survey weights, adult eligibility, missing
values, pooled income calculations, canonical identifiers and filenames retain their existing
definitions. In particular, the implemented income rule uses an unweighted R type-7 first
percentile of positive income and a 99th percentile of all nonmissing retained income.
Both pool all files and ages; caps apply to positive income only. The former comment
incorrectly described both percentile populations as positive-only and is corrected.

The full native replay used the three extracted CSVs from the failed isolated run, writing
only to `data/verification/cdmx-census-memory-20261014/candidate/`. It produced 879,968
individual records, 536,653 adults and 63 municipalities in approximately 7.34 seconds of
census processing. Maximum resident size was 3,471,458,304 bytes, approximately 3.23 GiB.
That measurement describes the native R process, not the complete Docker graph.

Every individual and collapsed value, type and row order matches the existing historical
Parquets exactly. Fixed provenance metadata matches; consumed CSV and compared Parquet
hashes remain unchanged. The historical files are refactor evidence, not an approved release
baseline. The independent fixtures check reporting-adult weights, zero/missing income,
pooled percentile values, a child's contribution to the cutoff, outside-metro exclusion,
lowercase headers, all three source files and both return modes.

The verifier now prints its exit status, reproduction status, failed stages and log paths,
including failures in the nested container report. It still saves the complete reports and
returns the same failure statuses. Tests reproduce failed-stage reporting without invoking
Docker. The portable suite passes 988 checks with no failures, errors, skips or warnings.

Local replay, memory, input identities, comparisons and logs remain under the ignored
`data/verification/cdmx-census-memory-20261014/` directory. Only Marcos runs Docker commands.
No source acquisition, original-input edit, dependency change, staging, commit or push was
performed by the agent.

## Review groups

Suggested subject: `fix: reduce CDMX census preparation memory`:

- `src/city_specific/cdmx.R`
- `tests/testthat/test-cdmx-census-memory.R` — new file.

Suggested subject: `fix: report verification failures in the terminal`:

- `src/general_utilities/verification_cli.R`
- `tools/reproduction/verify.R`
- `tests/testthat/test-verification-summary.R` — new file.

Suggested subject: `docs: record current migration verification and acceptance limits`:

- `README.md`
- `doc/HOW_TO_RUN.md`
- `doc/planning/remaining-work.md`
- `doc/planning/targets-migration.md`
- `doc/planning/targets-migration-handoff.md`
- `doc/planning/migration-verification-20261014.md` — this new record.

## Next execution and final cutover

After review, run the repaired code in a fresh isolated directory from the repository root:

```sh
export AIR_VERIFY_IMAGE=air-monitoring-verification:bounds-20261008
export AIR_VERIFY_RUN="$PWD/data/verification/manuscript-$(date -u +%Y%m%dT%H%M%SZ)"
PATH="/Library/Frameworks/R.framework/Resources/bin:$PATH" \
  RENV_CONFIG_AUTOLOADER_ENABLED=false \
  R_LIBS="$PWD/renv/library/macos/R-4.6/aarch64-apple-darwin23" \
  Rscript --vanilla tools/reproduction/verify.R --full --targets
AIR_VERIFY_STATUS=$?
printf 'Verification exit status: %s\n' "$AIR_VERIFY_STATUS"
cat "$AIR_VERIFY_RUN/report.json"
```

The first required result is rebuild exit zero, completed release tests and export. A later
stage may reveal another issue. Baseline registration can still keep the overall command at
exit 1; check the recorded stages instead of treating every exit 1 as the same failure.

Prepare the baseline candidate from a completed isolated rebuild, following
[the baseline handoff](migration-continuation-20261008.md#next-human-run-docker-check)
and [the review requirements](../HOW_TO_RUN.md#prepare-a-comparison-candidate).
Extend comparison coverage where needed and complete numerical, plot-data, rendering,
caption and human RStudio review without assigning approval flags on another person's behalf.

After acceptance, remove Makefile and its RStudio setting, make `run_pipeline.R` a targets
compatibility launcher, switch the verifier/Compose defaults together, update active commands,
and preserve direct acquisition, optional-analysis and legacy-validation recipes. Verify a
fresh default run, stage selection, no-op rerun and deleted-output regeneration. Optional
resolution acceptance, timezone methodology and independent reproduction retain separate scope.
