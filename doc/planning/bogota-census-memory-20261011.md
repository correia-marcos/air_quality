# Bogotá census memory repair and satellite rename — 11 October 2026

Marcos authorized these two repairs after the clean `05df9c4` checkout failed isolated
verification. The 9 October run built its image successfully, then stopped while preparing
Bogotá's 2018 census. Its container `oom_kill` counter increased from zero to one during
rebuilding. The log and counters do not identify the narrower container or Docker VM limit.
No Docker command was run by the agent during this repair.

## The revised census calculation

`bogota_harmonize_census_2018_data()` in
[bogota.R](../../src/city_specific/bogota.R) now reads only the required geographic columns
and filters their metropolitan matches before joining person records. A secondary index
supports the repeated survey-key joins while preserving every matching geographic row.
Person records are parsed in chunks of 10,000, with their five required columns kept as
character. The original delimiter is detected from the header. Each chunk is harmonized,
given canonical names/provenance and appended to the same single individual Parquet file.
[readr's chunk callback](https://readr.tidyverse.org/reference/read_delim_chunked.html)
provides the bounded person-file read; the installed Arrow writer appends its row groups.

The same saved individual file supplies an Arrow aggregation over adults aged 25+.
Grouping uses the original geographic code before canonical renaming. Means and education
shares retain the reporting-adult denominator; demographic shares retain all adults.
The column order, geographic identifiers, six education bands, survey-key multiplicity,
unit weights, person order, missing values and existing filenames are retained. Both
departments remain, and no census variant or scientific specification was dropped.

With `return_data = FALSE`, the production route returns the two saved paths and does not
retain the complete person table in R. Both the direct recipe and targets already use
this mode. `return_data = TRUE` remains the explicit interactive option that reads the
complete saved individual table into memory. The new `chunk_size` argument controls
resources, not eligibility or the estimand. A reader can inspect `people`, `joined`,
`canonical` and `collapse_data` in the function, or reopen the returned checkpoints.

The optional extraction script was renamed to `generate_panel_air_quality_merra2.R` by
Marcos. Its Make recipe, commented sequential-runner example and workflow-inventory row
now use that name. The default manuscript route still excludes satellite preparation.

## Executed evidence and its limits

| Check | Result |
|---|---|
| Portable suite | 926 passes; zero failures, errors, warnings or skips using Framework R 4.6.1 and the existing project library. |
| Original-function fixture comparison | Individual and collapsed values, types and order agree exactly with the saved pre-edit function for chunk sizes 1, 7 and 10,000. |
| Independent fixture expectations | Tests cover all schooling levels, reporting-adult denominators, an entirely missing-education group, minors/missing age, duplicate survey keys, missing keys, both departments, source preservation, metadata and empty samples. |
| Full native Bogotá 2018 replay | 9,028,674 individual records, 5,693,549 adults and 50,287 geographic units; approximately 43 seconds inside the preparation function. Outputs remain isolated under verification. |
| Native process memory | Maximum resident size 2,006,728,704 bytes, approximately 1.87 GiB, measured through subprocess resource usage in the final replay. This describes the native census process, not the complete Docker graph. |
| Real historical comparison | Every individual value/type/order and all 50,287 collapsed rows match the existing historical Parquets exactly. Fixed provenance keys match; compared-file hashes stayed unchanged. Historical files have no reviewed producing-revision/input manifest, so they remain refactor evidence. |
| Input preservation | The four consumed extracted CSV SHA-256 values match before and after preparation. Canonical production census outputs were read for comparison and not replaced. |
| Links and graph | Tracked documentation links pass after the rename; the candidate graph still constructs 352 targets. |

The native Apple `time -l` wrapper could not read restricted `sysctl` information after
its first successful R replay. A separate subprocess resource measurement succeeded.
Arrow also prints sandbox CPU-detection diagnostics locally; the completed native runs
and portable checks are recorded separately. No Linux/Docker memory measurement was
performed here, and no full scientific reproduction or reviewed baseline is claimed.

Local evidence is under `data/verification/bogota-census-memory-20261011/`: preserved
input identities, replay/comparison drivers and logs, process memory, candidate Parquets,
original-function comparisons and portable-test results. This directory is ignored.
The final real-data replay uses `candidate-final/`; earlier candidates remain separate.
`code-inventory.csv` identifies the uncommitted code used for the checks; the recorded Git
revision identifies its base checkout, rather than asserting those edits were committed.
The original source function and three rename files were preserved in a temporary snapshot.

## Files for human review

Suggested subject: `fix: stream Bogotá census preparation with bounded memory`:

- `src/city_specific/bogota.R`
- `tests/testthat/test-bogota-census-memory.R` — new file; include it with the function.
- `doc/planning/bogota-census-memory-20261011.md` — this evidence/validation handoff.

Suggested subject: `fix: align optional MERRA extraction script references`:

- `Makefile`
- `scripts/run_pipeline.R`
- `doc/planning/remaining-work.md`

Separate comment/spacing edits appeared in `scripts/process_data/process_bogota_data.R`
and `scripts/process_data/process_cdmx_data.R` during this task. Those edits were preserved
and are outside the agent's repair group.
No files were staged or committed by the agent; protected originals and `renv.lock`
were not edited.

## Next human-run verification

After reviewing the repair files, run from the repository root on this Mac:

```sh
export AIR_VERIFY_IMAGE=air-monitoring-verification:bounds-20261008
export AIR_VERIFY_RUN="$PWD/data/verification/manuscript-$(date -u +%Y%m%dT%H%M%SZ)"
PATH="/Library/Frameworks/R.framework/Resources/bin:$PATH" \
  RENV_CONFIG_AUTOLOADER_ENABLED=false \
  R_LIBS="$PWD/renv/library/macos/R-4.6/aarch64-apple-darwin23" \
  Rscript --vanilla tools/reproduction/verify.R --full --targets
```

This builds the current image-baked code and uses fresh derived outputs/cache with
original inputs mounted read-only. Check the rebuild stage, its memory-event counters,
release tests and export. A later stage may reveal another issue; the native census replay
does not establish complete pipeline success. Retain `report/report.json`, `report/rebuild.log`
and `report/tests.log` under the new run directory.

Baseline preparation must wait until the registered products exist. A successful rebuild
can still return exit 1 for incomplete comparison coverage, missing review or an unmatched
baseline. Review underlying plot data, numerical results, rendering and the human RStudio
route before scientific acceptance and the final scheduler cutover.
