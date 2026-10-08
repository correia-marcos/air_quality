# Targets migration: handoff for the next model

## Current implementation — 5 October 2026

Marcos authorized the revised station-only migration plan in the implementation session.
The manuscript graph now has a 48-line entry point, 14 declaration modules and 340 targets.
It excludes optional satellite preparation and historical balanced panels. Station-hourly
checkpoints, an IT2 episode CSV and the five appendix PDFs use current observed cleaned
partitions. Reproduction tools have moved to `tools/reproduction/`.

See [the current implementation and acceptance record](station-only-targets-implementation.md)
for exact checks, scientific findings and dependency-complete review groups. Full acceptance
and the final scheduler cutover remain pending. The CDMX cleaned dataset contains extreme
PM2.5 readings that require scientific review. Existing data, census vintages and methods
were preserved. There has been no staging, committing or pushing.

<details>
<summary>Historical migration and handoff records, through 30 September 2026</summary>

Status checked on 30 September 2026 at commit `7e75514`. This is one current task
handoff, not a new architecture or evidence of scientific acceptance. Recheck the
working tree before acting: Marcos reviews and commits between sessions.

## Prompt to start the next session

> Read `doc/planning/targets-migration-handoff.md` and the shared project guidance
> it references. Continue the approved reader-first targets migration from the
> current working tree. First close the unfinished temporal preparation batch:
> diagnose its five test failures, finish the isolated comparisons, reconcile
> the deleted loader's callers, and update the evidence and inventory. Then
> proceed through the remaining implementation in reviewable groups. Preserve
> my script layout, scientific definitions, protected inputs and package versions.
> Do not restore the rejected adapter architecture or remove Make before
> scientific acceptance. Report actual checks and blockers. Recommend exact
> review/commit groups; never stage, run git commit, or run git push.

This handoff-writing task changed documentation only. The prompt above is for
Marcos to give to the next model; it is not an instruction to execute from a
documentation read alone. Marcos owns scientific decisions and readability review.

## Read these first

- [Shared guidance](../ai/README.md), [architecture](../ai/architecture.md),
  [collaboration](../ai/collaboration.md), and [Git safety](../ai/rules/git-safety.md).
- [R style](../ai/rules/r-style.md), [data and paths](../ai/rules/data-and-paths.md),
  and the applicable workflow from the shared guidance index before changing code.
- [Approved migration](targets-migration.md), especially its active architecture
  and remaining implementation. Its historical sections preserve superseded designs.
- [Workflow inventory](remaining-work.md) and [implementation evidence](../ai/implementation.md).

Some status prose is behind the code: architecture still lists `src/pipeline/`,
and the migration/evidence records still describe two remaining temporal adapters.
The latest commit removed those files. Update the active descriptions after checking
their replacement; retain dated historical evidence with its original limitations.

## Decisions that must survive the handoff

The reader's starting point is an R script that can be run a few lines at a time
in RStudio without a targets cache or knowledge of orchestration internals.

- Section I: sources, shared settings, explicit input/output paths, and data reads.
- Section II: identifiable scientific calls returning meaningful named objects.
- Section III: save those objects in the same order. No separate inspection block.
- Large streaming or partitioned operations may compute and save together. Keep
  their returned paths; do not save them twice or collect entire datasets to RAM.
- Use spaced, named calls in the author's R style, with a **92-character maximum**.
  Keep the first argument on the function line when readable; shorten alignment
  when necessary. Comments should explain the data and purpose, not machinery.
- References: the author-edited Bogotá download/processing recipes, the distance
  recipe, and `compute_distance_band_descriptives.R` for comments. The
  [first-run guide](../guides/first-run.md) was explicitly accepted. Scientific
  explanations should follow the [worked IDW example](../reference/idw_golden_test.md).
- Scripts and targets call the same scientific functions. Ordinary repeated calls
  are acceptable; transformation algorithms and scientific settings stay shared.
- The loader/adapters/specification-table/input-dictionary architecture is
  **superseded**. Do not recreate it to make scripts shorter. Use ordinary grouped
  `tar_target()` declarations in `_targets.R`; never source analytical scripts from
  targets or use hidden `tar_read()` calls inside scientific functions.
- Keep `city_process()` as a complete convenience interface and retain the registry.
  Reader scripts expose the underlying operations. Acquisition and preparation stay
  separate; preparation consumes preserved local files and never downloads.
- Only manuscript processing enters targets, including the temporal MERRA-2 branch.
  Optional resolution, satellite/context, acquisition and legacy-validation entry
  points remain independent. Absence from targets does not justify deleting them.
- Preserve distance -> outlier dependencies, cross-year windows, one owner per
  complete station dataset and city/vintage IDW family, serial shared writes, seeds,
  output names, frozen resolution inputs and checksums.
- File targets must expose every owned file, including sidecars/directory membership.
  Select files explicitly; do not assume vector names survive file-target storage.
- Keep one public repository. Leaner Docker/release contents must retain the sources,
  methodological evidence and inventory required for verification.

Protect `data/raw/`, `data/downloads/`, `data/_legacy/`, `renv.lock` and credentials.
Reads of scientific inputs are allowed; changes and acquisition are outside this
structural refactor. Do not read or print credentials. Do not upgrade dependencies.
Shared rules remain in `doc/ai/`; this document records task state. Use independent
agents only if explicitly requested under the current project instructions.

## Immediate issue: commits and working tree are not yet a complete batch

Latest commits inspected:

| Commit | Change |
|---|---|
| `7e75514` | Removed all six tracked files under `src/pipeline/`. |
| `33dad05` | MERRA-2 refactor updates. |
| `b1da74a` | Moved setup package references into the then-existing pipeline package file. |
| `d30244e` | Distance function/style update. |

At inspection, **53 tracked files were modified, 12 were untracked, and nothing
was staged**, before adding this handoff. These include earlier work; do not
blanket-revert them or assume every difference belongs to the temporal batch.

The committed `_targets.R` still starts by sourcing `src/pipeline/load.R`, which
`7e75514` deleted. Its working-tree replacement sources subject modules directly.
A fresh checkout therefore does not contain the complete transition. Fix the
remaining integration and recommend a cohesive follow-up commit, not restoration
of the rejected loader. Inspect `git diff` and `git show HEAD:<file>` separately.

Important working-tree dependencies include:

```text
_targets.R
scripts/process_data/generate_panel_air_quality.R
scripts/process_data/prepare_station_temporal.R
src/city_specific/processing.R
src/general_utilities/reproducibility.R
src/general_utilities/config_utils_process_data.R
src/general_utilities/config_utils_plot_tables.R
tests/testthat/test-rendering-checkpoints.R
tests/testthat/helper-temporal.R                    # untracked
```

`scripts/run_targets.R` is also still untracked, as are several earlier scientific
modules and tests. Inspect the full status before proposing commits; a local pass
using untracked dependencies does not establish a runnable published checkout.

## Temporal batch: implemented, but verification is unfinished

The working-tree recipes expose four city computations and separate saving:

- `generate_panel_air_quality.R`: original shape/raster reads -> named `panel_*`
  objects through `process_merra2_region_hourly()` -> four aerosol CSVs.
- `prepare_station_temporal.R`: those CSVs and original balanced station RDS files
  -> `pm25_*` through `convert_and_add_pm25()` -> `series_*` through
  `combine_station_merra2_pm25()` -> four converted and four joined CSVs.
- Shared choices in `config/analysis_settings.R`: extraction `"mean"`, parallel
  execution enabled, and automatic core selection. Small verification runs override
  parallel execution to `FALSE`; this does not verify the parallel branch.
- `_targets.R` now separates city input reads, computations and writers. Existing
  public selections `generate_panel_air_quality`, `prepare_station_temporal` and
  `temporal` remain. Temporal figures consume their own city's saved series.
- `manuscript_city_config()` moved to `src/city_specific/processing.R` and
  `prepare_paper_export()` to `src/general_utilities/reproducibility.R`.
- The old process/plot setup files now contain their package vectors directly.
  Some optional workflows still use these broad setup files and their existing
  installation behavior; do not describe all repository setup as fully migrated.

Preserve the original station/geography contracts. Santiago uses `date2_hour` and
`pm25_validated`; other cities use `datetime` and `pm25`. São Paulo selects stations
using the original station geography's `sttn_cd`. Preserve current timestamp
handling, duplicate MERRA hours, all-missing station means and left-join support.
Timezone reform is a deferred scientific task, not part of this refactor.

### Test result requiring action

The last saved synthetic run reports **673 passes, 5 failures, 0 errors, 0 skips,
and 0 test warnings**. This was not rerun while writing this handoff.

All five failures are in `tests/testthat/test-station-temporal.R`, around line 60:
the four-cell raster means multiplied by `1e9` differ slightly from hand values
at tolerance `1e-10` (for example, `3.5000000675` versus `3.5`). Raster precision
conversion is a hypothesis, not a confirmed diagnosis. Trace the discrepancy;
do not loosen scientific tolerances or change the extraction method just to pass.
An exactly representable synthetic fixture may be appropriate if justified, but
preserve the existing comparison inputs and results before changing the fixture.

`helper-temporal.R` writes a **GeoTIFF with a MERRA-style `.nc4` filename** using
terra. This tests raster extraction, layer names and filename dates; it does not
test native netCDF decoding. It avoids a new netCDF-writing dependency. Keep this
limit explicit and test real netCDF files separately when sources are available.

Other assertions in that run covered manual/targets agreement on 12 CSVs, no-op
reruns, deleted-output regeneration without recomputing objects, city-specific
station-file changes, added/removed raster days, geographic sidecars and settings
changes. These successes do not cancel the five failures or prove real-data parity.

### Local evidence to resume, not overwrite

The ignored directory `data/verification/reader-temporal-20260930/` contains:

| Item | Meaning |
|---|---|
| `before/`, `revision.txt` | 56 R files preserved before the temporal edits; incoming HEAD `d30244e` plus then-current working-tree changes. |
| `checkpoint-hashes.json` | Recorded SHA-256 hashes of 12 existing temporal CSVs; recheck before asserting preservation. |
| `fixture/`, `baseline/`, `baseline.rds` | Small source fixture and results from the preserved implementation. |
| `manual/`, two `*-manual.rds` files | Results of the revised recipes in fresh R processes on redirected fixture roots. |
| `targets/`, `isolated-targets.R`, `store/` | Isolated execution of 51 actual target commands; log reports 51 completed, 0 skipped. |
| Four `*-converted-baseline.rds` files | Preserved conversion results from the existing real city aerosol panels. |

Each of `baseline/`, `manual/` and `targets/` contains 12 CSVs. The existence of
these files is verified; the final old/manual/targets comparison and real-panel
conversion comparison still need completion and a durable result record. This
dirty-tree snapshot is refactor evidence, not a reviewed scientific release baseline.

Temporary drivers are under `tests/_cache/`: `baseline_reader_temporal.R`,
`run_reader_temporal.R`, and `targets_reader_temporal.R`. Read before reusing them:
some recreate fixtures or overwrite outputs. Do not rerun the rewrite/reconnect
Python generators; they were one-time edits and are not pipeline commands.

Existing logs, which may disappear from the local temporary directory:

```text
/private/tmp/baseline-reader-temporal.log
/private/tmp/manual-reader-aerosols.log
/private/tmp/manual-reader-temporal.log
/private/tmp/targets-reader-temporal.log
/private/tmp/synthetic-reader-temporal.log
```

The last log also contains sandbox CPU-probe and temporary-file cleanup messages
outside testthat's warning count. Report environment limitations separately.
Local evidence and temporary drivers are not guaranteed to exist in another clone.

### Source and environment limits

These original source directories are absent in this working copy:

```text
data/raw/merra2_aerosol_products
data/raw/cities_shapefiles
data/raw/pollution_ground_stations
```

Saved city aerosol, converted PM2.5 and joined station CSVs exist. They support
bounded downstream checks, not source regeneration. Do not substitute the current
cleaned pollution datasets or census geographies for these historical inputs.

Use the same R executable as RStudio. Earlier plain `Rscript` selected a different
installation and could not find `here`. Ordinary project startup also stalled
in the agent environment. The prior synthetic run used this fallback from the
project root, without changing the lockfile or installing packages:

```sh
TMPDIR=/private/tmp \
R_LIBS="$PWD/renv/library/macos/R-4.6/aarch64-apple-darwin23" \
/Library/Frameworks/R.framework/Resources/bin/Rscript --vanilla \
  tests/testthat.R --mode=synthetic
```

Verify the executable/library paths on the next machine. `--vanilla` bypasses
normal startup; this run does not establish that ordinary renv activation works.

## Finish the plan in this order

1. **Close the temporal batch.** Diagnose the fixture failures; compare frozen old,
   manual and targets outputs in isolated roots; compare existing real-panel
   conversions; verify input hashes and function behavior. Finish line-length,
   script-readability, target-contract and inventory checks. Run the synthetic
   suite again. Record exactly which sources and execution branches were tested.
   Update active status in architecture, migration, inventory and implementation
   evidence. Mark missing-source prerequisites explicitly rather than claiming
   full reproduction. Ensure deleted pipeline files have no active callers.
2. **Finish optional analysis and legacy-validation recipes.** Inspect what already
   changed before extracting anything else. Remaining families include resolution
   preparation/estimation/figures, satellite/context maps, manual INEGI/heatmap/
   historical station figures, acquisition and legacy reports. Preserve optional
   commands, frozen inputs, scope distinctions and the Quarto report's narrative.
   Do not resolve scientific source/vintage ambiguity by silently choosing inputs.
3. **Relocate operational entry points.** Move tracked export and verification
   commands from `scripts/export/` and `scripts/verification/` to
   `tools/reproduction/`. Update every caller, Docker configuration, provenance
   inventory, test, ignore rule and active documentation together. Preserve the
   ignored local export script. Extend entry-point coverage beyond `scripts/`;
   keep exactly one inventory row per entry point, with command, inputs, outputs,
   consumers, readiness and next action. `tools/reproduction/` does not exist yet.
4. **Finish navigation and packaging.** Preserve the accepted first-run guide;
   keep README focused on one analysis and the full workflow. Put operational,
   agent and historical details off the introductory route. Keep the planning
   record public and dated; exclude it and host tooling from runtime contents
   where verification does not need them. Verify actual image/release contents.
5. **Scientific acceptance, then automation cutover.** Run isolated full verification,
   inspect manuscript links/rendering and establish a reviewed comparison baseline.
   Test missing outputs, source membership changes, settings/functions, manual
   agreement and unaffected-city caching. Only after acceptance remove Makefile and
   RStudio's Make setting; reduce `run_pipeline.R` to a targets compatibility launcher.
   Translate useful optional Make commands into documented independent R/CLI routes.

The current isolated verification command is:

```sh
Rscript tools/reproduction/verify.R --full --targets
```

Update that path after relocation and follow [the run guide](../HOW_TO_RUN.md).
No full run is claimed here. The comparison manifest
`config/verification_comparisons.csv` currently has only its header. The missing
manuscript appendix remains a recorded unresolved prerequisite; recheck availability.
Do not repeat older claims that Santiago's three preserved geographic responses or
São Paulo's weighting-area source are still missing: later evidence records them
as available. The transitional runner shares functions with targets and is not an
independent scientific baseline.

Keep the deferred timezone review, Santiago 2024 urban-conurbation versus whole-commune
population choice, and improved imputation methodology in `remaining-work.md`.
Gran Santiago is the preferred definition; do not force equal commune counts across
vintages. Santiago 2017 remains the main specification. Review paired census products
after the approved missing-education retention change; do not impute education.
Preserve the approved first-finite-reading imputation window, skipped unnecessary
fits and `OLS_imputed` label while migrating structure.

## Review groups and handoff reporting

Recommend groups only. The next temporal implementation group should be reviewed
with its already-committed prerequisites; do not blindly restage this list:

**`refactor: finish temporal recipes and remove pipeline loader dependencies`**

```text
_targets.R
config/analysis_settings.R
scripts/process_data/generate_panel_air_quality.R
scripts/process_data/prepare_station_temporal.R
src/general_utilities/process/merra2.R
src/city_specific/processing.R
src/general_utilities/reproducibility.R
src/general_utilities/config_utils_process_data.R
src/general_utilities/config_utils_plot_tables.R
tests/testthat/helper-temporal.R
tests/testthat/test-station-temporal.R
tests/testthat/test-rendering-checkpoints.R
tests/check-reader-workflows.R
tests/check-targets-contracts.R
tests/check-targets-engine.R
```

**`docs: record temporal verification and remaining migration work`**

```text
doc/ai/architecture.md
doc/ai/implementation.md
doc/planning/targets-migration.md
doc/planning/remaining-work.md
doc/planning/targets-migration-handoff.md
```

Only include files with relevant remaining changes. Audit older untracked
prerequisites separately; these lists do not account for the entire dirty tree.
Later groups should separate optional recipes, operational relocation and final
cutover, with exact file lists derived from their actual diffs.

On completing each group, report what changed, which objects a reader can inspect,
checks actually run, failures/skips, evidence locations and scientific limits.
Update this handoff rather than accumulating competing status files. The previous
station/figure batch's recorded 646 synthetic passes and 561 reader checks apply
to that earlier state; they are not the current temporal batch's result.

</details>
