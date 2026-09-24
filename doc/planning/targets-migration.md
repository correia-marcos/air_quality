# Readable R scripts and a simpler targets pipeline

Approved revision, 23 September 2026. **The distance pilot is implemented and its
readability accepted by Marcos; the four city recipes and their targets are now revised.**
Remaining analytical recipes, tooling and scientific cutover are pending. This replaces
the earlier requirements for analytical scripts to use pipeline loaders, specification
tables, stage adapters and input/output dictionaries. The collapsed historical record
below is superseded, including its earlier commit groups and environment status.

## The reader's route

Open `scripts/process_data/generate_distance_matrices.R`. Section I sources the
scientific functions and plain settings, names nine input files and reads `sf` objects.
Section II makes five explicit computations. Section III saves those results in the same
order. There is no extra inspection block or targets-cache requirement.

The defining function is in `src/general_utilities/process/distances.R`; shared choices
are in `config/analysis_settings.R`. The latter currently contains the migrated distance
choices and target seed. Other shared settings move there with their respective workflows,
without changing their values. City-specific definitions remain in the city modules.

The [first-run guide](../guides/first-run.md) works a 3–4–5 km station triangle by hand.
The [IDW example](../reference/idw_golden_test.md) remains the model for scientific
explanations: question, definition, small example, expected result and code connection.
The [entry-point inventory](remaining-work.md) is a maintainer reference, not onboarding.

## Pilot interface and preserved behavior

- `compute_distance_matrices()` returns the two distance tables and optional points/CRS.
  It no longer accepts `out_dir`, `out_name` or `overwrite`, reads old checkpoints, or
  writes analytical products. Its existing temporary GDAL conversion of curved geometries
  is retained, with cleanup on exit.
- `write_distance_matrices(result, out_dir, out_name = "matrix", overwrite = TRUE)`
  saves the applicable tables and returns their paths. `overwrite = FALSE` fails before
  writing either table when a destination exists. Read old files explicitly with Arrow.
- All repository callers use the split API, including INEGI preparation and resolution
  sensitivity. External scripts must move their output arguments to the writer; no
  compatibility wrapper is provided.
- Bogotá uses 2018 tracts (`GEO_ID`); CDMX uses 2024 municipalities (`CVE_MUN`) with
  the 2020 census context; Santiago uses 2017 zones (`zona_id`) and 2024 communes (`CUT`),
  both with the 2017 station selection; São Paulo uses 2010 weighting areas (`code_weighting`).
- Preserve AEQD, internal polygon points, station normalization, row ordering, schemas,
  filenames and geographic membership. Source acquisition and analytical definitions do
  not change.

`_targets.R` now contains ordinary declarations. Each distance context has input-reading,
computation and file-writing targets; Santiago shares its station-reading target.
`bogota_2018_distance_matrices`, for example, holds data while `bogota_2018_distances`
owns the two Parquet files. Existing distance-stage selections remain available.
The graph factory, custom declaration parser and distance adapters have been removed.
The graph loader and remaining analytical adapters are transitional.

## City preparation — implemented

The four city scripts now show local sources and station reads in Section I, scientific
calls and named spatial objects in Section II, and ordered saves in Section III.
They source the required modules directly. All script and graph lines fit 92 characters.

Bogotá's geographic preparation returns spatial objects without an output argument.
Other geographic and station functions accept `out_file = NULL`; `write_geopackage()` saves
their results. Census functions retain their existing data-return mode and add
`return_data = FALSE` for file-backed processing. Optional summary reads are shown in the
guide, using returned paths. The 20 km radius and UTC processing timezone have named city
configuration entries; their values and all previous configuration entries are unchanged.

The 24 September correction places census extraction/output paths in Section I and keeps
Section III for saving alone. Previews and reconstructed output inventories were removed
from all four recipes. Targets still checks its owned files independently.

Bogotá now separates `bogota_download_geography()` (acquire sources) from
`bogota_prepare_metro_area()` (read local sources and return geography). Scripts, targets
and `city_process()` use the latter; separate writers own the derived GeoPackages. The
download recipe retains an explicit preparation call for its source-region diagnostic.
The old `bogota_download_metro_area()` API was removed and repository callers updated:
external callers must choose acquisition, preparation and saving explicitly.

CDMX, Santiago and São Paulo still combine acquisition and preparation behind
`allow_download`; their separation remains a follow-up. Preserve each city's specific
source handling and selection rules when applying the Bogotá pattern.

Targets calls the scientific functions directly and caches spatial objects separately
from file writers. The complete `city_process()` interface remains available. Public
geography, station, pollution and census stage names and output filenames are unchanged.

All five real station selections agree with the preserved implementation and the target
commands. Synthetic checks cover offline geographic fixtures, file-backed census returns,
spatial caching and writer behavior. Full source-to-output city parity remains unverified;
see [the city-preparation evidence](../ai/implementation.md#city-preparation--23-september-2026).

## Remaining implementation

1. Apply the accepted recipe to outliers/IDW/exposure, summaries and
   rendering, then optional analysis and validation. Keep short repeated calls; share
   transformations, not script-shaped orchestration. Use meaningful names, named arguments
   and space between operations. Follow the author's call layout and step comments,
   recorded in [R style](../ai/rules/r-style.md#spacing-calls-and-comments).
   Computations return manageable objects; writers return
   actual files. Large streaming/partitioned datasets may compute and write together;
   retain their returned objects or paths without adding duplicate saves or reports.
2. Move genuine scientific operations into existing subject modules, put paths and reads
   back in scripts, and retire remaining pipeline machinery after its callers migrate.
   Retain complete `city_process()` convenience wrappers, registry consumers, offline
   preparation, error propagation and all supported city/census variants.
3. Keep targets confined to the manuscript, including its existing temporal MERRA inputs.
   Preserve distance → outlier dependencies, full-dataset ownership, cross-year windows,
   serial shared IDW writes and seeds. Track actual inputs, directory membership, sidecars,
   named settings and all outputs. Never source recipes or use hidden `tar_read()` calls
   inside scientific functions. Preserve public selections and artifact exports.
4. Move verification/export entry points to `tools/reproduction/`, updating callers,
   Docker, tests, provenance and ignore rules together. Preserve the ignored local export
   script. Keep optional/frozen workflows as separate commands and extend inventory
   coverage to relocated entry points. Absence from targets does not justify deletion.
5. Shorten README/onboarding around scripts and scientific examples. Keep detailed
   inventories, operational procedures, agent rules and history available off that route.
   Keep one public repository; exclude planning/history and host tooling from runtime
   contents while retaining the inventory and evidence verification actually uses.
6. After scientific acceptance, remove Makefile and RStudio's Make setting, reduce
   `run_pipeline.R` to a targets compatibility launcher and retire unused transitional
   plumbing. Until then the existing scheduler remains transitional, not an independent
   comparison baseline: it shares scientific functions with targets.

## Verification and limits

Before editing, code and old results were preserved under the local ignored directory
`data/verification/distance-pilot-20260923-192149/`. This is a refactor-comparison record,
not the reviewed release baseline. Its `baseline.R` and `compare.R` record the commands.

The real-data comparison reports zero numerical differences in both tables for all five
contexts (3,363,768 geographic pairs). It also checks identifiers, order, schemas, zero and
3/5/20 km membership, representative points/CRS, Parquet round-trips, and agreement between
the actual recipe sections and the production distance target commands. Only output roots
and upstream preparation were replaced for the isolated target run: it consumed the nine
existing prepared inputs and did not reproduce their source preparation.

The 29 recorded geographic/input-product/lock hashes are unchanged. Target tests exercise
no-op reruns, either output deleted, changed geography, shared settings/function changes,
unaffected-city caching and writer failure. Scientific tests retain the hand-worked IDW
fixture and the fixed-CRS resolution checks.

Final test results and environment limitations are recorded in
[implementation evidence](../ai/implementation.md#distance-pilot--23-september-2026).
The automated pilot ran in clean R processes, not the RStudio UI. Marcos subsequently
verified the pilot, accepted its readability and refined its comments and layout.
That review clears the readability gate for extending the pattern; it does not establish
full source reproduction.

Full reproduction and cutover remain pending: three Santiago 2017 geographic responses,
São Paulo's weighting-area source, a reviewed comparison baseline and the manuscript
appendix are still unavailable. INEGI's 2020/2024 geographic-contract discrepancy is not
resolved by updating its API call. No raw/download/legacy inputs, dependency versions or
lockfile were changed. Optional resolution definitions and frozen checksums remain intact.

## Pilot review groups

The city-preparation batch has its own review groups in
[the implementation record](../ai/implementation.md#city-preparation-review-groups).

The subsequent readability/style follow-up belongs in these same review groups.
If reviewed separately, the suggested subject is `docs: adopt the author's R script style`:

```text
scripts/process_data/generate_distance_matrices.R
doc/ai/rules/r-style.md
doc/ai/architecture.md
doc/ai/workflows/new-process-script.md
doc/ai/workflows/new-figure.md
doc/ai/workflows/review-r.md
doc/ai/roles/r-reproducibility-reviewer.md
doc/ai/implementation.md
doc/planning/targets-migration.md
doc/planning/remaining-work.md
```

These groups describe this pilot's changes relative to the preserved **dirty working
tree**, not independent commits from HEAD. Earlier pending migration prerequisites must
also be reviewed before a human assembles runnable commits. No staging, commits or pushes
were performed. The local `pilot.patch` isolates this batch from the earlier work.

**Suggested subject: `refactor: separate distance computation from Parquet saving`**

```text
config/analysis_settings.R
scripts/process_data/generate_distance_matrices.R
scripts/process_data/generate_inegi_lab_inputs.R
src/general_utilities/process/distances.R
src/general_utilities/process/resolution_workflow.R
tests/testthat/test-distance-matrices.R
tests/testthat/test-integration.R
tests/testthat/test-resolution-sensitivity.R
```

**Suggested subject: `refactor: declare distance targets and checkpoints directly`**

```text
_targets.R
scripts/run_targets.R
src/pipeline/core.R
src/pipeline/load.R
src/pipeline/graph.R (removed from the previously untracked candidate)
tests/check-reader-workflows.R
tests/check-targets-contracts.R
tests/testthat/test-distance-targets.R
```

**Suggested subject: `docs: make direct RStudio analysis the default reading path`**

```text
doc/HOW_TO_RUN.md
doc/ai/architecture.md
doc/ai/implementation.md
doc/ai/roles/r-reproducibility-reviewer.md
doc/ai/rules/r-style.md
doc/ai/workflows/add-city.md
doc/ai/workflows/new-figure.md
doc/ai/workflows/new-process-script.md
doc/ai/workflows/review-r.md
doc/guides/first-run.md
doc/planning/remaining-work.md
doc/planning/targets-migration.md
```

<details>
<summary>Superseded migration record — historical instructions and file groups</summary>

# City processing interfaces and manuscript targets migration

Approved scope, revised 2026-09-23. **Implementation candidate; scientific acceptance and
default cutover remain pending.** This supersedes the earlier illustrative proposal, which
used obsolete paths, recursive sourcing, opaque city dispatch and byte-only comparisons.

Keep one public repository. Maintain this record until acceptance, then mark it completed or
superseded and link verification evidence. Exclude it from runtime images; do not delete the
scientific development record merely to reduce image size.

## Reader-first revision — 23 September 2026

The approved RStudio requirement supersedes the earlier thin-script design: readers source
functions, read files, inspect named intermediate objects, and follow the transformation
without learning targets machinery. It is the first structural acceptance criterion in
the canonical architecture, R style, workflows and reviewer guidance.

Implementation now restores the 28 manuscript recipes and reviews all 56 R entry points and
the Quarto report. Each R script has three/four ordered sections; meaningful operations live
in shared functions. Buffer/city/pollutant loops retain named result collections. Optional
resolution preparation, estimation and plotting now expose separate scientific operations;
reference versus multicity definitions and frozen-input guards are preserved.

The graph uses 109 literal `tar_target()` declarations, including 13 renderer input
checkpoints. There is no command-construction/dependency dictionary. The small declaration
inspection helper reads those declarations; it is not a second graph. Existing stage
selection names remain available. Aggregate selections track files so changed contents
cannot be hidden by identical filenames. Renderers directly reference file dependencies
as well as their input-object checkpoints. Dataset and IDW writing ownership remains unchanged.

Every script/report is recorded in [remaining-work.md](remaining-work.md), including direct
commands, targets or separation reasons, inputs, consumers, readiness and next actions.
Coverage tests replace the old hard-coded unwired list. Only this executable inventory is
included from doc/planning in Docker, because runtime verification needs it; proposals stay
outside the image. Verification provenance records the inventory.

The intended end state is targets alone for manuscript scheduling. **Do not apply the older
step 7 instruction to retain manuscript Make recipes after cutover.** After scientific
acceptance, remove Makefile and the RStudio Make build setting, make run_pipeline.R a
compatibility launcher, and use the inventory's direct optional commands. Until then the
existing Makefile and sequential runner remain explicitly transitional. Their shared
functions prevent treating them as an independent baseline.

Current checks, failures and unperformed acceptance checks are recorded in
[implementation evidence](../ai/implementation.md#reader-first-targets-revision--2026-09-23).
This structural implementation does not establish numerical or rendered-output parity.

## Scope and interface

Migrate the manuscript pipeline first, including temporal MERRA-2 inputs consumed by its figures.
Preserve acquisition, resolution sensitivity, satellite comparison, context-map and validation
entry points. Frozen resolution inputs retain their checksums and are not regenerated here.

The common interface is:

```r
city_process(id, steps = c("geography", "stations_filter", "pollution_parquet", "census"),
             inputs = NULL, quiet = FALSE)
```

Selected stages include prerequisites in dependency order. `inputs = NULL` resolves configured
local sources; explicit inputs override whole named stage contracts. Missing sources fail
preflight; processing never initiates acquisition. Required failures propagate immediately.
Every stage returns named paths covering all owned products, including external sidecars and
all census variants. Output roots derive from configuration. Bogotá's historical `census_2005`
and `census_2018` selectors remain aliases.

| City | Preparation retained |
|---|---|
| Bogotá | Five geographic products; both 2005/2018 station selections; pollution uses 2018; basic and extended 2005 plus 2018 census, individual and collapsed. |
| CDMX | Municipality and AGEB geography; existing station-name corrections; combined pollution sources; extended 2020 census. |
| Santiago | Separate 2017/2024 geography; station selection uses 2017; both census vintages with `zona_id` and `CUT` keys respectively. |
| São Paulo | Municipalities, tracts, weighting areas; station filtering; pollution; individual/collapsed 2010 census. |

Preserve the 20 km station buffer, configured years, source precedence, UTC handling, geographic
definitions, weighting and missingness. Distance matrices, outliers, IDW and exposure remain
downstream stages. CDMX full-dataset writing now removes old partitions before export, as the
other city writers already do, so removed years cannot persist after recomputation. This changes
rebuild behavior, not the fresh-run estimator; numerical equivalence still needs execution.

Retain the registry for configuration and supported entry points, including legacy `city_cfg()`
consumers. `get_city()`, `list_cities()` and manual `city_process()` remain. Processing registration
is mandatory and validated; duplicate registration fails. `city_download()` reports unsupported
capabilities clearly. Unused `read_raw` and `normalize` slots are removed. An explicit loader
initializes cities once. The registry does not cache processing results.

## Dependency and return contracts

```text
preserved sources → geography/census → station selection → partitioned pollution
  → distance matrices → outliers → IDW/exposure → summaries → figures/tables → export
```

The graph declares concrete stage calls, city configurations and upstream targets directly. It does not cache an opaque `city_process(id)` command. Static construction needs only
`targets`, without `tarchetypes`. Manual scripts and targets share reusable functions in `src/`.
The explicit loader never recursively sources setup/install scripts.

- Manageable tables, `sf` objects, matrices and plots retain data returns in transformation
  functions. Stage adapters assign useful local results and return owned files.
- Named assignments alone do not establish dependencies; downstream commands reference targets.
  Logging, directory creation, database setup and deliberate writes remain side-effect calls.
- Large year-partitioned Parquet datasets stay file-backed; consumers reopen Arrow. No live
  Dataset handle or whole collected pollution dataset is stored in the targets cache.
- One owner per complete station dataset and city/vintage IDW family preserves year-boundary
  outlier windows and serial education/income/buffer reuse. No competing shared-output branches.
- File targets track source-directory membership and geographic sidecars. File storage removes
  vector names: `processing_file_roles()` reconstructs roles by matching explicit paths before
  city adapters select inputs/vintages; it does not assume cached order.
- Process adapters return exclusively owned directories. Renderers report concrete outputs,
  including nonselected companions, and check manuscript products against the manifest. Shared
  figure/table directories are not file-target outputs. Missing required plots fail.
- Census extraction refreshes owned checkpoints so an internal extraction cache cannot mask
  changed source files. Multi-file writers expose every path. Eight missingness calls now retain named tables;
  manageable geographic/station/census/plot results are assigned in adapters. Existing IDW path
  returns and its optional in-memory mode remain. Optional resolution helpers returning filename
  stems need concrete file contracts when that separate workflow migrates.

Explicit branches cover imputation, descriptive and distance-band summaries, station scatters,
and temporal preparation. Outliers depend on distance matrices; Make's missing prerequisite is
corrected. The threshold-hours manifest producer now points to `render_station_tables.R`.
Verification provenance includes `_targets.R`, configuration, target metadata and the cache path.

## Steps and responsibility

These are responsibility recommendations, not claims of separate agent reviews. Astra retains
interface architecture, scientific mappings, parity criteria and final cutover acceptance.

| Step | Candidate | Current implementation and required verification |
|---|---|---|
| 1. Contracts and comparison baseline | Astra | Sources, owners and dependencies mapped. Supply missing preserved inputs and a reviewed baseline; historical outputs do not become a baseline by assumption. |
| 2. Bogotá and registry | Astra | Complete stages, aliases, configured paths, registration validation and fail-fast returns implemented. Verify five geographies, both station selections and every census variant against identical sources. |
| 3. Other city wrappers | Astra; Terra for mechanical extraction | All three interfaces complete. Verify scientific geography, station corrections, source precedence, census keys/counts/weights/missingness and pollution contents. Acceptance stays with Astra. |
| 4. Dependencies, loader and pilot | Terra; Astra architecture review | Explicit loader and `bogota_geography` pilot implemented. Targets 1.12.0 loads locally. Reconcile the user-installed lockfile with the full scientific environment; real geography parity remains unexecuted. |
| 5. Analytical graph | Astra | 96 targets; explicit distance/outlier/IDW and auxiliary manuscript branches; complete dataset ownership. Engine fixtures pass. Require real stage-level numerical parity and invalidation checks. |
| 6. Rendering and consumers | Terra; Astra for calculations | 21 shared downstream adapters, file returns, manifest correction and canonical export implemented. Verify complete ownership, rendering, export hashes and manuscript includes. |
| 7. Make, Docker, verification and cutover | Astra; Terra for plumbing | Candidate commands, cache mounts and isolated verification route implemented. Default runner stays until acceptance. Then redirect manuscript Make recipes to target selections and replace `run_pipeline.R` with a compatibility shim, removing its second graph. |
| 8. Documentation and packaging | Luna for specified edits; Terra for packaging; Astra for evidence exclusions | Interface/run instructions and ignore rules updated. Verify actual image contents; retain methodological evidence, test inputs and verification provenance. |

Implementation: [`processing.R`](../../src/city_specific/processing.R),
[`preparation.R`](../../src/city_specific/preparation.R),
[`registry.R`](../../src/city_specific/registry.R),
`src/pipeline/graph.R` (retired; declarations now in [`_targets.R`](../../_targets.R)),
[`core.R`](../../src/pipeline/core.R),
[`stages`](../../src/pipeline/stages), [`_targets.R`](../../_targets.R),
[`runner`](../../scripts/run_targets.R), [`verification`](../../scripts/verification/verify.R).
Commands: [HOW_TO_RUN](../HOW_TO_RUN.md#candidate-manuscript-migration-to-targets).

## Evidence and acceptance limits

Reference scripts were inspected at `f8124e21dc146c6f9795d767bc024e0ffeb16784`. Their scientific
definitions remain the comparison reference. Refactoring and fixtures are not reproduction.

Checked during implementation on 2026-09-22–23:

- The actual targets engine loads/inspects the 96-target graph without processing data.
- All 21 extracted downstream stage bodies match the reference scripts after normalizing
  deliberate input validation, named-result assignment and output-path reporting. This static
  comparison does not establish numerical equivalence.
- The command-line `all --outdated` route succeeds in a separate R process and selects the
  90 manuscript targets/ancestors, excluding six optional convenience aliases.
- 145 R files parse. All three Compose configurations parse using the standalone Compose CLI.
  Updated run/architecture/plan Markdown links resolve; all 128 manifest producers and existing
  artifact files are present. The manuscript scanner finds no unmapped/unused manifest paths,
  but fails complete coverage because `data_appendix.tex` is absent. Presence does not establish
  freshness or successful regeneration.
- [`check-targets-contracts.R`](../../tests/check-targets-contracts.R): 109 assertions passed,
  covering interfaces, mocked geography wiring, prerequisites, missing inputs, failure propagation,
  aliases and graph contracts. Mocks verify arguments/ownership, not spatial correctness.
- [`check-targets-engine.R`](../../tests/check-targets-engine.R): 24 assertions passed with
  targets 1.12.0 and tiny isolated text fixtures. Checks cover input edits, directory-member
  additions/removals, deleted outputs, configuration/function changes, no-op reruns, unaffected
  branches, selected builds and named-file roles. They do not compare Parquet contents.
- `Rscript tests/testthat.R --mode=synthetic` was attempted. Normal renv activation waited on
  a sandbox lock and was interrupted. With autoload disabled and existing libraries selected,
  R crashed in `brio::read_lines()` before assertions (exit 139).
- `Rscript scripts/verification/verify.R --full --targets` was attempted with autoload disabled.
  R crashed loading a compiled dependency of `sf` (exit 139), before rebuilding or completing a
  report. A separate check also found Docker's default daemon endpoint unavailable.
- Successful native fixtures emitted a temporary-directory cleanup permission message at process
  shutdown. Assertions passed and processes returned zero.

Required before cutover:

1. Restore a usable full scientific environment while preserving original versions unless
   reviewed separately. The user's installation changed `renv.lock` from 175 to 48 packages,
   removing 132 original packages including Arrow, sf, DuckDB and testthat. The agent has not
   edited this protected input; installation intent was queried. Loading targets does not
   validate this reduced lockfile for scientific execution.
2. Supply the three Santiago 2017 responses and São Paulo's weighting-area RDS declared in
   [`input_sources.csv`](../../config/input_sources.csv). Do not substitute newer geographies.
   Obsolete raw paths and external-machine legacy paths require separate explicit decisions.
3. Run original scripts and new adapters on identical sources with isolated output roots. Hash
   sources before/after. Compare geography membership/CRS/IDs; station selection/names; census
   schemas/counts/weights/missingness; pollution partitions/keys/timestamps/values.
4. Compare distances, outlier windows/flags, IDW/exposure and every auxiliary branch with
   documented tolerances. Parquet metadata may contain revision/run information; byte equality
   alone is insufficient. Do not widen scientific tolerances without justification.
5. Verify complete, unique ownership and regeneration after deletion; test real partition/source
   membership changes, function/configuration changes and unaffected cities. Repeat no-op runs.
6. Pass portable checks and fresh offline verification. Register genuine revision-matched
   comparisons in [`verification_comparisons.csv`](../../config/verification_comparisons.csv),
   currently empty; record baseline provenance, human review and independent reproduction.
7. Inspect plot data, rendering, exported hashes and external manuscript links. The missing
   `doc/paper/data_appendix.tex` prevents full manuscript coverage; no appendix was invented.
8. Complete the Make/compatibility-shim cutover only after acceptance; preserve optional entry
   points and frozen resolution checksums throughout.

## Publication and packaging

Maintain one public development repository; make execution distributions leaner. `.dockerignore`
now omits the targets cache, disposable test caches, planning files, host AI configuration and
harness tooling/tests. Scientific source, specifications, manifests, fonts, methodological tests
and `doc/ai` verification evidence remain. The release Compose route uses the same image.
Git ignores `_targets/` and the actual `results/figures/maps/*_files/` assets.

No tracked results, scientific evidence, preserved sources or generated MERRA CSVs were removed
or untracked. Ignore rules do not remove tracked files. Consider removing historical MERRA CSV
checkpoints only after demonstrated regeneration/equivalence. Retain manuscript-selected outputs
until a reviewed release baseline permits a smaller distribution. A future source archive needs
an explicit export allowlist, not broad Git exclusions for methodological records.

## Recommended human review groups

Recommendations only: the agent did not stage, commit or push. The dependency group is
blocked on reconciliation of the user-owned lockfile; do not accept its current package removals
as part of this refactor. The shared-stage group is deliberately kept together because the
explicit loader references all its modules. Review scientific mappings before orchestration.
These lists cover the combined migration working tree, including earlier user-owned changes;
this reader-first revision did not change dependencies. They recommend human-created commits
only and authorize neither agent `git commit` nor `git push`.

### `build: pin targets while preserving the scientific environment`

```text
DESCRIPTION
renv.lock
```

### `refactor: share complete offline city and manuscript stages`

```text
config/paper_artifacts.csv
scripts/download_data/download_bogota_data.R
scripts/download_data/download_cdmx_data.R
scripts/download_data/download_merra2_data.R
scripts/download_data/download_santiago_data.R
scripts/download_data/download_sao_paulo_data.R
scripts/process_data/build_bogota_localidad_crosswalk.R
scripts/process_data/compute_descriptive_tables.R
scripts/process_data/compute_distance_band_descriptives.R
scripts/process_data/compute_station_scatter_inputs.R
scripts/process_data/detect_outliers.R
scripts/process_data/estimate_exposure.R
scripts/process_data/estimate_exposure_imputed.R
scripts/process_data/estimate_idw.R
scripts/process_data/estimate_resolution_sensitivity.R
scripts/process_data/generate_distance_matrices.R
scripts/process_data/generate_inegi_lab_inputs.R
scripts/process_data/generate_panel_air_quality.R
scripts/process_data/impute_missing_hourly.R
scripts/process_data/prepare_resolution_inputs.R
scripts/process_data/prepare_station_temporal.R
scripts/process_data/process_bogota_data.R
scripts/process_data/process_cdmx_data.R
scripts/process_data/process_merra2_panels.R
scripts/process_data/process_santiago_data.R
scripts/process_data/process_sao_paulo_data.R
scripts/tables_images/figure_aerosol_composition.R
scripts/tables_images/figure_imputation_diagnostics.R
scripts/tables_images/figure_kernel_distributions.R
scripts/tables_images/figure_merra2_vs_stations.R
scripts/tables_images/figure_missing_heatmap.R
scripts/tables_images/figure_pollution_quintile_maps.R
scripts/tables_images/figure_pollution_stations_by_hour.R
scripts/tables_images/figure_population_density_maps.R
scripts/tables_images/figure_quintile_kernel_distributions.R
scripts/tables_images/figure_resolution_sensitivity.R
scripts/tables_images/figure_station_scatter.R
scripts/tables_images/figure_station_temporal.R
scripts/tables_images/figure_stations_on_metro_area.R
scripts/tables_images/figure_study_area_maps.R
scripts/tables_images/generate_exposure_plots.R
scripts/tables_images/plot_station_monitoring_figures.R
scripts/tables_images/render_census_tables.R
scripts/tables_images/render_exposure_tables.R
scripts/tables_images/render_missing_tables.R
scripts/tables_images/render_station_tables.R
scripts/validation_old_version/bogota_report.qmd
scripts/validation_old_version/compare_bogota.R
scripts/validation_old_version/compare_bogota_raw_ground_stations.R
scripts/validation_old_version/compare_cdmx_raw_ground_stations.R
scripts/validation_old_version/compare_ground_stations_data.R
scripts/validation_old_version/plot_bogota_quintiles.R
src/city_specific/bogota.R
src/city_specific/cdmx.R
src/city_specific/preparation.R
src/city_specific/processing.R
src/city_specific/registry.R
src/city_specific/santiago.R
src/city_specific/sao_paulo.R
src/general_utilities/config_utils_plot_tables.R
src/general_utilities/config_utils_process_data.R
src/general_utilities/config_utils_resolution.R
src/general_utilities/plot/exposure_figures.R
src/general_utilities/plot/resolution_workflow.R
src/general_utilities/process/exposure_regressions.R
src/general_utilities/process/resolution_workflow.R
src/general_utilities/validation/report_helpers.R
src/pipeline/contracts.R
src/pipeline/core.R
src/pipeline/graph.R
src/pipeline/load.R
src/pipeline/packages.R
src/pipeline/stage_names.R
src/pipeline/stages/compute_descriptive_tables.R
src/pipeline/stages/compute_distance_band_descriptives.R
src/pipeline/stages/compute_station_scatter_inputs.R
src/pipeline/stages/estimate_exposure.R
src/pipeline/stages/estimate_exposure_imputed.R
src/pipeline/stages/figure_imputation_diagnostics.R
src/pipeline/stages/figure_kernel_distributions.R
src/pipeline/stages/figure_pollution_quintile_maps.R
src/pipeline/stages/figure_population_density_maps.R
src/pipeline/stages/figure_quintile_kernel_distributions.R
src/pipeline/stages/figure_station_scatter.R
src/pipeline/stages/figure_station_temporal.R
src/pipeline/stages/generate_exposure_plots.R
src/pipeline/stages/generate_panel_air_quality.R
src/pipeline/stages/impute_missing_hourly.R
src/pipeline/stages/plot_station_monitoring_figures.R
src/pipeline/stages/prepare_station_temporal.R
src/pipeline/stages/render_census_tables.R
src/pipeline/stages/render_exposure_tables.R
src/pipeline/stages/render_missing_tables.R
src/pipeline/stages/render_station_tables.R
tests/check-targets-contracts.R
tests/testthat/test-targets-contracts.R
```

### `build: add candidate targets execution and verification`

```text
Makefile
_targets.R
docker-compose.release.yml
docker-compose.verify.yml
docker-compose.yml
scripts/export/export_paper.R
scripts/run_pipeline.R
scripts/run_targets.R
scripts/verification/check_manuscript.R
scripts/verification/run_stage.R
scripts/verification/verify.R
src/general_utilities/verification_cli.R
tests/check-targets-engine.R
tests/testthat/test-results-freshness.R
tests/testthat/test-targets-engine.R
```

### `build: exclude development caches from execution images`

```text
.dockerignore
.gitignore
```

### `docs: record migration contracts and pending acceptance`

```text
doc/HOW_TO_RUN.md
doc/ai/README.md
doc/ai/architecture.md
doc/ai/implementation.md
doc/ai/roles/r-reproducibility-reviewer.md
doc/ai/rules/r-style.md
doc/ai/workflows/add-city.md
doc/ai/workflows/new-figure.md
doc/ai/workflows/new-process-script.md
doc/ai/workflows/review-r.md
doc/planning/deletion-candidates.md
doc/planning/remaining-work.md
doc/planning/targets-migration.md
tests/check-reader-workflows.R
tests/testthat/test-pipeline-paths.R
tests/testthat/test-reader-workflows.R
```

</details>
