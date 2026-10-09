# Station-only manuscript migration — 5 October 2026

This is the dated 5 October implementation record. For subsequent human commits, approved
particulate screening/schema changes, current checks and pending files, read
[the 8 October continuation](migration-continuation-20261008.md). The extreme-value and
missing-appendix findings below describe the earlier checkout, not today's accepted treatment.

The station-only graph and tooling changes are implemented in the working tree. Scientific acceptance
and the default scheduler cutover are still pending. No files were staged or committed.
The starting checkout was `7e75514`, with 66 modified tracked files, 177 untracked files and
an empty index. Those figures describe the starting state, not this implementation's diff.
Earlier work was preserved, including optional resolution outputs and historical validation.

## What changed

`_targets.R` is now a 48-line entry point. Four city declaration modules and ten subject
modules under [config/targets](../../config/targets/) return literal lists of ordinary
`tar_target()` declarations. Scientific functions remain in `src/`. Common packages are
explicit; declarations do not depend on the MERRA module attaching `foreach`. City-specific
target names, file ownership, formats and useful stage selections are retained.

The graph has 340 targets: 52 satellite/old-input preparation targets were removed and 11
station-hourly/episode targets were added. Before writer simplification, the 318 unaffected
targets retained identical commands and formats. After simplification, those same targets
retain equivalent commands, rendering arguments, formats and dependencies on other targets.
The combined entry point and declarations contain 4,464 lines. Relocation principally
improves navigation; it does not make the scientific work disappear.

Thirty-five remaining inline Cairo PDF calls now use `save_plot_pdf()`. The writer accepts
explicit DPI, background and size-limit arguments. Existing dimensions and background
behavior are preserved. Twelve calls with inferred devices remain unchanged, including PNG
outputs. Font/theme setup remains at plot-building and rendering worker boundaries.

[prepare_station_hourly.R](../../scripts/process_data/prepare_station_hourly.R) exposes four
datasets, four named hourly summaries and four writers. Its reusable
[station summary function](../../src/general_utilities/process/station_temporal.R) filters
the existing analysis-year partition, rejects duplicate station/timestamp records and averages
finite observed readings with equal station weights. It retains reporting counts, missing
hours and the complete annual calendar. It keeps the stored UTC-labelled convention; it does
not resolve source-timezone interpretation or introduce imputation.

[figure_station_temporal.R](../../scripts/tables_images/figure_station_temporal.R) reads those
Parquets and produces the same five manuscript filenames. Episode calculations are visible
in a named table and saved as `data/processed/station_hourly/episodes_it2_2023.csv`. They use
PM2.5 >= 50, break at missing/nonconsecutive hours, and handle episodes ending at the final
row consistently. Legend units wrap onto a second line to avoid clipping in the PDF.

Satellite comparison functions moved to
[merra2_figures.R](../../src/general_utilities/plot/merra2_figures.R); their bar charts now
belong to the optional satellite recipe. Optional extraction uses generated metropolitan
geography and separate [satellite settings](../../config/merra2_settings.R). Its station join
consumes current hourly checkpoints. Context maps and the Santiago 2013 station-hour
diagnostic also use current geography/partitions. Historical balanced-panel inputs remain
in validation. Santiago 2017 and São Paulo 2010 census vintages remain approved inputs.

Make and `run_pipeline.R` now invoke station-only preparation on the default manuscript
route. Satellite extraction/joining remains under `make merra2`. Make and the RStudio build
setting are retained until the acceptance gate is met. Their continued presence does not
constitute an independent scientific baseline.

Verification, export, manuscript-reference checking and stage reporting moved to
[tools/reproduction](../../tools/reproduction/). Callers, Compose configuration, wrappers,
tests, code inventories and documentation were updated. A baseline-preparation tool creates
new snapshots with every approval flag false and refuses to overwrite existing baselines.
The comparison function now supports keyed CSV/Parquet tables, literal TeX, and GeoPackage
attributes with exact geometry/CRS. Rendering and plot-data review remain separate.

The tutorial already existed. Its setup and expected result are clearer, and the documentation
index explains the tutorial/how-to/reference/explanation distinction. Human documentation
uses connected explanations; native agent wrappers still point to concise canonical shared
requirements. [Harness guidance](../ai/harnesses.md) distinguishes instructions, native
controls, adapter tests, current managed settings and unverified live behavior. `CLAUDE.md`
remains necessary for installed Claude Code 2.1.267. It has the same content as `AGENTS.md`.

## Executed checks

| Check | Result and limit |
|---|---|
| Portable R suite | 734 passed; zero failures, errors, skips or test warnings. R 4.6.1 and its matching existing project library were selected explicitly; renv autoactivation was disabled. |
| Public dependency candidate | 167 R files parsed; all 340 targets constructed in an isolated copy containing public working-tree files and no derived or satellite inputs. This is a candidate-file check, not a repaired committed HEAD. |
| Graph relocation/writers | The 318 unaffected target commands, formats and target-to-target dependencies match the saved starting graph after normalizing equivalent writer calls. |
| Hourly numerical oracle | Independently calculated sum/count means and reporting counts match all four current-year datasets; maximum observed arithmetic difference was 2.14e-14. |
| Real temporal route | Direct scripts and actual temporal target commands agree within absolute tolerance 1e-10, with exact counts/missingness. Targets ran in fresh R workers against existing cleaned inputs, with isolated outputs/cache. |
| Cache behavior | No-op reruns preserve metadata; deleting an hourly checkpoint and a PDF recreates them without recomputing cached summaries/plots. |
| Rendering | The five direct-script and worker PDFs render identically at 1,200 pixels. All five were visually inspected; CDMX's extreme values remain an acceptance issue. |
| Source preservation | Hashes of the consumed station partitions remained unchanged. `renv.lock` and protected source roots have no tracked diff. No acquisition or source regeneration ran. |
| Harness/document tools | 13 harness tests and nine documentation tests passed. Working-tree link checks pass. Native Codex rules forbid add/commit/push and leave status unmatched; these evaluations execute no Git operation. |
| Export preflight | All 128 manifest artifacts validated in dry-run mode. Availability/export consistency does not establish numerical freshness. |
| Manuscript references | All available literal references are mapped; the checker still fails because local `data_appendix.tex` is missing. Full draft compilation was not performed. |
| Full isolated verification | Attempted with `--full --targets`; Docker exited 1 before build/rebuild because its daemon/socket was unavailable. `reproduction_verified` is false. |

The ordinary PATH selected a different R executable; a diagnostic invocation with the
R 4.6 project library crashed while loading native packages. The successful suite used the
matching framework executable. Do not interpret that crash as a numerical test failure or
the successful fallback as verification of the default local RStudio environment.

The earlier five MERRA failures were float-precision expectations in a decimal raster
fixture. The retained tests now separate an exactly representable hand-computed fixture
from a decimal fixture that explicitly checks the installed extractor's float32 output grid
and half-ULP rounding bound. The original decimal fixture remains available. No extraction
algorithm or scientific conversion factor changed. The removed graph integration contract
was replaced by tests of the current station-only graph and optional join. Native NetCDF
import and real optional satellite regeneration remain unverified.

Local evidence is under `data/verification/station-only-20261005/`: input hashes, station
membership, hourly summaries, differences from the old series, target metadata and rendering
comparisons. Full-verifier failure evidence is under `data/verification/20261005T163110/`.
These ignored local files are not distributed in a clone. The original graph, selected source
files, five PDFs and local draft were also copied to a temporary snapshot before changes.

## Scientific findings and remaining acceptance

The following counts describe the current cleaned partitions. They are provisional analytical
results for review, rather than a scientifically accepted baseline.

| City | Stations with finite PM2.5 in 2023 | Reporting stations per hour | IT2 episodes | Longest episode |
|---|---:|---:|---:|---:|
| Bogotá | 47 | 22–45 | 0 | 0 hours |
| Mexico City | 27 | 9–25 | 20 | 11 hours |
| Santiago | 10 | 5–10 | 105 | 47 hours |
| São Paulo | 24 | 11–23 | 14 | 13 hours |

All four calendars have 8,760 rows and no entirely missing city-hours in the current data.
Santiago has three episodes exceeding 40 hours. The local draft's affected captions and
Bogotá statement were updated to match these calculations and explain station availability.
That draft is ignored; its changes need separate manuscript review and export of current PDFs.
Other numerical manuscript/table statements were not accepted by this temporal check.

**CDMX needs scientific review before acceptance.** Its cleaned data retain PM2.5 of 79,999
at CALPULALPAN on 17 July 2023, 22:00, and 78,330 on 16 July, 15:00. The latter produces a
city-hour mean of 4,903 over 16 reporting stations. Sixteen readings above 500 occur across
three stations. This distorts the ridgeline's displayed range and may affect episodes and
other analyses using those partitions. Parsing, units, source quality and outlier handling
have not yet been reconciled. Values were preserved; no range cap, station exclusion or
new cleaning rule was introduced. This issue is explicit in baseline review rather than
hidden through plotting limits.

The old series differ materially from the current summaries: 8,735 common finite Bogotá
hours change, 8,745 Mexico City hours change, 97 Santiago hours change and 8,738 São Paulo
hours change at absolute difference > 1e-10. These comparisons identify the changed input
route; they do not attribute every difference to station membership, cleaning or units.

The initial comparison registry covers four hourly Parquets and the episode CSV. An
unreviewed snapshot was prepared under
`data/verification/station-baseline-candidate-20261005/`; all approval flags remain false.
The tool `tools/reproduction/prepare_baseline.R` can prepare another new candidate. This
coverage is incomplete for the rest of the manuscript. Full acceptance still requires:

1. Resolve CDMX source/cleaning questions without silently changing the approved methods.
2. Review current station selection, input provenance, hourly calculations and scientific
   claims; extend the comparison registry to remaining manuscript numerical and plot data.
3. Restore the missing TeX include and review/compile the complete manuscript with current
   exported artifacts. Complete a human RStudio walkthrough.
4. Execute a fresh complete manuscript rebuild under read-only source mounts, with reviewed
   revision-matched baselines, rendering review and recorded failures/skips.
5. Only after acceptance, remove Make and its RStudio setting and replace `run_pipeline.R`
   with the targets compatibility launcher. Independent reproduction remains separate.

The optional resolution definitions and quintile default retain portable test coverage.
Frozen optional inputs/results were not regenerated or relabelled. Their historical code
manifests and scientific interpretations require their own review; the migration's test
pass does not newly accept those results.

## Review and possible human-created commits

Review the pre-existing work separately from this implementation using the saved starting
snapshot. Mixes within files need hunk review. The following groups are recommendations only.

| Suggested subject | Dependency-complete group |
|---|---|
| `fix: close direct-script migration dependencies` | Existing required untracked launchers/helpers/tests with their committed callers, including `verification_cli.R`, validation report helpers and resolution workflow definitions needed by the test runner. |
| `refactor: simplify manuscript graph and prepare station-only appendix figures` | `_targets.R`, all `config/targets/` modules, station temporal functions/recipes, shared PDF writer, plotting split, settings, default runner changes and affected graph/cache/numerical tests. These changes depend on each other; do not commit modules without their sourced functions. |
| `refactor: modernize optional satellite and context inputs` | Optional extraction/join/comparison recipes, `merra2_settings.R`, context maps, Santiago diagnostic and optional precision/join tests. Keep old-data validation functions. |
| `refactor: separate reproduction tooling and prepare baseline review` | Five `tools/reproduction/` commands, shared comparison/code-inventory functions, comparison registry, old command removals, Compose/caller updates and numerical comparison tests. |
| `docs: clarify current reproduction and harness boundaries` | Current human guides/index/dictionary, dated migration status, canonical agent guidance, native wrapper command paths and reviewed Claude file-denial settings. |
| `feat: complete optional resolution comparisons` | Previously added grouping/review functions, recipes, tests, method documentation and independently reviewed outputs; do not bundle scientific acceptance into structural migration. |

The regenerated five temporal PDFs and local draft changes need scientific review before
their publication group is committed. Other already-dirty results were not regenerated here.
Tracked-only link checks currently report required new files as missing because the index
was deliberately left untouched. A clean public-file candidate constructs the graph; the
committed checkout remains incomplete until the human includes its complete dependencies.
