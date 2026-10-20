# Specification sensitivity: file map

How much do analytical choices move the estimated education gap in pollution exposure?
This optional analysis varies four of them for Bogotá (2018 census), Santiago (2017) and
São Paulo (2010): the geographic support (`geo_id` resolution), the IDW buffer, the IDW
distance exponent and the definition of education groups. The manuscript keeps its own
specification (finest support, 3 km, exponent 1, quintiles). Definitions, interpretation
and limitations are in [the guide](../../../doc/RESOLUTION_SENSITIVITY.md); this page is
the map of scripts, functions and outputs.

**Design A is the primary design.** It keeps people, groups and weights fixed and averages
fine-unit exposure over each support. It runs on the full buffer × exponent grid. Designs
B (exposure rebuilt at each support) and C (area classification) are robustness checks at
3 and 20 km with exponent 1.

## Run order

| Step | Script | Role | Main functions | Inputs → outputs |
|---|---|---|---|---|
| 0 | `build_bogota_localidad_crosswalk.R` | Bogotá input | — | Manzanas and localidades → crosswalk |
| 1 | `prepare_resolution_inputs.R` | Freeze inputs | `resolution_prepare_*()` | Manuscript derived files → frozen, hashed inputs in `data/interim/resolution_sensitivity/<city>/` and support distance matrices |
| 2 | `estimate_resolution_sensitivity.R` | Robustness A/B/C | `resolution_multicity_*()` | Frozen inputs at `--buffer-km` (3, 20) and `--grouping` → `results/tables/specification_sensitivity/robustness_designs/` |
| 3 | `estimate_idw_specification_grid.R` | Primary inputs | `aggregate_idw_exposure()`, `idw_grid_anchor()` | Frozen fine matrix and 2023 panel → fine exposure for 10 buffers at exponent 1 and 6 at exponent 2 in `data/processed/resolution_sensitivity/idw_grid/` |
| 4 | `estimate_design_a_grid.R` | Primary estimates | `design_a_sample()`, `design_a_estimate()`, `design_a_decomposition()`, `idw_station_weights()` | Grid exposures → `results/tables/specification_sensitivity/design_a/` |
| 5 | `../../tables_images/specification_sensitivity/figure_resolution_review.R` | Coauthor review | `resolution_buffer_plot_data()`, `plot_resolution_buffer_gaps()` | Robustness and grid tables → `review/` tables and figures (3 vs 20 km, Design A and native B) |
| 6 | `build_specification_panel.R` | Analysis table | `specification_panel()`, `specification_add_baselines()` | Grid tables → `results/tables/specification_sensitivity/panel/specification_panel.csv` |
| 7 | `estimate_specification_variance_decomposition.R` | Which choices matter | `specification_variance_decomposition()` | Panel → variance shares per choice |
| 8 | `../../tables_images/specification_sensitivity/figure_design_a_grid.R` | Primary figures | `plot_design_a_*()`, `plot_variance_shares()` | Grid tables and shares → `results/figures/diagnostics/specification_sensitivity/design_a/` |
| — | `../../tables_images/specification_sensitivity/figure_resolution_sensitivity.R` | Robustness figures | `resolution_multicity_figure_*()` | Robustness tables → `robustness_designs/` figure packets |

Make targets: `resolution-multicity` (steps 1, 2 at 3 km and its figures),
`resolution-multicity-grouping GROUPING= BUFFER=`, `specification-grid` (3, 4) and
`specification-panel` (6, 7, 8). Step 5 and the 20 km robustness estimate are run
directly; see `doc/HOW_TO_RUN.md`.

## Functions

`src/general_utilities/process/specification_sensitivity/`, loaded by
`src/general_utilities/config_utils_resolution.R`:

| File | Contents |
|---|---|
| `resolution_sensitivity.R` | Shared primitives: support aggregation, group estimates, bootstrap, classification, matrix diagnostics |
| `resolution_workflow.R` | A/B/C robustness workflow (inputs, B rebuild, contrasts, checks, tables) |
| `resolution_review.R` | Review summaries, including the buffer comparison and its plot data |
| `design_a.R` | Design A sample rule, Design A on a supplied sample, fixed-sample decomposition |
| `idw_grid.R` | Station-weight descriptors and manuscript anchors of the IDW grid |
| `specification_panel.R` | Panel assembly, baselines and the variance decomposition |

Plot functions are in `src/general_utilities/plot/specification_sensitivity/`
(`resolution_workflow.R`, `resolution_review.R`, `design_a_grid.R`).

## Outputs

| Folder | Contents |
|---|---|
| `results/tables/specification_sensitivity/design_a/` | `design_a_grid_*.csv`: samples, support sizes, station weights, contrasts, profiles, decomposition, threshold ties, single-station checks, checks |
| `results/tables/specification_sensitivity/robustness_designs/` | `resolution_multicity_[<grouping>_][20km_]*.csv` (A/B/C at 3 and 20 km) |
| `results/tables/specification_sensitivity/review/` | `resolution_review_*.csv` (coauthor review, including the Design A buffer samples and decomposition) |
| `results/tables/specification_sensitivity/panel/` | `specification_panel.csv`, profiles, parity and regression anchors, checks, variance decomposition |
| `results/figures/diagnostics/specification_sensitivity/<role>/` | Figures of the same roles; `reference_bogota/` holds the historical Bogotá figures |
| `data/processed/resolution_sensitivity/` | Parquet checkpoints: per-run city folders, `idw_grid/`, `design_a_grid/`, `specification_panel/`; `_frozen_*` archives |

This exercise is the pilot of a subfolder layout (`doc/ai/architecture.md`); the rest of
the repository keeps the flat layout until a repo-wide plan.
