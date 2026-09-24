# Workflow inventory and remaining work

Updated 23 September 2026. Every R entry point and Quarto report under scripts/ has
one row below. Targets declarations own manuscript dependencies; scripts remain
executable RStudio recipes. The inventory is descriptive, not another scheduler.

Readiness is separate from workflow: **maintained** means checked against its contract,
**unverified** means execution/parity evidence is incomplete, and **blocked** identifies
a known prerequisite or scientific-contract issue. No full reproduction is asserted.
Absence from targets is not evidence for deletion.

Run commands from the project root, or execute the script sections in RStudio without
a targets cache. Preserve scientific settings and protected source inputs. See
[deletion candidates](deletion-candidates.md) for removal proposals and
[targets migration](targets-migration.md) for acceptance conditions.

## Complete entry-point inventory

<!-- workflow-inventory:start -->
| Entry point | Workflow / readiness | Targets or reason for separation | Direct command / prerequisites | Inputs -> outputs / consumers | Next action |
|---|---|---|---|---|---|
| [scripts/download_data/download_bogota_data.R](../../scripts/download_data/download_bogota_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_bogota_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, geographic products and acquisition logs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/download_data/download_cdmx_data.R](../../scripts/download_data/download_cdmx_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_cdmx_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, geographic products and acquisition logs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/download_data/download_merra2_data.R](../../scripts/download_data/download_merra2_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_merra2_data.R 2023-01-01 2023-12-31 M2T1NXAER.5.12.4 data/raw/merra2_aerosol_products`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, geographic products and acquisition logs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/download_data/download_santiago_data.R](../../scripts/download_data/download_santiago_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_santiago_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, geographic products and acquisition logs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/download_data/download_sao_paulo_data.R](../../scripts/download_data/download_sao_paulo_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_sao_paulo_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, geographic products and acquisition logs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/export/export_paper.R](../../scripts/export/export_paper.R) | operational utility; **unverified** | Targets: `paper_export` (shared export function). | `Rscript scripts/export/export_paper.R --destination data/verification/export-preview --dry-run`<br>Declared input files must already exist. | Artifact manifest and selected results -> checksum-verified manuscript export | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/build_bogota_localidad_crosswalk.R](../../scripts/process_data/build_bogota_localidad_crosswalk.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/build_bogota_localidad_crosswalk.R`<br>Declared input files must already exist. | Prepared 2018 manzanas/localities -> crosswalk for resolution workflows | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/compute_descriptive_tables.R](../../scripts/process_data/compute_descriptive_tables.R) | manuscript; **unverified** | Targets: `compute_descriptive_tables` | `Rscript scripts/process_data/compute_descriptive_tables.R`<br>Declared input files must already exist. | Raw/clean panels, distances, census -> missingness/counts/WHO/threshold/census summaries | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/compute_distance_band_descriptives.R](../../scripts/process_data/compute_distance_band_descriptives.R) | manuscript; **unverified** | Targets: `compute_distance_band_descriptives` | `Rscript scripts/process_data/compute_distance_band_descriptives.R`<br>Declared input files must already exist. | Geography, census, distances -> distance-band tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/compute_station_scatter_inputs.R](../../scripts/process_data/compute_station_scatter_inputs.R) | manuscript; **unverified** | Targets: `compute_station_scatter_inputs` | `Rscript scripts/process_data/compute_station_scatter_inputs.R`<br>Declared input files must already exist. | Cleaned panels, geography, census -> station socioeconomic tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/detect_outliers.R](../../scripts/process_data/detect_outliers.R) | manuscript; **unverified** | Targets: `outliers` | `Rscript scripts/process_data/detect_outliers.R`<br>Declared input files must already exist. | Pollution partitions and station distances -> cleaned partitions for IDW/imputation | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/estimate_exposure.R](../../scripts/process_data/estimate_exposure.R) | manuscript; **unverified** | Targets: `estimate_exposure` | `Rscript scripts/process_data/estimate_exposure.R`<br>Declared input files must already exist. | IDW families and distances -> exposure regressions for figures/tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/estimate_exposure_imputed.R](../../scripts/process_data/estimate_exposure_imputed.R) | manuscript; **unverified** | Targets: `estimate_exposure_imputed` | `Rscript scripts/process_data/estimate_exposure_imputed.R`<br>Declared input files must already exist. | Imputed panels, distances, census -> imputed IDW/regression families | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/estimate_idw.R](../../scripts/process_data/estimate_idw.R) | manuscript; **unverified** | Targets: `idw` | `Rscript scripts/process_data/estimate_idw.R`<br>Declared input files must already exist. | Cleaned 2023 data, distances and census -> city/vintage IDW families | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/estimate_resolution_sensitivity.R](../../scripts/process_data/estimate_resolution_sensitivity.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/estimate_resolution_sensitivity.R --scope=reference`<br>Completed reference inputs; --scope=multicity requires frozen inputs and saved reference anchors. | Reference or frozen multicity inputs -> A/B/C estimates, diagnostics, optional tables | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/generate_distance_matrices.R](../../scripts/process_data/generate_distance_matrices.R) | manuscript; **maintained** | Targets: `distances` | `Rscript scripts/process_data/generate_distance_matrices.R`<br>The nine prepared geographic/station files shown in Section I must exist. | Prepared geography/stations -> distance matrices for outliers and IDW | Distance pilot: explicit reads, five computations, ordered saves; prepared-input parity checked and readability accepted by Marcos. Full source reproduction pending; see targets-migration.md. |
| [scripts/process_data/generate_inegi_lab_inputs.R](../../scripts/process_data/generate_inegi_lab_inputs.R) | optional analysis; **blocked** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/generate_inegi_lab_inputs.R`<br>Declared input files must already exist. | CDMX 2023 and declared 2020/current 2024 geography -> INEGI CSV deliverables | Resolve the stated 2020 versus implemented 2024 geographic contract. |
| [scripts/process_data/generate_panel_air_quality.R](../../scripts/process_data/generate_panel_air_quality.R) | manuscript; **unverified** | Targets: `generate_panel_air_quality` | `Rscript scripts/process_data/generate_panel_air_quality.R`<br>Declared input files must already exist. | Preserved MERRA-2 rasters and original geography -> city aerosol CSV panels | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/impute_missing_hourly.R](../../scripts/process_data/impute_missing_hourly.R) | manuscript; **unverified** | Targets: `impute_missing_hourly` | `Rscript scripts/process_data/impute_missing_hourly.R`<br>Declared input files must already exist. | Cleaned panels -> imputed panels/predictions for exposure and diagnostics | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/prepare_resolution_inputs.R](../../scripts/process_data/prepare_resolution_inputs.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/prepare_resolution_inputs.R`<br>Freeze reviewed derived inputs deliberately; changed existing manifests must fail. | Reviewed derived city inputs -> frozen manifests, populations, crosswalks and B matrices | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/prepare_station_temporal.R](../../scripts/process_data/prepare_station_temporal.R) | manuscript; **unverified** | Targets: `prepare_station_temporal` | `Rscript scripts/process_data/prepare_station_temporal.R`<br>Declared input files must already exist. | Aerosol panels and original balanced station samples -> temporal PM2.5 series | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/process_bogota_data.R](../../scripts/process_data/process_bogota_data.R) | manuscript; **unverified** | Targets: `bogota_geography`, `bogota_stations_filter`, `bogota_pollution_parquet`, `bogota_census` | `Rscript scripts/process_data/process_bogota_data.R`<br>Declared input files must already exist. | bogota preserved sources -> interim geography/stations, partitioned pollution and all census variants | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/process_cdmx_data.R](../../scripts/process_data/process_cdmx_data.R) | manuscript; **unverified** | Targets: `cdmx_geography`, `cdmx_stations_filter`, `cdmx_pollution_parquet`, `cdmx_census` | `Rscript scripts/process_data/process_cdmx_data.R`<br>Declared input files must already exist. | cdmx preserved sources -> interim geography/stations, partitioned pollution and all census variants | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/process_merra2_panels.R](../../scripts/process_data/process_merra2_panels.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/process_merra2_panels.R`<br>Declared input files must already exist. | Temporal series and country/NASA inputs -> optional comparison tables | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/process_santiago_data.R](../../scripts/process_data/process_santiago_data.R) | manuscript; **blocked** | Targets: `santiago_geography`, `santiago_stations_filter`, `santiago_pollution_parquet`, `santiago_census` | `Rscript scripts/process_data/process_santiago_data.R`<br>Declared input files must already exist. | santiago preserved sources -> interim geography/stations, partitioned pollution and all census variants | Supply three preserved 2017 geographic responses; no vintage substitution. |
| [scripts/process_data/process_sao_paulo_data.R](../../scripts/process_data/process_sao_paulo_data.R) | manuscript; **blocked** | Targets: `sao_paulo_geography`, `sao_paulo_stations_filter`, `sao_paulo_pollution_parquet`, `sao_paulo_census` | `Rscript scripts/process_data/process_sao_paulo_data.R`<br>Declared input files must already exist. | sao_paulo preserved sources -> interim geography/stations, partitioned pollution and all census variants | Supply the preserved 2010 weighting-area source. |
| [scripts/run_pipeline.R](../../scripts/run_pipeline.R) | operational utility; **unverified** | Transitional; replace with compatibility launcher after acceptance. | `Rscript scripts/run_pipeline.R`<br>Declared input files must already exist. | Preserved sources -> transitional sequential manuscript execution | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/run_targets.R](../../scripts/run_targets.R) | operational utility; **unverified** | Targets: `paper_export` (launcher; accepts any declared target) | `Rscript scripts/run_targets.R all`<br>Declared input files must already exist. | Preserved sources and explicit graph -> selected target and prerequisites | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_aerosol_composition.R](../../scripts/tables_images/figure_aerosol_composition.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_aerosol_composition.R`<br>Declared input files must already exist. | Prepared MERRA-2 PM2.5 panels -> species-distribution PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_imputation_diagnostics.R](../../scripts/tables_images/figure_imputation_diagnostics.R) | manuscript; **unverified** | Targets: `figure_imputation_diagnostics` | `Rscript scripts/tables_images/figure_imputation_diagnostics.R`<br>Declared input files must already exist. | Predictions and station socioeconomic tables -> diagnostic PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_kernel_distributions.R](../../scripts/tables_images/figure_kernel_distributions.R) | manuscript; **unverified** | Targets: `figure_kernel_distributions` | `Rscript scripts/tables_images/figure_kernel_distributions.R`<br>Declared input files must already exist. | Cleaned partitions -> temporal density/exceedance PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_merra2_vs_stations.R](../../scripts/tables_images/figure_merra2_vs_stations.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_merra2_vs_stations.R`<br>Declared input files must already exist. | Temporal series, inversion/geographic inputs -> satellite PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_missing_heatmap.R](../../scripts/tables_images/figure_missing_heatmap.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_missing_heatmap.R`<br>Declared input files must already exist. | Raw partitions and optional missingness summaries -> heatmap PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_pollution_quintile_maps.R](../../scripts/tables_images/figure_pollution_quintile_maps.R) | manuscript; **unverified** | Targets: `figure_pollution_quintile_maps` | `Rscript scripts/tables_images/figure_pollution_quintile_maps.R`<br>Declared input files must already exist. | Geography, stations, census, pollution membership -> map PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_pollution_stations_by_hour.R](../../scripts/tables_images/figure_pollution_stations_by_hour.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_pollution_stations_by_hour.R`<br>Declared input files must already exist. | Historical Santiago 2013 sample -> station-hour PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_population_density_maps.R](../../scripts/tables_images/figure_population_density_maps.R) | manuscript; **unverified** | Targets: `figure_population_density_maps` | `Rscript scripts/tables_images/figure_population_density_maps.R`<br>Declared input files must already exist. | Geography, stations, census, pollution membership -> map PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_quintile_kernel_distributions.R](../../scripts/tables_images/figure_quintile_kernel_distributions.R) | manuscript; **unverified** | Targets: `figure_quintile_kernel_distributions` | `Rscript scripts/tables_images/figure_quintile_kernel_distributions.R`<br>Declared input files must already exist. | IDW families -> exposure-density PDFs at 3/20 km | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_resolution_sensitivity.R](../../scripts/tables_images/figure_resolution_sensitivity.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_resolution_sensitivity.R --scope=reference`<br>Completed reference inputs; --scope=multicity requires frozen inputs and saved reference anchors. | Completed resolution estimates -> PDFs and review packet | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_station_scatter.R](../../scripts/tables_images/figure_station_scatter.R) | manuscript; **unverified** | Targets: `figure_station_scatter` | `Rscript scripts/tables_images/figure_station_scatter.R`<br>Declared input files must already exist. | Station socioeconomic tables -> monitoring PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_station_temporal.R](../../scripts/tables_images/figure_station_temporal.R) | manuscript; **unverified** | Targets: `figure_station_temporal` | `Rscript scripts/tables_images/figure_station_temporal.R`<br>Declared input files must already exist. | Prepared temporal series -> hourly/episode PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_stations_on_metro_area.R](../../scripts/tables_images/figure_stations_on_metro_area.R) | optional analysis; **blocked** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_stations_on_metro_area.R`<br>Declared input files must already exist. | Obsolete CDMX paths -> two interactive station HTML widgets | Review obsolete paths and equivalence; preserve the interactive purpose. |
| [scripts/tables_images/figure_study_area_maps.R](../../scripts/tables_images/figure_study_area_maps.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_study_area_maps.R`<br>Context geography; terrain tiles may require networking. | Context geography and optional terrain tiles -> context maps | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/generate_exposure_plots.R](../../scripts/tables_images/generate_exposure_plots.R) | manuscript; **unverified** | Targets: `generate_exposure_plots` | `Rscript scripts/tables_images/generate_exposure_plots.R`<br>Declared input files must already exist. | Observed/imputed regressions -> exposure PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/plot_station_monitoring_figures.R](../../scripts/tables_images/plot_station_monitoring_figures.R) | manuscript; **unverified** | Targets: `plot_station_monitoring_figures` | `Rscript scripts/tables_images/plot_station_monitoring_figures.R`<br>Declared input files must already exist. | Distances, census and station socioeconomic tables -> monitoring PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/render_census_tables.R](../../scripts/tables_images/render_census_tables.R) | manuscript; **unverified** | Targets: `render_census_tables` | `Rscript scripts/tables_images/render_census_tables.R`<br>Declared input files must already exist. | Census and distance-band summaries -> descriptive LaTeX tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/render_exposure_tables.R](../../scripts/tables_images/render_exposure_tables.R) | manuscript; **unverified** | Targets: `render_exposure_tables` | `Rscript scripts/tables_images/render_exposure_tables.R`<br>Declared input files must already exist. | Exposure regressions -> education/income LaTeX tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/render_missing_tables.R](../../scripts/tables_images/render_missing_tables.R) | manuscript; **unverified** | Targets: `render_missing_tables` | `Rscript scripts/tables_images/render_missing_tables.R`<br>Declared input files must already exist. | Missingness and education availability -> LaTeX tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/render_station_tables.R](../../scripts/tables_images/render_station_tables.R) | manuscript; **unverified** | Targets: `render_station_tables` | `Rscript scripts/tables_images/render_station_tables.R`<br>Declared input files must already exist. | Counts, WHO, thresholds -> station/threshold LaTeX tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/validation_old_version/bogota_report.qmd](../../scripts/validation_old_version/bogota_report.qmd) | legacy validation; **unverified** | Outside manuscript targets: separate historical comparison. | `quarto render scripts/validation_old_version/bogota_report.qmd`<br>Matching preserved legacy inputs, comparison configuration and producer outputs. | Existing comparison products and matching cfg -> self-contained HTML | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/validation_old_version/compare_bogota.R](../../scripts/validation_old_version/compare_bogota.R) | legacy validation; **unverified** | Outside manuscript targets: separate historical comparison. | `Rscript scripts/validation_old_version/compare_bogota.R`<br>Matching preserved legacy inputs, comparison configuration and producer outputs. | Configured legacy/current inputs -> comparison products and report | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/validation_old_version/compare_bogota_raw_ground_stations.R](../../scripts/validation_old_version/compare_bogota_raw_ground_stations.R) | legacy validation; **unverified** | Outside manuscript targets: separate historical comparison. | `Rscript scripts/validation_old_version/compare_bogota_raw_ground_stations.R`<br>Matching preserved legacy inputs, comparison configuration and producer outputs. | Legacy/current Bogota pollution/geography -> raw comparison products | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/validation_old_version/compare_cdmx_raw_ground_stations.R](../../scripts/validation_old_version/compare_cdmx_raw_ground_stations.R) | legacy validation; **blocked** | Outside manuscript targets: separate historical comparison. | `Rscript scripts/validation_old_version/compare_cdmx_raw_ground_stations.R`<br>Matching preserved legacy inputs, comparison configuration and producer outputs. | Historical CDMX inputs -> station comparison widgets/tables | Resolve historical raw/geographic paths from provenance. |
| [scripts/validation_old_version/compare_ground_stations_data.R](../../scripts/validation_old_version/compare_ground_stations_data.R) | legacy validation; **blocked** | Outside manuscript targets: separate historical comparison. | `Rscript scripts/validation_old_version/compare_ground_stations_data.R`<br>Matching preserved legacy inputs, comparison configuration and producer outputs. | City comparison configuration -> comparison products and expected report template | Verify cfg$compare and the report template expected beneath results. |
| [scripts/validation_old_version/plot_bogota_quintiles.R](../../scripts/validation_old_version/plot_bogota_quintiles.R) | legacy validation; **unverified** | Outside manuscript targets: separate historical comparison. | `Rscript scripts/validation_old_version/plot_bogota_quintiles.R`<br>Matching preserved legacy inputs, comparison configuration and producer outputs. | 2005 census/geography and old stations -> validation map PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/verification/check_manuscript.R](../../scripts/verification/check_manuscript.R) | operational utility; **unverified** | Outside analytical graph: execution/export/verification utility. | `Rscript scripts/verification/check_manuscript.R doc/paper/paper_draft_part1.tex`<br>Declared input files must already exist. | TeX and manifest -> reference coverage diagnostics | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/verification/run_stage.R](../../scripts/verification/run_stage.R) | operational utility; **unverified** | Outside analytical graph: execution/export/verification utility. | `Rscript scripts/verification/run_stage.R scripts/process_data/compute_descriptive_tables.R`<br>Set AIR_VERIFY_RUN to an isolated verification directory. | One stage script -> isolated stage log and metadata | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/verification/verify.R](../../scripts/verification/verify.R) | operational utility; **unverified** | Outside analytical graph: execution/export/verification utility. | `Rscript scripts/verification/verify.R --full --targets`<br>Docker, complete preserved sources, installed dependencies and reviewed baseline. | Candidate, sources, environment and reviewed baseline -> isolated verification report | Verify execution and scientific parity independently; preserve the specification. |

<!-- workflow-inventory:end -->

## Explicit optional command sequences

Use the full script paths from the inventory. These preserve useful optional Make
commands without adding the workflows to the manuscript graph.

- Reference resolution: complete manuscript exposure; run build_bogota_localidad_crosswalk.R,
  estimate_resolution_sensitivity.R --scope=reference, then the corresponding figure script.
- Multicity resolution: supply reviewed frozen inputs and saved reference anchors; run
  prepare_resolution_inputs.R, estimate_resolution_sensitivity.R --scope=multicity, then
  figure_resolution_sensitivity.R --scope=multicity. Never automatically refresh the frozen
  inputs by invoking manuscript exposure.
- Satellite: complete temporal preparation; run process_merra2_panels.R,
  figure_merra2_vs_stations.R and figure_aerosol_composition.R with their extra inputs.
- Context maps: complete city preparation and run figure_study_area_maps.R explicitly.
- Legacy validation: run the selected comparison with matching preserved inputs, then
  render its report. A missing comparison is not evidence of agreement.

## Preserved historical methodological record

The dated material below preserves earlier findings. Its old paths, counts and execution
instructions are historical, not the current inventory or an acceptance claim.

> September 2026 note: [HOW_TO_RUN](../HOW_TO_RUN.md) is the operational source of truth and
> [doc/ai](../ai/README.md) is the shared harness guidance. The dated methodological findings below
> are retained as evidence. Current produced artifacts are tracked by
> `config/paper_artifacts.csv` under `results/figures/` and `results/tables/`; historical paths
> mentioned below do not define the current pipeline.

# What is left to build in this repo

What the manuscript needs from the **default pipeline** (`scripts/process_data/` →
`scripts/tables_images/`), and what the pipeline does not answer for. The legacy track is
mentioned only where it explains why something is absent.

Hand-maintained. Rewritten **31 August 2026**, revised **1 September 2026** against
`doc/paper/paper_draft_part1.tex`. Re-check before acting.

## The manuscript's inventory, re-derived

The `.tex` carries **123 distinct `\includegraphics` paths**, of which **117 are active**
and 6 are commented out. `appendix_distance_computation` is **not** `\input` anywhere, and
there is no `\include`.

**Current manuscript export uses `config/paper_artifacts.csv`.** It maps selected files under
`results/figures/` and `results/tables/` to unchanged manuscript-relative destinations. The
1 September 2026 `results/paper/` arrangement and its path-mapping script are historical
observations, not current instructions. The draft still carries **11 `\input` table targets**
plus `data_appendix`.

Nothing on the figure or table side is outstanding. What remains is in section D: choices
the code cannot make, and prose that disagrees with what the pipeline now computes.

---

## A. Deliberately not produced

The six commented-out `\includegraphics` paths. Three are the Santiago "others" panels
(O3, CO and NO2), whose whole figure block is commented out; three are non-imputed
`plot_quintiles_*` variants superseded by the `_imp` family. If the coauthors reinstate
the quintile ones the `_imp` chain already covers them; the O3/CO/NO2 ones would need the
interpolation widened past PM.

## B. Tables

**Eleven of the manuscript's twelve tables are now produced by code** and `\input` from
`results/paper/tables/`. Every fragment is a bare `tabular`; the manuscript keeps its own
float, caption and label.

| Table | Producer |
|---|---|
| `table_census_coverage`, `table_descriptives_a`, `table_descriptives_b` | `render_census_tables.R` |
| `stations_by_pollutant_2023`, `table_days_above_thresholds`, `table_avg_hours_above_thresholds` | `render_station_tables.R` |
| `missing_by_education_quintile_2023` | `render_missing_tables.R` |
| the four exposure-by-group tables | `render_exposure_tables.R` |

The twelfth, the WHO interim-target values, stays hardcoded in the `.tex`: it is a table
of definitional constants, not a result. `data_appendix` is hand-maintained prose and is
not a code target either.

Three things to know about the tables that were hardcoded until 1 September 2026 and now
come from the pipeline.

First, the **station counts** rose sharply against the published table (Bogotá 19 → 48
PM10, CDMX 23 → 32, São Paulo 29 → 28), because the metro station universe changed.

Second, **data availability by education quintile is far more unequal than published**:
Bogotá's Q1 PM10 share falls from 0.876 to 0.480, and Santiago's Q1 is `--` because that
city has no monitoring station in its lowest education quintile (§D.3).

Third, the **two income tables can no longer be cut in deciles for both cities** — CDMX is
estimated in quintiles (§D.5). The tables print Mexico City Q1–Q5 and São Paulo D1–D10 in
one tabular, and their captions say so. The body prose has not been updated to match.

Two things to know about the descriptive tables. First, they are computed from the
individual census, so the population row is the whole resident population; the published
version's density rows are not reproduced as printed, because the published "average
population density" was computed as total population times the mean of 1/area, which is
not a density and grows with the number of units. This repo reports mean(population/area).
Second, the row set differs by city because the censuses do: Bogotá's canonical census
carries no household-head or ethnicity variable, so the published "Share of HH women" and
"Share of black" rows have no counterpart, and Santiago's age comes from its raw column.

## C. Kept but not cited

The 5 km exposure figures, the PM2.5 twins of the station-distance panels, and the other
uncited figure families stay under the repo's own names as appendix and slide material.

## D. Carried-forward items

0. **The published density rows are wrong.** See §B: the "average population density" row
   of the two descriptive tables was computed as total population times the mean of 1/area.
   Demonstrated in `doc/audits/census_processing/`, and it explains why those cells grow
   with the radius while the total density falls. The new producer computes the mean of
   the units' own densities.
1. **The IDW stage now computes a 20 km buffer.** `estimate_idw.R` runs
   `buffers_km <- c(3, 5, 20)`. Only the exposure density figures use 20 km, so no
   regression is estimated on it and `estimate_exposure.R` keeps its own `c(3L, 5L)`.
   The 20 km pass is the slow one: Bogotá's 57,032 units against 58 stations at 8,760
   hours dominates the stage's runtime.

   `figure_exposure_by_quintile.R` was retired rather than repaired. Both of its modes
   read artifact names this repo no longer writes, its regression figures were already
   superseded by `generate_exposure_plots.R`, and the densities it was meant to draw now
   come from `figure_quintile_kernel_distributions.R`, which locates its inputs through
   `idw_artifact_path()` so the naming cannot drift again.
2. **The imputed specification is built for 2023 only.** The estimator fits one OLS per
   station per pollutant per year, and the cleaned panels run from 2000, so imputing
   every year is hours of fitting for results the manuscript never reports.
   `impute_missing_hourly.R` therefore passes `years = 2023L`. Widen it if another year
   is ever needed. The imputed panels are written to `data/processed/imputed_ols/` and
   the fitted values for the diagnostics figures alongside them, as
   `<city>_imputed_predictions.parquet`.

   Two things worth recording about the result. The model tracks the observed series
   closely: on Santiago's observed hours the correlation between prediction and reading
   is 0.93 for both pollutants. And the paper's claim that the imputed results are
   qualitatively similar holds — across the 80 education-gap estimates at 3 km, the
   observed and imputed specifications agree in sign 80 times out of 80, and the two
   sets of point estimates correlate at 0.995.
3. **Santiago has no monitoring station in the lowest education quintile.** With the 2017
   zonas censales, quintile 1 is empty in the availability table. Substantive, not a bug.
   Since 1 September 2026 the manuscript prints this: `table_share_missings` is now
   `\input` from the pipeline and shows `--` in Santiago's Q1 cell. **The surrounding
   prose still does not explain it**, which is now a visible gap rather than a latent one.
4. **The manuscript's prose disagrees with the census coverage table** on the geographic
   unit for three cities: Bogotá is 2018 census tracts here versus 2005 localities in the
   text, Mexico City has 63 municipalities versus 76, and Santiago is 1,654 zonas censales
   versus 384 census tracts. The vintages the pipeline uses are the current ones; the text
   needs updating, not the code.
5. **Mexico City income is estimated in quintiles, not deciles.** 63 municipalities leave
   too few clusters to identify ten coefficients. The two appendix tables were fixed on
   1 September 2026 — they now print Mexico City in quintiles and São Paulo in deciles,
   and their captions and notes say why. **Five prose locations and one figure caption
   still say deciles** (`\ref{figure_exposure_it1_decile}` and the paragraphs around
   lines 630, 637 and 716 of `paper_draft_part1.tex`).
6. **`doc/audits/` is stale in two places.** The São Paulo duplicate-station finding is
   fixed (`sp_process_stations_data_to_parquet()` now hard-stops if a station name carries
   more than one CETESB code, and the rebuilt panel has no duplicate station-hours), and
   the Bogotá geo-id and CDMX cluster-count findings are closed.
