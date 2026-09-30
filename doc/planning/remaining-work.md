# Workflow inventory and remaining work

Updated 30 September 2026. Every R entry point and Quarto report under scripts/ has
one row below. Targets declarations own manuscript dependencies; scripts remain
executable RStudio recipes. The inventory is descriptive, not another scheduler.

Readiness is separate from workflow: **maintained** means the entry point has execution
evidence and its current input/output contract has been reviewed; **unverified** means
that evidence remains incomplete; **blocked** identifies a known prerequisite or contract
issue. Each row states the evidence source. Maintained does not mean scientifically accepted
or fully reproduced.
Absence from targets is not evidence for deletion.

Run commands from the project root, or execute the script sections in RStudio without
a targets cache. Preserve scientific settings and protected source inputs. See
[deletion candidates](deletion-candidates.md) for removal proposals and
[targets migration](targets-migration.md) for acceptance conditions.

## Updating a row after a run

Update the existing row rather than adding a second entry. Record who ran the script,
when the run was reported, whether it completed, and any observed output issues. Link a
saved log or comparison when available; record the R environment and input version when
known. Keep methodological questions and isolated reproduction in the next action.

**24 September 2026:** Marcos reports successful local execution of all four city-processing
scripts, distance matrices, outlier detection, IDW and observed exposure. Their rows now
record that evidence as maintained. The report covers the author-edited scripts before
this comment/layout polish; it does not provide a release log, input hashes or independent
scientific validation. Review explanations and analytical assumptions in a later inquiry.

Imputation, imputed exposure and imputation diagnostics now have direct recipes and explicit
target commands. The small count table and station-ratio tables are inspectable summaries;
plots are computed before saving. These three entries remain unverified for real-data runs.
The broader descriptive summaries and remaining figures still need migration.

**30 September 2026:** The four table recipes and `generate_exposure_plots.R` now read
saved summaries, retain named LaTeX/plot objects, and save them in Section III. Isolated
execution of the preserved implementation, revised recipes and target commands agrees
on 24 LaTeX tables and 54 exposure figures; each recipe also runs in a fresh R session.
Their inventory rows record this rendering evidence as maintained. Upstream estimation,
GUI readability review and full reproduction remain separate checks. See the
[rendering evidence](../ai/implementation.md#tables-and-exposure-figures--30-september-2026).

**29 September 2026:** Marcos approved starting imputation at the first finite cleaned
reading for each station, pollutant and year, continuing through year-end. Complete windows
skip fitting; observed readings remain available as predictors for other stations. Earlier
gaps, unsupported calendar levels and non-estimable predictions remain missing. Models with
no residual degrees of freedom are rejected. The shared function implements this policy
and returns `per_station` fitting summaries; existing production outputs need a deliberate
rerun and downstream scientific review. The separate timezone review remains deferred.

Verification: 528 synthetic checks passed using RStudio's R executable with `--vanilla`
and the restored project library; ordinary startup stalled at the renv sandbox lock.
Isolated December 2023 checks preserved finite observations in both cities. Cañada El Hato
and Casa Victorio PM10 skipped fitting; the other four reported Bogotá series were rejected
for zero residual degrees of freedom. The five reported São Paulo PM2.5 series filled
26, 53, 23, 20 and 20 gaps respectively (Capão Redondo, Carapicuíba, Interlagos, Santo Amaro,
Taboão da Serra). These are December-slice checks, not a full-year or downstream rerun.

## Deferred development and validation of the imputation model

**30 September 2026 — requested by Marcos; continue the structural migration.**
Evaluate the current station-specific OLS model and propose an improved methodology
for the imputed robustness analysis. The first-reading policy above remains the current
specification; recording this task does not select or implement a replacement model.

- Define eligibility consistently by station and pollutant. Where available, use operating
  dates and previous-year records to distinguish outages from periods before installation
  or after closure. Treat the first finite cleaned reading as a conservative fallback,
  not a commissioning date. Do not introduce an arbitrary late-year cutoff or interpret
  a complete eligible window as complete annual monitoring coverage.
- Compare the current OLS model with a simpler calendar specification and a regularized
  linear model. Validate by hiding observed blocks that resemble actual gaps, including
  short outages, long gaps, edge gaps and simultaneous outages where relevant. Fit and
  tune using only the remaining observations; retain neighboring readings that would
  actually be available during reconstruction. Fix seeds and record the held-out blocks.
- Assess mean bias, MAE/RMSE, upper concentrations, threshold-exceedance hours and downstream
  socioeconomic exposure differences. Report performance and the fraction imputed by city,
  pollutant, gap length and socioeconomic group. Separate changes in analytical coverage
  from changes in estimated exposure. In-sample agreement and synthetic tests alone do
  not establish predictive accuracy.
- Assess methods that propagate parameter and residual uncertainty while respecting
  temporal and spatial dependence, including stochastic multiple imputation. Deterministic
  mean predictions can distort exceedance counts; clustered downstream standard errors
  alone do not account for the uncertainty of imputed concentrations.

Deliver a documented comparison and a proposed specification for scientific review before
changing the production model. Keep observed-data main results distinct from this robustness
analysis. Coordinate calendar definitions with the deferred timezone review below.
Start with [imputation.R](../../src/general_utilities/process/imputation.R),
[its diagnostics](../../src/general_utilities/plot/imputation_diagnostics.R) and
[imputed exposure](../../scripts/process_data/estimate_exposure_imputed.R).
Newly filled readings now use the label `OLS_imputed`; existing datasets retain their old
labels until deliberately regenerated. No production dataset was rebuilt for this rename.

## Deferred review of pollution timestamp metadata

**29 September 2026 — deferred by Marcos.** Review the timezone contract from source
readings through interim Parquet, outlier removal, imputation and temporal summaries.

The read-only comparison of Bogotá and São Paulo's 2023 panels found timezone-naive
interim timestamps and UTC-tagged cleaned timestamps. Numeric time values matched;
the change in metadata can change their display in R. The affected stations' late-year
PM coverage was already present in the preserved sources, not caused by this difference.
This finding does not establish timestamp consistency for other cities, years or joins.

- Document whether each source reports local wall-clock labels or actual UTC instants,
  including hour-ending conventions, `24:00` and historical daylight-saving changes.
  Distinguish attaching a timezone label from converting an instant between timezones.
- Agree on the stored timestamp contract and make R, Arrow and DuckDB metadata consistent.
  Preserve the intended scientific time alignment; do not blindly convert labels currently
  stored using UTC as a technical convention.
- Check year partitions, station-hour joins, outlier windows, imputation calendar factors
  and alignment with MERRA-2. Test midnight, year boundaries and relevant clock changes.
- Verify Parquet round trips and identical analytical results under UTC and city-local
  R session timezones, including RStudio and Rscript. Record any necessary changes to
  timestamps or sample membership separately from display-only corrections.

Start with the parsers in [bogota.R](../../src/city_specific/bogota.R) and
[sao_paulo.R](../../src/city_specific/sao_paulo.R), then
[outliers.R](../../src/general_utilities/process/outliers.R) and
[imputation.R](../../src/general_utilities/process/imputation.R).
No timestamp conversion or analytical output rebuild is included in this deferral.

## Deferred Santiago 2024 population and geography alignment

**24 September 2026 — deferred by Marcos; continue the structural migration.**
The coauthors prefer INE's **Gran Santiago conurbation**, rather than the separate
48-commune legal metropolitan area. Preserve that preference when revisiting this task.
Santiago 2017 remains the main specification; the 2024 branch is a robustness analysis.

The 2024 preparation selects urban portions of 39 communes, but census processing selects
all people in those communes. The audited source counts are 6,255,650 people in the urban
entities versus 6,706,391 in the selected whole communes. A shared CUT does not establish
the same geographic population. The 40 source rows become 39 communes because Lampa has
two entities; their dissolve preserves both geometries. Do not force equal commune counts
between 2017 and 2024. `cfg$cities_in_metro` applies to `metro_santiago`, not `gran_santiago`.

Before interpreting the 2024 robustness results:

- Resolve how to match census population to the preferred Gran Santiago footprint.
  An urban-only commune filter can still include settlements outside that footprint.
  If the available data require whole communes instead, seek a scientific decision and
  label that alternative explicitly; do not silently substitute the legal metro area.
- Align geography, census selection, representative points and population weights; verify
  membership and totals against the same geographic definition. Rebuild dependent products
  together and record the changed specification.
- State the census-vintage and population-concept differences. A 2017/2024 comparison is
  not a pure spatial-resolution test.

Code: `src/city_specific/santiago.R`, `santiago_prepare_metro_area_2024()` and
`santiago_process_census_2024()`. The local audit is in
`data/verification/santiago-definition-audit-20260924/`; it is ignored evidence, not a
required public file. This deferred correction does not block the structural migration
and does not authorize changing 2024 analytical outputs now.

The three preserved 2017 geographic responses are now available and were checked.
Marcos reports rerunning the Santiago processing script after removing the 2017
missing-education filter. Audit the paired census outputs and resulting downstream changes
before scientific acceptance; the run report alone does not establish their equivalence.

## Complete entry-point inventory

<!-- workflow-inventory:start -->
| Entry point | Workflow / readiness | Targets or reason for separation | Direct command / prerequisites | Inputs -> outputs / consumers | Next action |
|---|---|---|---|---|---|
| [scripts/download_data/download_bogota_data.R](../../scripts/download_data/download_bogota_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_bogota_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, source-region diagnostic and acquisition logs; derived GeoPackages belong to processing | Acquisition/preparation split implemented; live provider access remains unverified. |
| [scripts/download_data/download_cdmx_data.R](../../scripts/download_data/download_cdmx_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_cdmx_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, source-region diagnostic and acquisition logs; derived GeoPackages belong to processing | Acquisition/preparation split implemented; live provider access remains unverified. |
| [scripts/download_data/download_merra2_data.R](../../scripts/download_data/download_merra2_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_merra2_data.R 2023-01-01 2023-12-31 M2T1NXAER.5.12.4 data/raw/merra2_aerosol_products`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, geographic products and acquisition logs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/download_data/download_santiago_data.R](../../scripts/download_data/download_santiago_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_santiago_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, source-region diagnostic and acquisition logs; derived GeoPackages belong to processing | Acquisition/preparation split implemented; live provider access remains unverified. |
| [scripts/download_data/download_sao_paulo_data.R](../../scripts/download_data/download_sao_paulo_data.R) | acquisition; **unverified** | Outside targets: deliberate provider/network access. | `Rscript scripts/download_data/download_sao_paulo_data.R`<br>Provider access and required credentials/Selenium; never invoked by processing. | Provider inputs -> preserved downloads, source-region diagnostic and acquisition logs; derived GeoPackages belong to processing | Acquisition/preparation split implemented; live provider access remains unverified. |
| [scripts/export/export_paper.R](../../scripts/export/export_paper.R) | operational utility; **unverified** | Targets: `paper_export` (shared export function). | `Rscript scripts/export/export_paper.R --destination data/verification/export-preview --dry-run`<br>Declared input files must already exist. | Artifact manifest and selected results -> checksum-verified manuscript export | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/build_bogota_localidad_crosswalk.R](../../scripts/process_data/build_bogota_localidad_crosswalk.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/build_bogota_localidad_crosswalk.R`<br>Declared input files must already exist. | Prepared 2018 manzanas/localities -> crosswalk for resolution workflows | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/compute_descriptive_tables.R](../../scripts/process_data/compute_descriptive_tables.R) | manuscript; **unverified** | Targets: `compute_descriptive_tables` | `Rscript scripts/process_data/compute_descriptive_tables.R`<br>Declared input files must already exist. | Raw/clean panels, distances, census -> missingness/counts/WHO/threshold/census summaries | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/compute_distance_band_descriptives.R](../../scripts/process_data/compute_distance_band_descriptives.R) | manuscript; **unverified** | Targets: `compute_distance_band_descriptives` | `Rscript scripts/process_data/compute_distance_band_descriptives.R`<br>Declared input files must already exist. | Geography, census, distances -> distance-band tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/compute_station_scatter_inputs.R](../../scripts/process_data/compute_station_scatter_inputs.R) | manuscript; **unverified** | Targets: `compute_station_scatter_inputs` | `Rscript scripts/process_data/compute_station_scatter_inputs.R`<br>Declared input files must already exist. | Cleaned panels, geography, census -> station socioeconomic tables | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/detect_outliers.R](../../scripts/process_data/detect_outliers.R) | manuscript; **maintained** | Targets: `outliers` | `Rscript scripts/process_data/detect_outliers.R`<br>Declared input files must already exist. | Pollution partitions and station distances -> cleaned partitions for IDW/imputation | Marcos reports a successful local run (24 September); isolated recipe/target parity was checked previously. Scientific review and isolated reproduction remain pending. |
| [scripts/process_data/estimate_exposure.R](../../scripts/process_data/estimate_exposure.R) | manuscript; **maintained** | Targets: `estimate_exposure` | `Rscript scripts/process_data/estimate_exposure.R`<br>Declared input files must already exist. | IDW families and distances -> exposure regressions for figures/tables | Marcos reports a successful local run (24 September). Comment/layout polish retains the explicit city calls and buffer loop; estimates agree on isolated fixtures. Review scientific explanations later. |
| [scripts/process_data/estimate_exposure_imputed.R](../../scripts/process_data/estimate_exposure_imputed.R) | manuscript; **unverified** | Targets: `estimate_exposure_imputed` | `Rscript scripts/process_data/estimate_exposure_imputed.R`<br>Declared input files must already exist. | Imputed panels, distances, census -> imputed IDW/regression families | Direct IDW and regression calls, named estimates, separate table saving. Fixture recipe/target parity checked; real-data scientific acceptance remains pending. |
| [scripts/process_data/estimate_idw.R](../../scripts/process_data/estimate_idw.R) | manuscript; **maintained** | Targets: `idw` | `Rscript scripts/process_data/estimate_idw.R`<br>Declared input files must already exist. | Cleaned 2023 data, distances and census -> city/vintage IDW families | Marcos reports a successful local run (24 September); isolated file-family parity was checked previously. Scientific review and isolated reproduction remain pending. |
| [scripts/process_data/estimate_resolution_sensitivity.R](../../scripts/process_data/estimate_resolution_sensitivity.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/estimate_resolution_sensitivity.R --scope=reference`<br>Completed reference inputs; --scope=multicity requires frozen inputs and saved reference anchors. | Reference or frozen multicity inputs -> A/B/C estimates, diagnostics, optional tables | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/generate_distance_matrices.R](../../scripts/process_data/generate_distance_matrices.R) | manuscript; **maintained** | Targets: `distances` | `Rscript scripts/process_data/generate_distance_matrices.R`<br>The nine prepared geographic/station files shown in Section I must exist. | Prepared geography/stations -> distance matrices for outliers and IDW | Marcos reports a successful local run (24 September); pilot readability and prepared-input parity were checked previously. Review scientific geography choices separately. |
| [scripts/process_data/generate_inegi_lab_inputs.R](../../scripts/process_data/generate_inegi_lab_inputs.R) | optional analysis; **blocked** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/generate_inegi_lab_inputs.R`<br>Declared input files must already exist. | CDMX 2023 and declared 2020/current 2024 geography -> INEGI CSV deliverables | Resolve the stated 2020 versus implemented 2024 geographic contract. |
| [scripts/process_data/generate_panel_air_quality.R](../../scripts/process_data/generate_panel_air_quality.R) | manuscript; **unverified** | Targets: `generate_panel_air_quality` | `Rscript scripts/process_data/generate_panel_air_quality.R`<br>Declared input files must already exist. | Preserved MERRA-2 rasters and original geography -> city aerosol CSV panels | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/impute_missing_hourly.R](../../scripts/process_data/impute_missing_hourly.R) | manuscript; **unverified** | Targets: `impute_missing_hourly` | `Rscript scripts/process_data/impute_missing_hourly.R`<br>Declared input files must already exist. | Cleaned panels -> imputed panels/predictions for exposure and diagnostics | Direct city model calls and a named count table; targets own panels, predictions and count checkpoints. Fixture parity checked; run the revised recipe on real data before acceptance. |
| [scripts/process_data/prepare_resolution_inputs.R](../../scripts/process_data/prepare_resolution_inputs.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/prepare_resolution_inputs.R`<br>Freeze reviewed derived inputs deliberately; changed existing manifests must fail. | Reviewed derived city inputs -> frozen manifests, populations, crosswalks and B matrices | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/prepare_santiago_alternative_geography.R](../../scripts/process_data/prepare_santiago_alternative_geography.R) | optional analysis; **unverified** | Outside manuscript targets: alternative administrative-commune boundary. | `Rscript scripts/process_data/prepare_santiago_alternative_geography.R`<br>Preserved Cartografia_censo2024_Pais.zip and installed packages. | National 2024 archive -> santiago_metro_area_2024.gpkg; optional boundary inspection. | Preserves the former download-script branch; compare real-source geometry before analytical use. |
| [scripts/process_data/prepare_station_temporal.R](../../scripts/process_data/prepare_station_temporal.R) | manuscript; **unverified** | Targets: `prepare_station_temporal` | `Rscript scripts/process_data/prepare_station_temporal.R`<br>Declared input files must already exist. | Aerosol panels and original balanced station samples -> temporal PM2.5 series | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/process_data/process_bogota_data.R](../../scripts/process_data/process_bogota_data.R) | manuscript; **maintained** | Targets: `bogota_geography`, `bogota_stations_filter`, `bogota_pollution_parquet`, `bogota_census` | `Rscript scripts/process_data/process_bogota_data.R`<br>Declared input files must already exist. | bogota preserved sources -> interim geography/stations, partitioned pollution and all census variants | Marcos reports a successful local run (24 September). Review census/geography explanations and full source-to-output scientific reproduction later. |
| [scripts/process_data/process_cdmx_data.R](../../scripts/process_data/process_cdmx_data.R) | manuscript; **maintained** | Targets: `cdmx_geography`, `cdmx_stations_filter`, `cdmx_pollution_parquet`, `cdmx_census` | `Rscript scripts/process_data/process_cdmx_data.R`<br>Declared input files must already exist. | cdmx preserved sources -> interim geography/stations, partitioned pollution and all census variants | Marcos reports a successful local run (24 September). Review geographic vintage, source precedence and scientific interpretation later. |
| [scripts/process_data/process_merra2_panels.R](../../scripts/process_data/process_merra2_panels.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/process_data/process_merra2_panels.R`<br>Declared input files must already exist. | Temporal series and country/NASA inputs -> optional comparison tables | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/process_data/process_santiago_data.R](../../scripts/process_data/process_santiago_data.R) | manuscript; **maintained** | Targets: `santiago_geography`, `santiago_stations_filter`, `santiago_pollution_parquet`, `santiago_census` | `Rscript scripts/process_data/process_santiago_data.R`<br>Declared input files must already exist. | santiago preserved sources -> interim geography/stations, partitioned pollution and all census variants | Marcos reports a successful local run (24 September). Audit paired 2017 census outputs after filter removal; retain the deferred 2024 population/geography task above. |
| [scripts/process_data/process_sao_paulo_data.R](../../scripts/process_data/process_sao_paulo_data.R) | manuscript; **maintained** | Targets: `sao_paulo_geography`, `sao_paulo_stations_filter`, `sao_paulo_pollution_parquet`, `sao_paulo_census` | `Rscript scripts/process_data/process_sao_paulo_data.R`<br>Declared input files must already exist. | sao_paulo preserved sources -> interim geography/stations, partitioned pollution and all census variants | Marcos reports a successful local run (24 September). The preserved weighting-area source now exists. Scientific review and isolated reproduction remain pending. |
| [scripts/run_pipeline.R](../../scripts/run_pipeline.R) | operational utility; **unverified** | Transitional; replace with compatibility launcher after acceptance. | `Rscript scripts/run_pipeline.R`<br>Declared input files must already exist. | Preserved sources -> transitional sequential manuscript execution | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/run_targets.R](../../scripts/run_targets.R) | operational utility; **unverified** | Targets: `paper_export` (launcher; accepts any declared target) | `Rscript scripts/run_targets.R all`<br>Declared input files must already exist. | Preserved sources and explicit graph -> selected target and prerequisites | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_aerosol_composition.R](../../scripts/tables_images/figure_aerosol_composition.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_aerosol_composition.R`<br>Declared input files must already exist. | Prepared MERRA-2 PM2.5 panels -> species-distribution PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_imputation_diagnostics.R](../../scripts/tables_images/figure_imputation_diagnostics.R) | manuscript; **unverified** | Targets: `figure_imputation_diagnostics` | `Rscript scripts/tables_images/figure_imputation_diagnostics.R`<br>Declared input files must already exist. | Predictions and station socioeconomic tables -> diagnostic PDFs | Named station ratios and plots, separate PDF saving. Fixture plot-layer parity checked; inspect real-data figures and captions before acceptance. |
| [scripts/tables_images/figure_kernel_distributions.R](../../scripts/tables_images/figure_kernel_distributions.R) | manuscript; **unverified** | Targets: `figure_kernel_distributions` | `Rscript scripts/tables_images/figure_kernel_distributions.R`<br>Declared input files must already exist. | Cleaned partitions -> temporal density/exceedance PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_merra2_vs_stations.R](../../scripts/tables_images/figure_merra2_vs_stations.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_merra2_vs_stations.R`<br>Declared input files must already exist. | Temporal series, inversion/geographic inputs -> satellite PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_missing_heatmap.R](../../scripts/tables_images/figure_missing_heatmap.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_missing_heatmap.R`<br>Declared input files must already exist. | Raw partitions and optional missingness summaries -> heatmap PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_pollution_quintile_maps.R](../../scripts/tables_images/figure_pollution_quintile_maps.R) | manuscript; **unverified** | Targets: `figure_pollution_quintile_maps` | `Rscript scripts/tables_images/figure_pollution_quintile_maps.R`<br>Declared input files must already exist. | Geography, stations, census, pollution membership -> map PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_pollution_stations_by_hour.R](../../scripts/tables_images/figure_pollution_stations_by_hour.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_pollution_stations_by_hour.R`<br>Declared input files must already exist. | Historical Santiago 2013 sample -> station-hour PDFs | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_population_density_maps.R](../../scripts/tables_images/figure_population_density_maps.R) | manuscript; **unverified** | Targets: `figure_population_density_maps` | `Rscript scripts/tables_images/figure_population_density_maps.R`<br>Declared input files must already exist. | Geography, stations, census, pollution membership -> map PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_quintile_kernel_distributions.R](../../scripts/tables_images/figure_quintile_kernel_distributions.R) | manuscript; **unverified** | Targets: `figure_quintile_kernel_distributions` | `Rscript scripts/tables_images/figure_quintile_kernel_distributions.R`<br>Declared input files must already exist. | IDW families -> exposure-density PDFs at 3/20 km | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_resolution_sensitivity.R](../../scripts/tables_images/figure_resolution_sensitivity.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_resolution_sensitivity.R --scope=reference`<br>Completed reference inputs; --scope=multicity requires frozen inputs and saved reference anchors. | Completed resolution estimates -> PDFs and review packet | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/figure_resolution_review.R](../../scripts/tables_images/figure_resolution_review.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_resolution_review.R`<br>Completed 3 km and 20 km multicity products and frozen resolution inputs. | Frozen inputs and saved multicity tables -> review CSVs and PDFs (IT reading, schooling ties, Santiago composition, monitoring by support, 3 km vs 20 km) | Estimates nothing; verify against the saved multicity tables. |
| [scripts/tables_images/figure_station_scatter.R](../../scripts/tables_images/figure_station_scatter.R) | manuscript; **unverified** | Targets: `figure_station_scatter` | `Rscript scripts/tables_images/figure_station_scatter.R`<br>Declared input files must already exist. | Station socioeconomic tables -> monitoring PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_station_temporal.R](../../scripts/tables_images/figure_station_temporal.R) | manuscript; **unverified** | Targets: `figure_station_temporal` | `Rscript scripts/tables_images/figure_station_temporal.R`<br>Declared input files must already exist. | Prepared temporal series -> hourly/episode PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/figure_stations_on_metro_area.R](../../scripts/tables_images/figure_stations_on_metro_area.R) | optional analysis; **blocked** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_stations_on_metro_area.R`<br>Declared input files must already exist. | Obsolete CDMX paths -> two interactive station HTML widgets | Review obsolete paths and equivalence; preserve the interactive purpose. |
| [scripts/tables_images/figure_study_area_maps.R](../../scripts/tables_images/figure_study_area_maps.R) | optional analysis; **unverified** | Outside manuscript targets: optional scientific question. | `Rscript scripts/tables_images/figure_study_area_maps.R`<br>Context geography; terrain tiles may require networking. | Context geography and optional terrain tiles -> context maps | Verify execution and scientific parity independently; preserve the specification. |
| [scripts/tables_images/generate_exposure_plots.R](../../scripts/tables_images/generate_exposure_plots.R) | manuscript; **maintained** | Targets: `generate_exposure_plots` | `Rscript scripts/tables_images/generate_exposure_plots.R`<br>Declared input files must already exist. | Observed/imputed regressions -> exposure PDFs | Isolated rendering and fresh-session execution checked on saved inputs (30 September 2026); review in RStudio and verify fresh upstream results. |
| [scripts/tables_images/plot_station_monitoring_figures.R](../../scripts/tables_images/plot_station_monitoring_figures.R) | manuscript; **unverified** | Targets: `plot_station_monitoring_figures` | `Rscript scripts/tables_images/plot_station_monitoring_figures.R`<br>Declared input files must already exist. | Distances, census and station socioeconomic tables -> monitoring PDFs | Compare manual operations and targets on identical inputs; complete acceptance. |
| [scripts/tables_images/render_census_tables.R](../../scripts/tables_images/render_census_tables.R) | manuscript; **maintained** | Targets: `render_census_tables` | `Rscript scripts/tables_images/render_census_tables.R`<br>Declared input files must already exist. | Census and distance-band summaries -> descriptive LaTeX tables | Isolated rendering and fresh-session execution checked on saved inputs (30 September 2026); review in RStudio and verify fresh upstream results. |
| [scripts/tables_images/render_exposure_tables.R](../../scripts/tables_images/render_exposure_tables.R) | manuscript; **maintained** | Targets: `render_exposure_tables` | `Rscript scripts/tables_images/render_exposure_tables.R`<br>Declared input files must already exist. | Exposure regressions -> education/income LaTeX tables | Isolated rendering and fresh-session execution checked on saved inputs (30 September 2026); review in RStudio and verify fresh upstream results. |
| [scripts/tables_images/render_missing_tables.R](../../scripts/tables_images/render_missing_tables.R) | manuscript; **maintained** | Targets: `render_missing_tables` | `Rscript scripts/tables_images/render_missing_tables.R`<br>Declared input files must already exist. | Missingness and education availability -> LaTeX tables | Isolated rendering and fresh-session execution checked on saved inputs (30 September 2026); review in RStudio and verify fresh upstream results. |
| [scripts/tables_images/render_station_tables.R](../../scripts/tables_images/render_station_tables.R) | manuscript; **maintained** | Targets: `render_station_tables` | `Rscript scripts/tables_images/render_station_tables.R`<br>Declared input files must already exist. | Counts, WHO, thresholds -> station/threshold LaTeX tables | Isolated rendering and fresh-session execution checked on saved inputs (30 September 2026); review in RStudio and verify fresh upstream results. |
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
  inputs by invoking manuscript exposure. For the 20 km robustness run, repeat the
  estimation with --buffer-km=20 (optionally --city=<id> and --duckdb-mem-gb=<n>);
  then run figure_resolution_review.R.
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
