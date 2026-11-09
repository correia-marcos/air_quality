# Run the analysis script by script

This guide lists every recipe that produces the paper's results, in an order that works, so
you can run them one at a time in RStudio and inspect what each one creates. The recipes are
grouped by analysis, the same names used by the folders in
[`scripts/`](../../scripts/README.md). Each analysis's figures and tables come right after
its processing.

## Before you start

- Open `Coding.Rproj` with the restored project environment
  ([setup](../HOW_TO_RUN.md#development-and-acquisition)).
- The preserved source files must already be in `data/downloads/`. Processing never
  downloads; the download recipes in
  [`scripts/download_data/inputs/`](../../scripts/download_data/inputs/) acquire them
  separately and need network access and provider credentials.
- Restart R before each recipe (Session > Restart R). Each recipe sources its own functions
  and settings and reads its inputs from disk, so it never depends on objects left by an
  earlier one.
- Run a recipe whole with *Source*, or line by line to inspect each named object. Each
  recipe's header lists its sections, from I. Import data to the saving of its outputs.

Every analysis reads the inputs of steps 1–6. Two steps also read another analysis, as noted:
step 25 needs step 12, and step 32 needs step 17. The exact manuscript files each recipe
writes are listed in [`config/paper_artifacts.csv`](../../config/paper_artifacts.csv).

<!-- run-order:start -->

## Inputs

What every analysis reads: geography, stations, census and cleaned hourly pollution
([folder guide](../../scripts/process_data/inputs/README.md)).

| Step | Recipe | Produces |
|---|---|---|
| 1 | [process_bogota_data.R](../../scripts/process_data/inputs/process_bogota_data.R) | Bogotá's geography, stations, hourly panel and census |
| 2 | [process_cdmx_data.R](../../scripts/process_data/inputs/process_cdmx_data.R) | The same for Mexico City |
| 3 | [process_santiago_data.R](../../scripts/process_data/inputs/process_santiago_data.R) | The same for Santiago |
| 4 | [process_sao_paulo_data.R](../../scripts/process_data/inputs/process_sao_paulo_data.R) | The same for São Paulo |
| 5 | [generate_distance_matrices.R](../../scripts/process_data/inputs/generate_distance_matrices.R) | Station-to-station and unit-to-station distances |
| 6 | [detect_outliers.R](../../scripts/process_data/inputs/detect_outliers.R) | Cleaned hourly panels and quality summaries ([method](../reference/outlier_cleaning.md)) |

Steps 1–4 can run in any order.

## Descriptives

Monitoring coverage, data availability and the census populations.

| Step | Recipe | Produces |
|---|---|---|
| 7 | [compute_descriptive_tables.R](../../scripts/process_data/descriptives/compute_descriptive_tables.R) | Station counts, missing-data shares, threshold and WHO exceedances, census summaries |
| 8 | [compute_distance_band_descriptives.R](../../scripts/process_data/descriptives/compute_distance_band_descriptives.R) | The population living within each distance band of a station |
| 9 | [render_station_tables.R](../../scripts/tables_images/descriptives/render_station_tables.R) | Station-count and exceedance tables |
| 10 | [render_missing_tables.R](../../scripts/tables_images/descriptives/render_missing_tables.R) | Missingness tables by station, month, hour and education |
| 11 | [render_census_tables.R](../../scripts/tables_images/descriptives/render_census_tables.R) | Census and distance-band tables |

## Monitoring

Where the stations are, and what they record, relative to education and income.

| Step | Recipe | Produces |
|---|---|---|
| 12 | [compute_station_scatter_inputs.R](../../scripts/process_data/monitoring/compute_station_scatter_inputs.R) | Each station's pollution outcomes with the education and income around it |
| 13 | [plot_station_monitoring_figures.R](../../scripts/tables_images/monitoring/plot_station_monitoring_figures.R) | Station coverage and outcomes by education |
| 14 | [figure_station_scatter.R](../../scripts/tables_images/monitoring/figure_station_scatter.R) | Station outcomes against education and income |
| 15 | [figure_population_density_maps.R](../../scripts/tables_images/monitoring/figure_population_density_maps.R) | Maps of population density and active stations |
| 16 | [figure_pollution_quintile_maps.R](../../scripts/tables_images/monitoring/figure_pollution_quintile_maps.R) | Maps of education quintiles and station coverage |

## Exposure

Residents' exposure, estimated by inverse-distance weighting
([worked example](../reference/idw_golden_test.md)), and its gaps between groups.

| Step | Recipe | Produces |
|---|---|---|
| 17 | [estimate_idw.R](../../scripts/process_data/exposure/estimate_idw.R) | Exposure of each geographic unit at 3 and 20 km, and its population groups |
| 18 | [estimate_exposure.R](../../scripts/process_data/exposure/estimate_exposure.R) | Exposure levels and gaps between socioeconomic groups at 3 km |
| 19 | [generate_exposure_plots.R](../../scripts/tables_images/exposure/generate_exposure_plots.R) | Exposure level and gap figures |
| 20 | [render_exposure_tables.R](../../scripts/tables_images/exposure/render_exposure_tables.R) | Appendix exposure tables |
| 21 | [figure_quintile_kernel_distributions.R](../../scripts/tables_images/exposure/figure_quintile_kernel_distributions.R) | Population-weighted exposure densities by education quintile |

## Imputation

The exposure gaps again, with missing station-hours filled from the other stations.

| Step | Recipe | Produces |
|---|---|---|
| 22 | [impute_missing_hourly.R](../../scripts/process_data/imputation/impute_missing_hourly.R) | Hourly panels with missing readings predicted from the other stations |
| 23 | [estimate_exposure_imputed.R](../../scripts/process_data/imputation/estimate_exposure_imputed.R) | Education-group exposure with the imputed panels |
| 24 | [figure_exposure_imputed.R](../../scripts/tables_images/imputation/figure_exposure_imputed.R) | Exposure gap and level figures with imputed readings |
| 25 | [figure_imputation_diagnostics.R](../../scripts/tables_images/imputation/figure_imputation_diagnostics.R) | Predictions against observed readings; reads step 12 |

## Temporal

Hourly PM2.5 through the day, and episodes at or above 50 µg/m³ (WHO interim target 2).

| Step | Recipe | Produces |
|---|---|---|
| 26 | [prepare_station_hourly.R](../../scripts/process_data/temporal/prepare_station_hourly.R) | City-hour PM2.5 means of the observed stations |
| 27 | [figure_station_temporal.R](../../scripts/tables_images/temporal/figure_station_temporal.R) | Hourly profiles and episode durations |
| 28 | [figure_kernel_distributions.R](../../scripts/tables_images/temporal/figure_kernel_distributions.R) | Hourly PM distributions across cities |

## Robustness

The exposure gaps with one choice changed
([folder guide](../../scripts/process_data/robustness/README.md)).

| Step | Recipe | Produces |
|---|---|---|
| 29 | [estimate_idw_5km.R](../../scripts/process_data/robustness/buffer_5km/estimate_idw_5km.R) | Exposure of each geographic unit at 5 km |
| 30 | [estimate_exposure_5km.R](../../scripts/process_data/robustness/buffer_5km/estimate_exposure_5km.R) | Exposure gaps at 5 km |
| 31 | [figure_exposure_5km.R](../../scripts/tables_images/robustness/buffer_5km/figure_exposure_5km.R) | Exposure gap and level figures at 5 km |
| 32 | [estimate_exposure_individual.R](../../scripts/process_data/robustness/individual_regression/estimate_exposure_individual.R) | Education gaps estimated person by person at 3 km; reads step 17 |

<!-- run-order:end -->

## The whole sequence at once

`source(here::here("scripts", "run_pipeline.R"))` runs the same recipes in one R session, and
`make all` runs them with stage stamps ([manuscript pipeline](../HOW_TO_RUN.md#manuscript-pipeline)).

## Beyond the paper

These analyses are optional and run after the steps above:

- [`not_in_paper/`](../../scripts/process_data/not_in_paper/README.md): Santiago with the
  2024 census, satellite comparisons, supporting diagnostics and the legacy validation.
- [`specification_sensitivity/`](../../scripts/process_data/specification_sensitivity/README.md):
  geographic support, IDW buffer and exponent, and education groupings
  ([guide](../RESOLUTION_SENSITIVITY.md)).
