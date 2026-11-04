# Robustness checks

Each check here repeats the exposure analysis in [`../exposure/`](../exposure/) with one
choice changed, to show that the education and income gaps do not depend on it. The checks
run with the manuscript (`make all`, `Rscript scripts/run_targets.R all`, or `make
robustness` after the exposure stage). None of their tables or figures is among the exported
manuscript files in [`config/paper_artifacts.csv`](../../../config/paper_artifacts.csv).
Their settings are in the Robustness section of
[`config/analysis_settings.R`](../../../config/analysis_settings.R).

| Folder | Choice changed | Recipes, in run order | Outputs |
|---|---|---|---|
| `buffer_5km/` | IDW buffer: 5 km (`robustness_buffer_km`) instead of 3 km | `estimate_idw_5km.R`, `estimate_exposure_5km.R`, then [`figure_exposure_5km.R`](../../tables_images/robustness/buffer_5km/figure_exposure_5km.R) | `data/processed/idw_estimates/<city>/*_5km_*`, `data/processed/idw_regressions/exposure_*_5km_2023.*`, 18 PDFs in `results/figures/exposure/` |
| `individual_regression/` | Estimator: each person's exposure regressed on their education quintile, instead of group-level estimates | `estimate_exposure_individual.R` | `data/processed/idw_regressions/exposure_ci_estimates_education_individual_3km_2023.*` |

The exposure with imputed hourly readings is also a check on the main analysis, but its
figures are printed in the manuscript, so it has its own analysis folder,
[`../imputation/`](../imputation/).
