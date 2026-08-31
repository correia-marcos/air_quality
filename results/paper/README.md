# The manuscript's figures and tables

Everything the paper prints, and nothing else. Copy `figures/` and `tables/` into the
Overleaf project; every other artefact the pipeline produces stays under
`results/figures/` and `results/tables/`, which are the repo's own working outputs.

A figure the manuscript cites lives here and nowhere else. If you find the same file in
both trees, one of them is stale.

## What is in each folder

| Folder | Count | What it shows |
|---|---|---|
| `figures/monitoring_coverage/` | 12 | Stations within 3, 5 and 10 km of each census unit, and distance to the nearest one, against mean years of schooling |
| `figures/station_scatters/` | 28 | One point per station: hours above a WHO interim target, or the annual mean, against the education or income of the unit it sits in |
| `figures/exposure_by_quintile/` | 14 | Normalised exposure gaps by education quintile with clustered intervals, the two income panels, and the mean-by-quintile levels |
| `figures/exposure_imputed/` | 12 | The same specification estimated on the panels that combine observed and imputed readings |
| `figures/imputation_diagnostics/` | 16 | The imputation model's prediction against the observed series, and the ratio of predicted-missing to observed means by station |
| `figures/exposure_densities/` | 16 | Population-weighted exposure densities by education quintile, at 3 km and at 20 km (the `_v2` panels) |
| `figures/city_distributions/` | 6 | Hourly concentration densities by city, 2023 with 2019 and 2022 |
| `figures/hourly_profiles/` | 5 | Concentration by hour of day, and how long episodes above IT2 last |
| `figures/maps/` | 8 | Education quintiles with the 3 km station buffers, and population density |
| `tables/` | 3 | `table_census_coverage`, `table_descriptives_a`, `table_descriptives_b` |

Filenames are the manuscript's own, so they are not always descriptive: the
`distribution_3km_*_v2` files are the 20 km panels, and `_v2` on a
`stations_dis_num_*_3km` file marks a re-render rather than a different specification.

## Rebuilding

```sh
make figures tables
```

Every file here is written directly by a script; nothing is copied into place. The
freshness test (`tests/testthat/test-results-freshness.R`) fails if any of it is older
than the data it was built from.

## Updating the manuscript's paths

The folder names here describe content rather than mirroring the `.tex`, and every figure
is now a `.pdf`. `tex_path_mapping.csv` lists the old `\includegraphics` argument and its
replacement for all 117 paths, and `update_tex_paths.sh` applies them in one pass:

```sh
sh update_tex_paths.sh "IDB Discussion Paper March 2025.tex"
```

It writes a `.bak` beside the file it edits.
