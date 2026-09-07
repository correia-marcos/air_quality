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
| `tables/` | 11 | Every table the manuscript `\input`s — see below |

Filenames are the manuscript's own, so they are not always descriptive: the
`distribution_3km_*_v2` files are the 20 km panels, and `_v2` on a
`stations_dis_num_*_3km` file marks a re-render rather than a different specification.

### The tables

Each is a **bare `tabular`**, never a float: the manuscript supplies its own
`\begin{table}`, caption and label, so a fragment can be re-rendered without touching the
`.tex`. Written by three scripts.

| File | Written by |
|---|---|
| `table_census_coverage.tex` | `render_census_tables.R` |
| `table_descriptives_a.tex`, `table_descriptives_b.tex` | `render_census_tables.R` |
| `stations_by_pollutant_2023.tex` | `render_station_tables.R` |
| `table_days_above_thresholds.tex` | `render_station_tables.R` |
| `table_avg_hours_above_thresholds.tex` | `render_station_tables.R` |
| `missing_by_education_quintile_2023.tex` | `render_missing_tables.R` |
| `table_means_education_quintiles.tex` | `render_exposure_tables.R` |
| `table_means_income_groups.tex` | `render_exposure_tables.R` |
| `table_hours_above_education_quintiles.tex` | `render_exposure_tables.R` |
| `table_hours_above_income_groups.tex` | `render_exposure_tables.R` |

The two income tables print Mexico City in **quintiles** and São Paulo in **deciles**:
63 municipalities do not give enough clusters to identify ten coefficients. The
manuscript's captions say so; do not "fix" the asymmetry.

One table the manuscript prints is still hardcoded in the `.tex` and is not here: the WHO
interim-target values. Those are definitional constants, not a result of this pipeline.

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
replacement, and `update_tex_paths.sh` applies every row in one pass:

```sh
sh update_tex_paths.sh doc/paper/paper_draft_part1.tex
```

It writes a `.bak` beside the file it edits. The script reads its rules from the CSV, so
the two cannot drift apart — add a row there rather than editing the script.

The mapping carries 121 rows for 117 active paths: four figures are listed under both the
`_3km` and `_3km_v2` spelling, because different vintages of the draft cite them
differently. Rows that match nothing are simply no-ops.

`doc/paper/paper_draft_part1.tex` was rewritten with this in September 2026 and every one
of its 117 active `\includegraphics` targets now resolves under this folder. The six
commented-out figure blocks keep their legacy paths on purpose: nothing here can satisfy
them (see `doc/REMAINING_WORK.md` §A).
