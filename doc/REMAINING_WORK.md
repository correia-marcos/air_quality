# What is left to build in this repo

What the manuscript needs that the **default pipeline** (`scripts/process_data/` →
`scripts/tables_images/`) does not yet produce. The legacy track is mentioned only where it
explains why something is absent.

Hand-maintained. Written August 2026, against
`doc/paper/IDB Discussion Paper March 2025.tex`, which carries **123 distinct
`\includegraphics` paths** and 5 `\input` targets. Re-check before acting.

---

## A. No producer anywhere — 20 figures

| Family | Count | Note |
|---|---|---|
| `descriptives/model2_<city>[_pm25][_scatter].png` | 16 | Imputation diagnostics: the linear prediction vs the actual hourly series, and the ratio of predicted-missing to observed means by station. **The data exists** — `impute_missing_hourly.R` writes `data/processed/imputed_ols/<city>_imputed` — but no script in `tables_images/` reads that folder. This is the largest single gap and the cheapest to close, because only the plotting is missing. |
| `Final/maps/<city>_population_density_map.png` | 4 | No producer for the data or the figure. |

## B. Producer exists, coverage is narrower than the paper's — 34 figures

| Family | Paper needs | Repo produces | Gap |
|---|---|---|---|
| `Final/stations_dis_num_<city>_{3,5,10}km.png` | 12 (4 cities × 3 radii) | 4 (3 km only) | `plot_station_monitoring_figures.R` hardcodes `radius_km = 3` at four call sites. Make it a vector and loop. |
| `kernel_plots/distribution_3km_<city>_<pol>.png` | 16 | 2 (Bogotá only) | `figure_exposure_by_quintile.R` sets `kernel_specs <- specs[1]`. The other three cities were commented out during development; the spec list is now correct, so widening to `seq_along(specs)` is a one-line change **once someone confirms `mode = "geo"` works for kernel density**. |
| `kernel_plots/all[_pm25][_2019/_2022].png` | 6 | 0 | Pooled-across-cities variants, and two earlier years. No producer. |
| `Final/maps/map_<city>_3km.png` | 4 | 1 (Bogotá, as PDF) | `bogota_fig_pollution_quintiles_geo_id.R` is Bogotá-only by construction — the name says so. Generalising it to four cities is the intended fix. |

## C. Producer exists and is current; the manuscript cites legacy filenames — ~64 figures

**This is a paper-side edit, not a code gap.** The new pipeline's figures are correct and
newer; the `.tex` still points at the Stata-era `.png` names. Nobody should "fix" working code
here.

| Manuscript path | Superseded by |
|---|---|
| `Final/plot_hours_above_IT{1,2}_<city>_2023_3km_reg1.png` (18) and the 2 `plot_decile_*` | `results/figures/exposure_by_group/ci/*_ci.pdf` (14 files) |
| `Final/plot_quintiles_<city>_all_mean_2023_3km_imp.png` and the `descriptives/` variants (12) | `results/figures/exposure_by_group/levels/*_levels.pdf` (7 files) |
| `descriptives/scatter_plot_<city>_*.png` (28) | `results/figures/station_monitoring/<city>_{avg,hours_it1,hours_it2}_pm10_pm25_vs_education.png` (12 files) |

Two caveats before treating this as purely editorial:

- The `_imp` (imputed) variants of the hours-above figures have **no** counterpart in the new
  pipeline. `impute_missing_hourly.R` produces the imputed panels, but no exposure figure
  consumes them. If the paper keeps the imputed robustness check, that wiring is real work.
- The counts do not line up one-for-one (18 legacy vs 14 new, 28 vs 12). Confirm the new
  figures cover every cell the paper reports before deleting a reference.

## D. Cited and produced under the same name — 5 figures

`hour_average/<city>_ridge_plot.pdf` (4) and
`hour_above_iterim_target/distribution_hours_above_IT2.pdf` (1), all from
`figure_merra2_vs_stations.R`. These are the only paths where the manuscript and this repo
already agree.

---

## Tables

The paper has 5 `\input` targets:

| Target | Status |
|---|---|
| `tables/table_census_coverage` | **Produced, different filename.** `render_paper_tables.R` writes `results/tables/census_summary/census_summary_table.tex`. Point the paper at it or rename. |
| `tables/table_descriptives_a` | **No producer.** |
| `tables/table_descriptives_b` | **No producer.** |
| `appendix_distance_computation` | Prose, maintained by hand in `doc/paper/`. |
| `data_appendix` | Prose, maintained by hand. |

Everything else `render_paper_tables.R` writes — station counts, WHO exceedances, the
by-dimension missing shares, the education-quintile availability table — is **not** `\input`
by the paper. Those are appendix and slide material, or were pasted in by hand.

---

## Carried forward from the refactor

Concrete items, each already flagged in the code where it matters:

1. **Re-run the four `process_<city>_data.R` scripts.** The census writers emit Parquet as of
   Step 5 and nothing reads CSV anymore, so the readers have no input until the writers run.
   Re-run rather than converting the existing CSVs — converting would bake in the type damage
   the change exists to prevent. Afterwards the stale `.csv` files under
   `data/interim/census/` can be deleted.
2. **`renv::snapshot()`** to record `geosphere`, which `geosphere::distm` uses but neither
   `DESCRIPTION` nor `renv.lock` declared. `renv.lock` is protected from Claude.
3. **A join bug in `station_education_quintile()`** (`src/general_utilities/process/diagnostics.R`).
   It does a bare `as.character(geo_id)` on both sides. For CDMX the census side is numeric
   `9002` → `"9002"` while the distance matrix, built from the gpkg, holds `"09002"`. Those
   never match, so the CDMX rows of the education-quintile availability table are probably
   empty. The fix is a `geo_id_width` column in the `compute_descriptive_tables.R` spec, but it
   changes a reported table, so it is a decision rather than a cleanup.
4. **The `panel <- "raw"` choice in `render_paper_tables.R`.** The by-dimension missing tables
   describe structural missingness (hours the network never reported). Switching to `"clean"`
   folds in what `detect_outliers.R` removed, which mixes two phenomena. One word to change if
   the appendix wants the other one.
5. **`doc/deletion_candidates.md`** lists 10 functions with no caller and the dead code the
   merges dropped. None has been deleted; each is a one-line removal.
