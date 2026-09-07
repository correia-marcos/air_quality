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

**The draft has been rewired to `results/paper/`** (1 September 2026). All 117 active
figure paths now resolve there, and the draft carries **11 `\input` table targets** plus
`data_appendix`. That folder holds exactly the manuscript's deliverables and nothing else;
`results/figures/` and `results/tables/` keep the repo's own working artefacts. Filenames
are the manuscript's, but the folders are named for what the figures show rather than
mirroring the `.tex`, and everything is `.pdf`. `results/paper/tex_path_mapping.csv` gives
the old and new `\includegraphics` argument, and `update_tex_paths.sh` applies every row.

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
