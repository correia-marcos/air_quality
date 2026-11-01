# Specification sensitivity: geographic support, IDW buffer and exponent, education groups

October 2026: the exercise grew beyond geographic resolution and is now the
specification-sensitivity analysis. **Design A is the primary design**, estimated on a grid
of IDW buffers (1–20 km) and exponents (1 and 2); Designs B and C are robustness checks at
3 and 20 km with exponent 1. Its scripts, functions and outputs moved to
`specification_sensitivity/` subfolders, the pilot of a subfolder layout; the
[folder README](../scripts/process_data/specification_sensitivity/README.md) is the file
map. Inputs were re-frozen on 14 October 2026 from the 13 October manuscript products; the
previous outputs are archived in `data/{interim,processed}/resolution_sensitivity/_frozen_20261001/`.

Migration review, 5 October 2026: the four supported education definitions and quintile
default are retained. Portable numerical checks pass; this structural migration does not
accept the existing optional results as scientifically reproduced. Historical code manifests
must be assessed against the actual candidate code; frozen input files and saved optional
outputs were not regenerated or relabelled.

This optional methodological analysis covers Bogotá (2018 census), Santiago (2017),
and São Paulo (2010). Mexico City is excluded. It neither exports manuscript artifacts
nor changes the maintained manuscript run, census definitions, station cleaning, or
original inputs. Analysis remains in R, with reusable functions in `src/` and explicit
execution in `scripts/`.

## Run

Use the project's restored R environment with the existing derived census, geometry,
station, distance, and exposure products present:

```sh
make resolution
```

After the one-time Bogotá crosswalk (`build_bogota_localidad_crosswalk.R`, a frozen
input), this runs, in order:

```sh
Rscript scripts/process_data/specification_sensitivity/prepare_resolution_inputs.R
Rscript scripts/process_data/specification_sensitivity/estimate_resolution_sensitivity.R
Rscript scripts/tables_images/specification_sensitivity/figure_resolution_sensitivity.R
```

The recipes read no options. The variants they run are declared in
`config/analysis_settings.R`: `resolution_cities`, `resolution_specifications` (one IDW
buffer and one individual education grouping per id; quintile runs first) and
`resolution_figure_specifications` (the variants with review figures).
`resolution_specification()` validates each one and derives its folders and file prefix.
One declared variant runs alone through the specification-sensitivity targets graph, for
example `make targets STAGE=resolution_education_group3_20km`. Undeclared values (another
buffer, grouping or city subset) run with
`Rscript tools/explore/run_resolution_sensitivity.R --run-id=<id> --buffer-km=<km>
--grouping=<g>`, which calls the same functions and writes only to the git-ignored
`data/exploration/<id>/`.

The primary Design A grid then runs with `make specification-grid`, and after it
`make specification-panel`; the review script (`figure_resolution_review.R`) runs between
them. See the folder README for the order.

The preparation step records input MD5 hashes and refuses an unreviewed change to an
existing input manifest. It does not download source replacements. The recipes rebuild
every B exposure; only the exploration launcher can reuse one (`--reuse-b`), and only
when the hashes of pollution, distances, IDW code and frozen quintile cells match. A
`STATUS.txt` file says `incomplete` until city verification finishes. Combined tables and
figures require every declared city to be complete.

The B rebuild runs in DuckDB with the memory limit `resolution_duckdb_mem_gb` (32 GB) in
`config/analysis_settings.R`. Changing it changes run time and whether the run fits in
memory, not the estimates. To run on a smaller machine, lower it to fit the machine, or
the memory Docker Desktop allows in the container. The 3 km variants fit in 4 GB; Bogotá
at 20 km ran out of memory at 4 GB and ran at 32 GB. The exploration launcher takes
`--duckdb-mem-gb=<n>` for one run.

The historical Bogotá reference was retired in October 2026. It was the first,
Bogotá-only version of this analysis: the manuscript's manzana exposure re-keyed to five
supports, with Design A, a joint A/C specification and a 500-draw manzana bootstrap. Its
five tables remain as an archived, read-only input in
`data/processed/resolution_sensitivity/resolution_{ci,bootstrap,variance,decomposition,coverage}_bogota_3km_2023.*`.
The 3 km quintile variant checks that its Bogotá Design A ladder reproduces all 150
archived coefficients (`bogota_2018/all_levels_reference_anchor`).

## Fixed definitions and city supports

All designs use cleaned 2023 station data, the existing pollutant definitions, and hourly
renormalization over available readings. The robustness designs use positive distances no
greater than 3 km (or 20 km) and inverse distance exponent 1; the Design A grid varies
the buffer and the exponent (see below). Representative points use the existing
`st_point_on_surface()` construction after UTM validity repair and transformation to
WGS84. A city-specific evaluation CRS is fixed from the finest support for B.

Annual threshold outcomes count available hourly estimates **at or above** the existing
numerical thresholds: PM10 IT1/IT2 = 150/100 and PM2.5 IT1/IT2 = 75/50. These are hourly
peak-exposure outcomes, not WHO daily-compliance statistics. Missing hourly estimates
do not count as zero exposure. Report observed hours with threshold counts.

Individual education quintiles are taken from the maintained adult-group products,
formed before exposure selection. Their assignment is frozen: schooling ties depend
on geographic ID and row order, so re-cutting after geographic aggregation would change
the classification. Person weights remain unit weights for Bogotá/Santiago and existing
IBGE expansion weights for São Paulo. Adults are 25 or older under existing processing.

| City | Included supports | Specific qualification |
|---|---|---|
| Bogotá | Fine unit → sección → sector → municipio; separate fine → localidad/municipio → municipio | Fine units mix urban blocks and rural sections. Sections/sectors do not nest in localidades. |
| Santiago | Zona censal → distrito censal → comuna | Districts/comunas are metropolitan portions dissolved from selected 2017 zones, not whole administrative territories. |
| São Paulo | Área de ponderação → município | Individual microdata identify weighting areas; finer residence is not inferred. |

The identifier ledger preserves raw and canonical IDs. The crosswalk retains missing
parents; the unmatched-population table retains adults with no finest polygon. They can
enter native B at a valid parent support, but cannot enter a fine exposure assignment
without a fine match. Population audit counts and geometry counts are separate.

Rejected candidates are part of the audit, not silently omitted:

| City | Candidate | Reason for exclusion from the first ladder |
|---|---|---|
| Bogotá | Identifier prefixes 6–14 | About 80.6% of adults remain in one group; little useful intermediate urban resolution. |
| Bogotá | Department or metropolitan total | Insufficient within-city variation and geographic clusters. |
| Santiago | 2017 manzana | Geometry exists; maintained person-to-residence linkage stops at zona. |
| Santiago | Provincia | Five metropolitan portions; pilot B reached only one. Inferential support is inadequate. |
| Santiago | 2024 comuna | Changes census vintage, population, and geography simultaneously. |
| Santiago | Region/metropolitan total | Insufficient within-city resolution. |
| São Paulo | Census tract | 30,815 geometries exist, but weighting-area microdata do not identify finer individual residence. |
| São Paulo | District/subdistrict | 164 distinct IDs exist; no validated weighting-area nesting crosswalk. |

Counts in the rejection table document the approved repository audit, not newly fitted
specifications. The retained-level audit is generated on each run and is authoritative
for the current frozen inputs. No pooled estimate is produced; similar administrative
names do not imply equivalent size, population, or census definitions across cities.

## Design A: exposure aggregation

For each outcome, aggregate saved finest-level annual outcomes with all retained adult
population weights over the predeclared covered fine support, then assign the parent mean
back to the same fine-unit/quintile cells. Education-reporting adults estimate inequality;
all adults weight the aggregation. This deliberately preserves the Bogotá reference.
Averages of annual threshold counts differ from threshold counts of average hourly
concentrations. A never reconstructs a pollution surface or a parent monitoring matrix.

The fixed sample uses the intersection of valid crosswalks across the included supports.
Currently the extra Bogotá locality exclusions do not remove covered adults. Membership
and weights are checked exactly for every A outcome and level. Population variance uses
the aggregation weights and support. The between/within identity and non-increasing
variance are checked along each validated nested branch; no monotonicity is imposed on
the inequality gap or across the sector/locality branches.

## Design B: reconstructed exposure inputs

Dissolve finest polygons, construct new points, calculate new distances in the fixed city
CRS, apply the original eligibility rule, and rerun the maintained IDW estimator. Geometry
without adults remains part of the geographic support. Keep the station catalog and
reporting histories fixed; eligible and actually contributing stations may change.

Native B includes education-reporting adults with exposure at that level. Pairwise common
B restricts to people covered by both that level and the finest exposure. Both retain
frozen individual quintiles and weights. Native changes are decomposed exactly into:

1. selection from the native B population to the common population;
2. the B versus finest procedure difference on common people;
3. selection from the finest population to the common population.

The middle difference is also split using A on the common people. This distinguishes
exposure aggregation from the additional B procedure change; it does **not** separately
identify causal effects of point movement, station selection, missingness, and weights.
These mechanisms are reported jointly through matrices and exposure diagnostics.

Bogotá municipio, Santiago comuna, and São Paulo município remain visible even with weak
coverage or few clusters. Their gaps and profiles are descriptive; undefined contrasts
remain unavailable. No automatic larger buffer or fewer socioeconomic groups is used.

## Design C: classification only

Keep finest exposure fixed. Compute each area's mean education using education-reporting
population as denominator, then apply the repository's cumulative-population grouping
convention with total-adult area weights. Keep a fixed intersection of individually
classified adults whose areas can be classified at every included level. Report the five
profiles, actual population shares, and individual-to-area transition tables.

Large areas can skip cumulative quintile bins. Empty groups are not forced into existence;
missing Q1 or Q5 yields an unavailable gap. C changes the reference populations and must
not be interpreted as the same Q1–Q5 contrast as A or B. C is supplementary and descriptive.

## Design A over the IDW buffer and exponent grid (primary)

`estimate_idw_specification_grid.R` reruns the maintained estimator,
`aggregate_idw_exposure()`, on the frozen fine distance matrix and 2023 panel at buffers
{1, 2, 3, 5, 8, 10, 12, 15, 17, 20} km with exponent 1, and at {1, 2, 3, 5, 10, 20} km
with exponent 2 (`resolution_grid_*` in
`config/analysis_settings.R`; the manuscript keeps `idw_distance_power <- 1`). Exponent-1
files at 3, 5 and 20 km must reproduce the manuscript exposure files. Station catalogs
extend 20 km beyond each metropolitan area, so no buffer exceeds 20 km.
`estimate_design_a_grid.R` estimates Design A on every specification for the four
groupings, supports and six outcomes (3,840 estimates). Exponent 2 covers six
buffers because of the runtime budget of the 14 October 2026 run
(`resolution_grid_power2_buffers_km`); the variance decomposition uses the buffers present
at both exponents.

*What each choice changes.* The buffer decides which stations are eligible (`0 < d ≤ b`)
and therefore which fine units have exposure: the Design A sample grows with the buffer
and the samples are nested. The exponent only reweights eligible stations (`w = 1/d^p`):
samples, coverage and observed hours are identical at both exponents, so an exponent
comparison at a fixed buffer is a pure-weighting comparison on the same people. A unit
with one eligible station has the same exposure under any exponent; the exponent matters
where two or more stations are in range, which is rare at 3 km and common at 20 km.
Exponent 2 approaches nearest-station assignment and damps the smoothing of large
buffers. Annual means, threshold hours (non-linear in the hourly value), group means,
gaps, `sd0` and the variance retained by aggregation can change; distances, station
counts and coverage cannot.

*Threshold ties.* Threshold outcomes count interpolated hours at or above integer
thresholds, and many station readings are integers. Where one station sets the hourly
value, the interpolated `(v·w)/w` can round just below `v`, so an hour exactly at a
threshold is counted or not depending on the weight. `design_a_grid_threshold_ties.csv`
reports the share of station-hours exactly at each threshold, and
`design_a_grid_single_station.csv` the hour differences between exponents for
single-station units, which can only come from this rounding. The estimator is unchanged;
these tables bound the numerical part of threshold-hour differences.

## Fixed-sample decomposition across buffers

Let G(s; S) be the Design A gap with exposure specification s = (buffer, exponent)
averaged over sample S, r = (3 km, exponent 1) the reference at the same city, grouping,
support and outcome, and S_c = S_s ∩ S_r the people covered by both:

```text
G(s; S_s) − G(r; S_r) =  [G(s; S_c) − G(r; S_c)]     exposure change, same people
                       + [G(s; S_s) − G(s; S_c)]     people added by the larger buffer
                       − [G(r; S_r) − G(r; S_c)]     people the reference has and s lacks
```

Samples are nested in the buffer, so one sample term is zero. The other ordering would need
exposure for units outside the smaller buffer's coverage, which does not exist; this path
is the only computable one, and it is not a Shapley average. The exposure term bundles
more stations, more smoothing and different weights. A change dominated by the sample term
means the larger buffer describes a different population; it does not show that the 3 km
estimate is biased for its own population. Headline estimates always keep each buffer's
own sample; the common-sample gaps are supplementary.

| Comparison | People | What the difference measures |
|---|---|---|
| Supports, Design A, same buffer, exponent and grouping | Fixed (asserted) | Exposure aggregation only |
| Exponents, Design A, same buffer | Fixed (asserted) | Weighting only |
| Groupings, same specification | Same people and weights (asserted) | A different contrast |
| Buffers, own samples | Nested, changing | Exposure construction and population represented |
| Buffers, common sample | Fixed | Exposure construction only |
| Native B across supports (robustness) | Changing | Rebuilt exposure and selection |
| C across supports (robustness) | Fixed, area groups | Classification change |

## Station sets

The grid interpolates every station with a valid reading of the pollutant in 2023. A
station set keeps fewer stations by a rule on their reporting, declared in
`resolution_station_sets` (`config/analysis_settings.R`). A station enters the network of
pollutant p when p and every pollutant in `require` have valid hours in at least
`min_months` calendar months. Two sets run at exponent 1 on
`resolution_station_set_buffers_km` (1, 2, 3, 5, 10, 20 km):

| Set | Rule | What it removes |
|---|---|---|
| `both_pollutants` | `require` = PM10 and PM2.5, `min_months` = 1 | Stations that report only one pollutant |
| `three_months` | `require` = none, `min_months` = 3 | A pollutant's records shorter than three months |

*Where the rule sits.* Reporting is known only once the hourly panel exists, and the hours
that enter interpolation only after outlier cleaning. The city recipes choose stations by
location before any reading exists. Outlier cleaning compares each reading with
neighbouring stations, so removing stations before it would change the cleaned values of
the stations kept. The rule therefore sits after cleaning and before interpolation:
`station_reporting()` counts hours and months in the frozen 2023 panel,
`station_set_members()` applies the rule, and `restrict_station_panel()` blanks the
readings outside the set. IDW drops a missing reading from the numerator and the
denominator of each hourly mean, so a blanked reading is the same as removing that station
for that pollutant; `aggregate_idw_exposure()` runs unchanged on the frozen fine matrix.
The manuscript graph is unchanged.

*What a set can change.* A station that never reports a pollutant never contributes to
that pollutant's exposure, so `both_pollutants` changes PM2.5 only by removing PM2.5-only
stations and PM10 only by removing PM10-only stations. Removing stations can only shrink a
unit's eligible set, so each set's sample is nested in the default sample at the same
buffer. `estimate_design_a_station_sets.R` splits the change from the default stations at
the same buffer with the fixed-sample decomposition above (the people both cover, then
the people the set loses), and the change from the set's own 3 km estimate the same way.

*Checks.* Wherever a set keeps a pollutant's whole network, its exposure must equal the
default grid file at the same buffer (`default_anchors`). The default Design A gaps are
recomputed and must equal the saved grid; set samples must sit inside the default samples
and nest in the buffer; both decompositions must add up. Two groupings: the frozen
quintiles and quintiles with split ties. The variance decomposition is not rerun for the
sets.

## Specification panel and variance decomposition

`build_specification_panel.R` writes `specification_panel.csv`: one row per Design A
estimate of the lowest-minus-highest gap for one city, outcome, grouping, buffer, exponent
and support (a complete factorial within each city). B and C are excluded: they exist at
two buffers and one exponent and change several things at once.

| Field | Role | Definition | Units |
|---|---|---|---|
| `spec_id` | identifier | city, outcome, grouping, buffer, exponent, support | — |
| `city_id`, `census_year`, `outcome`, `pollutant`, `metric`, `outcome_unit` | identifier | city and census vintage covary; metric is annual mean or hours at or above IT1/IT2 | — |
| `grouping`, `n_groups`, `contrast_label` | choice | education grouping and its headline contrast | — |
| `buffer_km`, `distance_power` | choice | IDW eligibility radius; IDW exponent | km; 1, 2 |
| `level`, `level_label`, `level_rank`, `branch` | choice | support; rank 1 is finest; Bogotá's localidad branch | — |
| `sample_key` | sample | city, pollutant and buffer: equal keys mean identical people | — |
| `sample_population`, `sample_fine_units`, `sample_share_city` | sample | education-reporting adults, fine units, share of the city's education-reporting adults | persons; count; 0–1 |
| `low_group_share`, `high_group_share` | sample | population shares of the endpoint groups in the sample | 0–1 |
| `overlap_with_reference`, `share_in_reference` | sample | share of the 3 km sample in this sample; share of this sample in the 3 km sample | 0–1 |
| `n_clusters`, `n_level_units` | mechanism | support units with sampled adults; units at the support | count |
| `pw_unit_area_km2`, `median_unit_area_km2`, `pw_unit_population` | mechanism | area of the unit the average sampled adult lives in; median area; Σp²/Σp sampled adults | km²; km²; persons |
| `share_variance_retained` | mechanism | exposure variance retained after averaging over the support | 0–1 |
| `mean_eligible_stations`, `share_adults_multistation`, `mean_static_neff`, `mean_max_weight_share`, `median_nearest_eligible_km`, `mean_observed_hours` | mechanism | station weights of the specification, averaged over sampled adults | count; 0–1; count; 0–1; km; hours |
| `gap`, `low_mean`, `high_mean`, `normalized_pct`, `standardized_gap`, `sd0` | estimate | lowest minus highest group; 100(low/high − 1); gap / sd0 of this specification | outcome units; %; SD |
| `gap_paper_sd` | estimate | gap / sd0 of the paper baseline: one fixed scale per city and outcome | SD |
| `se`, `lower`, `upper`, `inference_status` | estimate | HC1 where the existing screens pass | outcome units |
| `gap_paper`, `delta_vs_paper`, `n_choices_vs_paper`, `sign_change_vs_paper` | reference | paper baseline (quintiles, finest, 3 km, exponent 1), change from it, choices changed | outcome units; count |
| `gap_ref_exposure`, `delta_exposure_total`, `delta_exposure_fixed_sample`, `delta_exposure_composition` | reference | change from 3 km and exponent 1 at the same grouping and support, decomposed | outcome units |
| `gap_ref_fine`, `delta_resolution` | reference | change from the finest support at the same specification (same people) | outcome units |

Fixed constants (2023, AEQD distances, point-on-surface points, hourly renormalization,
adults aged 25+, all-adult aggregation weights) are not columns. Groupings change the
estimand, so compare them on `gap_paper_sd` or `normalized_pct` with the endpoint shares.
`normalized_pct` is unstable when `high_mean` is near zero; no cutoff is applied.

`estimate_specification_variance_decomposition.R` splits the variance of the gap over each
city's complete grid exactly into main effects and interactions of support, grouping,
buffer and exponent (a saturated factorial ANOVA, i.e. a functional-ANOVA or Sobol
decomposition with every chosen value weighted equally). It reports first-order and total
shares, for all buffers and for buffers of 3 km or more, because the shares describe the
chosen grid. A buffer where some gap is undefined is dropped from that city's grid and
recorded. There is no test or interval: rows are deterministic transformations of the same
people and stations, not independent observations. For the same reason, and because
sample descriptors are functions of the choices, choices are nested in cities and adjacent
buffers are near-duplicates, a LASSO would select arbitrarily among aliased predictors;
the exact decomposition is preferred.

## Estimands and conditional uncertainty

Primary: weighted Q1 minus Q5 exposure, in documented concentration units or hours.
Also report all five means, population shares, normalized percent differences
`100 * (Q1 / Q5 - 1)`, and a standardized gap using the **fixed finest-level all-adult SD**.
Zero Q5 means or zero baseline SD produce NA for the corresponding normalized measure.

New paired comparisons use 999 draws, seed 20260910, resampling the comparison's coarser
parent units: Bogotá sección/sector and Santiago distrito. Each draw carries all fine
units and quintile cells together. Repeated parents contribute repeatedly without a
Cartesian join. Recomputing A within a whole-parent draw gives the same parent mean;
precollapsed parent/group numerator sums implement that identity exactly. B exposure
outputs are held fixed for common-sample resampling. Store every paired replicate.

Intervals are 95% percentile **conditional geographic-cluster bootstrap intervals**.
All draws must contain both endpoint groups; otherwise the interval is unavailable.
Localidad/comuna/municipal comparisons and C are descriptive. Native B decompositions
between changing populations do not receive paired confidence intervals.

As a conservative operational check, inference additionally requires at least 50 clusters,
effective population-weighted cluster count at least 30, and no cluster containing 20%
or more of the estimation population. These are transparent screening rules, not a
coverage guarantee. Level-specific HC1 geographic-cluster intervals for A/native B are
secondary and also require all five groups and a full-rank saturated model. The gap's
closed-form covariance is checked against the maintained weighted regression.

These intervals condition on pollution data and exposure construction. They omit station
measurement, interpolation-model, time-series, and full survey-design uncertainty.
Resampling parents does not eliminate spatial dependence or dependence through shared
stations. Exact observed-population differences are the primary evidence.

## Education groupings (optional)

The frozen individual quintiles cut inside tied schooling values, and
`assign_socio_group()` then splits the tied adults by geographic-code order (up to 30% of
a city's adults sit in a split value). Each declared variant names one individual
grouping. Every alternative uses the same people and weights as the quintile run
(asserted unit by unit); only the labels change.

| Grouping | Groups | Headline contrast | Output folder |
|---|---|---|---|
| `edu_quintile` (default) | Frozen quintiles; ties split by geo_id order | Q1 − Q5 | `resolution_sensitivity/` |
| `edu_quintile_split` | Quintiles; each tied value split across adjacent quintiles in proportion | Q1 − Q5 | `quintile_split/` |
| `edu_group3` | Three attainment groups: bands 1–2, 3–4, 5–6 below | Below secondary − bachelor's or more | `education_group3/` |
| `edu_level` | The six attainment bands below | None − graduate | `education_level/` |

Split quintiles (`split_socio_group_cells()`) give every adult with one schooling value the
same shares of the quintiles that value spans, so each quintile holds exactly 20% of the
weight and no geographic order enters. Cells then carry fractional weights. This is the
bridge to the paper's quintile specification.

The attainment groupings use the six bands each census module derives from the harmonized
`educ_years` (`education_level_bands` in `config/analysis_settings.R`):

| `edu_level` | Band | Label | Three groups | Bogotá | Santiago | São Paulo |
|---|---|---|---|---:|---:|---:|
| 1 | `no_education` | None | 1 Below secondary | 2.3 | 1.8 | 5.6 |
| 2 | `high_school_incomplete` | Below secondary | 1 Below secondary | 29.2 | 30.9 | 46.7 |
| 3 | `high_school_complete` | Secondary | 2 Secondary to some tertiary | 28.3 | 30.4 | 29.8 |
| 4 | `college_incomplete` | Some tertiary | 2 Secondary to some tertiary | 13.3 | 18.3 | — |
| 5 | `college_complete` | Bachelor's | 3 Bachelor's or more | 18.8 | 15.1 | 14.5 |
| 6 | `graduate_educ` | Graduate | 3 Bachelor's or more | 8.1 | 3.5 | 3.4 |

Percentages are shares of education-reporting adults; the three groups hold 31.5/41.5/27.0
(Bogotá), 32.7/48.7/18.6 (Santiago) and 52.3/29.8/17.9 (São Paulo). `assign_education_level()`
reads the level from the bands, so each source's own rules apply, and every reporting adult
must fall in exactly one band. A city's groups are those populated in its frozen population,
so São Paulo's structurally empty level 4 is never imputed. The six-band endpoints are
small (1.8–5.6% and 3.4–8.1%), and in Bogotá level 1 is less exposed than level 2. The three
groups keep every group large and populated in every city; they are the recommended
headline, pending the coauthors' decision, with the six-band profiles as a supplement.

The bands are not coded identically across censuses. Bogotá 2018 provides only
`P_NIVEL_ANOSR`, the highest level attended without a completion flag; the public person
files for Bogotá and Cundinamarca do not contain `P_NIVEL_ANOS`. Its "Universitario"
(17 years) therefore includes non-completers: 16.7% of adults aged 25 or older at that
level report current attendance (10.8% at técnica/tecnológica). São Paulo maps incomplete
college down to 12 and Santiago uses years approved. Technical tertiary degrees sit in
"Some tertiary" in Bogotá and Santiago; Brazilian tecnólogo degrees are expected among
completed graduação (17), which has not been verified against the IBGE codebook. These
differences matter for cross-city magnitudes more than for within-city resolution changes.

Estimands, decomposition, variances, bootstrap and feasibility screens are unchanged; the
gap is the lowest minus the highest group and HC1 requires every group of the city. In C,
both quintile groupings use area quintiles of mean schooling, and the attainment groupings
give each area the weighted median group of its education-reporting adults (lower median
on ties). With six bands few areas have an endpoint median, so the C gap is unavailable at
every support except Bogotá's fine units, where the level-1 area group holds 26 adults.

Alternative groupings reuse the quintile run's B exposures at the same buffer after
checking their pollution, distance-matrix and IDW-code hashes; interpolation never sees
socioeconomic groups. The matrix diagnostics are rerun as an independent reconciliation:

```sh
make targets STAGE=resolution_education_group3_3km     # or any declared id
make targets STAGE=resolution_education_group3_20km
Rscript scripts/tables_images/specification_sensitivity/figure_resolution_review.R
```

The quintile variant at the same buffer runs first; the validator refuses any other
declared order. Tables are named
`results/tables/specification_sensitivity/robustness_designs/resolution_multicity_<folder>_[20km_]*`. The review script draws the
3 km versus 20 km native-B comparison for every grouping and the Design A headline gaps of
the four definitions side by side (`resolution_review_definitions_*`). Quintile products,
the frozen inputs and the historical Bogotá anchor, which applies to quintiles only, are
untouched.

## Monitoring matrices and storage

Persisted repository matrices are long-format station-to-station and geography-to-station
distances. Eligibility is an implicit positive-distance ≤3 km filter. Static weights are
inverse distances; actual normalized weights vary by pollutant and hour. Station-to-station
distances and the fine pollution surface are invariant inputs to A.

For B, save full new distance Parquets, eligible edges, points, manifests, and per-unit
summaries. Distinguish catalog, active, eligible, and contributing stations. Diagnostics
include nearest distances, eligible station counts, population coverage, static/effective
station counts, weight concentration, row sums, zero-weight hours, contributing stations
gained/lost, and representative-point movement. Independently reconstruct hourly values
one unit at a time to reconcile annual means and threshold counts against core IDW.
No geography × station × hour tensor or new sparse-matrix dependency is introduced.
The rebuilt fine matrix is retained as verification evidence, alongside its comparison
with the original; it can be reconstructed from the frozen inputs.

## Outputs and review

| Location | Contents |
|---|---|
| `data/interim/resolution_sensitivity/<city>/` | Frozen input/source manifests, raw/canonical ID ledger, crosswalks, adult/quintile cells, nesting, geometries, points, unit populations, displacement |
| `data/processed/resolution_sensitivity/<city>/B/<level>/` | Rebuilt distances, exposure, frozen groups, eligible edges, station manifests, unit matrix diagnostics |
| `data/processed/resolution_sensitivity/<city>/` | Profiles, contrasts, paired differences/replicates, variance, sample decomposition, classification transitions, audits and verification |
| `results/tables/specification_sensitivity/robustness_designs/resolution_multicity_*.csv` | Compact three-city robustness tables |
| `results/figures/specification_sensitivity/robustness_designs/resolution_multicity*.pdf` | Separate design figures and one multipage review packet |
| `data/processed/resolution_sensitivity/idw_grid/power_<p>/<city>/` | Fine IDW exposure per buffer and exponent; `grid_manifest`, `manuscript_anchors` |
| `data/processed/resolution_sensitivity/design_a_grid/`, `results/tables/specification_sensitivity/design_a/` | Design A grid: samples, support sizes, station weights, contrasts, profiles, decomposition, threshold ties, single-station checks, checks |
| `data/processed/resolution_sensitivity/specification_panel/`, `results/tables/specification_sensitivity/panel/` | Specification panel, profiles, parity and regression anchors, checks, variance decomposition |
| `results/figures/specification_sensitivity/design_a/` | Design A grid figures and variance shares |
| `data/processed/resolution_sensitivity/station_sets/<set>/` | Restricted 2023 panels and fine IDW exposure per station set; `set_manifest`, `default_anchors`, `snapshot.csv` |
| `data/processed/resolution_sensitivity/station_sets/design_a/`, `results/tables/specification_sensitivity/station_sets/`, `results/figures/specification_sensitivity/station_sets/` | Station-set samples, station weights, contrasts, profiles, decompositions, checks, comparison table, station maps and figures |
| `results/tables/specification_sensitivity/review/stations_by_pollutant_sets_2023.csv`, `station_reporting_2023.csv` | Stations by pollutant (four cities), valid hours before and after cleaning, months, first and last hour |
| `results/tables/specification_sensitivity/review/support_variance_*.csv`, `results/figures/specification_sensitivity/review/support_variance.pdf` | Support size, variance retained, gap relative to the finest support and intervals; change of the gap against the variance lost |
| `results/tables/specification_sensitivity/review/`, `results/figures/specification_sensitivity/review/` | Coauthor review tables and figures, including the 3 km versus 20 km Design A comparison |
| `data/processed/resolution_sensitivity/resolution_*_bogota_3km_2023.*` | Archived tables of the retired Bogotá reference; read-only input of the 3 km quintile anchor |
| `data/processed/resolution_sensitivity/<folder>/[buffer_20km/]<city>/` | Same products for an alternative grouping (`quintile_split`, `education_group3`, `education_level`); its group cells; copied B exposures with `exposure_source.csv` |
| `.../robustness_designs/resolution_multicity_<folder>_[20km_]*.csv` and figures, `.../review/resolution_review_<folder>_buffer_*`, `resolution_review_definitions_*` | Alternative-grouping tables, figure packets, 3 km versus 20 km figures and the four-definition comparison |

Main plots use named city-specific supports, coarse to fine, with separate Bogotá locality
panels. Supplementary plots use log covered-unit counts without assuming geographic
comparability. No smoothing or monotone inequality constraint is applied. Whiskers in
matrix distribution figures are descriptive boxplot whiskers, not confidence intervals.

## Verification boundary and interpretation

The run checks identifiers, valid geometry, representative points, nesting, exact A cells,
fresh fine distances/IDW, group preservation, matrix eligibility and normalization, exposure
reconciliation, variance identities, sample decomposition, and inferential feasibility.
Synthetic tests cover unequal weights, missing education, tied schooling, missing parents,
non-nesting, empty groups, zero denominators, threshold nonlinearity, the 3 km boundary,
zero distances, missing hours, bootstrap duplication, and regression equivalence.

Preserved Santiago geometry responses and São Paulo's unfiltered weighting-area source
snapshot are absent at their declared locations. The derived geometries permit this
conditional analysis with recorded hashes. This is **not a clean-source reproduction**.
Recovering or replacing those snapshots needs a separate source-data decision. No full
release reproduction or independent researcher review is claimed.

A studies scale sensitivity of exposure assignment on fixed people and SES groups. B
studies the complete procedure a researcher with coarser data would apply. C studies
socioeconomic classification. None establishes personal exposure truth, a causal effect,
a MAUP zoning effect, or a universal advantage of finer supports. Residential assignment,
non-population-centered representative points, monitoring interpolation and gaps, historical
censuses, missing education, mobility, indoor exposure, and time-activity patterns remain
limitations. Findings may conditionally inform environmental-justice screening and
monitoring assessment; they do not establish optimal station placement or intervention
effects. Start with a coauthor methodological appendix; review results before any manuscript
prose reconciliation or export.

## Execution evidence

See the generated verification tables and `STATUS.txt` files for the current run. Test
results, reference comparisons, visual inspection, actual deviations from the pilot, and
remaining limitations are recorded below after implementation verification.

### Verified execution, 16 September 2026

- All three cities completed A, native/common B, classification-only C, and matrix
  diagnostics for their retained supports. Thirty-six paired intervals used all 999
  valid draws each. São Paulo's cross-resolution estimates remain descriptive.
- All five historical Bogotá products matched the isolated reference rerun exactly,
  including the 150 bootstrap summaries from 500 draws. The reference's 30 finest-level
  coefficients also matched the maintained paper table within `1.5e-12`. The new A
  implementation matched all 150 normalized group coefficients across the Bogotá ladder
  within `1.2e-12`.
- Fresh finest distance matrices matched saved distances exactly in all cities.
  Fresh finest IDW outcomes passed their saved-exposure comparisons; A cell membership
  and weights, B frozen groups, and weighted-mean/regression anchors passed.
- All 60 matrix/outcome reconciliation checks passed, with a numerical qualification:
  eight Santiago level/outcome checks required floating-point threshold-boundary
  envelopes. Each affected unit differed by at most one hour between independent R
  reconstruction and core DuckDB arithmetic. The core outputs and `>=` thresholds were
  retained. The diagnostic envelope is `64 * machine epsilon * threshold`; it is never
  used to round exposure, adjust thresholds, or replace analytical outcomes. Bogotá
  and São Paulo reconciled without this qualification.
- The synthetic suite passed **279 checks, zero failures, errors, skips, or warnings**.
  Scripts parsed and the Makefile dry run confirmed the three optional stages. Analysis
  used Framework R 4.6.1 and the project's installed library; no package or lockfile
  changes were made. Session information and code/input hashes accompany the outputs.
- The 16-page figure packet was rendered and visually inspected, including the separate
  locality branch, missing C groups, matrix panels, and conditional interval labels.
- Raw/download/legacy data and the manuscript pipeline were not modified. Full isolated
  release reproduction and independent researcher review were not performed.

The primary Design A annual-mean gaps are city-specific:

| City | Pollutant | Finest gap | Municipio/comuna gap |
|---|---|---:|---:|
| Bogotá | PM10 | 7.264 | 1.118 |
| Bogotá | PM2.5 | 2.956 | 0.349 |
| Santiago | PM10 | 2.111 | 2.115 |
| Santiago | PM2.5 | 1.610 | 1.633 |
| São Paulo | PM10 | 0.750 | 0.206 |
| São Paulo | PM2.5 | 0.084 | 0.188 |

These are weighted Q1–Q5 differences in the repository's concentration units, conditional
on the retained inputs. Preserve each city's reporting/reference-volume conventions;
no new cross-city conversions were introduced. Fine support does not uniformly produce
a larger gap. The pattern table records non-monotonic paths without imposing them.

Coarse B coverage agrees with the pilot: Bogotá municipio has 4 covered units and
224,520 education-reporting adults for each pollutant; Santiago comuna has 12 units and
1,449,393 adults; São Paulo município has 6 PM10 units (about 1,134,538 adults) and 4 PM2.5
units (about 848,105 adults). Coverage and populations differ from A. The municipal C
classification populates only two groups in Bogotá and three in São Paulo.

The geographic audit now distinguishes 49 Bogotá adult-populated IDs without finest
polygons from the separate locality-crosswalk exclusions. The population-denominator
output separately reports retained residents, adults, education-reporting adults, and
Santiago's read-only source-census counts. Source counts for Bogotá/São Paulo were not
independently rebuilt from raw during this extension and remain explicitly unavailable
in that field. The area audit records child-area minus dissolved-union area, including
small overlaps/repair differences; it is not proof of coverage against an independent
metropolitan boundary.

Matrix summaries distinguish catalog eligibility from active-pollutant eligibility.
Per-unit `eligible_stations` and static effective-station counts use stations with at
least one observed 2023 reading of that pollutant. Eligible-edge files also retain
catalog stations without such readings. Under the current positive-weight rules, an
active eligible station contributes in at least one observed hour; these last two
station counts consequently coincide. Observed-hour distributions remain explicit.

Current optional products occupy approximately 0.5 GB, mostly inspectable geometry and
long Parquet matrices; the combined vector figure packet is approximately 6 MB. No hourly
weight tensor was retained. Rejected geographic candidates remain documented above.

### Education groupings, 1 October 2026

- `edu_quintile_split`, `edu_group3` and `edu_level` each ran at 3 km and 20 km from the
  same code revision. Every verification table matches the quintile run at that buffer:
  all checks passed, with the same eight Santiago threshold-boundary qualifications at
  3 km and descriptive-only São Paulo inference. All paired intervals (36 per run) used
  999 valid draws. Regression anchors agreed within 4e-11.
- Unit education totals matched the quintile cells (exactly, or within 4e-15 relative
  for split quintiles and São Paulo's non-integer weights). Split quintiles hold 20.0% of
  the weight each. Variances, matrices, reconciliations, point movement and populations
  by design, support and outcome were identical to the quintile run; exposure changes
  differed by at most 4e-14 relative.
- With default arguments, the generalized code reproduced all 24 saved quintile tables of
  the three cities at zero tolerance. The rerun six-band gaps equal the first run's.
- Independent data.table recomputations of the finest and coarsest Design A gaps agreed
  within 1.3e-9 (3 km) and 6.4e-9 (20 km) absolute, from summation order over up to
  9.9 million weighted adults.
- Figure packets, the 3 km versus 20 km figures and the definition comparison were
  rendered and their main pages inspected. The review script's quintile tables were
  byte-identical and its quintile figures rendered identically after the additions.

Design A annual-mean PM10 gaps, lowest minus highest group, 3 km:

| City | Support | Quintiles, geo-order ties | Quintiles, split ties | Three groups | Six levels |
|---|---|---:|---:|---:|---:|
| Bogotá | Finest | 7.264 | 5.982 | 5.725 | 6.026 |
| Bogotá | Municipio | 1.118 | 1.217 | 1.215 | 1.204 |
| Santiago | Finest | 2.111 | 2.131 | 2.168 | 4.533 |
| Santiago | Comuna | 2.116 | 2.136 | 2.168 | 4.542 |
| São Paulo | Finest | 0.750 | 0.808 | 0.818 | 1.094 |
| São Paulo | Municipio | 0.206 | 0.164 | 0.185 | 0.261 |

The geographic-order tie split changes Bogotá's headline materially. With ties split
proportionally, the finest annual-mean gap falls from 7.26 to 5.98 (PM10) and from 2.96 to
2.07 µg/m³ (PM2.5), while the PM10 IT1 gap rises from 16.5 to 21.1 hours; at 20 km the
PM10 annual-mean gap falls from 4.06 to 2.99. Santiago changes little in annual means and
by 7–8% in threshold hours; São Paulo's PM2.5 annual-mean gap changes sign (0.084 to −0.096).
The finest Design A gap reproduces the maintained regression, so the same sensitivity
applies to the paper's quintile estimates.

Split quintiles and the three groups agree closely in every city and buffer, although the
three groups compare different population shares (Bogotá 30% and 29%, Santiago 37% and
14%, São Paulo 48% and 23% at the endpoints). Six levels compare 2–9% endpoints; Santiago's
gap doubles because of its small graduate group. Within each definition, the resolution
pattern is unchanged, and Design B changes at coarse supports remain mostly selection at
3 km (Santiago comuna PM10 IT1: +96 h, of which +98 h selection) and vanish at 20 km.
With three groups, C is defined at every support except Bogotá and São Paulo municipio;
Bogotá's localidad C gap is −6.3 µg/m³ and rests on few endpoint-median areas.

#### Six levels in detail

- `--grouping=edu_level` at 3 km completed all three cities in about four minutes. The
  verification table matches the quintile run's: all checks passed, with the same eight
  Santiago threshold-boundary qualifications and descriptive-only São Paulo inference.
  All 36 paired intervals used 999 valid draws.
- Every reporting adult fell in exactly one band. Unit education totals matched the
  quintile cells exactly in Bogotá and Santiago and within 6e-15 relative in São Paulo,
  whose weights are non-integer. The regression anchor agreed within 4e-11.
- Variances, matrices, reconciliations and point movement were identical to the quintile
  run, as were populations by design, support and outcome. São Paulo's exposure changes
  differed at most 5e-16 relatively, again through summation order.
- With default arguments, the generalized code reproduced all 24 saved quintile
  profile, contrast, interval, replicate, transition, composition, variance and
  exposure-change tables of the three cities at zero tolerance (scratch recomputation).
- An independent data.table recomputation of 24 finest and coarsest Design A gaps agreed
  within 5.7e-10 absolute (Bogotá municipio, IT2 hours) and 1.5e-10 relative. These are
  summation-order differences over 3.6 million weighted adults.
- The figure packet was rendered and its main pages were inspected.

Design A annual-mean gaps, none − graduate versus the quintile Q1 − Q5:

| City | Pollutant | Finest, levels | Finest, quintiles | Coarsest, levels | Coarsest, quintiles |
|---|---|---:|---:|---:|---:|
| Bogotá | PM10 | 6.026 | 7.264 | 1.204 | 1.118 |
| Bogotá | PM2.5 | 2.078 | 2.956 | 0.208 | 0.349 |
| Santiago | PM10 | 4.533 | 2.111 | 4.542 | 2.116 |
| Santiago | PM2.5 | 2.237 | 1.610 | 2.267 | 1.633 |
| São Paulo | PM10 | 1.094 | 0.750 | 0.261 | 0.206 |
| São Paulo | PM2.5 | 0.030 | 0.084 | 0.176 | 0.188 |

The resolution pattern is unchanged: Bogotá is stable through sector and attenuates at
localidad and municipio, Santiago does not move, and São Paulo PM10 shrinks at municipality.
Endpoints differ from quintiles, so magnitudes are not comparable one-to-one. In Bogotá the
"None" level is less exposed than "Below secondary", so the none − graduate gap is smaller
than the gradient from levels 2–4. Full profiles should accompany the headline contrast.

The 20 km level run (`--grouping=edu_level --buffer-km=20`) took about 14 minutes. It
reused the 20 km quintile B exposures, whose inputs were unchanged, and wrote
`education_level/buffer_20km/` and `results/tables/resolution_multicity_education_level_20km_*`.
All verification checks passed; no Santiago boundary envelopes were needed, as in the
20 km quintile run. All 36 paired intervals used 999 valid draws. The regression anchor
agreed within 1.4e-11. Grouping-independent tables were identical to the 20 km quintile
run, except São Paulo's exposure changes (at most 5e-14 relative). The independent
recomputation agreed within 9.9e-10 absolute.

| City | Pollutant | Finest, 3 km | Finest, 20 km | Coarsest, 3 km | Coarsest, 20 km | Finest, 20 km quintiles |
|---|---|---:|---:|---:|---:|---:|
| Bogotá | PM10 | 6.026 | 3.120 | 1.204 | 0.195 | 4.059 |
| Bogotá | PM2.5 | 2.078 | 0.936 | 0.208 | -0.044 | 1.390 |
| Santiago | PM10 | 4.533 | 2.612 | 4.542 | 2.581 | 1.663 |
| Santiago | PM2.5 | 2.237 | 0.807 | 2.267 | 0.802 | 0.607 |
| São Paulo | PM10 | 1.094 | 0.746 | 0.261 | 0.220 | 0.509 |
| São Paulo | PM2.5 | 0.030 | -0.110 | 0.176 | -0.011 | -0.063 |

These are Design A annual-mean none − graduate gaps. The 20 km sample is larger (Bogotá
5.55 million reporting adults rather than 4.26 million; Santiago 3.93 rather than 1.43;
São Paulo 9.87 rather than 3.21), and wide buffers smooth exposure, so the 3 and 20 km
gaps differ in both population and exposure construction. The resolution pattern within
each buffer is the same as at 3 km. As with quintiles, Design B selection nearly vanishes
at 20 km: Santiago's comuna selection component falls from 5.79 to 0 µg/m³ (PM10).
Bogotá's B municipio level still covers only 1.08 million adults because the Bogotá D.C.
representative point lies 22.2 km from the nearest station. Wide buffers smooth hourly
peaks: Bogotá's finest IT1 gaps fall to 0.42 h (PM10) and 0.10 h (PM2.5), against 20.9 h
and 10.8 h at 3 km. `scripts/tables_images/figure_resolution_review.R` draws the 3 km
versus 20 km native-B comparison for both groupings; the level versions are
`resolution_review_education_level_buffer_{pm10,pm25}.pdf` and
`resolution_review_education_level_buffer_gaps.csv`. Its quintile outputs were unchanged by
this addition. Isolated release verification and researcher review were not done.

### Specification grid and full rerun, 14–15 October 2026

- **Snapshot.** The 1 October outputs were archived under
  `data/interim/resolution_sensitivity/_frozen_20261001/` and
  `data/processed/resolution_sensitivity/_frozen_20261001/` (including the former flat
  `results/` tables and figures). Inputs were re-frozen from the 13 October derived files
  (about 3 minutes). Every later step ran from this one snapshot. The grid script stops if
  an input manifest or `snapshot.csv` differs.
- **Robustness designs.** A/B/C ran at 3 and 20 km for all four groupings, exponent 1
  (17:32–23:05, 14 October, including one run stopped for memory contention and rerun).
  All eight verification tables pass, with the usual Santiago 3 km threshold-boundary
  qualifications and descriptive São Paulo intervals.
  - Two changes were needed:
    - the B rebuild reads the panel's `year=2023` folder, because the new `_audit/`
      Parquet broke hive-partition reading;
    - DuckDB spills to a fixed temporary folder.
  - Bogotá's 20 km B fine exposure was copied from the grid's identical exponent-1 file.
    `exposure_source.csv` records this; the copy matched the manuscript file within
    2.8e-14.
- **IDW grid.** Ten buffers ran at exponent 1 and six at exponent 2 (1, 2, 3, 5, 10,
  20 km). The exponent-2 subset was a runtime reduction: Bogotá at 20 km took 80 minutes.
  Bogotá exponent-2 times were 34 s (1 km), 162 s (3 km) and 632 s (10 km).
  - Two crashes forced a lower configuration: a bus error (external disk or swap) and a
    DuckDB out-of-memory error at 4 threads. The grid then ran with 2 threads, 16 GB, a
    spill on the internal disk, and a resumable loop (17:34, 14 October – 00:32,
    15 October).
  - Santiago and São Paulo exponent-2 files at 8, 12, 15 and 17 km remain from the first
    attempt. They are unused.
  - Grid exposures at exponent 1 equal the manuscript IDW files at 3, 5 and 20 km: 72 of
    72 comparisons, max 6.8e-14. The md5 hashes of the 43 manuscript files in
    `data/processed/idw_estimates/` were identical before and after; a Finder `.DS_Store`
    file appeared in that folder.
- **Design A grid** (27 minutes) and **panel.** All nine grid checks and all panel checks
  pass:
  - 3,840 rows;
  - decomposition identity within 1.4e-14;
  - variance identity within 7.3e-10;
  - parity with the robustness Design A rows at 3 and 20 km: 480 of 480. This uses a
    relative population tolerance of 1e-10, because exact equality of floating sums
    failed on 156 rows;
  - manuscript regression anchor: 18 of 18, within 1.3e-12.

  The variance decomposition drops 8, 12, 15 and 17 km (no exponent 2) and records the
  drop.
- **Threshold ties.**
  - Between 0.001% and 0.53% of station-hours equal a threshold exactly
    (`design_a_grid_threshold_ties.csv`).
  - For single-station units, annual means agree across exponents within 3.6e-14.
    Threshold hours differ by up to 56 hours (Santiago PM2.5), because the hourly
    $vw/w$ can round one unit in the last place below an integer threshold
    (`design_a_grid_single_station.csv`).
  - The estimator was not changed. The same arithmetic applies to the manuscript's
    threshold outcomes.
- **Tests and figures.**
  - The synthetic suite passed **1,043 checks, zero failures, errors, skips or warnings**.
  - The grid, review and variance figures were rendered and inspected. The exponent-change
    figure was corrected to connect only buffers run at both exponents.
  - Isolated release verification and researcher review were not done.

### Station sets, 21–22 October 2026

- **Run.** `make specification-station-sets` from the 14 October snapshot, at exponent 1
  on 1–20 km. Santiago and São Paulo took about two minutes per set. Bogotá's
  `both_pollutants` run overlapped a period when the machine slowed or slept (about six
  hours of wall time). Its `three_months` run took 50 minutes (20 km: 38 minutes). A
  rerun of Bogotá at 1 km took 28 s and reproduced the overnight file exactly.
- **Resume fix.** The first resume stopped because the snapshot stored the rules as
  deparsed R code, which `fread()` did not read back unchanged. The snapshot now has one
  plain row per city and set. The old file was rewritten after its input checksums were
  confirmed to match.
- **Checks.**
  - Set exposures equal the default grid wherever a set keeps a pollutant's whole
    network: 144 of 144, max 8.5e-14. That covers Santiago in both sets, Bogotá PM2.5 in
    `both_pollutants` and São Paulo PM10 in `three_months`.
  - The recomputed default Design A equals the saved grid (360 rows, zero difference).
  - Set samples nest in the default and in the buffer.
  - Both decompositions add up within 1.4e-14.
  - The São Paulo and Santiago tables equal an earlier scratch run exactly.
  - The md5 of all 5,501 existing derived and result files are unchanged.
  - The synthetic suite passed 1,062 checks, with zero failures or warnings.
- **Networks.** Bogotá keeps 47/47 stations under `both_pollutants` and 45/44 under
  `three_months`; Santiago keeps 10/10 under both. São Paulo keeps 18/18 under
  `both_pollutants` and 28/18 under `three_months`, which drops the five December-only
  PM2.5 records and Pinheiros (November–December).
- **Results.** Bogotá and Santiago gaps keep their signs and barely move. In São Paulo:
  - `both_pollutants` keeps 71% of the 3 km PM10 sample and raises its mean gap from 0.75
    to 1.25;
  - `three_months` removes 0.91 of 2.86 million adults from the 3 km PM2.5 sample and
    doubles its IT2 gap (19.4 to 39.4 hours);
  - the município PM2.5 gap stays above the finest gap at 3 and 20 km under both sets.

  The interpretation is in the coauthor review note.

### Small implementation choices

A dedicated minimal dependency loader keeps this optional workflow from invoking package
installation or acquisition-specific setup. The fresh finest distance matrix is retained
alongside its original-input hash and exact comparison, so the isolated exposure rebuild
is directly reviewable; this adds approximately 43 MB for Bogotá relative to referencing
the original alone. Coarse outputs use the same long-Parquet convention. Whole-parent
bootstrap numerator aggregation is an exact computational shortcut for rebuilding each
parent mean inside each sampled parent draw, as verified by synthetic examples.

For Santiago, the source audit reproduces 6,139,087 residents and 4,037,849 adults before
missing-education processing, compared with 5,931,919 retained residents and 3,930,887
retained adults. Pollution coverage tables use the retained adult denominator and keep
these source counts separate.
