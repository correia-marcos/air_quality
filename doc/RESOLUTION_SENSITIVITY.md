# Geographic-resolution sensitivity

This optional methodological analysis covers Bogotá (2018 census), Santiago (2017),
and São Paulo (2010). Mexico City is excluded. It neither exports manuscript artifacts
nor changes the maintained manuscript run, census definitions, station cleaning, or
original inputs. Analysis remains in R, with reusable functions in `src/` and explicit
execution in `scripts/`.

## Run

Use the project's restored R environment with the existing derived census, geometry,
station, distance, and exposure products present:

```sh
make resolution-multicity
```

This runs, in order:

```sh
Rscript scripts/process_data/prepare_resolution_inputs.R
Rscript scripts/process_data/estimate_resolution_sensitivity.R --scope=multicity
Rscript scripts/tables_images/figure_resolution_sensitivity.R --scope=multicity
```

The preparation step records input MD5 hashes and refuses an unreviewed change to an
existing input manifest. It does not download source replacements. A rerun may reuse B
exposures with `--reuse-b`; reuse requires matching hashes for pollution, distances,
IDW code, and frozen quintile cells. All other analyses and diagnostics are recomputed.
A `STATUS.txt` file says `incomplete` until city verification finishes. Figures require
three completed cities. Intermediate checkpoints from a failed run are not final results.

The historical Bogotá numerical mode remains the default. Its C figure label and
caption now identify the joint specification and use its own population counts. To verify it without overwriting the
original saved sensitivity products:

```sh
Rscript scripts/process_data/estimate_resolution_sensitivity.R --scope=reference \
  --output-dir=data/processed/resolution_sensitivity/reference_rebuild
Rscript tests/testthat.R --mode=synthetic
```

The old 500-draw bootstrap uses fine-unit resampling and remains a reference check.
It is not the new uncertainty procedure. Its C results are the historical combined
exposure-aggregation and area-classification specification, not classification-only C.

## Fixed definitions and city supports

All designs use cleaned 2023 station data, the existing pollutant definitions,
positive distances no greater than 3 km, inverse distance exponent 1, and hourly
renormalization over available readings. Representative points use the existing
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
| `results/tables/resolution_multicity_*.csv` | Compact three-city review tables |
| `results/figures/diagnostics/resolution_multicity*.pdf` | Separate design figures and one multipage review packet |
| `data/processed/resolution_sensitivity/reference_rebuild/` | Isolated historical Bogotá rerun |

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
