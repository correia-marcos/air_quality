# Particulate screening and source provenance

The observed analysis applies a configurable quality hold to standardized hourly PM2.5
and PM10 before the existing statistical outlier procedure. Marcos approved the revised
defaults of 2,000 and 6,000 µg/m³ on 8 October 2026. Equality passes. Above-bound readings
remain preserved in the input and linked diagnostic records, but enter calculations as
missing. The main analysis applies these bounds uniformly, with no individual retain or
exclude decisions. Concentrations are never capped or rescaled.

These are project screening thresholds, not established physical maxima or estimated
probabilities of error. [Kelleher et al. (2018)](https://amt.copernicus.org/articles/11/1087/2018/)
reported a 24-hour PM2.5 concentration of 915 µg/m³ near a prescribed fire; a daily average
does not establish an hourly ceiling. [Shahsavani et al. (2012), abstract reproduced by
EPA](https://hero.epa.gov/reference/1255944/) reports an overall PM10 maximum of
5,337.6 µg/m³, while 2,028 µg/m³ describes the peak discussed for its longest dust event.
These examples motivate broader bounds than the initial 500/1,000 choices. They do not
derive the exact 2,000/6,000 values or validate every observation below them.

[SINAICA Manual 5](https://sinaica.inecc.gob.mx/archivo/guias/5%20-%20Protocolo%20de%20Manejo%20de%20Datos%20de%20la%20Calidad%20del%20Aire.pdf),
§3.2.1.1 and Cuadro 3 (printed p. 17), lists both 0–500 and 0–1,000 µg/m³ as typical
operating ranges for each particulate pollutant. It also allows local variation. It does
not assign one range to PM2.5 and the other to PM10. Those typical ranges are not used as
universal exclusion limits. The current
[BAM-1020 manufacturer datasheet](https://metone.com/wp-content/uploads/2026/03/MetOne-BAM-1020-Datasheet-v2.2-20260319-1.pdf)
reports a measurement range extending to 10,000 µg/m³; its
[older manual](https://metone.com/wp-content/uploads/2019/05/BAM-1020-9800-Manual-Rev-W.pdf)
distinguishes configured analog ranges and extended reporting. Neither establishes which
instrument or channel operated at CALPULALPAN in 2023. Station-specific ranges remain
unconfirmed. The bounds are not physical maxima or WHO health thresholds.

## Follow the observed data

The CDMX source audit motivated this additional stage. CALPULALPAN's 79,999 µg/m³ reading
was explicitly present in an archived unvalidated download. Its original station-month
p99 was 80,532.76, so the existing strict `value > p99` gate never tested that observation.
A contaminated month can also inflate the temporal and spatial reference distributions.
Statistical cleaning measures relative inconsistency; it does not detect every gross
source anomaly. Screening first prevents above-bound values from entering those moments.

The city merger retains its existing source selection and averaging. Screening follows
that standardization, so the reviewed quantity is the standardized station-hour, not every
unaggregated source record. Original downloads are preserved. Negative source values
continue to be withheld by the existing city processing; zero remains eligible.

`detect_pollution_outliers()` calls `screen_pollution_quality()` before percentiles,
temporal differences, spatial moments or eligible-neighbor selection. The same screening
covers the adjacent hours read across year boundaries. The existing p99, two-SD tolerances,
strict comparisons and missing-benchmark fallbacks remain unchanged. Changing quality
holds can change those benchmarks and statistical decisions at other hours.

The source clock is unchanged. Arrow boundary literals now match the stored timestamp
type. The implementation preserves the cleaner's existing use of appended boundary rows
when assessing neighbor availability; a review of its narrower documented year-only
interpretation is separate work.

The analytical panel now uses the five-column interface in the
[data dictionary](data_dictionary.md#9-observed-particulate-quality-fields): original
concentration, source validation, screening reason, statistical reason and final value.
In observed datasets a finite final value identifies a retained observation, without a
separate boolean. Statistical reason is missing when screening prevented assessment.
Source status from the merger becomes `source_validation` in the cleaned panel.

Actual CDMX source contributions are retained in the cleaned dataset's
`_audit/source_contributions/` partitions, including links for readings that survive.
The four temporal/spatial SD diagnostic counts are retained once per station-month in
`_audit/station_month_diagnostics.parquet`. Input identities are recorded separately in
`_audit/input_partitions.csv` and on original rows. These file-backed tables preserve
traceability without repeating the metadata throughout the hourly panel. Opening the
cleaned Arrow dataset excludes `_audit/`; its contents remain independently readable and
belong to the same targets file output.

## Change screening settings

Parameters enter through reusable functions. Recipes and targets pass the same named
setting from `config/analysis_settings.R`:

```r
pollution_upper_bounds <- c(pm25 = 2000, pm10 = 6000)
```

Use `upper_bounds = NULL` to disable the added upper bounds, or
`c(pm25 = Inf, pm10 = 6000)` to disable only PM2.5. Negative and nonfinite inputs remain
unusable. There are no per-observation eligibility gates or documented-decision overrides
in the current interface. The empty `config/pollution_quality_reviews.csv` remains a
historical artifact and is not read by recipes or targets. Activating exceptions would
require a separate approved method and a schema that records them explicitly.

The direct outlier recipe exposes four named quality summaries and screened-record tables
in RStudio. Targets cache those objects separately and declare CSV files under
`data/processed/pollution_quality/`. Existing `*_quality_review_records` target names and
`*_review_records.csv` filenames remain compatibility names: their contents are bound or
negative screening exclusions, not a pending-review workflow or manual decisions.
Summaries count source validation, screening codes and statistical reasons separately.
The station catalog and balanced station-year rows remain present. Usable reporting counts
may change when screening settings change.

## Revised 2023 comparison and column simplification

The bounded check at `data/verification/particulate-bounds-20261008/` reuses the preserved
2023 inputs and the preceding donor hour. The revised bounds withhold 44 CDMX PM2.5
readings and no PM10 readings in the four-city comparison. CDMX has 7 IT2 episodes and
28 episode hours. All three remaining Primaria Revolución concentrations (1,013, 899 and
578 µg/m³) exceed the monthly p99 of 489.84, but pass the unchanged temporal band. Their
benchmark differences are 103, -114 and 124, within approximately (-126.26, 127.59).
They remain eligible; no manual exclusion is introduced to obtain a preferred result.
The two São Paulo PM10 extremes are already statistical removals with reason 3.
These checks are not a complete source-to-manuscript rebuild or a reviewed release baseline.

The compact schema was approved and implemented on 8 October 2026 after these bounds
were accepted. Earlier evidence retains its older column names and categories; it must
not be treated as a revision-matched release baseline for newly generated audit products.

## Inspect CDMX provenance

`cdmx_merge_pollution_data(include_source_metadata = FALSE)` retains its previous output
interface by default. The current processing recipe, preparation adapter and targets enable
metadata explicitly. Both engines preserve actual contributing sources after negative
masking and the existing canonical-code join. A mean using validated and unvalidated
sources is `mixed`, preserving the averaging rule while disclosing that contribution.

The merger writes `cdmx_metro_source_manifest.csv` and `cdmx_metro_source_coverage.csv`
beside the interim dataset. Their linked IDs connect provider, archive path and SHA-256
to parsed station/pollutant/year coverage, units, first/last records, contributing counts,
negative counts and missing values. Requested query dates and historical retrieval dates
are explicitly unknown when no acquisition evidence supplies them; observed coverage is
not substituted for a requested query period. Manifests stay outside the Parquet directory.
Future acquisition logs retain small and incomplete runs as well as the secondary feed.
Do not run acquisition merely to populate historical metadata.

## Keep imputation and unresolved source work distinct

Original values and reasons describe the observed input even in an imputed robustness
dataset. OLS predictions retain the existing `OLS_imputed` identification; they do not become agency
validated observations. The existing imputation policy may fill quality-held gaps and may
change the first-finite-reading window. That behavior requires explicit sensitivity review;
the current implementation does not select a different model.

Suspicious-pattern methods, validated 2023 replacements, whole-station or interval
exclusions, and instrument-specific overrides are deferred for coauthor review. See
[remaining work](../planning/remaining-work.md) for acceptance evidence and those decisions.
Portable tests and a bounded source replay do not establish complete scientific reproduction.
