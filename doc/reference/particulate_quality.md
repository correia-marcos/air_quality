# Particulate screening and source provenance

The observed analysis applies a configurable quality hold to standardized hourly PM2.5
and PM10 before the existing statistical outlier procedure. The defaults are 500 and
1,000 µg/m³, respectively. Equality passes. Above-bound readings remain preserved in the
input and linked review records, but enter calculations as missing until a documented
review retains them. Concentrations are never capped or rescaled.

These are project review thresholds. [SINAICA Manual 5](https://sinaica.inecc.gob.mx/archivo/guias/5%20-%20Protocolo%20de%20Manejo%20de%20Datos%20de%20la%20Calidad%20del%20Aire.pdf),
§3.2.1.1 and Cuadro 3 (printed p. 17), lists both 0–500 and 0–1,000 µg/m³ as typical
operating ranges for each particulate pollutant. It also allows local variation. It does
not assign one range to PM2.5 and the other to PM10. Our differing defaults are explicit
analytical choices within those examples. The current
[BAM-1020 manufacturer datasheet](https://metone.com/wp-content/uploads/2026/03/MetOne-BAM-1020-Datasheet-v2.2-20260319-1.pdf)
reports a measurement range extending to 10,000 µg/m³; its
[older manual](https://metone.com/wp-content/uploads/2019/05/BAM-1020-9800-Manual-Rev-W.pdf)
distinguishes configured analog ranges and extended reporting. Neither establishes which
instrument or channel operated at CALPULALPAN in 2023. Station-specific ranges remain
unconfirmed. The bounds are not physical maxima or WHO health thresholds.

## Follow the observed data

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

| Field, for each pollutant | Meaning |
|---|---|
| `pm25_source_status` | Acquisition evidence: `validated`, `raw_unvalidated`, `mixed`, or `unknown`. This is independent of project screening. |
| `pm25_source_ids` | Actual nonnegative, finite contributing source IDs, linked to the CDMX manifest. Missing when none contribute. |
| `pm25_input` | Original standardized hourly concentration before project screening or statistical removal. |
| `pm25_qa_status` | `not_flagged`, `pending_review`, `reviewed_retained`, `excluded`, or `missing`. `not_flagged` does not certify validity. |
| `pm25_qa_reason` | Quality reason, independent of statistical removal reasons. |
| `pm25_qa_eligible` | Logical permission to enter the statistical cleaner. |
| `pm25_use` | Logical usability of the final observed concentration. |
| `pm25_outlier`, `pm25_outlier_reason` | Existing statistical decision and reason code. A held reading can have reason zero because it never enters that calculation. |
| `input_id`, `station_original` | Partition SHA-256 identity and original station label for review keys. |

PM10 has the same fields. Other cities receive `unknown` source status unless the input
already contains documented pollutant-specific status. The current CDMX source status
comes from its archived acquisition routes, which do not supply an individual validation
flag for every reading. It is not inferred from concentration magnitude or outlier flags.

## Change settings or document a decision

Parameters enter through reusable functions. Recipes and targets pass the same named
settings from `config/analysis_settings.R`:

```r
pollution_upper_bounds <- c(pm25 = 500, pm10 = 1000)
pollution_eligibility_cols <- NULL
```

Use `upper_bounds = NULL` to disable the added bounds, or `c(pm25 = Inf, pm10 = 1000)`
to disable only PM2.5. `eligibility_cols = c(pm25 = "pm25_qa_eligible")` requires an
existing logical column: only TRUE permits use; FALSE excludes and NA holds. The function
rejects missing or nonlogical mapped columns. An eligibility flag cannot clear a bound.

The initially empty `config/pollution_quality_reviews.csv` routes documented decisions by
city. Each decision also requires the original station, timestamp, pollutant, partition
`input_id`, original `value`, `decision` (`retain` or `exclude`), evidence, reviewer and
review date. A retained extreme still undergoes statistical cleaning and any independent
eligibility gate. Stale identities, mismatched values, unmatched keys and conflicting
reviews fail before existing cleaned output is replaced. A new source or changed partition
requires renewed review. No CALPULALPAN station-year exclusion is activated.

The outlier recipe exposes four named quality summaries and review-record tables in
RStudio. Targets cache those objects separately and declare their CSV files under
`data/processed/pollution_quality/`. Summaries count source status, quality reason,
statistical reason and observed usability separately. Review records retain the value and
input identity needed to investigate a hold; join explicit decisions to the registry by
those keys. The existing station catalog and balanced station-year rows remain present.
Usable reporting-station counts may change.

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

The observed-use fields describe observed readings even in an imputed robustness dataset.
OLS predictions retain the existing `OLS_imputed` identification; they do not become agency
validated observations. The existing imputation policy may fill quality-held gaps and may
change the first-finite-reading window. That behavior requires explicit sensitivity review;
the current implementation does not select a different model.

Suspicious-pattern methods, validated 2023 replacements, whole-station or interval
exclusions, and instrument-specific overrides are deferred for coauthor review. See
[remaining work](../planning/remaining-work.md) for acceptance evidence and those decisions.
Portable tests and a bounded source replay do not establish complete scientific reproduction.
