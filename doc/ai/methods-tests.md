# Methods to tests

This is a coverage map, not a claim of exhaustive correctness. All new checks refer to the structural-migration working tree based on 720f6412761bbfdcdaba008a8909bc2ce71b4889. Existing tests keep their original numerical explanations in tests/testthat/ and doc/idw_golden_test.md.

| Procedure / code in src/ | Independent evidence | Remaining limitation |
|---|---|---|
| process/idw_exposure.R | test-idw-exposure-golden.R: hand-calculated missingness renormalization, zero-distance exclusion and threshold counts. test-numerical-contracts.R: exact 3 km boundary and reversed input ordering. | Geographic coverage and all production years require full-data checks. |
| process/exposure_regressions.R | test-exposure-regressions.R: weighted group means, normalization, base group and cluster-count guard. test-numerical-contracts.R: explicit X'WX inverse and sums of cluster score outer products, HC1 corrections, t(G-1) intervals. | Does not establish validity of the clustering assumption or causal identification. |
| city_specific/cdmx.R census harmonization | test-numerical-contracts.R: adults of weights 1,3,100 with education 0,12,missing; reporting denominator=4, adult population=104, weighted mean=9. A weight-1000 child does not enter adult denominator. | Other cities and Mexican income winsorization need separate oracles. |
| process/outliers.R | test-integration.R: isolated peak fails temporal check with absent neighbor; missing adjacent observations trigger distinct missing-benchmark reason. | A co-moving neighbor rescues a peak using independently bounded differences; second-neighbor edge cases remain a coverage gap. |
| process/imputation.R | test-numerical-contracts.R: preserves observed values exactly, fills selected gaps, emits only requested year and rejects absent years. | Prediction plausibility and inferential effects remain scientific questions. |
| process/distances.R and end-to-end contract | test-integration.R: tiny polygons/stations, distance table, constant-signal IDW=60, zero group gap, rendered PDF/table and export. | Synthetic preparation does not validate every city's source parser. |
| process/geo_ids.R | Existing canonical-name/schema tests and retained Parquet provenance. | Full-data schemas require local inputs. |
| Export and harness adapters | test-export.R, test-test-runner.R, tests/harness/test_guard.py | Hooks supplement stronger OS boundaries; client trust must be verified. |

New structural-parity comparisons use abs(actual-expected) <= 1e-10 + 1e-8*abs(expected). Existing stricter numerical tolerances are retained. Identifiers, schemas, counts, integer values and missingness masks compare exactly. Any domain-specific exception needs a written justification before acceptance; do not enlarge a tolerance after seeing failure. PDF checksums establish copying integrity, not scientific equivalence. Compare plot data and inspect rendering separately.
