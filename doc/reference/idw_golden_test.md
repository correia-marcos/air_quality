# The IDW golden-value test, worked by hand

*What `tests/testthat/` proves, on a 3-station toy small enough that every number
can be checked with a pencil. Read this alongside the suite; if the estimator ever
changes, the fixture, the tests, and this page change together.*

Date: August 2026 · Author: Marcos Paulo (initial draft by Claude Code)

---

## 1. The estimator under test

For geographic unit $g$, hour $t$, and one pollutant, the exposure estimate is the
missingness-aware inverse-distance weighted mean over stations $s$:

$$
y_{g,t} \;=\;
\frac{\sum_s w_{gs}\, B_{gs}\, C_{st}\, p_{st}}
     {\sum_s w_{gs}\, B_{gs}\, C_{st}},
\qquad w_{gs} = d_{gs}^{-1}
$$

- $B_{gs} = \mathbf{1}\{0 < d_{gs} \le 3\text{ km}\}$ — the buffer. Pairs with
  $d = 0$ are **excluded and reported** (paper footnote), never silently dropped.
- $C_{st} = \mathbf{1}\{p_{st} \text{ is observed}\}$ — a station with a missing
  reading leaves **both** numerator and denominator for that hour: the weights of
  the surviving stations re-normalize.

Annual quantities use only *defined* geo-hours $H_g$ (hours with $\ge 1$
contributing station):

$$
\bar{y}_g = \frac{1}{|H_g|}\sum_{t \in H_g} y_{g,t},
\qquad
N^{(k)}_g = \sum_{t \in H_g} \mathbf{1}\{y_{g,t} \ge \theta_k\},
\qquad
\theta^{\text{PM10}} = (150,\, 100,\, 75,\, 50)
$$

Equal-population grouping sorts by (value, geo_id, row order) — fully deterministic
ties — and assigns group $k$ when $(k-1)/n < F_i \le k/n$, with $F_i$ the
cumulative population-weight share.

## 2. The problem set

**Distances** (km) and implied weights $w = 1/d$, buffer $B$ at 3 km:

| pair  | $d$ | $B$ | $w$  | note                         |
|-------|-----|-----|------|------------------------------|
| g1–s1 | 1   | 1   | 1    |                              |
| g1–s2 | 2   | 1   | 1/2  |                              |
| g1–s3 | 4   | 0   | —    | outside buffer               |
| g2–s1 | 1   | 1   | 1    |                              |
| g2–s2 | 1   | 1   | 1    |                              |
| g2–s3 | 2   | 1   | 1/2  |                              |
| g3–s1 | 0   | 0   | —    | $d = 0$: excluded, reported  |

**PM10 readings** ($C = 0$ for NA):

| hour | s1 | s2 | s3  |
|------|----|----|-----|
| h0   | 60 | 120| 300 |
| h1   | 40 | NA | 60  |
| h2   | NA | NA | NA  |

**Census** (for geo-level grouping): g1 (n = 100), g2 (n = 200), g3 (n = 50).

**Questions.** (a) Hourly IDW for every geo-hour. (b) Annual means and hour
counts. (c) WHO interim-target counts. (d) What happens to g3 and to h2?
(e) Assign 2 equal-population groups from values (1, 2, 3) with weights (2, 1, 1).

## 3. Solutions

**(a) Hourly.** g1 sees {s1, s2}; g2 sees {s1, s2, s3}:

$$
y_{g1,h0} = \frac{60\cdot 1 + 120\cdot \tfrac12}{1 + \tfrac12}
          = \frac{120}{1.5} = 80,
\qquad
y_{g2,h0} = \frac{60 + 120 + 300\cdot \tfrac12}{1 + 1 + \tfrac12}
          = \frac{330}{2.5} = 132
$$

At h1, s2 is missing, so the denominators shrink — this is the re-normalization:

$$
y_{g1,h1} = \frac{40}{1} = 40,
\qquad
y_{g2,h1} = \frac{40 + 60\cdot \tfrac12}{1 + \tfrac12}
          = \frac{70}{1.5} \approx 46.67
$$

**(b) Annual.** h2 has no observed station, so $|H_{g1}| = |H_{g2}| = 2$:

$$
\bar{y}_{g1} = \frac{80 + 40}{2} = 60,
\qquad
\bar{y}_{g2} = \frac{132 + 46.67}{2} \approx 89.33
$$

**(c) WHO counts** (150 / 100 / 75 / 50, counted with $\ge$):

| geo | hours   | it1 | it2 | it3 | it4 |
|-----|---------|-----|-----|-----|-----|
| g1  | 80, 40  | 0   | 0   | 1   | 1   |
| g2  | 132, 47 | 0   | 1   | 1   | 1   |

**(d) Edge cases.** g3's only pair has $d = 0$ → excluded (with a diagnostic
message) → g3 is **absent** from the output. h2 is all-missing → **no geo-hour
row**, so it inflates neither the mean nor the hour count.

**(e) Grouping.** Sorted cumulative shares: $F = (2/4,\ 3/4,\ 4/4)$. The 0.5 edge
satisfies $F_1 \le 0.5$, so the cut falls after the first row: groups **(1, 2,
2)**. Without weights the split would be (1, 1, 2) — the expansion weight moves
the boundary, which is why the quintiles are population-equal, not count-equal.
The suite also checks: exact fifths for unweighted values 1–10; a tie block split
by geo_id order (six units tied at the cut → {a,b,c,d} vs {e,f,g,h}), invariant
to input row order (`set.seed(20260808)`); NA value or weight → NA group, with
the weight totals computed on valid rows only.

## 4. The run

`Rscript tests/testthat.R` (first run needs network once: DuckDB fetches its ICU
extension). Output from the run of 2026-08-10:

```text
assign-socio-group: .......
idw-exposure-golden:
[toy] Starting DuckDB engine ...
[toy] Loading and normalizing distances ...
[toy] Distance table: 2 geo unit(s), 3 station(s).
[toy] Station overlap: 3 of 3 pollution station(s) overlap distance matrix.
[toy] Processing 1 year(s).
[toy] Year 2023 ...
[toy] Census match: 2 of 2 exposure geo unit(s) matched (0 unmatched).
...............

══ DONE ═══════════════════════════════════════════════════════════════════
```

How to read it: the summary reporter prints **one dot per expectation** —
7 for the five grouping tests, 15 for the integration test (14 assertions plus
the `expect_message` itself). Nothing after `DONE` means zero failures and zero
skips. Three things worth noticing:

- The breadcrumbs are soft checks in themselves: "2 geo unit(s)" because g3 was
  already filtered out; "3 of 3" because even out-of-buffer s3 stays in the
  matrix (it serves g2).
- The $d = 0$ diagnostic does **not** appear in the console: `expect_message(...,
  "excluded")` consumes the matching message — that *is* the assertion. Its
  absence from the log plus a green dot proves the footnote behavior fired.
- No results file survives: outputs are written to R's per-session tempdir and
  verified there (the last assertion re-reads the Parquet checkpoint from disk),
  then deleted on exit. Tests leave no trace in `data/`.

To inspect the toy artefacts by hand, source `tests/testthat/helper-fixtures.R`,
call `make_toy_fixture(<dir>)`, and run `aggregate_idw_exposure()` with
`out_dir = <dir>` — the same arguments as in
`tests/testthat/test-idw-exposure-golden.R`.

## 5. What has been verified, and how

- 2026-08-08 — golden numbers re-derived independently by simulating the
  estimator's SQL (CTEs `h` → `hr_geo` → annual sums) in `data.table`; the five
  grouping scenarios run against the real `assign_socio_group()`. All matched.
- 2026-08-10 — full suite run locally (macOS, R + arrow/duckdb/testthat):
  22 expectations, 0 failures.
