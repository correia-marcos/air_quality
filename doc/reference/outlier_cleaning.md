# Outlier cleaning, worked by hand

*How `detect_outliers.R` decides which hourly PM readings enter the analysis, on two
stations and 100 hours, small enough to check every number with a pencil. If the
procedure changes, this page and its test change together.*

Date: October 2026 · Author: Marcos Paulo

---

## 1. The question

A monitoring station sometimes reports an hour that is not plausible: a broken sensor, a
unit error, a stuck value. A reading can also be very high and real, for example when a
fire raises particulate levels across the city. Cleaning has to remove the first kind and
keep the second. It does this in two stages, applied to each city's standardized hourly
panel (one row per station-hour), using `detect_pollution_outliers()` in
[`src/general_utilities/process/outliers.R`](../../src/general_utilities/process/outliers.R).
[`detect_outliers.R`](../../scripts/process_data/inputs/detect_outliers.R) applies it to each city.

**Stage 1: the screen.** `screen_pollution_quality()` withholds every reading that cannot be
a concentration under the project's bounds, whatever its neighbours say:

| `screen_reason` | Reading | Is Kept? |
|---|---|---|
| 0 | Inside acceptable bounds[^1]    | ✓ |
| 1 | Above bound                     | ✗ |
| 2 | Negative value                  | ✗ |
| 3 | Missing, or not a finite number | ✗ | 

[^1]: Between 0 and the bound (2,000 µg/m³ for PM2.5, 6,000 for PM10), equality included.

Readings with codes 1–3 become missing in the analytical column, and their original value is
kept in `<pollutant>_original`. The bounds are `pollution_upper_bounds` in
[`config/analysis_settings.R`](../../config/analysis_settings.R); the
[screening note](particulate_quality.md) explains why they are 2,000 and 6,000.

**Stage 2: the statistical procedure.** For each station $s$, month and pollutant, let
$x_{s,t}$ be the screened reading at hour $t$. The procedure tests only the readings that
are unusually high for that station and month, then asks whether the station's own previous
and next hours, or its nearest neighbour, confirm them.

1. **Gate.** Test $x_{s,t}$ only if it is strictly above the station-month 99th percentile,
   $q_{0.99}$ (R's default `quantile()`, type 7). All other readings are retained (code 0).
2. **Temporal check.** The benchmark for comparison is the mean of the previous
   and next hours, $b_{s,t} = (x_{s,t-1} + x_{s,t+1})/2$, or the one of them that exists.
   The statistics comes from the station-month first differences
   $\Delta_{s,u} = x_{s,u} - x_{s,u-1}$. The reading passes when
   $$\bar\Delta - 2\,\mathrm{sd}(\Delta) \;<\; x_{s,t} - b_{s,t} \;<\; \bar\Delta + 2\,\mathrm{sd}(\Delta).$$
   If neither adjacent hour exists, there is no benchmark.
4. **Spatial check**, for a reading that failed the temporal check or had no benchmark. Take
   the nearest station $n$ that reported this pollutant during the year, and the difference
   at the same hour, $e_{s,t} = x_{s,t} - x_{n,t}$. Over the station-month, the reading
   passes when
   $$\bar e - 2\,\mathrm{sd}(e) \;<\; e_{s,t} \;<\; \bar e + 2\,\mathrm{sd}(e).$$

   If this comparison is not possible (the nearest station has no reading at that hour, or
   too few shared hours in the month), the second-nearest station is used instead.
5. **Decision.** A reading that passes either check is retained. Otherwise it is removed
   (the analytical value becomes missing) with a reason:

| `outlier_reason` | Temporal check | Spatial check | Is kept? |
|---|---|---| --- |
| NA | not assessed[^2] |                     | ✗ | 
| 0  | retained[^3]     |                     | ✓ |
| 1  | no benchmark[^4] | Unable to check[^5] | ✗ |
| 2  | failed[^6]       | Unable to check     | ✗ |
| 3  | failed           | failed              | ✗ |
| 4  | no benchmark     | failed              | ✗ |

[^2]: Screened out in stage 1.
[^3]: Not above $q_{0.99}$, or confirmed by one of the checks.
[^4]: Continued to Spatial check.
[^5]: Not possible with either neighbour.
[^6]: Outside acceptable range.

The panel is balanced first: every station has a row for every hour of the year, so a
missing hour is a row with code 3. One hour from each adjacent year is read so the first
and last hours of the year have a previous and next hour. A month with fewer than two
differences has no standard deviation, so its temporal check cannot pass.

**Why the screen comes first.** In CDMX, the Calpulalpan station reported 79,999 µg/m³ of
PM2.5 in 2023. Its station-month $q_{0.99}$ was 80,532.76, higher than the reading itself, so
the gate never tested it. Screening first keeps such values out of the
percentiles, the differences and the neighbour comparisons.

## 2. The problem set

Two stations, A and B, 1 km apart, report PM2.5 for the first 100 hours of January 2023.
Station A reads 20 µg/m³ every hour except hour 50, when it reads 300. Each station is the
other's only neighbour. Two versions of B:

- **Steady:** B reads 20 every hour, including hour 50.
- **Tracking:** B follows A, 5 µg/m³ above or below it in alternate hours, including hour 50,
  when it reads 305. Hours 49 and 51 at B read 15.

**Questions.**

- (a) Is A's hour 50 above the gate?
- (b) Does it pass the temporal check?
- (c) What does the spatial check decide in each version?
- (d) Separately, which screening codes do the readings 20, 2,000, 2,500, −3 and a
  missing value receive?

## 3. Solutions

**(a) Gate.** A's month has $n = 100$ readings. Sorted, the first 99 are 20 and the last,
$x_{(100)}$, is 300. R's default quantile (type 7) places $q_p$ at position
$h = 1 + (n - 1)p$ among the sorted readings and interpolates between the two readings
around it:

$$h = 1 + 99 \times 0.99 = 99.01, \qquad
q_{0.99} = x_{(99)} + 0.01\,\bigl(x_{(100)} - x_{(99)}\bigr) = 20 + 0.01 \times 280 = 22.8.$$

The weight 0.01 is the fractional part of $h$. In the notation of `?quantile`, $m = 1 - p$,
so $np + m = h$, $j = \lfloor h \rfloor = 99$ and $g = h - j = 0.01$; $g$ is zero only when
$h$ is a whole number. Hour 50 (300) is above 22.8 and is tested. Every other hour (20) is
not, and is retained.

**(b) Temporal check.** The benchmark is $(20 + 20)/2 = 20$, so $x - b = 280$. Of A's 99
first differences, two are nonzero (+280 into hour 50, −280 out of it). Their mean is 0 and

$$\mathrm{sd}(\Delta) = \sqrt{\frac{280^2 + 280^2}{98}} = \sqrt{1600} = 40,$$

so the band is $(-80, 80)$. 280 lies outside it: the temporal check fails. A single jump
this large is not normal hour-to-hour variation for this station.

**(c) Spatial check.**

- **Steady B.** The differences A − B are 0 in 99 hours and 280 at hour 50. Their mean is
  2.8 and

  $$\mathrm{sd}(e) = \sqrt{\frac{99 \times 2.8^2 + (280 - 2.8)^2}{99}} = \sqrt{784} = 28,$$

  so the band is $2.8 \pm 56 = (-53.2, 58.8)$. 280 is outside. Both checks failed, so hour
  50 is removed with **reason 3**. The neighbour saw nothing unusual.
- **Tracking B.** A − B alternates −5 and +5: mean 0, standard deviation
  $\sqrt{100 \times 25 / 99} \approx 5.03$, band about $(-10.05, 10.05)$. At hour 50,
  A − B = 300 − 305 = −5, inside the band. Hour 50 is **retained** (reason 0). The
  neighbour saw the same peak, so it is treated as a real event. B's own hour 50 (305) also
  fails its temporal check and is confirmed by A in the same way.

**(d) Screening.** 20 → 0; 2,000 → 0 (equality passes); 2,500 → 1; −3 → 2; missing → 3. Only
the first two enter stage 2.

## 4. In the real data

`detect_outliers.R` runs this procedure for the four cities with the settings declared in
`config/analysis_settings.R`:

| Setting | Value | Meaning |
|---|---|---|
| `pollution_upper_bounds` | 2,000 / 6,000 | The screen's bounds for PM2.5 and PM10, in µg/m³ |
| `outlier_quantile` | 0.99 | The gate: only readings above this station-month quantile are tested |
| `outlier_sd_band` | 2 | Half-width of the temporal and spatial bands, in standard deviations |
| `outlier_missing_temporal` | `"continue"` | A reading without a benchmark still gets the spatial check |
| `outlier_missing_neighbor` | `"second"` | The second-nearest station is used when the nearest cannot be |
| `outlier_neighbor_eligibility` | `"with_data"` | A neighbour must report the pollutant during the year |

The recipe writes the cleaned panels to
`data/processed/monitoring_stations_outliers/<city>_metro_clean/`. Each pollutant has five
columns: the original value, the source validation, `screen_reason`, `outlier_reason` and the
final value. The recipe also writes to `data/processed/pollution_quality/`:

- `<city>_quality_summary.csv`: hours by station, year, pollutant, source validation and the
  two reasons, from `summarize_pollution_quality()`;
- `<city>_review_records.csv`: every reading withheld by the screen for being above the bound
  or negative, from `collect_pollution_screening_records()`.

The [data dictionary](data_dictionary.md#9-observed-particulate-quality-fields) defines
every column, including source validation and the `_audit/` tables kept beside each cleaned
dataset.

## 5. The test

[`tests/testthat/test-outlier-worked-example.R`](../../tests/testthat/test-outlier-worked-example.R)
runs `detect_pollution_outliers()` on this problem set with the manuscript settings and
checks every answer above: the gate value, the temporal and spatial bands, reason 3 for A's
peak against steady B, retention against tracking B, and the five screening codes.
