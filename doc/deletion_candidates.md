# Deletion candidates

Functions that moved from `scripts/` into `src/` during the Step 4 refactor but look removable.
**None of these has been deleted.** Each row says what it does, why it looks removable, and what
breaks if it goes, so the call is yours. Deleting any of them is a one-line change now that they
all live in `src/`.

Written August 2026. Re-check before acting — the "used by" column is a snapshot.

---

## Dead code

| Function | Now in | Why removable | What breaks |
|---|---|---|---|
| `prepare_station_scatter_data` | `plot/station_monitoring.R` | **No caller anywhere.** Verified with a repo-wide grep: the only occurrence is its own definition and doc block. | Nothing. It has never run. |

## Thin wrappers over functions that already error usefully

| Function | Now in | Why removable | What breaks |
|---|---|---|---|
| `safe_read_parquet` | `plot/station_monitoring.R` | Adds a `file.exists()` check in front of `arrow::read_parquet()`, which already errors with the path. The rules discourage defensive wrapping. | 8 call sites in `plot_station_monitoring_figures.R` would call `arrow::read_parquet()` directly; the error message loses the "File not found:" prefix but still names the file. |
| ~~`safe_read_csv`~~ | — | **Deleted in Step 5**, as predicted: its only inputs were the four collapsed census CSVs, and those are now Parquet. Its callers moved to `safe_read_parquet`. | Already gone; nothing referenced it. |

## Column-name fallback lists

| Function | Now in | Why removable | What breaks |
|---|---|---|---|
| `find_col` | `base_utils.R` | Takes a list of acceptable column names and returns the first present — a guardrail standing in for a test. The city spec table already names `geo_id_col` explicitly; the other four columns could be named the same way. | 6 call sites in `station_education_quintile()`. Removing it means adding 4 columns to the city spec table in `compute_missing_proportions.R`. Stricter, and a schema change would then fail at the spec rather than silently picking a different column. |

## Formatting one-liners

| Function | Now in | Why removable | What breaks |
|---|---|---|---|
| `format_int_latex` | `base_utils.R` | One `formatC`-style call: `format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)`. | 3 call sites in `generate_census_summary_table.R`. Inlining is trivial; keeping it means the `scientific = FALSE` decision is made once. |

## Not duplicates — findings that contradict the original plan

Two functions were listed for deletion on the assumption they duplicated something in `src/`.
**Both assumptions are wrong**, so both were moved rather than deleted:

| Function | Now in | Finding |
|---|---|---|
| `.escape_latex` → **not moved, replaced** | — | It escaped **3** characters (`&`, `%`, `_`); the canonical `latex_escape()` escapes **9** (adds `\`, `$`, `#`, `{`, `}`, `~`, `^`). Switching is a behaviour *improvement*, not a no-op. Safe here because the only values escaped are the census level labels — "Census tract", "Municipality", "Weighting area" — none of which contain any special character. Verify if that column ever gains a new value. |
| `write_station_count_latex` | `plot/latex_tables.R` | **Not** superseded by `table_stations_by_pollutant()`. That one renders city × year × pollutant from a coverage summary; this one renders the paper's compact three-column city / PM10 / PM2.5 table for a single year. Different outputs. **It also had a real bug**, fixed during the move: two header rows ended in a single `\` where LaTeX needs `\\`, so the rendered `.tex` had rows that did not terminate. Compare the regenerated table against `results/tables/station_counts/stations_by_pollutant_2023.tex` to see the fix. |

## Path and format closures — already gone

These were 2-5 line closures over script-level paths. They were replaced by named `src/`
functions with explicit arguments rather than moved verbatim, so there is nothing left to decide:

`read_idw` / `dist_pq` → `read_idw_artifact()`, `idw_artifact_path()` ·
`read_artifact` → four direct `arrow::read_parquet()` calls ·
`save_both` → `save_table_parquet_csv()` · `meta_first` → `set_meta_cols_first()` ·
`stack_runs` → `stack_city_tables()` · `save_pdf` → `save_plot_pdf()` ·
`group_axis_label` → `exposure_group_axis_label()` · `.read_table` → deleted (your call;
every census input is Parquet as of Step 5) · `normalize_station_id` → deleted (5th
byte-identical copy of `normalize_station()`).

## Unused non-function items

Not functions, listed here so they are not forgotten:

- `has_ggrepel` in `plot_station_monitoring_figures.R` — computed a package availability flag
  and never used it. **Removed** during the move; noted here for the record.
- `results/figures/maps/santiago_grid_merra.pdf` and `sao_paulo_grid_merra.pdf` — present in
  `results/` with **no producing script anywhere in the repo**. Either a script was deleted or
  these are stale outputs from an earlier run. Not touched.
