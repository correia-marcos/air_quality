# Data dictionary — processed layer

Every file under `data/interim/census/` and `data/processed/` uses the column names below. The
names are identical across the four cities; they differ between the *individual* (micro) and
*collapsed* (geo) file only where the underlying **concept** differs.

The provider's own column names are not carried alongside as duplicate columns. They are recorded
here, and in each city's `cfg$schema` (`src/city_specific/<city>.R`), which is the mapping the
processing scripts actually apply. Provider columns that survive into the output but are not part
of the canonical schema carry a `raw_` prefix, so the boundary is visible in the file itself.

Contents: [1. Census micro](#1-census-micro) · [2. Census geo](#2-census-geo) ·
[3. Stations & panels](#3-stations-and-panels) · [4. Native → canonical](#4-native--canonical-by-city) ·
[5. Identifiers](#5-identifier-provenance) · [6. Deleted duplicates](#6-duplicate-columns-deleted) ·
[7. Known gaps](#7-known-gaps)

---

## 1. Census micro

One row per person. Path: `data/interim/census/<city_id>/census_*_individual*.parquet`.

| Column | Type | Definition |
|---|---|---|
| `geo_id` | character | Code of the geographic unit of analysis. Character always, so leading zeros survive a CSV round trip. |
| `geo_level` | character | What kind of unit `geo_id` is: `manzana_censal`, `municipio`, `zona_censal`, `comuna`, `area_ponderacao`. |
| `person_weight` | double | Census expansion factor for this person. `1` where the census ships no factor (Bogotá 2018, Santiago 2017, Santiago 2024) — those are full enumerations, not samples. |
| `educ_years` | double | Completed years of schooling, harmonised to a common scale across cities. |
| `income` | double | Monthly income, winsorised. `NA` where the census does not collect it (Bogotá, Santiago). |
| `income_raw` | double | Income as the census shipped it, before winsorising. |
| `adult` | double | 1 if the person is in the adult population used by every aggregation. |
| `women`, `employed`, `indigena`, `hh_head`, `hh_head_women` | double | Indicator variables, 0/1. |
| `no_education`, `high_school_incomplete`, `high_school_complete`, `college_incomplete`, `college_complete`, `graduate_educ` | double | Education-category indicators derived from `educ_years`. |
| `comuna_id` | character | **Santiago 2017 only.** The commune — a *coarser* unit than `geo_id` (which is the zona censal). Named distinctly so it cannot be mistaken for the analysis unit. |
| `raw_*` | varies | Provider-native columns kept for auditability. Each is an input to a derived column above, so the derivation is checkable. |

## 2. Census geo

One row per geographic unit. Path: `data/interim/census/<city_id>/*collapse*.parquet`.

| Column | Type | Definition |
|---|---|---|
| `geo_id`, `geo_level` | | as above |
| `pop_total` | double | Σ `person_weight` over adults in the unit. **This is the population weight every downstream stage uses.** |
| `n_records` | integer | Count of census records behind the unit. Equals `pop_total` wherever `person_weight ≡ 1`. |
| `education_mean` | double | Population-weighted mean `educ_years` among adults: `Σ(educ_years · person_weight) / pop_total`. |
| `income_mean` | double | Population-weighted mean income among adults reporting income. |
| `count_no_ed`, `count_hs_inc`, `count_hs_com`, `count_col_inc`, `count_col_com`, `count_grad`, `count_employed`, `count_women`, … | double | Population-weighted counts: `Σ(indicator · person_weight)`. |
| `share_no_ed_pop`, `share_hs_inc_pop`, `share_hs_com_pop`, `share_col_inc_pop`, `share_col_com_pop`, `share_grad_pop`, `share_employed_pop`, `share_women_pop`, … | double | The matching `count_* / pop_total`. |

`n_records` and `pop_total` are separate columns because they answer different questions: how many
records support this cell, versus how many people it represents. Before this schema they were both
called `n`/`weight` and were identical in three cities but not the fourth.

## 3. Stations and panels

| Column | Type | Definition |
|---|---|---|
| `station_id` | character | Normalised station identifier; the join key between panels, distance matrices and station geometry. |
| `station_name` | character | Human-readable station label as the provider publishes it. |
| `datetime` | timestamp | Observation timestamp. |
| `pm10`, `pm25`, `ozone`, `no2`, `co`, `so2` | double | Hourly concentrations, µg/m³ (CO in mg/m³). |
| `geo_id`, `station_id`, `distance_km` | | Distance-matrix schema, unchanged — it was already canonical. |

## 4. Native → canonical, by city

The authoritative copy of each mapping is `cfg$schema` in the city module. Reproduced here so the
dictionary stands alone.

### Bogotá 2018 — `geo_level = "manzana_censal"`

| Native | Canonical | File | Note |
|---|---|---|---|
| `GEO_ID` | `geo_id` | both | DANE MGN área/manzana censal code, 22 chars |
| `fe` | `person_weight` | micro | injected as `1`; the 2018 CNPV is a full enumeration |
| `escolaridad` | `educ_years` | micro | already years, not a raw code |
| `weight` | `pop_total` | geo | |
| `n` | *dropped* | geo | was identical to `weight` |
| `COD_DANE_ANM`, `MUNI_CODE`, `COD_ENCUESTAS`, `P_SEXO`, `P_EDADR`, `P_NIVEL_ANOSR`, `P_TRABAJO`, `raw_group` | `raw_*` | micro | DANE source variables |

### CDMX 2020 — `geo_level = "municipio"`

| Native | Canonical | File | Note |
|---|---|---|---|
| `CVE_MUN` | `geo_id` | both | INEGI key, 2-digit state + 3-digit municipality; **stored as double today**, becomes zero-padded character |
| `FACTOR` | `person_weight` | micro | INEGI expansion factor; a real sample weight |
| `escolaridad` | `educ_years` | micro | |
| `ingtrmen` | `income_raw` | micro & geo | |
| `weight` | `pop_total` | geo | |
| `n` | *dropped* | geo | identical to `weight` |

### Santiago 2017 — `geo_level = "zona_censal"`

| Native | Canonical | File | Note |
|---|---|---|---|
| `zona_id` | `geo_id` | both | `geocodigo`: CUT(5) + distrito(2) + área(1) + zona(3) |
| `comuna` | `comuna_id` | micro | **coarser than `geo_id`** — joining on it would silently aggregate to commune level |
| `fe` | `person_weight` | micro | injected as `1` |
| `educ_years` | `educ_years` | micro | unchanged |
| `escolaridad` | `raw_escolaridad` | micro | the raw census code — **differs from `educ_years` for 24,967 records**, so both are kept |
| `p07`, `p08`, `p09`, `p14`, `p15`, `p16`, `p17`, `hogar_ref_id` | `raw_*` | micro | |
| `weight` | `pop_total` | geo | |
| `n` | `n_records` | geo | the one city where `n` was a genuine `dplyr::n()` count |

### Santiago 2024 — `geo_level = "comuna"`

| Native | Canonical | File | Note |
|---|---|---|---|
| `comuna` | `geo_id` | micro | the analysis unit at this vintage |
| `CUT` | `geo_id` | geo | Código Único Territorial — the same unit as `comuna`, renamed at `santiago.R:2528`. **Unifying these removes the only case that forced `run_idw_city()` to take two id parameters.** |
| `fe` | `person_weight` | micro | injected as `1` |
| `educ_years` | `educ_years` | micro | |
| `escolaridad` | *dropped* | micro | verified identical to `educ_years` |
| `parentesco`, `sexo`, `sit_fuerza_trabajo`, `p28_autoid_pueblo`, `edad_num` | `raw_*` | micro | |
| `weight` | `pop_total` | geo | |
| `n` | *dropped* | geo | identical to `weight` |

### São Paulo 2010 — `geo_level = "area_ponderacao"`

| Native | Canonical | File | Note |
|---|---|---|---|
| `code_weighting` | `geo_id` | both | IBGE Área de Ponderação, from census `V0011`; 13 digits, kept character |
| `weight` | `person_weight` (micro) / `pop_total` (geo) | | the one city where the same native name plays both roles |
| `years_schooling` | `educ_years` | micro | |
| `V6525` | `income_raw` | micro | |
| `V0010`, `V1004`, `V0601`, `V0606`, `V0633`, `V0634`, `V0648`, `V6036`, `V6400` | `raw_*` | micro | IBGE source variables |
| `n`, `total_adult_pop` | *dropped* | geo | both identical to `weight` |

## 5. Identifier provenance

| City | Native | What it is |
|---|---|---|
| Bogotá | `GEO_ID` | DANE *Marco Geoestadístico Nacional* área/manzana censal code, 22 characters, hierarchical. Rural sectors appear zero-filled in the census (`1100130070600000000000`) and un-filled spatially (`11001300706`) — reconciled by `reconcile_geo_ids()`. |
| CDMX | `CVE_MUN` | INEGI municipality/alcaldía key. Spatial layers ship the padded string `"09002"`; the census ships the number `9002`. Reconciled by `canonical_geo_id(width = 5)`. |
| Santiago | `zona_id` | Chilean *zona censal*: CUT(5) + distrito(2) + área(1) + zona(3). |
| Santiago | `CUT` / `comuna` | *Código Único Territorial*, the 5-digit commune. The first five characters of `zona_id`. |
| São Paulo | `code_weighting` | IBGE *Área de Ponderação*, the smallest unit at which the 2010 census sample is representative. |

## 6. Duplicate columns deleted

Twenty-four columns in the collapsed files held values identical to another column in the same
file. They were verified identical with `all.equal()` on the current data, not inferred from
source, and the canonical schema keeps one name per group:

| File | Identical groups | Deleted |
|---|---|---|
| `bogota_2018` collapsed | `weight == n`; `education_mean == escolaridad == escolaridad_avg`; `share_grad_pop == share_graduate_educ_pop`; `share_employed_pop == share_employed` | 5 |
| `cdmx_extended_2020` collapsed | `weight == n`; `education_mean == escolaridad`; `income_mean == income`; `share_grad_pop == share_grad_educ_pop` | 4 |
| `santiago_2017` collapsed | `n == weight`; `education_mean == avg_escolaridad` | 2 |
| `santiago_2024` collapsed | `weight == n`; `education_mean == educ_years == avg_escolaridad`; `share_grad_pop == share_grad_educ_pop` | 4 |
| `sao_paulo_2010` collapsed | `weight == n == total_adult_pop`; `education_mean == years_schooling == avg_escolaridad`; `income_mean == income`; `share_employed_pop == share_employed`; `share_women_pop == share_female`; `share_black_pop == share_black` | 8 |
| `santiago_2024` micro | `escolaridad == educ_years` | 1 |

A twin column carries no checkable information — there is nothing to verify, only an invariant
nobody enforces. That is different from a `raw_*` column, which differs from its derived
counterpart by a *transformation* a referee can check. Twins are deleted; `raw_*` is kept.

## 7. Known gaps

- **São Paulo `college_incomplete` is structurally zero.** The `years_schooling` recode
  (`sao_paulo.R:1621-1651`) can only produce the values {0, 2, 4, 4.5, 5, 9, 12, 17, 19, 23}, but
  `col_incomplete` is defined as `years_schooling >= 13 & years_schooling <= 16`
  (`sao_paulo.R:1659-1661`) — a range the recode never emits. Respondents with incomplete tertiary
  education (`V0633 == "11" & V0634 == "2"`) are mapped to 12 years and therefore counted as
  *high-school complete*. `count_col_inc` is 0 for all 633 units and `col_incomplete` is 0 for all
  1,216,611 records, while the other three cities populate the category. **Not fixed here** —
  changing it moves published numbers and is a definitional decision, not a naming one.
- **Bogotá 2018 has no `high_school_incomplete` / `college_incomplete` indicators at all**
  (`bogota.R:3502-3505` builds four education dummies, the other cities build six).
- **The 2005 Bogotá vintages** (`bogota_basic_2005`, `bogota_extended_2005`) are validation-track
  inputs whose structure deliberately mirrors the legacy pipeline. They are **not** converted to
  this schema.
- **Income exists only for CDMX and São Paulo.** Bogotá's and Santiago's censuses do not collect it.
