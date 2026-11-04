# Inputs: the data every analysis reads

These recipes turn the preserved downloads into the geography, stations, census and cleaned
hourly pollution that every analysis folder reads. They never download; acquisition is in
[`scripts/download_data/inputs/`](../../download_data/inputs/). The targets equivalents are
in [`config/targets/inputs/`](../../../config/targets/inputs/).

**Granularity follows the sources.** Each city's sources come in their own formats, so each
city has its own processing recipe. Distances and outlier cleaning apply one method to every
city, so one recipe runs them for all four.

## Run order

| Step | Recipe | Reads | Writes |
|---|---|---|---|
| 1–4 | `process_bogota_data.R`, `process_cdmx_data.R`, `process_santiago_data.R`, `process_sao_paulo_data.R`, in any order | Preserved downloads in `data/downloads/<city>/` | Geography and selected stations in `data/interim/geospatial_data/<city>/`, hourly station panels in `data/interim/monitoring_stations/`, census records in `data/interim/census/` |
| 5 | `generate_distance_matrices.R` | The stations and geographic units of steps 1–4 | Station-to-station and unit-to-station distances in `data/processed/distances_matrices/<context>/` |
| 6 | `detect_outliers.R` | The hourly panels of steps 1–4 and the station-to-station distances of step 5 | Cleaned panels in `data/processed/monitoring_stations_outliers/<city>_metro_clean/` and screening summaries in `data/processed/pollution_quality/` |

The geographic contexts are Bogotá's 2018 census blocks and rural sections, Mexico City's
2024 municipalities (with the 2020 census), Santiago's 2017 census zones and São Paulo's 2010
weighting areas.

## How the cities flow

Each city stays a separate chain until the analysis tables stack the four cities. Steps 5
and 6 loop over the cities but never combine them.

1. `download_<city>` → `process_<city>`: the city's own sources and formats.
2. `distances_<city>`: station-to-station distances feed outlier cleaning; unit-to-station
   distances feed IDW exposure, the descriptive tables and the monitoring figures.
3. `outliers_<city>`: the cleaned panels feed every analysis.
4. Each analysis (exposure, descriptives, monitoring, imputation, temporal) computes city by
   city.
5. The analysis tables and figures stack the four cities.
