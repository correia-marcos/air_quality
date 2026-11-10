# Scripts

Each script here is a recipe: it reads declared files, calls functions from
[`src/`](../src/), and saves named results. To run them one at a time in the order that
produces the paper's results, follow the
[run-order guide](../doc/guides/run-order.md).

## Three stages

| Stage | What it does |
|---|---|
| [`download_data/`](download_data/) | Acquires the source files from their providers into `data/downloads/`. Needs network access and, for some providers, credentials. Never run by processing |
| [`process_data/`](process_data/) | Turns the downloads into analysis data in `data/interim/` and `data/processed/`, and estimates the results |
| [`tables_images/`](tables_images/) | Reads the prepared data in `data/interim/` and `data/processed/` and writes the figures and tables in `results/` |

## The same analysis names in every stage

Inside each stage, folders are named after the analysis they serve. The same names organise
the targets modules in [`config/targets/`](../config/targets/) and the folders of
`results/figures/` and `results/tables/`, so one name leads from the processing recipe to
its figures and tables and to its targets:

| Analysis | Question |
|---|---|
| `inputs` | Geography, stations, census and cleaned hourly pollution, read by every analysis ([guide](process_data/inputs/README.md)) |
| `descriptives` | Monitoring coverage, data availability and the census populations |
| `monitoring` | Where stations are, and what they record, relative to education and income |
| `exposure` | Residents' exposure by IDW, and its gaps between socioeconomic groups |
| `imputation` | The exposure gaps with missing station-hours filled in |
| `temporal` | Hourly PM2.5 through the day, and pollution episodes |
| `specification_sensitivity` | Optional: how the results change with geographic support, IDW choices and education groups ([guide](process_data/specification_sensitivity/README.md)) |

A stage only has the folders it needs: `inputs` has no figures, and `descriptives` produces
tables only.

## Main analyses, robustness checks and work beyond the paper

- Main analyses sit directly under each stage.
- [`robustness/`](process_data/robustness/README.md) repeats the exposure analysis with one
  choice changed: a 5 km buffer, and individual-level regressions. These run with the paper.
- [`not_in_paper/`](process_data/not_in_paper/README.md) holds work the paper does not
  report: Santiago with the 2024 census, satellite comparisons, supporting diagnostics and
  the legacy validation.

[`run_pipeline.R`](run_pipeline.R) runs every manuscript recipe in one session, and
[`run_targets.R`](run_targets.R) runs the same steps through the targets graph
([how to run](../doc/HOW_TO_RUN.md#manuscript-pipeline)).
