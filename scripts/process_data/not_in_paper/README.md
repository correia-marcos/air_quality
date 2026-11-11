# Not in the paper

The analyses here answer questions the paper does not report. None of them runs with the
manuscript (`make all` or `Rscript scripts/run_targets.R all`), and none is a prerequisite of
it. Each folder has a twin under
[`scripts/tables_images/not_in_paper/`](../../tables_images/not_in_paper/) for its figures
and tables. Every recipe has a row in the
[workflow inventory](../../../doc/planning/remaining-work.md) with its inputs and status.

**`santiago_2024/`** repeats Santiago's exposure analysis with the 2024 census, grouping
residents by commune. The paper uses the 2017 census zones because the 2024 microdata
identifies residence only by commune (39 communes against 1,654 zones in the processed 2017
census), so the 2024 run
trades spatial resolution for recency. Run it with `make santiago-2024` after the main
Santiago products exist. Its tables go to `data/processed/idw_regressions/santiago_2024/`.
It uses only the 3 and 5 km buffers (`santiago_2024_buffers_km`). Unlike the main cities,
it has no 20 km IDW and no individual-level regression: few of its results are reported,
and nothing reads those two.

**`satellite/`** extracts MERRA-2 aerosol estimates for the four study areas and compares
them with the ground stations and with country references. It needs NASA inputs that the
manuscript does not use; run it with `make merra2` after acquiring them. Extraction choices
are in `config/merra2_settings.R`.

**`supporting/`** holds one-off inputs and diagnostics: the alternative 2024 Metro Santiago
boundary, the auxiliary files sent to INEGI's remote lab, and the share of reported readings
removed by cleaning, by education quintile (a supplement to the manuscript's availability
table). Its figures are context maps and station inspections. Each recipe runs on its own
with `Rscript`.

**`legacy_validation/`** compares this pipeline with the coauthor's original code, which
produced the published numbers, and attributes every change in a result to one updated input
(the Step 0–4 framework in [validation rules](../../../doc/ai/rules/validation.md)). It reads
the read-only originals in `data/_legacy/`. Run it with `make validate`;
`prepare_bogota_2005_inputs.R` builds the 2005 Bogotá inputs it compares. Its outputs
(comparison tables, maps and the HTML report) go to the ignored `data/validation/<city>/`.
