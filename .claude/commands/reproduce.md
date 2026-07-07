---
description: Guide a first-time user (editor / referee / student) through reproducing the paper
---

# Reproduce the analysis

Help me (or a referee) reproduce results from a clean clone. Be a patient guide: explain each
step in one or two sentences, and pause where a human must act.

Walk through, checking state as you go:

1. **Prerequisites** — Docker installed? (`docker --version`). If they'd rather run natively:
   R 4.5.3 + the system libs listed in the `Dockerfile` `base` stage.
2. **Credentials** — copy `.env.example` to `.env`. Explain that Earthdata / Stadia keys are only
   needed to *re-download* raw data; the provided processed data runs without them.
3. **Launch** — `docker compose up`, then open RStudio at `http://localhost:8787`
   (user/password from `.env`). Confirm the project loaded (`Coding.Rproj`, working dir
   `/air_monitoring`).
4. **Data** — is `data/raw/` populated (from Zenodo) or do they need the download scripts? The
   download steps in `run_pipeline.R` are commented out by default because they're large.
5. **Run** — `source(here::here("scripts","run_pipeline.R"))` for the full pipeline, or point them
   to a single `scripts/` file for one stage. Encourage them to inspect `data/interim/` and
   `data/processed/` Parquet files in the RStudio environment as they go.
6. **Outputs** — where figures and tables land (`results/figures/`, `results/tables/`).

If anything fails, help diagnose (missing package → `renv::restore()`; path issue → check
`here::here()` and working dir) rather than papering over it. The success test: they can see the
data change at each step and understand it.
