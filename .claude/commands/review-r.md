---
description: Review R changes against this project's conventions and reproducibility rules
argument-hint: '[path or "staged"]'
---

# Review R

Review $ARGUMENTS (a path, or the staged diff if "staged"/empty) against project standards. This
is a read-and-report pass — propose fixes, don't apply them until I approve.

Check, and cite line numbers:

- **Style:** line length ≤ 90; header block present and correctly formatted; comments ~every 4
  lines saying *why* / what happens to the data; numbered `# ===` sections.
- **Paths:** every path via `here::here()`; no absolute paths, no `setwd()`.
- **Packages:** loaded via the stage `config_utils_*.R`, not stray `library()`; anything new is in
  `DESCRIPTION` + `renv.lock`.
- **Data discipline:** reads/writes hit the correct layer; nothing writes `data/raw` or
  `data/_legacy`; `tables_images/` reads only from `data/processed`/`interim`; intermediates are
  Parquet.
- **Reproducibility:** `set.seed()` where there's randomness; `src/` has no top-level side-effects.
- **Reader-first:** could a referee follow the data through this? Flag deep pipe chains or hidden
  intermediate shapes.
- **Anti-bloat:** unrequested abstraction/flexibility, error handling for impossible cases,
  200-lines-that-should-be-50. Call these out.

End with a short TL;DR table: issue | location | severity | suggested fix. For deeper audits,
hand off to the `r-reproducibility-reviewer` subagent.
