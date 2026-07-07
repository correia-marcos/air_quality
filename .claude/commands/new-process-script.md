---
description: Scaffold a new scripts/process_data/ script with the standard header and wiring
argument-hint: '[short_name] [what it produces]'
---

# New process-data script

Create `scripts/process_data/$1.R` that follows this project's conventions exactly.

Before writing, confirm with me (one line each) if unclear:
- What are the **inputs** (which `data/raw` or `data/interim` files) and their format?
- What is the **output** and where does it land (`data/interim/` or `data/processed/`)?
- Is it **city-specific** (needs `registry.R` + a city module) or cross-city?

Then generate the file:

1. Standard header block (`@Goal / @Description / @Summary / @Date / @Author`) — copy the exact
   style from an existing `scripts/process_data/*.R`. `@Goal` = "$2" if given.
2. Setup section sourcing the utilities it needs:
   `source(here::here("src","general_utilities","config_utils_process_data.R"))`
   plus `registry.R` and the city module if city-specific.
3. Numbered sections (`# I:`, `# II:`, ...) with `# ===` dividers.
4. All paths via `here::here(...)`. Write output as Parquet to the correct data layer.
5. Respect line length ≤ 90 and comment ~every 4 lines.

Do **not** add it to `run_pipeline.R` yet — show me the script first, tell me where in the
pipeline order it belongs, and let me confirm before wiring it in.
