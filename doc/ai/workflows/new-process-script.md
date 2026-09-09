# New process-data script

Create `scripts/process_data/short_name.R` that follows this project's conventions exactly.

Before writing, confirm with me (one line each) if unclear:
- What are the **inputs** (which `data/raw` or `data/interim` files) and their format?
- What is the **output** and where does it land (`data/interim/` or `data/processed/`)?
- Is it **city-specific** (needs `registry.R` + a city module) or cross-city?

Then generate the file:

1. Standard header block (`@Goal / @Description / @Summary / @Date / @Author`) with `#'` on the
   `@tag` lines and plain `#` on continuations — copy the exact style from an existing
   `scripts/process_data/*.R`. `@Goal` = "output_description" if given.
2. Setup section sourcing the utilities it needs:
   `source(here::here("src","general_utilities","config_utils_process_data.R"))`
   plus `registry.R` and the city module if city-specific.
3. Numbered sections (`# I:`, `# II:`, ...) with `# ===` dividers.
4. All paths via `here::here(...)`. Write output as Parquet to the correct data layer.
5. Respect `doc/ai/rules/r-style.md`: line length ≤ 90, and rationale in `@Description` /
   `@Details` rather than in long in-body comment blocks.

Do **not** add it to `run_pipeline.R` yet — show me the script first, tell me where in the
pipeline order it belongs, and let me confirm before wiring it in.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
