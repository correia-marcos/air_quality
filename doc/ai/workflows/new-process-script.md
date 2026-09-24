# New process-data script

First acceptance criterion: follow the reader-first requirement in
[architecture](../architecture.md#the-reader-is-a-human-not-just-a-machine) and
[R style](../rules/r-style.md). The reader can run a few lines in RStudio, inspect
named intermediate objects, and locate the applied function without learning targets.
Use three or four meaningful executable sections; do not hide the analysis in one call.


Create `scripts/process_data/short_name.R` that follows this project's conventions exactly.

Before writing, confirm with me (one line each) if unclear:
- What are the **inputs** (which `data/raw` or `data/interim` files) and their format?
- What is the **output** and where does it land (`data/interim/` or `data/processed/`)?
- Is it **city-specific** (needs `registry.R` + a city module) or cross-city?

Then generate the file:

1. Standard header block (`@Goal / @Description / @Summary / @Date / @Author`) with `#'` on the
   `@tag` lines and plain `#` on continuations — copy the exact style from an existing
   `scripts/process_data/*.R`. `@Goal` = "output_description" if given.
2. Use the author-edited Bogotá recipes for layout, as specified in [R style](../rules/r-style.md).
   Follow `scripts/process_data/generate_distance_matrices.R` for separate saving: source subject
   modules and `config/analysis_settings.R`, show paths and reads, then call scientific
   functions with named arguments. Section II computes named results; Section III saves
   them in the same order. No extra inspection block or pack/unpack wrapper. City scripts
   expose the underlying operations; `city_process()` remains a convenience command.
3. Numbered sections (`# I:`, `# II:`, ...) with `# ===` dividers.
4. All paths via `here::here(...)`. Write output as Parquet to the correct data layer.
5. Follow the spacing, call layout and concrete step comments in
   [R style](../rules/r-style.md#spacing-calls-and-comments). Keep function rationale in
   `@Description` / `@details` rather than long in-body comment blocks.

Do **not** add it to the manuscript graph or `run_pipeline.R` yet — show me the script first,
declare its upstream targets and owned files, tell me where in the
pipeline order it belongs, and let me confirm before wiring it in.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
