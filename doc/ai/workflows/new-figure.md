# New figure / table script

Create `scripts/tables_images/short_name.R` producing a publication artefact.

Constraints for this stage (see `doc/ai/rules/data-and-paths.md`):
- **Read only from `data/processed/`** (or `data/interim/`). No raw data, no cleaning here.
- Figures → `results/figures/<subfolder>/`; tables → flat `results/tables/`
  (LaTeX via `kableExtra`, or CSV). Match existing naming: `<city>_<year>_<what>`.
- Fonts: the project ships Tex Gyre Pagella in `fonts/` and uses `showtext`. Reuse the existing
  plotting helpers in `src/general_utilities/config_utils_plot_tables.R` rather than restyling.
- Style per `doc/ai/rules/r-style.md`: line length ≤ 90, and rationale in `@Description` /
  `@Details` rather than in long in-body comment blocks.

Steps:
1. Ask me which processed dataset feeds it and what the artefact should show, if not obvious.
2. Standard header block; setup sources `config_utils_plot_tables.R`.
3. Build the plot/table, then save to the right `results/` subfolder with a clear filename.
4. Keep it flat and readable — this is single-use plotting code, not a framework.

Show me the output path and a description of what it renders before finalizing.

Current output contract supersedes old destinations: figures use the seven topic
folders under results/figures/; tables are flat under results/tables/. Never write
results/paper/. The tracked manifest exports only selected manuscript artifacts.


Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
