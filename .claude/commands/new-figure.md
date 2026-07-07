---
description: Scaffold a new figure or table script in scripts/tables_images/
argument-hint: '[figure|table] [short_name]'
---

# New figure / table script

Create `scripts/tables_images/$2.R` producing a publication artefact.

Constraints for this stage (see `.claude/rules/data-and-paths.md`):
- **Read only from `data/processed/`** (or `data/interim/`). No raw data, no cleaning here.
- Figures → `results/figures/<subfolder>/`; tables → `results/tables/<subfolder>/`
  (LaTeX via `kableExtra`, or CSV). Match existing naming: `<city>_<year>_<what>`.
- Fonts: the project ships Tex Gyre Pagella in `fonts/` and uses `showtext`. Reuse the existing
  plotting helpers in `src/general_utilities/config_utils_plot_tables.R` rather than restyling.

Steps:
1. Ask me which processed dataset feeds it and what the artefact should show, if not obvious.
2. Standard header block; setup sources `config_utils_plot_tables.R`.
3. Build the plot/table, then save to the right `results/` subfolder with a clear filename.
4. Keep it flat and readable — this is single-use plotting code, not a framework.

Show me the output path and a description of what it renders before finalizing.
