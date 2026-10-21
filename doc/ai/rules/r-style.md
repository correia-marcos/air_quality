# R style

This file details the [reader-first requirement](../../../AGENTS.md#the-reader-comes-first),
the highest structural priority. Scientific definitions and protected inputs remain unchanged.

## Executable recipes

Default sections: I. Import data (including sources, settings and all input, extraction
and output paths), II. Process data, III. Save outputs. Save named results in the same
order as their computations. Section III contains saving calls, not previews, file
inventories or reads of results just written.
Do not add an "Inspect before saving" block. Use a fourth section when preparing data and
estimating or rendering are distinct. Operational launchers use accurate headings such
as Settings, Checks, Execute and Report. Quarto reports retain their narrative structure.
The header summary must match the executable sections. Do not add empty sections.

Assign useful data, estimates and plots to named objects. Keep results from repeated
city/pollutant runs in named lists. Intentional logging, directory creation and other
pure side effects do not require assignments. Functions that also write must say so;
keep their returned objects or paths without writing them twice or adding a report merely
to fill a Save section. A streaming recipe may have two sections: Import data, then
Process and save data. Optional inspection examples belong in the guide and reuse those
returns; do not reconstruct paths or reopen an existing Arrow Dataset handle.

Scripts source the required scientific definitions directly, read explicit files, and
call the same scientific functions as targets with named arguments and breathing space.
Shared choices live as plain named values in `config/analysis_settings.R`; city-specific
definitions remain in the city configurations. Do not hide reads or settings in a
pack/unpack helper, replace a readable workflow with one orchestration call, dump a
function environment, or require tar_read() to inspect an ordinary processing script.
Return manageable data; retain paths for large partitioned datasets and show a bounded
read when inspection helps. Split functions by scientific operation, not line count.

For every new or changed function, document its transformation, return type, important
identifiers, and writing or in-place effects. Arguments must determine actual inputs.
For every structural change, demonstrate the path from declared inputs to a named
intermediate object and its defining function. Preserve the analytical specification.

## Command-line options

Recipes in `scripts/` produce the paper's results from one declared specification, so they
read no command-line options. A recipe must give the same outputs however it is called, and
a reader must see every choice in the file or in `config/analysis_settings.R`. Variants that
are reported or reviewed (other buffers, groupings, grids) are declared there as named
settings; the recipe loops over them, as `estimate_idw.R` loops over `idw_buffers_km`.
Options belong to launchers, which select what to run without changing what a recipe means:
`scripts/run_*.R`, `tools/reproduction/` and, for exploring undeclared values,
`tools/explore/`, which writes only to an ignored exploration folder.
`scripts/download_data/download_merra2_data.R` is the documented acquisition exception.

## Wrapper functions

A function may wrap other functions when it performs one scientific operation a student
could name in a sentence. Its arguments are inputs the script declares, and it returns one
documented object (or a small named list of paths) that the script keeps under one name.
Its `@details` lists the inner steps and the functions that perform them, so a reader can
descend one level from the console. `run_idw_city()` is the model: one call per city,
results kept in `idw_bogota[["3"]]`, and `aggregate_idw_exposure()` callable on its own.

Signs that a boundary is wrong: the script unpacks a returned list into many names; the
next call takes back what the previous one returned; a function takes and returns the same
accumulators; its name describes a script section rather than an operation; its
documentation would fit any function. The test: with only the script open, a student can
say for every named object which function produced it, what it contains and where it is
saved. A boundary that would make a sensible target usually makes a sensible function.

## Spacing, calls and comments

Exemplars, each for one purpose:

| Exemplar | Shows |
|---|---|
| `scripts/download_data/download_bogota_data.R`, `scripts/process_data/process_bogota_data.R` | Layout: grouped paths, aligned arguments, step comments |
| `scripts/process_data/generate_distance_matrices.R` | Computing in Section II, then saving in Section III |
| `scripts/process_data/compute_distance_band_descriptives.R` | Brief comments beside each operation |
| `doc/reference/idw_golden_test.md` | Explaining a method |

Keep short calls on one line. For longer calls, put the first argument on the same
line as the function when readable, align continuation arguments, and close the call
after its last argument, or on its own line for a multiline call like the Bogotá writers.
Use `<-` with spaces; align assignments within a small
related group when helpful, without padding unrelated statements. Leave blank lines
between operations so the reader can follow one step at a time.

Group Section I into sources, settings, source folders, output folders, input files and
reads, as appropriate, with a comment above each group. Keep paths visible without naming
every single-use path fragment. In every section, put one short comment above each
operation that says what happens to the data; distinguish objects in memory, connections
to on-disk data and returned filenames. Keep early source checks when they prevent partial
output or incomplete samples; they are not a targets requirement.

Keep every R line within 92 characters, including comments and section dividers.
If aligning arguments under the opening parenthesis exceeds that limit, use a shorter,
consistent continuation indent. Move the first argument to the next line when needed.
Preserve meaningful names; adjust alignment and wrapping rather than exceed the limit.
Script comments use short, concrete descriptions: set the paths, read the stations,
compute distances, save the tables. Explain geographic vintages or other choices where
they help the reader. Polish the English without replacing this voice with boilerplate.
Brevity applies to each comment, not to their number: a script with few comments is not
more concise, only harder to follow.

## Hard rules

- **Maximum line length: 92 characters.** Call alignment must fit within this limit.
- **Comments in `src/` functions: one home for rationale.** *Why* a function does what it
  does belongs in its `@Description` / `@details` block — written once, updated in place.
  Comments **inside** a function body are at most **2 lines** and say only what happens *to
  the data* here, or point back: `# see @Details: cluster identification`. Never restate in
  the body what the block above already says. If a change needs more explanation, extend
  `@details` — do not grow a paragraph mid-function. *Exempt:* the file header block and the
  `# ---` doc block above a `src/` function; those are the designated home and are long by
  design. Scripts follow the step-comment rule above instead.
- **Paths are always `here::here(...)`.** No absolute paths, no `setwd()`, no `~`.
- **Setup belongs in Section I.** Source the required subject modules and settings
  explicitly. Modules may declare and attach their required installed packages at the
  top, as in `src/city_specific/bogota.R`; callers need not repeat those package calls.
  Prefer qualified calls when attachment is unnecessary.
  Do not load the targets graph or install packages during analysis. Existing broad
  loaders are transitional, not templates. Dependency changes require separate approval
  and the reviewed `DESCRIPTION`/`renv.lock` workflow.
- **Set a seed** (`set.seed(...)`) in any script with randomness (sampling, jitter, bootstraps).
- **`src/` defines reusable logic.** Top-level package requirements and configuration are
  setup exceptions; analytical execution and file writes belong in `scripts/`.

## Every script starts with this header

```r
# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: <one line: what this script produces>
#
#' @Description: <2-4 lines: inputs, what is done, outputs and where they land>
# <continuation lines keep the plain # prefix>
#
#' @Summary:
#   I.   Import data: source functions and settings, declare paths, read files.
#   II.  Process data: compute named results.
#   III. Save outputs: save results in the same order.
#
#' @Date: <Month Year>
#' @Author: <name>
# ========================================================================================
```

**The `@tag` line takes `#'`; continuation lines and the `# ===` banner rules stay plain `#`.**
The `#'` prefix is for RStudio only — it colours the tags and is what makes these blocks
scannable. roxygen2 never runs here (no `R/`, no `NAMESPACE`, `DESCRIPTION` is `Type: Project`,
`Coding.Rproj` is `BuildType: Makefile`), so the custom `@Goal` / `@Summary` tags cost nothing.

Section dividers inside the script reuse the same `# ===` rule with a `# I:` / `# II:`
label. Banner rules must also fit within 92 characters.

## Every `src/` function has a doc block

Same `#'`-on-the-tag-line rule, with roxygen's standard argument tags:

```r
# ----------------------------------------------------------------------------------------
# Function: assign_socio_group
#
#' @param dt      data.table; modified in place. Must contain a `geo_id` column.
#' @param out_col string; name of the group column to create.
#
#' @return  the same data.table, invisibly, with `out_col` added.
#
#' @details
#   Why it does what it does. This is the one home for rationale.
#
#' @Written_on : July 2026
#' @Written_by : Marcos Paulo
# ----------------------------------------------------------------------------------------
```

Use `@param` / `@return` / `@details` — **not** the old `@Arg` / `@Output` / `@Details`. `@param`
takes the name then the description, with no colon between them. `@Purpose`, `@Written_on` and
`@Written_by` stay as project-specific tags.

## Idioms in this codebase

- Stack: `data.table`/`dplyr` for tables, `arrow`/`duckdb` for on-disk data, `sf`/`terra` for
  geospatial. Prefer Arrow/DuckDB over loading big data fully into memory.
- Write intermediates as **Parquet** (`arrow::write_parquet`) to `data/interim/` or
  `data/processed/`; use RDS only for non-tabular R objects. This keeps outputs inspectable.
- Add a city through `src/city_specific/registry.R` (`register_city(...)`) and a per-city module,
  never by copy-pasting a whole script. See `/add-city`.
- Prefer clear intermediate objects over deep pipe chains when it helps a reader see the data's
  shape at each step.

## Don't

- Don't add abstraction, configurability, or error handling that wasn't asked for. Remove
  code that does nothing for the reader, but never shorten a script by hiding its steps.
  Single-use code stays flat.
- Don't wrap uncertainty in defensive code — if you're unsure what an output looks like, ask
  Marcos to run and check it first.

Exceptions: .Rprofile container startup setwd;
explicit stage setup loaders and registry initialization; helper definitions in tests/.
`_targets.R` declares ordinary scientific calls and their saved-file targets. A computation
returns manageable objects; a writer returns the files it actually wrote. Large streaming
datasets can compute and write together, with that behavior documented. Temporary GDAL
geometry-conversion files are not analytical checkpoints.

Explain methods like `doc/reference/idw_golden_test.md`: a scientific question, a small
example, the expected result, and its connection to the function. Keep operational and
agent instructions out of the teaching path.
