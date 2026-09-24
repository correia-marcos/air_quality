# R style

A reader must be able to open RStudio, execute a few lines of a script—including
sourcing functions and reading files—inspect meaningful intermediate objects, identify
the function applied, and understand the analysis without first learning the pipeline's
internal machinery.

This is the highest structural priority. It takes precedence over minimizing script
length, eliminating every repeated function call, or abstracting orchestration.
Scientific definitions and protected inputs remain unchanged.

## Executable recipes

Default sections: I. Import data (including sources, settings and paths), II. Process
data, III. Save outputs. Save named results in the same order as their computations.
Do not add an "Inspect before saving" block. Use a fourth section when preparing data and
estimating or rendering are distinct. Operational launchers use accurate headings such
as Settings, Checks, Execute and Report. Quarto reports retain their narrative structure.
The header summary must match the executable sections. Do not add empty sections.

Assign useful data, estimates and plots to named objects. Keep results from repeated
city/pollutant runs in named lists. Intentional logging, directory creation and other
pure side effects do not require assignments. Functions that also write must say so;
report their existing files rather than write them twice merely to fill a Save section.

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

## Spacing, calls and comments

Follow the author-edited `scripts/process_data/generate_distance_matrices.R`.
Keep short calls on one line. For longer calls, put the first argument on the same
line as the function when readable, align continuation arguments, and close the call
after its last argument. Do not force every call into one argument per line with a
separate closing parenthesis. Use `<-` with spaces; align assignments within a small
related group when helpful, without padding unrelated statements. Leave blank lines
between operations so the reader can follow one step at a time.

Keep every R line within 90 characters, including comments and section dividers.
If aligning arguments under the opening parenthesis exceeds that limit, use a shorter,
consistent continuation indent. Move the first argument to the next line when needed.
Preserve meaningful names; adjust alignment and wrapping rather than exceed the limit.
Script comments use short, concrete descriptions: set the paths, read the stations,
compute distances, save the tables. Explain geographic vintages or other choices where
they help the reader. Polish the English without replacing this voice with boilerplate.

## Hard rules

- **Maximum line length: 90 characters.** Call alignment must fit within this limit.
- **Comments: one home for rationale.** *Why* a function does what it does belongs in its
  `@Description` / `@details` block — written once, updated in place. Comments **inside** a
  function body are at most **2 lines** and say only what happens *to the data* here, or point
  back: `# see @Details: cluster identification`. Never restate in the body what the block
  above already says. If a change needs more explanation, extend `@details` — do not grow a
  paragraph mid-function. *Exempt:* the file header block and the `# ---` doc block above a
  `src/` function; those are the designated home and are long by design.
- **Paths are always `here::here(...)`.** No absolute paths, no `setwd()`, no `~`.
- **Setup belongs in Section I.** Source the required subject modules and settings
  explicitly. Attach necessary installed packages there, or use qualified calls.
  Do not load the targets graph or install packages during analysis. Existing broad
  loaders are transitional, not templates. Dependency changes require separate approval
  and the reviewed `DESCRIPTION`/`renv.lock` workflow.
- **Set a seed** (`set.seed(...)`) in any script with randomness (sampling, jitter, bootstraps).
- **`src/` holds functions only.** No top-level side-effects there. Runnable code lives in `scripts/`.

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

Use `scripts/process_data/generate_distance_matrices.R` as the processing example. Section
dividers inside the script reuse the same `# ===` rule with a `# I:` / `# II:` label. Banner
rules must also fit within 90 characters.

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

- Don't add abstraction, configurability, or error handling that wasn't asked for. If 200 lines
  could be 50, write 50. Single-use code stays flat.
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
