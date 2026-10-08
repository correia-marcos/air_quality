# Add a city

First acceptance criterion: follow the reader-first requirement in
[architecture](../architecture.md#the-reader-is-a-human-not-just-a-machine) and
[R style](../rules/r-style.md). The reader can run a few lines in RStudio, inspect
named intermediate objects, and locate the applied function without learning targets.
Use three or four meaningful executable sections; do not hide the analysis in one call.


Add `city_id` to the analysis using the registry pattern in `src/city_specific/`. Do **not** clone an
existing city's scripts wholesale.

First read `src/city_specific/registry.R`, `processing.R`, `preparation.R`, and `bogota.R`.
Match the configuration and common `process(cfg, steps, inputs, quiet)` contract.
The optional `download` capability is separate; there are no `read_raw` or `normalize` slots.

Then, and only after I confirm the data sources for city_id:

1. Create `src/city_specific/city_id.R` defining `city_id_cfg` (paths, metro-area definition, station
   sources, census source, CRS, buffer km, analysis year) and the city functions.
2. Define complete offline `geography`, `stations_filter`, `pollution_parquet`, and `census`
   operations and a convenience processing wrapper. Declare every input/output, including
   geographic vintages, all census
   variants and external sidecars. Extend prerequisite selection and the explicit module loader.
3. Register it: `register_city("city_id", cfg = city_id_cfg, download = ..., process = ...)`.
   A processing function is mandatory; duplicate registrations fail. Missing acquisition
   capabilities report an actionable error. Keep a stable lowercase slug.
4. Add `scripts/process_data/process_city_id_data.R` with explicit subject-module sources,
   settings and file reads. Call the scientific operations directly and retain manageable
   results. Save them in the same order in Section III. File-backed processing reports
   already-written outputs there, without writing twice.
5. Extend `_targets.R` with explicit scientific calls and upstream targets. Use the same
   functions, not one opaque city-processing target. Select multi-file outputs by explicit
   filenames, never by vector names or assumed positions.
6. Tell me which preserved inputs belong in `data/raw/` or `data/downloads/`, and record them in
   `config/input_sources.csv`. I will supply them; processing must never acquire missing data.
7. Verify stage prerequisites, error propagation, complete output ownership, geography/station
   selections, census schemas/weights/missingness, and pollution partition contents. Synthetic
   contract tests supplement, but do not replace, isolated parity against approved sources.

Surface any assumption (CRS, metro definition, census vintage) explicitly and ask me to confirm
before coding. Flag which paper figures/tables will need a new `city_id` entry.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
