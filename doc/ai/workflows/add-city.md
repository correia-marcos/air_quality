# Add a city

Add `city_id` to the analysis using the registry pattern in `src/city_specific/`. Do **not** clone an
existing city's scripts wholesale.

First read `src/city_specific/registry.R` and one existing module (e.g. `bogota.R`) to match the
exact shape of `cfg` and the `download` / `process` / `read_raw` / `normalize` functions.

Then, and only after I confirm the data sources for city_id:

1. Create `src/city_specific/city_id.R` defining `city_id_cfg` (paths, metro-area definition, station
   sources, census source, CRS, buffer km, analysis year) and the city functions.
2. Register it: `register_city("city_id", cfg = city_id_cfg, download = ..., process = ...)`.
3. Add a thin `scripts/process_data/process_city_id_data.R` that sources the utils + registry + module
   and calls `city_process("city_id")`, mirroring `process_bogota_data.R`.
4. Tell me what raw inputs city_id needs and where they must be placed under `data/raw/` — I will supply
   them; don't invent data.

Surface any assumption (CRS, metro definition, census vintage) explicitly and ask me to confirm
before coding. Flag which paper figures/tables will need a new `city_id` entry.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
