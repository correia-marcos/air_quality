---
description: Register a new city through the registry pattern (not by copy-pasting scripts)
argument-hint: '[city_id]  e.g. lima'
---

# Add a city

Add `$1` to the analysis using the registry pattern in `src/city_specific/`. Do **not** clone an
existing city's scripts wholesale.

First read `src/city_specific/registry.R` and one existing module (e.g. `bogota.R`) to match the
exact shape of `cfg` and the `download` / `process` / `read_raw` / `normalize` functions.

Then, and only after I confirm the data sources for $1:

1. Create `src/city_specific/$1.R` defining `${1}_cfg` (paths, metro-area definition, station
   sources, census source, CRS, buffer km, analysis year) and the city functions.
2. Register it: `register_city("$1", cfg = ${1}_cfg, download = ..., process = ...)`.
3. Add a thin `scripts/process_data/process_$1_data.R` that sources the utils + registry + module
   and calls `city_process("$1")`, mirroring `process_bogota_data.R`.
4. Tell me what raw inputs $1 needs and where they must be placed under `data/raw/` — I will supply
   them; don't invent data.

Surface any assumption (CRS, metro definition, census vintage) explicitly and ask me to confirm
before coding. Flag which paper figures/tables will need a new `$1` entry.
