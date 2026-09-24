# Run, verify and export

Navigation: [documentation index](README.md) · [first run](guides/first-run.md) ·
[data dictionary](reference/data_dictionary.md) · [resolution sensitivity](RESOLUTION_SENSITIVITY.md).

This is the operational guide for the repository. The analytical methods are unchanged by
the infrastructure work. Passing tests, a completed pipeline, reviewed numerical parity, and
independent researcher reproduction are different forms of evidence. Complete scientific
reproduction is not yet claimed.

## Development and acquisition

Restore the recorded R environment before running locally:

```r
renv::restore()
```

Use `docker compose up` for interactive RStudio with live code. Run
`docker compose --profile acquisition up` when a download script needs Selenium. The
release Compose file starts interactive RStudio from code baked into the image:

```sh
docker compose -f docker-compose.release.yml up
```

Copy `.env.example` to `.env` only for interactive RStudio configuration. Keep Earthdata
and other source credentials outside the repository. The batch verifier needs neither
credentials nor Selenium.

Acquisition is a deliberate, online first step. Use the relevant script in
`scripts/download_data/`, retain the authorized source files in `data/downloads/`, and record
provider, requested year/version, retrieval date and checksum. Preserve `data/raw/`,
`data/downloads/`, `data/_legacy/`, and `renv.lock`; do not regenerate or overwrite them as
a routine processing step.

Some inputs have restricted access, laboratory-only availability, or non-redistributable
licenses. File hashes identify the local input used; they do not establish public access,
provider provenance, or scientific equivalence to an unavailable historical version.

## Input preparation

Each city processing script first prepares the geographic layers it consumes from declared
local sources. Processing passes `allow_download = FALSE`: it must report a missing source and
the acquisition route instead of contacting a provider or accepting an old generated layer.
The geographic definitions remain the city modules' responsibility, including their current
years, identifiers, clipping, CRS transformations, and membership rules.

For a clean run:

1. Acquire all authorized sources online through the existing acquisition functions.
2. Preserve the downloaded files and record their metadata and hashes.
3. Start with empty `data/interim/`, `data/processed/`, and `results/` directories.
4. Run the city processors; they regenerate geography and the downstream products from local
   sources.

Santiago's package-managed census database and São Paulo's package-managed census data are
also declared inputs. Their source copies must be available locally; any writable database
working copy belongs in `data/interim/`. Do not rely on an installed package cache or runtime
download during processing or verification.

If an historical provider version is unavailable, do not treat a newly retrieved version as
equivalent without scientific review. Existing geographic intermediates may aid comparison but
do not replace the source-based preparation step.

### Acquire the missing package/provider sources only

After loading the download configuration and the relevant city module, the calls below
preserve sources without running the station-download workflow. For example:

```r
source(here::here("src", "general_utilities", "config_utils_download_data.R"))
source(here::here("src", "city_specific", "registry.R"))
source(here::here("src", "city_specific", "santiago.R"))
santiago_download_metro_area_2017(allow_download = TRUE)
santiago_acquire_census_2017()

source(here::here("src", "city_specific", "sao_paulo.R"))
sao_paulo_download_weighting_areas(
  keep_municipality = sao_paulo_cfg$cities_in_metro, allow_download = TRUE)
sao_paulo_acquire_census_2010()
```

These calls contact providers when sources are absent. For historical replication, supply the
reviewed `version` to `santiago_acquire_census_2017(version = ...)`; the default uses the
package provider default and does not establish equivalence to an old run. Existing source
copies are reused. Refreshing them requires explicit `overwrite_source` (geography) or
`overwrite` (census). Bogotá, CDMX and the other geographic archives are acquired by the
geography sections of their existing download scripts, using each city's configured paths.

An existing unfiltered package-cache file can instead be preserved explicitly, without any
download. Identify its release and retain its provenance before treating it as a replication
input:

```r
santiago_acquire_census_2017(local_source = "/path/to/censo2017_duckdb_v155.sql")
sao_paulo_acquire_census_2010(local_source = "/path/to/2010_population_v0.5.0.parquet")
```

These helpers verify the original and copied SHA-256 hashes. Their sidecars distinguish the
local preservation date from the unknown original provider retrieval date. They do not certify
that a cached file produced a historical manuscript result.

The declared additional sources are:

| Source | Preserved location under `data/downloads/` |
|---|---|
| Santiago 2017 ArcGIS responses | `santiago/metro_area/2017/GRAN_SANTIAGO_13_metro.geojson`, `GRAN_SANTIAGO_13_zonas.geojson`, `GRAN_SANTIAGO_13_count.json` |
| São Paulo 2010 weighting areas before metro filtering | `sao_paulo/metro_area/sp_weighting_areas_2010.rds` |
| Santiago census before project filtering | `santiago/census/2017/censo2017.duckdb` |
| São Paulo population before project filtering | `sao_paulo/census/2010_population.parquet` |

New acquisitions write `.source.json` sidecars with provider, requested version, acquisition
time and SHA-256. Preserved cache copies record preservation time separately. Older archives
without sidecars retain their original files; their retrieval
dates are not invented. The verifier's `preparation-inputs.csv` reports these geographic and
package-managed census prerequisites; it is not an exhaustive analytical-input certification.
Use the isolated verifier to create fresh derived directories without deleting existing work.

## Inspect the analysis in RStudio

Open `Coding.Rproj`, then `scripts/process_data/generate_distance_matrices.R`.
Section I shows the source functions, shared settings and nine geographic input files.
Run those lines to create the station and geographic `sf` objects. Section II makes five
calls to `compute_distance_matrices()`; each result has `station_matrix` and
`geo_station_matrix` tables. Section III saves those results in the same order.

For Bogotá, inspect `bogota_stations_2018_sf`, `bogota_metro_2018_sf`, and then
`bogota_distances$geo_station_matrix`. The calculation is defined in
`src/general_utilities/process/distances.R`; the method choices are named in
`config/analysis_settings.R`. No targets cache or registry lookup is needed.

The saved station matrix feeds outlier detection; the geographic matrix feeds IDW.
Both are written under `data/processed/distances_matrices/bogota_2018/`, with the same
structure for the other contexts. See the [small first-run example](guides/first-run.md)
if you do not have the prepared city inputs. The
[complete workflow inventory](planning/remaining-work.md) is a maintainer reference.

The four city-processing scripts now follow the same three-section layout. For example,
`process_bogota_data.R` exposes `station_locations`, `metro_2018`, and `stations_2018`.
Section III saves the spatial objects. Pollution and census functions write during
processing; their returned `pollution` and `census_2018` objects remain available.
The 20 km station radius and UTC processing timezone are named in each city's configuration.
All declared sources must be present; processing never downloads missing files.

For Bogotá, `download_bogota_data.R` acquires the geographic source files.
`process_bogota_data.R` lists their paths in Section I, calls
`bogota_prepare_metro_area()` in Section II, and saves the five layers in Section III.
The download recipe prepares one temporary footprint for its source-region diagnostic;
it does not write the processing workflow's geographic products.
The other cities follow the same acquisition/preparation split. Geographic preparation
reads explicit local sources and returns spatial objects; saving is a separate call.
For the optional administrative-commune Santiago boundary formerly built during download,
run `Rscript scripts/process_data/prepare_santiago_alternative_geography.R`.
This does not rebuild manuscript inputs or start acquisition.

Other inspection routes still awaiting simplification:

- Analysis: inspect missingness and station-count objects in `compute_descriptive_tables.R`,
  and `estimates_by_buffer` in `estimate_exposure.R`.
- Figures: inspect input tables and the named plot collections in `figure_station_scatter.R`.
- Optional resolution: select the reference or multicity scope in the resolution recipes;
  inspect `prepared` or `results_by_city`. Their input checks never refresh manuscript inputs.
- Validation: `compare_bogota.R` exposes comparison objects; `bogota_report.qmd` reads named
  artifacts in narrative chunks. Missing comparison files remain pending evidence.

After running Bogotá's pollution function, `pollution` is already an Arrow Dataset.
To inspect a few rows without loading the whole dataset:

```r
preview <- pollution |>
  dplyr::filter(year == 2023L) |>
  utils::head(10L) |>
  dplyr::collect()
```

Likewise, read a saved census summary using its returned path when you need it:

```r
census_2018_summary <- arrow::read_parquet(census_2018[["collapsed"]])
```

Functions that write while processing say so in their contract. Do not rerun a whole recipe
merely to inspect an existing file. Use the visible reading calls with existing products.
Actual RStudio walkthroughs and full scientific parity remain acceptance requirements.

## Manuscript pipeline

The following entry points are transitional until scientific acceptance of the targets
candidate. They share scientific functions with it and are not independent baselines.
From the repository root, after inputs are available:

```sh
make all
```

```r
source(here::here("scripts", "run_pipeline.R"))
```

Both routes execute city processing, the core station/census analysis, preserved temporal
preparation, and manuscript figures and tables. `make` uses stage stamps; after changing or
reacquiring source data, force a rebuild with `make -B all`.

The candidate's direct command is `Rscript scripts/run_targets.R all`. Inspect the ordinary
declarations in `_targets.R`; cached station counts, missingness, census summaries,
exposure tables and figure inputs have named data targets. After scientific acceptance,
targets becomes the sole manuscript scheduler: remove Makefile and RStudio's Make build
setting, and reduce `run_pipeline.R` to a compatibility launcher. This cutover is pending.

Distance targets now separate input reading, calculation and file writing. For example,
`bogota_2018_distance_matrices` stores the computed tables and `bogota_2018_distances`
owns the two Parquet files. The `distances` selection still requests all five contexts.
The candidate graph retains upstream city preparation: an existing derived file alone
does not bypass missing preserved sources in a fresh targets run.

The temporal manuscript figures intentionally retain the legacy station panels and MERRA-2
timestamp support. This preserves their historical time support and sample. It does not claim
that the figures are independent of that preparation.

## Supporting analyses

The inventory records direct commands and prerequisites for every optional workflow,
including resolution, manual diagnostics and legacy validation. These commands remain
separate from manuscript targets. Existing optional Make commands are transitional aliases.

Run optional satellite comparisons separately:

```sh
make merra2
```

This reuses the temporal prerequisites and adds country/NASA comparisons, satellite
correlations, aerosol figures, thermal-inversion comparisons, and grid illustrations. Acquire
its additional MERRA-2/NASA inputs before running it. `make merra2` is supporting analysis and
does not replace `make all` as the manuscript route.

Context maps are also optional: `make context-maps` prepares the city inputs and renders
additional context figures. Its Stadia basemap may require online access and credentials;
these unselected figures are outside offline manuscript verification. Legacy comparison uses
`make validate` and its separate authorized legacy inputs.

## Checks

```sh
Rscript tests/testthat.R --mode=synthetic
Rscript tests/testthat.R --mode=release
Rscript scripts/verification/verify.R
```

Synthetic mode runs portable checks. Release test mode treats missing full-data requirements as
incomplete. `verify.R` records local code and input inventories, checks, export status, and
known limitations; it is not a clean-room reproduction claim.

## Isolated batch verification

```sh
Rscript scripts/verification/verify.R --full
```

`--full` builds the image and invokes `docker-compose.verify.yml`. The verifier mounts
`data/raw/`, `data/downloads/`, and `data/_legacy/` read-only; starts generated directories
empty; uses image-baked code; and disables runtime networking. It executes `make -B all`, so
geography and other derived products must be recreated from the declared source copies.

The verifier reports missing prerequisites before expensive stages where possible. It records
the supplied code revision and patch separately from the container's code inventory; it only
queries Git when repository metadata is present. A failed stage, missing source, skipped
required check, incomplete manuscript inspection, or missing reviewed baseline remains a
failure or limitation in the report.

Keep a tested image by identity rather than mutable tag:

```sh
docker image inspect air-monitoring-verification:local --format '{{.Id}}'
docker save -o air-monitoring-tested-image.tar air-monitoring-verification:local
shasum -a 256 air-monitoring-tested-image.tar
```

Record the image identifier and checksum with the code revision/patch, lockfile hash,
installed package/system-library versions, declared input inventory, run report, numerical
comparisons, and rendering review.

## Results and manuscript export

`config/paper_artifacts.csv` maps each selected product to its unchanged manuscript path and
producer script. Export only to an authorized local manuscript copy:

```sh
Rscript scripts/export/export_paper.R --destination /your/local/paper --dry-run
Rscript scripts/export/export_paper.R --destination /your/local/paper
Rscript scripts/export/export_paper.R --destination /your/local/paper --overwrite
Rscript scripts/verification/check_manuscript.R doc/paper/paper_draft_part1.tex
```

The exporter validates the manifest, checks copied-file hashes, and never edits TeX or deletes
unrelated manuscript files. The scanner reports absent includes and dynamic constructs; it is
not a TeX interpreter. The unavailable `data_appendix` prevents a complete manuscript-coverage
claim.

## Acceptance limits

An accepted scientific reproduction still needs a fresh isolated run, declared and unchanged
inputs, revision-matched numerical baselines, review of plot data and rendering, and a recorded
independent researcher run. Do not fill comparison or review evidence on another person's
behalf, manufacture a baseline, widen tolerances, or substitute newer inputs silently.

## Dependency snapshots

`renv/settings.json` keeps explicit snapshots and enables `snapshot.dev`, so the project's
`Suggests` (including testthat) enter the lockfile. Required dependency fields remain
`Imports`, `Depends`, and `LinkingTo`; adding `Suggests` globally would also pull optional
packages from downstream dependencies. Dependency changes use renv, with analytical versions
reviewed separately from the testing additions.

## Candidate manuscript migration to targets

The candidate graph is available alongside the current default runner. Scientific acceptance
and default cutover remain pending. It uses the same reusable transformations as the manual
scripts, tracks files and directory membership, and never downloads inputs or installs packages.
Restore the full project environment before execution; installing `targets` alone is insufficient.

```sh
make targets STAGE=bogota_geography  # pilot; writes derived geography
make targets STAGE=bogota_pollution_parquet  # prerequisites included
make targets                       # manuscript export and all its prerequisites
make targets-outdated              # inspect the manuscript selection without processing
Rscript scripts/verification/verify.R --full --targets
```

Development and image-baked Compose services mount `_targets/` as the persistent cache.
The verification service uses a fresh per-run cache, read-only sources and isolated output roots.
`AIR_TARGETS_STORE` overrides the cache location, not the data/output roots. Selecting a different
cache does **not** isolate data writes; use verification Compose for comparison runs.
The complete graph requires canonical data roots because downstream analytical readers use
that layout. A noncanonical city output root is rejected; manual city wrappers can use separate
configured roots for city-level comparisons.

The four manual interfaces are complete:

```r
source(here::here("src", "pipeline", "load.R"))
load_manuscript_functions(kind = "process", packages = TRUE)
processed <- city_process("bogota")
geography <- city_process("santiago", steps = "geography")
```

Common stages are `geography`, `stations_filter`, `pollution_parquet`, and `census`; prerequisites
run first. `inputs = NULL` resolves configured local sources. An explicit input list overrides
whole named stages; inspect `city_processing_inputs("bogota", city_cfg("bogota"))` for the shape.
Bogotá's `census_2005` and `census_2018` selectors remain available. Missing inputs fail preflight,
and required stage failures stop processing. Results contain named path vectors for every stage.
Reusable transformation functions retain manageable data returns and named local inspection
points. Stage adapters return paths; large partitioned datasets are reopened by their consumers.

The graph keeps pollution years together and serializes each city/vintage's IDW buffers and
income reuse. It includes imputation, descriptions, scatter inputs and temporal MERRA-2 inputs.
Optional resolution, acquisition, context maps, satellite comparison and validation retain their
existing entry points. Frozen resolution inputs are not regenerated by this graph.

The public development record in `doc/planning/targets-migration.md` lists acceptance gaps and
model responsibility; that planning file is deliberately excluded from runtime images.

## Optional three-city resolution sensitivity

`make resolution-multicity` runs the separate A/B/C methodological analysis for Bogotá,
Santiago, and São Paulo from existing derived inputs. It does not invoke or export the
manuscript workflow. Definitions, input requirements, reference verification, outputs,
and clean-source limitations are in [RESOLUTION_SENSITIVITY.md](RESOLUTION_SENSITIVITY.md).
