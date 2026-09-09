# Run, verify and export

The analytical methods are unchanged by the structural migration. Existing numerical tests and audits support specific properties. A complete reproduction requires declared inputs, a recorded clean run, revision-matched comparisons and rendering review. No independent researcher reproduction is claimed.

## Daily development

Use the R version and dependency versions in renv.lock; restore them explicitly with renv::restore(). Do not regenerate the lockfile to resolve an incidental local library problem. The Dockerfile supplies spatial system libraries and bundled fonts. `make` preserves the existing stage orchestrator; `make merra2` runs the satellite track. Acquisition (`make download`) is always separate and may require credentials and interactive source access.

```sh
Rscript tests/testthat.R                         # synthetic tests plus available local checks
Rscript tests/testthat.R --mode=synthetic        # synthetic and structural checks only
Rscript tests/testthat.R --mode=release          # full-data checks; missing requirements fail
Rscript scripts/verification/verify.R            # local inventory, checks and export report
Rscript scripts/verification/verify.R --full     # build and attempt isolated full reproduction
```

Development reports optional missing full-data checks as skips. Assertion failures and errors produce a nonzero exit. Release treats skipped checks as incomplete. Test-suite release success alone is not a complete reproduction claim: the verification command additionally requires a fresh rebuilt run, unchanged inputs and reviewed comparisons. The package loader refuses automatic dependency installation when AIR_VERIFY_STRICT=1.

Local machine note (September 2026): existing compiled libraries work with the framework R at `/Library/Frameworks/R.framework/Versions/4.6/Resources/bin/Rscript`. Homebrew R 4.6.1 crashed when loading those binaries. Do not mix those installations. This is a local compatibility finding, not a portable invocation requirement. Restoring the container library remains the packaged route.

## Input access and preparation

| Input family | Access / acquisition | Placement and limitation |
|---|---|---|
| Station observations | City providers, through scripts/download_data/download_<city>_data.R | Preserve downloaded/source files in data/downloads and data/raw. Standardized hourly panels are derived and now belong in data/interim/monitoring_stations. Provider availability may change. |
| Census | DANE (Bogotá), INEGI (Mexico), INE (Santiago), IBGE (São Paulo); see city modules and source-specific scripts | Preserve original archives and source tables. Extracted working files belong in data/interim/census_extracted. Restricted access, laboratory-only inputs and redistribution rights must be verified with the data owner. |
| Geographic boundaries | Source definitions in each city module | Generated layers belong in data/interim/geospatial_data. Several existing builders are coupled to acquisition; an empty clean workspace may expose a missing preparation stage. Copied legacy layers do not count as regenerated layers. |
| MERRA-2 | NASA Earthdata; scripts/download_data/download_merra2_data.R | Earthdata access is required for acquisition. Preserve original granules and document collection/version. No credentials are needed merely to read authorized local copies. |
| Legacy comparison | Coauthor-supplied data/_legacy | Local audit inputs; no assumption of redistribution permission. Outputs in ignored data/validation/<city>. |

The repository does not establish that every input can be publicly redistributed. Before release, supply a file-level provider/version/access/license inventory, including instructions and contact/access route for restricted material. Do not invent a Zenodo deposit or identifier. The verification input CSV records SHA-256 hashes and sizes; those hashes establish file identity, not legal access or provenance on their own.

Migration preserves original source files. `config/data_migration.csv` classifies derived roots; the complete local copied-file map is in data/verification/migration-baseline/derived_data_migration.csv. No consumer should silently fall back to the former derived location under raw. Original archives remain intact.

## Isolated batch reproduction

`verify.R --full` builds the current working-tree code with the unchanged renv lock, records image identity, and invokes docker-compose.verify.yml. The service overrides the interactive entrypoint: no RStudio or Selenium is required. Source raw/downloads/_legacy mounts are read-only, generated intermediate/processed/result directories start empty, and runtime networking is disabled. Image construction can download dependencies; acquisition is not part of verification. Rebuilding is refused if the expected Linux source mounts are not read-only.

Local reports live under ignored data/verification/<timestamp>. They record code revision and uncommitted patch, code/input/output inventories, SHA-256 checksums, R/renv/platform/spatial-library versions, image identity, seed and thread settings, commands, timings, failures and limitations. Existing Parquet metadata is retained. Failures are evidence to investigate, not a reason to silently install packages, reuse stale outputs or widen numerical tolerances.

Historical figures and tables have unknown producing revisions. To register a reviewed baseline, populate config/verification_comparisons.csv with processed Parquet paths, unique semicolon-separated keys, baseline file paths, tolerances and any prior scientific justification. Keep restricted baseline files local. A baseline-review.json must identify baseline_revision, candidate_revision, candidate_code_sha256 (from the run code inventory), reviewer, human_reviewed, baseline_sha256 (path-to-hash mapping), plot_data_reviewed and rendering_reviewed. For isolated runs, store these materials in data/verification/baseline (or AIR_BASELINE_ROOT); it is mounted read-only at /baseline. Baseline paths in the comparison manifest refer to that container location. Never fill these fields on someone else's behalf. The verifier rejects unreviewed hashes, different schemas/identifiers/counts/missingness and uncovered processed Parquet products. New numeric defaults are atol=1e-10 and rtol=1e-8; existing stricter oracles remain unchanged.

Review all underlying plot data as well as rendering. PDF bytes can differ due to metadata/fonts; a byte difference alone is not a scientific discrepancy. A colleague's run is independent reproduction only after that colleague actually executes and reports it.

Retain the tested image, not only its mutable tag:

```sh
docker image inspect air-monitoring-verification:local --format '{{.Id}}'
docker save -o air-monitoring-tested-image.tar air-monitoring-verification:local
shasum -a 256 air-monitoring-tested-image.tar
```

Archive the image identity, tar checksum, code revision/patch, run report and authorized data inventory together. Rebuilding a Docker tag later can resolve different system packages. No tested-image retention is claimed until these commands run after an accepted verification.

## Results and manuscript export

Only two result roots exist: results/figures and results/tables. Figure topics are maps, monitoring, exposure, imputation, temporal, satellite and diagnostics. Tables are flat and descriptively named. Interactive HTML assets remain beside their HTML. Analytical Parquet intermediates belong in data layers. config/artifact_migration.csv maps old artifact paths to preserved new paths.

config/paper_artifacts.csv tracks artifact_id, source_path, paper_path and producer_script. Its initial selection contains 117 figure paths and 11 tables from the local draft; future drafts may legitimately change these counts. The exporter validates the entire manifest before writing, preserves existing manuscript-relative paths, checks SHA-256 after copying, refuses destination collisions and requires explicit overwrite. It never edits TeX, deletes unrelated files or synchronizes Overleaf.

```sh
Rscript scripts/export/export_paper.R --destination /your/local/paper --dry-run
Rscript scripts/export/export_paper.R --destination /your/local/paper
# Explicitly replace previously exported mapped files:
Rscript scripts/export/export_paper.R --destination /your/local/paper --overwrite
Rscript scripts/verification/check_manuscript.R doc/paper/paper_draft_part1.tex
```

The scanner ignores comments, follows available literal local TeX includes and checks ordinary includegraphics/table input references. It reports missing includes and dynamic constructs; it is not a TeX interpreter. The current absent data_appendix prevents a complete manuscript-coverage assertion. Keep the local draft unchanged. The ignored scripts/export/paper.local.sh stores only personal invocation/destination; reusable selection and export logic remain tracked.

The tracked Quarto report is scripts/validation_old_version/bogota_report.qmd. Render generated reports and comparison products under ignored data/validation/<city>, including self-contained HTML. The migration map preserves existing local artifacts; do not delete historical comparison evidence during cleanup.

## Clean-room protocol and resources

A colleague obtains the documented revision and retained image, acquires authorized inputs solely from package instructions, verifies their checksums, runs synthetic tests, then invokes the full isolated verification in a new directory. They report machine/image identity, commands, elapsed time, resource use, failures, skips, numerical differences and rendering assessment. Record their name/date only with their actual report. Resolve missing sources or scientific discrepancies before acceptance.

Observed local input footprint at migration: about 4.6 GB raw, 14 GB downloads, 2.7 GB interim and 470 MB processed, excluding image/library overhead. Allow additional space for a fresh run and retained image. These are observed storage sizes, not measured minimum RAM requirements. Full runtime and peak memory are not yet established; stage timings are recorded by the verifier and should replace estimates after a completed run. Satellite processing and imputation can dominate runtime. See doc/ai/implementation.md for this implementation's measured check results and run outcome.
