# Implementation and evidence

## Container remediation — 9 September 2026

The accepted remediation prepares geography inside existing city processors, preserves
provider/package source copies, separates temporal manuscript production from optional
satellite comparisons, and retains three distinct container modes. `snapshot.dev = TRUE`
captures project testing dependencies without selecting downstream optional dependencies.
Scientific definitions and manuscript destinations are retained.

Measured checks during implementation:

- Native synthetic suite: 210 passed, no failures/errors/skips/warnings, using framework R
  with the declared existing local library. This is separate from container testing.
- Seven geographic products regenerated from local archives matched historical interim
  rows, identifiers, attributes, CRS and binary geometry exactly: Bogotá 2018 metro and
  tracts, CDMX municipalities and AGEB, Santiago 2024 metro, São Paulo municipalities and
  census tracts. All eight source-file SHA-256 hashes were unchanged.
- Temporal fixtures passed; the extracted 33 preparation expressions and both plotting
  loops matched their original expressions. Real temporal inputs are missing locally.
- Five harness tests passed from the repository and an unrelated working directory.

Local evidence is under `data/verification/remediation-20260909/`. Historical intermediates
are comparison evidence, not an approved revision-matched baseline. Six declared geographic/
census source snapshots remain unavailable; the verifier reports these before analytical
rebuilding. Full source-to-manuscript execution, rendering review, and independent researcher
reproduction remain incomplete. No provider version was substituted to bypass these limits.

## Historical migration record — 7 September 2026

Implemented in the working tree based on revision `720f6412761bbfdcdaba008a8909bc2ce71b4889` on 7 September 2026. The following records describe that historical migration and its review state at the time. Analytical formulas, samples, weighting and missingness specifications were preserved. Existing audits retain their evidential value and original scope.

## What changed

1. **Baseline captured.** Local `data/verification/migration-baseline/` contains the original code archive, file inventory, original report source and migration checks. Existing result files are historical references because their producing revision was not established.
2. **Shared agent guidance.** `doc/ai/` is canonical. Root entry points and Claude commands/rules/reviewers route to it. Seven Codex skills, two reviewer definitions, minimal project configuration, command rules and hook wiring were added. Optional common guard policy lives in `tools/harness/guard_policy.py`; client adapters retain supported native decisions.
3. **Reliable checks.** Development, synthetic and release modes return nonzero on assertion failures/errors; synthetic and release reject skips. Deliberate failure, missing-input and subprocess fixtures exercise the runner. Freshness checks use the selected files of each producer, so unrelated historical plots in a newly shared topic folder do not falsely date that producer.
4. **Data layers corrected.** The four city configurations and their generated-geography/station/extraction producers and consumers now use interim paths. Bogotá's callable processing wrapper was also corrected, not just its standalone script. Original raw files remain. Original legacy satellite panels/geographies retain their specification-specific source paths; substituting current standardized panels would change the analysis and was not done.
5. **Results and export.** Only figures/ and tables/ remain under results. Figure topics follow the agreed seven categories; tables are flat. Interactive assets stay beside their HTML. The tracked manifest selects 117 current figure paths and 11 tables while preserving manuscript destinations. Eight matching table copies were consolidated; the additional identical census-table alias was consolidated too. Fourteen analogous exposure-figure pairs have different bytes and remain preserved historical presentations; no visual equivalence was assumed. Current producers save selected plot objects only once.
6. **Numerical evidence and packaging.** Added independent covariance, reporting-population denominator, IDW boundary/order, observed-value imputation, outlier temporal/spatial and synthetic integration checks. Added table parity comparisons, run/stage reports, extended optional Parquet provenance, a synthetic CI workflow, batch container configuration, font inclusion, build-time ICU caching and citation metadata from DESCRIPTION.

## Checks actually completed

| Check | Observed result |
|---|---|
| Synthetic suite before the final two exporter edge cases | 119 passed; no failures, errors, skips or warnings |
| Local release test suite | 245 passed; no failures, errors, skips or warnings |
| Additional exporter edge cases | Unbraced TeX references are reported; symlink escape is rejected; exporter test file passes |
| Shared guard and native adapter tests | Five Python tests passed, including multi-file patches, rename/delete, protected inputs, shell behavior and .env.example |
| Migration integrity | 417 artifact-map records accounted for; 146 copied derived files have identical source/destination SHA-256; no integrity findings |
| Original inputs | 7,111 original files retain baseline size and nanosecond mtime. SHA-256 inventories were subsequently captured. Initial source hashes were not captured, so this is not a retrospective cryptographic proof of their pre-migration contents. |
| Manuscript export | Dry-run and actual local export completed for all 128 selected artifacts, with copied-file checksum verification |
| Manuscript scanner | No unmapped or unused manifest paths; missing `doc/paper/data_appendix.tex` prevents complete coverage |
| Static checks | R source parsing, project TOML/JSON parsing, Compose configuration and YAML parsing passed; seven skill targets and frontmatters resolve |
| Installed Codex discovery | CLI 0.153.0 lists the pinned Context7 and DeepWiki configurations; startup remains unverified because npx is absent from this process's PATH |

The skill-creator quick_validate.py could not start because PyYAML is unavailable. Frontmatter was independently validated with the installed R YAML parser; this is not a claim that the official Python validator ran successfully. Hooks were tested as adapters; installed-client trust and live hook operation were not verified. No hook protection is advertised as active.

The first local inventory/check/export report is [data/verification/20260907T201921/report.json](../../data/verification/20260907T201921/report.json). Its development test stage took about 28 seconds (231 checks at that point, one explicitly skipped release-only check). Later tests include the additional layout and runner assertions shown above. The local [release verification](../../data/verification/release-check/report.json) additionally records the absence of a reviewed baseline and fresh isolated rebuild. Reports describe the code snapshot they captured; later documentation edits are not silently attributed to earlier runs.

## Acceptance still outstanding

- **Full container reproduction was not executed.** Docker Desktop startup was approved, but the separate Docker build/run escalation was rejected by the user and was not retried. The [attempt record](../../data/verification/full-attempt/report.json) records that boundary. No tested image or full runtime measurement exists from this task.
- **Revision-matched numerical parity remains unverified.** The comparison registry is intentionally empty until genuine baseline provenance, unique keys and review are supplied. The verifier reports incomplete evidence rather than fabricating a baseline. Existing historical files were preserved.
- **Clean source-to-geography preparation requires execution evidence.** City processors now
  rebuild their geographic prerequisites from preserved local sources with acquisition disabled.
  A fresh offline run from empty derived directories, source-hash checks, and geographic
  comparisons remain required before clean-room acceptance.
- **External manuscript coverage is incomplete.** The absent appendix was not invented or replaced. The local TeX draft was not edited.
- **New image/CI and Quarto rendering have not been executed.** Their configurations parse, but runtime operation requires their declared environment. Scientific rendering review, human review of new numerical evidence, input redistribution/access review and independent researcher reproduction remain separate recorded actions.

The repository now distinguishes a passing local test suite, successful copying of existing exhibits, and a verified clean reproduction. Only the first two have been established here. Use [HOW_TO_RUN](../HOW_TO_RUN.md) for commands and the clean-room protocol; use [evidence.md](evidence.md) to record reviewed scientific decisions.
