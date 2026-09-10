# Decision and evidence index

Status vocabulary: **open** (unresolved), **accepted** (explicitly accepted specification/evidence), **fixed** (defect corrected and verified against its original case), **superseded** (replaced with a linked decision). Human review is a separate field. Assistant execution never constitutes independent researcher reproduction.

| ID / claim or decision | Specification / code and revision | Evidence | Status | Human review and limitation |
|---|---|---|---|---|
| E01 Structural migration preserves analytical specifications | User-approved implementation plan; baseline 720f6412761bbfdcdaba008a8909bc2ce71b4889; current working tree | config/artifact_migration.csv; config/data_migration.csv; local data/verification/migration-baseline | accepted (scope) | User authorized scope; resulting changes await review. Numerical parity of full rebuilt products remains open. |
| E02 IDW and weighted regressions implement the tested examples | src/general_utilities/process/{idw_exposure,exposure_regressions}.R at baseline and working tree | Retained golden tests; new independent covariance and boundary tests; methods-tests.md | open | Tests support specific properties; human review of new evidence pending. |
| E03 Census reporting denominator | src/city_specific/cdmx.R at working tree; adults 25+, reporting weights | test-numerical-contracts.R | open | Mexican synthetic example passes; cross-city review pending. |
| E04 Every selected manuscript artifact can be copied without changing TeX | config/paper_artifacts.csv; src/general_utilities/reproducibility.R | Export dry-run and checksums; local manuscript scanner | open | Manifest reflects local draft; missing external includes prevent complete manuscript-coverage claim. |
| E05 Full scientific reproduction | Makefile, Dockerfile, scripts/verification/verify.R | Local verification reports, including failures and missing evidence | open | Historical outputs have unknown producing revision. No independent reproduction or reviewed baseline has been registered. |
| E06 Shared client guard behavior | tools/harness/guard_policy.py and native adapters | tests/harness/test_guard.py; installed-client discovery notes in implementation.md | open | Adapter tests are not evidence of client hook trust or runtime activation. |

Preserve local audits and assess findings at their original code revision before changing status. To promote an audit, record its original revision, exact claim, relevant method, reproduction command, sanitized supporting evidence, reviewer, date and remaining limitation. Do not copy restricted data or unpublished draft excerpts into tracked reports. Future entries should link a fixed code revision, not just a moving branch.
