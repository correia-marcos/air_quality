# Repository review and implementation status

Review date: **21 September 2026**. Baseline:
`512e96ea571a24f7e40289e3a4060cbc1cabc230`, plus the unstaged changes described below.
Prepared with Codex. This is a repository/process assessment, not a scientific procedure
audit or independent reproduction. Human review of this implementation is pending.
The [8 August assessment](reviews/repository/2026-08-08.md) is preserved byte-for-byte.

## Findings and diagnosis

The historical Git boundary was incomplete. At revision `4c80f71`:

- `.codex/rules/project.rules:1–4` forbade plain push and hard reset, but not commit.
- `.claude/settings.json:35–48` placed add, commit, and push under **ask**, not deny.
- `tools/harness/guard_policy.py:39–47` prompted on destructive commands/force-push but
  returned no restriction for ordinary commit or push.
- Root/shared instructions did not consistently encode the requested recommendation-only
  boundary. The process could treat reviewable commit batches as execution authorization.
  A permission prompt is not a prohibition.

These gaps explain how the behavior was possible; they do not prove which permission source
authorized the historical execution. The earlier investigation identified an agent commit
and broad Git escalation request, but exact approval provenance remains unknown. The evidence
does not establish a model-specific cause or show that using a different model would prevent it.

The P0 corrections are in user-created revisions `b8c2611` and `57b9023`:
[Git safety](ai/rules/git-safety.md) prohibits staging, `git commit`, `git push`, and equivalent
indirect actions; [shared policy](../tools/harness/guard_policy.py),
[Codex rules](../.codex/rules/project.rules), and [Claude permissions](../.claude/settings.json)
encode denials. Tests or approval of file groupings do not authorize Git mutations.

**Live activation is no longer pending for the tested Codex client.** Its project hook was
enabled/trusted and blocked a harmless invalid Git option before execution. No activation
change was needed. [Host enforcement](ai/host-enforcement.md) records the version, scope,
fixture results, and remaining deployment work. Claude live behavior is unverified because
the client was not found on PATH.

| Earlier concern/claim | Current assessment |
|---|---|
| No tests or CI | No longer a problem: `tests/testthat/` and `.github/workflows/synthetic.yml` exist. Preserve and use them. |
| Hooks guarantee protected inputs | Too strong: text inspection has coverage limits. Host filesystem controls need separate evidence. |
| Docker/renv establish full bit-for-bit reproduction | Unsupported as a blanket claim. Declared inputs, fresh execution, comparisons, and researcher review are still required. |
| A targets migration is required | Not established. The Makefile and R orchestrator need coordinated maintenance and have path tests. Keep the targets document as a proposal. |
| Every audit folder should move | Unnecessary. Preserve historical local reports and classify future reports by method and purpose. |

## Priorities and model delegation

Assignments below reflect task risk, not measured model benchmarks. Every model receives
the same no-stage/no-commit/no-push boundary. No subagents were used in this continuation.

| Step | Disposition / acceptance | Context and reasoning | Delegation |
|---|---|---|---|
| P0 Git instructions/denials | Existing corrections retained; fixtures must deny commit and push as strings | Repository-wide authorization/tool semantics | Astra owns policy; Terra can implement bounded adapters/tests |
| P1 Live Codex hook | Enabled/trusted; benign denial observed; inspection allowed | Client-specific evidence | Terra repeats prescribed checks; Astra assesses coverage |
| P1 Machine enforcement | Tested bundle prepared; admin installation and effective-policy checks pending | Cross-tool publishing, permissions, Git/common-directory topology | Astra retains design and acceptance; Terra can run a fixed fixture |
| P2 Document moves | Implemented with a checksum ledger and historical copies; unused redirects removed 22 September 2026 | Mechanical once the map is fixed | Luna or Terra with exact file ownership and content comparison |
| P2 Audience routes/taxonomy | Implemented; review status separate from audit method/provenance | Repository-wide information organization | Terra drafts; Astra reviews architecture/scientific terminology |
| P2 Current repository assessment | This page replaces stale current claims; original archived | Reconcile revisions, evidence, literature | Astra |
| P2 Link checks and CI | Offline checker and negative fixtures added | Bounded software engineering | Terra; Luna can update approved links and run checks |
| P3 Scientific reproduction | Existing outstanding requirement; not completed here | City definitions, provenance, numerical/visual evidence | Astra and researcher retain decisions; Terra executes an approved protocol; Luna inventories/formats results |

Changes to estimands, missingness, geographic definitions, Design A/B/C interpretation,
legacy comparability, pipeline architecture, or manuscript artifact selection remain outside
this documentation task. They need researcher-owned specifications and separate validation.

## Documentation architecture

[The documentation index](README.md) routes students, reviewers, contributors, auditors,
and agent users to maintained pages:

```text
doc/
  README.md                     Audience and topic navigation
  HOW_TO_RUN.md                 Stable operational entry point
  REPO_REVIEW.md                Current dated assessment
  RESOLUTION_SENSITIVITY.md     Stable supporting-analysis guide
  guides/                      First run, contributing, procedure-audit guide
  reference/                   Data dictionary and IDW worked reference
  planning/                    Remaining work, deletion candidates, targets proposal
  reviews/                     Catalog, report template, move ledger
    repository/                Preserved historical assessments/guides
  ai/                          Canonical shared agent guidance and evidence
  audits/                      Local reports; only README public
  notes/                       Local research explanations; only README public
  paper/                       Local manuscript sources; only README public
```

Navigation update, **22 September 2026**: the seven former dictionary, IDW, planning, setup,
and procedure-guide redirect pages were removed after checking maintained links and
correcting three R-test comment references. Historical paths in archived reports and the
move ledger are preserved. External bookmarks cannot be established by repository inspection.
Root README, operational navigation, shared architecture/index,
methods/tests map, canonical procedure workflow, manuscript README, and Claude hook README
point to maintained locations. The hook README had a pre-existing extra `../`; it is fixed.
No static-site framework or new documentation dependency was introduced.

[Report types](reviews/README.md#report-types) distinguish procedure audits, repository
reviews, reproducibility audits, focused investigations, and research explanations. A report
is **workflow-produced** only when provenance identifies the workflow/version followed.
Three-way content alone cannot establish which tool invocation produced it. New procedure
reports use unique dated filenames and [metadata](reviews/procedure-template.md). Existing
local names and contents remain intact. Finding disposition and human review remain separate
fields in [the evidence index](ai/evidence.md).

## Preservation and verification

[The move ledger](reviews/document-moves.csv) records the original revision/hash and target
hash at migration. Five maintained documents retain their content, with only relative-link
adjustments where needed. Three archived documents are exact copies. Their relative links
are historical and explicitly excluded from the current-navigation check. The ledger records
this migration; maintained documents may subsequently evolve through reviewed changes.

No analytical R code, city configurations, source data, dependency declarations, orchestration,
artifact manifest, or resolution-sensitivity scientific guide is changed. The operational
guide receives navigation only. Design A/B/C qualifications, city-specific comparability,
Step 0–4 interpretation, and uncertainty retain their stated scope. Moving a reference does
not approve a new method.

Check harness/Markdown fixtures, local links and anchors, and the synthetic R suite. Validate
the distributed file inventory separately from ignored local evidence. Compare moved content
and scientific-file hashes, check whitespace, and confirm unchanged HEAD/index. Release
verification, external acquisition, manuscript rendering, and scientific reproduction are
separate tasks. Actual outcomes and exact recommended file groups are in the
[implementation record](ai/implementation.md#documentation-and-host-policy-continuation--21-september-2026).

## Sources and recommendation basis

| Area / source | Recommendation supported | Limit |
|---|---|---|
| AI safety: [NIST least privilege](https://csrc.nist.gov/glossary/term/least_privilege); [OWASP excessive agency](https://genai.owasp.org/llmrisk/llm062025-excessive-agency/) | Separate editing from publishing authority; enforce boundaries outside prompts | General principles do not certify a client configuration |
| AI tools: [OpenAI permissions](https://learn.chatgpt.com/docs/permissions), [managed configuration](https://learn.chatgpt.com/docs/enterprise/managed-configuration), [hooks](https://learn.chatgpt.com/docs/hooks) | Supported profiles, host requirements, explicit trust/dispatch checks | Version/platform-specific; local evidence has bounded coverage |
| Git: [official hooks reference](https://git-scm.com/docs/githooks), [configuration](https://git-scm.com/docs/git-config) | Git hooks/configuration need an independent enforcement boundary | Protecting metadata alone does not prevent pushing existing commits |
| Engineering/research: [Wilson et al. (2017), PLOS Computational Biology](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005510) | Readable organization, versioned documentation, small reviewable changes, automated checks | Does not require wholesale refactoring or a targets migration |
| Reproducible research: [Sandve et al. (2013), PLOS Computational Biology](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285); [AEA Data Editor guidance](https://aeadataeditor.github.io/aea-de-guidance/preparing-for-data-deposit) | Preserve input/code versions, intermediate evidence, access conditions, and program/output mapping | Hashes, tests, and historical parity are not independent reproduction |
| Information organization: [Diátaxis](https://diataxis.fr/start-here/) | Distinct reader routes for learning, tasks, reference, and explanation | Design framework; does not require four literal folder names |

## Assumptions, risks, and remaining questions

- The user authorized implementation after reviewing P0 and selected all Codex projects
  on this Mac. This does not authorize staging, commits, pushes, or analytical changes.
- Administrator authentication is still needed for deployment. Current hook success is not
  machine-wide or Claude evidence. Cloud/MDM requirements may override system policy.
- Nested/bare repositories and linked-worktree common directories require exact registration.
  The tested client rejects general read-access globs. See the host guide before relying on it.
- Historical audits/drafts may be absent or restricted in a clone; their omission is intentional.
- The link checker covers inline Markdown links and heading/explicit-ID anchors. It does not
  check remote availability, arbitrary HTML, PDF internals, or rendered scientific content.
- Scientific reproduction and review of earlier findings remain open. This reorganization
  neither resolves them nor invalidates their existing evidence.
