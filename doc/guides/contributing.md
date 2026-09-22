# Contributing a bounded change

Read the [architecture](../ai/architecture.md), [collaboration agreement](../ai/collaboration.md),
and [Git boundary](../ai/rules/git-safety.md). Humans own scientific specifications and
perform Git mutations; agents inspect, edit within scope, test, and recommend commit groups.

## Before editing

Identify the intended behavior, affected files, input/output contracts, and a check that
would detect a mistake. Preserve existing user changes. Separate implementation defects
from unresolved scientific choices. Use researcher-approved specifications for changes to
estimands, geography, census vintage, samples, weights, missingness, or numerical tolerances.
Existing authorization remains valid; clarify only unresolved consequential choices.

Use the [shared index](../ai/README.md) to load relevant rules. R changes require the
[R style](../ai/rules/r-style.md); data/pipeline work also requires
[data and paths](../ai/rules/data-and-paths.md). Environment changes require
[reproducibility guidance](../ai/rules/reproducibility.md).

## Implement and verify

Keep reusable analysis in R under `src/`, and execution in `scripts/`. Follow the existing
registry for new cities. Keep both maintained pipeline entry points consistent when a
stage changes. Consult [dated remaining work](../planning/remaining-work.md) before treating
a missing artifact as a defect; the [manifest](../../config/paper_artifacts.csv) defines
selected manuscript paths.

Run relevant checks and the portable suite:

```sh
python3 -B -m unittest discover -s tests/harness
python3 -B tools/docs/check_links.py
Rscript tests/testthat.R --mode=synthetic
git diff --check
```

The Python harness checks are relevant to harness changes; the link check to documentation.
Use [isolated verification](../HOW_TO_RUN.md#isolated-batch-verification) when the scope
requires fresh analytical execution. Report failures and skipped checks separately.

## Hand off

Use the [handoff template](../ai/handoff.md). Explain what changed, why, what ran, and what
remains uncertain. Provide commit subjects and explicit file groupings only. Do not stage,
commit, push, or request an exception through tool escalation. Reviewed scientific decisions
belong in the [existing evidence index](../ai/evidence.md), not a competing ledger.
