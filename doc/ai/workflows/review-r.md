# Review R

First acceptance criterion: follow the reader-first requirement in
[architecture](../architecture.md#the-reader-is-a-human-not-just-a-machine) and
[R style](../rules/r-style.md). The reader can run a few lines in RStudio, inspect
named intermediate objects, and locate the applied function without learning targets.
Use three or four meaningful executable sections; do not hide the analysis in one call.


Review the requested path or staged diff (a path, or the staged diff if "staged"/empty) against project standards. This
is a read-and-report pass — propose fixes, don't apply them until I approve.

Check, and cite line numbers:

- **Style:** follow the author's spacing, call layout and step comments in
  [R style](../rules/r-style.md#spacing-calls-and-comments), rather than formatter defaults;
  enforce the 90-character maximum, including comments and banners; header block present
  and correctly formatted; numbered `# ===` sections.
  **Comments** per `doc/ai/rules/r-style.md` — flag any in-body comment block over
  2 lines, and any rationale duplicated between the body and `@Description` / `@details`.
- **Paths:** every path via `here::here()`; no absolute paths, no `setwd()`.
- **Setup:** Section I sources the required scientific definitions and plain settings,
  shows input reads, and attaches only necessary installed packages. It does not load
  the targets graph or install dependencies. New dependencies require reviewed records.
- **Data discipline:** reads/writes hit the correct layer; nothing writes `data/raw` or
  `data/_legacy`; `tables_images/` reads only from `data/processed`/`interim`; intermediates are
  Parquet.
- **Reproducibility:** `set.seed()` where there's randomness; `src/` has no top-level side-effects.
- **Reader-first:** could a referee follow the data through this? Flag deep pipe chains or hidden
  intermediate shapes, positional argument clusters, and pack/unpack adapters. Section II
  computes named objects; Section III saves them in the same order. Check the actual
  defining function and use the distance recipe as the structural reference.
- **Anti-bloat:** unrequested abstraction/flexibility, error handling for impossible cases,
  200-lines-that-should-be-50. Call these out.

End with a short TL;DR table: issue | location | severity | suggested fix. For requested independent audits, use the shared R reviewer role.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.


## Execution contract

Inputs: the requested task specification, relevant source/data contracts, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and unresolved scientific decisions.
Completion: satisfy the workflow-specific conditions above and leave a concise factual handoff. A report-only audit does not authorize implementation.
