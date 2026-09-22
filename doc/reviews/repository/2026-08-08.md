# Repo review: structure and style against the scientific-computing literature

An external-style assessment of this repository's organization and code style,
benchmarked against the canonical reproducible-research literature. Requested by
Marcos; prepared by Claude Code (kimi-k3) on 2026-08-08. Read-only review — no code
was changed. Facts asserted about the repo were verified the same day (e.g. no
`tests/`, no `.github/workflows`, no testthat in `DESCRIPTION`).

**Verdict:** the repo is a research compendium in the sense of Marwick, Boettiger &
Mullen (2018) and satisfies all three of its principles; several of its conventions
(named scripts, the enforced read-only ratchet, the Step 0–4 validation track) are at
or beyond the current literature. The clearest gaps, by the same literature, are the
absence of an automated test suite and the dual source of truth for the pipeline DAG.

## 1. Benchmarks used

| Reference | What it contributes here |
|---|---|
| Wilson et al. 2014, *Best Practices for Scientific Computing*, PLOS Biology | The eight practices; esp. #1 (write for people), #5 (plan for mistakes), #7 (document design, not mechanics) |
| Wilson et al. 2017, *Good Enough Practices in Scientific Computing*, PLOS Comput Biol | Box 4 project layout (`doc/`, `data/`, `results/`, `src/`; README/LICENSE; avoid sequential-number filenames) |
| Marwick, Boettiger & Mullen 2018, *Packaging Data Analytical Work Reproducibly Using R (and Friends)*, The American Statistician | The research-compendium principles: community conventions; separation of data, method, output; data read-only |
| Sandve et al. 2013, *Ten Simple Rules for Reproducible Computational Research*, PLOS Comput Biol | Provenance; record intermediate results |
| Boettiger 2015, *An introduction to Docker for reproducible research*, ACM SIGOPS OSR | Containerized bit-for-bit environments |
| Landau 2021, *The targets R package*, JOSS | R-native pipeline DAG; up-to-date targets as "tangible evidence of reproducibility" |
| AEA Data and Code Availability Policy (+ DCAS) | Economics replication-package baseline: master script, README mapping programs to outputs, trusted-repository archiving |

## 2. Where the repo is exemplary — textbook or better

- **Layout.** `src/` (functions only) / `scripts/` (execution) / `data/` (layered) /
  `results/` / `doc/` is exactly Wilson 2017 Box 4 and the Marwick compendium
  principles. All three compendium principles hold; most published compendia manage
  two.
- **The read-only ratchet is enforced, not just recommended.** Marwick's "treat data
  as read-only" is stated in the literature as *discipline*; here a PreToolUse hook
  mechanically stops writes to `data/raw/` and `data/_legacy/`. The literature's
  consistent finding is that discipline-only norms fail under time pressure —
  mechanization is the known fix, and this repo applies it.
- **Named scripts + `run_pipeline.R` instead of numbered scripts.** Wilson 2017 says
  avoid sequential-number filenames because they desynchronize; the economics norm
  (`01_`, `02_`, …) ignores this. Meaningful names + one orchestrator holding the run
  order is the correct reading of that advice.
- **Comment policy.** Wilson 2014 #7 ("document design and purpose, not mechanics")
  is operationalized as a hard rule: rationale lives once in `@Description`/`@Details`;
  body comments are ≤ 2 lines and say what happens *to the data*. Rarely seen this
  strictly enforced outside of style-guide exemplars.
- **Environment.** renv + Docker + `.Rproj` is the Boettiger container pattern with
  pinned packages — the bit-for-bit tier, above the "good enough" tier. Meets the AEA
  baseline (master script, README, program→output mapping, Zenodo archiving of data).
- **Parquet checkpoints.** Sandve's "record intermediate results" rule, implemented
  with an out-of-core stack (Arrow/DuckDB) so any stage's output can be opened and
  inspected without rerunning anything upstream.
- **"The reader is a human" as the top constraint.** This is Wilson 2014 #1 verbatim.
  Every idiosyncrasy (named scripts, one-home comments, inspectable intermediates)
  derives from that single principle — which is why the structure feels coherent.

## 3. Where the repo goes beyond the literature

- **The Step 0–4 validation track.** Differential testing against a legacy reference
  implementation, decomposed by input layer (metro definitions → station data → census
  → code), has no real analogue in the reproducibility literature. The closest concepts
  are regression testing and "turn bugs into test cases" (Wilson 2014 #5), but those
  catch *future* bugs in *your* code; Step 0–4 quantifies *which input update moves
  which published number*. That is closer to a lab notebook than to a replication
  package, and it is what makes per-procedure audits (`doc/audits/`) cheap to run.
- **The audit trail.** `doc/audits/`, `REMAINING_WORK.md`, and
  `deletion_candidates.md` record negative results and pending decisions — the Turing
  Way's "record what didn't work" norm, again usually aspirational rather than actual.
- **The `.claude/` harness.** Rules, agents, skills, and the procedure-audit workflow
  codify reviewer discipline for AI-assisted work. The literature has not caught up
  with this yet; it is a genuine strength, with one caveat — it concentrates process
  knowledge in a single assistant's configuration. Mitigation is already in place:
  the same rules are written as plain markdown that humans can read without any tool.

## 4. Where the literature would push back

1. **No automated tests.** Wilson 2014 #5 ("plan for mistakes": assertions, unit
   tests, bugs→test cases) is the clearest gap — verified: no `tests/`, no testthat,
   no CI workflow. The validation track is a *system-level* substitute but cannot
   catch a regression introduced today in, say, `assign_socio_group()`. The
   literature distinguishes defensive input validation (rightly banned here as bloat)
   from correctness tests of scientific logic (prescribed). Minimal move: a small
   testthat suite of golden-value regression tests on tiny fixtures — e.g. a
   3-station × 2-geo-unit IDW toy whose hourly and annual answers are computable by
   hand. That is the cheapest referee-confidence available.
2. **Two sources of truth for the DAG.** Run order lives in `run_pipeline.R`,
   dependencies in the `Makefile`; they can drift. The R-native fix is `targets`
   (Landau 2021), where the dependency graph *is* the code and freshness checks are
   the reproducibility evidence. `doc/TARGETS_MIGRATION_PLAN.md` already exists; the
   literature strongly endorses finishing it.
3. **Long orchestrator functions.** `aggregate_idw_exposure()` (~680 lines) violates
   the classic modularization rule (Wilson 2014 #4). Defensible for a referee-facing
   compendium — linear step-through is the point — but it compounds gap #1: long
   functions are exactly what unit tests cannot reach. The right middle path is the
   one already started: extract pure data-transform helpers (`assign_socio_group`),
   keep the I/O orchestration linear.
4. **Style pluralism (nit).** data.table + dplyr and `%>%` + `|>` coexist.
   Style-guide literature values consistency above the specific choice; within-stage
   consistency, which the repo has, is what a reader actually needs. Nit-level only.

## 5. TL;DR

| Dimension | Literature benchmark | This repo |
|---|---|---|
| Layout | Wilson 2017 Box 4 / Marwick compendium | Matches, plus named-scripts improvement |
| Raw-data integrity | "read-only" discipline | Mechanically enforced (hook) |
| Environment | renv/Docker (Boettiger 2015) | Full bit-for-bit tier |
| Orchestration | One authoritative DAG | ⚠️ run_pipeline.R + Makefile can drift → finish targets migration |
| Tests | Wilson 2014 #5 | ⚠️ None — add golden-value regression tests |
| Validation | — (no real benchmark exists) | Step 0–4 differential framework — beyond the literature |

## Sources

- Wilson G, Bryan J, Cranston K, Kitzes J, Nederbragt L, Teal TK (2017).
  [Good enough practices in scientific computing](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005510).
  PLOS Comput Biol 13(6): e1005510.
- Wilson G, Aruliah DA, Brown CT, et al. (2014).
  [Best practices for scientific computing](https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1001745).
  PLOS Biology 12(1): e1001745.
- Marwick B, Boettiger C, Mullen L (2018).
  [Packaging data analytical work reproducibly using R (and friends)](https://www.tandfonline.com/doi/abs/10.1080/00031305.2017.1375986).
  The American Statistician 72(1): 80–88.
  ([free PDF](https://faculty.washington.edu/bmarwick/PDFs/Marwick-Boettiger-Mullen-2018-TAS-research-compendia.pdf))
- Sandve GK, Nekrutenko A, Taylor J, Hovig E (2013).
  [Ten simple rules for reproducible computational research](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285).
  PLOS Comput Biol 9(10): e1003285.
- Boettiger C (2015).
  [An introduction to Docker for reproducible research](https://doi.org/10.1145/2723872.2723882).
  ACM SIGOPS Operating Systems Review 49(1): 71–79.
- Landau W (2021).
  [The targets R package: a dynamic Make-like function-oriented pipeline toolkit for
  reproducibility and high-performance computing](https://joss.theoj.org/papers/10.21105/joss.02959.pdf).
  JOSS 6(57): 2959. [User manual](https://books.ropensci.org/targets/index.html).
- [AEA Data and Code Availability Policy](https://www.aeaweb.org/journals/data/data-code-policy)
  and [AEA Data Editor guidance](https://aeadataeditor.github.io/aea-de-guidance/).
