# Project instructions

Read `doc/ai/README.md` and `doc/ai/architecture.md` at task start. Explicitly load
relevant shared rules and workflows from that index; never assume wrapper metadata
loaded their contents. Shared guidance is maintained only in doc/ai/.

Preserve scientific specifications unless their change is explicitly authorized.
Protect original data/raw/, data/downloads/, data/_legacy/, renv.lock, and credentials.
Do not print credentials. src/ defines reusable logic; scripts/ executes it, with
setup exceptions documented in the shared R style. Analysis remains R.

Proceed with routine authorized work; clarify consequential scientific ambiguity.
Report actual checks, failures, skips, and limitations. A test pass is not a complete
reproduction claim. Use specialist agents only for requested independent work.

Run `Rscript tests/testthat.R --mode=synthetic` for portable checks; release evidence
uses `Rscript scripts/verification/verify_release.R`. See doc/REPRODUCIBILITY.md.
