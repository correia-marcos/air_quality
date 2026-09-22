# First run: inspect a small example

This route introduces the repository without acquiring city data. It uses the existing
synthetic tests and their hand-worked IDW example; it does not reproduce the manuscript.

1. Read the [repository map](../ai/architecture.md#repository-layout). Functions live in
   `src/`; entry-point scripts live in `scripts/`. Open a function and its calling script
   side by side to see where inputs become outputs.
2. Use the declared R environment. Follow [environment setup](../HOW_TO_RUN.md#development-and-acquisition)
   if it is not available. Do not install replacement versions merely to make a test pass.
3. From the repository root, run:

   ```sh
   Rscript tests/testthat.R --mode=synthetic
   ```

   Read the final counts and exit status. Missing packages or startup failures mean the
   check did not complete. The synthetic suite requires its declared dependencies, but
   does not require the full city source datasets.
4. Work through the [IDW toy example](../reference/idw_golden_test.md), then open
   [its test](../../tests/testthat/test-idw-exposure-golden.R). Check a missing station
   reading and a zero-distance pair against the hand calculation. This connects one
   mathematical claim to an executable check.
5. Consult the [data dictionary](../reference/data_dictionary.md) before interpreting
   real interim/processed tables. When authorized city inputs are available, follow
   the [run guide](../HOW_TO_RUN.md) and inspect stage outputs in RStudio. Do not infer
   that a historical intermediate proves fresh source-to-output execution.

Next: [review coverage and its limits](../ai/methods-tests.md), or
[contribute a bounded change](contributing.md).
