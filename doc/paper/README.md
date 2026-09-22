# doc/paper/ — local manuscript draft (NOT committed)

This directory holds local paper sections used by the
[procedure-audit workflow](../ai/workflows/audit-procedure.md). Only this README is tracked.
The draft is not distributed with the code repository or included in the Docker build.

The researcher can manually export the relevant TeX from Overleaf, or synchronize a separate
manuscript checkout and copy the required sections here. Do not clone into `doc/paper/`:
it already contains this README. Record the draft version and hashes used by each audit;
refreshing a draft does not update the evidence in an older report. Keep authentication
tokens out of commands, reports, and tracked files. Agents do not stage, commit, or push.

See [the human audit guide](../guides/procedure-audit.md) for access and reporting steps.
[Remaining work](../planning/remaining-work.md) was assessed against the draft available
at its recorded date; verify the draft and code revision before relying on its counts.
