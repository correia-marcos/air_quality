# Shared project guidance

This directory is the canonical source for Claude, Codex, and other assistants.
Read architecture.md at task start, then load only the relevant guidance below.
Edit the shared source once; native wrappers contain discovery metadata and links.

| Task | Read |
|---|---|
| Every task | [Architecture](architecture.md), [collaboration](collaboration.md), [Git safety](rules/git-safety.md) |
| R/QMD/Rmd changes | [R style](rules/r-style.md) |
| Data, src, scripts | [Data and paths](rules/data-and-paths.md) |
| Container/dependency changes | [Reproducibility](rules/reproducibility.md) |
| Legacy comparisons | [Validation](rules/validation.md) |
| Repeated task | The matching file in [workflows](workflows/) |
| Independent reviewer | The matching file in [roles](roles/) |
| Scientific decisions | [Evidence index](evidence.md), [methods/tests](methods-tests.md) |
| Client setup and hooks | [Harnesses](harnesses.md) |
| Host permission boundary | [Host enforcement](host-enforcement.md) |
| Human documentation and report types | [Documentation index](../README.md), [review catalog](../reviews/README.md) |
| Implementation status | [Migration evidence](implementation.md) |
| Session transition | [Handoff](handoff.md) |

A scoped rule may have documented exceptions. Existing user authorization is retained.
Changes to methods, source inputs, and dependencies require the corresponding task scope.
Implementation authorization does not authorize Git mutations; agents recommend commits only.
