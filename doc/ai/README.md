# Agent guidance

Agents start at [AGENTS.md](../../AGENTS.md), the shared core every harness loads; Claude
Code reads it through `CLAUDE.md`. This folder holds the detail that file routes to.

| Folder or file | Contents |
|---|---|
| [rules/](rules/) | R style, data and paths, reproducibility, validation, Git safety |
| [workflows/](workflows/) | Repeated tasks; each harness wraps them as commands or skills |
| [roles/](roles/) | Independent read-only reviewers |
| [architecture.md](architecture.md) | Repository structure, migration status, validation steps |
| [harnesses.md](harnesses.md) | Harness controls, setup and verification |
| [evidence.md](evidence.md), [methods-tests.md](methods-tests.md) | Scientific decisions and their tests |
| [collaboration.md](collaboration.md) | Working agreement and handoff template |

Edit each rule once, here. Harness wrappers import or link it; they never copy it.
