# Native harness setup and boundaries

Root AGENTS.md and CLAUDE.md route to the same doc/ai/ sources. Claude commands, rules and role files retain discovery metadata. Codex workflows are skills under .agents/skills/, not a proprietary .codex/skills directory. Codex configuration, two read-only reviewer agents, command rules and the hook adapter live under .codex/.

Project config uses workspace-write and on-request approval. Context7 and DeepWiki preserve the existing npx package pins. `codex mcp list` discovers both in installed Codex CLI 0.153.0. This machine currently has no npx on PATH, so startup is not verified. Install/use the project's existing Node tooling before treating either server as available; do not change package pins as a workaround.

The shared guard policy is optional development tooling at
`tools/harness/guard_policy.py`; it protects source directories, the dependency lockfile and
secrets, while `.env.example` remains editable. Client adapters parse their own payloads.
Codex checks every add/update/delete/rename path in a multi-file apply_patch call and never
emits the unsupported pre-tool ask decision. Dangerous shell calls are blocked in Codex; Claude
retains its prompt decision. Structural setup exceptions produce context rather than blocking
ordinary authorized setup.

Shell parsing is supplementary and does not prove that arbitrary shell/Python/R commands cannot write a protected input. Read-only mounts and filesystem permissions are the stronger boundary. Tests exercise adapters; they cannot demonstrate trust in an interactive client's hook system. Review/enable the hooks through the installed client's hook interface and test an innocuous protected-path attempt before describing protection as active. No trust setting was silently granted by this migration.

Use reviewer roles for explicitly requested independent work. Do not automatically split small tasks. Updating a scientific workflow means editing doc/ai/workflows once; wrappers should remain thin links. Sources: [Codex skills](https://developers.openai.com/codex/skills/), [configuration](https://developers.openai.com/codex/config-reference/), [hooks](https://developers.openai.com/codex/hooks/).
