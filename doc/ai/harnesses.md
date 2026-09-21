# Native harness setup and boundaries

Root AGENTS.md and CLAUDE.md route to the same doc/ai/ sources. Claude commands, rules and role files retain discovery metadata. Codex workflows are skills under .agents/skills/, not a proprietary .codex/skills directory. Codex configuration, two read-only reviewer agents, command rules and the hook adapter live under .codex/.

Project config uses workspace-write and on-request approval. Context7 and DeepWiki preserve the existing npx package pins. `codex mcp list` discovers both in installed Codex CLI 0.153.0. This machine currently has no npx on PATH, so startup is not verified. Install/use the project's existing Node tooling before treating either server as available; do not change package pins as a workaround.

The shared guard policy is optional development tooling at `tools/harness/guard_policy.py`; it protects source directories, the dependency lockfile and secrets, while `.env.example` remains editable. Client adapters parse their own payloads.

Codex checks every add/update/delete/rename path in a multi-file apply_patch call and never emits the unsupported pre-tool ask decision. Dangerous shell calls are blocked in Codex; Claude
retains its prompt decision for non-Git destructive commands. Git mutations are denied in both adapters. Structural setup exceptions produce context rather than blocking ordinary authorized setup.

Shell parsing is supplementary and does not prove that arbitrary shell/Python/R commands cannot write a protected input. Read-only mounts and filesystem permissions are the stronger boundary. Tests exercise adapters; they cannot demonstrate trust in an interactive client's hook system. Review/enable the hooks through the installed client's hook interface and test an innocuous protected-path attempt before describing protection as active. No trust setting was silently granted by this migration.

## Git boundary

Every client follows [Git safety](rules/git-safety.md): agents inspect Git and recommend
human-created commits only. Neither implementation approval nor a passing test authorizes
staging, committing, or pushing. Never request a broad Git escalation.

Codex rules forbid plain staging, commit, push, and common history-producing commands.
Claude settings deny the corresponding commands rather than asking permission. The shared
hook additionally recognizes common literal invocations with path options, absolute Git
paths, compounds, and shell -c wrappers. It denies unknown Git commands/aliases and config
overrides, and rejects direct edits to the checkout's .git metadata. To inspect Git config,
use an explicit read form such as `git config --get core.hooksPath`.

This is bounded text inspection: arbitrary scripts, Git libraries, dynamic expansion,
backticks, remote tools, and alternative tool payloads are not comprehensively covered.
Command rules also depend on client matching and trust. Do not advertise these files as a
complete cross-harness security boundary.

For hard enforcement, the host must make the actual Git directory and common directory
read-only to the agent, including linked-worktree targets, and withhold write credentials
and publishing tools. Policies must be outside the agent's writable scope. Protecting Git
metadata alone does not prevent pushing already-existing commits. Machine-level deployment
and live-client verification remain separate from repository implementation.

## Verification before enabling an implementation session

1. Record the installed client/version and effective policy sources. Inspect hook trust
   through the client; do not bypass it. Check every enabled execution/publishing tool.
2. Run `python3 -B -m unittest discover -s tests/harness`. Tests pass forbidden commands
   as strings to policy/adapters; they never execute those commands.
3. Check native Codex rules with `codex execpolicy check --rules
   .codex/rules/project.rules -- <command tokens>`. This evaluates rules without executing
   the command. Plain commit/push must be forbidden and Git status must remain unmatched.
4. In an isolated harness fixture with a mocked execution sink, verify denial before
   dispatch, allowed inspection, and behavior on malformed payloads, hook failure, disabled
   hooks, and subdirectory invocation. Do not test against a live remote or real project
   history. Check the host boundary independently of the hook.
5. Record actual outcomes and outstanding coverage. Adapter tests and rule evaluation do
   not prove that an interactive client invoked the hooks.

Use reviewer roles for explicitly requested independent work. Do not automatically split small tasks. Updating a scientific workflow means editing doc/ai/workflows once; wrappers should remain thin links. Sources: [Codex skills](https://developers.openai.com/codex/skills/), [configuration](https://developers.openai.com/codex/config-reference/), [hooks](https://developers.openai.com/codex/hooks/).
