# Native harness setup and boundaries

Root AGENTS.md is the shared core every harness loads; CLAUDE.md imports it with `@AGENTS.md`. Claude rules and commands keep their discovery metadata and import the shared file (`@../../doc/ai/rules/<rule>.md`, `@doc/ai/workflows/<workflow>.md`) instead of asking the model to read it. Codex workflows are skills under .agents/skills/, not a proprietary .codex/skills directory. Codex configuration, two read-only reviewer agents, command rules and the hook adapter live under .codex/.

Project config uses workspace-write and on-request approval. Context7 and DeepWiki retain
their package pins. Discovery under CLI 0.153.0 and the missing-npx limitation are historical
observations in [the implementation record](implementation.md), not current startup evidence.
The prepared [machine policy](host-enforcement.md) disables external integrations when installed.

The shared guard policy is optional development tooling at `tools/harness/guard_policy.py`; it protects source directories, the dependency lockfile and secrets, while `.env.example` remains editable. Client adapters parse their own payloads.

Codex checks every add/update/delete/rename path in a multi-file apply_patch call and never emits the unsupported pre-tool ask decision. Dangerous shell calls are blocked in Codex; Claude
retains its prompt decision for non-Git destructive commands. Git mutations are denied in both adapters. Structural setup exceptions produce context rather than blocking ordinary authorized setup.

Shell parsing does not prove that arbitrary shell/Python/R commands cannot write a protected
input. Read-only mounts and filesystem permissions are the stronger boundary. Adapter tests
do not demonstrate client trust. On 21 September 2026, installed Codex CLI 0.154.0-alpha.6.2
reported this project hook enabled/trusted, and the live shell tool denied a harmless invalid
Git option before execution. No trust setting was changed. See [measured scope and remaining
deployment checks](host-enforcement.md). Claude live operation remains unverified.

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

## Current client review — 5 October 2026

Updated 20 October 2026: the VS Code extension and the terminal CLI both run Claude Code
2.1.304; Codex CLI is 0.155.1. (The 2.1.267 recorded on 5 October was the terminal CLI;
the extension bundles its own binary.) Direct `AGENTS.md` loading needs 2.1.277+, an enabled
built-in plugin and a user-level setting, and stops when a `CLAUDE.local.md` exists
([official memory documentation](https://code.claude.com/docs/en/memory)). `CLAUDE.md`
therefore contains only `@AGENTS.md`, which loads it in every version without duplication.
`AGENTS.md` itself uses no `@` imports, because Codex would read them as plain text.
Loading was verified on 20 October with an `InstructionsLoaded` hook passed through
`--settings` to a headless session: each rule's import appears with load reason `include`.
The imports expand at session start, so the four shared rules (about 6,800 tokens together
with `AGENTS.md`) are in context from the first turn; `paths:` now scopes only the wrapper.
A one-turn probe without tools confirmed that a command's `@` reference attaches the
workflow file. Claude Code applies a project's `permissions.allow` rules only after
the workspace trust dialog is accepted; this repository had not been trusted, so those
rules were ignored.

Written project rules are instructions, not enforced controls. Claude's native `Edit` deny
rules now include downloads as well as raw, legacy and lockfile paths. `Read` and `Edit`
path rules cover built-in file reading/editing; native `Write(path)` rules are unsupported.
[Official permission documentation](https://code.claude.com/docs/en/permissions) explains
these distinctions. Bash and external tools require separate controls. Shared hook tests
exercise policy parsing, not live Claude dispatch. Rules under `.claude/rules/` retain
supported native discovery metadata and links to canonical shared requirements.

Codex discovers hierarchical AGENTS instructions under its [official loading rules](https://developers.openai.com/codex/guides/agents-md).
Its [configuration precedence](https://developers.openai.com/codex/config-basic) means the
project's `on-request` setting does not override a managed `never` policy. This implementation
session uses managed no-escalation permissions and read-only Git metadata; project settings
remain a fallback declaration. Native command-rule evaluation and shared guard tests are
bounded evidence. No other harness-specific wrapper was found in the repository inventory;
adding a new harness still requires native loading and enforcement verification.
