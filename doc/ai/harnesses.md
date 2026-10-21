# Harness setup and controls

Every coding agent follows [AGENTS.md](../../AGENTS.md). This page lists each control the
repository relies on, where its shared source lives, how Codex and Claude Code implement it,
and how to check it. To add another harness, such as Hermes, follow
[Add a coding harness](workflows/add-harness.md) and add a column here.

Written instructions are guidance; hooks, command rules, sandboxes and file permissions are
controls. A passing adapter test shows that the policy parses a payload, not that a live
client invoked it.

## Control matrix

| Control | Shared source | Codex | Claude Code | Check |
|---|---|---|---|---|
| Entry instructions | `AGENTS.md` | Loads `AGENTS.md` natively (32 KiB cap; no imports) | `CLAUDE.md` contains `@AGENTS.md` | Ask for a verbatim line without tools |
| Detailed rules | `doc/ai/rules/` | Routing table in `AGENTS.md`; read on demand | `.claude/rules/*.md` import the shared file; loaded at session start | `InstructionsLoaded` hook (below) |
| Workflows | `doc/ai/workflows/` | `.agents/skills/<name>/SKILL.md` links the workflow | `.claude/commands/<name>.md` attaches it with `@` | One-turn quote probe |
| Reviewer roles | `doc/ai/roles/` | `.codex/agents/*.toml`, `sandbox_mode = "read-only"` | `.claude/agents/*.md`, tools `Read, Grep, Glob, Bash`; body points to the role | Role reports without editing |
| Pre-tool guard | `tools/harness/guard_policy.py` | `.codex/hooks.json` → `.codex/hooks/guard.py` | `.claude/settings.json` hooks → `.claude/hooks/guard.py` | `tests/harness`; probe `git --idb-hook-probe` |
| Git command rules | [Git safety](rules/git-safety.md) | `.codex/rules/project.rules` (`forbidden`) | `permissions.deny` `Bash(git …)` entries | `codex execpolicy check`; `/permissions` |
| Other shell rules | `AGENTS.md` refuse-and-explain | Hook denies `rm -rf`; sandbox blocks the rest | Deny `rm`, `rmdir`, `mv`, `chmod`, `chown`, `docker` | Probe table |
| Inputs of record, credentials | [Data and paths](rules/data-and-paths.md), [reproducibility](rules/reproducibility.md) | Hook denies edits; managed policy denies credential reads | `Read`/`Edit` deny rules, also applied to sandboxed writes; sandbox `credentials` | Probe table |
| Sandbox and network | — | Managed profile `research_no_publish`: workspace writes, read-only `.git`, `.codex`, `.agents`, no network | Seatbelt sandbox, `.git` write-denied, no allowed domains, no unsandboxed retry | `touch -c .git/description` fails |
| Approval behaviour | Refuse and explain | `approval_policy = "never"` (managed) | `defaultMode: "dontAsk"` for CLI sessions; VS Code panel in Auto | Probe table |
| Machine policy | `tools/harness/managed-requirements.toml` | Installed at `/etc/codex/requirements.toml` (22 Sep 2026) | None (deferred) | [Codex machine policy](#codex-machine-policy) |
| MCP servers | — | `.codex/config.toml`; disabled by the managed policy | `.mcp.json`; tools allowed as `mcp__context7__*`, `mcp__deepwiki__*` | `/mcp` |
| Memory | `doc/ai/evidence.md` for durable decisions | `~/.codex/memories` (user level) | `~/.claude/projects/<repo>/memory` (user level) | Not shared between harnesses |

Each wrapper imports or links its shared file and never copies it, so a rule or workflow is
edited once in `doc/ai/`. `AGENTS.md` uses no `@` imports because Codex would read them as
plain text.

## Codex

Machine requirements outrank the project's `.codex/config.toml` (`on-request`,
`workspace-write`): the only permitted approval policy is `never`. Codex therefore asks
less and refuses more. Anything beyond the boundary fails inside the sandbox or is refused
by a rule or hook. The project hook runs alongside a root-owned managed Git hook. Versions
on 21 October 2026: Codex CLI 0.155.1.

### Codex machine policy

`tools/harness/managed-requirements.toml` is the source of the root-owned policy installed
at `/etc/codex/requirements.toml` on 22 September 2026, for every Codex project on this Mac.

- **Profile.** It permits one permission profile, `research_no_publish`: workspace
  editing; read-only Git metadata, `.codex` and `.agents`; no sandbox escalation; no
  command network access; credential paths denied; approval policy `never`.
- **Git guard.** A host-owned Git hook (`tools/harness/managed_git_guard.py`) reuses the
  repository's command policy, and native rules forbid staging, commit, push and history
  mutations.
- **Integrations.** MCP servers, apps, plugins, and browser or computer use are disabled,
  because they can publish without Git; web search stays available. Package downloads,
  online acquisition, Docker and external integrations therefore run outside Codex.
- **Reads.** The profile keeps broad read access for research tools and external legacy
  inputs, so it is not a complete secret-discovery policy.

The installer registers this checkout's actual and common Git directories as exact read
paths, because the client rejects read-path globs. Before using a parent directory, linked
worktree, submodule or bare repository as a workspace, register its Git and common
directories in the administrator-owned profile and repeat the check.

To install on another Mac:

1. Review the bundle, then run `sudo sh tools/harness/install_managed_policy.sh` in your
   own Terminal. It refuses to overwrite an existing policy. Never give the administrator
   password to an agent.
2. Restart the Codex clients. Inspect `configRequirements/read`, `config/read` and
   `hooks/list` through the app-server API. Confirm the managed profile, the approval
   policy, the feature pins, the empty MCP list, root-owned hook files and the Git
   directory restrictions. Higher-precedence cloud or MDM policy may alter the result.
3. In a normal Terminal (Python 3.11+), run
   `python3 -B tools/harness/verify_host_profile.py`. It exercises the profile against
   disposable fixture paths and invokes no model, Git mutation or remote service.

Re-check after every Codex upgrade. To roll back, an administrator reviews the installed
files' ownership and contents, removes only those files, and restarts the client. Agents
never remove or weaken the policy. Sources:
[permission profiles](https://learn.chatgpt.com/docs/permissions),
[managed configuration](https://learn.chatgpt.com/docs/enterprise/managed-configuration).
The September deployment record is in the
[implementation record](../planning/implementation-record.md#machine-enforcement-record).

## Claude Code

Versions on 21 October 2026: the VS Code extension and the terminal CLI both run 2.1.304.
The extension bundles its own binary, so `/status` in the panel reports the version it runs.

- **Instructions.** `CLAUDE.md` imports `AGENTS.md`, which loads in every version and never
  twice. Direct `AGENTS.md` loading needs 2.1.277+, the built-in plugin and a user-level
  setting, and stops when a `CLAUDE.local.md` exists. Rule imports expand at session start:
  the four shared rules and `AGENTS.md` (about 6,800 tokens) are in context from the first
  turn, and `paths:` scopes only the wrapper.
- **Trust.** Project `permissions.allow` rules apply only after the workspace trust dialog
  has been accepted once in an interactive terminal session. This repository was trusted on
  21 October 2026; before that its allow rules were ignored and every command prompted.
- **Sandbox.** Shell commands run in macOS Seatbelt with no unsandboxed retry, `.git`
  write-denied, credential files hidden, no network domains and the `Edit` deny paths
  write-denied. `includeGitInstructions: false` removes Claude Code's built-in commit
  guidance. `RENV_CONFIG_SANDBOX_ENABLED=FALSE` is set for agent commands: renv's own
  sandbox writes into the user cache, which the Bash sandbox blocks, and project R
  startup stalled. On this machine R's system library holds only the 29 base and
  recommended packages, so this changes no package visibility. Elsewhere the isolated
  Docker verification remains authoritative. On native Windows there is no sandbox, and
  commands run under the same permission rules.
- **Permission mode.** CLI sessions start in `dontAsk` from the project settings. The VS
  Code extension never reads project settings for its starting mode. It uses
  `claudeCode.initialPermissionMode`, then the Manual, Edit automatically or Auto mode
  last picked in its indicator, then `defaultMode` in `~/.claude/settings.json`. The
  panel cannot select `dontAsk`, so on 21 October Marcos chose **Auto**. Under the
  sandbox and deny rules, Auto still denies Git mutations, deletion, `mv` and Docker, and
  refuses connections to unlisted hosts. Unlike `dontAsk`, its classifier may pause on an
  action it judges risky. Keep `defaultMode` out of `~/.claude/settings.json`, where it
  would govern terminal sessions in every project. VS Code's terminal mode
  (`claudeCode.useTerminal`) has not been tested.
- **Measured on 21 October 2026.** A sandboxed `touch -c .git/description` failed with
  `Operation not permitted` while `git status` worked. The synthetic suite passed inside
  the sandbox (1,042 tests, 65 s); arrow logs four harmless `sysctlbyname` warnings.

## Git boundary

Every client follows [Git safety](rules/git-safety.md): agents inspect Git and recommend
human-created commits only. Neither implementation approval nor a passing test authorizes
staging, committing, or pushing. Never request a broad Git escalation.

The shared hook recognizes common literal invocations with path options, absolute Git
paths, compounds, and `shell -c` wrappers. It denies unknown Git commands, aliases and
config overrides, and rejects direct edits to `.git`. To inspect Git config, use an explicit
read form such as `git config --get core.hooksPath`. This is bounded text inspection:
arbitrary scripts, Git libraries, dynamic expansion, remote tools and other tool payloads
are not covered. The sandboxes' read-only `.git` is the stronger boundary, and it still
does not stop pushing commits that already exist. Withhold write credentials and publishing
tools, and keep policies outside the agent's writable scope.

## Verification

Run these after changing a harness, its configuration or its client version. None of them
executes a forbidden Git command against real history.

1. Record the client, version and effective policy sources (`/status`, `/permissions`,
   `codex` configuration inspection). Inspect hook trust through the client.
2. `python3 -B -m unittest discover -s tests/harness`: the policy and both adapters,
   with forbidden commands passed as strings only.
3. `codex execpolicy check --rules .codex/rules/project.rules -- git commit -m x`:
   evaluates without executing. Mutations must be `forbidden` and `git status` unmatched.
   In zsh, split a command held in a variable with `${=cmd}`.
4. Instruction loading in Claude: run a headless session with an `InstructionsLoaded` hook
   passed through `--settings` (for example
   `{"hooks":{"InstructionsLoaded":[{"hooks":[{"type":"command","command":"cat >> $LOG"}]}]}}`)
   and read a matching file. Imports appear with load reason `include`. For commands, a
   one-turn probe without tools that asks for a verbatim workflow line shows the
   attachment.
5. Sandbox: `touch -c .git/description` must fail; `git status` must work.
6. Probe table, in a fresh session per harness: record whether reads, `git status`, the
   synthetic tests, a workspace edit, `git add --dry-run .`, `git --idb-hook-probe`, a
   network request and `rm` of a scratch file run, are refused, or prompt.

Record actual outcomes, dates and versions. Earlier client reviews (21 September and
5 October 2026) are in the [implementation record](../planning/implementation-record.md).

Sources: [Codex skills](https://developers.openai.com/codex/skills/),
[configuration](https://developers.openai.com/codex/config-reference/),
[hooks](https://developers.openai.com/codex/hooks/),
[Claude memory](https://code.claude.com/docs/en/memory),
[permissions](https://code.claude.com/docs/en/permissions),
[permission modes](https://code.claude.com/docs/en/permission-modes),
[sandboxing](https://code.claude.com/docs/en/sandboxing).
