# Add a coding harness

Give a new coding harness (for example Hermes) the same rules and boundaries as Codex and
Claude Code, without copying policy. The [control matrix](../harnesses.md#control-matrix)
lists every control, its shared source and the two existing implementations; the new
harness becomes another column.

Before writing, confirm (one line each, if unclear): the harness and its installed version,
its official documentation for instructions, skills or commands, subagents, hooks,
permissions and sandboxing, and whether it is for one person or the whole team.

1. **Instructions.** From the official documentation, establish how the harness loads
   project instructions. If it reads `AGENTS.md`, add nothing. Otherwise add the thinnest
   native entry file and make it import `AGENTS.md` natively; use a pointer sentence only
   when no import exists, and record that limitation.
2. **Rules and workflows.** For each file in `doc/ai/rules/` and `doc/ai/workflows/`,
   choose the native mechanism (path rule, skill, command) that delivers its content. Name
   wrappers like the existing ones. Wrappers import or link; they never copy.
3. **Reviewer roles.** If the harness supports subagents, define read-only reviewers that
   point to `doc/ai/roles/`, with editing tools withheld.
4. **Guard.** If the harness has pre-tool hooks, add `.<harness>/hooks/guard.py`. It parses
   the harness's own payload and calls `tools/harness/guard_policy.py`. Map `deny` and
   `prompt` to a denial and `context` to a note; never ask. Add the harness's payloads to
   `tests/harness/test_guard.py`.
5. **Command rules and permissions.** Deny the Git commands in
   [Git safety](../rules/git-safety.md) and the shell commands denied in
   `.claude/settings.json`. Deny reads of credential files and edits to inputs of record.
6. **Sandbox and approvals.** Enable the harness's OS sandbox with workspace writes,
   read-only `.git`, no network and no escalation. Prefer refusing to asking, following the
   refuse-and-explain rule in `AGENTS.md`.
7. **Record.** Add the harness as a column of the control matrix, mark the rows it cannot
   support, and record its version and the date.
8. **Verify.** Run the [verification](../harnesses.md#verification) steps: instruction
   loading, adapter tests, rule evaluation, the sandbox probe and the probe table. Never
   test forbidden Git commands against real history or a remote.

Do not install the harness or change user or machine configuration; give Marcos those
commands. Do not weaken an existing control to make the new harness work.

Authorization already given in the conversation satisfies a workflow confirmation.
Do not repeat it. State checks actually run, failures, skips, and unresolved evidence.

## Execution contract

Inputs: the harness, its version and official documentation, and the current revision.
Permitted actions: those described above within the user-authorized scope; routine reversible work proceeds independently.
Required evidence: identify files inspected/changed, commands actually run, their outcomes, and the matrix rows the harness cannot support.
Completion: satisfy the steps above and leave a concise factual handoff with recommended commit groups.
