# Git: recommendations only

Agents may inspect Git state and recommend commit subjects, descriptions, and exact
file groupings. Humans stage, commit, and push outside the agent session.

Agents must not stage changes, create or amend commits, or push changes. Do not use
scripts, libraries, aliases, other tools, or delegated agents to perform these actions.
This includes history-producing operations such as merge, rebase, cherry-pick, and am.
Git use by agents is limited to inspection; leave the index and history unchanged.

Implementation approval, successful tests, approval of a commit plan, and instructions
to use reviewable batches do not authorize Git mutations. Do not request an escalation
or a broad Git permission to bypass this boundary. Do not weaken active controls to
complete a task. A prohibition is not a request for confirmation.

At handoff, report the working-tree changes, checks actually run, unresolved limitations,
and proposed commit subjects with explicit file lists. Recommend groups only; do not
stage them. Separate policy, infrastructure, documentation, and analytical changes when
they are independently reviewable.

## Enforcement and limits

Native command denials and the shared hook supplement this instruction. The hook checks
common literal Git invocations, global path options, shell compounds, and shell -c
wrappers. Unknown Git commands/aliases and configuration overrides are denied because
they can hide mutation. It does not interpret arbitrary scripts, dynamic shell expansion,
Git libraries, or remote tools, and it cannot protect a session where it is inactive.

Hard enforcement requires controls outside the agent's writable scope: protect the actual
Git directory and common directory (including linked worktrees), withhold write credentials
and publishing tools, and verify the installed harness. Read-only Git metadata alone does
not stop a push. Repository-local tests do not establish live client enforcement.
See [harness setup and verification](../harnesses.md).
