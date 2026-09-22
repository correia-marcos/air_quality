# Machine enforcement and live-client evidence

Status recorded on 21 September 2026 for Codex CLI `0.154.0-alpha.6.2` on macOS.
The user selected **every Codex project on this Mac** as the deployment scope.

## What is active

The installed app server's `hooks/list` returned the project's `preToolUse` hook as
enabled and trusted, matching `^(Bash|apply_patch)$`. Its current trust hash was
`sha256:3c1ff62b2892b99bcb1b4c1c08431bb5678c1c5c5e1212e8aaaef1e2990cb697`.
No hook trust or activation setting was changed.

A live shell-tool request containing `git --idb-hook-probe` was denied by PreToolUse
before execution with the inspection-only policy reason. This is a harmless invalid Git
option even if a client misses the hook. Ordinary Git inspection succeeded. No commit,
push, or staging command was executed. This verifies this tool path and client state;
it does not establish every tool's behavior or protection after an upgrade/config change.

The current session declares `.git`, `.codex`, and `.agents` read-only. This checkout's
actual and common Git directories both resolve to `.git`. Claude Code is absent from
PATH, so live Claude dispatch remains unverified.

## Prepared machine policy

[Managed requirements](../../tools/harness/managed-requirements.toml) define a single
permitted profile, `research_no_publish`, with workspace editing, read-only root Git
metadata, no sandbox escalation, and no command network access. Credential paths are
denied. A host-owned Git hook reuses the repository's command policy; native rules
explicitly forbid `git commit` and `git push` as well as staging/history mutations.
Project hooks remain enabled alongside the managed hook.

MCP servers, apps/connectors, plugins, and browser/computer-use capabilities are disabled
because they can publish without the Git executable. Web search is left available.
This intentionally restricts package downloads, online acquisition, Docker socket use,
and external integrations in Codex sessions. Researchers can perform those activities
outside the agent session. Existing analysis code and environment declarations are unchanged.

The profile retains broad filesystem read access for existing research tools and external
legacy inputs, with explicit credential exclusions. It is not a complete secret-discovery
policy: credentials may exist elsewhere or in inherited environment variables. Offline
command execution and disabled publishing capabilities are separate protections.

## Fixture results and topology limits

Run `python3 -B tools/harness/verify_host_profile.py` in a normal macOS Terminal with
Python 3.11+ and this installed Codex version. Running a sandbox inside an existing agent
sandbox may require host execution; the fixture invokes no model, Git mutation, or remote
service. It creates and removes only disposable files.

Ten checks passed: workspace write; metadata read; denial of writes to root metadata,
explicitly registered nested metadata, an explicitly registered common-directory fixture,
an explicitly protected policy directory, and a metadata symlink; denial of metadata
rename; denial of a dummy credential read; and denial of loopback networking. Protected
fixture contents remained unchanged. This tests the candidate profile with fixture paths;
it is not evidence that system requirements or feature restrictions have been deployed.

An earlier fixture showed that unregistered nested `.git` directories remained writable.
The client also rejected read-access glob paths. Therefore, before using a parent directory,
linked worktree, submodule, or bare repository as a workspace, register the actual Git and
common directories as **exact read paths** in the administrator-owned profile and repeat
the check. The installer registers this checkout's actual/common directories automatically.
It does not discover every nested or future repository on the Mac. Opening each ordinary
checkout at its own root uses the standard `.git` protection.

Arbitrary programs can construct Git objects in unrelated writable paths; literal-command
hooks cannot interpret all program semantics. Do not describe this policy as a proof that
every possible Git-equivalent computation is impossible. It protects registered metadata
and removes the tested publishing channels in supported clients.

## Install and verify

**Not installed in this session.** `/etc/codex/requirements.toml` was absent, and
`sudo -n /usr/bin/true` reported that a password is required. No system policy was changed.
The researcher must enter the administrator password locally; never send it to an agent.

From the repository root in your own Terminal:

```sh
sudo sh tools/harness/install_managed_policy.sh
```

The [installer](../../tools/harness/install_managed_policy.sh) creates root-owned files
under `/etc/codex/`, refuses to overwrite an existing policy/hooks directory, and performs
Git inspection only. Review this bundle before running it. Installer shell syntax was
checked; privileged installation itself has not been executed.

After installation, restart the affected Codex clients. Inspect effective requirements
and hook metadata through `configRequirements/read`, `config/read`, and `hooks/list` in
the installed app-server API. Confirm the managed profile, approval policy, feature pins,
empty MCP allowlist, root-owned hook files, and all actual/common directory restrictions.
Higher-precedence cloud/MDM policy may alter the result. Verify that a client cannot select
full access or override the managed restrictions; do this through configuration inspection,
not a real commit or push. Recheck the harmless invalid-option probe and allowed inspection.

Check a normal workspace edit, Git inspection, and the synthetic suite in a new session.
Never test forbidden Git commands against real history or a remote. Record each client
version and actual result. Until these deployment checks pass, machine-wide enforcement
remains pending. An administrator can roll back by removing only these installed files
after reviewing their ownership and contents, then restarting the client; agents must
not remove or weaken the policy themselves.

The official [permission-profile documentation](https://learn.chatgpt.com/docs/permissions)
defines filesystem and network boundaries. [Managed configuration](https://learn.chatgpt.com/docs/enterprise/managed-configuration)
documents enforceable requirements and their client/version limits. These support the
deployment design; the measured fixture and live-hook results above are local evidence.
