# Hooks

One PreToolUse hook, `guard.py`, wired in `.claude/settings.json`.

## What it does

Before any `Edit`, `Write`, `NotebookEdit`, or `Bash` call it checks the target:

- **Deny** — writing an *input of record* or a secret:
  `renv.lock`, `data/raw/`, `data/_legacy/`, `.Renviron`, `.netrc`, `.env`.
  These define reproducibility and must not change silently.
- **Ask** — irreversible or out-of-scope shell actions:
  `rm -rf`, `git push --force`, `git reset --hard`, redirects into `renv.lock`,
  or any command touching `data/raw/` or `data/_legacy/`.
- **Allow (silent)** — everything else. Ordinary analysis is never interrupted.

The guard fails open: if the payload can't be parsed it exits 0 and allows the action,
so a bug in the hook can never block your work.

## Turning it off

Per session: `claude --setting disableAllHooks=true`, or add
`{ "disableAllHooks": true }` to a personal `settings.local.json` (git-ignored).

## Extending it

Edit the `PROTECTED_WRITE` and `ASK_BASH` tuples at the top of `guard.py`.
Test without launching Claude:

```bash
echo '{"tool_name":"Edit","tool_input":{"file_path":"renv.lock"}}' | python3 .claude/hooks/guard.py
```

An empty response means "allow"; a JSON `permissionDecision` of `deny`/`ask` is a stop.
