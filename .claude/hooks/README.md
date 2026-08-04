# Hooks

One PreToolUse hook, `guard.py`, wired in `.claude/settings.json`.

## What it does

Before any `Edit`, `Write`, `NotebookEdit`, or `Bash` call it checks the target:

- **Deny** — writing an *input of record* or a secret:
  `renv.lock`, `data/raw/`, `data/_legacy/`, `.Renviron`, `.netrc`, `.env`.
  These define reproducibility and must not change silently.
- **Ask** — irreversible or out-of-scope shell actions:
  `rm -rf`, `git push --force`, `git reset --hard`, redirects into `renv.lock` or `.claude/`,
  any command touching `data/raw/`, `data/_legacy/`, `.env`, `.netrc` or `.Renviron`.
- **Ask** — rewriting project-shaping config: `.claude/settings.json`, `CLAUDE.md`,
  `.claude/hooks/`.
- **Ask** — the two structural rules:
  a top-level `name <- function(` under `scripts/` (functions belong in `src/`), and a
  top-level *action* in `src/` (`ggsave`, `setwd`, `font_add`, `theme_set`, `dir.create`, …).
  `src/` defines functions; `scripts/` runs them.
- **Allow (silent)** — everything else. Ordinary analysis is never interrupted.

The structural checks are deliberately `ask`, never `deny`: they have legitimate exceptions and
a hard block would stop refactors. They anchor at column 0, so nested helpers and `::`-qualified
calls inside a function body never match; `config_utils_*.R`, `setup_packages.R`, `registry.R`
and `theme_paper.R` are exempt from the `src/` rule because they legitimately run setup. For
`Edit` the payload is only a fragment, so an indented insert will not match — this is a nudge at
whole-file writes, not a linter.

The guard fails open: if the payload can't be parsed it exits 0 and allows the action,
so a bug in the hook can never block your work.

## Turning it off

Per session: `claude --setting disableAllHooks=true`, or add
`{ "disableAllHooks": true }` to a personal `settings.local.json` (git-ignored).

## Extending it

Edit the `PROTECTED_WRITE`, `ASK_WRITE`, `ASK_BASH`, `SCRIPT_FN_RE`, `SRC_EFFECT_RE` or
`SRC_EXEMPT` definitions at the top of `guard.py`. Test without launching Claude:

```bash
echo '{"tool_name":"Edit","tool_input":{"file_path":"renv.lock"}}' | python3 .claude/hooks/guard.py
```

Payloads with newlines in `content` must be built with `python3 -c` or a heredoc, not `echo` —
shell `echo` turns `\n` into a real newline, which makes the JSON invalid and the guard (which
fails open) silently allows.

An empty response means "allow"; a JSON `permissionDecision` of `deny`/`ask` is a stop.
