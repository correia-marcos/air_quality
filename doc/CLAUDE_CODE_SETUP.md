# Claude Code setup — what it is and why

This repo ships a Claude Code configuration tuned for a reproducible economics paper. It has two
jobs: (1) make Claude a reliable collaborator on *this* pollution project, and (2) serve as a
**template** for future paper repos — copy `.claude/`, `CLAUDE.md`, and `.mcp.json` into a new
project and edit the project-specific lines.

Everything below lives in the repo so it is versioned and shared with coauthors.

## The pieces

### `CLAUDE.md` (project memory)
Loaded into every Claude session. Kept under ~200 lines so Claude actually follows it. It states
what the project is, the `src/` vs `scripts/` split, how to run it, the coding conventions in
brief, the behavior guidelines (think before coding, surface trade-offs, no vibe-coded bloat, be
succinct), and the reader-first principle. Detailed rules are pushed out to `.claude/rules/` so
the memory file stays short.

### `.claude/rules/*.md` (path-scoped)
Each rule carries a `paths:` frontmatter naming the files it governs, so context stays lean and
relevant.

> **How they actually load.** Nothing *in this repo* parses that frontmatter — there is no hook or
> `@import` doing it. Conditional loading is either native to the Claude Code build in use, or it
> does not happen and the rules reach Claude because `CLAUDE.md` points at them and the agents and
> commands below read them explicitly. Either way, treat `paths:` as documentation of scope, not
> as a guarantee the file was loaded.

- `r-style.md` (`**/*.R`, `.qmd`) — line length, the header block, `here::here()`, package loading,
  the anti-bloat rules, and the comment rule. That last one is the canonical copy: rationale lives
  once in `@Description` / `@Details`, and a comment inside a function body is at most two lines
  saying what happens *to the data* there. `CLAUDE.md`, `/new-process-script`, `/review-r` and
  `r-reproducibility-reviewer` point here rather than restating it, because five copies of a rule
  drift.
- `reproducibility.md` (Dockerfile, compose, `renv.lock`, `.Rprofile`, `DESCRIPTION`, `.env*`) — the
  three pinned layers (Docker/renv/paths) and the invariants to preserve when editing infra.
- `validation.md` (`scripts/validation_old_version/**`, the validation util, reports) — the legacy
  audit track and the Step 0-4 attribution framework; what's tracked vs regenerated.
- `data-and-paths.md` (`scripts/**`, `src/**`) — the one-way data-layer ratchet and read-only
  `data/raw` + `data/_legacy`.

### `.claude/commands/*.md` (slash commands)
Repeatable workflows so you don't re-explain them each time:
`/new-process-script`, `/new-figure`, `/add-city`, `/validate-city`, `/reproduce`, `/review-r`.
They encode the house patterns (standard header, registry-based city addition, processed-only
plotting, the reproduce walkthrough for referees).

### `.claude/agents/*.md` (subagents)
Specialized, read-only reviewers that run in their own context so a big audit doesn't crowd the
main session:
- `r-reproducibility-reviewer` — style + reproducibility + reader-transparency review of R.
- `legacy-validation-auditor` — the Step 0-4 comparison against the coauthor's legacy code.

### `.claude/settings.json` (permissions + hook)
Auto-allows the safe, high-frequency actions (run `Rscript`/`quarto`, read/edit/grep, ordinary
git); **asks** before irreversible or heavy ones (`rm`, `mv`, `git push`, `docker`); and **denies**
reading secrets (`.env`, `.Renviron`, `.netrc`) or editing inputs of record (`renv.lock`,
`data/raw`, `data/_legacy`). `respectGitignore` keeps ignored data out of Claude's view.
Personal tweaks go in `settings.local.json` (git-ignored; copy `settings.local.json.example`).

### `.claude/hooks/guard.py` (safety net)
A `PreToolUse` hook that enforces the same guardrails at the point of action: it **denies** writes
to raw/legacy data, `renv.lock`, and secrets, and **asks** before `rm -rf`, force-push, hard reset,
or anything touching raw/legacy data. It fails open — a bug in the hook can never block real work.

### `.mcp.json` (extra tools)
Two MCP servers: **context7** (pulls current docs for R packages like sf, arrow, data.table on
demand) and **deepwiki** (queries library/repo docs). Handy for accurate, version-correct R help
instead of guessing.

## Why this shape

- **Reader-first, everywhere.** Editors, referees, and students must run the code and *see the data
  change*. Every rule and command pushes toward inspectable, linear, Parquet-checkpointed code over
  clever abstraction.
- **Reproducibility is protected, not assumed.** The pinned layers (Docker, renv, `here::here()`)
  are guarded by both settings and the hook so they can't drift by accident.
- **The legacy audit is first-class.** The Step 0-4 framework has its own rule, command, and agent,
  because attributing result changes to specific input updates is the heart of your contribution.
- **Lean context.** Short `CLAUDE.md` + path-scoped rules means Claude loads only what's relevant to
  the file at hand, which improves adherence.

## Reusing this for a new paper repo

1. Copy `.claude/`, `CLAUDE.md`, `.mcp.json`, and `doc/CLAUDE_CODE_SETUP.md` into the new repo.
2. Edit `CLAUDE.md`: project description, layout, run instructions, author.
3. Adjust `.claude/rules/` paths and any stack-specific conventions (e.g. if not R-only).
4. Keep or trim commands/agents to match the new project's workflows.
5. Review `.claude/settings.json` deny/ask lists for the new repo's inputs of record.

## TL;DR

| Piece | Role | Loads when |
|---|---|---|
| `CLAUDE.md` | Project memory, conventions, behavior rules | Every session |
| `.claude/rules/*` | Detailed, path-scoped guidance | Scope declared in `paths:`; see the caveat above |
| `.claude/commands/*` | Repeatable workflows (`/new-process-script`, ...) | You invoke them |
| `.claude/agents/*` | Read-only specialist reviewers | You delegate to them |
| `.claude/settings.json` | Permissions (allow/ask/deny) + hook wiring | Every session |
| `.claude/hooks/guard.py` | Protects raw/legacy data, renv.lock, secrets; nudges on `src/` vs `scripts/` structure | Before each tool use |
| `.mcp.json` | context7 + deepwiki for live R docs | On demand |
