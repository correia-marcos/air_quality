#!/usr/bin/env python3
"""PreToolUse guard for the Air Monitoring replication package.

Protects the inputs of record (raw data, legacy data, the renv lockfile) and
secrets, and asks before irreversible shell actions. It NEVER blocks ordinary
analysis work: anything not explicitly matched is allowed silently.

It also nudges on the two structural rules that keep the repo readable: functions belong in
src/, and src/ defines them without running anything. Those are "ask", never "deny" -- they
have legitimate exceptions, and a hard block would stop refactors.

Scope note: the structural checks anchor at column 0, so nested helpers and ::-qualified calls
inside a function body never match. For Edit the payload carries only a fragment, so an
indented insert will not match either. This is a nudge at whole-file writes and top-level
inserts, not a linter -- the reviewer catches the rest.

Contract (Claude Code PreToolUse hook):
  - stdin  : JSON with tool_name and tool_input
  - stdout : JSON with hookSpecificOutput.permissionDecision in
             {"allow","ask","deny"} plus a human-readable reason
  - exit 0 : always (a crashing guard must not halt legitimate work)
"""
import json
import re
import sys

# Files that are inputs of record or secrets: writing them needs a hard stop.
PROTECTED_WRITE = (
    "renv.lock",
    "data/raw/",
    "data/_legacy/",
    ".renviron",
    ".netrc",
    ".env",
)

# Config that shapes how the whole project is worked on: confirm before rewriting it.
ASK_WRITE = (
    ".claude/settings.json",
    "claude.md",
    ".claude/hooks/",
)

# src/ files that legitimately run setup at top level, so the src/ check skips them.
SRC_EXEMPT = ("config_utils_", "setup_packages.r", "registry.r", "theme_paper.r")

# A top-level `name <- function(` under scripts/ means logic escaped src/.
# Only `<-` counts: a column-0 `error = function(e)` is a tryCatch handler
# argument in a multi-line call, not a definition.
SCRIPT_FN_RE = re.compile(r"^[A-Za-z._][A-Za-z0-9._]*\s*<-\s*function\s*\(", re.M)

# Top-level *actions* in src/. src/ declares packages and defines functions; it does not run.
SRC_EFFECT_RE = re.compile(
    r"^(setwd|set\.seed|rm|dir\.create|ggsave|writeLines|write\.csv|read\.csv|"
    r"readRDS|saveRDS|theme_set|font_add|showtext_auto)\s*\(", re.M)

# Shell fragments that are irreversible or reach outside the analysis: confirm first.
ASK_BASH = (
    r"\brm\s+-rf\b",
    r"\bgit\s+push\b.*--force",
    r"\bgit\s+reset\s+--hard\b",
    r">\s*renv\.lock",
    r"data/raw/",
    r"data/_legacy/",
    r"\.env\b",
    r"\.netrc\b",
    r"\.Renviron\b",
    r">\s*\.claude/",
)


def decide(tool, tool_input):
    """Return (decision, reason) or (None, None) to stay out of the way."""
    # Edit / Write / NotebookEdit: check the target path against protected inputs.
    if tool in ("Edit", "Write", "NotebookEdit"):
        path = (tool_input.get("file_path")
                or tool_input.get("notebook_path") or "").lower()

        # Inputs of record first: these deny even if a structural rule also matches.
        for p in PROTECTED_WRITE:
            if p in path:
                return ("deny",
                        f"'{p}' is an input of record or a secret. Ask Marcos "
                        "before changing it; don't edit or regenerate it silently.")

        for p in ASK_WRITE:
            if p in path:
                return ("ask",
                        f"'{p}' configures how the whole project is worked on. "
                        "Confirm with Marcos before rewriting it.")

        content = (tool_input.get("content")
                   or tool_input.get("new_string") or "")

        if "/scripts/" in path and path.endswith(".r"):
            if SCRIPT_FN_RE.search(content):
                return ("ask",
                        "This puts a top-level function definition in scripts/. Functions "
                        "live in src/ (CLAUDE.md). If it is single-use, inline it or use a "
                        "spec table + loop; if reusable, put it in the stage's src/ file.")

        if "/src/" in path and path.endswith(".r"):
            if not any(x in path for x in SRC_EXEMPT):
                hit = SRC_EFFECT_RE.search(content)
                if hit:
                    return ("ask",
                            f"'{hit.group(1)}(' runs at the top level of src/. src/ defines "
                            "functions; scripts/ runs them. Move the call into the calling "
                            "script (theme goes in set_paper_theme()).")

        return (None, None)

    # Bash: scan the command for irreversible or out-of-scope actions.
    if tool == "Bash":
        cmd = tool_input.get("command", "")
        for pat in ASK_BASH:
            if re.search(pat, cmd, flags=re.IGNORECASE):
                return ("ask",
                        "This command is irreversible or touches raw/legacy data. "
                        "Confirm with Marcos before running it.")
        return (None, None)

    return (None, None)


def main():
    try:
        payload = json.load(sys.stdin)
    except Exception:
        sys.exit(0)  # No parseable input: do nothing.

    tool = payload.get("tool_name", "")
    tool_input = payload.get("tool_input", {}) or {}
    decision, reason = decide(tool, tool_input)

    if decision:
        print(json.dumps({
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": decision,
                "permissionDecisionReason": reason,
            }
        }))
    sys.exit(0)


if __name__ == "__main__":
    main()
