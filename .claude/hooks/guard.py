#!/usr/bin/env python3
"""PreToolUse guard for the Air Monitoring replication package.

Protects the inputs of record (raw data, legacy data, the renv lockfile) and
secrets, and asks before irreversible shell actions. It NEVER blocks ordinary
analysis work: anything not explicitly matched is allowed silently.

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

# Shell fragments that are irreversible or reach outside the analysis: confirm first.
ASK_BASH = (
    r"\brm\s+-rf\b",
    r"\bgit\s+push\b.*--force",
    r"\bgit\s+reset\s+--hard\b",
    r">\s*renv\.lock",
    r"data/raw/",
    r"data/_legacy/",
)


def decide(tool, tool_input):
    """Return (decision, reason) or (None, None) to stay out of the way."""
    # Edit / Write / NotebookEdit: check the target path against protected inputs.
    if tool in ("Edit", "Write", "NotebookEdit"):
        path = (tool_input.get("file_path")
                or tool_input.get("notebook_path") or "").lower()
        for p in PROTECTED_WRITE:
            if p in path:
                return ("deny",
                        f"'{p}' is an input of record or a secret. Ask Marcos "
                        "before changing it; don't edit or regenerate it silently.")
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
