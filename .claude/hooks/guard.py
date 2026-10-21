#!/usr/bin/env python3
"""Claude adapter: deny and prompt decisions deny; context is a note, as for Codex."""
import json
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools" / "harness"))
from guard_policy import edit_decision, shell_decision


def main():
    try:
        payload = json.load(sys.stdin)
        tool = payload.get("tool_name", "")
        args = payload.get("tool_input", {})
        if tool in ("Edit", "Write", "NotebookEdit"):
            decision, reason = edit_decision(
                args.get("file_path", args.get("notebook_path", "")),
                args.get("content", args.get("new_string", "")), ROOT)
        elif tool == "Bash":
            decision, reason = shell_decision(args.get("command", ""))
        else:
            decision, reason = None, None
        out = {"hookEventName": "PreToolUse"}
        if decision in ("deny", "prompt"):
            out.update(permissionDecision="deny", permissionDecisionReason=reason)
        elif decision == "context":
            out["additionalContext"] = reason
        else:
            return 0
        print(json.dumps({"hookSpecificOutput": out}))
    except (ValueError, TypeError, AttributeError):
        print("Project guard could not inspect the payload.", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
