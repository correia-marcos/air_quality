#!/usr/bin/env python3
"""Claude payload adapter for the shared project guard policy."""
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
        if decision:
            print(json.dumps({"hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "ask" if decision in ("prompt", "context") else decision,
                "permissionDecisionReason": reason}}))
    except (ValueError, TypeError, AttributeError):
        print("Project guard could not inspect the payload.", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
