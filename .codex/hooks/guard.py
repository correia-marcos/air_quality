#!/usr/bin/env python3
"""Codex adapter: canonical apply_patch payloads; no unsupported ask decision."""
import json
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools" / "harness"))
from guard_policy import edit_decision, patch_edits, shell_decision


def main():
    try:
        payload = json.load(sys.stdin)
        tool = payload.get("tool_name", "")
        args = payload.get("tool_input", {})
        if tool == "apply_patch":
            decisions = [edit_decision(p, c, ROOT)
                         for p, c in patch_edits(args.get("command", ""))]
        elif tool == "Bash":
            decisions = [shell_decision(args.get("command", ""))]
        else:
            decisions = []
        blocked = [reason for decision, reason in decisions if decision in ("deny", "prompt")]
        context = [reason for decision, reason in decisions if decision == "context"]
        out = {"hookEventName": "PreToolUse"}
        if blocked:
            out.update(permissionDecision="deny", permissionDecisionReason="; ".join(blocked))
        elif context:
            out["additionalContext"] = "; ".join(context)
        else:
            return 0
        print(json.dumps({"hookSpecificOutput": out}))
    except (ValueError, TypeError, AttributeError):
        print("Project guard could not inspect the payload; inspect the call before retrying.", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
