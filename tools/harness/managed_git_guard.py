#!/usr/bin/env python3
"""Machine hook: reuse Git command policy without project-specific scientific rules."""  # noqa: EXE001
import json
import sys
from pathlib import Path

from guard_policy import GIT_DENIAL, git_shell_denial, patch_edits


def decision(payload):
    tool = payload["tool_name"]
    args = payload["tool_input"]
    if tool == "Bash":
        command = args["command"]
        if not isinstance(command, str) or not command.strip():
            return "Missing command text."
        return git_shell_denial(command)
    if tool == "apply_patch":
        for path, _ in patch_edits(args["command"]):
            if ".git" in Path(path).parts:
                return GIT_DENIAL
        return None
    return "Unexpected tool in managed Git hook."


if __name__ == "__main__":
    try:
        reason = decision(json.load(sys.stdin))
    except (KeyError, ValueError, TypeError, AttributeError):
        print("Managed Git hook could not inspect the payload.", file=sys.stderr)
        sys.exit(2)
    if reason:
        print(json.dumps({"hookSpecificOutput": {
            "hookEventName": "PreToolUse", "permissionDecision": "deny",
            "permissionDecisionReason": reason,
        }}))
