#!/bin/sh
# Run by the researcher with sudo; never solicits or records a password itself.
# Installs the machine-wide Claude Code policy: it applies to every Claude project on this Mac.
set -eu
if [ "$(id -u)" -ne 0 ]; then
  echo 'Run this script in your own Terminal with sudo.' >&2
  exit 1
fi
source_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
destination="/Library/Application Support/ClaudeCode"
if [ -e "$destination/managed-settings.json" ] || [ -L "$destination/managed-settings.json" ] || [ -e "$destination/hooks" ] || [ -L "$destination/hooks" ]; then
  echo 'Existing managed settings/hooks found. Review and merge manually; nothing overwritten.' >&2
  exit 1
fi
if [ -L "$destination" ]; then
  echo 'Refusing a symlinked managed-settings directory.' >&2
  exit 1
fi
# Add this checkout's actual and common Git directories as exact write denials, next to
# the general /**/.git pattern. No credentials or Git changes.
repo_root=$(CDPATH= cd -- "$source_dir/../.." && pwd)
git_dir=$(git -C "$repo_root" rev-parse --absolute-git-dir)
common_dir=$(git -C "$repo_root" rev-parse --path-format=absolute --git-common-dir)
policy_file=$(mktemp /private/tmp/claude-managed-settings.XXXXXX)
trap 'rm -f "$policy_file"' EXIT HUP INT TERM
/usr/bin/python3 - "$source_dir/claude-managed-settings.json" "$policy_file" "$git_dir" "$common_dir" <<'PY'
import json
from pathlib import Path
import sys
policy = json.loads(Path(sys.argv[1]).read_text())
deny = policy["sandbox"]["filesystem"]["denyWrite"]
for path in sorted({str(Path(p).resolve()) for p in sys.argv[3:]}):
    if path not in deny:
        deny.append(path)
Path(sys.argv[2]).write_text(json.dumps(policy, indent=2) + "\n")
PY
# Hooks are installed before the settings that call them. Files stay readable by users;
# Claude Code skips a managed file it cannot read.
install -d -o root -g wheel -m 755 "$destination" "$destination/hooks"
install -o root -g wheel -m 644 "$source_dir/guard_policy.py" "$source_dir/managed_git_guard.py" "$destination/hooks/"
install -o root -g wheel -m 644 "$policy_file" "$destination/managed-settings.json"
echo 'Installed. Restart Claude Code (reload every VS Code window) and verify before relying on it.'
