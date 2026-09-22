#!/bin/sh
# Run by the researcher with sudo; never solicits or records a password itself.
set -eu
if [ "$(id -u)" -ne 0 ]; then
  echo 'Run this script in your own Terminal with sudo.' >&2
  exit 1
fi
source_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
destination=/etc/codex
if [ -e "$destination/requirements.toml" ] || [ -L "$destination/requirements.toml" ] || [ -e "$destination/hooks" ] || [ -L "$destination/hooks" ]; then
  echo 'Existing managed policy/hooks found. Review and merge manually; nothing overwritten.' >&2
  exit 1
fi
if [ -L "$destination" ]; then
  echo 'Refusing a symlinked managed-policy directory.' >&2
  exit 1
fi
# Resolve this checkout's actual/common metadata before making system changes.
repo_root=$(CDPATH= cd -- "$source_dir/../.." && pwd)
git_dir=$(git -C "$repo_root" rev-parse --absolute-git-dir)
common_dir=$(git -C "$repo_root" rev-parse --path-format=absolute --git-common-dir)
policy_file=$(mktemp /private/tmp/codex-requirements.XXXXXX)
trap 'rm -f "$policy_file"' EXIT HUP INT TERM
cp "$source_dir/managed-requirements.toml" "$policy_file"
/usr/bin/python3 - "$policy_file" "$git_dir" "$common_dir" <<'PY'
import json
from pathlib import Path
import sys
with Path(sys.argv[1]).open('a') as stream:
    for path in sorted({str(Path(p).resolve()) for p in sys.argv[2:]}):
        stream.write('\n[permissions.research_no_publish.filesystem.' + json.dumps(path) + ']\n')
        stream.write('"." = "read"\n')
PY
# Hooks are installed before activating requirements. No credentials or Git changes.
install -d -o root -g wheel -m 755 "$destination" "$destination/hooks"
install -o root -g wheel -m 644 "$source_dir/guard_policy.py" "$source_dir/managed_git_guard.py" "$destination/hooks/"
install -o root -g wheel -m 644 "$policy_file" "$destination/requirements.toml"
echo 'Installed. Restart Codex and verify effective requirements before relying on enforcement.'
