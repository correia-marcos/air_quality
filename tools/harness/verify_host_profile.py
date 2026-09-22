#!/usr/bin/env python3
"""Exercise the candidate macOS sandbox on disposable files; never invoke Git mutations."""  # noqa: EXE001
import json
import platform
import subprocess
import sys
import tempfile
from pathlib import Path

import tomllib  # type: ignore


def inline_toml(value):
    if isinstance(value, dict):
        return "{" + ", ".join(json.dumps(k) + " = " + inline_toml(v)
                                for k, v in value.items()) + "}"
    return json.dumps(value)


def main():
    if platform.system() != "Darwin":
        raise SystemExit("This fixture requires the macOS Codex sandbox; no check performed.")
    policy = tomllib.loads(Path(__file__).with_name("managed-requirements.toml").read_text())
    profile = policy["permissions"]["research_no_publish"]
    with tempfile.TemporaryDirectory(prefix="codex-policy-fixture-") as folder:
        root = Path(folder).resolve()
        names = (".git", "nested/.git", "external-common", "managed-policy")
        for name in names:
            (root / name).mkdir(parents=True)
            (root / name / "probe").write_text("unchanged")
        (root / "dummy-secret").write_text("fixture-only")
        (root / "metadata-alias").symlink_to(root / ".git", target_is_directory=True)
        # Read globs are unsupported: register nested/common directories explicitly.
        for name in names[1:]:
            profile["filesystem"][str(root / name)] = "read"
        profile["filesystem"][str(root / "dummy-secret")] = "deny"
        code = r'''
import json, socket
from pathlib import Path
root = Path.cwd()
results = {}
(root / "normal.txt").write_text("allowed")
results["workspace_write"] = True
results["metadata_read"] = (root / ".git/probe").read_text() == "unchanged"
for name in (".git", "nested/.git", "external-common", "managed-policy", "metadata-alias"):
    try:
        with (root / name / "probe").open("a") as stream: stream.write("unexpected")
        results[name + "_write_denied"] = False
    except PermissionError: results[name + "_write_denied"] = True
try:
    (root / ".git/probe").rename(root / "renamed-probe")
    results["metadata_rename_denied"] = False
except PermissionError: results["metadata_rename_denied"] = True
try:
    (root / "dummy-secret").read_text()
    results["credential_fixture_denied"] = False
except PermissionError: results["credential_fixture_denied"] = True
sock = socket.socket()
try:
    sock.connect(("127.0.0.1", 9))
    results["network_denied"] = False
except PermissionError: results["network_denied"] = True
except OSError: results["network_denied"] = False
finally: sock.close()
print(json.dumps(results, sort_keys=True))
raise SystemExit(not all(results.values()))
'''
        command = ["codex", "sandbox", "-c", "permissions.research_no_publish=" + inline_toml(profile),
                   "-P", "research_no_publish", "-C", str(root), "--", sys.executable, "-B", "-c", code]
        result = subprocess.run(command, check=False, capture_output=True, text=True, timeout=30)
        print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, file=sys.stderr, end="")
        intact = all((root / name / "probe").is_file() and
                     (root / name / "probe").read_text() == "unchanged" for name in names)
        print("Fixture integrity:", intact)
        return result.returncode or int(not intact)


if __name__ == "__main__":
    raise SystemExit(main())
