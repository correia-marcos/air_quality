import json
import subprocess
import sys
import unittest
from pathlib import Path

from managed_git_guard import decision
from test_guard import ALLOWED_INSPECTION, FORBIDDEN_GIT


class ManagedGuardTests(unittest.TestCase):
    def test_git_policy_strings(self):
        for command in FORBIDDEN_GIT:
            with self.subTest(command=command):
                self.assertTrue(decision({"tool_name": "Bash", "tool_input": {"command": command}}))
        for command in ALLOWED_INSPECTION:
            self.assertIsNone(decision({"tool_name": "Bash", "tool_input": {"command": command}}))

    def test_nested_metadata_patch(self):
        patch = "*** Begin Patch\n*** Delete File: nested/.git/index\n*** End Patch"
        self.assertTrue(decision({"tool_name": "apply_patch", "tool_input": {"command": patch}}))

    def test_entrypoint_malformed_payload_and_denial(self):
        script = Path(__file__).resolve().parents[2] / "tools/harness/managed_git_guard.py"
        for payload, code in (({}, 2), ({"tool_name": "Bash", "tool_input": {"command": "git push"}}, 0)):
            result = subprocess.run([sys.executable, "-B", str(script)], check=False, input=json.dumps(payload),
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, code)
            if code == 0:
                self.assertEqual(json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"], "deny")


if __name__ == "__main__":
    unittest.main()
