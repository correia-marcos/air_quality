"""Run with python3 -m unittest discover -s tests/ai."""
from pathlib import Path
import sys
import unittest
import tempfile

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts" / "ai"))
from guard_policy import edit_decision, patch_edits, shell_decision


class GuardTests(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.TemporaryDirectory()
        self.addCleanup(self.root.cleanup)

    def test_protected_and_example(self):
        for path in ("data/raw/a", "data/_legacy/a", "data/downloads/a", "renv.lock", ".env", ".env.prod", ".netrc"):
            self.assertEqual(edit_decision(path, "", self.root.name)[0], "deny")
        self.assertIsNone(edit_decision(".env.example", "", self.root.name)[0])
        self.assertIsNone(edit_decision("doc/ai/new.md", "", self.root.name)[0])

    def test_patch_all_paths(self):
        patch = "*** Begin Patch\n*** Update File: src/a.R\n*** Move to: data/raw/a.R\n+x <- 1\n*** Delete File: renv.lock\n*** Add File: doc/new.md\n+hello\n*** End Patch"
        edits = patch_edits(patch)
        self.assertEqual([p for p, _ in edits], ["src/a.R", "data/raw/a.R", "renv.lock", "doc/new.md"])
        self.assertEqual(sum(edit_decision(p, c, self.root.name)[0] == "deny" for p, c in edits), 2)
        with self.assertRaises(ValueError):
            patch_edits("not a patch")

    def test_symlink_and_normalization(self):
        root = Path(self.root.name)
        (root / "data/raw").mkdir(parents=True)
        (root / "alias").symlink_to(root / "data/raw", target_is_directory=True)
        self.assertEqual(edit_decision("alias/a", "", root)[0], "deny")
        self.assertEqual(edit_decision("src/../renv.lock", "", root)[0], "deny")

    def test_structural_nudge_and_shell(self):
        self.assertEqual(edit_decision("scripts/a.R", "f <- function() {}", self.root.name)[0], "context")
        self.assertIsNone(edit_decision("src/general_utilities/config_utils_x.R", "setwd('x')", self.root.name)[0])
        self.assertEqual(shell_decision("git reset --hard")[0], "prompt")
        self.assertEqual(shell_decision("cat .env")[0], "deny")
        self.assertIsNone(shell_decision("cat .env.example")[0])
        self.assertIsNone(shell_decision("Rscript tests/testthat.R")[0])


if __name__ == "__main__":
    unittest.main()

class AdapterTests(unittest.TestCase):
    def test_native_payloads(self):
        import json
        import subprocess
        root = Path(__file__).resolve().parents[2]
        cases = [
            (".codex/hooks/guard.py", "apply_patch", {"command": "*** Begin Patch\n*** Add File: doc/ai/ok.md\n+ok\n*** Delete File: renv.lock\n*** End Patch"}, "deny"),
            (".codex/hooks/guard.py", "apply_patch", {"command": "*** Begin Patch\n*** Update File: src/a.R\n*** Move to: data/raw/a.R\n+x\n*** End Patch"}, "deny"),
            (".codex/hooks/guard.py", "apply_patch", {"command": "*** Begin Patch\n*** Update File: .env.example\n+x\n*** End Patch"}, None),
            (".codex/hooks/guard.py", "Bash", {"command": "git reset --hard"}, "deny"),
            (".claude/hooks/guard.py", "Write", {"file_path": "data/raw/example.csv", "content": "x"}, "deny"),
            (".claude/hooks/guard.py", "Write", {"file_path": ".env.example", "content": "KEY="}, None),
        ]
        for adapter, tool, args, expected in cases:
            with self.subTest(adapter=adapter, tool=tool, expected=expected):
                proc = subprocess.run([sys.executable, str(root / adapter)],
                    input=json.dumps({"tool_name": tool, "tool_input": args}), text=True, capture_output=True)
                self.assertEqual(proc.returncode, 0, proc.stderr)
                answer = json.loads(proc.stdout) if proc.stdout.strip() else {}
                decision = answer.get("hookSpecificOutput", {}).get("permissionDecision")
                self.assertEqual(decision, expected)
                if adapter.startswith(".codex"):
                    self.assertNotEqual(decision, "ask")
