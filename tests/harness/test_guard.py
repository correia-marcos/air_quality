"""Run with python3 -m unittest discover -s tests/harness."""
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools" / "harness"))
from guard_policy import edit_decision, patch_edits, shell_decision

# These strings are policy inputs only; never execute them as shell commands.
FORBIDDEN_GIT = (
    "git add file", "git commit -m example", "git commit --amend --no-edit",
    "git push", "git push --force-with-lease", "git push --all origin",
    "git push --tags origin", "git -C . commit -m example",
    "git -C. push", "git --git-dir=.git --work-tree=. push",
    "git --git-dir .git --work-tree . commit -m example",
    "/usr/bin/git 'push' origin main", "git --no-pager commit -m example",
    "env GIT_OPTIONAL_LOCKS=0 git push", "command git commit -m example",
    "GIT_OPTIONAL_LOCKS=0 git push", "git status && git push",
    "git diff; git commit -m example", "git status\ngit push",
    "git status || git push", "git push | cat", "(git push)",
    "echo $(git push)", "bash -lc 'git push'",
    "env sh -c 'git -C . commit -m example'",
    "git ci", "git -c alias.ci=commit ci", "git --config-env=alias.ci=VALUE ci",
    "git config alias.ci commit", "git merge topic", "git rebase topic",
    "git cherry-pick HEAD", "git am patch", "git commit-tree tree",
    "git update-ref refs/heads/main HEAD", "git send-pack origin main",
    "git fast-import", "git reset --hard", "git checkout -- file",
)
ALLOWED_INSPECTION = (
    "git status --short", "git diff --check", "git log -5", "git show HEAD:file",
    "git -C . --no-pager diff", "git rev-parse --git-common-dir",
    "git ls-files", "git config --get core.hooksPath", "git --version",
    "bash -lc 'git status && git diff'", "git status\ngit diff",
    "printf '%s\\n' 'git commit' 'git push'", "rg 'git commit' doc",
)


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
        self.assertEqual(shell_decision("git reset --hard")[0], "deny")
        self.assertEqual(shell_decision("rm -rf scratch")[0], "prompt")
        self.assertEqual(shell_decision("cat .env")[0], "deny")
        self.assertIsNone(shell_decision("cat .env.example")[0])
        self.assertIsNone(shell_decision("Rscript tests/testthat.R")[0])

    def test_git_mutations_are_denied(self):
        for command in FORBIDDEN_GIT:
            with self.subTest(command=command):
                self.assertEqual(shell_decision(command)[0], "deny")

    def test_git_inspection_and_quoted_documentation(self):
        for command in ALLOWED_INSPECTION:
            with self.subTest(command=command):
                self.assertIsNone(shell_decision(command)[0])

    def test_git_metadata_edits_are_denied(self):
        for path in (".git", ".git/index", ".git/refs/heads/main"):
            with self.subTest(path=path):
                self.assertEqual(edit_decision(path, "", self.root.name)[0], "deny")

    def test_uninspectable_shell_is_denied(self):
        self.assertEqual(shell_decision("git 'commit")[0], "deny")
        for command in ("", None, ["git", "push"]):
            with self.subTest(command=command):
                self.assertEqual(shell_decision(command)[0], "deny")

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
            (".claude/hooks/guard.py", "Write", {"file_path": ".git/index", "content": "x"}, "deny"),
            (".codex/hooks/guard.py", "apply_patch", {"command": "*** Begin Patch\n*** Delete File: .git/index\n*** End Patch"}, "deny"),
            (".claude/hooks/guard.py", "Write", {"file_path": ".env.example", "content": "KEY="}, None),
        ]
        for adapter in (".codex/hooks/guard.py", ".claude/hooks/guard.py"):
            cases.extend((adapter, "Bash", args, "deny")
                         for args in ({}, {"command": None}, {"command": ""}))
            cases.extend((adapter, "Bash", {"command": command}, "deny")
                         for command in FORBIDDEN_GIT)
            cases.extend((adapter, "Bash", {"command": command}, None)
                         for command in ALLOWED_INSPECTION)
        for adapter, tool, args, expected in cases:
            with self.subTest(adapter=adapter, tool=tool, expected=expected):
                proc = subprocess.run([sys.executable, "-B", str(root / adapter)],
                    check=False, input=json.dumps({"tool_name": tool, "tool_input": args}), text=True, capture_output=True)
                self.assertEqual(proc.returncode, 0, proc.stderr)
                answer = json.loads(proc.stdout) if proc.stdout.strip() else {}
                decision = answer.get("hookSpecificOutput", {}).get("permissionDecision")
                self.assertEqual(decision, expected)
                if adapter.startswith(".codex"):
                    self.assertNotEqual(decision, "ask")

    def test_malformed_payloads_block(self):
        import subprocess
        root = Path(__file__).resolve().parents[2]
        for adapter in (".codex/hooks/guard.py", ".claude/hooks/guard.py"):
            proc = subprocess.run([sys.executable, "-B", str(root / adapter)],
                                  check=False, input="{broken", text=True, capture_output=True)
            self.assertEqual(proc.returncode, 2, proc.stderr)


if __name__ == "__main__":
    unittest.main()
