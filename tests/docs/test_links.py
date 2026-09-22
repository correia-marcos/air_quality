import importlib.util
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    "check_links", Path(__file__).resolve().parents[2] / "tools/docs/check_links.py")
LINKS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(LINKS)


class LinkTests(unittest.TestCase):
    def check_text(self, text, target="# Present\n", files=None):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / "README.md").write_text(text)
            (root / "target.md").write_text(target)
            return LINKS.check(root, files or {"README.md", "target.md"})

    def test_valid_and_duplicate_anchors(self):
        errors, _, count = self.check_text("[one](target.md#present) [two](target.md#present-1)",
                                           "# Present\n# Present\n")
        self.assertEqual(errors, [])
        self.assertEqual(count, 2)

    def test_missing_anchor_and_file(self):
        errors, _, _ = self.check_text("[one](target.md#absent) [two](missing.md)")
        self.assertEqual(len(errors), 2)

    def test_ignored_local_evidence_is_reported(self):
        errors, local, _ = self.check_text("[run](data/verification/run.json) [index](doc/audits/README.md)")
        self.assertEqual(local, {"data/verification/run.json"})
        self.assertEqual(len(errors), 1)  # public README must exist

    def test_repository_escape(self):
        errors, _, _ = self.check_text("[outside](../outside.md)")
        self.assertIn("escapes repository", errors[0])

    def test_examples_and_external_links(self):
        errors, _, count = self.check_text("```md\n[x](missing.md)\n```\n`[x](missing.md)`\n[x](https://example.org)")
        self.assertEqual((errors, count), ([], 0))

    def test_untracked_file_does_not_hide_missing_public_target(self):
        errors, _, _ = self.check_text("[x](target.md)", files={"README.md"})
        self.assertIn("missing public target", errors[0])

    def test_explicit_redirect_anchor(self):
        errors, _, _ = self.check_text('[x](target.md#old-section)', '<a id="old-section"></a>\n')
        self.assertEqual(errors, [])

    def test_missing_tracked_non_markdown_target(self):
        errors, _, _ = self.check_text('[x](missing.csv)', files={"README.md", "missing.csv"})
        self.assertIn("missing public target", errors[0])


if __name__ == "__main__":
    unittest.main()
