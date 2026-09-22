#!/usr/bin/env python3
"""Check public Markdown file links and anchors without network access or dependencies."""
import argparse
import csv
import re
import subprocess
from pathlib import Path
from urllib.parse import unquote, urlsplit

ROOT = Path(__file__).resolve().parents[2]
LOCAL_ROOTS = ("data/", "doc/audits/", "doc/notes/", "doc/paper/")
PUBLIC_LOCAL = {"doc/audits/README.md", "doc/notes/README.md", "doc/paper/README.md"}
LINK = re.compile(r"!?\[[^\]\n]*\]\(\s*(<[^>]+>|[^\s)]+)(?:\s+\"[^\"]*\")?\s*\)")


def prose(text):
    """Discard fenced examples while preserving line numbers for findings."""
    fence = None
    lines = []
    for line in text.splitlines():
        match = re.match(r"^\s*(`{3,}|~{3,})", line)
        if match:
            marker = match[1]
            if fence is None:
                fence = marker
            elif marker[0] == fence[0] and len(marker) >= len(fence):
                fence = None
            lines.append("")
        else:
            lines.append("" if fence else line)
    return "\n".join(lines)


def anchors(text):
    text = prose(text)
    found = set(re.findall(r'<a\s+id=[\"\x27]([^\"\x27]+)', text))
    counts = {}
    for heading in re.findall(r"^#{1,6}\s+(.+?)\s*#*\s*$", text, re.M):
        heading = re.sub(r"\[([^]]+)\]\([^)]+\)", r"\1", heading)
        slug = re.sub(r"[^\w\- ]", "", heading.lower()).replace(" ", "-")
        count = counts.get(slug, 0)
        counts[slug] = count + 1
        found.add(slug + (f"-{count}" if count else ""))
    return found


def check(root, files, archives=()):
    root = root.resolve()
    files = set(files)
    errors, local = [], set()
    checked = 0
    for name in sorted(files):
        if not name.endswith(".md") or name in archives:
            continue
        source = root / name
        if not source.is_file():
            errors.append(f"{name}: listed public file is missing")
            continue
        for number, line in enumerate(prose(source.read_text()).splitlines(), 1):
            # Inline code can contain literal link syntax in examples.
            line = re.sub(r"(`+).*?\1", "", line)
            for match in LINK.finditer(line):
                target = match[1].strip("<>")
                url = urlsplit(target)
                if url.scheme or url.netloc:
                    continue
                checked += 1
                path = (source.parent / unquote(url.path)).resolve() if url.path else source
                try:
                    relative = path.relative_to(root).as_posix()
                except ValueError:
                    errors.append(f"{name}:{number}: link escapes repository: {target}")
                    continue
                if relative.startswith(LOCAL_ROOTS) and relative not in PUBLIC_LOCAL:
                    local.add(relative)
                    continue
                is_dir = any(f.startswith(relative.rstrip('/') + '/') for f in files)
                if (relative not in files and not is_dir) or not path.exists():
                    errors.append(f"{name}:{number}: missing public target: {target}")
                elif url.fragment and path.suffix == ".md" and path.is_file():
                    if unquote(url.fragment) not in anchors(path.read_text()):
                        errors.append(f"{name}:{number}: missing anchor: {target}")
    return errors, local, checked


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tracked-only", action="store_true",
                        help="Exclude unstaged new files (CI/fresh-clone inventory).")
    args = parser.parse_args()
    command = ["git", "ls-files", "-z", "--cached"]
    if not args.tracked_only:
        command += ["--others", "--exclude-standard"]
    files = subprocess.check_output(command, cwd=ROOT).decode().rstrip("\0").split("\0")
    ledger = ROOT / "doc/reviews/document-moves.csv"
    archives = {row["new_path"] for row in csv.DictReader(ledger.open())
                if row["new_path"].startswith("doc/reviews/repository/")}
    errors, local, checked = check(ROOT, files, archives)
    for error in errors:
        print(error)
    print(f"Checked {checked} local links; {len(errors)} errors; "
          f"{len(local)} declared local-only targets; {len(archives)} historical pages excluded.")
    return bool(errors)


if __name__ == "__main__":
    raise SystemExit(main())
