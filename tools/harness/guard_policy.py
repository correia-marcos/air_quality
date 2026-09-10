"""Shared advisory policy. Filesystem sandboxing remains the enforcement boundary."""
from pathlib import Path
import re

EXEMPT = ("config_utils_", "setup_packages.r", "registry.r", "theme_paper.r")
SECRET = re.compile(r"(?:^|/)(?:\.env(?!\.example(?:$|/))(?:\.[^/]*)?|\.renviron|\.netrc)$", re.I)


def relative_path(path, root):
    candidate = Path(path)
    if not candidate.is_absolute():
        candidate = Path(root) / candidate
    try:
        return candidate.resolve().relative_to(Path(root).resolve()).as_posix().lower()
    except ValueError:
        return candidate.resolve().as_posix().lower()


def edit_decision(path, content, root):
    """Return severity and explanation; structural feedback is not a hard block."""
    p = relative_path(path, root)
    if SECRET.search(p) or p == "renv.lock" or any(
        p == folder or p.startswith(folder + "/")
        for folder in ("data/raw", "data/downloads", "data/_legacy")
    ):
        return "deny", "Input of record or credential: use a separately authorized acquisition/dependency procedure."
    if p in ("agents.md", "claude.md", ".claude/settings.json") or p.startswith((".codex/", ".agents/")):
        return "context", "Project configuration change: check existing user authorization and preserve shared guidance."
    if p.startswith("scripts/") and p.endswith(".r") and re.search(
        r"^[A-Za-z._][\w.]*\s*<-\s*function\s*\(", content, re.M
    ):
        return "context", "Reusable R functions belong in src/; scripts/ executes them."
    if p.startswith("src/") and p.endswith(".r") and not any(x in p for x in EXEMPT):
        if re.search(r"^(setwd|set\.seed|dir\.create|ggsave|writeLines|saveRDS|theme_set)\s*\(", content, re.M):
            return "context", "Keep top-level execution in scripts/; retain documented setup exceptions."
    return None, None


def shell_decision(command):
    # Text inspection is intentionally not advertised as filesystem-effect analysis.
    if re.search(r"\brm\s+-[A-Za-z]*r[A-Za-z]*f|\brm\s+-[A-Za-z]*f[A-Za-z]*r|\bgit\s+reset\s+--hard|\bgit\s+push\b.*--force", command):
        return "prompt", "Destructive command: use explicit scoped approval or a reversible alternative."
    if re.search(r"(?<![\w.])(?:\.env(?!\.example\b)(?:\.[\w.-]+)?|\.Renviron|\.netrc)(?![\w.])", command, re.I):
        return "deny", "Command names a credential file; avoid printing or rewriting credentials."
    if re.search(r"\bdata/(raw|downloads|_legacy)/|\brenv\.lock\b", command):
        return "context", "Inputs of record: reads are allowed; preserve contents. Shell text cannot prove write safety."
    return None, None


def patch_edits(patch):
    """Parse every add/update/delete/move path; examine added content, not removed text."""
    edits = []; path = None; added = []
    for line in patch.splitlines():
        if line.startswith(("*** Add File: ", "*** Update File: ", "*** Delete File: ")):
            if path is not None:
                edits.append((path, "\n".join(added)))
            path = line.split(": ", 1)[1]; added = []
        elif line.startswith("*** Move to: "):
            if path is None:
                raise ValueError("Move without a source")
            edits.append((path, "\n".join(added)))
            path = line.split(": ", 1)[1]; added = []
        elif line.startswith("+"):
            added.append(line[1:])
    if path is not None:
        edits.append((path, "\n".join(added)))
    if not edits or not patch.startswith("*** Begin Patch") or "*** End Patch" not in patch:
        raise ValueError("Unsupported or malformed patch")
    return edits
