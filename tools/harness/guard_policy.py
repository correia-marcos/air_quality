"""Shared advisory policy. Filesystem sandboxing remains the enforcement boundary."""
import re
import shlex
from pathlib import Path

EXEMPT = ("config_utils_", "setup_packages.r", "registry.r", "theme_paper.r")
SECRET = re.compile(r"(?:^|/)(?:\.env(?!\.example(?:$|/))(?:\.[^/]*)?|\.renviron|\.netrc)$", re.IGNORECASE)
GIT_READ_COMMANDS = frozenset({
    "status", "diff", "log", "show", "blame", "ls-files", "ls-tree", "rev-parse",
    "rev-list", "cat-file", "diff-tree", "diff-index", "grep", "shortlog",
    "describe", "check-ignore", "version", "help", "--version", "--help",
})
GIT_DENIAL = (
    "Git is inspection-only for agents: do not stage, commit, push, or mutate history. "
    "Recommend commit names and file groupings for the human; do not request a bypass."
)


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
    if p == ".git" or p.startswith(".git/"):
        return "deny", GIT_DENIAL
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
    if p.startswith("src/") and p.endswith(".r") and not any(x in p for x in EXEMPT) and re.search(r"^(setwd|set\.seed|dir\.create|ggsave|writeLines|saveRDS|theme_set)\s*\(", content, re.MULTILINE):
        return "context", "Keep top-level execution in scripts/; retain documented setup exceptions."
    return None, None


def git_arguments_allowed(args):
    """Recognize inspection commands without consulting or executing Git aliases."""
    while args:
        option = args[0]
        if option in ("-C", "--git-dir", "--work-tree"):
            if len(args) < 2:
                return False
            args = args[2:]
        elif option.startswith(("--git-dir=", "--work-tree=")) or (
            option.startswith("-C") and len(option) > 2
        ) or option in ("--no-pager", "--no-optional-locks", "--literal-pathspecs"):
            args = args[1:]
        else:
            break
    if not args:
        return False
    if args[0] == "config":
        # Explicit legacy read forms only; config writes and overrides can define aliases.
        return len(args) > 1 and args[1] in (
            "--get", "--get-all", "--get-regexp", "--list", "-l",
        )
    return args[0] in GIT_READ_COMMANDS


def git_shell_denial(command, depth=0):
    """Inspect common literal invocations, not arbitrary shell/program semantics."""
    if depth > 8:
        return "Shell nesting exceeds the guard's inspection limit. Simplify the command."
    try:
        lexer = shlex.shlex(command, posix=True, punctuation_chars=";&|()\n")
        lexer.whitespace = " \t\r"
        segments = [[]]
        for token in lexer:
            if token and all(c in ";&|()\n" for c in token):
                segments.append([])
            else:
                segments[-1].append(token)
    except ValueError:
        return "Shell text could not be inspected. Use a simpler, well-formed command."
    shells = {"sh", "bash", "zsh", "dash"}
    wrappers = {"env", "command", "exec", "sudo", "time", "timeout", "nohup"}
    for args in segments:
        while args and re.match(r"^[A-Za-z_][A-Za-z_0-9]*=", args[0]):
            args = args[1:]
        if not args:
            continue
        # Only inspect argument positions as executables for recognized wrappers.
        positions = range(len(args)) if Path(args[0]).name in wrappers else range(1)
        for i in positions:
            executable = Path(args[i]).name
            if executable in ("git", "git.exe"):
                if not git_arguments_allowed(args[i + 1:]):
                    return GIT_DENIAL
                break
            if executable in shells:
                for j in range(i + 1, len(args) - 1):
                    if args[j].startswith("-") and "c" in args[j][1:]:
                        reason = git_shell_denial(args[j + 1], depth + 1)
                        if reason:
                            return reason
                        break
                break
    return None


def shell_decision(command):
    # Text inspection is intentionally not advertised as filesystem-effect analysis.
    if not isinstance(command, str) or not command.strip():
        return "deny", "Missing shell command text; the guard cannot inspect this call."
    git_reason = git_shell_denial(command)
    if git_reason:
        return "deny", git_reason
    if re.search(r"\brm\s+-[A-Za-z]*r[A-Za-z]*f|\brm\s+-[A-Za-z]*f[A-Za-z]*r", command):
        return "prompt", "Destructive command: use explicit scoped approval or a reversible alternative."
    if re.search(r"(?<![\w.])(?:\.env(?!\.example\b)(?:\.[\w.-]+)?|\.Renviron|\.netrc)(?![\w.])", command, re.IGNORECASE):
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
