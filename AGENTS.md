# Codex instructions

Before working in this repository, read and follow these files in order:

1. `../AGENTS.md` for the shared Codex adaptations.
2. `../CLAUDE.md` in full for the shared R working environment instructions.
3. This repository's `CLAUDE.md` in full for project-specific instructions and
   explicit overrides of the shared defaults.

Resolve these paths relative to this file, not the shell's current directory.
The shared directory is `C:/Rworking` on Windows, `/mnt/c/Rworking` in WSL,
and is also available through `/home/rfisher/Rworking_wsl` on this machine.
For a worktree, locate the shared files relative to the main checkout instead;
`git worktree list` identifies that checkout. If the shared files cannot be
found, report which files are missing before making changes that depend on them.

The Claude files are the authoritative shared instructions for both assistants.
Apply instructions addressed to Claude to Codex where they describe development
behaviour. Attribute prompt-log entries to **Codex**, with the actual model
identifier when available. Preserve existing `<TOPIC>-claude.md` specification
filenames and Claude-directed comment markers.

This repository is the `toxval` R package. Read `DESCRIPTION` before changing
package code or dependencies, and `notes/implementation/02_decisions.md` before
changing a value that `ecx()` or `nsec()` returns. Format R code with `air`,
since `air.toml` is present. Do not edit `NEWS.md`: `fledge` builds it from
commit messages, so the user-visible summary of a change goes in its commit
message.

Claude's hooks do not run automatically in Codex. After writing or editing a
`.md` or `.qmd` file, run the shared check explicitly when available:

```bash
python3 ~/.claude/hooks/prose-check.py <file>
```

Follow the shared prose review requirements as well. If a required check or
skill is unavailable, report that limitation instead of claiming it ran.
