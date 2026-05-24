# Agent Instructions

This is a **version-controlled Emacs configuration directory**. A symlink from the recognized Emacs config location (~/.config/emacs or similar) points to this repo. Changes made here take effect **the next time Emacs starts**, not immediately.

This project uses **bd** (beads) for issue tracking. Run `bd prime` for full workflow context.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work atomically
bd close <id>         # Complete work
bd dolt push          # Push beads data to remote
```

## Non-Interactive Shell Commands

**ALWAYS use non-interactive flags** with file operations to avoid hanging on confirmation prompts.

Shell commands like `cp`, `mv`, and `rm` may be aliased to include `-i` (interactive) mode on some systems, causing the agent to hang indefinitely waiting for y/n input.

**Use these forms instead:**
```bash
# Force overwrite without prompting
cp -f source dest           # NOT: cp source dest
mv -f source dest           # NOT: mv source dest
rm -f file                  # NOT: rm file

# For recursive operations
rm -rf directory            # NOT: rm -r directory
cp -rf source dest          # NOT: cp -r source dest
```

**Other commands that may prompt:**
- `scp` - use `-o BatchMode=yes` for non-interactive
- `ssh` - use `-o BatchMode=yes` to fail instead of prompting
- `apt-get` - use `-y` flag
- `brew` - use `HOMEBREW_NO_AUTO_UPDATE=1` env var

## Directory Survey

Subdirectories under `lisp/`:

| Path | Files | Purpose |
|------|-------|---------|
| `lisp/config/` | 6 | Core config: elpaca bootstrap, env, bindings, defuns, autocompletion, minor modes |
| `lisp/tools/` | 5 | Formatters, terminal, MCP server, VC (magit/transient), prodigy |
| `lisp/langs/` | 18 | Per-language configs (cc, clojure, css, elixir, emacs-lisp, haskell, java, js, perl, ruby, rust, sql, tex, ts, misc data modes, misc major modes) |
| `lisp/org/` | 1 | Org mode configuration |

Other top-level directories:

- `site/` — Site-local libraries (vendored `.el` files)
- `elpaca/` — Package manager repos and builds (gitignored)
- `backups/` — Backup files
- `snippets/` — Snippet files

**Key tools used:** `use-package` for declarative config, `elpaca` for package management, Fira Code ligatures, tree-sitter, `metalheart` theme.

## Key Conventions

- **`use-package-always-ensure t`** — set in `my-elpaca.el`. Every `use-package` gets implicit `:ensure t`. Use `:ensure nil` for built-in packages or when providing an explicit `:elpaca` recipe (must appear before `:elpaca` in the form to prevent elpaca from double-queuing).
- **`lexical-binding: t`** — all `.el` files use the `-*- lexical-binding: t; -*-` cookie.
- **Startup order:** elpaca bootstrap → global deps → env/bindings/autocompletion → org → minor modes → tools → language modes → site libs. `elpaca-wait` is called after each phase.
<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:ca08a54f -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->
