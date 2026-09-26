---
name: committing-safely
description: Commit exactly your own paths in a Midgard working tree that other sessions and agents share, without sweeping up their staged work, triggering the stash-everything pre-commit shim, or committing the blueprint. Use when about to stage, commit, or push; when the tree or index holds changes that are not yours; when committing only some hunks of a file; when deleting or cleaning files picked out of `git status`; or when a commit swept, reverse-staged, or lost someone else's changes.
---

# Committing safely

This working tree is shared. Several sessions and agents edit, stage and commit
in it at once, so **the index is not yours** and neither is every modified
file. A safe commit is _exact_: it contains your paths, only your paths, and
leaves everything else — staged, unstaged or untracked — as it found it.

Every rule below names what enforces it. `[review]` means nothing but a reader
does; those are the ones that need you.

## The default: `commit-paths`

```bash
node .agents/skills/committing-safely/scripts/commit-paths.mjs \
  -m "Imperative subject line" -m "Body paragraph explaining why." \
  -- path/one.ts path/two.ak
```

It builds a temporary index from `HEAD`, adds exactly the named paths, checks
their formatting, commits, and resets the real index for those paths only.
Other sessions' staged and unstaged work stays out of the commit and
untouched. It runs no git hooks at all, so the Nix stash shim cannot fire.

Refusals (nothing is committed, exit 1): a directory or the repository root,
a path outside the repository, `onchain/aiken/plutus.json`, a path with no
change against `HEAD`, a path whose real-index entry is a staged version that
is neither `HEAD`'s nor the one being committed, unformatted content, an
`aiken` that is not the pinned fork, an in-progress merge/rebase/cherry-pick/
revert, a message carrying a tool attribution trailer, and `HEAD` moving
while it ran. No paths or no message is a usage error (exit 2).

Formatting is checked on exactly the content being committed, not
rewritten: `demo/**/*.{ts,tsx,md}` against demo's prettier, `*.ak` against the
pinned fork's formatter with the trailing-whitespace normalization the
`aiken-ci.yml` "Run normalized Aiken auto-formatter check" step applies. A
missing tool exits 3 ("could not check", nothing committed); re-run with
`--allow-unchecked` only when you have formatted by other means.

Options: `-F <file>` for the message, `--patch <file>` to commit only the hunks
in a diff against `HEAD` (see the hunk recipe), `--dry-run` to see the stat
without committing. Exit 4 means the commit landed but the index reset failed;
the output says what to run.

The completion criterion for any commit: `git show --stat HEAD` lists only
your paths, and `git status --short -- <your paths>` prints nothing unless you
deliberately left hunks uncommitted.

## Rules

`commit-paths` below is
`.agents/skills/committing-safely/scripts/commit-paths.mjs`.

1. **Stage and commit explicit paths only.** Never `git add -A`, `git add .`,
   `git add -u`, `git commit -a`, or a pathspec naming a directory: each
   sweeps up files another session staged or edited.
   `commit-paths` accepts only explicit file paths. With plain git nothing
   stops a sweep; the hook refuses only the blueprint. [review]
2. **Never derive a destructive command's arguments from `git status`.**
   Status lines carry prefixes (`A `, ` M`, `??`, `R  old -> new`); on
   2026-09-09 status prefixes became `rm` arguments and deleted real files.
   List candidates with `git ls-files --others --exclude-standard` or `ls`,
   read the list, then delete by explicit path. [review]
3. **`onchain/aiken/plutus.json` is never committed.** It is a build output
   of whichever compiler and deployment profile last ran, and it is not
   gitignored, so a sweep picks it up. The hook refuses it when staged
   (`.githooks/pre-commit` lines 49–60); `commit-paths` refuses it. Blind
   spots: `MIDGARD_SKIP_HOOKS=1` and `--no-verify` bypass the hook, a clone
   without `bash .githooks/install` has no hook, and no CI step checks for a
   tracked blueprint. [hook: pre-commit]
4. **Keep the Nix `pre-commit.local` shim away from a shared tree.** In the
   main checkout the repository hook ends by running
   `.git/hooks/pre-commit.local`, a pre-commit framework shim that stashes
   every unstaged tracked file (`git checkout -- .`) while its hooks run and
   re-applies them afterwards, whatever is staged. An edit another session
   makes in that window can be lost. The shim does not run in linked
   worktrees or under `MIDGARD_SKIP_HOOKS=1`; `commit-paths` runs no hooks.
   Details in [references/pre-commit-hook.md](references/pre-commit-hook.md).
   [review]
5. **When hooks are skipped, format first.** `MIDGARD_SKIP_HOOKS=1` also skips
   the hook's prettier, eslint and `aiken fmt`. `commit-paths` refuses
   unformatted TS/MD/`.ak` content but does not run eslint. CI catches the
   rest after the push: `midgard-node-ci` "Lint the demo workspace", and
   `aiken-ci` "Run normalized Aiken auto-formatter check" for `.ak`.
   [ci: midgard-node-ci/Format-check the demo workspace]
6. **Commit through a temporary index when the tree holds other sessions'
   work, then resync the real index for exactly your paths** so they do not
   show as reverse-staged. `commit-paths` does this; by hand, see
   [references/manual-recipes.md](references/manual-recipes.md). [review]
7. **Commit only your hunks of a file others also edited:**
   `git diff HEAD -- <file> > mine.patch`, delete the hunks that are not
   yours, then `commit-paths --patch mine.patch -m ...`.
   `commit-paths` applies the patch to `HEAD` in the temporary index and
   leaves the rest of the file unstaged in the tree. [review]
8. **No tool attribution** in commits or PRs: no tool
   `Co-Authored-By` trailers, no "Generated with" lines, no mention in titles
   or descriptions. `commit-paths` refuses the common trailer and "Generated
   with" forms in its own messages. Blind spot: PR text and plain
   `git commit`. [script: .agents/skills/committing-safely/scripts/commit-paths.mjs]
9. **Write messages the way this repository does.** From the last 300
   non-merge commits (as of 2026-09-25): an imperative sentence subject, no
   trailing period, no `type:` prefix (24 of 300 use one), median 66
   characters; a body that says why the change exists and what was wrong
   before; issue links as a `(#NNN)` subject suffix or `Closes #NNN` /
   `Refs #NNN` in the body. [review]
10. **Do not push unless told to, and never push to `main`.** As of
    2026-09-25 `main` has no branch protection on GitHub
    (`gh api repos/Anastasia-Labs/midgard/branches/main` reports
    `"protected": false`), so nothing but you stops it. [review]

## When something already went wrong

Read [references/manual-recipes.md](references/manual-recipes.md) when a commit
swept a foreign file, a committed path shows as reverse-staged, or you need
the temporary-index or hunk recipe without the script.

Read [references/pre-commit-hook.md](references/pre-commit-hook.md) when a
commit was blocked, formatting was skipped, or files changed or reverted
during a commit.

Hook installation and the rest of the contributor rules live in
[AGENTS.md](../../../AGENTS.md).
