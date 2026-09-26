# Manual recipes and recovery

Use `commit-paths` when you can: it does all of this, checks formatting, and
moves `HEAD` with a compare-and-swap so a commit another session lands
mid-run is never reverted. The recipes below are what it does, for when the
script is unavailable or you need to see the steps. They were run in a
throwaway repository on 2026-09-25.

## Commit through a temporary index

```bash
# 1. Format your files first; this commit skips the hook that would.
#    demo TS/MD:  (cd demo && ./node_modules/.bin/prettier --write <paths relative to demo/>)
#    .ak:         node onchain/aiken/scripts/pinned-compiler.mjs && aiken fmt <files> \
#                   && sed -i 's/[[:space:]]\+$//' <files>

# 2. Build a private index from HEAD and stage only your paths into it.
base=$(git rev-parse HEAD)
tmp=$(mktemp -d)/index                      # must be an absolute path
GIT_INDEX_FILE=$tmp git read-tree HEAD
GIT_INDEX_FILE=$tmp git add -- path/one.ts path/two.ak

# 3. Check the commit contains your paths and nothing else.
GIT_INDEX_FILE=$tmp git diff --cached --stat HEAD

# 4. Commit, only if nobody committed since step 2 (git commit takes whatever
#    HEAD is now as the parent, so a moved HEAD would be silently reverted).
test "$(git rev-parse HEAD)" = "$base" &&
  GIT_INDEX_FILE=$tmp MIDGARD_SKIP_HOOKS=1 git commit -m "Subject" -m "Body"

# 5. Resync the real index for exactly those paths, then clean up.
git reset -q -- path/one.ts path/two.ak
rm -rf "$(dirname "$tmp")"
```

Step 4 still has a small window between the test and the commit; the script
closes it with `git update-ref HEAD <new> <base>`, which fails if `HEAD` moved.

Why step 5: the real index still holds the old `HEAD` version of your paths,
so right after the commit `git status` shows them as staged changes that
would undo the commit (`MM path`). `git reset -q -- <paths>` sets those
entries to the new `HEAD`; the other entries in the real index are untouched.

Before starting, check the real index for your paths:
`git diff --cached --stat -- <paths>`. If it shows a staged version you did
not stage, it is someone else's work; step 5 would discard it from the index.
`commit-paths` refuses in that case.

## Commit only your hunks of a shared file

```bash
git diff HEAD -- demo/midgard-node/src/x.ts > /tmp/mine.patch
# Edit /tmp/mine.patch: delete the hunks that are not yours. Inside a hunk you
# may turn a '-' line into a ' ' context line and delete a '+' line;
# `--recount` fixes the hunk headers.
GIT_INDEX_FILE=$tmp git apply --cached --recount /tmp/mine.patch
```

Then continue from step 3 above. Diff against `HEAD`, not the index: the
temporary index starts from `HEAD`, and `git diff` (worktree against the real
index) will not apply if anything is staged for that file. With the script:
`commit-paths --patch /tmp/mine.patch -m "..."`.

After the commit the rest of the file's changes remain unstaged in the tree.

## Recovery

- **Committed paths show as staged after a temporary-index commit**: the
  resync was skipped. `git status --short` shows `MM` for a modified path,
  `D ` plus `??` for an added one, `AD` for a deleted one. Run
  `git reset -q -- <the committed paths>`.
- **A commit swept up someone else's files and is not pushed.** If it is
  still `HEAD`, `git reset --soft HEAD^` moves `HEAD` back without touching
  the tree. The index keeps the swept commit's content, so files that were
  unstaged or partially staged before are now fully staged (a partially staged
  file's earlier staged version is gone from the index). Recommit your own
  paths with `commit-paths`, and tell the owner of the other files rather than
  guessing what to unstage.
- **It is pushed.** Do not rewrite shared history. Report it and wait for a
  decision; a revert is a new commit and needs the same care.
- **Files were deleted from a status-derived list.** Stop, and commit nothing
  until they are back: a sweep would record the deletions.
  `git restore -- <path>` brings a file back from the index, which still holds
  its staged version if it had one; untracked files do not come back from git.
