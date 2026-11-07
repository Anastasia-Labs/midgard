# Lane worktrees

```sh
node scripts/contrib.mjs worktree create --branch lane/name [--base REF] [--package NAME]
node scripts/contrib.mjs worktree setup [--package NAME]
node scripts/contrib.mjs worktree remove --root /path/to/worktree [--force]
```

`create` adds a linked worktree named `midgard-<last branch segment>` under
the worktree root, on a new branch from `--base` (default `HEAD`) or on the
existing branch, then runs `setup` in it. The worktree root is
`MIDGARD_WORKTREE_ROOT`, else `git config midgard.worktreeRoot`, else the
directory holding the main checkout; no path is built in.

`setup` is everything a fresh checkout needs before its suites run, under the
checkout's workspace lease: `pnpm install --frozen-lockfile` in `demo/`, a
ready blueprint (copied from a checkout whose build matches this tree, else
built), and every stale prerequisite dist of the named package, or of every
tested package without `--package`. A brief needs no setup steps of its own;
`contrib test` repeats the blueprint and dist part whenever they go stale.

`remove` refuses a worktree with uncommitted changes (any tracked change or
untracked file except the blueprint `setup` placed) or with commits that no
other branch and no remote-tracking branch contains; `--force` removes it
anyway and reports what it overrode. It then drops the checkout's own test
databases and schemas (`midgard_test_<hash>_*`, `midgard_tools_test_<hash>_*`,
`midgard_contrib_<hash>_*`, where `<hash>` is the checkout's path hash from
`scripts/lib/worktree-identity.mjs`) and runs `git worktree remove`. The
branch is kept. It never removes the main checkout.

`contrib test` names each run's databases `midgard_contrib_<hash>_<random>`
and drops them when the run ends; the receipt's `databaseCleanup` says what
was dropped, or why nothing could be.
