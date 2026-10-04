# Worktree cleanup

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Read git worktree list --porcelain and disk usage. Resolve paths from git instead of guessing host-managed worktree locations.

2. Inspect every candidate for dirty/untracked/ignored work, unpushed commits, open PRs, active sessions, and owned running processes. An unknown owner or state is a hold.

3. Present concrete reclaimable paths and reasons. Apply deletion only within the user's cleanup authorization, using git worktree remove without force.

4. Preserve task evidence, state directories, and all unrelated work. Report removed paths, reclaimed space, and held candidates.
