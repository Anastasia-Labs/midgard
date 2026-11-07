# Autopilot stack

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Pin the queue and one linear base-branch chain for the user to review and land.

2. Implement and verify each unit with one writer per branch/worktree. Each child starts from its parent tip and targets that parent branch.

3. Review the actual patch and required checks for each head. Restack using ordinary git or the user-selected stack tool; update receipt SHAs after changes.

4. Deliver the stack with ordered PR links, bases, head SHAs, checks, and remaining issues. Preserve the user-owned landing gate.
