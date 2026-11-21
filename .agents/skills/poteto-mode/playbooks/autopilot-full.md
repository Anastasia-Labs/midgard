# Autopilot full

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Pin the independent PR queue, acceptance bar, authorized merge scope, and dependency order.

2. Assign each unit one owner in an isolated worktree. The owner implements, verifies, creates the requested PR, and responds to confirmed CI/review findings.

3. Independently review every current merge-ready head. Attach required check and runtime receipts to that exact SHA.

4. Apply the shipping gate before an authorized merge. A later push requires a new verdict for its changed patch.

5. Reconcile all queued units and workers. Return merged/open PRs, exact validation, gaps, and attention items.
