# Orchestrate

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Confirm the program spans independently deliverable units beyond a single bounded task. State scope, done predicate, dependencies, authorization, and concurrency limit.

2. Create a task-local program record with one owner per unit, branch/worktree, current SHA, child IDs, state, required checks, and evidence paths. A single coordinator writes aggregate state; workers publish separate receipts.

3. Pilot one unit through implementation, verification, and the requested delivery boundary. Improve briefs and unit size from the pilot before expanding.

4. Dispatch a rolling window of native workers through the host adapter. Consolidate updated user directives into every fresh brief. Use external CLI seats only for read-only design or review.

5. Drain completed work, inspect artifacts, and integrate verified increments. Recompute stack bases and SHAs from git/GitHub after each mutation; record missing evidence as a gap.

6. Close by reconciling all workers to terminal states and checking the program predicate. Preserve the record for handoff. A restarted host re-discovers actual processes and PRs; it does not assume children survived.
