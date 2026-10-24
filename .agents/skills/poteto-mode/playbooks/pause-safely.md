# Pause safely

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Pause only at the user's explicit request. Compaction requires a continuation capsule while the same task remains active.

2. Bring owned writes to a consistent checkpoint. Preserve dirty work and stop only processes owned by this task when the pause requires it.

3. Write a capsule with goal, branch/head, dirty paths and ownership, artifact locations, processes, checks, existing authorization, blockers, and next action.

4. Report the checkpoint and resume instructions. Leave unrelated sessions and deployments untouched.
