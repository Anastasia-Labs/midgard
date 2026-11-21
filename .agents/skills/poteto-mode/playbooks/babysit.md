# Babysit

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Resolve the PR or stack and declare snapshot versus watch mode. A status question gets a snapshot; an explicit request to bring it to green gets the bounded repair/watch loop.

2. Inspect current head SHA, check conclusions, merge state, and review threads with GitHub read tools or gh. Use [debugging-ci-failures](../../debugging-ci-failures/SKILL.md) before repairing failed CI.

3. Triage automated review findings using [the review guidance](../references/bugbot-triage.md). Assess each against intent and evidence.

4. Fix in-scope confirmed issues, verify, and update only the owned branch. Refresh the head after every mutation; stale receipts do not prove the new head.

5. Repeat until merge-ready or a real external blocker. Report exact head, checks, review disposition, and merge readiness. Merge only if authorized.
