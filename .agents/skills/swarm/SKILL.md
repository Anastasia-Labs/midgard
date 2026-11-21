---
name: swarm
description: "Partition a task into independent coverage slices or comparison arms and synthesize their evidence."
disable-model-invocation: true
---

# Swarm

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

1. Define the done predicate and coverage matrix. Choose separate slices, identical racing briefs, or both. State the selection rule (`first pass`, `rank all`, or `best-of`) before launch. [review]
2. Resolve `swarm workers` from `.agents/pstack-models.json`. Size the worker count to the distinct slices and the host's real concurrency limit. Give writers separate worktrees. Measurements name exact SHAs, sample count, sample definition, and trial order. [review]
3. Dispatch self-contained briefs through the adapter. Each includes scope, artifacts, verification, and a result of `PASS`, `ISSUES`, or `BLOCKED` with evidence. Report every proven issue in the slice. [review]
4. Drain all required seats. Validate receipts against the brief. Retry one missing or malformed result with consolidated scope; a second miss is a gap. Gaps do not count as passes. [review]
5. Aggregate overlapping findings and apply the declared race rule. Return a compact table with evidenced issues, gaps, actual providers/models, and the done predicate's state. [review]
