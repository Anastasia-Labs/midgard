---
name: show-me-your-work
description: "Keep an evidence-backed TSV decision trail for long tasks, handoffs, and later review."
disable-model-invocation: true
---

# Show me your work

Read [the host adapter](../poteto-mode/references/host-adapter.md) before accessing history or dispatching a reviewer.

Keep one append-only TSV per task with columns `ts`, `phase`, `decision`, `why`, `evidence`, and `result`. Each cell is one line. Evidence is a resolving file, commit, PR, trace, or command-output reference. A mistaken row gets a superseding row.

Use the Node helper:

```bash
node .agents/skills/show-me-your-work/scripts/log.mjs <logfile> <phase> <decision> <why> <evidence> <result>
```

It creates the header, stamps UTC time, strips cell tabs/newlines, and escapes spreadsheet formulas. Keep a single writer. Use a task-specific scratch path by default; commit a sanitized trail only when the requested deliverable needs it.

Log meaningful forks, verified units, pivots, reversions, and blockers. A new conversation or replacement agent starts with phase `start`, naming its run and the range it did not write. On a later turn, check whether another run appended before continuing.

Before handoff, reconcile this run's rows with actual artifacts and its scoped transcript or task digest. Check that each decision occurred and each evidence pointer supports its claim. Supersede incorrect rows and fill significant gaps.

For consequential work, obtain an independent review of the trail using the adapter. Flag weak evidence, skipped checks, scope changes, and risky decisions. If the second provider or transcript is unavailable, state that gap and review artifact receipts directly. Return the trail path, actual review provider, and actionable attention items.
