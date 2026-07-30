# Subagent assignment brief — <TASK-ID>

TEMPLATE (GOAL_SPEC.md §5.1). Quote the task's manifest row; the brief must
be self-contained — the agent has no other context.

## Task

- Manifest row: `<paste the READY row from canonical-v1-goal-task-manifest-v1.json>`
- Acceptance (verbatim from GOAL_SPEC.md): `<...>`
- Current state (from GOAL_PROGRESS.md): `<what exists, what remains>`

## Lease

- Writable paths: `<exact list>` — edit nothing else.
- Prohibited: GOAL_SPEC.md, GOAL_PROGRESS.md, onchain/aiken/plutus.json,
  registries/matrices/manifests, package manifests/lockfiles, any path
  outside the lease. Never stage, commit, or push.

## Verification you must run

- `<focused commands>` — report exact collected/passed counts; a filter that
  collects zero is a failure, not a pass.

## Return format

1. What changed (per file, one line each).
2. Exact test/command results (counts, durations, exit codes).
3. Anything discovered outside the lease (report, do not fix).
4. Open risks or blockers.
