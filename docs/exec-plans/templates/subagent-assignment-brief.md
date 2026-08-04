# Task assignment brief — F05

WORKED TEMPLATE ARTIFACT (GOAL_SPEC.md §5.1). This concrete structure example
is excluded from evidence aggregation. A dispatched brief must quote the
validated manifest row and remain self-contained.

## Task

- Manifest row: `F05`, `detailStatus: DETAILED`, from
  `canonical-v1-goal-task-manifest-v1.json`
- Acceptance: the canonical manifest has the exact 186-task GOAL_SPEC set and
  passes the strict assignment-quality verifier.
- Current state: the manifest and verifier exist; F41 remains the only current
  non-PASS dependency recorded by the F05 row.

## Lease

- Writable paths:
  `docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json` and
  `demo/scripts/verify-canonical-v1-goal-task-manifest-quality.mjs` — edit
  nothing else.
- Prohibited: GOAL_SPEC.md, GOAL_PROGRESS.md, onchain/aiken/plutus.json,
  registries/matrices/manifests, package manifests/lockfiles, any path
  outside the lease. Never stage, commit, or push.

## Required verification

- `node demo/scripts/verify-canonical-v1-goal-task-manifest-quality.mjs --json`
  — report exact collected/passed counts; a filter that
  collects zero is a failure, not a pass.

## Return

1. What changed (per file, one line each).
2. Exact test/command results (counts, durations, exit codes).
3. Anything discovered outside the lease (report, do not fix).
4. Open risks or blockers.
