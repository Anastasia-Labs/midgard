# Structural N/A claim — <coverage row>

TEMPLATE — a real claim is a matrix row plus an *executable* adversarial
test; prose alone is insufficient (GOAL_SPEC.md §9.1).

- Coverage row: `<matrix section / row id>`
- Claim kind: `unrepresentable | L1-enforced | reduces-to <family>`
- Normative basis: `<spec/decision citation>`

## Executable adversarial evidence

- Test: `<exact module + test name>` — constructs the allegedly impossible
  state (`<what it builds>`) and proves the structural guard rejects it at
  `<validator/codec path>`.
- Command: `cd onchain/aiken && node scripts/run-focused-check.mjs <module> <test>`
  (or the focused TS suite) — collected exactly `<N>/<N>`.
- Mutation control: weakening `<the guard>` makes the test construct the
  state successfully, proving the test is load-bearing.

## Removal

Unreachable proof surface removed at `<paths>` (or: none existed). Matrix row
set to `N/A` citing this file.
