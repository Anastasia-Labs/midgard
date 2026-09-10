# TQ-16 — Consolidate repeated setup while preserving scenario coverage

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit section**: §7
- **Rules**: R8, R9 in [principles](principles.md)
- **Coordination**: TQ-15 may share setup work; it does not authorize deleting
  live measurements or require this task to wait for every fit-ledger change.

## Problem and boundary

Copied family setup can drift and increase maintenance cost. Similar-looking
cases can also intentionally test different validators, entry points, fixture
controls, or resource limits. Treat the original file counts and similarity
estimates as leads to recheck, not proof that the suites are duplicates.

A shared assertion helper does not make omission a compile error: callers can
simply fail to call it. A single-row `it.each` can be a readable, typed scenario
form and is not itself a defect. Summed timeout allowances are not measured
runtime.

## Focused candidates

- [Withdrawn-input honest refusal](../../demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawn-input-honest.test.ts)
  and [invalid-withdrawal refusal](../../demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawn-input-invalid-withdrawal.test.ts)
  share a step-03 submission pattern but intentionally use different evidence.
  A named shared driver may remove setup duplication while retaining both
  scenario names, refusal oracles, and surviving-thread checks.
- The five `submit-init-emulator-*-tier2.test.ts` family suites: enumerate each
  family's required carriage and negative-control cases before aligning them.
  A missing shared property needs a scenario-coverage check, not merely a
  helper function that suites may omit.
- `prepare-*.test.ts`, fabricated deposit/withdrawal scenarios, and repeated
  inspection assertions: share setup or a typed driver only where inputs and
  observations can remain clear at the call site. Verify current executable
  and diagnostic boundaries before treating preparers as interchangeable.
- Repeated canonical serialization helpers may be shared, while independent
  expected wire vectors must remain independent. Text-byte equality and
  parsed-value equality may test different contracts.
- Aiken cases wrapping different compiled validators or collecting distinct
  cost readings are not duplicates solely because their bodies look alike.

## Work and acceptance

1. Select one bounded family and record its required scenario set, fixtures,
   asserted outcomes, and current CI lane.
2. Introduce a readable descriptor/driver only if it reduces maintenance
   without obscuring the claim. Preserve isolation between mutable emulator
   scenarios; shared setup must not leak chain or wallet state across cases.
3. Where common coverage is mandatory, validate the descriptor set against an
   independently declared requirement and ensure every row is executed. A
   missing helper call is not automatically rejected by TypeScript.
4. Keep failure output specific to the affected family and scenario. Run the
   relevant suite and record observed runtime only when performance is part
   of the motivation, including effects on file-level parallelism.
5. Retain a single-row table or duplicated local assertion when it improves
   readability or independent coverage. Neither fewer lines nor fewer files
   is an acceptance criterion by itself.
