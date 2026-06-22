# Task 09: Conformance, Budget, Integration, And Spec Completion

Status: launch-gate implementation in progress as of 2026-06-20. The live
evidence is recorded in `.codex/transition-trace-task-09/plan.md`.

## Goal

Turn the transition-trace work from an implementation draft into a launch gate.

This task is complete when the spec, TypeScript, Aiken, DA committee, block
producer, challenger tooling, and e2e flows all agree on the same commitments
and all proof budgets are recorded.

## Files To Update

Spec and architecture:

- `demo/midgard-node/docs/TRANSITION_TRACE_COMMITMENTS.md`
- `demo/midgard-node/docs/transition-trace-implementation/*.md`
- `technical-spec/`

Golden fixtures:

- `demo/midgard-node/tests/sdk-abi-fixtures.test.ts`
- Add fixture JSON/CBOR files under `demo/midgard-node/tests/fixtures/`.

Integration tests:

- `demo/midgard-node/tests/confirm-block-commitments.test.ts`
- `demo/midgard-node/tests/deposit-flow-emulator.test.ts`
- `demo/midgard-node/tests/operator-lifecycle-emulator.test.ts`
- `demo/midgard-node/tests/da-attestation.test.ts`
- `demo/midgard-node/tests/e2e-da-gates.test.ts`
- `demo/da-committee-node/tests/payload.test.ts`
- `demo/midgard-fault-proofs/tests/`

Build and deployment:

- `demo/docker-compose.dev.yaml`
- deployment/config files that pin protocol version or header shape
- Aiken script hash snapshots or generated artifacts
- clean redeploy runbooks under `docs/agents/` if they mention header roots

## Required Test Matrix

### ABI Conformance

Golden fixtures for:

- `HeaderV2` / current `Header`
- `ForcedInclusionTx`
- `TransitionStep`
- `EventKey`
- `EventToStepValue`
- `DaPayloadBodyV2`
- every transition fraud-proof redeemer

The JSON/CBOR fixture file is checked from TypeScript. The existing Aiken test
framework in this repository uses embedded Aiken fixtures rather than importing
external JSON fixture files, so Aiken conformance is covered by source-level
tests for the same transition-trace proof families and schemas.

### Ordering And Counts

Tests for:

- withdrawals before forced txs
- forced txs before normal L2 txs
- normal L2 txs before deposits
- phase boundary step indexes
- `total_event_count` sum
- `transition_step_count == total_event_count`
- trace count proof matches header

### Validity Semantics

Tests for:

- invalid withdrawals included as no-op steps
- invalid forced transactions included as no-op steps
- invalid normal L2 transaction requests excluded before commitment
- deposits always effectful when included
- duplicate normal L2 `tx_id` handling
- duplicate forced transaction order handling

### Negative Fault Tests

Tests for:

- omitted withdrawal
- omitted forced tx order
- omitted deposit
- duplicate event key
- dangling event-to-step entry
- dangling trace event
- trace/event mismatch
- trace link mismatch
- wrong final root
- wrong count
- out-of-window L1 source event

### Deposit Phase Regression

Regression test:

```text
same-block deposit spending is rejected
```

This must remain true unless the protocol phase order is deliberately changed
in the spec and all validators.

### Budget Tests

For each Aiken fault proof:

- record CPU and memory budget
- record witness size
- add failure thresholds if the existing test framework supports them

Current Task09 evidence:

- `/home/gumbo/.aiken/bin/aiken check -m midgard/fraud_proofs/transition_trace/proof --plain-numbers`
  passed 23/23 transition-trace proof tests.
- Largest observed proof-test budget: `accepts_l2_transaction_transition_fault`
  at CPU `614855610`, memory `1362484`.
- Largest observed deposit proof budget: `accepts_valid_deposit_transition_fault`
  at CPU `540951867`, memory `1083744`.
- Valid forced transactions remain fail-closed:
  `rejects_valid_forced_transaction_unsupported_transition_fault` passed at CPU
  `213347707`, memory `221458`.
- Largest golden `.proof` fixtures are `invalid-forced-no-op` and
  `valid-forced-unsupported` at `1178` bytes; largest corresponding
  `.continue-redeemer` fixtures are `1190` bytes.
- No established local threshold pattern was found, so arbitrary budget
  thresholds were not added in Task09.

## Verification Commands

Run the full launch gate:

```sh
cd demo && pnpm run build
cd demo && pnpm run typecheck
cd demo && pnpm run test
cd demo && pnpm run lint
cd demo && pnpm run format-check
cd ../onchain/aiken && aiken build
make spec
```

Run the Midgard e2e acceptance flow after a clean redeploy, because this is a
header and datum shape change.

## Documentation Exit Criteria

Before launch, the architecture doc must explicitly state:

- exact source roots and key/value shapes
- exact phase order
- exact trace step schema
- exact count invariants
- exact event-to-step invariant
- which events are obligatory
- which invalid events become no-op steps
- which invalid events are excluded
- same-block deposit spending rule
- every fault family and its witnesses

## Code Exit Criteria

- No production path still constructs the old header shape.
- DA committee validation covers every committed root and count.
- Fraud proof validators cover every transition-trace invariant.
- Challenger tooling can build proofs without trusted operator data.
- All new Aiken scripts have budget evidence.
- Clean redeploy instructions are updated and tested.
