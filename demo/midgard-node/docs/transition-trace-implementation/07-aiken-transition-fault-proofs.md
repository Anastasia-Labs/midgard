# Task 07: Aiken Transition Fault Proofs

## Goal

Implement on-chain fraud-proof validators for transition trace commitments.

This task is complete when each fault family from the architecture document has
an Aiken redeemer, TypeScript witness builder, ABI fixture, and budget test.

## Current Production Review Status

Status: complete for the production-reachable transition-trace proof surface.

The implementation now has header-bound Aiken/TypeScript coverage for the
transition-trace proof family, including phase-specific one-step effect
verification. The proof validator recomputes expected ledger-root transitions
from opened source events plus explicit UTxO membership, non-membership,
deletion, and insertion witnesses, then accepts only when the committed trace
leaf disagrees with that derived result.

The valid-deposit branch derives the inserted L2 output from the authenticated
L1 deposit reference input and committed `DepositInfo`, including
`l2_network_id`; witnesses cannot provide arbitrary projected bytes. Valid
withdrawals delete the opened UTxO, invalid withdrawals and invalid forced
transactions are no-op checks, and normal L2 transactions verify opened spend
and output preimages against the compact transaction before applying the
delete/insert witness sequence.

Valid forced transactions remain deliberately fail-closed: block production
refuses to commit an effectful forced-transaction trace until forced-transaction
ledger deltas/preimages exist, and the Aiken validator rejects the unsupported
valid-forced redeemer path. This is a launch-gate restriction, not a proof
workaround; no production block can currently commit that transition class.

## Files To Update

Aiken libraries:

- Add modules under
  `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/`
- Update existing fraud-proof shared libraries under
  `onchain/aiken/lib/midgard/fraud-proofs/`
- `onchain/aiken/lib/midgard/ledger-state.ak`
- Aiken root/proof helper modules.

Aiken validators:

- Add one or more validators under
  `onchain/aiken/validators/fraud-proofs/`
- Update any fraud-proof registry or token policy that enumerates proof kinds.
- Update state queue removal paths if they match proof constructors by name.

TypeScript witness builders:

- `demo/midgard-sdk/src/ledger-state.ts`
- Add `demo/midgard-sdk/src/fraud-proofs/transition-trace.ts`
- `demo/midgard-fault-proofs/src/`
- `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts`

Tests:

- Add Aiken unit tests for each redeemer shape.
- Add TypeScript/Aiken fixture tests under `demo/midgard-node/tests/`.
- Add emulator tests for at least one fraudulent block removal path.

## Fault Families

### Trace Boundary Fault

Proves either:

- `transition_trace[0].pre_utxos_root != header.prev_utxos_root`
- last trace step's `post_utxos_root != header.utxos_root`

Witnesses:

- header
- trace membership proof for first or last step
- count/range proof for last step

### Trace Link Fault

Proves:

```text
transition_trace[i].post_utxos_root
  != transition_trace[i + 1].pre_utxos_root
```

Witnesses:

- adjacent trace proofs for `i` and `i + 1`

### Event-To-Step Mismatch

Proves:

- trace step claims `event_key`
- `event_to_step_root[event_key]` is absent, or
- it maps to a different `step_index`, or
- it maps to a different phase

Witnesses:

- trace membership proof
- event-to-step membership or non-membership proof

### Source Membership Mismatch

Proves:

- event-to-step entry exists, but the source event is absent from its phase root
- source event exists, but event-to-step is absent
- source event phase does not match the committed `TransitionStep.phase`

Witnesses:

- source root membership or non-membership proof
- event-to-step membership proof
- trace membership proof when needed

### Invalid One-Step Transition

Proves the opened source event does not transform:

```text
pre_utxos_root -> post_utxos_root
```

for the phase-specific state machine.

Cases:

- withdrawal effect or no-op validity classification is wrong
- forced transaction effect or no-op validity classification is wrong
- normal L2 transaction effect is wrong
- deposit insertion effect is wrong

The trace leaf does not contain a claimed effect. The validator derives the
expected effect from the opened source event and witnesses.

### Omitted Due L1 Event

Proves:

- L1 event exists
- event is due for the block window
- event key is absent from the relevant source root

Cases:

- due withdrawal omitted
- due forced tx order omitted
- due deposit omitted

### Duplicate Trace Event

Proves two trace leaves contain the same `EventKey`.

Witnesses:

- trace membership proofs for both indices
- inequality proof for distinct indices

### Out-Of-Window Source Event

Proves a source root contains an L1 event outside the block window. For forced
transaction orders, the window check uses the transaction validity range
extracted from the ordered transaction body, not an `inclusion_time` field.

### Count Fault

Proves:

- source counts do not sum to `total_event_count`
- `transition_step_count != total_event_count`
- root count proof contradicts the header count

## Implementation Notes

- Keep redeemers small and phase-specific where it helps budget.
- Use shared proof parsing helpers, but avoid one giant catch-all redeemer that
  becomes impossible to audit.
- Every redeemer must bind to the header hash being challenged.
- Every proof must be replay-safe against unrelated headers.
- Budget tests are part of this task, not later polish.

## Tests And Verification

Add tests for every fault family above, with at least one negative test proving
the validator rejects a malformed witness.

Run:

```sh
cd ../onchain/aiken && aiken check
cd demo && pnpm run test -- sdk-abi-fixtures
cd demo && pnpm run test -- fraud
```

## Exit Criteria

- Every transition commitment invariant has an enforceable on-chain proof.
- Proof witnesses can be constructed by TypeScript and accepted by Aiken.
- Budgets are recorded and reviewed before launch.
