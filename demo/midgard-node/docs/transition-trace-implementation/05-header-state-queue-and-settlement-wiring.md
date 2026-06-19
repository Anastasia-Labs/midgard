# Task 05: Header, State Queue, And Settlement Wiring

## Goal

Carry transition trace commitments through the on-chain state queue and
settlement flows so the new roots are consensus data, not DA-only metadata.

This task is complete when the committed header, state queue merge redeemer,
latest blocks datum update, and settlement datum all agree on the new root
surface.

## Files To Update

State queue Aiken:

- `onchain/aiken/lib/midgard/state-queue.ak`
- `onchain/aiken/validators/state-queue.ak`
- `onchain/aiken/lib/midgard/ledger-state.ak`

State queue TypeScript:

- `demo/midgard-sdk/src/state-queue.ts`
- `demo/midgard-sdk/src/state-queue-production.ts`
- `demo/midgard-node/src/workers/commit-block-header/submission.ts`

Settlement Aiken:

- `onchain/aiken/lib/midgard/settlement.ak`
- `onchain/aiken/validators/settlement.ak`

Settlement TypeScript:

- `demo/midgard-sdk/src/settlement.ts`
- `demo/midgard-node/src/workers/commit-block-header/submission.ts`
- `demo/midgard-node/src/commands/event-settlement-proof.ts`

Tests:

- `demo/midgard-sdk/tests/state-queue.test.ts`
- `demo/midgard-node/tests/sdk-abi-fixtures.test.ts`
- `demo/midgard-node/tests/operator-lifecycle-emulator.test.ts`
- `demo/midgard-node/tests/deposit-flow-emulator.test.ts`
- Settlement-related emulator tests under `demo/midgard-node/tests/`.

## Header Wiring

Update all header construction and comparison sites to include:

```text
forced_transactions_root
transition_trace_root
event_to_step_root

withdrawal_count
forced_transaction_count
l2_transaction_count
deposit_count
total_event_count
transition_step_count
```

The state queue merge redeemer must carry the same root and count values that
are claimed by the merged block header.

## State Queue Validator Checks

Add validator checks for:

- `prev_utxos_root` matches the previous confirmed state.
- final `utxos_root` matches the claimed merged output.
- source roots match the roots in the merge redeemer.
- trace and event-to-step roots match the roots in the merge redeemer.
- `total_event_count` is the sum of source counts.
- `transition_step_count == total_event_count`.

Do not treat trace roots as optional metadata.

## Settlement Datum

Settlement currently carries source roots needed to prove event inclusion. Add:

```text
forced_transactions_root
```

Keep:

```text
deposits_root
withdrawals_root
transactions_root
```

Only include `transition_trace_root` and `event_to_step_root` in settlement
datum if a settlement validator path directly verifies transition faults. If
transition faults are handled by dedicated fraud-proof validators that read the
state queue header, settlement datum can remain source-root focused.

## Settlement Event Proofs

Update event-spend proofs:

- deposits prove membership in `deposits_root`
- withdrawals prove membership in `withdrawals_root`
- forced transaction orders prove membership in `forced_transactions_root`
- normal L2 transactions prove membership in `transactions_root` only where a
  normal L2 transaction proof is actually required

Do not prove tx-order settlement through `transactions_root`.

## Tests And Verification

Add tests for:

- merge redeemer missing a new root is rejected by ABI fixtures
- header hash changes when trace root changes
- state queue merge rejects mismatched trace root
- state queue merge rejects mismatched event-to-step root
- state queue merge rejects mismatched event counts
- settlement tx-order proof uses `forced_transactions_root`
- old settlement proof through `transactions_root` cannot settle a tx order

Run:

```sh
cd demo && pnpm run test -- state-queue
cd demo && pnpm run test -- sdk-abi-fixtures
cd demo && pnpm run typecheck
cd ../onchain/aiken && aiken check
```

## Exit Criteria

- The authoritative L1 header commits to all trace roots and counts.
- State queue merge cannot omit or substitute trace commitments.
- Settlement no longer conflates forced tx orders with normal L2 transactions.

