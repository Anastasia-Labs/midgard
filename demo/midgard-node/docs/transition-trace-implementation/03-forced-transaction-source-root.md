# Task 03: Forced Transaction Source Root

## Goal

Introduce `forced_transactions_root` as the canonical source root for L1
transaction-order events.

This task is complete when the node can ingest due tx-order UTxOs, classify
their operator validity, include every due order exactly once in
`forced_transactions_root`, and expose enough payload data for challengers to
prove omission, invalid classification, or wrong execution.

## Files To Update

SDK and Aiken event definitions:

- `demo/midgard-sdk/src/ledger-state.ts`
- `demo/midgard-sdk/src/user-events/tx-order.ts`
- `onchain/aiken/lib/midgard/ledger-state.ak`
- `onchain/aiken/lib/midgard/user-events/tx-order.ak`
- `onchain/aiken/validators/user-events/tx-order.ak`

Node ingestion and storage:

- Add a new database module such as
  `demo/midgard-node/src/database/forcedTransactions.ts`
- Add a migration under
  `demo/midgard-node/src/database/migrations/sql/`
- Add or update the tx-order watcher/fetcher fiber under
  `demo/midgard-node/src/workers/` if no production path exists yet.
- `demo/midgard-node/src/workers/commit-block-header/event-roots.ts`
- `demo/midgard-node/src/workers/utils/mpf.ts`

Commit and settlement paths:

- `demo/midgard-node/src/workers/commit-block-header/submission.ts`
- `demo/midgard-sdk/src/state-queue-production.ts`
- `demo/midgard-sdk/src/settlement.ts`
- `onchain/aiken/validators/settlement.ak`

Tests:

- `demo/midgard-node/tests/`
- `demo/midgard-sdk/tests/`
- `onchain/aiken/tests/` if the tx-order validator has unit tests.

## Types To Add Or Change

### TxOrderId

Align the type across Aiken and TypeScript:

```text
TxOrderId = OutputReference
```

The current TypeScript shape uses a hash-like id in places. Replace that with
the L1 order output reference where this is production-facing.

### ForcedInclusionTx

Source-root value:

```text
MidgardTxCompactWithoutValidity {
  body
  wits
}

ForcedInclusionTx {
  tx_compact: MidgardTxCompactWithoutValidity
  operator_validity: MidgardTxValidity
}
```

Root:

```text
forced_transactions_root =
  MapRoot<TxOrderId -> ForcedInclusionTx>
```

### EventKey

Forced tx event keys must use:

```text
forced_tx:<tx_order_id>
```

not:

```text
tx:<tx_id>
```

## Implementation Notes

- Every due L1 tx-order event is obligatory. Missing due orders must be
  challengeable with L1 evidence plus non-membership in `forced_transactions_root`.
- Invalid forced transactions remain source events. They become no-op trace
  steps with challengeable `operator_validity`.
- Duplicate L1 orders carrying the same `tx_compact.body` are distinct source
  events because the root is keyed by `tx_order_id`.
- `transactions_root` must not be used as the tx-order source root.
- Do not add `inclusion_time` to the forced-transaction source value. Due-window
  checks for transaction orders use the validity range extracted from
  `tx_compact.body` plus L1 order evidence.

## Tests And Verification

Add tests for:

- two tx orders with the same `tx_compact.body` but different `tx_order_id`
- omitted due tx-order event
- invalid tx-order included as no-op trace source event
- wrong `operator_validity`
- root key encoded as output reference, not transaction id
- settlement proof uses `forced_transactions_root` for tx orders

Run:

```sh
cd demo && pnpm run test -- tx-order
cd demo && pnpm run typecheck
cd ../onchain/aiken && aiken check
```

## Exit Criteria

- `forced_transactions_root` is built and committed for every block.
- L1 tx-order settlement and fault proofs no longer rely on
  `transactions_root`.
- Forced and normal transactions cannot collide through a shared `tx_id` event
  identity.
