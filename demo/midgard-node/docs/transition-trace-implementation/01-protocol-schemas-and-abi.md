# Task 01: Protocol Schemas And ABI

## Goal

Define the canonical TypeScript, Aiken, and fixture schemas for transition trace
commitments before changing block production or validators.

This task is complete when the codebase has a single agreed ABI for:

- `HeaderV2`
- `TransitionStep`
- `TransitionPhase`
- `EventKey`
- `EventToStepValue`
- `ForcedInclusionTx`
- event and trace counts

## Files To Update

Core schemas:

- `demo/midgard-sdk/src/ledger-state.ts`
- `onchain/aiken/lib/midgard/ledger-state.ak`
- `demo/midgard-sdk/src/index.ts`

State queue and settlement schemas that embed header/root data:

- `demo/midgard-sdk/src/state-queue.ts`
- `demo/midgard-sdk/src/state-queue-production.ts`
- `demo/midgard-sdk/src/settlement.ts`
- `onchain/aiken/lib/midgard/state-queue.ak`
- `onchain/aiken/lib/midgard/settlement.ak`

ABI fixture tests:

- `demo/midgard-node/tests/sdk-abi-fixtures.test.ts`
- Add a dedicated fixture file under `demo/midgard-node/tests/fixtures/` if
  the existing fixture file becomes too large.

Technical specification:

- `technical-spec/1-ledger-state/1-block.tex`
- `technical-spec/1-ledger-state/5-transaction-order-event.tex`
- `technical-spec/1-ledger-state/6-transaction.tex`
- Any generated CDDL or ABI appendix used by the spec build.

## Types To Add Or Change

### HeaderV2

Add the new roots and counts to the block header datum:

```text
HeaderV2 {
  prev_utxos_root
  utxos_root

  withdrawals_root
  forced_transactions_root
  transactions_root
  deposits_root

  transition_trace_root
  event_to_step_root

  withdrawal_count
  forced_transaction_count
  l2_transaction_count
  deposit_count
  total_event_count
  transition_step_count

  start_time
  end_time
  protocol_version
  prev_header_hash
  operator_vkey
}
```

`total_event_count` must equal:

```text
withdrawal_count
+ forced_transaction_count
+ l2_transaction_count
+ deposit_count
```

`transition_step_count` must equal `total_event_count`.

### TransitionPhase

Define an enum with stable CBOR constructors:

```text
Withdrawal
ForcedTransaction
L2Transaction
Deposit
```

### EventKey

Define source-specific keys:

```text
WithdrawalEventKey { withdrawal_id }
ForcedTransactionEventKey { tx_order_id }
L2TransactionEventKey { tx_id }
DepositEventKey { deposit_id }
```

Do not collapse forced transactions and normal L2 transactions into a shared
`tx_id` key. A forced transaction event is identified by the L1 order UTxO.

### EventToStepValue

Use a compact value that lets the verifier bind the exact source event to a
trace step:

```text
EventToStepValue {
  step_index
  phase
}
```

If the root primitive requires a value hash instead of a structured value, make
that hash a domain-separated hash of this structure.

### TransitionStep

Define the trace leaf:

```text
TransitionStep {
  schema_version
  step_index
  event_key
  phase
  pre_utxos_root
  post_utxos_root
}
```

Do not add claimed effect fields, read-set hashes, consumed-set hashes, or local
result commitments to the base step.

### ForcedInclusionTx

Define the forced transaction source-root value:

```text
ForcedInclusionTx {
  tx_order_id
  tx_id
  tx_compact
  operator_validity
  inclusion_time
}
```

`tx_order_id` is the L1 order identity, expected to align with the order UTxO
output reference. `tx_id` is the L2 transaction id.

## Implementation Notes

- Use stable field order. Header hash stability depends on exact CBOR shape.
- Keep `transactions_root` as `MidgardTxId -> MidgardTxCompact`.
- Introduce `forced_transactions_root` as
  `TxOrderId -> ForcedInclusionTx`.
- Keep `deposits_root` and `withdrawals_root` source-event keyed.
- Add explicit versioning only where it is actually serialized. Avoid hidden
  version assumptions in helper functions.

## Tests And Verification

- Add golden TypeScript/Aiken round-trip fixtures for all new types.
- Extend `sdk-abi-fixtures.test.ts` to assert the exact constructor and field
  order expected by Aiken.
- Add a fixture proving `hashBlockHeader` changes when any new root or count
  changes.
- Run:

  ```sh
  cd demo && pnpm run typecheck
  cd demo && pnpm run test -- sdk-abi-fixtures
  cd ../onchain/aiken && aiken check
  make spec
  ```

## Exit Criteria

- Aiken and TypeScript agree on every new serialized type.
- The old header shape is no longer used in production commit paths.
- The spec has the same field names, phase names, and count invariants as the
  code.

