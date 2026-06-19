# Task 04: Trace And Event-To-Step Builders

## Goal

Build `transition_trace_root` and `event_to_step_root` during block production,
using the same event order the ledger is supposed to execute.

This task is complete when `processMpfs` or its successor produces:

- final `utxos_root`
- all source roots
- `transition_trace_root`
- `event_to_step_root`
- source counts
- transition step count
- retained trace/event members for DA and fault proofs

## Files To Update

Primary block production:

- `demo/midgard-node/src/workers/utils/mpf.ts`
- `demo/midgard-node/src/workers/commit-block-header/event-roots.ts`
- Add `demo/midgard-node/src/workers/commit-block-header/transition-trace.ts`
  if the builder is large enough to deserve its own module.

Commit submission:

- `demo/midgard-node/src/workers/commit-block-header/submission.ts`
- `demo/midgard-node/src/workers/commit-block-header/pending-journal.ts`

Pending finalization persistence:

- `demo/midgard-node/src/database/pendingBlockFinalizations.ts`
- `demo/midgard-node/src/database/migrations/sql/0005_pending_finalization_journal_payloads.sql`
- Add follow-up migration files for new installations and upgrade tests.

Tests:

- `demo/midgard-node/tests/confirm-block-commitments.test.ts`
- `demo/midgard-node/tests/deposit-flow-emulator.test.ts`
- `demo/midgard-node/tests/operator-lifecycle-emulator.test.ts`
- Add focused trace builder tests under `demo/midgard-node/tests/`.

## Builder Inputs

The trace builder receives canonical source-event arrays:

```text
withdrawals: WithdrawalEvent[]
forced_transactions: ForcedInclusionTx[]
l2_transactions: MidgardTxCompact[]
deposits: DepositEvent[]
```

The canonical execution order is:

```text
withdrawals
forced_transactions
l2_transactions
deposits
```

## Builder Outputs

```text
TraceBuildResult {
  final_utxos_root
  transition_trace_root
  event_to_step_root

  transition_trace_members
  event_to_step_members

  withdrawal_count
  forced_transaction_count
  l2_transaction_count
  deposit_count
  total_event_count
  transition_step_count
}
```

Each `transition_trace_member` is:

```text
step_index -> TransitionStep
```

Each `event_to_step_member` is:

```text
EventKey -> EventToStepValue
```

## Implementation Notes

- Current block production applies deposits before normal transactions by
  prepending deposit operations to the ledger operation list. Replace that with
  explicit phase execution.
- Same-block deposit spending must be rejected because deposits execute last.
- Build trace steps by applying one source event at a time to a working UTxO
  root.
- For a no-op event, `pre_utxos_root == post_utxos_root`.
- For an effectful event, `post_utxos_root` is the root after applying exactly
  that event.
- Reject duplicate source keys before root construction.
- Reject duplicate event keys before building `event_to_step_root`.
- Reject dangling trace steps whose `event_key` has no source-root member.
- `transition_step_count` must equal `total_event_count`.

## Event-Key Mapping

Construct keys as:

```text
withdrawal:<withdrawal_id>
forced_tx:<tx_order_id>
tx:<tx_id>
deposit:<deposit_id>
```

The step index is the event's position after phase concatenation.

## Tests And Verification

Add tests for:

- exact phase order
- same-block deposit spend rejection
- no-op invalid forced transaction step
- no-op invalid withdrawal step
- normal valid L2 transaction step
- effectful deposit insertion step
- trace root changes when any intermediate root changes
- event-to-step root changes when any step index changes
- duplicate source key rejection
- duplicate trace event rejection
- count mismatch rejection

Run:

```sh
cd demo && pnpm run test -- confirm-block-commitments
cd demo && pnpm run test -- deposit-flow-emulator
cd demo && pnpm run typecheck
```

## Exit Criteria

- Block production no longer computes only the final UTxO root.
- Every event has an authenticated step.
- Every step has an authenticated source event.
- Deposits are applied last in production block construction.

